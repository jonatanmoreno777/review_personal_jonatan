
############## okkkkkkkkkkkk
#install.packages('devtools')
#devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.2.8/catboost-R-windows-x86_64-1.2.8.tgz', INSTALL_opts = c("--no-multiarch"))

# ---------------------------
# Librerías
# ---------------------------
library(sf)
library(sp)
library(raster)
library(dplyr)
library(FNN)
library(randomForest)
library(lightgbm)
library(xgboost)
library(catboost)
library(gstat)
library(GWmodel)
library(foreach)
library(doParallel)
library(lubridate)
library(meteo) #https://github.com/AleksandarSekulic/Rmeteo


# ---------------------------
# 1️⃣ Cargar datos de estaciones
# ---------------------------
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_huallaga.csv")
data$time <- as.Date(data$time, format = "%m/%d/%Y")

# ---------------------------
# 2️⃣ Leer cuenca y crear grilla
# ---------------------------
cuenca <- st_read("D:/S/Serbia1km/Interpolation/Titicaca_border/watershed.shp")
cuenca <- st_transform(cuenca, crs = 4326)
bbox <- st_bbox(cuenca)
lon_seq <- seq(bbox["xmin"], bbox["xmax"], by = 0.05)
lat_seq <- seq(bbox["ymin"], bbox["ymax"], by = 0.05)
grid_df <- expand.grid(lon = lon_seq, lat = lat_seq)
grid_sf <- st_as_sf(grid_df, coords = c("lon", "lat"), crs = 4326)
grid_dentro <- st_join(grid_sf, cuenca, join = st_within, left = FALSE)
coords <- st_coordinates(grid_dentro)
grid_cells <- data.frame(lon = coords[,1], lat = coords[,2])

# ---------------------------
# 3️⃣ Leer rásters estáticos
# ---------------------------
r_dem   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/dem.tif")
r_twi   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/twi.tif")
r_sin   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/sin.tif")
r_cos   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/cos.tif")
r_slope <- raster("D:/S/Serbia1km/Interpolation/Huallaga/slope.tif")

# ---------------------------
# 4️⃣ Extraer valores estáticos
# ---------------------------
coordinates(grid_cells) <- ~lon + lat
proj4string(grid_cells) <- CRS("+proj=longlat +datum=WGS84")
grid_cells$dem   <- raster::extract(r_dem, grid_cells)
grid_cells$twi   <- raster::extract(r_twi, grid_cells)
grid_cells$sin   <- raster::extract(r_sin, grid_cells)
grid_cells$cos   <- raster::extract(r_cos, grid_cells)
grid_cells$slope <- raster::extract(r_slope, grid_cells)

grid_static_df <- as.data.frame(grid_cells) %>%
  dplyr::select(lon, lat, dem, twi, sin, cos, slope) %>%
  mutate(across(c(twi, sin, cos, slope), ~ round(.x, 2))) %>%
  mutate(
    sin = ifelse(is.na(sin), 0, sin),
    cos = ifelse(is.na(cos), 0, cos)
  )

# Función para rellenar NA con vecino más cercano (solo para variables topográficas)
rellenar_na_vecino <- function(df, col) {
  na_rows <- which(is.na(df[[col]]))
  if(length(na_rows) == 0) return(df)
  no_na_rows <- which(!is.na(df[[col]]))
  vecinos <- get.knnx(df[no_na_rows, c("lon", "lat")], df[na_rows, c("lon", "lat")], k=1)$nn.index
  df[[col]][na_rows] <- df[[col]][no_na_rows][vecinos]
  return(df)
}

grid_static_df1 <- grid_static_df %>%
  rellenar_na_vecino("dem") %>%
  rellenar_na_vecino("twi") %>%
  rellenar_na_vecino("slope")

# ---------------------------
# 5️⃣ Función GWR + IDW de respaldo
# ---------------------------
predecir_gwr <- function(datos_dia, grid_df) {
  tryCatch({
    estaciones <- datos_dia[, c("lon", "lat", "prcp", "dem", "twi", "slope")]
    coordinates(estaciones) <- ~lon + lat
    proj4string(estaciones) <- CRS("+proj=longlat +datum=WGS84")
    
    grid_sp <- grid_df
    coordinates(grid_sp) <- ~lon + lat
    proj4string(grid_sp) <- CRS("+proj=longlat +datum=WGS84")
    
    bw <- suppressWarnings(
      gwr.sel(prcp ~ dem + twi + slope, data = estaciones, coords = coordinates(estaciones), adaptive = TRUE)
    )
    
    pred_gwr <- gwr.predict(prcp ~ dem + twi + slope, data = estaciones,
                            predictdata = grid_sp, bw = bw, kernel = "bisquare",
                            adaptive = TRUE)
    
    return(pmax(0, pred_gwr$SDF$pred))
  }, error = function(e) {
    message("⚠️ GWR falló, usando IDW.")
    df_sp <- datos_dia[, c("lon", "lat", "prcp")]
    coordinates(df_sp) <- ~lon + lat
    proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
    idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_sp, idp = 2)
    return(pmax(0, idw_p$var1.pred))
  })
}


# ---------------------------
# 6️⃣ Función ML (rf, lgbm, xgb, ctb) con fallback a GWR
# ---------------------------
predecir_prcp <- function(modelo, datos_dia, grid_df, predictors) {
  
  # 1️⃣ Preparar datos de entrenamiento
  train <- datos_dia[, c(predictors, "prcp")]
  train <- train[complete.cases(train), ]
  
  # 2️⃣ Validación temprana: pocas observaciones
  if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
    return(predecir_gwr(datos_dia, grid_df))
  }
  
  # ---------------------------
  # 3️⃣ Modelo según selección
  # ---------------------------
  
  #if (modelo == "rf") {
    # Random Forest estándar
    #rf <- randomForest(prcp ~ ., data = train, ntree = 100)
    #preds <- predict(rf, newdata = grid_df)
    #return(pmax(0, preds))
    
  # } else
    
  if (modelo == "rfsi") {
    # 🔹 Random Forest Spatial Interpolation (paquete {meteo})
    out <- tryCatch({
      # Crear objeto sf de entrenamiento
      train_sf <- st_as_sf(train, coords = c("lon","lat"), crs = 4326, remove = FALSE)
      
      # Crear objeto sf de predicción
      grid_sf <- st_as_sf(grid_df, coords = c("lon","lat"), crs = 4326, remove = FALSE)
      
      # Fórmula: precipitación en función de predictores
      form <- as.formula(paste("prcp ~", paste(predictors, collapse = " + ")))
      
      # Ejecutar RFSI
      rfsi_mod <- meteo::rfsi(
        formula = form,
        data = train_sf,
        newdata = grid_sf,
        n.obs = 3,       # n# ← Clave: pocos vecinos por mala distribución DE ESTACIONES
        ntree   = 150,
        importance = TRUE,
        cpus = 1         # en tu loop paralelo, deja 1
      )
      
      return(pmax(0, rfsi_mod$pred))
      #preds <- if (!is.null(rfsi_mod$pred)) rfsi_mod$pred else rfsi_mod$predictions
      #return(pmax(0, as.numeric(preds)))
      
    }, error = function(e) {
      message("⚠️ RFSI (meteo) falló: ", conditionMessage(e), " -> fallback a GWR")
      return(predecir_gwr(datos_dia, grid_df))
    })
    return(out)
    
  } else if (modelo == "lgbm") {
    # LightGBM
    train_x <- as.matrix(train[, predictors]); train_y <- train$prcp
    dtrain <- lgb.Dataset(data = train_x, label = train_y)
    params <- list(objective = "regression", metric = "rmse", learning_rate = 0.05,
                   num_leaves = 31, max_depth = 6, min_data_in_leaf = 3, verbosity = -1)
    model <- lgb.train(params, dtrain, nrounds = 300, verbose = -1)
    test_x <- as.matrix(grid_df[, predictors]); preds <- predict(model, test_x)
    return(pmax(0, preds))
    
  } else if (modelo == "xgb") {
    # XGBoost
    train_x <- as.matrix(train[, predictors]); train_y <- train$prcp
    dtrain <- xgb.DMatrix(data = train_x, label = train_y)
    params <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 6,
                   subsample = 0.8, colsample_bytree = 0.8)
    model <- xgb.train(params, dtrain, nrounds = 300, verbose = 0)
    test_x <- as.matrix(grid_df[, predictors]); preds <- predict(model, test_x)
    return(pmax(0, preds))
    
  } else if (modelo == "ctb") {
    # CatBoost con manejo robusto de errores
    out <- tryCatch({
      train_df <- as.data.frame(train[, predictors], stringsAsFactors = FALSE)
      train_df[] <- lapply(train_df, function(x) as.numeric(as.character(x)))
      train_y  <- as.numeric(train$prcp)
      
      test_df <- as.data.frame(grid_df[, predictors], stringsAsFactors = FALSE)
      test_df[] <- lapply(test_df, function(x) as.numeric(as.character(x)))
      
      # Rellenar NAs en test con la media de train
      for (col in names(test_df)) {
        if (all(is.na(test_df[[col]]))) {
          test_df[[col]] <- mean(train_df[[col]], na.rm = TRUE)
        } else {
          nas <- which(is.na(test_df[[col]]))
          if (length(nas) > 0) test_df[[col]][nas] <- mean(train_df[[col]], na.rm = TRUE)
        }
      }
      
      train_pool <- catboost.load_pool(data = train_df, label = train_y)
      test_pool  <- catboost.load_pool(data = test_df)
      
      params <- list(loss_function = "RMSE",
                     iterations = 300,
                     depth = 6,
                     learning_rate = 0.05,
                     logging_level = "Silent",
                     train_dir = tempfile())
      
      model_ctb <- catboost.train(train_pool, params = params)
      preds <- catboost.predict(model_ctb, test_pool)
      
      return(pmax(0, as.numeric(preds)))
      
    }, error = function(e) {
      message("⚠️ CatBoost falló: ", conditionMessage(e), " -> fallback a GWR")
      return(predecir_gwr(datos_dia, grid_df))
    })
    return(out)
    
  } else {
    # Modelo no reconocido, fallback a GWR
    warning("Modelo '", modelo, "' no reconocido. Usando GWR como respaldo.")
    return(predecir_gwr(datos_dia, grid_df))
  }
}

# ---------------------------
# 7️⃣ Función combinada ML + GWR + IDW para zonas con pocas estaciones
# ---------------------------
predecir_prcp_combinado <- function(modelo, datos_dia, grid_df, predictors, umbral_gwr = 5, umbral_dist = 0.5, peso_ml = 0.7) {
  
  # Si hay pocas estaciones, usar solo GWR
  if (nrow(datos_dia) < umbral_gwr || length(unique(datos_dia$prcp)) < 2) {
    return(predecir_gwr(datos_dia, grid_df))
  }
  
  # 1️⃣ Predicción principal con ML
  preds_ml <- predecir_prcp(modelo, datos_dia, grid_df, predictors)
  
  # 2️⃣ Calcular distancia mínima a la estación más cercana
  coords_grid <- as.matrix(grid_df[, c("lon", "lat")])
  coords_est <- as.matrix(datos_dia[, c("lon", "lat")])
  dist_min <- FNN::get.knnx(coords_est, coords_grid, k = 1)$nn.dist[,1]
  
  # 3️⃣ Detectar celdas lejanas (donde aplicar GWR)
  idx_gwr <- which(dist_min > umbral_dist)
  
  # 4️⃣ Predicción GWR solo en celdas lejanas
  if (length(idx_gwr) > 0) {
    subgrid <- grid_df[idx_gwr, ]
    preds_gwr <- predecir_gwr(datos_dia, subgrid)
    
    # Mezclar ML y GWR ponderadamente
    preds_ml[idx_gwr] <- peso_ml*preds_ml[idx_gwr] + (1 - peso_ml)*preds_gwr
  }
  
  return(preds_ml)
}

# ---------------------------
# 8️⃣ Configuración
# ---------------------------
predictors <- c("lon","lat","dem","twi","sin","cos","slope","tmax","tmin","precland")
modelo_prcp <- "ctb"  # "rfsi", "lgbm", "xgb", "ctb"

# ---------------------------
# 9️⃣ Loop paralelo por fechas
# ---------------------------
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

fechas <- sort(unique(data$time))

resultados_list <- foreach(
  fecha = fechas,
  .packages = c("dplyr","sp","gstat","randomForest","lightgbm","xgboost","catboost","GWmodel","raster","FNN", "meteo")
) %dopar% {
  
  datos_dia <- dplyr::filter(data, time == fecha)
  #if (nrow(datos_dia) < 2) return(NULL)
  
  grid_df_day <- grid_static_df1
  
  # Interpolación diaria de tmax, tmin, precland usando IDW
  for (var in c("tmax","tmin","precland")) {
    if (!var %in% names(datos_dia)) {
      grid_df_day[[var]] <- NA_real_; next
    }
    if (length(unique(datos_dia[[var]])) < 2) {
      grid_df_day[[var]] <- mean(datos_dia[[var]], na.rm = TRUE); next
    }
    
    df_sp <- datos_dia[, c("lon","lat",var)]
    coordinates(df_sp) <- ~lon+lat
    proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
    
    pred_vec <- tryCatch({
      idw_out <- idw(as.formula(paste(var,"~1")), df_sp, newdata=grid_df_day, idp=2)
      idw_out$var1.pred
    }, error = function(e) rep(NA_real_, nrow(grid_df_day)))
    
    grid_df_day[[var]] <- pred_vec
  }
  
  # Predecir precipitación combinando ML + GWR + IDW
  grid_df_day$prcp <- predecir_prcp_combinado(modelo_prcp, datos_dia, grid_df_day, predictors)
  
  # Convertir a raster
  xyz <- grid_df_day[, c("lon","lat","prcp")]
  xyz <- xyz[complete.cases(xyz), ]
  raster_prcp <- tryCatch({
    rasterFromXYZ(xyz, res = 0.05, crs = CRS("+proj=longlat +datum=WGS84"))
  }, error = function(e) NULL)
  
  list(fecha=as.character(fecha), raster=raster_prcp)
}

stopCluster(cl)

# --- Organizar resultados por año ---
resultados_por_anio <- list()
fechas_por_anio <- list()

for (res in resultados_list) {
  if (is.null(res)) next
  anio <- substr(res$fecha, 1, 4)
  if (!anio %in% names(resultados_por_anio)) {
    resultados_por_anio[[anio]] <- stack()
    fechas_por_anio[[anio]] <- character(0)
  }
  resultados_por_anio[[anio]] <- addLayer(resultados_por_anio[[anio]], res$raster)
  fechas_por_anio[[anio]] <- c(fechas_por_anio[[anio]], res$fecha)
}

# --- Guardar archivos raster ---
output_dir <- "D:/S/Serbia1km/Interpolation/Output"
if (!dir.exists(output_dir)) dir.create(output_dir)

for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    names(s) <- fechas_utilizadas
    file_name <- file.path(output_dir, paste0("huallagagwr_", modelo_prcp, "_", anio, ".tif"))
    writeRaster(s, filename = file_name, format = "GTiff", overwrite = TRUE)
    message("Guardado: ", file_name)
  }
}

library(raster)

# Cargar el raster multicapas de un año
r <- stack("D:/S/Serbia1km/Interpolation/Output/huallagagwr_rf_2000.tif")

plot(r)

library(raster)

output_dir <- "D:/S/Serbia1km/Interpolation/Output"
anio <- "2001"
dias <- 1:4
modelos <- c("rfsi", "xgb", "ctb", "lgbm")

# Fechas reales (suponiendo que la capa 1 = 2000-01-01)
fechas <- as.Date(paste0(anio, "-01-01")) + (dias - 1)

# ---------------------------
# Layout de la ventana gráfica
par(mfrow = c(length(modelos), length(dias)), 
    mar = c(1, 1, 2.1, 1),   # espacio dse cada celda
    oma = c(1,3,0,1))  # espacio a la izquierda y arriba

# ---------------------------
# Graficar cada modelo y los primeros 4 días
for (m_idx in seq_along(modelos)) {
  modelo <- modelos[m_idx]
  file_path <- file.path(output_dir, paste0("huallagagwr_", modelo, "_", anio, ".tif"))
  
  if (!file.exists(file_path)) {
    warning("No se encontró archivo: ", file_path)
    next
  }
  
  r_stack <- stack(file_path)
  n_dias <- min(length(dias), nlayers(r_stack))
  
  for (i in 1:n_dias) {
    r_day <- r_stack[[i]]
    
    # Si es primera fila → título con fecha (mayúscula y negrita), si no → sin título
    if (m_idx == 1) {
      titulo <- toupper(format(fechas[i], "%Y-%m-%d"))
    } else {
      titulo <- ""
    }
    
    plot(r_day, 
         main = titulo, 
         col = terrain.colors(20),
         cex.main = 1.2,      # tamaño títulos columnas
         font.main = 2.5,       # negrita
         legend.width = 2.3,  # ancho de la barra de colores
         legend.shrink = 0.8, # altura relativa de la barra
         axes = FALSE,        # quitamos ejes por defecto
         box = FALSE)         # quitamos borde por defecto
    
    # --- Mostrar LAT solo en primera columna ---
    if (i == 1) {
      axis(2, las = 1, cex.axis = 0.8)
    }
    
    # --- Mostrar LON solo en última fila ---
    if (m_idx == length(modelos)) {
      axis(1, cex.axis = 0.8)
    }
    
    # Dibujar borde del mapa
    box()
    
    # --- Título de fila con nombre del modelo ---
    if (i == 1) {
      mtext(toupper(modelo), side = 2, line = 2.5, cex = 1.1, font = 2)
    }
  }
}


##################### FIN ###############################################################################


# PARA REGION

predecir_prcp_combinado_regional <- function(modelo, datos_dia, grid_df, predictors, 
                                             umbral_gwr = 4, 
                                             umbral_dist_norte = 0.3,
                                             umbral_dist_sur = 1.2,
                                             peso_ml_norte = 0.8,
                                             peso_ml_sur = 0.3) {
  
  # 1. Clasificar zonas de la cuenca (basado en coordenadas)
  grid_df$region <- ifelse(grid_df$lat > -8.5, "norte", 
                           ifelse(grid_df$lat < -9.5, "sur", "centro"))
  
  # 2. Si pocas estaciones a nivel global → GWR puro
  if (nrow(datos_dia) < umbral_gwr) {
    return(predecir_gwr(datos_dia, grid_df))
  }
  
  # 3. Predicción base con ML
  preds_ml <- predecir_prcp(modelo, datos_dia, grid_df, predictors)
  
  # 4. Ajustar por región
  for(region in unique(grid_df$region)) {
    
    # Seleccionar celdas de esta región
    idx_region <- which(grid_df$region == region)
    subgrid <- grid_df[idx_region, ]
    
    # Definir parámetros por región
    if(region == "norte") {
      umbral_dist <- umbral_dist_norte
      peso_ml <- peso_ml_norte
    } else if(region == "sur") {
      umbral_dist <- umbral_dist_sur  
      peso_ml <- peso_ml_sur
    } else { # centro
      umbral_dist <- 0.6  # Valor intermedio
      peso_ml <- 0.6
    }
    
    # Calcular distancias para esta subregión
    coords_subgrid <- as.matrix(subgrid[, c("lon", "lat")])
    coords_est <- as.matrix(datos_dia[, c("lon", "lat")])
    dist_min <- FNN::get.knnx(coords_est, coords_subgrid, k = 1)$nn.dist[,1]
    
    # Aplicar GWR en zonas lejanas
    idx_gwr <- which(dist_min > umbral_dist)
    
    if(length(idx_gwr) > 0) {
      subgrid_gwr <- subgrid[idx_gwr, ]
      preds_gwr <- predecir_gwr(datos_dia, subgrid_gwr)
      
      # Mezcla ponderada
      preds_ml[idx_region][idx_gwr] <- peso_ml * preds_ml[idx_region][idx_gwr] + 
        (1 - peso_ml) * preds_gwr
    }
  }
  
  return(preds_ml)
}



# para peru

predecir_prcp_combinado_peru <- function(modelo, datos_dia, grid_df, predictors) {
  
  # 1. Clasificar cada celda de la grilla
  grid_df$region <- mapply(clasificar_region_peru, grid_df$lat, grid_df$lon, grid_df$dem)
  
  # 2. Parámetros por región natural
  parametros_region <- list(
    "costa" = list(umbral_gwr = 3, umbral_dist = 0.4, peso_ml = 0.8),
    "sierra" = list(umbral_gwr = 5, umbral_dist = 0.7, peso_ml = 0.6),
    "sierra_alta" = list(umbral_gwr = 2, umbral_dist = 1.0, peso_ml = 0.2),
    "selva" = list(umbral_gwr = 2, umbral_dist = 1.5, peso_ml = 0.3)
  )
  
  # 3. Si pocas estaciones a nivel nacional → estrategia conservadora
  if (nrow(datos_dia) < 5) {
    return(predecir_gwr_con_satelite(datos_dia, grid_df))  # Versión mejorada
  }
  
  # 4. Predicción base con ML
  preds_ml <- predecir_prcp(modelo, datos_dia, grid_df, predictors)
  
  # 5. Ajuste regional inteligente
  for(region in unique(grid_df$region)) {
    if(region %in% names(parametros_region)) {
      params <- parametros_region[[region]]
      # ... aplicar lógica específica por región
    }
  }
  
  return(preds_ml)
}

# Fase 1: Validar en una cuenca piloto (ej: Rímac en Lima)
resultados_rimac <- predecir_prcp_combinado_peru(
  modelo = "xgb",
  datos_dia = datos_dia_rimac,
  grid_df = grid_rimac,
  predictors = predictors
)

# Fase 2: Expandir a otras regiones
# Fase 3: Implementación nacional escalada



predecir_prcp <- function(modelo, datos_dia, grid_df, predictors) {
  
  # 1️⃣ Preparar datos de entrenamiento
  train <- datos_dia[, c(predictors, "prcp")]
  train <- train[complete.cases(train), ]
  
  # 2️⃣ Validación temprana: pocas observaciones
  if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
    return(predecir_gwr(datos_dia, grid_df))
  }
  
  # ---------------------------
  # 3️⃣ Modelo según selección
  # ---------------------------
  
  if (modelo == "rf") {
    # Random Forest estándar
    rf <- randomForest(prcp ~ ., data = train, ntree = 100)
    preds <- predict(rf, newdata = grid_df)
    return(pmax(0, preds))
    
  } else if (modelo == "rfsi") {
    # 🔹 Random Forest Spatial Interpolation
    out <- tryCatch({
      # Crear objeto sf de entrenamiento
      train_sf <- st_as_sf(train, coords = c("lon","lat"), crs = 4326)
      train_sf$prcp <- train$prcp
      
      # Crear objeto sf de predicción
      grid_sf <- st_as_sf(grid_df, coords = c("lon","lat"), crs = 4326)
      
      # Ejecutar RFSI
      preds <- rfsi(
        formula = prcp ~ dem + twi + slope + tmax + tmin + precland,
        data = train_sf,
        newdata = grid_sf,
        n.obs = 5,      # vecinos más cercanos
        ntrees = 100    # número de árboles
      )
      
      return(pmax(0, preds$pred))
      
    }, error = function(e) {
      message("⚠️ RFSI falló: ", conditionMessage(e), " -> fallback a GWR")
      return(predecir_gwr(datos_dia, grid_df))
    })
    return(out)
    
  } else if (modelo == "lgbm") {
    # LightGBM
    train_x <- as.matrix(train[, predictors]); train_y <- train$prcp
    dtrain <- lgb.Dataset(data = train_x, label = train_y)
    params <- list(objective = "regression", metric = "rmse", learning_rate = 0.05,
                   num_leaves = 31, max_depth = 6, min_data_in_leaf = 3, verbosity = -1)
    model <- lgb.train(params, dtrain, nrounds = 300, verbose = -1)
    test_x <- as.matrix(grid_df[, predictors]); preds <- predict(model, test_x)
    return(pmax(0, preds))
    
  } else if (modelo == "xgb") {
    # XGBoost
    train_x <- as.matrix(train[, predictors]); train_y <- train$prcp
    dtrain <- xgb.DMatrix(data = train_x, label = train_y)
    params <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 6,
                   subsample = 0.8, colsample_bytree = 0.8)
    model <- xgb.train(params, dtrain, nrounds = 300, verbose = 0)
    test_x <- as.matrix(grid_df[, predictors]); preds <- predict(model, test_x)
    return(pmax(0, preds))
    
  } else if (modelo == "ctb") {
    # CatBoost con manejo robusto de errores
    out <- tryCatch({
      # Coerción a data.frame numérico
      train_df <- as.data.frame(train[, predictors], stringsAsFactors = FALSE)
      train_df[] <- lapply(train_df, function(x) as.numeric(as.character(x)))
      train_y  <- as.numeric(train$prcp)
      
      test_df <- as.data.frame(grid_df[, predictors], stringsAsFactors = FALSE)
      test_df[] <- lapply(test_df, function(x) as.numeric(as.character(x)))
      
      # Rellenar NAs en test con la media de train
      for (col in names(test_df)) {
        if (all(is.na(test_df[[col]]))) {
          test_df[[col]] <- mean(train_df[[col]], na.rm = TRUE)
        } else {
          nas <- which(is.na(test_df[[col]]))
          if (length(nas) > 0) test_df[[col]][nas] <- mean(train_df[[col]], na.rm = TRUE)
        }
      }
      
      # Crear Pools
      train_pool <- catboost.load_pool(data = train_df, label = train_y)
      test_pool  <- catboost.load_pool(data = test_df)
      
      # Entrenar modelo
      params <- list(loss_function = "RMSE",
                     iterations = 300,
                     depth = 6,
                     learning_rate = 0.05,
                     logging_level = "Silent",
                     train_dir = tempfile())
      
      model_ctb <- catboost.train(train_pool, params = params)
      preds <- catboost.predict(model_ctb, test_pool)
      
      return(pmax(0, as.numeric(preds)))
      
    }, error = function(e) {
      message("⚠️ CatBoost falló: ", conditionMessage(e), " -> fallback a GWR")
      return(predecir_gwr(datos_dia, grid_df))
    })
    return(out)
    
  } else {
    # Modelo no reconocido, fallback a GWR
    warning("Modelo '", modelo, "' no reconocido. Usando GWR como respaldo.")
    return(predecir_gwr(datos_dia, grid_df))
  }
}




predecir_prcp <- function(modelo, datos_dia, grid_df, predictors) {
  
  # 1️⃣ Preparar datos de entrenamiento
  train <- datos_dia[, c(predictors, "prcp")]
  train <- train[complete.cases(train), ]
  
  # 2️⃣ Validación temprana: pocas observaciones
  if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
    return(predecir_gwr(datos_dia, grid_df))
  }
  
  # ---------------------------
  # 3️⃣ Modelo según selección
  # ---------------------------
  
  if (modelo == "rf") {
    # Random Forest
    rf <- randomForest(prcp ~ ., data = train, ntree = 100)
    preds <- predict(rf, newdata = grid_df)
    return(pmax(0, preds))
    
  } else if (modelo == "lgbm") {
    # LightGBM
    train_x <- as.matrix(train[, predictors]); train_y <- train$prcp
    dtrain <- lgb.Dataset(data = train_x, label = train_y)
    params <- list(objective = "regression", metric = "rmse", learning_rate = 0.05,
                   num_leaves = 31, max_depth = 6, min_data_in_leaf = 3, verbosity = -1)
    model <- lgb.train(params, dtrain, nrounds = 300, verbose = -1)
    test_x <- as.matrix(grid_df[, predictors]); preds <- predict(model, test_x)
    return(pmax(0, preds))
    
  } else if (modelo == "xgb") {
    # XGBoost
    train_x <- as.matrix(train[, predictors]); train_y <- train$prcp
    dtrain <- xgb.DMatrix(data = train_x, label = train_y)
    params <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 6,
                   subsample = 0.8, colsample_bytree = 0.8)
    model <- xgb.train(params, dtrain, nrounds = 300, verbose = 0)
    test_x <- as.matrix(grid_df[, predictors]); preds <- predict(model, test_x)
    return(pmax(0, preds))
    
  } else if (modelo == "ctb") {
    # CatBoost con manejo robusto de errores
    out <- tryCatch({
      # Coerción a data.frame numérico
      train_df <- as.data.frame(train[, predictors], stringsAsFactors = FALSE)
      train_df[] <- lapply(train_df, function(x) as.numeric(as.character(x)))
      train_y  <- as.numeric(train$prcp)
      
      test_df <- as.data.frame(grid_df[, predictors], stringsAsFactors = FALSE)
      test_df[] <- lapply(test_df, function(x) as.numeric(as.character(x)))
      
      # Rellenar NAs en test con la media de train
      for (col in names(test_df)) {
        if (all(is.na(test_df[[col]]))) {
          test_df[[col]] <- mean(train_df[[col]], na.rm = TRUE)
        } else {
          nas <- which(is.na(test_df[[col]]))
          if (length(nas) > 0) test_df[[col]][nas] <- mean(train_df[[col]], na.rm = TRUE)
        }
      }
      
      # Crear Pools
      train_pool <- catboost.load_pool(data = train_df, label = train_y)
      test_pool  <- catboost.load_pool(data = test_df)
      
      # Entrenar modelo
      params <- list(loss_function = "RMSE",
                     iterations = 300,
                     depth = 6,
                     learning_rate = 0.05,
                     logging_level = "Silent",
                     train_dir = tempfile())
      
      model_ctb <- catboost.train(train_pool, params = params)
      preds <- catboost.predict(model_ctb, test_pool)
      
      return(pmax(0, as.numeric(preds)))
      
    }, error = function(e) {
      message("⚠️ CatBoost falló: ", conditionMessage(e), " -> fallback a GWR")
      return(predecir_gwr(datos_dia, grid_df))
    })
    
    return(out)
    
  } else {
    # Modelo no reconocido, fallback a GWR
    warning("Modelo '", modelo, "' no reconocido. Usando GWR como respaldo.")
    return(predecir_gwr(datos_dia, grid_df))
  }
}


