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
# Función GWRK (GWR + kriging de residuos)
# ---------------------------
predecir_gwrk <- function(datos_dia, grid_df) {
  tryCatch({
    # 1️⃣ Preparar datos de estaciones
    estaciones <- datos_dia[, c("lon", "lat", "prcp", "dem", "twi", "slope")]
    coordinates(estaciones) <- ~lon + lat
    proj4string(estaciones) <- CRS("+proj=longlat +datum=WGS84")
    
    # 2️⃣ Preparar grilla
    grid_sp <- grid_df
    coordinates(grid_sp) <- ~lon + lat
    proj4string(grid_sp) <- CRS("+proj=longlat +datum=WGS84")
    
    # 3️⃣ Selección de ancho de banda y GWR
    bw <- suppressWarnings(
      gwr.sel(prcp ~ dem + twi + slope,
              data = estaciones,
              coords = coordinates(estaciones),
              adaptive = TRUE)
    )
    
    pred_gwr <- gwr.predict(prcp ~ dem + twi + slope, 
                            data = estaciones,
                            predictdata = grid_sp, 
                            bw = bw, 
                            kernel = "bisquare",
                            adaptive = TRUE)
    
    # 4️⃣ Calcular residuos en estaciones
    gwr_fit <- gwr(prcp ~ dem + twi + slope, 
                   data = estaciones, 
                   coords = coordinates(estaciones),
                   bw = bw,
                   kernel = "bisquare",
                   adaptive = TRUE)
    estaciones$resid <- gwr_fit$SDF$residual
    
    # 5️⃣ Ajustar variograma y kriging
    vgm_mod <- variogram(resid ~ 1, estaciones)
    vgm_fit <- fit.variogram(vgm_mod, vgm(c("Sph","Exp","Gau")))
    
    krig_res <- krige(resid ~ 1, estaciones, grid_sp, model = vgm_fit)
    
    # 6️⃣ Predicción final = GWR + residuos krigeados
    pred_final <- pred_gwr$SDF$pred + krig_res$var1.pred
    
    return(pmax(0, pred_final))
    
  }, error = function(e) {
    message("⚠️ GWRK falló, usando IDW. Error: ", conditionMessage(e))
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
    return(predecir_gwrk(datos_dia, grid_df)) # predecir_gwrk
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
      return(predecir_gwrk(datos_dia, grid_df))
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
      return(predecir_gwrk(datos_dia, grid_df))
    })
    return(out)
    
  } else {
    # Modelo no reconocido, fallback a GWR
    warning("Modelo '", modelo, "' no reconocido. Usando GWR como respaldo.")
    return(predecir_gwrk(datos_dia, grid_df)) # predecir_gwrk
  }
}

# ---------------------------
# 7️⃣ Función combinada ML + GWR + IDW para zonas con pocas estaciones
# ---------------------------
predecir_prcp_combinado <- function(modelo, datos_dia, grid_df, predictors, umbral_gwr = 5, umbral_dist = 0.5, peso_ml = 0.7) {
  
  # Si hay pocas estaciones, usar solo GWR
  if (nrow(datos_dia) < umbral_gwr || length(unique(datos_dia$prcp)) < 2) {
    return(predecir_gwrk(datos_dia, grid_df)) # predecir_gwrk
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
    preds_gwr <- predecir_gwrk(datos_dia, subgrid) #predecir_gwrk
    
    # Mezclar ML y GWR ponderadamente
    preds_ml[idx_gwr] <- peso_ml*preds_ml[idx_gwr] + (1 - peso_ml)*preds_gwr
  }
  
  return(preds_ml)
}

# ---------------------------
# 8️⃣ Configuración
# ---------------------------
predictors <- c("lon","lat","dem","twi","sin","cos","slope","tmax","tmin","precland")
modelo_prcp <- "rfsi"  # "rfsi", "lgbm", "xgb", "ctb"

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
output_dir <- "D:/QSWAT/Output"
if (!dir.exists(output_dir)) dir.create(output_dir)

for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    names(s) <- fechas_utilizadas
    file_name <- file.path(output_dir, paste0("huallagagwrk_", modelo_prcp, "_", anio, ".tif"))
    writeRaster(s, filename = file_name, format = "GTiff", overwrite = TRUE)
    message("Guardado: ", file_name)
  }
}

library(raster)

# Cargar el raster multicapas de un año
r <- stack("D:/QSWAT/Output/huallagagwrk_rfsi_2000.tif")

plot(r)




library(sf); library(sp); library(raster); library(dplyr); library(FNN)
library(gstat); library(randomForest); library(lightgbm); library(xgboost)
library(catboost); library(GWmodel); library(meteo); library(parallel)
library(doParallel)

# --- Datos ---
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_huallaga.csv")
data$time <- as.Date(data$time, "%m/%d/%Y")
cuenca <- st_read("D:/S/Serbia1km/Interpolation/Titicaca_border/watershed.shp") |> 
  st_transform(4326)

# --- Grilla ---
bbox <- st_bbox(cuenca)
grid_df <- expand.grid(lon = seq(bbox["xmin"], bbox["xmax"], 0.05),
                       lat = seq(bbox["ymin"], bbox["ymax"], 0.05)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_join(cuenca, join = st_within, left = FALSE)
coords <- st_coordinates(grid_df)
grid_static_df <- data.frame(lon = coords[,1], lat = coords[,2])

# --- Extraer rásters estáticos ---
for (v in c("dem","twi","sin","cos","slope")) {
  r <- raster(paste0("D:/S/Serbia1km/Interpolation/Huallaga/", v, ".tif"))
  grid_static_df[[v]] <- raster::extract(r, grid_static_df[,c("lon","lat")])
}
grid_static_df[is.na(grid_static_df)] <- 0

# --- Función GWRK ---
predecir_gwrk <- function(datos, grid) {
  tryCatch({
    est <- datos[,c("lon","lat","prcp","dem","twi","slope")]
    coordinates(est) <- ~lon+lat; proj4string(est) <- CRS("+proj=longlat +datum=WGS84")
    grid_sp <- grid; coordinates(grid_sp) <- ~lon+lat; proj4string(grid_sp) <- CRS("+proj=longlat +datum=WGS84")
    
    bw <- gwr.sel(prcp ~ dem+twi+slope, data=est, coords=coordinates(est), adaptive=TRUE)
    pred_gwr <- gwr.predict(prcp ~ dem+twi+slope, data=est, predictdata=grid_sp, bw=bw, kernel="bisquare", adaptive=TRUE)
    fit <- gwr(prcp ~ dem+twi+slope, data=est, coords=coordinates(est), bw=bw, kernel="bisquare", adaptive=TRUE)
    est$resid <- fit$SDF$residual
    
    vgm_fit <- fit.variogram(variogram(resid~1, est), vgm(c("Sph","Exp","Gau")))
    krig_res <- krige(resid~1, est, grid_sp, vgm_fit)
    
    pmax(0, pred_gwr$SDF$pred + krig_res$var1.pred)
  }, error=function(e) {
    idw(prcp~1, est, grid_sp, idp=2)$var1.pred
  })
}

# --- Función ML + fallback GWRK ---
predecir_prcp <- function(modelo, datos, grid, preds) {
  train <- datos[,c(preds,"prcp")] |> na.omit()
  if (nrow(train)<5 || length(unique(train$prcp))<2) return(predecir_gwrk(datos, grid))
  
  if (modelo=="rfsi") {
    tryCatch({
      rfsi_mod <- meteo::rfsi(prcp~., st_as_sf(train,coords=c("lon","lat"),crs=4326),
                              st_as_sf(grid,coords=c("lon","lat"),crs=4326),
                              n.obs=3, ntree=150)
      pmax(0, rfsi_mod$pred)
    }, error=function(e) predecir_gwrk(datos, grid))
    
  } else if (modelo=="xgb") {
    model <- xgb.train(list(objective="reg:squarederror", eta=0.05, max_depth=6),
                       xgb.DMatrix(as.matrix(train[,preds]), label=train$prcp),
                       nrounds=300, verbose=0)
    pmax(0, predict(model, as.matrix(grid[,preds])))
    
  } else if (modelo=="lgbm") {
    model <- lgb.train(list(objective="regression", metric="rmse", learning_rate=0.05,
                            num_leaves=31, max_depth=6, min_data_in_leaf=3),
                       lgb.Dataset(as.matrix(train[,preds]), label=train$prcp), 300)
    pmax(0, predict(model, as.matrix(grid[,preds])))
    
  } else if (modelo=="ctb") {
    pool <- catboost.load_pool(train[,preds], label=train$prcp)
    model <- catboost.train(pool, params=list(loss_function="RMSE", iterations=300, depth=6, learning_rate=0.05, logging_level="Silent"))
    pmax(0, as.numeric(catboost.predict(model, catboost.load_pool(grid[,preds]))))
    
  } else predecir_gwrk(datos, grid)
}

# --- Configuración ---
predictors <- c("lon","lat","dem","twi","sin","cos","slope","tmax","tmin","precland")
modelo <- "rfsi"   # "rfsi","xgb","lgbm","ctb"

# --- Loop paralelo ---
cl <- makeCluster(detectCores()-1); registerDoParallel(cl)
fechas <- sort(unique(data$time))
resultados <- foreach(f=fechas, .packages=c("dplyr","sp","gstat","randomForest","lightgbm","xgboost","catboost","GWmodel","raster","FNN","meteo")) %dopar% {
  dia <- filter(data, time==f); grid_day <- grid_static_df
  
  # Interpolar tmax,tmin,precland por IDW
  for (v in c("tmax","tmin","precland")) {
    df_sp <- dia[,c("lon","lat",v)]; coordinates(df_sp)<-~lon+lat; proj4string(df_sp)<-CRS("+proj=longlat +datum=WGS84")
    grid_day[[v]] <- idw(as.formula(paste(v,"~1")), df_sp, grid_day, idp=2)$var1.pred
  }
  
  grid_day$prcp <- predecir_prcp(modelo, dia, grid_day, predictors)
  raster_prcp <- rasterFromXYZ(grid_day[,c("lon","lat","prcp")], res=0.05, crs=CRS("+proj=longlat +datum=WGS84"))
  list(fecha=as.character(f), raster=raster_prcp)
}
stopCluster(cl)

