
###############################################################################################
#                                lgbm, xgb
#############################################################################################

library(dplyr)
library(sf)
library(sp)
library(gstat)
library(lightgbm)
library(xgboost)
library(randomForest)
library(raster)
library(lubridate)
library(progress)
library(FNN)
library(foreach)
library(doParallel)

# --- Cargar datos ---
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_puno.csv")
data$time <- as.Date(data$time, format = "%m/%d/%Y")

# --- Leer cuenca y crear grilla dentro ---
cuenca <- st_read("D:/S/Serbia1km/Interpolation/Titicaca_border/Titicaca.shp")
cuenca <- st_transform(cuenca, crs = 4326)
bbox <- st_bbox(cuenca)
lon_seq <- seq(bbox["xmin"], bbox["xmax"], by = 0.05)
lat_seq <- seq(bbox["ymin"], bbox["ymax"], by = 0.05)
grid <- expand.grid(lon = lon_seq, lat = lat_seq)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
grid_dentro <- st_join(grid_sf, cuenca, join = st_within, left = FALSE)
coords <- st_coordinates(grid_dentro)
grid <- data.frame(lon = coords[, 1], lat = coords[, 2])

# --- Asignar variables estáticas ---
estaciones_sf <- st_as_sf(
  dplyr::select(data, lon, lat, dem, twi, sin, cos) %>% distinct(),
  coords = c("lon", "lat"), crs = 4326
)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

est_coords <- st_coordinates(estaciones_sf)
grid_coords <- st_coordinates(grid_sf)

nn_idx <- FNN::get.knnx(est_coords, grid_coords, k = 1)$nn.index[,1]
grid$dem <- estaciones_sf$dem[nn_idx]
grid$twi <- estaciones_sf$twi[nn_idx]
grid$sin <- estaciones_sf$sin[nn_idx]
grid$cos <- estaciones_sf$cos[nn_idx]

# Convertir grid a SpatialPoints para IDW
coordinates(grid) <- ~lon + lat
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")

# --- Preparar paralelización ---
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

fechas <- sort(unique(data$time))

# --- Modelo a usar ---
modelo_prcp <- "lgbm"   # cambiar a "rf", "lgbm" o "xgb"
predictors <- c("lon", "lat", "dem", "twi", "tmax", "tmin", "sin", "cos")

# --- Paralelizar ---
resultados_list <- foreach(fecha = fechas,
                           .packages = c("dplyr", "sp", "gstat",
                                         "lightgbm", "xgboost", "randomForest",
                                         "raster", "lubridate")) %dopar% {
                                           datos_dia <- dplyr::filter(data, time == fecha)
                                           if (nrow(datos_dia) < 5) return(NULL)
                                           
                                           grid_temp <- as.data.frame(grid)
                                           
                                           # --- Interpolación IDW para Tmax y Tmin ---
                                           for (var in c("tmax", "tmin")) {
                                             df_sp <- datos_dia[, c("lon", "lat", var)]
                                             coordinates(df_sp) <- ~lon + lat
                                             proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
                                             
                                             idw_out <- idw(as.formula(paste(var, "~ 1")), df_sp, newdata = grid, idp = 2)
                                             grid_temp[[var]] <- idw_out$var1.pred
                                           }
                                           
                                           # ==========================================
                                           #   MODELADO DE PRECIPITACIÓN (RF, LGBM, XGB)
                                           # ==========================================
                                           
                                           if (modelo_prcp == "rf") {
                                             rf <- randomForest(prcp ~ lon + lat + dem + twi + tmax + tmin + sin + cos,
                                                                data = datos_dia, ntree = 200)
                                             preds <- predict(rf, newdata = grid_temp)
                                             grid_temp$prcp <- pmax(0, preds)   # nunca negativos
                                             
                                           } else if (modelo_prcp == "lgbm") {
                                             train <- dplyr::select(datos_dia, dplyr::all_of(c(predictors, "prcp")))
                                             train <- train[complete.cases(train), ]
                                             n <- nrow(train)
                                             
                                             if (n < 5 || length(unique(train$prcp)) < 2) {
                                               df_sp_p <- datos_dia[, c("lon","lat","prcp")]
                                               coordinates(df_sp_p) <- ~lon + lat
                                               proj4string(df_sp_p) <- CRS("+proj=longlat +datum=WGS84")
                                               idw_p <- idw(prcp ~ 1, df_sp_p, newdata = grid, idp = 2)
                                               grid_temp$prcp <- pmax(0, idw_p$var1.pred)
                                             } else {
                                               train_x <- as.matrix(dplyr::select(train, dplyr::all_of(predictors)))
                                               train_y <- log1p(train$prcp)
                                               
                                               dtrain <- lightgbm::lgb.Dataset(data = train_x, label = train_y)
                                               
                                               params <- list(
                                                 objective = "regression",
                                                 metric = "rmse",
                                                 boosting = "gbdt",
                                                 learning_rate = 0.05,
                                                 num_leaves = min(31, max(4, floor(n/2))),
                                                 max_depth = 6,
                                                 min_data_in_leaf = max(1, floor(n/10)),
                                                 feature_fraction = 1.0,
                                                 bagging_fraction = 1.0,
                                                 bagging_freq = 0,
                                                 verbosity = -1
                                               )
                                               
                                               lgb_model <- lightgbm::lgb.train(
                                                 params = params,
                                                 data = dtrain,
                                                 nrounds = 300,
                                                 verbose = -1
                                               )
                                               
                                               test_x <- as.matrix(dplyr::select(grid_temp, dplyr::all_of(predictors)))
                                               preds_log <- predict(lgb_model, test_x)
                                               preds <- expm1(preds_log)
                                               preds[preds < 0] <- 0   # nunca negativos
                                               grid_temp$prcp <- preds
                                             }
                                             
                                           } else if (modelo_prcp == "xgb") {
                                             train <- dplyr::select(datos_dia, dplyr::all_of(c(predictors, "prcp")))
                                             train <- train[complete.cases(train), ]
                                             n <- nrow(train)
                                             
                                             if (n < 5 || length(unique(train$prcp)) < 2) {
                                               df_sp_p <- datos_dia[, c("lon","lat","prcp")]
                                               coordinates(df_sp_p) <- ~lon + lat
                                               proj4string(df_sp_p) <- CRS("+proj=longlat +datum=WGS84")
                                               idw_p <- idw(prcp ~ 1, df_sp_p, newdata = grid, idp = 2)
                                               grid_temp$prcp <- pmax(0, idw_p$var1.pred)
                                             } else {
                                               train_x <- as.matrix(dplyr::select(train, dplyr::all_of(predictors)))
                                               train_y <- log1p(train$prcp)
                                               
                                               dtrain <- xgboost::xgb.DMatrix(data = train_x, label = train_y)
                                               
                                               params <- list(
                                                 objective = "reg:squarederror",
                                                 eta = 0.05,
                                                 max_depth = 6,
                                                 subsample = 0.9,
                                                 colsample_bytree = 0.9,
                                                 verbosity = 0
                                               )
                                               
                                               xgb_model <- xgboost::xgb.train(
                                                 params = params,
                                                 data = dtrain,
                                                 nrounds = 300,
                                                 verbose = 0
                                               )
                                               
                                               test_x <- as.matrix(dplyr::select(grid_temp, dplyr::all_of(predictors)))
                                               preds_log <- predict(xgb_model, test_x)
                                               preds <- expm1(preds_log)
                                               preds[preds < 0] <- 0   # nunca negativos
                                               grid_temp$prcp <- preds
                                             }
                                           }
                                           
                                           # --- Convertir a raster ---
                                           coordinates(grid_temp) <- ~lon + lat
                                           gridded(grid_temp) <- TRUE
                                           raster_prcp <- raster(grid_temp["prcp"])
                                           
                                           list(fecha = as.character(fecha), raster = raster_prcp)
                                         }

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

stopCluster(cl)

# --- Guardar resultados ---
output_dir <- "D:/S/Serbia1km/Interpolation"
if (!dir.exists(output_dir)) dir.create(output_dir)

for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    names(s) <- fechas_utilizadas
    
    file_name <- file.path(output_dir, paste0("titicacaLGBM1_", anio, ".tif"))
    writeRaster(s, filename = file_name, format = "GTiff", overwrite = TRUE)
    
    message("Guardado: ", file_name)
  } else {
    warning("No se guardó el año ", anio, ": capas = ", nlayers(s),
            ", fechas = ", length(fechas_utilizadas))
  }
}

############################################################################################
#                    lgbm, rf, xgb with valitation   okkkkkkkkkkkkkkk
###########################################################################################

## --- Librerías ---
library(rgdal)
library(sf)
library(sp)
library(gstat)
library(randomForest)
library(lightgbm)
library(xgboost)
library(raster)
library(lubridate)
library(progress)
library(FNN)
library(dplyr)
library(foreach)
library(doParallel)

# --- Cargar datos ---
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_puno.csv")
data$time <- as.Date(data$time, format = "%m/%d/%Y")

# --- Leer cuenca y crear grilla dentro ---
cuenca <- st_read("D:/S/Serbia1km/Interpolation/Titicaca_border/Titicaca.shp")
cuenca <- st_transform(cuenca, crs = 4326)
bbox <- st_bbox(cuenca)
lon_seq <- seq(bbox["xmin"], bbox["xmax"], by = 0.05)
lat_seq <- seq(bbox["ymin"], bbox["ymax"], by = 0.05)
grid <- expand.grid(lon = lon_seq, lat = lat_seq)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
grid_dentro <- st_join(grid_sf, cuenca, join = st_within, left = FALSE)
coords <- st_coordinates(grid_dentro)
grid <- data.frame(lon = coords[, 1], lat = coords[, 2])

# --- Asignar variables estáticas (dem, twi, sin, cos) ---
estaciones_sf <- st_as_sf(
  dplyr::select(data, lon, lat, dem, twi, sin, cos) %>% distinct(),
  coords = c("lon", "lat"), crs = 4326
)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

est_coords <- st_coordinates(estaciones_sf)
grid_coords <- st_coordinates(grid_sf)
nn_idx <- get.knnx(est_coords, grid_coords, k = 1)$nn.index[,1]

grid$dem <- estaciones_sf$dem[nn_idx]
grid$twi <- estaciones_sf$twi[nn_idx]
grid$sin <- estaciones_sf$sin[nn_idx]
grid$cos <- estaciones_sf$cos[nn_idx]

# Convertir grid a SpatialPoints
coordinates(grid) <- ~lon + lat
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")

# --- Función auxiliar para prcp ---
predecir_prcp <- function(modelo, datos_dia, grid_temp, predictors) {
  if (modelo == "rf") {
    rf <- randomForest(prcp ~ lon + lat + dem + twi + tmax + tmin + sin + cos,
                       data = datos_dia, ntree = 100)
    preds <- predict(rf, newdata = grid_temp)
    return(pmax(0, preds))   # no negativos
    
  } else if (modelo == "lgbm") {
    train <- dplyr::select(datos_dia, dplyr::all_of(c(predictors, "prcp")))
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      df_sp <- datos_dia[, c("lon","lat","prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid, idp = 2)
      return(pmax(0, idw_p$var1.pred))
    }
    train_x <- as.matrix(dplyr::select(train, dplyr::all_of(predictors)))
    train_y <- train$prcp
    dtrain <- lgb.Dataset(data = train_x, label = train_y)
    params <- list(objective="regression", metric="rmse",
                   learning_rate=0.05, num_leaves=31,
                   max_depth=6, min_data_in_leaf=3, verbosity=-1)
    model <- lgb.train(params=params, data=dtrain, nrounds=300, verbose=-1)
    test_x <- as.matrix(dplyr::select(grid_temp, dplyr::all_of(predictors)))
    preds <- predict(model, test_x)
    return(pmax(0, preds))   # no negativos
    
  } else if (modelo == "xgb") {
    train <- dplyr::select(datos_dia, dplyr::all_of(c(predictors, "prcp")))
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      df_sp <- datos_dia[, c("lon","lat","prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid, idp = 2)
      return(pmax(0, idw_p$var1.pred))
    }
    train_x <- as.matrix(dplyr::select(train, dplyr::all_of(predictors)))
    train_y <- train$prcp
    dtrain <- xgb.DMatrix(data = train_x, label = train_y)
    params <- list(objective="reg:squarederror", eta=0.05, max_depth=6,
                   subsample=0.8, colsample_bytree=0.8)
    model <- xgb.train(params=params, data=dtrain, nrounds=300, verbose=0)
    test_x <- as.matrix(dplyr::select(grid_temp, dplyr::all_of(predictors)))
    preds <- predict(model, test_x)
    return(pmax(0, preds))   # no negativos
  }
}

# --- Configuración ---
predictors <- c("lon","lat","dem","twi","tmax","tmin","sin","cos")

# Cambia aquí el modelo que quieres usar: "rf", "lgbm", "xgb"
modelo_prcp <- "xgb"

# --- Paralelización ---
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

fechas <- sort(unique(data$time))

# --- Interpolación y predicción ---
resultados_list <- foreach(
  fecha = fechas,
  .packages = c("dplyr","sp","gstat","randomForest","lightgbm","xgboost",
                "raster","lubridate")
) %dopar% {
  
  datos_dia <- dplyr::filter(data, time == fecha)
  if (nrow(datos_dia) < 5) return(NULL)
  
  grid_temp <- as.data.frame(grid)
  
  # Interpolar tmax y tmin
  for (var in c("tmax", "tmin")) {
    df_sp <- datos_dia[, c("lon", "lat", var)]
    coordinates(df_sp) <- ~lon + lat
    proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
    idw_out <- idw(as.formula(paste(var, "~ 1")), df_sp, newdata = grid, idp = 2)
    grid_temp[[var]] <- idw_out$var1.pred
  }
  
  # Predecir prcp
  grid_temp$prcp <- predecir_prcp(modelo_prcp, datos_dia, grid_temp, predictors)
  
  # Convertir a raster
  coordinates(grid_temp) <- ~lon + lat
  gridded(grid_temp) <- TRUE
  raster_prcp <- raster(grid_temp["prcp"])
  
  list(fecha = as.character(fecha), raster = raster_prcp)
}

stopCluster(cl)

# --- Organizar resultados por año y guardar ---
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

output_dir <- "D:/S/Serbia1km/Interpolation"
if (!dir.exists(output_dir)) dir.create(output_dir)

for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    names(s) <- fechas_utilizadas
    file_name <- file.path(output_dir, paste0("titicaca_xgb", modelo_prcp, "_", anio, ".tif"))
    writeRaster(s, filename = file_name, format = "GTiff", overwrite = TRUE)
    message("Guardado: ", file_name)
  }
}

###################################################################################
#             validation
###################################################################################

# --- Función de evaluación con leave-one-station-out ---
evaluar_modelo <- function(modelo, datos_dia, predictors) {
  if (nrow(datos_dia) < 5) return(NULL)  # muy pocos datos
  
  errores <- c()
  for (i in 1:nrow(datos_dia)) {
    # Separar estación de validación
    test <- datos_dia[i, ]
    train <- datos_dia[-i, ]
    
    # Skip si no hay variación en train
    if (length(unique(train$prcp)) < 2) next
    
    grid_temp <- as.data.frame(test) # solo con la estación test
    # Entrenar y predecir
    pred <- tryCatch({
      predecir_prcp(modelo, train, grid_temp, predictors)
    }, error = function(e) NA)
    
    if (!is.na(pred)) {
      errores <- c(errores, test$prcp - pred)
    }
  }
  
  if (length(errores) == 0) return(NULL)
  
  rmse <- sqrt(mean(errores^2, na.rm=TRUE))
  mae  <- mean(abs(errores), na.rm=TRUE)
  bias <- mean(errores, na.rm=TRUE)
  
  return(data.frame(modelo=modelo, RMSE=rmse, MAE=mae, Bias=bias))
}

# --- Validación cruzada en todas las fechas ---
modelos <- c("rf", "lgbm", "xgb")
resultados_cv <- list()

for (fecha in sort(unique(data$time))) {
  datos_dia <- dplyr::filter(data, time == fecha)
  if (nrow(datos_dia) < 5) next
  
  res_dia <- lapply(modelos, evaluar_modelo, datos_dia=datos_dia, predictors=predictors)
  res_dia <- do.call(rbind, res_dia)
  res_dia$fecha <- fecha
  resultados_cv[[as.character(fecha)]] <- res_dia
}

resultados_cv <- do.call(rbind, resultados_cv)

# --- Resumen por modelo ---
resumen_modelos <- resultados_cv %>%
  group_by(modelo) %>%
  summarise(RMSE = mean(RMSE, na.rm=TRUE),
            MAE  = mean(MAE, na.rm=TRUE),
            Bias = mean(Bias, na.rm=TRUE),
            .groups="drop")

print(resumen_modelos)

###################################################################################
#               model best
##################################################################################

# --- Librerías ---
library(dplyr)
library(sf)
library(sp)
library(gstat)
library(randomForest)
library(lightgbm)
library(xgboost)
library(raster)
library(lubridate)
library(FNN)
library(foreach)
library(doParallel)

# --- Cargar datos ---
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_puno.csv")
data$time <- as.Date(data$time, format = "%m/%d/%Y")

# --- Leer cuenca y crear grilla ---
cuenca <- st_read("D:/S/Serbia1km/Interpolation/Titicaca_border/Titicaca.shp")
cuenca <- st_transform(cuenca, crs = 4326)
bbox <- st_bbox(cuenca)
lon_seq <- seq(bbox["xmin"], bbox["xmax"], by = 0.05)
lat_seq <- seq(bbox["ymin"], bbox["ymax"], by = 0.05)
grid <- expand.grid(lon = lon_seq, lat = lat_seq)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
grid_dentro <- st_join(grid_sf, cuenca, join = st_within, left = FALSE)
coords <- st_coordinates(grid_dentro)
grid <- data.frame(lon = coords[, 1], lat = coords[, 2])

# --- Asignar variables estáticas ---
estaciones_sf <- st_as_sf(data %>%
                            select(lon, lat, dem, twi, sin, cos) %>%
                            distinct(),
                          coords = c("lon", "lat"), crs = 4326)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

est_coords <- st_coordinates(estaciones_sf)
grid_coords <- st_coordinates(grid_sf)
nn_idx <- get.knnx(est_coords, grid_coords, k = 1)$nn.index[,1]

grid$dem <- estaciones_sf$dem[nn_idx]
grid$twi <- estaciones_sf$twi[nn_idx]
grid$sin <- estaciones_sf$sin[nn_idx]
grid$cos <- estaciones_sf$cos[nn_idx]

coordinates(grid) <- ~lon + lat
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")
grid_sp <- grid
grid_df <- as.data.frame(grid_sp)

predictors <- c("lon", "lat", "dem", "twi", "tmax", "tmin", "sin", "cos")

# --- Función genérica de predicción ---
predecir_prcp <- function(modelo, train, grid_temp, predictors) {
  if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
    df_sp_p <- train[, c("lon","lat","prcp")]
    coordinates(df_sp_p) <- ~lon + lat
    proj4string(df_sp_p) <- CRS("+proj=longlat +datum=WGS84")
    idw_p <- idw(prcp ~ 1, df_sp_p, newdata = grid_sp, idp = 2)
    return(pmax(0, idw_p$var1.pred))   # <- aseguramos positivo
  }
  
  if (modelo == "rf") {
    rf <- randomForest(prcp ~ ., data = train[, c(predictors,"prcp")], ntree = 100)
    preds <- predict(rf, newdata = grid_temp)
    return(pmax(0, preds))   # <- aseguramos positivo
  }
  
  if (modelo == "lgbm") {
    train_x <- as.matrix(train[, predictors])
    train_y <- log1p(train$prcp)   # transformación log
    dtrain <- lightgbm::lgb.Dataset(data = train_x, label = train_y)
    lgb_model <- lightgbm::lgb.train(
      params = list(objective = "regression", metric = "rmse", learning_rate = 0.05,
                    num_leaves = min(31, max(4, floor(nrow(train)/2))), max_depth = 6,
                    min_data_in_leaf = max(1, floor(nrow(train)/10))),
      data = dtrain, nrounds = 300, verbose = -1
    )
    preds_log <- predict(lgb_model, as.matrix(grid_temp[, predictors]))
    preds <- expm1(preds_log)  # inversa de log1p
    return(pmax(0, preds))     # <- aseguramos positivo
  }
  
  if (modelo == "xgb") {
    dtrain <- xgboost::xgb.DMatrix(data = as.matrix(train[, predictors]),
                                   label = log1p(train$prcp))
    xgb_model <- xgboost::xgb.train(
      params = list(objective = "reg:squarederror", eta = 0.05, max_depth = 6),
      data = dtrain, nrounds = 300, verbose = 0
    )
    preds_log <- predict(xgb_model, as.matrix(grid_temp[, predictors]))
    preds <- expm1(preds_log)  # inversa de log1p
    return(pmax(0, preds))     # <- aseguramos positivo
  }
}

# --- Validación cruzada leave-one-station-out ---
evaluar_modelo <- function(modelo, datos_dia, predictors) {
  errores <- c()
  for (i in 1:nrow(datos_dia)) {
    test <- datos_dia[i, ]
    train <- datos_dia[-i, ]
    if (length(unique(train$prcp)) < 2) next
    grid_temp <- as.data.frame(test)
    pred <- tryCatch(predecir_prcp(modelo, train, grid_temp, predictors), error=function(e) NA)
    if (!is.na(pred)) errores <- c(errores, test$prcp - pred)
  }
  if (length(errores) == 0) return(NULL)
  return(data.frame(modelo=modelo,
                    RMSE=sqrt(mean(errores^2, na.rm=TRUE)),
                    MAE=mean(abs(errores), na.rm=TRUE),
                    Bias=mean(errores, na.rm=TRUE)))
}

modelos <- c("rf","lgbm","xgb")
resultados_cv <- list()

for (fecha in sort(unique(data$time))) {
  datos_dia <- dplyr::filter(data, time == fecha)
  if (nrow(datos_dia) < 5) next
  res_dia <- lapply(modelos, evaluar_modelo, datos_dia=datos_dia, predictors=predictors)
  res_dia <- do.call(rbind, res_dia)
  res_dia$fecha <- fecha
  resultados_cv[[as.character(fecha)]] <- res_dia
}

resultados_cv <- do.call(rbind, resultados_cv)
resumen_modelos <- resultados_cv %>%
  group_by(modelo) %>%
  summarise(RMSE=mean(RMSE,na.rm=TRUE),
            MAE=mean(MAE,na.rm=TRUE),
            Bias=mean(Bias,na.rm=TRUE), .groups="drop")
print(resumen_modelos)

# --- Selección del mejor modelo ---
mejor_modelo <- resumen_modelos %>%
  arrange(RMSE) %>%
  slice(1) %>%
  pull(modelo)
message("Mejor modelo seleccionado: ", mejor_modelo)

# --- Paralelización para interpolación final ---
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

fechas <- sort(unique(data$time))

resultados_list <- foreach(fecha = fechas,
                           .packages=c("dplyr","sp","gstat","randomForest","lightgbm","xgboost","raster","lubridate")) %dopar% {
                             datos_dia <- dplyr::filter(data, time == fecha)
                             if (nrow(datos_dia) < 5) return(NULL)
                             
                             grid_temp <- as.data.frame(grid_df)
                             
                             # Interpolar tmax y tmin con IDW
                             for (var in c("tmax","tmin")) {
                               df_sp <- datos_dia[, c("lon","lat",var)]
                               coordinates(df_sp) <- ~lon + lat
                               proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
                               idw_out <- idw(as.formula(paste(var,"~1")), df_sp, newdata=grid_sp, idp=2)
                               grid_temp[[var]] <- idw_out$var1.pred
                             }
                             
                             # Predecir prcp con el mejor modelo
                             grid_temp$prcp <- predecir_prcp(mejor_modelo, datos_dia, grid_temp, predictors)
                             
                             coordinates(grid_temp) <- ~lon+lat
                             gridded(grid_temp) <- TRUE
                             raster_prcp <- raster(grid_temp["prcp"])
                             
                             list(fecha=as.character(fecha), raster=raster_prcp)
                           }

stopCluster(cl)

# --- Guardar resultados ---
output_dir <- "D:/S/Serbia1km/Interpolation"
if (!dir.exists(output_dir)) dir.create(output_dir)

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

for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    names(s) <- fechas_utilizadas
    file_name <- file.path(output_dir, paste0("titicaca_best_", mejor_modelo, "_", anio, ".tif"))
    writeRaster(s, filename=file_name, format="GTiff", overwrite=TRUE)
    message("Guardado: ", file_name)
  }
}

#####################################################################################
#                    lgbm okkkkkkk of millones de datos
#####################################################################################

library(dplyr)
library(sf)
library(sp)
library(gstat)
library(lightgbm)
library(raster)
library(lubridate)
library(FNN)
library(foreach)
library(doParallel)

# --- Cargar datos ---
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_puno.csv")
data$time <- as.Date(data$time, format = "%m/%d/%Y")

# --- Leer cuenca y crear grilla dentro ---
cuenca <- st_read("D:/S/Serbia1km/Interpolation/Titicaca_border/Titicaca.shp")
cuenca <- st_transform(cuenca, crs = 4326)
bbox <- st_bbox(cuenca)
lon_seq <- seq(bbox["xmin"], bbox["xmax"], by = 0.05)
lat_seq <- seq(bbox["ymin"], bbox["ymax"], by = 0.05)
grid <- expand.grid(lon = lon_seq, lat = lat_seq)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
grid_dentro <- st_join(grid_sf, cuenca, join = st_within, left = FALSE)
coords <- st_coordinates(grid_dentro)
grid <- data.frame(lon = coords[, 1], lat = coords[, 2])

# --- Asignar variables estáticas ---
estaciones_sf <- st_as_sf(
  dplyr::select(data, lon, lat, dem, twi, sin, cos) %>% distinct(),
  coords = c("lon", "lat"), crs = 4326
)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

est_coords <- st_coordinates(estaciones_sf)
grid_coords <- st_coordinates(grid_sf)

nn_idx <- FNN::get.knnx(est_coords, grid_coords, k = 1)$nn.index[,1]
grid$dem <- estaciones_sf$dem[nn_idx]
grid$twi <- estaciones_sf$twi[nn_idx]
grid$sin <- estaciones_sf$sin[nn_idx]
grid$cos <- estaciones_sf$cos[nn_idx]

# Convertir grid a SpatialPoints
coordinates(grid) <- ~lon + lat
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")

# --- Paralelización ---
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

fechas <- sort(unique(data$time))
predictors <- c("lon", "lat", "dem", "twi", "tmax", "tmin", "sin", "cos")

# --- Paralelizar con SOLO LightGBM ---
resultados_list <- foreach(fecha = fechas,
                           .packages = c("dplyr", "sp", "gstat",
                                         "lightgbm", "raster", "lubridate")) %dopar% {
                                           datos_dia <- dplyr::filter(data, time == fecha)
                                           if (nrow(datos_dia) < 5) return(NULL)
                                           
                                           grid_temp <- as.data.frame(grid)
                                           
                                           # Interpolación IDW para Tmax y Tmin
                                           for (var in c("tmax", "tmin")) {
                                             df_sp <- datos_dia[, c("lon", "lat", var)]
                                             coordinates(df_sp) <- ~lon + lat
                                             proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
                                             
                                             idw_out <- idw(as.formula(paste(var, "~ 1")), df_sp, newdata = grid, idp = 2)
                                             grid_temp[[var]] <- idw_out$var1.pred
                                           }
                                           
                                           # ===============================
                                           #       LightGBM con log-transform
                                           # ===============================
                                           train <- dplyr::select(datos_dia, dplyr::all_of(c(predictors, "prcp")))
                                           train <- train[complete.cases(train), ]
                                           n <- nrow(train)
                                           
                                           if (n < 5 || length(unique(train$prcp)) < 2) {
                                             # fallback IDW
                                             df_sp_p <- datos_dia[, c("lon","lat","prcp")]
                                             coordinates(df_sp_p) <- ~lon + lat
                                             proj4string(df_sp_p) <- CRS("+proj=longlat +datum=WGS84")
                                             idw_p <- idw(prcp ~ 1, df_sp_p, newdata = grid, idp = 2)
                                             grid_temp$prcp <- pmax(0, idw_p$var1.pred)
                                             
                                           } else {
                                             train_x <- as.matrix(dplyr::select(train, dplyr::all_of(predictors)))
                                             train_y <- log1p(train$prcp)
                                             
                                             dtrain <- lightgbm::lgb.Dataset(data = train_x, label = train_y)
                                             
                                             params <- list(
                                               objective = "regression",
                                               metric = "rmse",
                                               boosting = "gbdt",
                                               learning_rate = 0.05,
                                               num_leaves = min(63, max(4, floor(n/5))),   # ajustado para datos grandes
                                               max_depth = -1,                             # sin límite
                                               min_data_in_leaf = max(20, floor(n/50)),    # hojas mínimas según dataset
                                               feature_fraction = 0.8,                     # reduce sobreajuste
                                               bagging_fraction = 0.8,
                                               bagging_freq = 1,
                                               verbosity = -1
                                             )
                                             
                                             lgb_model <- lightgbm::lgb.train(
                                               params = params,
                                               data = dtrain,
                                               nrounds = 200,      # más bajo que 300 para velocidad
                                               verbose = -1
                                             )
                                             
                                             test_x <- as.matrix(dplyr::select(grid_temp, dplyr::all_of(predictors)))
                                             preds_log <- predict(lgb_model, test_x)
                                             grid_temp$prcp <- pmax(0, expm1(preds_log))  # nunca negativos
                                           }
                                           
                                           # --- Convertir a raster ---
                                           coordinates(grid_temp) <- ~lon + lat
                                           gridded(grid_temp) <- TRUE
                                           raster_prcp <- raster(grid_temp["prcp"])
                                           
                                           list(fecha = as.character(fecha), raster = raster_prcp)
                                         }

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

# --- Guardar resultados por año ---
output_dir <- "D:/S/Serbia1km/Interpolation"
if (!dir.exists(output_dir)) dir.create(output_dir)

for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    names(s) <- fechas_utilizadas
    file_name <- file.path(output_dir, paste0("titicaca_best_lgbm_", anio, ".tif"))
    writeRaster(s, filename=file_name, format="GTiff", overwrite=TRUE)
    message("Guardado: ", file_name)
  }
}

stopCluster(cl)
