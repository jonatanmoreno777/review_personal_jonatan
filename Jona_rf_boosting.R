
# =================================================================
# Script completo: Interpolación y predicción diaria de precipitación    Interpolation idw :
# =================================================================

library(rgdal)
library(sf)
library(sp)
library(gstat)
library(randomForest)
library(lightgbm)
library(xgboost)
library(catboost)
library(raster)
library(lubridate)
library(progress)
library(FNN)
library(dplyr)
library(foreach)
library(doParallel)
library(caret)

# ---------------------------
# 1️⃣ Cargar datos
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

# Dataframe limpio con predictores estáticos
grid_static_df <- as.data.frame(grid_cells) %>%
  dplyr::select(lon, lat, dem, twi, sin, cos, slope) %>%
  mutate(across(c(twi, sin, cos, slope), ~ round(.x, 2)))

# ---------------------------
# 5️⃣ Función para rellenar NAs por vecino más cercano
# ---------------------------
rellenar_na_vecino <- function(df, col) {
  na_rows <- which(is.na(df[[col]]))
  if(length(na_rows) == 0) return(df)
  no_na_rows <- which(!is.na(df[[col]]))
  vecinos <- get.knnx(df[no_na_rows, c("lon", "lat")], df[na_rows, c("lon", "lat")], k=1)$nn.index
  df[[col]][na_rows] <- df[[col]][no_na_rows][vecinos]
  return(df)
}

# Ejemplo de uso:
# grid_static_df <- rellenar_na_vecino(grid_static_df, 'twi')
# grid_static_df <- rellenar_na_vecino(grid_static_df, 'sin')
# grid_static_df <- rellenar_na_vecino(grid_static_df, 'cos')
# grid_static_df <- rellenar_na_vecino(grid_static_df, 'slope')

# ---------------------------
# 6️⃣ Función de predicción de precipitación
# ---------------------------
predecir_prcp <- function(modelo, datos_dia, grid_df, grid_sp = NULL, predictors) {
  
  if (modelo == "rf") {
    rf <- randomForest(prcp ~ ., data = datos_dia[, c(predictors, "prcp")], ntree = 100)
    preds <- predict(rf, newdata = grid_df)
    return(pmax(0, preds))
    
  } else if (modelo == "lgbm") {
    train <- datos_dia[, c(predictors, "prcp")]
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      if (is.null(grid_sp)) stop("grid_sp required for IDW fallback")
      df_sp <- datos_dia[, c("lon", "lat", "prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_sp, idp = 2)
      return(pmax(0, idw_p$var1.pred))
    }
    train_x <- as.matrix(train[, predictors])
    train_y <- train$prcp
    dtrain <- lgb.Dataset(data = train_x, label = train_y)
    params <- list(objective = "regression", metric = "rmse", learning_rate = 0.05,
                   num_leaves = 31, max_depth = 6, min_data_in_leaf = 3, verbosity = -1)
    model <- lgb.train(params = params, data = dtrain, nrounds = 300, verbose = -1)
    test_x <- as.matrix(grid_df[, predictors])
    preds <- predict(model, test_x)
    return(pmax(0, preds))
    
  } else if (modelo == "xgb") {
    train <- datos_dia[, c(predictors, "prcp")]
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      if (is.null(grid_sp)) stop("grid_sp required for IDW fallback")
      df_sp <- datos_dia[, c("lon", "lat", "prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_sp, idp = 2)
      return(pmax(0, idw_p$var1.pred))
    }
    train_x <- as.matrix(train[, predictors])
    train_y <- train$prcp
    dtrain <- xgb.DMatrix(data = train_x, label = train_y)
    params <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 6,
                   subsample = 0.8, colsample_bytree = 0.8)
    model <- xgb.train(params = params, data = dtrain, nrounds = 300, verbose = 0)
    test_x <- as.matrix(grid_df[, predictors])
    preds <- predict(model, test_x)
    return(pmax(0, preds))
    
  } else if (modelo == "ctb") {
    train <- datos_dia[, c(predictors, "prcp")]
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      if (is.null(grid_sp)) stop("grid_sp required for IDW fallback")
      df_sp <- datos_dia[, c("lon", "lat", "prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_sp, idp = 2)
      return(pmax(0, idw_p$var1.pred))
    }
    train_pool <- catboost.load_pool(data = train[, predictors], label = train$prcp)
    params <- list(loss_function = "RMSE",
                   iterations = 300,
                   depth = 6,
                   learning_rate = 0.05,
                   logging_level = "Silent")
    model <- catboost.train(train_pool, params = params)
    test_pool <- catboost.load_pool(data = grid_df[, predictors])
    preds <- catboost.predict(model, test_pool)
    return(pmax(0, preds))
  }
}

# ---------------------------
# 7️⃣ Configuración de predictores y modelo
# ---------------------------
predictors <- c("lon", "lat", "dem", "twi", "sin", "cos", "slope", "tmax", "tmin", "precland")
modelo_prcp <- "xgb"  # "rf", "lgbm", "xgb", "ctb"

# ---------------------------
# 8️⃣ Preparar grilla para IDW
# ---------------------------
grid_sp <- grid_cells  # SpatialPointsDataFrame ya contiene lon, lat y predictores estáticos

# ---------------------------
# 9️⃣ Loop paralelo
# ---------------------------
num_cores <- parallel::detectCores()
num_cores_use <- ifelse(is.na(num_cores) || num_cores < 2, 1, max(1, num_cores - 1))
cl <- makeCluster(num_cores_use)
registerDoParallel(cl)

fechas <- sort(unique(data$time))

resultados_list <- foreach(
  fecha = fechas,
  .packages = c("dplyr","sp","gstat","randomForest","lightgbm","xgboost",
                "raster","lubridate","FNN","catboost")
) %dopar% {
  
  datos_dia <- dplyr::filter(data, time == fecha)
  if (nrow(datos_dia) < 2) return(NULL)
  
  # Copiar grid estático
  grid_df_day <- grid_static_df
  grid_sp_day <- grid_sp
  
  # Interpolación diaria de tmax, tmin, precland
  for (var in c("tmax", "tmin", "precland")) {
    if (!var %in% names(datos_dia)) {
      grid_df_day[[var]] <- NA_real_
      next
    }
    if (length(unique(datos_dia[[var]])) < 2) {
      grid_df_day[[var]] <- mean(datos_dia[[var]], na.rm = TRUE)
      next
    }
    
    df_sp <- datos_dia[, c("lon", "lat", var)]
    coordinates(df_sp) <- ~lon + lat
    proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
    
    pred_vec <- tryCatch({
      idw_out <- idw(as.formula(paste(var, "~ 1")), df_sp, newdata = grid_sp_day, idp = 2)
      idw_out$var1.pred
    }, error = function(e) rep(NA_real_, nrow(grid_df_day)))
    
    grid_df_day[[var]] <- pred_vec
  }
  
  # Predecir precipitación
  grid_df_day$prcp <- predecir_prcp(
    modelo = modelo_prcp,
    datos_dia = datos_dia,
    grid_df = grid_df_day,
    grid_sp = grid_sp_day,
    predictors = predictors
  )
  
  # Convertir a raster
  xyz <- grid_df_day[, c("lon", "lat", "prcp")]
  xyz <- xyz[complete.cases(xyz), ]
  raster_prcp <- tryCatch({
    rasterFromXYZ(xyz, res = 0.05, crs = CRS("+proj=longlat +datum=WGS84"))
  }, error = function(e) NULL)
  
  list(fecha = as.character(fecha), raster = raster_prcp)
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
output_dir <- "D:/S/Serbia1km/Interpolation/predictionhuallaga"
if (!dir.exists(output_dir)) dir.create(output_dir)

for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    names(s) <- fechas_utilizadas
    file_name <- file.path(output_dir, paste0("huallaga_", modelo_prcp, "_", anio, ".tif"))
    writeRaster(s, filename = file_name, format = "GTiff", overwrite = TRUE)
    message("Guardado: ", file_name)
  }
}


######################### ok jontan plus dinamic variable too

library(rgdal)
library(sf)
library(sp)
library(gstat)
library(randomForest)
library(lightgbm)
library(xgboost)
library(catboost)
library(raster)
library(lubridate)
library(FNN)
library(dplyr)
library(foreach)
library(doParallel)
library(caret)

# --- Cargar datos de estaciones ---
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_huallaga.csv")
data$time <- as.Date(data$time, format = "%m/%d/%Y")

# --- Leer cuenca y crear grilla dentro ---
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

# --- Leer rásters estáticos ---
r_dem   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/dem.tif")
r_twi   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/twi.tif")
r_sin   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/sin.tif")
r_cos   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/cos.tif")
r_slope <- raster("D:/S/Serbia1km/Interpolation/Huallaga/slope.tif")

# --- Extraer valores estáticos a cada celda ---
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

grid_sp <- grid_cells  # SpatialPointsDataFrame para fallback IDW

# --- Cargar rásters dinámicos ---
r_tmax  <- stack("D:/S/Serbia1km/Interpolation/Huallaga/tmax_2000_2003.tif")
r_tmin  <- stack("D:/S/Serbia1km/Interpolation/Huallaga/tmin_2000_2003.tif")
r_precland <- stack("D:/S/Serbia1km/Interpolation/Huallaga/precland_2000_2003.tif")

start_date <- as.Date("2000-01-01")

get_layer_index <- function(r_stack, date, start_date) {
  diff_days <- as.integer(difftime(date, start_date, units = "days")) + 1
  if(diff_days > 0 && diff_days <= nlayers(r_stack)) return(diff_days)
  else return(NA)
}

extraer_valores_dinamicos <- function(grid_df, r_stack, fecha, start_date) {
  layer_idx <- get_layer_index(r_stack, fecha, start_date)
  if (is.na(layer_idx)) return(rep(NA_real_, nrow(grid_df)))
  r_layer <- raster(r_stack, layer_idx)
  values <- raster::extract(r_layer, grid_df[, c("lon", "lat")])
  return(values)
}

# --- Función de predicción de precipitación ---
predecir_prcp <- function(modelo, datos_dia, grid_df, grid_sp = NULL, predictors) {
  if (modelo == "rf") {
    rf <- randomForest(prcp ~ ., data = datos_dia[, c(predictors, "prcp")], ntree = 100)
    preds <- predict(rf, newdata = grid_df)
    return(pmax(0, preds))
  } else if (modelo == "lgbm") {
    train <- datos_dia[, c(predictors, "prcp")]
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      if (is.null(grid_sp)) stop("grid_sp required for IDW fallback")
      df_sp <- datos_dia[, c("lon", "lat", "prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_sp, idp = 2)
      return(pmax(0, idw_p$var1.pred))
    }
    train_x <- as.matrix(train[, predictors])
    train_y <- train$prcp
    dtrain <- lgb.Dataset(data = train_x, label = train_y)
    params <- list(objective = "regression", metric = "rmse", learning_rate = 0.05,
                   num_leaves = 31, max_depth = 6, min_data_in_leaf = 3, verbosity = -1)
    model <- lgb.train(params = params, data = dtrain, nrounds = 300, verbose = -1)
    test_x <- as.matrix(grid_df[, predictors])
    preds <- predict(model, test_x)
    return(pmax(0, preds))
  } else if (modelo == "xgb") {
    train <- datos_dia[, c(predictors, "prcp")]
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      if (is.null(grid_sp)) stop("grid_sp required for IDW fallback")
      df_sp <- datos_dia[, c("lon", "lat", "prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_sp, idp = 2)
      return(pmax(0, idw_p$var1.pred))
    }
    train_x <- as.matrix(train[, predictors])
    train_y <- train$prcp
    dtrain <- xgb.DMatrix(data = train_x, label = train_y)
    params <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 6,
                   subsample = 0.8, colsample_bytree = 0.8)
    model <- xgb.train(params = params, data = dtrain, nrounds = 300, verbose = 0)
    test_x <- as.matrix(grid_df[, predictors])
    preds <- predict(model, test_x)
    return(pmax(0, preds))
  } else if (modelo == "ctb") {
    train <- datos_dia[, c(predictors, "prcp")]
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      if (is.null(grid_sp)) stop("grid_sp required for IDW fallback")
      df_sp <- datos_dia[, c("lon", "lat", "prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_sp, idp = 2)
      return(pmax(0, idw_p$var1.pred))
    }
    train_pool <- catboost.load_pool(data = train[, predictors], label = train$prcp)
    params <- list(loss_function = "RMSE",
                   iterations = 300,
                   depth = 6,
                   learning_rate = 0.05,
                   logging_level = "Silent")
    model <- catboost.train(train_pool, params = params)
    test_pool <- catboost.load_pool(data = grid_df[, predictors])
    preds <- catboost.predict(model, test_pool)
    return(pmax(0, preds))
  }
}

# --- Configuración ---
predictors <- c("lon", "lat", "dem", "twi", "sin", "cos", "slope", "tmax", "tmin", "precland")
modelo_prcp <- "xgb"  # rf, lgbm, xgb, ctb

# --- Loop diario ---
num_cores <- parallel::detectCores()
num_cores_use <- ifelse(is.na(num_cores) || num_cores < 2, 1, max(1, num_cores - 1))
cl <- makeCluster(num_cores_use)
registerDoParallel(cl)

fechas <- sort(unique(data$time))

resultados_list <- foreach(
  fecha = fechas,
  .packages = c("dplyr","sp","gstat","randomForest","lightgbm","xgboost",
                "raster","lubridate","FNN","catboost")
) %dopar% {
  
  datos_dia <- dplyr::filter(data, time == fecha)
  if (nrow(datos_dia) < 2) return(NULL)
  
  # Copiar grilla estática
  grid_df_day <- grid_static_df
  grid_sp_day <- grid_sp
  
  # Extraer dinámicos desde rásters diarios
  grid_df_day$tmax      <- extraer_valores_dinamicos(grid_df_day, r_tmax, fecha, start_date)
  grid_df_day$tmin      <- extraer_valores_dinamicos(grid_df_day, r_tmin, fecha, start_date)
  grid_df_day$precland  <- extraer_valores_dinamicos(grid_df_day, r_precland, fecha, start_date)
  
  # Predecir precipitación
  grid_df_day$prcp <- predecir_prcp(
    modelo = modelo_prcp,
    datos_dia = datos_dia,
    grid_df = grid_df_day,
    grid_sp = grid_sp_day,
    predictors = predictors
  )
  
  # Convertir a raster
  xyz <- grid_df_day[, c("lon", "lat", "prcp")]
  xyz <- xyz[complete.cases(xyz), ]
  raster_prcp <- tryCatch({
    rasterFromXYZ(xyz, res = 0.05, crs = CRS("+proj=longlat +datum=WGS84"))
  }, error = function(e) NULL)
  
  list(fecha = as.character(fecha), raster = raster_prcp)
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
output_dir <- "D:/S/Serbia1km/Interpolation/predictionhuallaga"
if (!dir.exists(output_dir)) dir.create(output_dir)

for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    names(s) <- fechas_utilizadas
    file_name <- file.path(output_dir, paste0("huallaga_", modelo_prcp, "_", anio, ".tif"))
    writeRaster(s, filename = file_name, format = "GTiff", overwrite = TRUE)
    message("Guardado: ", file_name)
  }
}

# --- 7. (Opcional) Guardar rásters diarios ---
# for(i in seq_along(resultados_list)) {
#   r <- resultados_list[[i]]$raster
#   if(!is.null(r)) {
#     writeRaster(r, filename = paste0("D:/S/Serbia1km/Interpolation/Huallaga/prcp_", resultados_list[[i]]$fecha, ".tif"),
#                 format = "GTiff", overwrite = TRUE)
#   }
# }


######################### save as netcdf

library(rgdal)
library(sf)
library(sp)
library(gstat)
library(randomForest)
library(lightgbm)
library(xgboost)
library(catboost)
library(raster)
library(lubridate)
library(progress)
library(FNN)
library(dplyr)
library(foreach)
library(doParallel)
library(caret)
library(ncdf4)

# --- Cargar datos de estaciones ---
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_huallaga.csv")
data$time <- as.Date(data$time, format = "%m/%d/%Y")

# --- Leer cuenca y crear grilla dentro ---
cuenca <- st_read("D:/S/Serbia1km/Interpolation/Titicaca_border/watershed.shp") %>%
  st_transform(crs = 4326)
bbox <- st_bbox(cuenca)
lon_seq <- seq(bbox["xmin"], bbox["xmax"], by = 0.05)
lat_seq <- seq(bbox["ymin"], bbox["ymax"], by = 0.05)
grid_df <- expand.grid(lon = lon_seq, lat = lat_seq)
grid_sf <- st_as_sf(grid_df, coords = c("lon","lat"), crs = 4326)
grid_dentro <- st_join(grid_sf, cuenca, join = st_within, left = FALSE)
coords <- st_coordinates(grid_dentro)
grid_cells <- data.frame(lon = coords[,1], lat = coords[,2])

# --- Leer rásters estáticos ---
r_dem   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/dem.tif")
r_twi   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/twi.tif")
r_sin   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/sin.tif")
r_cos   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/cos.tif")
r_slope <- raster("D:/S/Serbia1km/Interpolation/Huallaga/slope.tif")

coordinates(grid_cells) <- ~lon + lat
proj4string(grid_cells) <- CRS("+proj=longlat +datum=WGS84")
grid_cells$dem   <- raster::extract(r_dem, grid_cells)
grid_cells$twi   <- raster::extract(r_twi, grid_cells)
grid_cells$sin   <- raster::extract(r_sin, grid_cells)
grid_cells$cos   <- raster::extract(r_cos, grid_cells)
grid_cells$slope <- raster::extract(r_slope, grid_cells)

# --- Dataframe estático final ---
grid_static_df <- as.data.frame(grid_cells) %>%
  dplyr::select(lon, lat, dem, twi, sin, cos, slope) %>%
  mutate(across(c(twi, sin, cos, slope), ~ ifelse(is.na(.x), 0, round(.x,2))))

# --- Cargar rásters dinámicos ---
r_tmax  <- stack("D:/S/Serbia1km/Interpolation/Huallaga/tmax_2000_2003.tif")
r_tmin  <- stack("D:/S/Serbia1km/Interpolation/Huallaga/tmin_2000_2003.tif")
r_precland <- stack("D:/S/Serbia1km/Interpolation/Huallaga/precland_2000_2003.tif")
start_date <- as.Date("2000-01-01")

# --- Función para obtener capa de ráster según fecha ---
get_layer <- function(r_stack, date) {
  idx <- as.integer(difftime(date, start_date, units="days")) + 1
  if(idx > 0 && idx <= nlayers(r_stack)) return(r_stack[[idx]])
  else return(NULL)
}

# --- Configuración de modelo y predictores ---
predictors <- c("lon","lat","dem","twi","sin","cos","slope","tmax","tmin","precland")
modelo_prcp <- "xgb"  # "rf","lgbm","xgb","ctb"

# --- Función de predicción (mantener fallback IDW) ---
predecir_prcp <- function(modelo, datos_dia, grid_df, grid_sp=NULL, predictors) {
  if(nrow(datos_dia) < 5 || length(unique(datos_dia$prcp)) < 2) {
    if(!is.null(grid_sp)) {
      df_sp <- datos_dia[, c("lon","lat","prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_out <- idw(prcp ~ 1, df_sp, newdata = grid_sp, idp = 2)
      return(pmax(0, idw_out$var1.pred))
    } else return(rep(NA_real_, nrow(grid_df)))
  }
  
  train <- datos_dia[, c(predictors, "prcp")] %>% na.omit()
  if(modelo == "rf") {
    rf <- randomForest(prcp ~ ., data=train, ntree=100)
    return(pmax(0, predict(rf, newdata=grid_df)))
  } else if(modelo == "xgb") {
    dtrain <- xgb.DMatrix(as.matrix(train[,predictors]), label=train$prcp)
    model <- xgb.train(list(objective="reg:squarederror", eta=0.05, max_depth=6, subsample=0.8, colsample_bytree=0.8),
                       data=dtrain, nrounds=300, verbose=0)
    return(pmax(0, predict(model, as.matrix(grid_df[,predictors]))))
  } else if(modelo == "lgbm") {
    dtrain <- lgb.Dataset(data=as.matrix(train[,predictors]), label=train$prcp)
    model <- lgb.train(list(objective="regression", metric="rmse", learning_rate=0.05,
                            num_leaves=31, max_depth=6, min_data_in_leaf=3, verbosity=-1),
                       dtrain, nrounds=300)
    return(pmax(0, predict(model, as.matrix(grid_df[,predictors]))))
  } else if(modelo == "ctb") {
    pool <- catboost.load_pool(data=train[,predictors], label=train$prcp)
    model <- catboost.train(pool, list(loss_function="RMSE", iterations=300, depth=6, learning_rate=0.05, logging_level="Silent"))
    test_pool <- catboost.load_pool(grid_df[,predictors])
    return(pmax(0, catboost.predict(model, test_pool)))
  }
}

# --- Paralelización ---
num_cores <- parallel::detectCores()
num_cores_use <- ifelse(is.na(num_cores) || num_cores < 2, 1, max(1, num_cores-1))
cl <- makeCluster(num_cores_use)
registerDoParallel(cl)

fechas <- sort(unique(data$time))
errores <- c()  # para registrar fechas problemáticas

# --- Loop principal ---
resultados_list <- foreach(fecha=fechas, .packages=c("dplyr","sp","gstat","randomForest","lightgbm","xgboost","catboost","raster","FNN")) %dopar% {
  
  datos_dia <- filter(data, time==fecha)
  if(nrow(datos_dia)<2) return(NULL)
  
  grid_df_day <- grid_static_df
  grid_sp_day <- grid_cells
  
  # Extraer dinámicos de rásters
  for(var in c("tmax","tmin","precland")) {
    r_stack <- switch(var, tmax=r_tmax, tmin=r_tmin, precland=r_precland)
    r_layer <- get_layer(r_stack, fecha)
    if(is.null(r_layer)) {
      grid_df_day[[var]] <- NA_real_
    } else {
      grid_df_day[[var]] <- raster::extract(r_layer, grid_sp_day)
      grid_df_day[[var]][is.na(grid_df_day[[var]])] <- mean(grid_df_day[[var]], na.rm=TRUE)
    }
  }
  
  # Predecir precipitación
  grid_df_day$prcp <- tryCatch({
    predecir_prcp(modelo_prcp, datos_dia, grid_df_day, grid_sp_day, predictors)
  }, error=function(e) {
    NA_real_
  })
  
  # Convertir a raster
  xyz <- grid_df_day[, c("lon","lat","prcp")] %>% na.omit()
  raster_prcp <- tryCatch({
    rasterFromXYZ(xyz, res=0.05, crs=CRS("+proj=longlat +datum=WGS84"))
  }, error=function(e) NULL)
  
  if(is.null(raster_prcp)) errores <- c(errores, as.character(fecha))
  
  list(fecha=as.character(fecha), raster=raster_prcp)
}

stopCluster(cl)

# --- Guardar resultados en NetCDF ---
# Convertir lista de raster a array
if(length(resultados_list)>0){
  fechas_validas <- sapply(resultados_list, function(x) x$fecha)
  rasters_validos <- lapply(resultados_list, function(x) x$raster)
  
  # dimensiones
  nlon <- ncol(rasters_validos[[1]])
  nlat <- nrow(rasters_validos[[1]])
  nt <- length(rasters_validos)
  
  prcp_array <- array(NA_real_, dim=c(nlon,nlat,nt))
  for(i in 1:nt){
    prcp_array[,,i] <- t(as.matrix(rasters_validos[[i]]))  # transponer para match lon-lat
  }
  
  nc_out <- nc_create("prcp_huallaga.nc",
                      list(
                        nc_dim("lon", nlon),
                        nc_dim("lat", nlat),
                        nc_dim("time", nt)
                      ))
  ncvar_def <- ncvar_def("prcp","mm",list(nc_out$dim$lon,nc_out$dim$lat,nc_out$dim$time))
  ncvar_put(nc_out, ncvar_def, prcp_array)
  nc_close(nc_out)
}


############## SHAP 

# ============================================
# Predicción diaria de precipitación + SHAP
# ============================================

library(rgdal)
library(sf)
library(sp)
library(gstat)
library(randomForest)
library(xgboost)
library(lightgbm)
library(catboost)
library(raster)
library(lubridate)
library(progress)
library(FNN)
library(dplyr)
library(foreach)
library(doParallel)
library(caret)
library(SHAPforxgboost)
library(ggplot2)

# --- 1. Cargar datos ---
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_huallaga.csv")
data$time <- as.Date(data$time, format = "%m/%d/%Y")

# --- 2. Leer cuenca y crear grilla ---
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

# --- 3. Cargar rásters estáticos ---
r_dem   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/dem.tif")
r_twi   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/twi.tif")
r_sin   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/sin.tif")
r_cos   <- raster("D:/S/Serbia1km/Interpolation/Huallaga/cos.tif")
r_slope <- raster("D:/S/Serbia1km/Interpolation/Huallaga/slope.tif")

# --- 4. Extraer valores estáticos ---
coordinates(grid_cells) <- ~lon + lat
proj4string(grid_cells) <- CRS("+proj=longlat +datum=WGS84")
grid_cells$dem   <- raster::extract(r_dem, grid_cells)
grid_cells$twi   <- raster::extract(r_twi, grid_cells)
grid_cells$sin   <- raster::extract(r_sin, grid_cells)
grid_cells$cos   <- raster::extract(r_cos, grid_cells)
grid_cells$slope <- raster::extract(r_slope, grid_cells)

grid_static_df <- as.data.frame(grid_cells) %>%
  dplyr::select(lon, lat, dem, twi, sin, cos, slope) %>%
  mutate(across(c(twi, sin, cos, slope), ~ round(.x, 2)))

# --- 5. Rellenar NA con vecino más cercano ---
rellenar_na_vecino <- function(df, col) {
  na_rows <- which(is.na(df[[col]]))
  if(length(na_rows) == 0) return(df)
  no_na_rows <- which(!is.na(df[[col]]))
  vecinos <- get.knnx(df[no_na_rows, c("lon", "lat")], df[na_rows, c("lon", "lat")], k=1)$nn.index
  df[[col]][na_rows] <- df[[col]][no_na_rows][vecinos]
  return(df)
}

grid_static_df <- grid_static_df %>%
  # rellenar_na_vecino("twi") %>%   # opcional
  mutate(
    sin = ifelse(is.na(sin), 0, sin),
    cos = ifelse(is.na(cos), 0, cos)
  )

# --- 6. Configuración de predictores ---
predictors <- c("lon","lat","dem","twi","sin","cos","slope","tmax","tmin","precland")
modelo_prcp <- "xgb"  # cambiar si deseas

# --- 7. Preparar paralelización ---
num_cores <- parallel::detectCores()
num_cores_use <- ifelse(is.na(num_cores) || num_cores < 2, 1, max(1, num_cores - 1))
cl <- makeCluster(num_cores_use)
registerDoParallel(cl)

fechas <- sort(unique(data$time))

# --- 8. Loop diario de predicción ---
resultados_list <- foreach(
  fecha = fechas,
  .packages = c("dplyr","sp","gstat","randomForest","xgboost","raster","lubridate","FNN","catboost")
) %dopar% {
  
  datos_dia <- dplyr::filter(data, time == fecha)
  if(nrow(datos_dia) < 2) return(NULL)
  
  # Copiar grilla estática
  grid_df_day <- grid_static_df
  grid_sp_day <- grid_cells
  
  # Interpolación diaria de tmax, tmin, precland
  for(var in c("tmax","tmin","precland")){
    if(!var %in% names(datos_dia)){
      grid_df_day[[var]] <- NA_real_
      next
    }
    if(length(unique(datos_dia[[var]])) < 2){
      grid_df_day[[var]] <- mean(datos_dia[[var]], na.rm = TRUE)
      next
    }
    
    df_sp <- datos_dia[,c("lon","lat",var)]
    coordinates(df_sp) <- ~lon + lat
    proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
    
    pred_vec <- tryCatch({
      idw_out <- idw(as.formula(paste(var,"~1")), df_sp, newdata=grid_sp_day, idp=2)
      idw_out$var1.pred
    }, error=function(e) rep(NA_real_, nrow(grid_df_day)))
    
    grid_df_day[[var]] <- pred_vec
  }
  
  # --- 9. Predecir precipitación ---
  grid_df_day$prcp <- predecir_prcp(
    modelo=modelo_prcp,
    datos_dia=datos_dia,
    grid_df=grid_df_day,
    grid_sp=grid_sp_day,
    predictors=predictors
  )
  
  # --- 10. Raster de precipitación ---
  xyz <- grid_df_day[,c("lon","lat","prcp")]
  xyz <- xyz[complete.cases(xyz),]
  raster_prcp <- tryCatch({
    rasterFromXYZ(xyz,res=0.05, crs=CRS("+proj=longlat +datum=WGS84"))
  }, error=function(e) NULL)
  
  # --- 11. SHAP para este día ---
  shap_values <- NULL
  if(modelo_prcp=="xgb"){
    train <- datos_dia[, c(predictors,"prcp")] %>% na.omit()
    if(nrow(train) >= 5 && length(unique(train$prcp))>1){
      dtrain <- xgb.DMatrix(data=as.matrix(train[,predictors]), label=train$prcp)
      model_xgb <- xgb.train(params=list(objective="reg:squarederror", eta=0.05, max_depth=6, subsample=0.8, colsample_bytree=0.8),
                             data=dtrain, nrounds=300, verbose=0)
      shap_values <- shap.values(xgb_model=model_xgb, X_train=as.matrix(train[,predictors]))$mean_shap_score
    }
  }
  
  list(fecha=as.character(fecha), raster=raster_prcp, shap=shap_values)
}

stopCluster(cl)

# --- 12. SHAP promedio global ---
shap_list <- lapply(resultados_list, function(x) x$shap)
shap_list <- shap_list[!sapply(shap_list,is.null)]
shap_matrix <- do.call(rbind, shap_list)
shap_mean <- colMeans(shap_matrix)
shap_df <- data.frame(predictor=names(shap_mean), mean_abs_shap=shap_mean) %>%
  arrange(desc(mean_abs_shap))

# --- 13. Plot SHAP ---
ggplot(shap_df, aes(x=reorder(predictor, mean_abs_shap), y=mean_abs_shap)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  labs(title="Importancia de predictores (SHAP)", x="Predictor", y="Valor absoluto medio SHAP") +
  theme_minimal(base_size=14)






# install.packages("xgboost")
# install.packages("SHAPforxgboost")
# install.packages("ggplot2")
library(xgboost)
library(SHAPforxgboost)
library(ggplot2)