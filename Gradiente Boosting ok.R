# --- Librerías ---

############## okkkkkkkkkkkk

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
library(catboost) 

# Cargar datos de estaciones
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_huallaga.csv")
data$time <- as.Date(data$time, format = "%m/%d/%Y")

# Leer cuenca y crear grilla dentro
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

# Leer rásters estáticos
r_dem  <- raster("D:/S/Serbia1km/Interpolation/Huallaga//dem.tif")
r_twi  <- raster("D:/S/Serbia1km/Interpolation/Huallaga//twi.tif")
r_sin  <- raster("D:/S/Serbia1km/Interpolation/Huallaga//sin.tif")
r_cos  <- raster("D:/S/Serbia1km/Interpolation/Huallaga//cos.tif")

# Extraer valores estáticos a cada celda
coordinates(grid_cells) <- ~lon + lat
proj4string(grid_cells) <- CRS("+proj=longlat +datum=WGS84")
grid_cells$dem <- raster::extract(r_dem, grid_cells)
grid_cells$twi <- raster::extract(r_twi, grid_cells)
grid_cells$sin <- raster::extract(r_sin, grid_cells)
grid_cells$cos <- raster::extract(r_cos, grid_cells)

grid_static_df <- as.data.frame(grid_cells) %>%
  dplyr::select(lon, lat, dem, twi, sin, cos) %>%
  mutate(across(c(twi, sin, cos), ~ round(.x, 2)))

# Rellenar NA en sin y cos con vecino más cercano
rellenar_na_vecino <- function(df, col) {
  na_rows <- which(is.na(df[[col]]))
  if(length(na_rows) == 0) return(df)
  no_na_rows <- which(!is.na(df[[col]]))
  vecinos <- get.knnx(df[no_na_rows, c("lon", "lat")], df[na_rows, c("lon", "lat")], k=1)$nn.index
  df[[col]][na_rows] <- df[[col]][no_na_rows][vecinos]
  return(df)
}

grid_static_df1 <- grid_static_df %>%
  rellenar_na_vecino("sin") %>%
  rellenar_na_vecino("cos")

# Cargar rásters multibanda para variables dinámicas
r_tmax <- stack("D:/S/Serbia1km/Interpolation/Huallaga/tmax_2000_2003.tif")
r_tmin <- stack("D:/S/Serbia1km/Interpolation/Huallaga/tmin_2000_2003.tif")
#r_imerg <- stack("D:/path/imerg_2000_2003.tif")

start_date <- as.Date("2000-01-01")

get_layer_index <- function(r_stack, date, start_date) {
  diff_days <- as.integer(difftime(date, start_date, units = "days")) + 1
  if(diff_days > 0 && diff_days <= nlayers(r_stack)) {
    return(diff_days)
  } else {
    return(NA)
  }
}

# Función auxiliar para predecir precipitación

predecir_prcp <- function(modelo, datos_dia, grid_temp, predictors) {
  if (modelo == "rf") {
    rf <- randomForest(prcp ~ ., data = datos_dia[, c(predictors, "prcp")], ntree = 100)
    preds <- predict(rf, newdata = grid_temp)
    return(pmax(0, preds))
    
  } else if (modelo == "lgbm") {
    train <- datos_dia[, c(predictors, "prcp")]
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      df_sp <- datos_dia[, c("lon", "lat", "prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_cells, idp = 2)
      return(pmax(0, idw_p$var1.pred))
    }
    train_x <- as.matrix(train[, predictors])
    train_y <- train$prcp
    dtrain <- lgb.Dataset(data = train_x, label = train_y)
    params <- list(objective = "regression", metric = "rmse", learning_rate = 0.05,
                   num_leaves = 31, max_depth = 6, min_data_in_leaf = 3, verbosity = -1)
    model <- lgb.train(params = params, data = dtrain, nrounds = 300, verbose = -1)
    test_x <- as.matrix(grid_temp[, predictors])
    preds <- predict(model, test_x)
    return(pmax(0, preds))
    
  } else if (modelo == "xgb") {
    train <- datos_dia[, c(predictors, "prcp")]
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      df_sp <- datos_dia[, c("lon", "lat", "prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_cells, idp = 2)
      return(pmax(0, idw_p$var1.pred))
    }
    train_x <- as.matrix(train[, predictors])
    train_y <- train$prcp
    dtrain <- xgb.DMatrix(data = train_x, label = train_y)
    params <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 6,
                   subsample = 0.8, colsample_bytree = 0.8)
    model <- xgb.train(params = params, data = dtrain, nrounds = 300, verbose = 0)
    test_x <- as.matrix(grid_temp[, predictors])
    preds <- predict(model, test_x)
    return(pmax(0, preds))
    
  } else if (modelo == "ctb") {
    train <- datos_dia[, c(predictors, "prcp")]
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      df_sp <- datos_dia[, c("lon", "lat", "prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_cells, idp = 2)
      return(pmax(0, idw_p$var1.pred))
    }
    train_pool <- catboost.load_pool(data = train[, predictors], label = train$prcp)
    params <- list(loss_function = "RMSE",
                   iterations = 300,
                   depth = 6,
                   learning_rate = 0.05,
                   logging_level = "Silent")
    model <- catboost.train(train_pool, params = params)
    test_pool <- catboost.load_pool(data = grid_temp[, predictors])
    preds <- catboost.predict(model, test_pool)
    return(pmax(0, preds))
  }
}


# Configuración
predictors <- c("lon", "lat", "dem", "twi", "sin", "cos", "tmax", "tmin")
modelo_prcp <- "rf"  # "rf", "lgbm", "xgb"

# Paralelización
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

fechas <- sort(unique(data$time))

resultados_list <- foreach(fecha = fechas,
                           .packages = c("dplyr","sp","gstat","randomForest","catboost",
                                         "lightgbm","xgboost","raster","lubridate")) %dopar% {
                             
                             datos_dia <- dplyr::filter(data, time == fecha)
                             if (nrow(datos_dia) < 5) return(NULL)
                             
                             grid_temp <- grid_static_df1
                             
                             layer_tmax <- get_layer_index(r_tmax, fecha, start_date)
                             layer_tmin <- get_layer_index(r_tmin, fecha, start_date)
                             
                             if (!is.na(layer_tmax)) grid_temp$tmax <- raster::extract(r_tmax[[layer_tmax]], grid_cells)
                             else grid_temp$tmax <- NA
                             if (!is.na(layer_tmin)) grid_temp$tmin <- raster::extract(r_tmin[[layer_tmin]], grid_cells)
                             else grid_temp$tmin <- NA
                             
                             # Puedes añadir imerg si lo deseas y actualizar vector de predictors
                             
                             grid_temp <- grid_temp %>% filter(!is.na(tmax), !is.na(tmin))
                             
                             grid_temp$prcp <- predecir_prcp(modelo_prcp, datos_dia, grid_temp, predictors)
                             
                             coordinates(grid_temp) <- ~lon + lat
                             gridded(grid_temp) <- TRUE
                             raster_prcp <- raster(grid_temp["prcp"])
                             
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
    file_name <- file.path(output_dir, paste0("huallagaok_", modelo_prcp, "_", anio, ".tif"))
    writeRaster(s, filename = file_name, format = "GTiff", overwrite = TRUE)
    message("Guardado: ", file_name)
  }
}


library(raster)

# Cargar el raster multicapas de un año
r <- stack("D:/S/Serbia1km/Interpolation/predictionhuallaga/huallagaok_rf_2000.tif")

# Revisar capas
nlayers(r)  # cuántos días tiene
names(r)    # nombres (fechas)

# Graficar la primera capa
# Graficar los primeros 4 días
plot(r)

################# validacion cruzada ok
# Función para validación cruzada k-fold
validacion_cruzada <- function(datos, modelo, predictors, k = 5, semilla = 1234) {
  set.seed(semilla)
  folds <- createFolds(datos$prcp, k = k, list = TRUE, returnTrain = FALSE)
  
  resultados <- data.frame(Fold=integer(), RMSE=double(), MAE=double(), R2=double(), Modelo=character(), stringsAsFactors=FALSE)
  
  for(i in seq_along(folds)) {
    test_idx <- folds[[i]]
    train_data <- datos[-test_idx, ]
    test_data <- datos[test_idx, ]
    train_data <- train_data[complete.cases(train_data[, c(predictors, "prcp")]), ]
    test_data <- test_data[complete.cases(test_data[, c(predictors, "prcp")]), ]
    
    if(nrow(train_data) < 5 || length(unique(train_data$prcp)) < 2) next
    
    if(modelo == "rf") {
      mod <- randomForest(prcp ~ ., data = train_data[, c(predictors, "prcp")], ntree = 100)
      preds <- predict(mod, newdata = test_data[, predictors])
      
    } else if(modelo == "lgbm") {
      dtrain <- lgb.Dataset(as.matrix(train_data[, predictors]), label = train_data$prcp)
      params <- list(objective = "regression", metric = "rmse",
                     learning_rate = 0.05, num_leaves = 31, max_depth = 6,
                     min_data_in_leaf = 3, verbosity = -1)
      mod <- lgb.train(params, dtrain, nrounds = 300, verbose = -1)
      preds <- predict(mod, as.matrix(test_data[, predictors]))
      
    } else if(modelo == "xgb") {
      dtrain <- xgb.DMatrix(data = as.matrix(train_data[, predictors]), label = train_data$prcp)
      params <- list(objective = "reg:squarederror", eta = 0.05,
                     max_depth = 6, subsample = 0.8, colsample_bytree = 0.8)
      mod <- xgb.train(params, dtrain, nrounds = 300, verbose = 0)
      preds <- predict(mod, as.matrix(test_data[, predictors]))
      
    } else if(modelo == "ctb") {
      train_pool <- catboost.load_pool(data = train_data[, predictors], label = train_data$prcp)
      params <- list(loss_function = "RMSE", iterations = 300,
                     depth = 6, learning_rate = 0.05, logging_level = "Silent")
      mod <- catboost.train(train_pool, params = params)
      test_pool <- catboost.load_pool(data = test_data[, predictors])
      preds <- catboost.predict(mod, test_pool)
    }
    
    rmse <- sqrt(mean((preds - test_data$prcp)^2))
    mae <- mean(abs(preds - test_data$prcp))
    r2 <- cor(preds, test_data$prcp)^2
    
    resultados <- rbind(resultados, data.frame(Fold = i, RMSE = rmse, MAE = mae, R2 = r2, Modelo = modelo, stringsAsFactors = FALSE))
  }
  
  return(resultados)
}

# Validar cada modelo
resultados_rf <- validacion_cruzada(data, "rf", predictors)
resultados_lgbm <- validacion_cruzada(data, "lgbm", predictors)
resultados_xgb <- validacion_cruzada(data, "xgb", predictors)
resultados_ctb <- validacion_cruzada(data, "ctb", predictors)

# Unir resultados
resultados_todos <- rbind(resultados_rf, resultados_lgbm, resultados_xgb, resultados_ctb)

# Mostrar resumen
library(ggplot2)
ggplot(resultados_todos, aes(x = Modelo, y = RMSE)) + geom_boxplot() + ggtitle("Comparación RMSE por modelo")




#########################################################################################
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

# --- Leer rásters estáticos (.tif) ---
r_dem  <- raster("D:/S/Serbia1km/Interpolation/Huallaga//dem.tif")
r_twi  <- raster("D:/S/Serbia1km/Interpolation/Huallaga//twi.tif")
r_sin  <- raster("D:/S/Serbia1km/Interpolation/Huallaga//sin.tif")
r_cos  <- raster("D:/S/Serbia1km/Interpolation/Huallaga//cos.tif")

# --- Extraer valores estáticos a cada celda ---
coordinates(grid_cells) <- ~lon + lat
proj4string(grid_cells) <- CRS("+proj=longlat +datum=WGS84")

grid_cells$dem <- raster::extract(r_dem, grid_cells)
grid_cells$twi <- raster::extract(r_twi, grid_cells)
grid_cells$sin <- raster::extract(r_sin, grid_cells)
grid_cells$cos <- raster::extract(r_cos, grid_cells)

# Guardar una copia como data.frame para foreach
grid_static_df <- as.data.frame(grid_cells) %>%
  dplyr::select(lon, lat, dem, twi, sin, cos)

# 2 decimales
grid_static_df <- grid_static_df %>%
  mutate(across(c(twi, sin, cos), ~ round(.x, 2)))

# Suponiendo que quieres revisar todas las columnas excepto "lon" y "lat"
#grid_static_df <- grid_static_df %>%
#filter(rowSums(is.na(select(., -lon, -lat))) < ncol(select(., -lon, -lat)))

# Rellenar na para cos y sin inthe grilled
rellenar_na_vecino <- function(df, col) {
  na_rows <- which(is.na(df[[col]]))
  if(length(na_rows) == 0) return(df)
  no_na_rows <- which(!is.na(df[[col]]))
  vecinos <- get.knnx(df[no_na_rows, c("lon", "lat")], df[na_rows, c("lon", "lat")], k=1)$nn.index
  df[[col]][na_rows] <- df[[col]][no_na_rows][vecinos]
  return(df)
}

grid_static_df1 <- grid_static_df %>%
  rellenar_na_vecino("sin") %>%
  rellenar_na_vecino("cos")

# --- Función auxiliar para prcp ---
predecir_prcp <- function(modelo, datos_dia, grid_temp, predictors) {
  if (modelo == "rf") {
    rf <- randomForest(prcp ~ lon + lat + dem + twi + tmax + tmin + sin + cos,
                       data = datos_dia, ntree = 100)
    preds <- predict(rf, newdata = grid_temp)
    return(pmax(0, preds))
    
  } else if (modelo == "lgbm") {
    train <- dplyr::select(datos_dia, dplyr::all_of(c(predictors, "prcp")))
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      df_sp <- datos_dia[, c("lon","lat","prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_cells, idp = 2)
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
    return(pmax(0, preds))
    
  } else if (modelo == "xgb") {
    train <- dplyr::select(datos_dia, dplyr::all_of(c(predictors, "prcp")))
    train <- train[complete.cases(train), ]
    if (nrow(train) < 5 || length(unique(train$prcp)) < 2) {
      df_sp <- datos_dia[, c("lon","lat","prcp")]
      coordinates(df_sp) <- ~lon + lat
      proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
      idw_p <- idw(prcp ~ 1, df_sp, newdata = grid_cells, idp = 2)
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
    return(pmax(0, preds))
  }
}

# --- Configuración ---
predictors <- c("lon","lat","dem","twi","tmax","tmin","sin","cos")
modelo_prcp <- "rf"  # "rf", "lgbm", "xgb"

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
  
  # Copia de los estáticos
  grid_temp <- grid_static_df1
  
  # Interpolar tmax y tmin
  for (var in c("tmax", "tmin")) {
    df_sp <- datos_dia[, c("lon", "lat", var)]
    coordinates(df_sp) <- ~lon + lat
    proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
    idw_out <- idw(as.formula(paste(var, "~ 1")), df_sp, newdata = grid_cells, idp = 2)
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


library(raster)

# Cargar el raster multicapas de un año
r <- stack("D:/S/Serbia1km/Interpolation/huallagaok_rf_2000.tif")

# Revisar capas
nlayers(r)  # cuántos días tiene
names(r)    # nombres (fechas)

# Graficar la primera capa
# Graficar los primeros 4 días
plot(r)

