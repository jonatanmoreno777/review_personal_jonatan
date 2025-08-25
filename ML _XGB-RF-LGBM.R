
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
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_lima.csv")
data$time <- as.Date(data$time, format = "%m/%d/%Y")

# --- Leer cuenca y crear grilla dentro ---
cuenca <- st_read("D:/S/Serbia1km/Interpolation/Titicaca_border/Lima.shp")
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
    params <- list(objective="regression", metric="rmse", # boosting_type ="dart ,n_estimators= 1000, learning_rate =0.01
                   learning_rate=0.05, num_leaves=31,   #max_depth= 5, subsample= 0.8, feature__fracction= 0.9
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
# --- Configuración ---
predictors <- c("lon","lat","dem","twi","tmax","tmin","sin","cos","precland")
modelo_prcp <- "lgbm"  # Cambiar entre "rf", "lgbm", "xgb"

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
  
  # Interpolar tmax, tmin y precland
  for (var in c("tmax", "tmin", "precland")) {
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
output_dir <- "D:/S/Serbia1km/Interpolation"
if (!dir.exists(output_dir)) dir.create(output_dir)

for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    names(s) <- fechas_utilizadas
    file_name <- file.path(output_dir, paste0("limaland_", modelo_prcp, "_", anio, ".tif"))
    writeRaster(s, filename = file_name, format = "GTiff", overwrite = TRUE)
    message("Guardado: ", file_name)
  }
}
