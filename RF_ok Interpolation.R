library(dplyr)
library(sf)
library(sp)
library(gstat)
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

# --- Asignar variables estáticas dem, twi, sin, cos a la grilla ---
estaciones_sf <- st_as_sf(data %>%
                            select(lon, lat, dem, twi, sin, cos) %>%
                            distinct(),
                          coords = c("lon", "lat"), crs = 4326)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

est_coords <- st_coordinates(estaciones_sf)
grid_coords <- st_coordinates(grid_sf)

library(FNN)  # para get.knnx
nn_idx <- get.knnx(est_coords, grid_coords, k = 1)$nn.index[,1]

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

# Paralelizar con foreach
resultados_list <- foreach(fecha = fechas, .packages = c("dplyr", "sp", "gstat", "randomForest", "raster", "lubridate")) %dopar% {
  datos_dia <- dplyr::filter(data, time == fecha)
  if (nrow(datos_dia) < 5) return(NULL)
  
  # Interpolar tmax y tmin con IDW
  grid_temp <- as.data.frame(grid)
  
  for (var in c("tmax", "tmin")) {
    df_sp <- datos_dia[, c("lon", "lat", var)]
    coordinates(df_sp) <- ~lon + lat
    proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
    
    idw_out <- idw(as.formula(paste(var, "~ 1")), df_sp, newdata = grid, idp = 2)
    grid_temp[[var]] <- idw_out$var1.pred
  }
  
  # Ajustar RF y predecir prcp
  rf <- randomForest(prcp ~ lon + lat + dem + twi + tmax + tmin + sin + cos,
                     data = datos_dia, ntree = 100)
  grid_temp$prcp <- predict(rf, newdata = grid_temp)
  
  coordinates(grid_temp) <- ~lon + lat
  gridded(grid_temp) <- TRUE
  raster_prcp <- raster(grid_temp["prcp"])
  
  list(fecha = as.character(fecha), raster = raster_prcp)
}

# Organizar resultados por año
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

stopCluster(cl)  # liberar recursos

# --- Guardar resultados ---
output_dir <- "D:/S/Serbia1km/Interpolation"
if (!dir.exists(output_dir)) dir.create(output_dir)

for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    names(s) <- fechas_utilizadas
    
    file_name <- file.path(output_dir, paste0("titicaca_RFpuno7_", anio, ".tif"))
    writeRaster(s, filename = file_name, format = "GTiff", overwrite = TRUE)
    
    message("Guardado: ", file_name)
  } else {
    warning("No se guardó el año ", anio, ": capas = ", nlayers(s), ", fechas = ", length(fechas_utilizadas))
  }
}




#####################################################################################################

############################################################################################################

library(dplyr)
library(sf)
library(sp)
library(gstat)
library(randomForest)
library(raster)
library(lubridate)
library(progress)
library(FNN)

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

# --- Asignar variables estáticas dem, twi, sin, cos a la grilla ---
estaciones_sf <- st_as_sf(data %>%
                            select(lon, lat, dem, twi, sin, cos) %>%
                            distinct(),
                          coords = c("lon", "lat"), crs = 4326)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

# Encontrar estación más cercana para cada punto de la grilla (kNN)
est_coords <- st_coordinates(estaciones_sf)
grid_coords <- st_coordinates(grid_sf)

nn_idx <- get.knnx(est_coords, grid_coords, k = 1)$nn.index[,1]
grid$dem <- estaciones_sf$dem[nn_idx]
grid$twi <- estaciones_sf$twi[nn_idx]
grid$sin <- estaciones_sf$sin[nn_idx]
grid$cos <- estaciones_sf$cos[nn_idx]

# Convertir grid a SpatialPoints para interpolaciones
coordinates(grid) <- ~lon + lat
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")

# --- Ciclo sobre fechas para interpolar tmax, tmin y predecir prcp ---
fechas <- sort(unique(data$time))
resultados_por_anio <- list()
fechas_por_anio <- list()

pb <- progress_bar$new(total = length(fechas), format = "[:bar] :percent Día: :current/:total")

for (fecha in fechas) {
  pb$tick()
  
  datos_dia <- filter(data, time == fecha)
  if (nrow(datos_dia) < 5) next
  
  # Interpolar tmax y tmin con IDW para la fecha
  for (var in c("tmax", "tmin")) {
    df_sp <- datos_dia[, c("lon", "lat", var)]
    coordinates(df_sp) <- ~lon + lat
    proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")
    
    idw_out <- idw(as.formula(paste(var, "~ 1")), df_sp, newdata = grid, idp = 2)
    grid[[var]] <- idw_out$var1.pred
  }
  
  # Preparar data.frame para Random Forest
  grid_df <- as.data.frame(grid)
  
  # Ajustar modelo RF con datos del día (puedes ajustar ntree y demás parámetros)
  rf <- randomForest(prcp ~ lon + lat + dem + twi + tmax + tmin + sin + cos,
                     data = datos_dia, ntree = 100)
  
  # Predecir precipitación para la grilla
  grid_df$prcp <- predict(rf, newdata = grid_df)
  
  # Crear raster de prcp
  coordinates(grid_df) <- ~lon + lat
  gridded(grid_df) <- TRUE
  raster_prcp <- raster(grid_df["prcp"])
  
  anio <- as.character(year(fecha))
  if (!anio %in% names(resultados_por_anio)) {
    resultados_por_anio[[anio]] <- stack()
    fechas_por_anio[[anio]] <- c()
  }
  
  resultados_por_anio[[anio]] <- addLayer(resultados_por_anio[[anio]], raster_prcp)
  fechas_por_anio[[anio]] <- c(fechas_por_anio[[anio]], as.character(fecha))
}


# Directorio de salida
output_dir <- "D:/S/Serbia1km/Interpolation"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Guardar .tif con nombre correcto y capas con fechas como nombre
for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    names(s) <- fechas_utilizadas
    
    # Especificar filename completo con extensión
    file_name <- file.path(output_dir, paste0("titicaca_RFpuno_", anio, ".tif"))
    
    writeRaster(
      s,
      filename = file_name,
      format = "GTiff",
      overwrite = TRUE
    )
    
    message("Guardado: ", file_name)
  } else {
    warning("No se guardó el año ", anio, ": capas = ", nlayers(s), ", fechas = ", length(fechas_utilizadas))
  }
}
















































