library(raster)
library(sp)
library(randomForest)
library(dplyr)
library(lubridate)
library(progress)
# Cargar datos
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_prec.csv")
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# Crear grilla
lon_seq <- sort(seq(min(data$Lon), max(data$Lon), by = 0.05))
lat_seq <- sort(seq(min(data$Lat), max(data$Lat), by = 0.05))
#grid <- expand.grid(Lon = lon_seq, Lat = lat_seq)

########## #####################shp #############################################
# Leer cuenca y generar grilla dentro del polígono
library(sf)
library(dplyr)

cuenca <- st_read("D:/S/Serbia1km/Interpolation/Titicaca_border/Titicaca.shp")  # Ajusta ruta
cuenca <- st_transform(cuenca, crs = 4326)
bbox <- st_bbox(cuenca)
lon_seq <- seq(bbox["xmin"], bbox["xmax"], by = 0.05)
lat_seq <- seq(bbox["ymin"], bbox["ymax"], by = 0.05)
grid <- expand.grid(Lon = lon_seq, Lat = lat_seq)
grid_sf <- st_as_sf(grid, coords = c("Lon", "Lat"), crs = 4326)
grid_dentro <- st_join(grid_sf, cuenca, join = st_within, left = FALSE)
coords <- st_coordinates(grid_dentro)
grid <- data.frame(Lon = coords[, 1], Lat = coords[, 2])

################################################################################

# Fechas únicas ordenadas
fechas <- sort(unique(data$Date))

# Inicializar resultados
resultados_por_anio <- list()
fechas_por_anio <- list()

# Barra de progreso
pb <- progress_bar$new(total = length(fechas), format = "[:bar] :percent Día: :current/:total")

for (fecha in fechas) {
  pb$tick()
  
  datos_dia <- filter(data, Date == fecha)
  if (nrow(datos_dia) < 5) next  #  avoid missing 
  
  rf <- randomForest(pr ~ Lon + Lat, data = datos_dia, ntree = 100)
  
  grid_temp <- grid
  grid_temp$pr <- predict(rf, newdata = grid_temp)
  
  coordinates(grid_temp) <- ~Lon + Lat
  gridded(grid_temp) <- TRUE
  raster_dia <- raster(grid_temp["pr"])
  
  anio <- as.character(year(fecha))
  
  if (!anio %in% names(resultados_por_anio)) {
    resultados_por_anio[[anio]] <- stack()
    fechas_por_anio[[anio]] <- c()
  }
  
  resultados_por_anio[[anio]] <- addLayer(resultados_por_anio[[anio]], raster_dia)
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
    file_name <- file.path(output_dir, paste0("titicaca_RF1_", anio, ".tif"))
    
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


# Cargar raster ya guardado
r_1981 <- stack("D:/S/Serbia1km/Interpolation/titicaca_RF1_1970.tif")
plot(r_1981)
# Ver fechas de las capas
names(r_1981)[1:5]  # primeras fechas

# Graficar una capa específica
plot(r_1981[[1]], main = names(r_1981)[1], xlab = "Longitud", ylab = "Latitud")

####################################################################################


# Directorio de salida
output_dir <- "D:/S/Serbia1km/Interpolation3"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Guardar raster y fechas por año
for (anio in names(resultados_por_anio)) {
  s <- resultados_por_anio[[anio]]
  fechas_utilizadas <- fechas_por_anio[[anio]]
  
  if (nlayers(s) > 0 && length(fechas_utilizadas) == nlayers(s)) {
    
    # Convertir y ordenar fechas
    fechas_convertidas <- as.Date(as.numeric(fechas_utilizadas), origin = "1970-01-01")
    orden <- order(fechas_convertidas)
    s <- s[[orden]]
    fechas_ordenadas <- format(fechas_convertidas[orden], "%Y-%m-%d")
    names(s) <- fechas_ordenadas
    
    # Guardar raster
    file_name <- file.path(output_dir, paste0("precipitacion_RF2_", anio, ".tif"))
    writeRaster(s, filename = file_name, format = "GTiff", overwrite = TRUE)
    message("Guardado: ", file_name)
    
    # Guardar CSV con fechas
    #write.csv(data.frame(Date = fechas_ordenadas),
              #file = file.path(output_dir, paste0("fechas_RF1_", anio, ".csv")),
              #row.names = FALSE)
    
  } else {
    warning("No se guardó el año ", anio, ": capas = ", nlayers(s), ", fechas = ", length(fechas_utilizadas))
  }
}


