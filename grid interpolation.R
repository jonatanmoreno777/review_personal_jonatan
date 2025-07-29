
library(ncdf4)    #to read the netcdf data
library(raster)   #to extract times series data from raster
library(rgdal)#to mutate data, apply statistics
library(yaImpute)
library(sp)
library(sf)
library(ggplot2)
library(dplyr)
library(purrr)

rm(list = ls())
#memory.limit(size = 19600)
setwd("D:/Paper_Climate/Data/siguiente paper/of interpolation")
#setwd("D:/X/cuadrantes")
# Load the shapefile
c_shp <- st_read("SHP/PERU.shp")  # c_shp <- st_read("SHP/vectorPeru1.shp")
# c_shp <- st_read("c1/powerc1.shp") 
# Check the CRS of the shapefile
print(st_crs(c_shp))

# Verificar los límites del shapefile
st_bbox(c_shp)


# Reproyect the shp to WGS84 if neccessary
if  (st_crs(c_shp)$epsg != 4326) {
  c_shp <- st_transform(c_shp, crs = 4326)
}

# Define the range and resolution
lat_range <- seq(-18.39, -9.09, by = 0.1)  # Ajuste a los límites de latitud peruvector
lon_range <- seq(-74.89, -68.39, by = 0.1)

lat_range <- seq(-13.54, -10.56, by = 0.05)  # Ajuste a los límites de latitud mantaro
lon_range <- seq(-76.65, -73.92, by = 0.05)

lat_range <- seq(-19.03, 2.07, by = 0.07)  # Ajuste a los límites de latitud of Peru #seq(-19.35, 2.32, by = 0.1) 
lon_range <- seq(-81.34, -66.98, by = 0.07)   # seq(-82.48, -66.67, by = 0.1)

# Create a grid of latitude and longitude
grid <- expand.grid(lon = lon_range, lat= lat_range)

# Convert the grid to an sf object
grid_sf <- st_as_sf(grid, coords = c("lon","lat"), crs = 4326)

# Extract points withn the shp
points_within_pd <- st_intersection(grid_sf, c_shp)

# Guardar los puntos como un archivo shapefile
#st_write(points_within_pd, "D:/X/Peru7OKkm.shp", driver = "ESRI Shapefile")

# plot the grid points-shp
ggplot() +
  geom_sf(data = c_shp, fill=NA, color = "black") +
  geom_sf(data = points_within_pd, color = "blue", size = 1.5, shape= 3) +
  labs(title = "",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

# Save the coordnates to csv (Optional)
points_within_pd_df <- st_coordinates(points_within_pd) %>% as.data.frame()
points_within_pd_df$Grid <- seq_len(nrow(points_within_pd_df))
names(points_within_pd_df) <- c("Lon", "Lat", "Grid")

# Selecciona solo las columnas Lat, Lon y Grid
points_within_pd_df <- points_within_pd_df[, c("Lat", "Lon", "Grid")]

write.csv(points_within_pd_df, "CDBC_Peru7km.csv", row.names = FALSE)  #okkkk
#write_parquet(points_within_pd_df, "CDBC_Peru7km.parquet")
#write.csv(points_within_pd_df, "grid2.csv", row.names = FALSE)
load(file="D:/Paper_Climate/Data/siguiente paper/of interpolation/grid2.rda")
ls()
print(points_within_pd_df)
################# Location of Shape File ###############

setwd("D:/Paper_Climate/Data/siguiente paper/of interpolation")
#pos <- load(file="D:/Paper_Climate/Data/siguiente paper/of interpolation/grid2.rda")

##############     LOAD GRILLED DATA OF PERU #########3###

obsg <- read.csv("A6.csv")#######obsg <- read.csv("POWER-PE7Km/Per-A1.csv")

robs <- rasterFromXYZ(cbind(obsg[, 2], obsg[, 1], obsg[,3])) 

#robs <- rasterFromXYZ(pos[, c(2, 1, 3)]) 

###########################################################
#################D GRILLED DATA .nc #########################
#setwd("D:/Paper_Climate/Data/siguiente paper/ncf4/area_shp")
setwd("D:/X/LANDPERU")#D:/X/RAIN4PE    #PowerNasaPeru1 # #D:/X/LANDPERU
fln <- list.files(pattern = ".nc")

for(f in 1:length(fln)) {
  file_path <- paste0(getwd(), "/", fln[f])  # Construye la ruta completa al archivo
  nc <- nc_open(file_path)                   # Abre el archivo NetCDF
  print(nc)                                  # Imprime el contenido del archivo (dimensiones y variables)
  nc_close(nc)                               # Cierra el archivo NetCDF
}

############################  BILINEAL INTERPOLATION ######################
for(f in 1 : length(fln))  {
  var <- brick(fln[f], varname = "ppt", stopIfNotEqualSpaced = F) # cargar variable
  
  # Interpolación bilineal
  rpre <- resample(var, robs, method= "bilinear")
  
  # Extraer puntos interpolados
  dat <- rasterToPoints(rpre)
  
  # Redondear lon/lat interpolados
  dat[, 1] <- round(dat[, 1], 2)  # lon
  dat[, 2] <- round(dat[, 2], 2)  # lat
  
  # Crear coordenadas redondeadas de observaciones
  obsl <- cbind(round(obsg[,2], 2), round(obsg[,1], 2))  # lon, lat
  
  # Combinar observaciones con interpolados
  datt <- merge(obsl, dat, by= c(1,2))
  
  # Revisar coordenadas de 'obsg' que NO fueron encontradas en los interpolados
  obsl_coords <- paste(obsl[,1], obsl[,2])         # lon lat
  dat_coords <- paste(datt[,1], datt[,2])          # lon lat que sí fueron interpolados
  missing_coords <- obsl[!(obsl_coords %in% dat_coords), ]
  
  # Ordenar resultados según obsg
  datt <- datt[order(match(paste(datt[,1], datt[,2]), obsl_coords)), ]
  
  # Guardar resultados
  if(f == 1) {
    Dat <- datt
  } else {
    Dat <- cbind(Dat, datt[, 3:ncol(datt)])
  }
  
  print(f)
  flush.console()
}
# OKKK
#extent(prec)#extent(robs
print(missing_coords)# see coordinates out of the grilled raster
#view
var
rpre
#View(Dat)

tDat <- round(t(Dat[, 3: ncol(Dat)])*1, 2)
#View(tDat)

hd <- paste(Dat[, 1], "X", Dat [,2], sep = "")
colnames(tDat) <- hd
#View(tDat)
#tDat
# Convertir las fechas y añadir como columna "Date"
tDatok <- data.frame(Date = format(as.Date(gsub("X", "", rownames(tDat)), 
                                         format = "%Y.%m.%d"), "%Y-%m-%d"), tDat, check.names = FALSE)

# Verifica si hay fechas duplicadas
duplicated_dates <- tDatok$Date[duplicated(tDatok$Date)]
if (length(duplicated_dates) > 0) {
  cat("Fechas duplicadas encontradas:\n")
  print(duplicated_dates)
  
  # Elimina filas con fechas duplicadas
  tDatok <- tDatok[!duplicated(tDatok$Date), ]
  cat("Fechas duplicadas eliminadas.\n")
}

# Ordena los datos por la columna "Date" para asegurar el orden cronológico
tDat1 <- tDatok[order(tDatok$Date), ] # of each A1, A2...
tDat2 <- tDatok[order(tDatok$Date), ]
tDat3 <- tDatok[order(tDatok$Date), ]
tDat4 <- tDatok[order(tDatok$Date), ]

lista_df <- list(tDat1, tDat2,tDat3, tDat4)
tDat_mergedOK <- reduce(lista_df, full_join, by = "Date")


setwd("D:/Paper_Climate/Data/siguiente paper/ncf4")
write.csv(tDatok, "D:/X/BC/a3.2prueba.csv", row.names = F)

library(arrow)
# Guardar como archivo Parquet
write_parquet(tDat_mergedOK, "..../Peru7kmOK.parquet")

#######################################################
#############  SAVE OF BIAS CORRECTION ##############

# Obtener los nombres de las columnas (coordenadas)
coordinates <- colnames(tDat1)[-1]  # Sin la columna 'Date'

# Extraer las latitudes y longitudes de los nombres de las columnas
latitudes <- as.numeric(gsub(".*X(.*)", "\\1", coordinates))
longitudes <- as.numeric(gsub("(.*)X.*", "\\1", coordinates))

# Crear las filas de latitud y longitud
lat_row <- c("N", latitudes)
lon_row <- c("E", longitudes)

# Extraer las fechas y datos de precipitación
dates <- tDat1$Date
precipitation_data <- as.matrix(tDat1[, -1])

# Combinar todo en un solo DataFrame
final_output <- rbind(lat_row, lon_row, cbind(dates, precipitation_data)) # full time 1981 until 2024
write.csv(final_output, "D:/X/BC/A6okkk.csv", row.names = F, col.names = F)
# saved of historical and future data
fechas <- as.Date(final_output[-c(1, 2), 1])

filtro <- fechas >= as.Date("2016-01-01") & fechas <= as.Date("2024-12-31") # put time of historical and future data
#                   as.Date("2016-01-01") & fechas <= as.Date("2024-12-31")
# Crear nuevo DataFrame con N, E y solo las fechas filtradas
final_output_hist_futur <- rbind(   # hist or future data
  final_output[1, ],          # Fila N
  final_output[2, ],          # Fila E
  final_output[-c(1, 2), ][filtro, ]
)


# save
cat(
  apply(final_output_hist_futur, 1, paste, collapse = ","),
  file = "D:/X/BC/A13hist.csv",
  sep = "\n"
)

###################### GRILLA OF PERU ###########################################

#memory.limit(size = 19600)
setwd("D:/Paper_Climate/Data/siguiente paper/of interpolation")

# Definir el rango total de Perú
# Coordenadas ajustadas para cubrir Perú y Ecuador
# Coordenadas ajustadas según el bounding box
lat_min <- -18.35  # Latitud mínima -19.03
lat_max <- -0.04  # Latitud máxima   2.07
lon_min <- -81.35  # Longitud mínima   -81.34
lon_max <- -68.65  # Longitud máxima       -66.98

# Tamaño de cada subregión (en grados)
lat_step <- 4.10   # Paso de latitud  # 3.0  
lon_step <- 4.25   # Paso de longitud # Paso de longitud# 6.5

# Crear un data.frame con las coordenadas de las subregiones
lat_seq <- seq(lat_min, lat_max, by = lat_step)
lon_seq <- seq(lon_min, lon_max, by = lon_step)

# Crear un data.frame para almacenar las subregiones
subregiones_df <- data.frame(
  lon_min = rep(lon_seq, each = length(lat_seq)),
  lat_min = rep(lat_seq, times = length(lon_seq)),
  lon_max = rep(lon_seq, each = length(lat_seq)) + lon_step,
  lat_max = rep(lat_seq, times = length(lon_seq)) + lat_step
)

# Crear un objeto sf para las subregiones
subregiones_df$geometry <- mapply(function(lon_min, lat_min, lon_max, lat_max) {
  st_polygon(list(matrix(c(
    lon_min, lat_min,
    lon_max, lat_min,
    lon_max, lat_max,
    lon_min, lat_max,
    lon_min, lat_min
  ), ncol = 2, byrow = TRUE)))
}, subregiones_df$lon_min, subregiones_df$lat_min, subregiones_df$lon_max, subregiones_df$lat_max, SIMPLIFY = FALSE)

# Convertir el data.frame a un objeto sf y definir CRS
subregiones_sf <- st_sf(subregiones_df)
st_crs(subregiones_sf) <- 4326  # Definir CRS WGS 84
# Guardar el objeto subregiones_sf en un archivo shapefile
#st_write(subregiones_sf, "D:/X/subregiones7km.shp", delete_dsn = TRUE)

# Leer el archivo shapefile y definir CRS si es necesario
shapefile_path <- "SHP/PERU.shp"  # Cambia esto a la ruta de tu shapefile  shapefile_path <- "SHP/Area_cut.shp"  
shapefile_sf <- st_read(shapefile_path)

# Asegurarse de que el shapefile tenga CRS WGS 84
if (is.na(st_crs(shapefile_sf))) {
  st_crs(shapefile_sf) <- 4326  # Definir CRS WGS 84 si no está definido
}

# Crear el mapa
ggplot() +
  geom_sf(data = subregiones_sf, fill = NA, color = "blue", lwd = 0.5) +
  geom_sf(data = shapefile_sf, fill = "lightgray", color = "black", lwd = 0.5) +
  coord_sf() +
  labs(title = "Subregiones de 8x8 grados en Perú",
       x = "Longitud",
       y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "none")

################   of 7 columns and 3 rows #####################

# Número de divisiones deseadas
num_filas <- 7
num_columnas <- 2

# Calcular pasos dinámicos para latitud y longitud
lat_step <- (lat_max - lat_min) / num_filas
lon_step <- (lon_max - lon_min) / num_columnas

# Crear secuencias de latitudes y longitudes
lat_seq <- seq(lat_min, lat_max, by = lat_step)
lon_seq <- seq(lon_min, lon_max, by = lon_step)

# Crear un data.frame para almacenar las subregiones
subregiones_df <- data.frame(
  lon_min = rep(lon_seq[-length(lon_seq)], each = length(lat_seq) - 1),
  lat_min = rep(lat_seq[-length(lat_seq)], times = length(lon_seq) - 1),
  lon_max = rep(lon_seq[-1], each = length(lat_seq) - 1),
  lat_max = rep(lat_seq[-1], times = length(lon_seq) - 1)
)

# Crear geometrías sf
subregiones_df$geometry <- mapply(function(lon_min, lat_min, lon_max, lat_max) {
  st_polygon(list(matrix(c(
    lon_min, lat_min,
    lon_max, lat_min,
    lon_max, lat_max,
    lon_min, lat_max,
    lon_min, lat_min
  ), ncol = 2, byrow = TRUE)))
}, subregiones_df$lon_min, subregiones_df$lat_min, subregiones_df$lon_max, subregiones_df$lat_max, SIMPLIFY = FALSE)

# Convertir el data.frame a un objeto sf y definir CRS
subregiones_sf <- st_sf(subregiones_df)
st_crs(subregiones_sf) <- 4326  # Definir CRS WGS 84
# Guardar el objeto subregiones_sf en un archivo shapefile
st_write(subregiones_sf, "D:/X/PERU7km7x2.shp", delete_dsn = TRUE)

# Leer el shapefile de referencia
shapefile_path <- "SHP/PERU.shp"  # Ruta al shapefile
shapefile_sf <- st_read(shapefile_path)

# Verificar y definir CRS del shapefile
if (is.na(st_crs(shapefile_sf))) {
  st_crs(shapefile_sf) <- 4326  # Definir CRS WGS 84
}

# Crear el mapa
library(ggplot2)
ggplot() +
  geom_sf(data = subregiones_sf, fill = NA, color = "blue", lwd = 0.5) +
  geom_sf(data = shapefile_sf, fill = "lightgray", color = "black", lwd = 0.5) +
  coord_sf() +
  labs(title = "Subregiones divididas en 2 columnas y 7 filas",
       x = "Longitud",
       y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "none")

########  RAMDOM FOREST #################################

# Establecer el directorio de trabajo y listar los archivos NetCDF
setwd("D:/Paper_Climate/Data/siguiente paper/ncf4/area_shp") # 
fln <- list.files(pattern = ".nc")

library(raster)
library(randomForest)
library(ncdf4)

# Nombre de la variable y tiempo
# Nombre de la variable y tiempo
varname <- "PRECTOTCORR"  # Cambia por el nombre de la variable que necesitas

# Cargar los datos de observación
# Cargar los datos de observación
obsg <- read.csv("ruta_a_tus_obsg.csv")  # Cambia el nombre del archivo según corresponda
colnames(obsg) <- c("lat", "lon", "Grid")  # Asegúrate de que las columnas están correctas

# Inicializar lista para almacenar predicciones
predictions_list <- list()

# Procesar cada archivo NetCDF
for (f in 1:length(fln)) {
  # Abre el archivo NetCDF
  nc <- nc_open(fln[f])
  
  # Verifica si la variable existe en el archivo
  if (varname %in% names(nc$var)) {
    # Lee la variable seleccionada
    nc_var <- ncvar_get(nc, varname)
    time <- ncvar_get(nc, "time")  # Cambia "time" al nombre correcto en tu archivo NetCDF
    
    # Convierte el tiempo a una fecha legible
    time_units <- ncatt_get(nc, "time", "units")$value
    time_origin <- as.Date(sub(".*since ", "", time_units))
    time <- time_origin + time  # Ajusta según sea necesario
    
    # Dimensiones espaciales (lon y lat)
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    
    # Crear un objeto brick desde la variable leída
    rasters <- list()
    for (t in 1:dim(nc_var)[3]) {  # Itera sobre el tiempo
      layer <- nc_var[, , t]
      r <- raster(layer, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84"))
      rasters[[t]] <- r
    }
    
    # Combina los rasters en un brick
    prec <- brick(rasters)
    
    # Extraer datos de los rasters directamente en las ubicaciones de obsg
    for (t in 1:dim(prec)[3]) {  # Itera sobre el tiempo
      values <- extract(prec[[t]], obsg[, c("lon", "lat")])
      train_data <- data.frame(lon = obsg$lon, lat = obsg$lat, values = values)
      
      # Eliminar filas con NA
      train_data <- na.omit(train_data)
      
      # Entrenar el modelo Random Forest solo si hay datos válidos
      if (nrow(train_data) > 0) {
        rf_model <- randomForest(values ~ lon + lat, data = train_data)
        
        # Predecir usando el conjunto de datos 'obsg'
        predictions <- predict(rf_model, newdata = obsg[, c("lon", "lat")])
        
        # Almacenar predicciones
        predictions_list[[t]] <- predictions
      } else {
        predictions_list[[t]] <- rep(NA, nrow(obsg))  # Placeholder para predicciones
      }
    }
    
    print(f)
    flush.console()
  } else {
    cat("La variable", varname, "no está disponible en el archivo:", fln[f], "\n")
  }
  
  # Cierra el archivo NetCDF después de usarlo
  nc_close(nc)
}

########### BILINEAR ###########################


##############
# Bucle para procesar cada archivo
for (f in 1:length(fln)) {
  
  # Leer el archivo NetCDF como RasterBrick
  var <- brick(fln[f], varname = "ppt", stopIfNotEqualSpaced = FALSE)  # Cambia "ppt" si es necesario
  
  # Interpolación bilineal al raster de referencia 'robs'
  rpre <- resample(var, robs, method = "bilinear")
  
  # Convertir el raster interpolado a puntos (lon, lat, valores)
  dat <- rasterToPoints(rpre)  # columnas: lon, lat, valores de cada capa temporal
  dat <- as.data.frame(dat)
  
  # Redondear las coordenadas (lon, lat) a dos decimales
  dat$lon <- round(dat$x, 2)
  dat$lat <- round(dat$y, 2)
  
  # Crear las coordenadas de referencia de 'obsg' también redondeadas
  obsl <- data.frame(
    lon = round(obsg$Lon, 2),
    lat = round(obsg$Lat, 2)
  )
  
  # Combinar datos interpolados con los puntos de observación (coincidencia exacta de coordenadas)
  datt <- merge(obsl, dat, by = c("lon", "lat"))
  
  # Ordenar para que coincida exactamente con el orden de 'obsg'
  index <- match(paste(obsl$lon, obsl$lat), paste(datt$lon, datt$lat))
  datt <- datt[index, ]
  
  # Guardar los datos interpolados (solo valores, sin coordenadas)
  if (f == 1) {
    Dat <- datt[, -(1:2)]  # Excluye lon y lat
  } else {
    Dat <- cbind(Dat, datt[, -(1:2)])
  }
  
  cat("Year", f, "Processed\n")
  flush.console()
}

dat_coords <- paste(dat$lon, dat$lat)       # ya están redondeadas
obsl_coords <- paste(obsl$lon, obsl$lat)    # también redondeadas

# Filtrar coordenadas de 'obsl' que NO están en 'dat'
obsl_not_in_dat <- obsl[!(obsl_coords %in% dat_coords), ]
obsl_not_in_dat

var
rpre
#View(Dat)

tDat <- round(t(Dat[, 3: ncol(Dat)])*1, 2)
#View(tDat)

hd <- paste0(
  format(round(as.numeric(Dat[, 1]), 2), nsmall = 2),
  "X",
  format(round(as.numeric(Dat[, 2]), 2), nsmall = 2)
)

#hd <- paste(Dat[, 1], "X", Dat [,2], sep = "")
colnames(tDat) <- hd
#View(tDat)
#tDat
# Convertir las fechas y añadir como columna "Date"
tDatok <- data.frame(Date = format(as.Date(gsub("X", "", rownames(tDat)), 
                                           format = "%Y.%m.%d"), "%Y-%m-%d"), tDat, check.names = FALSE)

# Verifica si hay fechas duplicadas
duplicated_dates <- tDatok$Date[duplicated(tDatok$Date)]
if (length(duplicated_dates) > 0) {
  cat("Fechas duplicadas encontradas:\n")
  print(duplicated_dates)
  
  # Elimina filas con fechas duplicadas
  tDatok <- tDatok[!duplicated(tDatok$Date), ]
  cat("Fechas duplicadas eliminadas.\n")
}

# Ordena los datos por la columna "Date" para asegurar el orden cronológico
tDat1 <- tDatok[order(tDatok$Date), ] 
tDat2 <- tDat1[!is.na(tDat1$Date), ] 
# Muestra las últimas 3 filas para verificar


coordinates <- colnames(tDat2)[-1]  # Sin la columna 'Date'

# Extraer las latitudes y longitudes de los nombres de las columnas
latitudes <- as.numeric(gsub(".*X(.*)", "\\1", coordinates))
longitudes <- as.numeric(gsub("(.*)X.*", "\\1", coordinates))

# Crear las filas de latitud y longitud
lat_row <- c("N", latitudes)
lon_row <- c("E", longitudes)

# Extraer las fechas y datos de precipitación
dates <- tDat2$Date
precipitation_data <- as.matrix(tDat2[, -1])

# Combinar todo en un solo DataFrame
final_output <- rbind(lat_row, lon_row, cbind(dates, precipitation_data)) # full time 1981 until 2024
write.csv(final_output, "D:/X/BC/mor7.csv", row.names = F, col.names = F)



