
library(ncdf4)    #to read the netcdf data
library(raster)   #to extract times series data from raster
library(rgdal)#to mutate data, apply statistics
library(yaImpute)
library(sp)
library(sf)
library(ggplot2)
library(dplyr)

rm(list = ls())
#memory.limit(size = 19600)
setwd("D:/Paper_Climate/Data/siguiente paper/of interpolation")
#setwd("D:/X/cuadrantes")
# Load the shapefile
c_shp <- st_read("SHP/Area_cut.shp")  # c_shp <- st_read("SHP/vectorPeru1.shp")
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
#st_write(points_within_pd, "Peru5km.shp", driver = "ESRI Shapefile")

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

write.csv(points_within_pd_df, "CDBC_Peru-Ecuad7km.csv", row.names = FALSE)  #okkkk
#write_parquet(points_within_pd_df, "CDBC_Peru7km.parquet")
#write.csv(points_within_pd_df, "grid2.csv", row.names = FALSE)
load(file="D:/Paper_Climate/Data/siguiente paper/of interpolation/grid2.rda")
ls()
print(points_within_pd_df)
################# Location of Shape File ###############

setwd("D:/Paper_Climate/Data/siguiente paper/of interpolation")
#pos <- load(file="D:/Paper_Climate/Data/siguiente paper/of interpolation/grid2.rda")
#pos <- get(pos)
#pos <- read.csv ("gridok2.csv", header= T)
#pos <- pos [, 1:2]

#head(pos)

obsg <- read.csv("POWER-PE7Km/Per-A2.csv")
#obsg  <- read_parquet("CDBC_Peru8km.parquet")
#head(obsg)

robs <- rasterFromXYZ(cbind(obsg[, 2], obsg[, 1], obsg[,3])) 

#robs <- rasterFromXYZ(pos[, c(2, 1, 3)]) 

###########################################################
###########################################################
###########################################################

setwd("D:/Paper_Climate/Data/siguiente paper/ncf4/area_shp")
setwd("D:/X/RAIN4PE")#D:/X/RAIN4PE    #PowerNasaPeru1
fln <- list.files(pattern = ".nc")

for(f in 1:length(fln)) {
  file_path <- paste0(getwd(), "/", fln[f])  # Construye la ruta completa al archivo
  nc <- nc_open(file_path)                   # Abre el archivo NetCDF
  print(nc)                                  # Imprime el contenido del archivo (dimensiones y variables)
  nc_close(nc)                               # Cierra el archivo NetCDF
}


for(f in 1 : length(fln))  {
var <- brick(fln[f], stopIfNotEqualSpaced = F)
# run write "prec" in console to get resolution of GCM
rpre <- resample(var, robs, method= "bilinear")
dat <- rasterToPoints(rpre)

obsl <- cbind(obsg[,2], obsg[, 1])
datt <- merge(obsl, dat, by= c(1,2))

if(f==1) Dat <- datt
if(f>1) Dat <- cbind(Dat, datt[, 3: ncol(datt)])
print(f)
flush.console()
}

nc <- nc_open(fln[f])
print(nc$var)  # Imprime las variables disponibles
nc_close(nc)

############################  BILINEAL INTERPOLATION ######################

for(f in 1 : length(fln))  {
    #var <- brick(fln[f], stopIfNotEqualSpaced = F)  # NO HAY VARIABLE
    var <- brick(fln[f], varname = "pcp", stopIfNotEqualSpaced = F) #PRECTOTCORR
    # Realizar la interpolación bilineal
    rpre <- resample(var, robs, method= "bilinear")
    
    # Convertir los datos interpolados en puntos (lat, lon, valor)
    dat <- rasterToPoints(rpre)
    
    # Redondear las coordenadas latitud y longitud para que coincidan con los puntos del archivo 'obsg'
    dat[, 1] <- round(dat[, 1], 2)  # Redondear la latitud
    dat[, 2] <- round(dat[, 2], 2)  # Redondear la longitud
    
    # Crear las coordenadas de las observaciones
    obsl <- cbind(round(obsg[,2], 2), round(obsg[, 1], 2))  # Redondear también las coordenadas de 'obsg'
    obsl <- cbind(obsg[,2], obsg[, 1])
    # Combinar (merge) los puntos de las observaciones con los datos interpolados
    datt <- merge(obsl, dat, by= c(1,2))
    
    # Ordenar el resultado final para que coincida con el orden de 'obsg'
    datt <- datt[order(match(paste(datt[, 1], datt[, 2]), paste(obsl[, 1], obsl[, 2]))), ]
    
    # Almacenar los datos interpolados
    if(f==1) Dat <- datt
    if(f>1) Dat <- cbind(Dat, datt[, 3: ncol(datt)])
    print(f)
    flush.console()
}  # OKKK

#extent(prec)
#extent(robs)
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
tDat3 <- data.frame(Date = format(as.Date(gsub("X", "", rownames(tDat)), 
                                         format = "%Y.%m.%d"), "%Y-%m-%d"), tDat, check.names = FALSE)

# Verifica si hay fechas duplicadas
duplicated_dates <- tDat3$Date[duplicated(tDat3$Date)]
if (length(duplicated_dates) > 0) {
  cat("Fechas duplicadas encontradas:\n")
  print(duplicated_dates)
  
  # Elimina filas con fechas duplicadas
  tDat3 <- tDat3[!duplicated(tDat3$Date), ]
  cat("Fechas duplicadas eliminadas.\n")
}

# Ordena los datos por la columna "Date" para asegurar el orden cronológico
tDat3 <- tDat3[order(tDat3$Date), ]

# Unir tDat2 y tDat3 utilizando la columna "Date" como clave
tDat_merged <- merge(tDat2, tDat3, by = "Date", all = TRUE)

setwd("D:/Paper_Climate/Data/siguiente paper/ncf4")
#write.csv(tDat3, "D:/Paper_Climate/Data/siguiente paper/of interpolation/POWER-PE7Km//BC-PER-A1prueba.csv", row.names = F)

library(arrow)

# Guardar como archivo Parquet
write_parquet(tDat_merged, "CDBC-POWERPeru5km_c1-c3.parquet")

##############333333333333333333333333333333333333333333333333
#############  SAVE OF BIAS CORRECTION ##############

# Obtener los nombres de las columnas (coordenadas)
coordinates <- colnames(tDat3)[-1]  # Sin la columna 'Date'

# Extraer las latitudes y longitudes de los nombres de las columnas
latitudes <- as.numeric(gsub(".*X(.*)", "\\1", coordinates))
longitudes <- as.numeric(gsub("(.*)X.*", "\\1", coordinates))

# Crear las filas de latitud y longitud
lat_row <- c("N", latitudes)
lon_row <- c("E", longitudes)

# Extraer las fechas y datos de precipitación
dates <- tDat3$Date
precipitation_data <- as.matrix(tDat3[, -1])

# Combinar todo en un solo DataFrame
final_output <- rbind(lat_row, lon_row, cbind(dates, precipitation_data))

write.csv(final_output, "D:/Paper_Climate/Data/siguiente paper/BC-POWER-PE7Km/PERURAIN4PE-A2-BC.csv", row.names = FALSE, col.names = FALSE)

# Inicializa la lista de nombres de columnas basada en las ubicaciones

###################### GRILLA OF PERU ###########################################

#memory.limit(size = 19600)
setwd("D:/Paper_Climate/Data/siguiente paper/of interpolation")

# Definir el rango total de Perú
# Coordenadas ajustadas para cubrir Perú y Ecuador
# Coordenadas ajustadas según el bounding box
lat_min <- -19.03  # Latitud mínima
lat_max <- 2.07   # Latitud máxima
lon_min <- -81.34  # Longitud mínima
lon_max <- -66.98  # Longitud máxima

# Tamaño de cada subregión (en grados)
lat_step <- 3.0   # Paso de latitud  # 3.0  
lon_step <- 6.5   # Paso de longitud # Paso de longitud# 6.5

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
shapefile_path <- "SHP/Area_cut.shp"  # Cambia esto a la ruta de tu shapefile
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
num_filas <- 3
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

# Leer el shapefile de referencia
shapefile_path <- "SHP/Area_cut.shp"  # Ruta al shapefile
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































# Definir el directorio y cargar las observaciones
setwd("D:/Paper_Climate/Data/siguiente paper/of interpolation")
obsg <- read.csv("CDBC_Mantaro.csv")

# Crear un raster a partir de las observaciones (sin redondear)
robs <- rasterFromXYZ(cbind(obsg[, 2], obsg[, 1], obsg[, 3]))

# Cambiar el directorio a la carpeta donde se encuentran los archivos NetCDF
setwd("D:/Paper_Climate/Data/siguiente paper/BIAS-MERRA2/PCP")
fln <- list.files(pattern = ".nc")

# Iterar sobre los archivos NetCDF
for(f in 1:length(fln)) {
  file_path <- paste0(getwd(), "/", fln[f])  # Construir la ruta completa al archivo
  nc <- nc_open(file_path)                   # Abrir el archivo NetCDF
  print(nc)                                  # Imprimir el contenido del archivo (dimensiones y variables)
  nc_close(nc)                               # Cerrar el archivo NetCDF
}

# Abrir el último archivo NetCDF y obtener las variables disponibles
nc <- nc_open(fln[f])
print(nc$var)  # Imprime las variables disponibles
nc_close(nc)

############################  INTERPOLACIÓN BILINEAL ######################

for(f in 1 : length(fln))  {
  # Cargar la variable de precipitación (PRECTOTCORR) desde el archivo NetCDF
  var <- brick(fln[f], varname = "PRECTOTCORR", stopIfNotEqualSpaced = F)
  
  # Realizar la interpolación bilineal
  rpre <- resample(var, robs, method= "bilinear")
  
  # Convertir los datos interpolados en puntos (lat, lon, valor)
  dat <- rasterToPoints(rpre)
  
  # Usar las coordenadas originales sin redondear
  # Crear las coordenadas de las observaciones sin redondear
  obsl <- cbind(obsg[, 2], obsg[, 1])  # Usar las coordenadas originales de 'obsg'
  
  # Combinar (merge) los puntos de las observaciones con los datos interpolados
  datt <- merge(obsl, dat, by = c(1, 2))
  
  # Ordenar el resultado final para que coincida con el orden de 'obsg'
  datt <- datt[order(match(paste(datt[, 1], datt[, 2]), paste(obsl[, 1], obsl[, 2]))), ]
  
  # Almacenar los datos interpolados
  if(f == 1) {
    Dat <- datt
  } else {
    Dat <- cbind(Dat, datt[, 3:ncol(datt)])  # Añadir las nuevas columnas sin repetir lat/lon
  }
  
  print(f)
  flush.console()
}

# Transformar los datos en una tabla con las coordenadas como nombres de columna
tDat <- round(t(Dat[, 3:ncol(Dat)]) * 1, 2)

# Crear los nombres de las columnas a partir de las coordenadas
hd <- paste(Dat[, 1], "X", Dat[, 2], sep = "")
colnames(tDat) <- hd

# Convertir las fechas y añadir como columna "Date"
tDat2 <- data.frame(Date = format(as.Date(gsub("X", "", rownames(tDat)), 
                                          format = "%Y.%m.%d"), "%Y-%m-%d"), tDat, check.names = FALSE)

# Guardar los resultados en un archivo CSV
setwd("D:/Paper_Climate/Data/siguiente paper/ncf4")
write.csv(tDat2, "CDBC-mantaro-7.csv", row.names = FALSE)














#        okkkkkk
for (f in 1:length(fln)) {
  var <- brick(fln[f], varname = "PRECTOTCORR", stopIfNotEqualSpaced = F)  # Carga NetCDF
  rpre <- resample(var, robs, method = "bilinear")  # Interpolación bilineal
  dat <- rasterToPoints(rpre)  # Extraer datos interpolados
  dat[, 1] <- round(dat[, 1], 2)  # Redondear lat
  dat[, 2] <- round(dat[, 2], 2)  # Redondear lon
  
  # Coordenadas de observación
  obsl <- cbind(round(obsg[, 2], 2), round(obsg[, 1], 2))
  datt <- merge(obsl, dat, by = c(1, 2))  # Combinar datos
  
  datt <- datt[order(match(paste(datt[, 1], datt[, 2]), paste(obsl[, 1], obsl[, 2]))), ]
  if (f == 1) Dat <- datt
  if (f > 1) Dat <- cbind(Dat, datt[, 3:ncol(datt)])
  print(f)
  flush.console()
}

# Transponer y guardar
tDat <- round(t(Dat[, 3:ncol(Dat)]) * 1, 2)
hd <- paste(Dat[, 1], "X", Dat[, 2], sep = "")
colnames(tDat) <- hd

# Crear DataFrame con fechas
tDat3 <- data.frame(Date = format(as.Date(gsub("X", "", rownames(tDat)), format = "%Y.%m.%d"), "%Y-%m-%d"), tDat, check.names = FALSE)

