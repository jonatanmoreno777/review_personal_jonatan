
#Load the libraries
library("ecmwfr")
library(terra)
library(sf)
library(raster)

# set the key 
wf_set_key(key = "92995875-320a-4851-b78c-ddf26bc96cb4")

# Define years, months, and days
Year <- 2020:2021
Month <- sprintf("%02d", 1:12)  # Format months as "01" to "12"

# Define the request for ERA5 monthly averaged data
request <- list(
  dataset_short_name = "reanalysis-era5-land-monthly-means", # month
  product_type = "reanalysis",
  variable = "total_precipitation",
  year = Year,
  month = Month,
  time = "00:00",
  format = "netcdf",               
  area = c(20.0, 72.5, 7.5, 85.0),  #NWSE
  target = "era5-ppt.nc"         
)

# call:

save_path <- "D:/J"

file_1 <- wf_request(
  request  = request,  # the request
  transfer = TRUE,     # download the file
  path     = save_path      # store data in current working directory
)

unzip(file_1, exdir = save_path)
nc_files <- list.files(save_path, pattern = "\\.nc$", full.names = TRUE)

# Renombrar (si existe archivo .nc)
# Renombrar al nombre deseado
# Renombrar el archivo
if (length(nc_files) > 0) {
  old_name <- nc_files[1]
  new_name <- file.path(save_path, "era5-ppt.nc")
  if (file.rename(old_name, new_name)) {
    message("✅ Renombrado exitosamente.")
    
    # 🔄 Actualizar lista después de renombrar
    nc_files <- list.files(save_path, pattern = "\\.nc$", full.names = TRUE)
    print(nc_files)
  } else {
    warning("❌ Falló el renombrado.")
  }
}

#Open the NC file

tp_raster_stack <- brick(nc_files, varname="tp")

nlayers(tp_raster_stack)
plot(tp_raster_stack )

#Visualize the mean layer

mean_layer <- mean(tp_raster_stack , na.rm = TRUE)

# Plot the mean raster layer 
plot(mean_layer, main = "ERA-5 Reanalysis Data (total_precipitation 2020-21)")
     

# Add world map and shapefile layers
maps::map("world", add = TRUE)

### ############  DAILY DATA ERA5-LAND  ########################################

library("ecmwfr")
library(terra)
library(raster)
library(maps)
library(ncdf4)

# 1. Clave de API (solo una vez)
wf_set_key(key = "92995875-320a-4851-b78c-ddf26bc96cb4")

# 2. Crear función para descargar por año
descargar_precipitacion_diaria <- function(year) {
  request <- list(
    dataset_short_name = "reanalysis-era5-land", # daily
    product_type = "reanalysis",
    variable = "total_precipitation",
    year = as.character(year),
    month = sprintf("%02d", 1:12),
    day = sprintf("%02d", 1:31),
    time = "00:00",
    format = "netcdf",
    area = c(-0.1, -81.5, -19.0, -68.5),  # N, W, S, E (Perú)
    target = paste0("era5_tp_", year, ".nc")
  )
  
  # Descargar datos
  wf_request(
    user = "ecmwfr",         # Tu nombre de usuario CDS
    request = request,
    transfer = TRUE,
    path = "D:/J"
  )
}

# 3. Descargar de 1981 a 2024
years <- 1981:2024
lapply(years, descargar_precipitacion_diaria)

# Carpeta con los .zip
zip_folder <- "D:/J/era5-land-mmday"

# Listar todos los archivos .zip
zip_files <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)

# Función para descomprimir y renombrar .nc
descomprimir_y_renombrar <- function(zip_path) {
  # Carpeta destino
  output_dir <- dirname(zip_path)
  
  # Nombre base del .zip sin extensión
  zip_base <- tools::file_path_sans_ext(basename(zip_path))
  
  # Descomprimir temporalmente
  temp_files <- unzip(zip_path, exdir = output_dir)
  
  # Encontrar el .nc extraído (ej. "data_0.nc")
  nc_file <- temp_files[grepl("\\.nc$", temp_files)]
  
  # Nuevo nombre: el mismo que el .zip pero con extensión .nc
  new_nc_path <- file.path(output_dir, paste0(zip_base, ".nc"))
  
  # Renombrar
  file.rename(nc_file, new_nc_path)
  
  return(new_nc_path)
}

# Ejecutar para todos
resultados <- lapply(zip_files, descomprimir_y_renombrar)

cat("✅ Archivos extraídos y renombrados correctamente.\n")


# Leer archivo descargado
nc_path <- "D:/J//era5-land-mmday/era5_tp_2024.nc"
r <- brick(nc_path, varname = "tp")

# Convertir de metros a milímetros
r_mm_day <- r * 1000

# Guardar como nuevo archivo in mm day
writeRaster(r_mm_day, filename = "D:/J/era5-land-mmday/era5mmday-2024.nc", format = "CDF", varname = "ppt", overwrite = TRUE)

tp_raster_stack <- brick("D:/J/era5-land-mmday/era5mmday-2007.nc", varname = "ppt")

mean_layer <- mean(tp_raster_stack , na.rm = TRUE)

# Plot the mean raster layer 
plot(mean_layer, main = "ERA-5 Reanalysis Data (total_precipitation 2020-21)")

# Add world map and shapefile layers
maps::map("world", add = TRUE)


