library("nasapower")
library("readr")

# Ruta al archivo CSV

# Ruta del archivo CSV que contiene la información de los puntos
ruta_archivo <- "D:/Paper_Climate/Data/siguiente paper/cfsr_pisco/CFSR_prueba.csv"

# Leer el archivo CSV con la información de los puntos
datos_puntos <- read.csv(ruta_archivo)

# Ruta de la carpeta donde se guardarán los archivos CSV
folder_path <- "D:/Paper_Climate/Data/siguiente paper/cfsr_pisco"

# Si la carpeta no existe, crearla
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

# Descargar y guardar datos para cada punto
for (i in 1:nrow(datos_puntos)) {
  nombre <- datos_puntos[i, "NAME"]
  lonlat <- c(datos_puntos[i, "LONG"], datos_puntos[i, "LAT"])
  
  tryCatch({
    # Obtener datos
    datos <- get_power(
      community = "ag",
      lonlat = lonlat,
      pars = c("WS2M", "ALLSKY_SFC_SW_DWN", "RH2M", "T2M", "PRECTOTCORR"),
      dates = c("1985-01-01", "2000-01-01"),
      temporal_api = "daily"
    )
    
    # Nombre del archivo CSV
    file_name <- paste0(nombre, "_datos.csv")
    
    # Ruta completa del archivo CSV
    file_path <- file.path(folder_path, file_name)
    
    # Guardar datos en el archivo CSV
    write.csv(datos, file = file_path, row.names = FALSE)
    
    print(paste("Datos para", nombre, "guardados en", file_path))
  }, error = function(e) {
    print(paste("Error al procesar datos para", nombre, ":", e$message))
  })
}

#  https://power.larc.nasa.gov/api/system/manager/system/groupings   "here encuentras las variables"


https://github.com/ropensci/nasapower



# Definir la región de interés (en este caso, un rectángulo de longitud y latitud)
library("nasapower")
library("ncdf4")

# Definir la región de interés (en este caso, un rectángulo de longitud y latitud)
lonlat <- c(-82.48, -19.35, -66.65, 2.32)

# Parámetros a obtener
pars <- c("RH2M", "T2M")

# Obtener los datos
daily_region_ag <- get_power(
  community = "ag",
  lonlat = lonlat,
  pars = pars,
  dates = c("1985-01-01", "1985-01-31"),
  temporal_api = "daily"

)



https://docs.ropensci.org/nasapower/articles/nasapower.html



ruta_archivo <- "D:/Paper_Climate/Data/siguiente paper/cfsr_pisco/CFSR_prueba.csv"

# Leer el archivo CSV con la información de los puntos
datos_puntos <- read.csv(ruta_archivo)

# Ruta de la carpeta donde se guardarán los archivos CSV
folder_path <- "D:/Paper_Climate/Data/siguiente paper/cfsr_pisco"

# Parámetros de interés
parametros <- c("WS2M", "ALLSKY_SFC_SW_DWN", "RH2M")

# Si la carpeta no existe, crearla
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

# Descargar y guardar datos para cada punto y parámetro
for (parametro in parametros) {
  # Data frame para almacenar los datos del parámetro actual
  datos_totales <- data.frame()
  
  for (i in 1:nrow(datos_puntos)) {
    nombre <- datos_puntos[i, "NAME"]  # Nombre de la estación
    lonlat <- c(datos_puntos[i, "LONG"], datos_puntos[i, "LAT"])
    
    tryCatch({
      # Obtener datos
      datos <- get_power(
        community = "ag",
        lonlat = lonlat,
        pars = parametro,
        dates = c("1985-01-01", "2000-01-01"),
        temporal_api = "daily"
      )
      
      # Agregar columna de estación
      datos$Station <- nombre
      
      # Almacenar datos en el data frame total
      datos_totales <- rbind(datos_totales, datos)
      
      print(paste("Datos para", nombre, "(", parametro, ") descargados y agregados"))
    }, error = function(e) {
      print(paste("Error al procesar datos para", nombre, "(", parametro, ") :", e$message))
    })
  }
  
  # Eliminar columnas no deseadas
  datos_totales <- subset(datos_totales, select = -c(LON, LAT, YEAR, MM, DD, DOY))
  
  # Nombre del archivo CSV para los datos totales del parámetro actual
  file_name_total <- paste0(parametro, "_datos_totales.csv")
  file_path_total <- file.path(folder_path, file_name_total)
  
  # Guardar datos totales del parámetro actual en un archivo CSV
  write.csv(datos_totales, file = file_path_total, row.names = FALSE)
  print(paste("Datos totales para el parámetro", parametro, "guardados en", file_path_total))
}




ruta_archivo <- "D:/Paper_Climate/Data/siguiente paper/cfsr_pisco/CFSR_prueba.csv"

# Leer el archivo CSV con la información de los puntos
datos_puntos <- read.csv(ruta_archivo)

# Ruta de la carpeta donde se guardarán los archivos CSV
folder_path <- "D:/Paper_Climate/Data/siguiente paper/cfsr_pisco"

# Parámetros de interés
parametros <- c("WS2M", "ALLSKY_SFC_SW_DWN", "RH2M")

# Si la carpeta no existe, crearla
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

# Descargar y guardar datos para cada punto y parámetro
for (parametro in parametros) {
  # Data frame para almacenar los datos del parámetro actual
  datos_totales <- data.frame()
  
  for (i in 1:nrow(datos_puntos)) {
    nombre <- datos_puntos[i, "NAME"]  # Nombre de la estación
    lonlat <- c(datos_puntos[i, "LONG"], datos_puntos[i, "LAT"])
    
    tryCatch({
      # Obtener datos
      datos <- get_power(
        community = "ag",
        lonlat = lonlat,
        pars = parametro,
        dates = c("1985-01-01", "2000-01-01"),
        temporal_api = "daily"
      )
      
      # Agregar columna de estación
      datos$Station <- nombre
      
      # Reordenar las columnas y los datos
      datos <- datos[, c("YYYYMMDD", "Station", parametro)]   #  datos <- datos[, c("YYYYMMDD", "Station", parametro)]
      
      # Renombrar las columnas
      colnames(datos) <- c("Date", "Station", parametro)
      
      # Almacenar datos en el data frame total
      datos_totales <- rbind(datos_totales, datos)
      
      print(paste("Datos para", nombre, "(", parametro, ") descargados y agregados"))
    }, error = function(e) {
      print(paste("Error al procesar datos para", nombre, "(", parametro, ") :", e$message))
    })
  }
  
  # Nombre del archivo CSV para los datos totales del parámetro actual
  file_name_total <- paste0(parametro, "_datos_totales.csv")
  file_path_total <- file.path(folder_path, file_name_total)
  
  # Guardar datos totales del parámetro actual en un archivo CSV
  write.csv(datos_totales, file = file_path_total, row.names = FALSE)
  print(paste("Datos totales para el parámetro", parametro, "guardados en", file_path_total))
}


########################################################################################

###  guardar para SWAT DATABASE

########################################################################################

ruta_archivo <- "D:/Paper_Climate/Data/siguiente paper/cfsr_pisco/CFSR_prueba.csv"

# Leer el archivo CSV con la información de los puntos
datos_puntos <- read.csv(ruta_archivo)

# Ruta de la carpeta donde se guardarán los archivos CSV
folder_path <- "D:/Paper_Climate/Data/siguiente paper/cfsr_pisco"

# Parámetros de interés
parametros <- c("WS2M", "ALLSKY_SFC_SW_DWN", "RH2M")

# Si la carpeta no existe, crearla
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

# Descargar y guardar datos solo para los parámetros de interés
for (parametro in parametros) {
  # Data frame para almacenar los datos del parámetro actual
  datos_totales <- data.frame()
  
  for (i in 1:nrow(datos_puntos)) {
    nombre <- datos_puntos[i, "NAME"]  # Nombre de la estación
    lonlat <- c(datos_puntos[i, "LONG"], datos_puntos[i, "LAT"])
    
    tryCatch({
      # Obtener datos
      datos <- get_power(
        community = "ag",
        lonlat = lonlat,
        pars = parametro,
        dates = c("1985-01-01", "2000-01-01"),
        temporal_api = "daily"
      )
      
      # Agregar columna de estación
      datos$Station <- nombre
      
      # Reordenar las columnas y los datos
      datos <- datos[, c("YYYYMMDD", "Station", parametro)]
      
      # Renombrar las columnas
      colnames(datos) <- c("Date", "Station", parametro)
      
      # Almacenar datos en el data frame total
      datos_totales <- rbind(datos_totales, datos)
      
      print(paste("Datos para", nombre, "(", parametro, ") descargados y agregados"))
    }, error = function(e) {
      print(paste("Error al procesar datos para", nombre, "(", parametro, ") :", e$message))
    })
  }
  
  # Nombre del archivo CSV para los datos totales del parámetro actual
  file_name_total <- paste0(parametro, "_datos_totales.csv")
  file_path_total <- file.path(folder_path, file_name_total)
  
  # Guardar datos totales del parámetro actual solo si está en la lista de parámetros de interés
  if (parametro %in% c("WS2M", "ALLSKY_SFC_SW_DWN", "RH2M")) {
    write.csv(datos_totales, file = file_path_total, row.names = FALSE)
    print(paste("Datos totales para el parámetro", parametro, "guardados en", file_path_total))
  }
}

