
##################################################################################
#                diario

library(sf)
library(dplyr)
library(meteo)
library(ranger)
library(terra)
library(lubridate)
library(CAST)

# ==================== CONFIGURACIÓN ====================
cat("=== PROCESAMIENTO RFSI DIARIO CON DATOS UTM ===\n")

# Directorios
dir_datos <- "D:/S/Serbia1km/Interpolation/"
dir_salida <- "D:/QSWAT/Output_RFSI_Diario/"

# Crear directorio de salida
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
  cat("Directorio creado:", dir_salida, "\n")
}

# ==================== 1. CARGAR Y PREPARAR DATOS ====================
cat("\n1. CARGANDO DATOS DE ESTACIONES...\n")
data <- read.csv(paste0(dir_datos, "stfdf_pt_huallaga.csv"))

# Procesar datos
data <- data %>%
  mutate(
    time = as.Date(time, format = "%m/%d/%Y"),
    date = time,
    doy = yday(date),
    month = month(date),
    year = year(date),
    staid = sp.ID
  ) %>%
  filter(
    !is.na(prcp), !is.na(precland), !is.na(tmax), !is.na(tmin),
    !is.na(lon), !is.na(lat), !is.na(dem)
  )

cat("Datos cargados:", nrow(data), "registros\n")
cat("Rango de fechas:", min(data$date), "a", max(data$date), "\n")

# ==================== 2. CONVERTIR ESTACIONES A UTM ====================
cat("\n2. CONVIRTIENDO ESTACIONES A UTM...\n")

# Convertir a UTM zona 18S (EPSG:32718)
data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
data_sf <- st_transform(data_sf, crs = 32718)

# Extraer coordenadas UTM
coords_utm <- st_coordinates(data_sf)
data <- data %>%
  mutate(
    x_utm = coords_utm[, "X"],
    y_utm = coords_utm[, "Y"],
    z = as.numeric(date - min(date)),
    doy_sin = sin(2 * pi * doy / 365.25),
    doy_cos = cos(2 * pi * doy / 365.25)
  )

cat("Estaciones convertidas a UTM\n")

# ==================== 3. CARGAR RASTERS UTM ====================
cat("\n3. CARGANDO RASTERS UTM...\n")

dir_utm <- paste0(dir_datos, "Huallaga/variabl_utm/")

# Cargar rasters estáticos UTM
r_dem_utm <- rast(paste0(dir_utm, "dem_utm.tif"))
r_twi_utm <- rast(paste0(dir_utm, "twi_utm.tif"))
r_sin_utm <- rast(paste0(dir_utm, "sin_utm.tif"))
r_cos_utm <- rast(paste0(dir_utm, "cos_utm.tif"))
r_slope_utm <- rast(paste0(dir_utm, "slope_utm.tif"))

# Cargar rasters dinámicos UTM (multibanda: días desde 2000-01-01)
r_precland_utm <- rast(paste0(dir_utm, "precland_2000_2003_utm.tif"))
r_tmax_utm <- rast(paste0(dir_utm, "tmax_2000_2003_utm.tif"))
r_tmin_utm <- rast(paste0(dir_utm, "tmin_2000_2003_utm.tif"))

# Verificar que todos los rasters existen
cat("  dem_utm.tif:", ifelse(nrow(r_dem_utm) > 0, "OK", "ERROR"), "\n")
cat("  twi_utm.tif:", ifelse(nrow(r_twi_utm) > 0, "OK", "ERROR"), "\n")
cat("  sin_utm.tif:", ifelse(nrow(r_sin_utm) > 0, "OK", "ERROR"), "\n")
cat("  cos_utm.tif:", ifelse(nrow(r_cos_utm) > 0, "OK", "ERROR"), "\n")
cat("  slope_utm.tif:", ifelse(nrow(r_slope_utm) > 0, "OK", "ERROR"), "\n")
cat("  precland_2000_2003_utm.tif:", ifelse(nrow(r_precland_utm) > 0, "OK", "ERROR"), "\n")
cat("  tmax_2000_2003_utm.tif:", ifelse(nrow(r_tmax_utm) > 0, "OK", "ERROR"), "\n")
cat("  tmin_2000_2003_utm.tif:", ifelse(nrow(r_tmin_utm) > 0, "OK", "ERROR"), "\n")

# Definir la fecha de inicio de los rasters dinámicos (asumimos 2000-01-01)
fecha_inicio_raster <- as.Date("2000-01-01")

# ==================== 4. CREAR GRILLA EN UTM ====================
cat("\n4. CREANDO GRILLA EN UTM...\n")

# Leer cuenca y convertir a UTM
cuenca <- st_read(paste0(dir_datos, "Titicaca_border/watershed.shp"))
cuenca <- st_transform(cuenca, crs = 32718)

# Usar la extensión de los rasters UTM
ext_utm <- ext(r_dem_utm)

# Crear grilla de puntos UTM (resolución de 5000m = 5km)
resolucion_m <- 5000
x_seq <- seq(ext_utm[1], ext_utm[2], by = resolucion_m)
y_seq <- seq(ext_utm[3], ext_utm[4], by = resolucion_m)
grid_utm <- expand.grid(x = x_seq, y = y_seq)

# Convertir a sf y filtrar puntos dentro de la cuenca
grid_sf <- st_as_sf(grid_utm, coords = c("x", "y"), crs = 32718)
grid_dentro <- grid_sf[st_intersects(grid_sf, cuenca, sparse = FALSE), ]

# Extraer coordenadas
coords_grid <- st_coordinates(grid_dentro)
grid_cells_utm <- data.frame(x = coords_grid[, "X"], y = coords_grid[, "Y"])

cat("Grilla UTM creada:", nrow(grid_cells_utm), "puntos\n")

# ==================== 5. EXTRAER VALORES DE RASTERS ESTÁTICOS UTM ====================
cat("\n5. EXTRAYENDO VALORES DE RASTERS ESTÁTICOS...\n")

# Crear SpatVector para extracción
grid_vect <- vect(grid_cells_utm, geom = c("x", "y"), crs = "EPSG:32718")

# Extraer valores de cada raster estático
grid_cells_utm$dem <- terra::extract(r_dem_utm, grid_vect)[, 2]
grid_cells_utm$twi <- terra::extract(r_twi_utm, grid_vect)[, 2]
grid_cells_utm$sin <- terra::extract(r_sin_utm, grid_vect)[, 2]
grid_cells_utm$cos <- terra::extract(r_cos_utm, grid_vect)[, 2]
grid_cells_utm$slope <- terra::extract(r_slope_utm, grid_vect)[, 2]

# Rellenar NAs
grid_cells_utm <- grid_cells_utm %>%
  mutate(
    dem = ifelse(is.na(dem), median(dem, na.rm = TRUE), dem),
    twi = ifelse(is.na(twi), median(twi, na.rm = TRUE), twi),
    sin = ifelse(is.na(sin), 0, sin),
    cos = ifelse(is.na(cos), 0, cos),
    slope = ifelse(is.na(slope), median(slope, na.rm = TRUE), slope)
  )

cat("Valores estáticos extraídos y procesados\n")

# ==================== 6. ENTRENAR MODELO RFSI ====================
cat("\n6. ENTRENANDO MODELO RFSI...\n")

# Fórmula del modelo
fm.RFSI <- as.formula("prcp ~ precland + tmax + tmin + dem + twi + slope + 
                      sin + cos + doy_sin + doy_cos + month")

# Entrenar modelo
set.seed(42)
n_cores_modelo <- max(1, parallel::detectCores() - 2)

rfsi_model <- rfsi(
  formula = fm.RFSI,
  data = data,
  data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
  n.obs = 5,
  soil3d = FALSE,
  cpus = n_cores_modelo,
  progress = TRUE,
  seed = 42,
  num.trees = 200,
  mtry = 5,
  min.node.size = 5,
  sample.fraction = 0.8
)

cat("Modelo entrenado exitosamente\n")

# ==================== 7. FUNCIÓN MEJORADA QUE USA RASTERS DINÁMICOS UTM ====================
procesar_y_guardar_dia_con_rasters <- function(fecha, datos_dia, grid_base, modelo, datos_entrenamiento, dir_salida,
                                               r_precland, r_tmax, r_tmin, fecha_inicio) {
  
  cat(format(fecha, "%Y-%m-%d"), "... ")
  
  tryCatch({
    # 1. Preparar grilla del día
    grid_dia <- grid_base
    
    # 2. Calcular el índice de la banda (día) en los rasters dinámicos
    # Asumimos que la primera banda es fecha_inicio (2000-01-01)
    indice_dia <- as.numeric(fecha - fecha_inicio) + 1
    
    # Verificar que el índice esté dentro del rango de bandas
    if (indice_dia < 1 | indice_dia > nlyr(r_precland)) {
      cat("Índice de día fuera de rango - SKIP\n")
      return(FALSE)
    }
    
    # 3. Extraer valores de los rasters dinámicos para este día
    grid_vect <- vect(grid_dia[, c("x", "y")], geom = c("x", "y"), crs = "EPSG:32718")
    
    # Extraer la banda correspondiente de cada raster dinámico
    grid_dia$precland <- terra::extract(r_precland[[indice_dia]], grid_vect)[, 2]
    grid_dia$tmax <- terra::extract(r_tmax[[indice_dia]], grid_vect)[, 2]
    grid_dia$tmin <- terra::extract(r_tmin[[indice_dia]], grid_vect)[, 2]
    
    # 4. Preparar dataframe para predicción
    grid_pred_df <- data.frame(
      staid = 1:nrow(grid_dia),
      x = grid_dia$x,
      y = grid_dia$y,
      z = as.numeric(fecha - min(datos_entrenamiento$date)),
      doy_sin = sin(2 * pi * yday(fecha) / 365.25),
      doy_cos = cos(2 * pi * yday(fecha) / 365.25),
      month = month(fecha),
      dem = grid_dia$dem,
      twi = grid_dia$twi,
      slope = grid_dia$slope,
      sin = grid_dia$sin,
      cos = grid_dia$cos,
      precland = grid_dia$precland,
      tmax = grid_dia$tmax,
      tmin = grid_dia$tmin
    )
    
    # 5. Eliminar NAs
    grid_clean <- na.omit(grid_pred_df)
    if (nrow(grid_clean) == 0) {
      cat("Sin datos - SKIP\n")
      return(FALSE)
    }
    
    # 6. Muestrear si hay muchos puntos (para eficiencia)
    max_puntos <- 10000
    if (nrow(grid_clean) > max_puntos) {
      set.seed(42)
      idx_muestra <- sample(1:nrow(grid_clean), max_puntos)
      grid_clean <- grid_clean[idx_muestra, ]
    }
    
    # 7. Predecir con RFSI
    prediccion <- suppressWarnings(
      pred.rfsi(
        model = modelo,
        data = datos_entrenamiento,
        obs.col = "prcp",
        data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
        newdata = grid_clean,
        newdata.staid.x.y.z = c("staid", "x", "y", "z"),
        output.format = "data.frame",
        cpus = 1,
        progress = FALSE,
        soil3d = FALSE
      )
    )
    
    # 8. Crear y guardar raster UTM
    grid_clean$pred <- pmax(0, prediccion$pred)
    
    # Crear raster UTM
    raster_dia_utm <- terra::rast(
      grid_clean[, c("x", "y", "pred")],
      type = "xyz",
      crs = "EPSG:32718"
    )
    
    # Convertir a WGS84
    raster_dia_wgs <- terra::project(raster_dia_utm, "EPSG:4326")
    
    # 9. Guardar directamente
    nombre_archivo <- paste0(dir_salida, "prcp_", 
                             format(fecha, "%Y%m%d"), ".tif")
    
    terra::writeRaster(
      raster_dia_wgs,
      filename = nombre_archivo,
      filetype = "GTiff",
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW", "PREDICTOR=2")
    )
    
    # Limpiar memoria
    rm(raster_dia_utm, raster_dia_wgs)
    gc()
    
    cat("OK - Guardado\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(FALSE)
  })
}

# ==================== 8. PROCESAR DÍAS EN SECUENCIA ====================
cat("\n7. PROCESANDO DÍAS...\n")

# Obtener fechas únicas
fechas <- sort(unique(data$date))

# PRIMERO: Probar con solo 5 días
fechas_prueba <- fechas[1:5]
cat("PRUEBA: Procesando", length(fechas_prueba), "días\n")

# Lista para resultados
archivos_guardados <- character()
dias_exitosos <- 0

# Procesar días en secuencia
for (i in 1:length(fechas_prueba)) {
  fecha_actual <- fechas_prueba[i]
  datos_dia <- data %>% filter(date == fecha_actual)
  
  if (nrow(datos_dia) >= 3) {
    cat(sprintf("[%d/%d] ", i, length(fechas_prueba)))
    
    exito <- procesar_y_guardar_dia_con_rasters(
      fecha_actual, 
      datos_dia, 
      grid_cells_utm, 
      rfsi_model, 
      data, 
      dir_salida,
      r_precland_utm,
      r_tmax_utm,
      r_tmin_utm,
      fecha_inicio_raster
    )
    
    if (exito) {
      nombre_archivo <- paste0("prcp_", format(fecha_actual, "%Y%m%d"), ".tif")
      archivos_guardados <- c(archivos_guardados, nombre_archivo)
      dias_exitosos <- dias_exitosos + 1
    }
  } else {
    cat(sprintf("[%d/%d] %s: Pocos datos (%d estaciones) - SKIP\n", 
                i, length(fechas_prueba), format(fecha_actual, "%Y-%m-%d"), nrow(datos_dia)))
  }
}

# ==================== 9. VERIFICAR ARCHIVOS GUARDADOS ====================
cat("\n8. VERIFICANDO ARCHIVOS GUARDADOS...\n")

archivos_en_disco <- list.files(dir_salida, pattern = "\\.tif$", full.names = TRUE)

if (length(archivos_en_disco) > 0) {
  cat("Archivos encontrados en disco:", length(archivos_en_disco), "\n\n")
  
  for (archivo in archivos_en_disco) {
    # Verificar que el archivo se pueda leer
    prueba <- try(terra::rast(archivo), silent = TRUE)
    if (inherits(prueba, "try-error")) {
      cat("  ✗ ERROR:", basename(archivo), "no se puede leer\n")
    } else {
      valores <- terra::values(prueba)
      min_val <- min(valores, na.rm = TRUE)
      max_val <- max(valores, na.rm = TRUE)
      cat("  ✓", basename(archivo), 
          sprintf("(%.2f-%.2f mm, %d×%d celdas)\n", 
                  min_val, max_val, ncol(prueba), nrow(prueba)))
    }
  }
} else {
  cat("No se encontraron archivos TIF en el directorio de salida\n")
}

# ==================== 10. RESUMEN FINAL ====================
cat("\n")
cat(rep("=", 60), "\n", sep="")
cat("PROCESAMIENTO COMPLETADO\n")
cat(rep("=", 60), "\n", sep="")

cat("\nRESUMEN:\n")
cat("  Total días disponibles:", length(fechas), "\n")
cat("  Días procesados (prueba):", length(fechas_prueba), "\n")
cat("  Días exitosos:", dias_exitosos, "\n")
cat("  Archivos guardados:", length(archivos_en_disco), "\n")
cat("  Directorio:", dir_salida, "\n")

# Guardar metadata
metadata <- data.frame(
  fecha_procesamiento = Sys.time(),
  total_dias = length(fechas),
  dias_procesados_prueba = length(fechas_prueba),
  dias_exitosos = dias_exitosos,
  resolucion_m = resolucion_m,
  modelo = "RFSI",
  num_arboles = 200,
  zona_utm = "18S (EPSG:32718)",
  formula = as.character(fm.RFSI)[3],
  uso_rasters_dinamicos = "Sí"
)

write.csv(metadata, paste0(dir_salida, "metadata.csv"), row.names = FALSE)
cat("\nMetadata guardada en: metadata.csv\n")

# Instrucciones para procesar todos los días
cat("\n", rep("-", 60), "\n", sep="")
cat("PARA PROCESAR TODOS LOS DÍAS:\n")
cat("1. La prueba fue exitosa si tienes archivos .tif válidos\n")
cat("2. Cambia en la sección 8:\n")
cat("   De: fechas_prueba <- fechas[1:5]\n")
cat("   A: fechas_prueba <- fechas  # Todos los días\n")
cat("3. Ajusta parámetros si es necesario (num.trees, resolucion_m)\n")
cat("4. Ejecuta de nuevo desde la sección 8\n")
cat(rep("-", 60), "\n", sep="")
library(raster)

# Cargar el raster multicapas de un año
r <- stack("D:/QSWAT/Output_RFSI_Diario/prcp_20000104.tif")

plot(r)


######################################################################################
#              anual


library(sf)
library(dplyr)
library(meteo)
library(ranger)
library(terra)
library(lubridate)
library(CAST)

# ==================== CONFIGURACIÓN ====================
cat("=== PROCESAMIENTO RFSI ANUAL CON DATOS UTM ===\n")

# Directorios
dir_datos <- "D:/S/Serbia1km/Interpolation/"
dir_salida <- "D:/QSWAT/Output_RFSI_Anual/"

# Crear directorio de salida
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
  cat("Directorio creado:", dir_salida, "\n")
}

# ==================== 1. CARGAR Y PREPARAR DATOS ====================
cat("\n1. CARGANDO DATOS DE ESTACIONES...\n")
data <- read.csv(paste0(dir_datos, "stfdf_pt_huallaga.csv"))

# Procesar datos
data <- data %>%
  mutate(
    time = as.Date(time, format = "%m/%d/%Y"),
    date = time,
    doy = yday(date),
    month = month(date),
    year = year(date),
    staid = sp.ID
  ) %>%
  filter(
    !is.na(prcp), !is.na(precland), !is.na(tmax), !is.na(tmin),
    !is.na(lon), !is.na(lat), !is.na(dem)
  )

cat("Datos cargados:", nrow(data), "registros\n")
cat("Rango de fechas:", min(data$date), "a", max(data$date), "\n")

# ==================== 2. CONVERTIR ESTACIONES A UTM ====================
cat("\n2. CONVIRTIENDO ESTACIONES A UTM...\n")

# Convertir a UTM zona 18S (EPSG:32718)
data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
data_sf <- st_transform(data_sf, crs = 32718)

# Extraer coordenadas UTM
coords_utm <- st_coordinates(data_sf)
data <- data %>%
  mutate(
    x_utm = coords_utm[, "X"],
    y_utm = coords_utm[, "Y"],
    z = as.numeric(date - min(date)),
    #doy_sin = sin(2 * pi * doy / 365.25),
    #doy_cos = cos(2 * pi * doy / 365.25)
  )

cat("Estaciones convertidas a UTM\n")

# ==================== 3. CARGAR RASTERS UTM ====================
cat("\n3. CARGANDO RASTERS UTM...\n")

dir_utm <- paste0(dir_datos, "Huallaga/variabl_utm/")

# Cargar rasters estáticos UTM
r_dem_utm <- rast(paste0(dir_utm, "dem_utm.tif"))
r_twi_utm <- rast(paste0(dir_utm, "twi_utm.tif"))
r_sin_utm <- rast(paste0(dir_utm, "sin_utm.tif"))
r_cos_utm <- rast(paste0(dir_utm, "cos_utm.tif"))
r_slope_utm <- rast(paste0(dir_utm, "slope_utm.tif"))

# Cargar rasters dinámicos UTM (multibanda: días desde 2000-01-01)
r_precland_utm <- rast(paste0(dir_utm, "precland_2000_2003_utm.tif"))
r_tmax_utm <- rast(paste0(dir_utm, "tmax_2000_2003_utm.tif"))
r_tmin_utm <- rast(paste0(dir_utm, "tmin_2000_2003_utm.tif"))

# Verificar que todos los rasters existen
cat("  dem_utm.tif:", ifelse(nrow(r_dem_utm) > 0, "OK", "ERROR"), "\n")
cat("  twi_utm.tif:", ifelse(nrow(r_twi_utm) > 0, "OK", "ERROR"), "\n")
cat("  sin_utm.tif:", ifelse(nrow(r_sin_utm) > 0, "OK", "ERROR"), "\n")
cat("  cos_utm.tif:", ifelse(nrow(r_cos_utm) > 0, "OK", "ERROR"), "\n")
cat("  slope_utm.tif:", ifelse(nrow(r_slope_utm) > 0, "OK", "ERROR"), "\n")
cat("  precland_2000_2003_utm.tif:", ifelse(nrow(r_precland_utm) > 0, "OK", "ERROR"), "\n")
cat("  tmax_2000_2003_utm.tif:", ifelse(nrow(r_tmax_utm) > 0, "OK", "ERROR"), "\n")
cat("  tmin_2000_2003_utm.tif:", ifelse(nrow(r_tmin_utm) > 0, "OK", "ERROR"), "\n")

# Definir la fecha de inicio de los rasters dinámicos (asumimos 2000-01-01)
fecha_inicio_raster <- as.Date("2000-01-01")

# ==================== 4. CREAR GRILLA EN UTM ====================
cat("\n4. CREANDO GRILLA EN UTM...\n")

# Leer cuenca y convertir a UTM
cuenca <- st_read(paste0(dir_datos, "Titicaca_border/watershed.shp"))
cuenca <- st_transform(cuenca, crs = 32718)

# Usar la extensión de los rasters UTM
ext_utm <- ext(r_dem_utm)

# Crear grilla de puntos UTM (resolución de 5000m = 5km)
resolucion_m <- 5000
x_seq <- seq(ext_utm[1], ext_utm[2], by = resolucion_m)
y_seq <- seq(ext_utm[3], ext_utm[4], by = resolucion_m)
grid_utm <- expand.grid(x = x_seq, y = y_seq)

# Convertir a sf y filtrar puntos dentro de la cuenca
grid_sf <- st_as_sf(grid_utm, coords = c("x", "y"), crs = 32718)
grid_dentro <- grid_sf[st_intersects(grid_sf, cuenca, sparse = FALSE), ]

# Extraer coordenadas
coords_grid <- st_coordinates(grid_dentro)
grid_cells_utm <- data.frame(x = coords_grid[, "X"], y = coords_grid[, "Y"])

cat("Grilla UTM creada:", nrow(grid_cells_utm), "puntos\n")

# ==================== 5. EXTRAER VALORES DE RASTERS ESTÁTICOS UTM ====================
cat("\n5. EXTRAYENDO VALORES DE RASTERS ESTÁTICOS...\n")

# Crear SpatVector para extracción
grid_vect <- vect(grid_cells_utm, geom = c("x", "y"), crs = "EPSG:32718")

# Extraer valores de cada raster estático
grid_cells_utm$dem <- terra::extract(r_dem_utm, grid_vect)[, 2]
grid_cells_utm$twi <- terra::extract(r_twi_utm, grid_vect)[, 2]
grid_cells_utm$sin <- terra::extract(r_sin_utm, grid_vect)[, 2]
grid_cells_utm$cos <- terra::extract(r_cos_utm, grid_vect)[, 2]
grid_cells_utm$slope <- terra::extract(r_slope_utm, grid_vect)[, 2]

# Rellenar NAs
grid_cells_utm <- grid_cells_utm %>%
  mutate(
    dem = ifelse(is.na(dem), median(dem, na.rm = TRUE), dem),
    twi = ifelse(is.na(twi), median(twi, na.rm = TRUE), twi),
    sin = ifelse(is.na(sin), 0, sin),
    cos = ifelse(is.na(cos), 0, cos),
    slope = ifelse(is.na(slope), median(slope, na.rm = TRUE), slope)
  )

cat("Valores estáticos extraídos y procesados\n")

# ==================== 6. ENTRENAR MODELO RFSI ====================
cat("\n6. ENTRENANDO MODELO RFSI...\n")

# Fórmula del modelo
fm.RFSI <- as.formula("prcp ~ precland + tmax + tmin + dem + twi + slope + 
                      sin + cos + month")

# Entrenar modelo
set.seed(42)
n_cores_modelo <- max(1, parallel::detectCores() - 2)

rfsi_model <- rfsi(
  formula = fm.RFSI,
  data = data,
  data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
  n.obs = 5,
  soil3d = FALSE,
  cpus = n_cores_modelo,
  progress = TRUE,
  seed = 42,
  num.trees = 200,
  mtry = 5,
  min.node.size = 5,
  sample.fraction = 0.8
)

cat("Modelo entrenado exitosamente\n")

# ==================== 7. FUNCIÓN QUE DEVUELVE RASTER POR DÍA ====================
procesar_dia_a_raster <- function(fecha, datos_dia, grid_base, modelo, datos_entrenamiento,
                                  r_precland, r_tmax, r_tmin, fecha_inicio) {
  
  tryCatch({
    # 1. Preparar grilla del día
    grid_dia <- grid_base
    
    # 2. Calcular el índice de la banda (día) en los rasters dinámicos
    # Asumimos que la primera banda es fecha_inicio (2000-01-01)
    indice_dia <- as.numeric(fecha - fecha_inicio) + 1
    
    # Verificar que el índice esté dentro del rango de bandas
    if (indice_dia < 1 | indice_dia > nlyr(r_precland)) {
      return(NULL)
    }
    
    # 3. Extraer valores de los rasters dinámicos para este día
    grid_vect <- vect(grid_dia[, c("x", "y")], geom = c("x", "y"), crs = "EPSG:32718")
    
    # Extraer la banda correspondiente de cada raster dinámico
    grid_dia$precland <- terra::extract(r_precland[[indice_dia]], grid_vect)[, 2]
    grid_dia$tmax <- terra::extract(r_tmax[[indice_dia]], grid_vect)[, 2]
    grid_dia$tmin <- terra::extract(r_tmin[[indice_dia]], grid_vect)[, 2]
    
    # 4. Preparar dataframe para predicción
    grid_pred_df <- data.frame(
      staid = 1:nrow(grid_dia),
      x = grid_dia$x,
      y = grid_dia$y,
      z = as.numeric(fecha - min(datos_entrenamiento$date)),
      #doy_sin = sin(2 * pi * yday(fecha) / 365.25),
      #doy_cos = cos(2 * pi * yday(fecha) / 365.25),
      month = month(fecha),
      dem = grid_dia$dem,
      twi = grid_dia$twi,
      slope = grid_dia$slope,
      sin = grid_dia$sin,
      cos = grid_dia$cos,
      precland = grid_dia$precland,
      tmax = grid_dia$tmax,
      tmin = grid_dia$tmin
    )
    
    # 5. Eliminar NAs
    grid_clean <- na.omit(grid_pred_df)
    if (nrow(grid_clean) == 0) {
      return(NULL)
    }
    
    # 6. Muestrear si hay muchos puntos (para eficiencia)
    max_puntos <- 10000
    if (nrow(grid_clean) > max_puntos) {
      set.seed(42)
      idx_muestra <- sample(1:nrow(grid_clean), max_puntos)
      grid_clean <- grid_clean[idx_muestra, ]
    }
    
    # 7. Predecir con RFSI
    prediccion <- suppressWarnings(
      pred.rfsi(
        model = modelo,
        data = datos_entrenamiento,
        obs.col = "prcp",
        data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
        newdata = grid_clean,
        newdata.staid.x.y.z = c("staid", "x", "y", "z"),
        output.format = "data.frame",
        cpus = 1,
        progress = FALSE,
        soil3d = FALSE
      )
    )
    
    # 8. Crear raster UTM
    grid_clean$pred <- pmax(0, prediccion$pred)
    
    # Crear raster UTM
    raster_dia_utm <- terra::rast(
      grid_clean[, c("x", "y", "pred")],
      type = "xyz",
      crs = "EPSG:32718"
    )
    
    # Convertir a WGS84
    raster_dia_wgs <- terra::project(raster_dia_utm, "EPSG:4326")
    
    # Nombre de la capa con la fecha
    names(raster_dia_wgs) <- format(fecha, "%Y%m%d")
    
    return(raster_dia_wgs)
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(NULL)
  })
}

# ==================== 8. PROCESAR POR AÑOS ====================
cat("\n7. PROCESANDO POR AÑOS...\n")

# Obtener años únicos
anios <- unique(year(data$date))
cat("Años disponibles:", toString(anios), "\n")

# Procesar cada año por separado
for (anio_actual in anios) {
  cat("\n", rep("-", 50), "\n", sep="")
  cat("PROCESANDO AÑO:", anio_actual, "\n")
  cat(rep("-", 50), "\n", sep="")
  
  # Filtrar fechas del año actual
  fechas_anio <- sort(unique(data$date[year(data$date) == anio_actual]))
  
  # PRIMERO: Probar con solo 5 días del año
  fechas_procesar <- fechas_anio[1:366]  # Cambiar a fechas_anio para todos los días # fechas_procesar <- fechas_anio
  
  cat("Días a procesar:", length(fechas_procesar), "de", length(fechas_anio), "\n\n")
  
  # Lista para almacenar rasters del año
  rasters_anio <- list()
  dias_exitosos <- 0
  
  # Procesar días del año
  for (i in 1:length(fechas_procesar)) {
    fecha_actual <- fechas_procesar[i]
    datos_dia <- data %>% filter(date == fecha_actual)
    
    if (nrow(datos_dia) >= 3) {
      cat(sprintf("[%d/%d] %s ... ", i, length(fechas_procesar), format(fecha_actual, "%Y-%m-%d")))
      
      raster_dia <- procesar_dia_a_raster(
        fecha_actual, 
        datos_dia, 
        grid_cells_utm, 
        rfsi_model, 
        data,
        r_precland_utm,
        r_tmax_utm,
        r_tmin_utm,
        fecha_inicio_raster
      )
      
      if (!is.null(raster_dia)) {
        rasters_anio <- c(rasters_anio, list(raster_dia))
        dias_exitosos <- dias_exitosos + 1
        cat("OK\n")
        
        # Liberar memoria
        rm(raster_dia)
        gc()
      } else {
        cat("SKIP (raster nulo)\n")
      }
    } else {
      cat(sprintf("[%d/%d] %s: Pocos datos (%d estaciones) - SKIP\n", 
                  i, length(fechas_procesar), format(fecha_actual, "%Y-%m-%d"), nrow(datos_dia)))
    }
  }
  
  # ==================== 9. CREAR Y GUARDAR RASTER MULTIBANDA ANUAL ====================
  if (length(rasters_anio) > 0) {
    cat("\nCreando raster multibanda para el año", anio_actual, "...\n")
    
    # Combinar todos los rasters del año en un solo objeto multibanda
    raster_anual <- terra::rast(rasters_anio)
    
    # Guardar el raster multibanda anual
    nombre_archivo <- paste0(dir_salida, "prcp_", anio_actual, ".tif")
    
    cat("Guardando:", nombre_archivo, "\n")
    cat("Número de bandas (días):", nlyr(raster_anual), "\n")
    
    terra::writeRaster(
      raster_anual,
      filename = nombre_archivo,
      filetype = "GTiff",
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES")
    )
    
    # Verificar que se guardó correctamente
    if (file.exists(nombre_archivo)) {
      # Leer el archivo guardado para verificar
      archivo_verif <- try(terra::rast(nombre_archivo), silent = TRUE)
      if (!inherits(archivo_verif, "try-error")) {
        cat("✓ Archivo verificado:", basename(nombre_archivo), 
            sprintf("(%d bandas, %d×%d celdas)\n", 
                    nlyr(archivo_verif), ncol(archivo_verif), nrow(archivo_verif)))
        
        # Guardar información sobre las bandas (fechas)
        nombres_bandas <- names(rasters_anio)
        fechas_bandas <- sapply(rasters_anio, function(x) names(x))
        
        info_bandas <- data.frame(
          banda = 1:length(fechas_bandas),
          fecha = fechas_bandas,
          nombre = nombres_bandas
        )
        
        write.csv(info_bandas, 
                  paste0(dir_salida, "bandas_prcp_", anio_actual, ".csv"),
                  row.names = FALSE)
      }
    }
    
    # Liberar memoria
    rm(raster_anual, rasters_anio)
    gc()
  } else {
    cat("No se generaron rasters para el año", anio_actual, "\n")
  }
}

# ==================== 10. RESUMEN FINAL ====================
cat("\n")
cat(rep("=", 60), "\n", sep="")
cat("PROCESAMIENTO COMPLETADO\n")
cat(rep("=", 60), "\n", sep="")

# Listar archivos generados
archivos_anuales <- list.files(dir_salida, pattern = "prcp_\\d{4}\\.tif$", full.names = TRUE)

cat("\nARCHIVOS GENERADOS:\n")
if (length(archivos_anuales) > 0) {
  for (archivo in archivos_anuales) {
    # Obtener información básica del archivo
    info <- try(terra::rast(archivo), silent = TRUE)
    if (!inherits(info, "try-error")) {
      cat("  ✓", basename(archivo), 
          sprintf("(%d bandas, %d×%d celdas, %.1f MB)\n", 
                  nlyr(info), ncol(info), nrow(info),
                  file.size(archivo) / (1024*1024)))
    } else {
      cat("  ✗", basename(archivo), "(error al leer)\n")
    }
  }
} else {
  cat("No se encontraron archivos anuales\n")
}

# Guardar metadata general
metadata <- data.frame(
  fecha_procesamiento = Sys.time(),
  años_procesados = toString(anios),
  resolucion_m = resolucion_m,
  modelo = "RFSI",
  num_arboles = 200,
  zona_utm = "18S (EPSG:32718)",
  formula = as.character(fm.RFSI)[3],
  uso_rasters_dinamicos = "Sí",
  directorio_salida = dir_salida
)

write.csv(metadata, paste0(dir_salida, "metadata_general.csv"), row.names = FALSE)
cat("\nMetadata general guardada en: metadata_general.csv\n")

# Instrucciones finales
cat("\n", rep("-", 60), "\n", sep="")
cat("INSTRUCCIONES:\n")
cat("1. Archivos anuales guardados como prcp_AAAA.tif\n")
cat("2. Cada archivo contiene múltiples bandas (una por día)\n")
cat("3. Para procesar TODOS los días de cada año:\n")
cat("   Cambia en la sección 8:\n")
cat("   De: fechas_procesar <- fechas_anio[1:5]\n")
cat("   A: fechas_procesar <- fechas_anio\n")
cat("4. Los archivos CSV de bandas contienen la correspondencia banda-fecha\n")
cat(rep("-", 60), "\n", sep="")


library(raster)

# Cargar el raster multicapas de un año
r <- stack("D:/QSWAT/Output_RFSI_fisico_Anual/prcp_2000.tif")

plot(r)

#######################################################################################
#  RFSI PC OPERACIONAL... PUBLICABLE
######################################################################################


library(sf)
library(dplyr)
library(meteo)
library(ranger)
library(terra)
library(lubridate)
library(CAST)

# ==================== CONFIGURACIÓN ====================
cat("=== PROCESAMIENTO RFSI ANUAL CON DATOS UTM ===\n")

# Directorios
dir_datos <- "D:/S/Serbia1km/Interpolation/"
dir_salida <- "D:/QSWAT/Output_RFSI_fisico_Anual/"

# Umbrales físicos para post-procesamiento
UMBRAL_MIN_MM <- 0.5    # Menos de 0.5mm = 0 (eliminar lloviznas artificiales)
UMBRAL_MAX_MM <- 8.0    # Más de 8mm = 8mm (límite físico diario regional)

# Crear directorio de salida
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
  cat("Directorio creado:", dir_salida, "\n")
}

# ==================== 1. CARGAR Y PREPARAR DATOS ====================
cat("\n1. CARGANDO DATOS DE ESTACIONES...\n")
data <- read.csv(paste0(dir_datos, "stfdf_pt_huallaga.csv"))

# Procesar datos
data <- data %>%
  mutate(
    time = as.Date(time, format = "%m/%d/%Y"),
    date = time,
    doy = yday(date),
    month = month(date),
    year = year(date),
    staid = sp.ID
  ) %>%
  filter(
    !is.na(prcp), !is.na(precland), !is.na(tmax), !is.na(tmin),
    !is.na(lon), !is.na(lat), !is.na(dem)
  )

cat("Datos cargados:", nrow(data), "registros\n")
cat("Rango de fechas:", min(data$date), "a", max(data$date), "\n")

# ==================== 2. CONVERTIR ESTACIONES A UTM ====================
cat("\n2. CONVIRTIENDO ESTACIONES A UTM...\n")

# Convertir a UTM zona 18S (EPSG:32718)
data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
data_sf <- st_transform(data_sf, crs = 32718)

# Extraer coordenadas UTM
coords_utm <- st_coordinates(data_sf)
data <- data %>%
  mutate(
    x_utm = coords_utm[, "X"],
    y_utm = coords_utm[, "Y"],
    z = as.numeric(date - min(date)),
    #doy_sin = sin(2 * pi * doy / 365.25),
    #doy_cos = cos(2 * pi * doy / 365.25)
  )

cat("Estaciones convertidas a UTM\n")

# ==================== 3. CARGAR RASTERS UTM ====================
cat("\n3. CARGANDO RASTERS UTM...\n")

dir_utm <- paste0(dir_datos, "Huallaga/variabl_utm/")

# Cargar rasters estáticos UTM
r_dem_utm <- rast(paste0(dir_utm, "dem_utm.tif"))
r_twi_utm <- rast(paste0(dir_utm, "twi_utm.tif"))
r_sin_utm <- rast(paste0(dir_utm, "sin_utm.tif"))
r_cos_utm <- rast(paste0(dir_utm, "cos_utm.tif"))
r_slope_utm <- rast(paste0(dir_utm, "slope_utm.tif"))

# Cargar rasters dinámicos UTM (multibanda: días desde 2000-01-01)
r_precland_utm <- rast(paste0(dir_utm, "precland_2000_2003_utm.tif"))
r_tmax_utm <- rast(paste0(dir_utm, "tmax_2000_2003_utm.tif"))
r_tmin_utm <- rast(paste0(dir_utm, "tmin_2000_2003_utm.tif"))

# Verificar que todos los rasters existen
cat("  dem_utm.tif:", ifelse(nrow(r_dem_utm) > 0, "OK", "ERROR"), "\n")
cat("  twi_utm.tif:", ifelse(nrow(r_twi_utm) > 0, "OK", "ERROR"), "\n")
cat("  sin_utm.tif:", ifelse(nrow(r_sin_utm) > 0, "OK", "ERROR"), "\n")
cat("  cos_utm.tif:", ifelse(nrow(r_cos_utm) > 0, "OK", "ERROR"), "\n")
cat("  slope_utm.tif:", ifelse(nrow(r_slope_utm) > 0, "OK", "ERROR"), "\n")
cat("  precland_2000_2003_utm.tif:", ifelse(nrow(r_precland_utm) > 0, "OK", "ERROR"), "\n")
cat("  tmax_2000_2003_utm.tif:", ifelse(nrow(r_tmax_utm) > 0, "OK", "ERROR"), "\n")
cat("  tmin_2000_2003_utm.tif:", ifelse(nrow(r_tmin_utm) > 0, "OK", "ERROR"), "\n")

# Definir la fecha de inicio de los rasters dinámicos (asumimos 2000-01-01)
fecha_inicio_raster <- as.Date("2000-01-01")

# ==================== 4. CREAR GRILLA EN UTM ====================
cat("\n4. CREANDO GRILLA EN UTM...\n")

# Leer cuenca y convertir a UTM
cuenca <- st_read(paste0(dir_datos, "Titicaca_border/watershed.shp"))
cuenca <- st_transform(cuenca, crs = 32718)

# Usar la extensión de los rasters UTM
ext_utm <- ext(r_dem_utm)

# Crear grilla de puntos UTM (resolución de 5000m = 5km)
resolucion_m <- 5000
x_seq <- seq(ext_utm[1], ext_utm[2], by = resolucion_m)
y_seq <- seq(ext_utm[3], ext_utm[4], by = resolucion_m)
grid_utm <- expand.grid(x = x_seq, y = y_seq)

# Convertir a sf y filtrar puntos dentro de la cuenca
grid_sf <- st_as_sf(grid_utm, coords = c("x", "y"), crs = 32718)
grid_dentro <- grid_sf[st_intersects(grid_sf, cuenca, sparse = FALSE), ]

# Extraer coordenadas
coords_grid <- st_coordinates(grid_dentro)
grid_cells_utm <- data.frame(x = coords_grid[, "X"], y = coords_grid[, "Y"])

cat("Grilla UTM creada:", nrow(grid_cells_utm), "puntos\n")

# ==================== 5. EXTRAER VALORES DE RASTERS ESTÁTICOS UTM ====================
cat("\n5. EXTRAYENDO VALORES DE RASTERS ESTÁTICOS...\n")

# Crear SpatVector para extracción
grid_vect <- vect(grid_cells_utm, geom = c("x", "y"), crs = "EPSG:32718")

# Extraer valores de cada raster estático
grid_cells_utm$dem <- terra::extract(r_dem_utm, grid_vect)[, 2]
grid_cells_utm$twi <- terra::extract(r_twi_utm, grid_vect)[, 2]
grid_cells_utm$sin <- terra::extract(r_sin_utm, grid_vect)[, 2]
grid_cells_utm$cos <- terra::extract(r_cos_utm, grid_vect)[, 2]
grid_cells_utm$slope <- terra::extract(r_slope_utm, grid_vect)[, 2]

# Rellenar NAs
grid_cells_utm <- grid_cells_utm %>%
  mutate(
    dem = ifelse(is.na(dem), median(dem, na.rm = TRUE), dem),
    twi = ifelse(is.na(twi), median(twi, na.rm = TRUE), twi),
    sin = ifelse(is.na(sin), 0, sin),
    cos = ifelse(is.na(cos), 0, cos),
    slope = ifelse(is.na(slope), median(slope, na.rm = TRUE), slope)
  )

cat("Valores estáticos extraídos y procesados\n")

# ==================== 6. ENTRENAR MODELO RFSI ====================
cat("\n6. ENTRENANDO MODELO RFSI...\n")

# Fórmula del modelo
fm.RFSI <- as.formula("prcp ~ precland + tmax + tmin + dem + twi + slope + 
                      sin + cos + month")

# Entrenar modelo
set.seed(42)
n_cores_modelo <- max(1, parallel::detectCores() - 2)

rfsi_model <- rfsi(
  formula = fm.RFSI,
  data = data,
  data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
  n.obs = 5,
  soil3d = FALSE,
  cpus = n_cores_modelo,
  progress = TRUE,
  seed = 42,
  num.trees = 200,
  mtry = 5,
  min.node.size = 5,
  sample.fraction = 0.8
)

cat("Modelo entrenado exitosamente\n")
cat("Umbrales físicos configurados:\n")
cat("  - Precipitación mínima:", UMBRAL_MIN_MM, "mm (valores < se ponen a 0)\n")
cat("  - Precipitación máxima:", UMBRAL_MAX_MM, "mm (valores > se truncán)\n")

# ==================== 7. FUNCIÓN QUE DEVUELVE RASTER POR DÍA ====================
procesar_dia_a_raster <- function(fecha, datos_dia, grid_base, modelo, datos_entrenamiento,
                                  r_precland, r_tmax, r_tmin, fecha_inicio,
                                  umbral_min = 0.5, umbral_max = 8.0) {
  
  tryCatch({
    # 1. Preparar grilla del día
    grid_dia <- grid_base
    
    # 2. Calcular el índice de la banda (día) en los rasters dinámicos
    # Asumimos que la primera banda es fecha_inicio (2000-01-01)
    indice_dia <- as.numeric(fecha - fecha_inicio) + 1
    
    # Verificar que el índice esté dentro del rango de bandas
    if (indice_dia < 1 | indice_dia > nlyr(r_precland)) {
      return(NULL)
    }
    
    # 3. Extraer valores de los rasters dinámicos para este día
    grid_vect <- vect(grid_dia[, c("x", "y")], geom = c("x", "y"), crs = "EPSG:32718")
    
    # Extraer la banda correspondiente de cada raster dinámico
    grid_dia$precland <- terra::extract(r_precland[[indice_dia]], grid_vect)[, 2]
    grid_dia$tmax <- terra::extract(r_tmax[[indice_dia]], grid_vect)[, 2]
    grid_dia$tmin <- terra::extract(r_tmin[[indice_dia]], grid_vect)[, 2]
    
    # 4. Preparar dataframe para predicción
    grid_pred_df <- data.frame(
      staid = 1:nrow(grid_dia),
      x = grid_dia$x,
      y = grid_dia$y,
      z = as.numeric(fecha - min(datos_entrenamiento$date)),
      #doy_sin = sin(2 * pi * yday(fecha) / 365.25),
      #doy_cos = cos(2 * pi * yday(fecha) / 365.25),
      month = month(fecha),
      dem = grid_dia$dem,
      twi = grid_dia$twi,
      slope = grid_dia$slope,
      sin = grid_dia$sin,
      cos = grid_dia$cos,
      precland = grid_dia$precland,
      tmax = grid_dia$tmax,
      tmin = grid_dia$tmin
    )
    
    # 5. Eliminar NAs
    grid_clean <- na.omit(grid_pred_df)
    if (nrow(grid_clean) == 0) {
      return(NULL)
    }
    
    # 6. Muestrear si hay muchos puntos (para eficiencia)
    max_puntos <- 10000
    if (nrow(grid_clean) > max_puntos) {
      set.seed(42)
      idx_muestra <- sample(1:nrow(grid_clean), max_puntos)
      grid_clean <- grid_clean[idx_muestra, ]
    }
    
    # 7. Predecir con RFSI
    prediccion <- suppressWarnings(
      pred.rfsi(
        model = modelo,
        data = datos_entrenamiento,
        obs.col = "prcp",
        data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
        newdata = grid_clean,
        newdata.staid.x.y.z = c("staid", "x", "y", "z"),
        output.format = "data.frame",
        cpus = 1,
        progress = FALSE,
        soil3d = FALSE
      )
    )
    
    # 8. APLICAR ZERO-THRESHOLDING POST-RFSI (CORRECCIÓN FÍSICA)
    cat("  -> Aplicando correcciones físicas... ")
    
    # Asegurar valores no negativos primero
    pred_values <- pmax(0, prediccion$pred)
    
    if (sum(datos_dia$prcp > 0, na.rm = TRUE) < 2) {
      pred_values[] <- 0
      cat("  -> Día SIN soporte pluviométrico (<2 estaciones con lluvia): raster forzado a 0\n")
    }
    
    # 8.1 Eliminar lloviznas artificiales (valores menores al umbral mínimo)
    # En hidrología operativa, < 0.5mm se considera "no lluvia medible"
    # Percentil 10% histórico del día
    #umbral_min_dia <- max(umbral_min, quantile(datos_dia$prcp, 0.1, na.rm = TRUE))
    pred_values[pred_values < umbral_min] <- 0 # pred_values[pred_values < umbral_min_dia] <- 0
    
    # 8.2 Límite físico diario regional (evitar sobreestimación)
    # Basado en estadísticas históricas de la región
    # Umbral dinámico basado en observaciones del día
    
    ####### COMBINAR
    # if (sum(datos_dia$prcp > 0, na.rm = TRUE) >= 3) {
    #   umbral_max_dia <- min(quantile(datos_dia$prcp, 0.95, na.rm = TRUE), UMBRAL_MAX_MM)
    # } else {
    #   umbral_max_dia <- UMBRAL_MAX_MM
    # }
    
    if (sum(datos_dia$prcp > 0) >= 3) {
      umbral_max_dia <- quantile(datos_dia$prcp, 0.95, na.rm = TRUE)
    } else {
      umbral_max_dia <- umbral_max  # fallback
    }
    
    pred_values[pred_values > umbral_max_dia] <- umbral_max_dia
    
    
    cat(sprintf("Corregido: %.1f%% celdas a 0, %.1f%% truncadas a %.1fmm\n",
                sum(pred_values == 0) / length(pred_values) * 100,
                sum(pred_values == umbral_max) / length(pred_values) * 100,
                umbral_max))
    
    # 9. Crear raster UTM
    grid_clean$pred <- pred_values
    
    # Crear raster UTM
    raster_dia_utm <- terra::rast(
      grid_clean[, c("x", "y", "pred")],
      type = "xyz",
      crs = "EPSG:32718"
    )
    
    # Convertir a WGS84
    raster_dia_wgs <- terra::project(raster_dia_utm, "EPSG:4326")
    
    # Nombre de la capa con la fecha
    names(raster_dia_wgs) <- format(fecha, "%Y%m%d")
    
    return(raster_dia_wgs)
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(NULL)
  })
}

# ==================== 8. FUNCIÓN PARA CALCULAR ESTADÍSTICAS DE CORRECCIÓN ====================
calcular_estadisticas_correccion <- function(raster_dia, umbral_min, umbral_max) {
  valores <- terra::values(raster_dia)
  valores <- valores[!is.na(valores)]
  
  if (length(valores) == 0) {
    return(data.frame(
      media = NA,
      mediana = NA,
      pct_ceros = NA,
      pct_truncados = NA,
      max_original = NA,
      max_final = NA
    ))
  }
  
  stats <- data.frame(
    fecha = names(raster_dia),
    media = mean(valores, na.rm = TRUE),
    mediana = median(valores, na.rm = TRUE),
    pct_ceros = sum(valores == 0) / length(valores) * 100,
    pct_truncados = sum(valores == umbral_max) / length(valores) * 100,
    max_original = max(valores, na.rm = TRUE),
    max_final = min(max(valores, na.rm = TRUE), umbral_max)
  )
  
  return(stats)
}

# ==================== 9. PROCESAR POR AÑOS ====================
cat("\n7. PROCESANDO POR AÑOS...\n")

# Obtener años únicos
anios <- unique(year(data$date))
cat("Años disponibles:", toString(anios), "\n")

# Dataframe para almacenar estadísticas de corrección
estadisticas_correccion <- data.frame()

# Procesar cada año por separado
for (anio_actual in anios) {
  cat("\n", rep("-", 50), "\n", sep="")
  cat("PROCESANDO AÑO:", anio_actual, "\n")
  cat("Umbrales: MIN =", UMBRAL_MIN_MM, "mm, MAX =", UMBRAL_MAX_MM, "mm\n")
  cat(rep("-", 50), "\n", sep="")
  
  # Filtrar fechas del año actual
  fechas_anio <- sort(unique(data$date[year(data$date) == anio_actual]))
  
  # PRIMERO: Probar con solo 5 días del año
  # Para procesar todo el año, cambia la siguiente línea a: fechas_procesar <- fechas_anio
  fechas_procesar <- fechas_anio[1:366]  # Cambiar a fechas_anio para todos los días
  
  cat("Días a procesar:", length(fechas_procesar), "de", length(fechas_anio), "\n\n")
  
  # Lista para almacenar rasters del año
  rasters_anio <- list()
  dias_exitosos <- 0
  
  # Procesar días del año
  for (i in 1:length(fechas_procesar)) {
    fecha_actual <- fechas_procesar[i]
    datos_dia <- data %>% filter(date == fecha_actual)
    
    if (nrow(datos_dia) >= 3) {
      cat(sprintf("[%d/%d] %s ... ", i, length(fechas_procesar), format(fecha_actual, "%Y-%m-%d")))
      
      raster_dia <- procesar_dia_a_raster(
        fecha_actual, 
        datos_dia, 
        grid_cells_utm, 
        rfsi_model, 
        data,
        r_precland_utm,
        r_tmax_utm,
        r_tmin_utm,
        fecha_inicio_raster,
        UMBRAL_MIN_MM,
        UMBRAL_MAX_MM
      )
      
      if (!is.null(raster_dia)) {
        # Calcular estadísticas de corrección para este día
        stats_dia <- calcular_estadisticas_correccion(raster_dia, UMBRAL_MIN_MM, UMBRAL_MAX_MM)
        stats_dia$fecha <- format(fecha_actual, "%Y-%m-%d")
        stats_dia$anio <- anio_actual
        estadisticas_correccion <- rbind(estadisticas_correccion, stats_dia)
        
        rasters_anio <- c(rasters_anio, list(raster_dia))
        dias_exitosos <- dias_exitosos + 1
        cat("OK\n")
        
        # Liberar memoria
        rm(raster_dia)
        gc()
      } else {
        cat("SKIP (raster nulo)\n")
      }
    } else {
      cat(sprintf("[%d/%d] %s: Pocos datos (%d estaciones) - SKIP\n", 
                  i, length(fechas_procesar), format(fecha_actual, "%Y-%m-%d"), nrow(datos_dia)))
    }
  }
  
  # ==================== 10. CREAR Y GUARDAR RASTER MULTIBANDA ANUAL ====================
  if (length(rasters_anio) > 0) {
    cat("\nCreando raster multibanda para el año", anio_actual, "...\n")
    
    # Combinar todos los rasters del año en un solo objeto multibanda
    raster_anual <- terra::rast(rasters_anio)
    
    # Guardar el raster multibanda anual
    nombre_archivo <- paste0(dir_salida, "prcp_", anio_actual, ".tif")
    
    cat("Guardando:", nombre_archivo, "\n")
    cat("Número de bandas (días):", nlyr(raster_anual), "\n")
    
    terra::writeRaster(
      raster_anual,
      filename = nombre_archivo,
      filetype = "GTiff",
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES")
    )
    
    # Verificar que se guardó correctamente
    if (file.exists(nombre_archivo)) {
      # Leer el archivo guardado para verificar
      archivo_verif <- try(terra::rast(nombre_archivo), silent = TRUE)
      if (!inherits(archivo_verif, "try-error")) {
        cat("✓ Archivo verificado:", basename(nombre_archivo), 
            sprintf("(%d bandas, %d×%d celdas)\n", 
                    nlyr(archivo_verif), ncol(archivo_verif), nrow(archivo_verif)))
        
        # Guardar información sobre las bandas (fechas)
        nombres_bandas <- names(rasters_anio)
        fechas_bandas <- sapply(rasters_anio, function(x) names(x))
        
        info_bandas <- data.frame(
          banda = 1:length(fechas_bandas),
          fecha = fechas_bandas,
          nombre = nombres_bandas
        )
        
        write.csv(info_bandas, 
                  paste0(dir_salida, "bandas_prcp_", anio_actual, ".csv"),
                  row.names = FALSE)
      }
    }
    
    # Liberar memoria
    rm(raster_anual, rasters_anio)
    gc()
  } else {
    cat("No se generaron rasters para el año", anio_actual, "\n")
  }
}

# ==================== 11. GUARDAR ESTADÍSTICAS DE CORRECCIÓN ====================
cat("\n\n=== ESTADÍSTICAS DE CORRECCIÓN ZERO-THRESHOLDING ===\n")

if (nrow(estadisticas_correccion) > 0) {
  # Resumen anual
  resumen_anual <- estadisticas_correccion %>%
    group_by(anio) %>%
    summarise(
      dias_procesados = n(),
      media_precip = mean(media, na.rm = TRUE),
      pct_ceros_prom = mean(pct_ceros, na.rm = TRUE),
      pct_truncados_prom = mean(pct_truncados, na.rm = TRUE),
      max_historico = max(max_original, na.rm = TRUE)
    )
  
  cat("\nResumen anual de correcciones:\n")
  print(resumen_anual)
  
  # Guardar estadísticas detalladas
  write.csv(estadisticas_correccion,
            paste0(dir_salida, "estadisticas_correccion_post_rfsi.csv"),
            row.names = FALSE)
  
  # Guardar resumen anual
  write.csv(resumen_anual,
            paste0(dir_salida, "resumen_correccion_anual.csv"),
            row.names = FALSE)
  
  cat("\n✓ Estadísticas guardadas en:", dir_salida, "\n")
} else {
  cat("No se generaron estadísticas de corrección\n")
}

# ==================== 12. RESUMEN FINAL ====================
cat("\n", rep("=", 60), "\n", sep="")
cat("PROCESO COMPLETADO EXITOSAMENTE\n")
cat(rep("=", 60), "\n\n")

cat("RESUMEN DE LA IMPLEMENTACIÓN ZERO-THRESHOLDING:\n")
cat("1. Modelo RFSI predice precipitación continua\n")
cat("2. Corrección física POST-RFSI aplicada:\n")
cat("   a) Valores < ", UMBRAL_MIN_MM, " mm → 0 (eliminar lloviznas artificiales)\n")
cat("   b) Valores > ", UMBRAL_MAX_MM, " mm → ", UMBRAL_MAX_MM, " mm (límite físico)\n")
cat("\nVENTAJAS:\n")
cat("• No falla con pocas estaciones\n")
cat("• Controla sobreestimación del modelo\n")
cat("• Defendible científicamente (hidrología operativa)\n")
cat("• Mantiene la estructura espacial del RFSI\n")
cat("\nArchivos generados en:", dir_salida, "\n")



# ######
#  RFSI PC ESTRICTA
######################################################################################


library(sf)
library(dplyr)
library(meteo)
library(ranger)
library(terra)
library(lubridate)
library(CAST)

# ==================== CONFIGURACIÓN ====================
cat("=== PROCESAMIENTO RFSI ANUAL CON DATOS UTM ===\n")

# Directorios
dir_datos <- "D:/S/Serbia1km/Interpolation/"
dir_salida <- "D:/QSWAT/Output_PC-RFSI7_fisico_Anual/"

# Umbrales físicos para post-procesamiento
UMBRAL_MIN_MM <- 0.5    # Menos de 0.5mm = 0 (eliminar lloviznas artificiales)

# Crear directorio de salida
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
  cat("Directorio creado:", dir_salida, "\n")
}

# ==================== 1. CARGAR Y PREPARAR DATOS ====================
cat("\n1. CARGANDO DATOS DE ESTACIONES...\n")
data <- read.csv(paste0(dir_datos, "stfdf_pt_huallaga.csv"))

# Procesar datos
data <- data %>%
  mutate(
    time = as.Date(time, format = "%m/%d/%Y"),
    date = time,
    doy = yday(date),
    month = month(date),
    year = year(date),
    staid = sp.ID
  ) %>%
  filter(
    !is.na(prcp), !is.na(precland), !is.na(tmax), !is.na(tmin),
    !is.na(lon), !is.na(lat), !is.na(dem)
  )

cat("Datos cargados:", nrow(data), "registros\n")
cat("Rango de fechas:", min(data$date), "a", max(data$date), "\n")

# ==================== 2. CONVERTIR ESTACIONES A UTM ====================
cat("\n2. CONVIRTIENDO ESTACIONES A UTM...\n")

# Convertir a UTM zona 18S (EPSG:32718)
data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
data_sf <- st_transform(data_sf, crs = 32718)

# Extraer coordenadas UTM
coords_utm <- st_coordinates(data_sf)
data <- data %>%
  mutate(
    x_utm = coords_utm[, "X"],
    y_utm = coords_utm[, "Y"],
    z = as.numeric(date - min(date)),
    #doy_sin = sin(2 * pi * doy / 365.25),
    #doy_cos = cos(2 * pi * doy / 365.25)
  )

cat("Estaciones convertidas a UTM\n")

# ==================== 3. CARGAR RASTERS UTM ====================
cat("\n3. CARGANDO RASTERS UTM...\n")

dir_utm <- paste0(dir_datos, "Huallaga/variabl_utm/")

# Cargar rasters estáticos UTM
r_dem_utm <- rast(paste0(dir_utm, "dem_utm.tif"))
r_twi_utm <- rast(paste0(dir_utm, "twi_utm.tif"))
r_sin_utm <- rast(paste0(dir_utm, "sin_utm.tif"))
r_cos_utm <- rast(paste0(dir_utm, "cos_utm.tif"))
r_slope_utm <- rast(paste0(dir_utm, "slope_utm.tif"))

# Cargar rasters dinámicos UTM (multibanda: días desde 2000-01-01)
r_precland_utm <- rast(paste0(dir_utm, "precland_2000_2003_utm.tif"))
r_tmax_utm <- rast(paste0(dir_utm, "tmax_2000_2003_utm.tif"))
r_tmin_utm <- rast(paste0(dir_utm, "tmin_2000_2003_utm.tif"))

# Verificar que todos los rasters existen
cat("  dem_utm.tif:", ifelse(nrow(r_dem_utm) > 0, "OK", "ERROR"), "\n")
cat("  twi_utm.tif:", ifelse(nrow(r_twi_utm) > 0, "OK", "ERROR"), "\n")
cat("  sin_utm.tif:", ifelse(nrow(r_sin_utm) > 0, "OK", "ERROR"), "\n")
cat("  cos_utm.tif:", ifelse(nrow(r_cos_utm) > 0, "OK", "ERROR"), "\n")
cat("  slope_utm.tif:", ifelse(nrow(r_slope_utm) > 0, "OK", "ERROR"), "\n")
cat("  precland_2000_2003_utm.tif:", ifelse(nrow(r_precland_utm) > 0, "OK", "ERROR"), "\n")
cat("  tmax_2000_2003_utm.tif:", ifelse(nrow(r_tmax_utm) > 0, "OK", "ERROR"), "\n")
cat("  tmin_2000_2003_utm.tif:", ifelse(nrow(r_tmin_utm) > 0, "OK", "ERROR"), "\n")

# Definir la fecha de inicio de los rasters dinámicos (asumimos 2000-01-01)
fecha_inicio_raster <- as.Date("2000-01-01")

# ==================== 4. CREAR GRILLA EN UTM ====================
cat("\n4. CREANDO GRILLA EN UTM...\n")

# Leer cuenca y convertir a UTM
cuenca <- st_read(paste0(dir_datos, "Titicaca_border/watershed.shp"))
cuenca <- st_transform(cuenca, crs = 32718)

# Usar la extensión de los rasters UTM
ext_utm <- ext(r_dem_utm)

# Crear grilla de puntos UTM (resolución de 5000m = 5km)
resolucion_m <- 5000
x_seq <- seq(ext_utm[1], ext_utm[2], by = resolucion_m)
y_seq <- seq(ext_utm[3], ext_utm[4], by = resolucion_m)
grid_utm <- expand.grid(x = x_seq, y = y_seq)

# Convertir a sf y filtrar puntos dentro de la cuenca
grid_sf <- st_as_sf(grid_utm, coords = c("x", "y"), crs = 32718)
grid_dentro <- grid_sf[st_intersects(grid_sf, cuenca, sparse = FALSE), ]

# Extraer coordenadas
coords_grid <- st_coordinates(grid_dentro)
grid_cells_utm <- data.frame(x = coords_grid[, "X"], y = coords_grid[, "Y"])

cat("Grilla UTM creada:", nrow(grid_cells_utm), "puntos\n")

# ==================== 5. EXTRAER VALORES DE RASTERS ESTÁTICOS UTM ====================
cat("\n5. EXTRAYENDO VALORES DE RASTERS ESTÁTICOS...\n")

# Crear SpatVector para extracción
grid_vect <- vect(grid_cells_utm, geom = c("x", "y"), crs = "EPSG:32718")

# Extraer valores de cada raster estático
grid_cells_utm$dem <- terra::extract(r_dem_utm, grid_vect)[, 2]
grid_cells_utm$twi <- terra::extract(r_twi_utm, grid_vect)[, 2]
grid_cells_utm$sin <- terra::extract(r_sin_utm, grid_vect)[, 2]
grid_cells_utm$cos <- terra::extract(r_cos_utm, grid_vect)[, 2]
grid_cells_utm$slope <- terra::extract(r_slope_utm, grid_vect)[, 2]

# Rellenar NAs
grid_cells_utm <- grid_cells_utm %>%
  mutate(
    dem = ifelse(is.na(dem), median(dem, na.rm = TRUE), dem),
    twi = ifelse(is.na(twi), median(twi, na.rm = TRUE), twi),
    sin = ifelse(is.na(sin), 0, sin),
    cos = ifelse(is.na(cos), 0, cos),
    slope = ifelse(is.na(slope), median(slope, na.rm = TRUE), slope)
  )

cat("Valores estáticos extraídos y procesados\n")

# ==================== 6. ENTRENAR MODELO RFSI ====================
cat("\n6. ENTRENANDO MODELO RFSI...\n")

# Fórmula del modelo
fm.RFSI <- as.formula("prcp ~ precland + tmax + tmin + dem + twi + slope + 
                      sin + cos + month")

# Entrenar modelo
set.seed(42)
n_cores_modelo <- max(1, parallel::detectCores() - 2)

rfsi_model <- rfsi(
  formula = fm.RFSI,
  data = data,
  data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
  n.obs = 5,
  soil3d = FALSE,
  cpus = n_cores_modelo,
  progress = TRUE,
  seed = 42,
  num.trees = 200,
  mtry = 5,
  min.node.size = 5,
  sample.fraction = 0.8
)

cat("Modelo entrenado exitosamente\n")
cat("Correcciones físicas PC-RFSI:\n")
cat("  - Umbral mínimo físico:", UMBRAL_MIN_MM, "mm\n")
cat("  - Umbral máximo: percentiles observacionales (día + mes)\n")

# ==================== CLIMATOLOGÍA MENSUAL ROBUSTA ====================
cat("\nCalculando umbrales mensuales dinámicos...\n")

umbral_mensual <- data %>%
  filter(prcp > 0) %>%
  group_by(month) %>%
  summarise(
    p95 = quantile(prcp, 0.95, na.rm = TRUE),
    p99 = quantile(prcp, 0.99, na.rm = TRUE),
    n = n()
  )

print(umbral_mensual)

# Límite físico absoluto (seguridad)
#UMBRAL_ABS_MAX <- 20  # mm/día (evento extremo regional)

# ==================== 7. FUNCIÓN QUE DEVUELVE RASTER POR DÍA ====================
procesar_dia_a_raster <- function(fecha, datos_dia, grid_base, modelo, datos_entrenamiento,
                                  r_precland, r_tmax, r_tmin, fecha_inicio
                                  ) {
  
  tryCatch({
    # 1. Preparar grilla del día
    grid_dia <- grid_base
    
    # 2. Calcular el índice de la banda (día) en los rasters dinámicos
    # Asumimos que la primera banda es fecha_inicio (2000-01-01)
    indice_dia <- as.numeric(fecha - fecha_inicio) + 1
    
    # Verificar que el índice esté dentro del rango de bandas
    if (indice_dia < 1 | indice_dia > nlyr(r_precland)) {
      return(NULL)
    }
    
    # 3. Extraer valores de los rasters dinámicos para este día
    grid_vect <- vect(grid_dia[, c("x", "y")], geom = c("x", "y"), crs = "EPSG:32718")
    
    # Extraer la banda correspondiente de cada raster dinámico
    grid_dia$precland <- terra::extract(r_precland[[indice_dia]], grid_vect)[, 2]
    grid_dia$tmax <- terra::extract(r_tmax[[indice_dia]], grid_vect)[, 2]
    grid_dia$tmin <- terra::extract(r_tmin[[indice_dia]], grid_vect)[, 2]
    
    # 4. Preparar dataframe para predicción
    grid_pred_df <- data.frame(
      staid = 1:nrow(grid_dia),
      x = grid_dia$x,
      y = grid_dia$y,
      z = as.numeric(fecha - min(datos_entrenamiento$date)),
      #doy_sin = sin(2 * pi * yday(fecha) / 365.25),
      #doy_cos = cos(2 * pi * yday(fecha) / 365.25),
      month = month(fecha),
      dem = grid_dia$dem,
      twi = grid_dia$twi,
      slope = grid_dia$slope,
      sin = grid_dia$sin,
      cos = grid_dia$cos,
      precland = grid_dia$precland,
      tmax = grid_dia$tmax,
      tmin = grid_dia$tmin
    )
    
    # 5. Eliminar NAs
    grid_clean <- na.omit(grid_pred_df)
    if (nrow(grid_clean) == 0) {
      return(NULL)
    }
    
    # 6. Muestrear si hay muchos puntos (para eficiencia)
    max_puntos <- 10000
    if (nrow(grid_clean) > max_puntos) {
      set.seed(42)
      idx_muestra <- sample(1:nrow(grid_clean), max_puntos)
      grid_clean <- grid_clean[idx_muestra, ]
    }
    
    # 7. Predecir con RFSI
    prediccion <- suppressWarnings(
      pred.rfsi(
        model = modelo,
        data = datos_entrenamiento,
        obs.col = "prcp",
        data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
        newdata = grid_clean,
        newdata.staid.x.y.z = c("staid", "x", "y", "z"),
        output.format = "data.frame",
        cpus = 1,
        progress = FALSE,
        soil3d = FALSE
      )
    )
    
    # 8. APLICAR ZERO-THRESHOLDING POST-RFSI (CORRECCIÓN FÍSICA)
    cat("  -> Aplicando correcciones físicas... ")
    
    # Asegurar valores no negativos primero
    pred_values <- pmax(0, prediccion$pred)
    
    # 1. Día sin soporte pluviométrico
    # 1. Día sin soporte pluviométrico
    if (sum(datos_dia$prcp > 0, na.rm = TRUE) < 2) {
      pred_values[] <- 0
      cat("Día seco → raster forzado a 0\n")
      
    } else {
      
      # 2. Umbral mínimo adaptativo
      umbral_min_dia <- max(
        UMBRAL_MIN_MM,
        quantile(datos_dia$prcp, 0.10, na.rm = TRUE)
      )
      pred_values[pred_values < umbral_min_dia] <- 0
      
      # 3. Umbral máximo dinámico (OBSERVACIONAL)
      mes_actual <- month(fecha)
      
      # Percentil mensual climatológico
      umbral_mes <- umbral_mensual$p95[
        umbral_mensual$month == mes_actual
      ]
      
      if (length(umbral_mes) == 0 || is.na(umbral_mes)) {
        umbral_mes <- Inf
      }
      
      # Percentil diario (solo con soporte)
      if (sum(datos_dia$prcp > 0, na.rm = TRUE) >= 5) {
        umbral_dia <- quantile(datos_dia$prcp, 0.95, na.rm = TRUE)
      } else {
        umbral_dia <- umbral_mes
      }
      
      # Umbral final PC-RFSI
      umbral_max_final <- min(umbral_dia, umbral_mes)
      
      pred_values[pred_values > umbral_max_final] <- umbral_max_final
      
      cat(sprintf(
        "Corrección: min=%.2f mm | max=%.2f mm (mes %d)\n",
        umbral_min_dia, umbral_max_final, mes_actual
      ))
    }
    # 9. Crear raster UTM
    grid_clean$pred <- pred_values
    
    # Crear raster UTM
    raster_dia_utm <- terra::rast(
      grid_clean[, c("x", "y", "pred")],
      type = "xyz",
      crs = "EPSG:32718"
    )
    
    # Convertir a WGS84
    raster_dia_wgs <- terra::project(raster_dia_utm, "EPSG:4326")
    
    # Nombre de la capa con la fecha
    names(raster_dia_wgs) <- format(fecha, "%Y%m%d")
    
    return(raster_dia_wgs)
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(NULL)
  })
}

# ==================== 9. PROCESAR POR AÑOS ====================
cat("\n7. PROCESANDO POR AÑOS...\n")

# Obtener años únicos
anios <- unique(year(data$date))
cat("Años disponibles:", toString(anios), "\n")

# Dataframe para almacenar estadísticas de corrección
estadisticas_correccion <- data.frame()

# Procesar cada año por separado
for (anio_actual in anios) {

  # Filtrar fechas del año actual
  fechas_anio <- sort(unique(data$date[year(data$date) == anio_actual]))
  
  # PRIMERO: Probar con solo 5 días del año
  # Para procesar todo el año, cambia la siguiente línea a: fechas_procesar <- fechas_anio
  fechas_procesar <- fechas_anio[1:366]  # Cambiar a fechas_anio para todos los días
  
  cat("Días a procesar:", length(fechas_procesar), "de", length(fechas_anio), "\n\n")
  
  # Lista para almacenar rasters del año
  rasters_anio <- list()
  dias_exitosos <- 0
  
  # Procesar días del año
  for (i in 1:length(fechas_procesar)) {
    fecha_actual <- fechas_procesar[i]
    datos_dia <- data %>% filter(date == fecha_actual)
    
    if (nrow(datos_dia) >= 3) {
      cat(sprintf("[%d/%d] %s ... ", i, length(fechas_procesar), format(fecha_actual, "%Y-%m-%d")))
      
      raster_dia <- procesar_dia_a_raster(
        fecha_actual, 
        datos_dia, 
        grid_cells_utm, 
        rfsi_model, 
        data,
        r_precland_utm,
        r_tmax_utm,
        r_tmin_utm,
        fecha_inicio_raster
      )
      
      if (!is.null(raster_dia)) {
        # 
        rasters_anio <- c(rasters_anio, list(raster_dia))
      }
    }
  }
  
  # ==================== 10. CREAR Y GUARDAR RASTER MULTIBANDA ANUAL ====================
  if (length(rasters_anio) > 0) {
    #cat("\nCreando raster multibanda para el año", anio_actual, "...\n")
    
    # Combinar todos los rasters del año en un solo objeto multibanda
    raster_anual <- terra::rast(rasters_anio)
    
    # Guardar el raster multibanda anual
    nombre_archivo <- paste0(dir_salida, "prcp_", anio_actual, ".tif")
    
    cat("Guardando:", nombre_archivo, "\n")
    cat("Número de bandas (días):", nlyr(raster_anual), "\n")
    
    terra::writeRaster(
      raster_anual,
      filename = nombre_archivo,
      filetype = "GTiff",
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES")
    )
    
    # Verificar que se guardó correctamente
    cat("✓ Guardado:", archivo_salida, "\n")
  }
}


# ==================== 12. RESUMEN FINAL ====================
cat("\n", rep("=", 60), "\n", sep="")
cat("PROCESO COMPLETADO EXITOSAMENTE\n")
cat(rep("=", 60), "\n\n")

cat("RESUMEN DE LA IMPLEMENTACIÓN ZERO-THRESHOLDING:\n")
cat("1. Modelo RFSI predice precipitación continua\n")
cat("2. Corrección física POST-RFSI aplicada:\n")
cat("   a) Valores < ", UMBRAL_MIN_MM, " mm → 0 (eliminar lloviznas artificiales)\n")
cat("   b) Valores máximos controlados por percentiles observados (día y mes)\n")
cat("\nVENTAJAS:\n")
cat("• No falla con pocas estaciones\n")
cat("• Controla sobreestimación del modelo\n")
cat("• Defendible científicamente (hidrología operativa)\n")
cat("• Mantiene la estructura espacial del RFSI\n")
cat("\nArchivos generados en:", dir_salida, "\n")

############ PUBLICABLE 2 OKK

library(sf)
library(dplyr)
library(meteo)
library(ranger)
library(terra)
library(lubridate)
library(CAST)

# ==================== CONFIGURACIÓN ====================
cat("=== PROCESAMIENTO RFSI ANUAL CON DATOS UTM ===\n")

# Directorios
dir_datos <- "D:/S/Serbia1km/Interpolation/"
dir_salida <- "D:/QSWAT/Output_PC-RFSI3_fisico_Anual/"

# Umbrales físicos para post-procesamiento
UMBRAL_MIN_MM <- 0.5    # Menos de 0.5mm = 0 (eliminar lloviznas artificiales)

# Crear directorio de salida
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
  cat("Directorio creado:", dir_salida, "\n")
}

# ==================== FUNCIÓN AUXILIAR: CALCULAR ESTADÍSTICAS ====================
calcular_estadisticas_dia <- function(raster_dia) {
  if (is.null(raster_dia)) {
    return(data.frame(
      media = NA,
      maximo = NA,
      minimo = NA,
      celdas_cero = NA,
      celdas_total = NA
    ))
  }
  
  valores <- values(raster_dia)
  valores <- valores[!is.na(valores)]
  
  if (length(valores) == 0) {
    return(data.frame(
      media = 0,
      maximo = 0,
      minimo = 0,
      celdas_cero = 0,
      celdas_total = 0
    ))
  }
  
  data.frame(
    media = round(mean(valores, na.rm = TRUE), 2),
    maximo = round(max(valores, na.rm = TRUE), 2),
    minimo = round(min(valores[valores > 0], na.rm = TRUE), 2),
    celdas_cero = sum(valores == 0),
    celdas_total = length(valores)
  )
}

# ==================== 1. CARGAR Y PREPARAR DATOS ====================
cat("\n1. CARGANDO DATOS DE ESTACIONES...\n")
data <- read.csv(paste0(dir_datos, "stfdf_pt_huallaga.csv"))

# Procesar datos
data <- data %>%
  mutate(
    time = as.Date(time, format = "%m/%d/%Y"),
    date = time,
    doy = yday(date),
    month = month(date),
    year = year(date),
    staid = sp.ID
  ) %>%
  filter(
    !is.na(prcp), !is.na(precland), !is.na(tmax), !is.na(tmin),
    !is.na(lon), !is.na(lat), !is.na(dem)
  )

cat("Datos cargados:", nrow(data), "registros\n")
cat("Rango de fechas:", min(data$date), "a", max(data$date), "\n")
cat("Número de estaciones únicas:", length(unique(data$staid)), "\n")

# ==================== 2. CONVERTIR ESTACIONES A UTM ====================
cat("\n2. CONVIRTIENDO ESTACIONES A UTM...\n")

# Convertir a UTM zona 18S (EPSG:32718)
data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
data_sf <- st_transform(data_sf, crs = 32718)

# Extraer coordenadas UTM
coords_utm <- st_coordinates(data_sf)
data <- data %>%
  mutate(
    x_utm = coords_utm[, "X"],
    y_utm = coords_utm[, "Y"],
    z = as.numeric(date - min(date))
  )

cat("Estaciones convertidas a UTM\n")

# ==================== 3. CARGAR RASTERS UTM ====================
cat("\n3. CARGANDO RASTERS UTM...\n")

dir_utm <- paste0(dir_datos, "Huallaga/variabl_utm/")

# Verificar existencia de archivos
archivos_necesarios <- c(
  "dem_utm.tif", "twi_utm.tif", "sin_utm.tif", "cos_utm.tif", "slope_utm.tif",
  "precland_2000_2003_utm.tif", "tmax_2000_2003_utm.tif", "tmin_2000_2003_utm.tif"
)

for (archivo in archivos_necesarios) {
  ruta <- paste0(dir_utm, archivo)
  if (file.exists(ruta)) {
    cat("  ✓", archivo, "\n")
  } else {
    cat("  ✗ ERROR:", archivo, "no encontrado\n")
    stop("Archivo requerido no encontrado: ", archivo)
  }
}

# Cargar rasters estáticos UTM
r_dem_utm <- rast(paste0(dir_utm, "dem_utm.tif"))
r_twi_utm <- rast(paste0(dir_utm, "twi_utm.tif"))
r_sin_utm <- rast(paste0(dir_utm, "sin_utm.tif"))
r_cos_utm <- rast(paste0(dir_utm, "cos_utm.tif"))
r_slope_utm <- rast(paste0(dir_utm, "slope_utm.tif"))

# Cargar rasters dinámicos UTM
r_precland_utm <- rast(paste0(dir_utm, "precland_2000_2003_utm.tif"))
r_tmax_utm <- rast(paste0(dir_utm, "tmax_2000_2003_utm.tif"))
r_tmin_utm <- rast(paste0(dir_utm, "tmin_2000_2003_utm.tif"))

# Obtener información de los rasters dinámicos
cat("\nInformación de rasters dinámicos:\n")
cat("  precland: ", nlyr(r_precland_utm), "bandas\n")
cat("  tmax:     ", nlyr(r_tmax_utm), "bandas\n")
cat("  tmin:     ", nlyr(r_tmin_utm), "bandas\n")

# Definir la fecha de inicio de los rasters dinámicos
fecha_inicio_raster <- as.Date("2000-01-01")

# ==================== 4. CREAR GRILLA EN UTM ====================
cat("\n4. CREANDO GRILLA EN UTM...\n")

# Leer cuenca y convertir a UTM
cuenca <- st_read(paste0(dir_datos, "Titicaca_border/watershed.shp"))
cuenca <- st_transform(cuenca, crs = 32718)

# Usar la extensión de los rasters UTM
ext_utm <- ext(r_dem_utm)

# Crear grilla de puntos UTM (resolución de 5000m = 5km)
resolucion_m <- 5000
x_seq <- seq(ext_utm[1], ext_utm[2], by = resolucion_m)
y_seq <- seq(ext_utm[3], ext_utm[4], by = resolucion_m)
grid_utm <- expand.grid(x = x_seq, y = y_seq)

# Convertir a sf y filtrar puntos dentro de la cuenca
grid_sf <- st_as_sf(grid_utm, coords = c("x", "y"), crs = 32718)
grid_dentro <- grid_sf[st_intersects(grid_sf, cuenca, sparse = FALSE), ]

# Extraer coordenadas
coords_grid <- st_coordinates(grid_dentro)
grid_cells_utm <- data.frame(x = coords_grid[, "X"], y = coords_grid[, "Y"])

cat("Grilla UTM creada:", nrow(grid_cells_utm), "puntos\n")

# ==================== 5. EXTRAER VALORES DE RASTERS ESTÁTICOS UTM ====================
cat("\n5. EXTRAYENDO VALORES DE RASTERS ESTÁTICOS...\n")

# Crear SpatVector para extracción
grid_vect <- vect(grid_cells_utm, geom = c("x", "y"), crs = "EPSG:32718")

# Extraer valores de cada raster estático
grid_cells_utm$dem <- terra::extract(r_dem_utm, grid_vect)[, 2]
grid_cells_utm$twi <- terra::extract(r_twi_utm, grid_vect)[, 2]
grid_cells_utm$sin <- terra::extract(r_sin_utm, grid_vect)[, 2]
grid_cells_utm$cos <- terra::extract(r_cos_utm, grid_vect)[, 2]
grid_cells_utm$slope <- terra::extract(r_slope_utm, grid_vect)[, 2]

# Contar NAs iniciales
nas_inicial <- sum(is.na(grid_cells_utm$dem))

# Rellenar NAs con medianas
grid_cells_utm <- grid_cells_utm %>%
  mutate(
    dem = ifelse(is.na(dem), median(dem, na.rm = TRUE), dem),
    twi = ifelse(is.na(twi), median(twi, na.rm = TRUE), twi),
    sin = ifelse(is.na(sin), 0, sin),
    cos = ifelse(is.na(cos), 0, cos),
    slope = ifelse(is.na(slope), median(slope, na.rm = TRUE), slope)
  )

cat("Valores estáticos extraídos y procesados\n")
cat("  NAs reemplazados:", nas_inicial, "\n")

# ==================== 6. CALCULAR CLIMATOLOGÍA MENSUAL ====================
cat("\n6. CALCULANDO CLIMATOLOGÍA MENSUAL...\n")

umbral_mensual <- data %>%
  filter(prcp > 0) %>%
  group_by(month) %>%
  summarise(
    p95 = quantile(prcp, 0.95, na.rm = TRUE),
    p99 = quantile(prcp, 0.99, na.rm = TRUE),
    n_dias = n()
  )

print(umbral_mensual)

# ==================== 7. ENTRENAR MODELO RFSI ====================
cat("\n7. ENTRENANDO MODELO RFSI...\n")

# Fórmula del modelo
fm.RFSI <- as.formula("prcp ~ precland + tmax + tmin + dem + twi + slope + 
                      sin + cos + month")

# Entrenar modelo
set.seed(42)
n_cores_modelo <- max(1, parallel::detectCores() - 2)

cat("  Entrenando modelo con", n_cores_modelo, "núcleos...\n")
rfsi_model <- rfsi(
  formula = fm.RFSI,
  data = data,
  data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
  n.obs = 5,
  soil3d = FALSE,
  cpus = n_cores_modelo,
  progress = TRUE,
  seed = 42,
  num.trees = 200,
  mtry = 5,
  min.node.size = 5,
  sample.fraction = 0.8
)

cat("Modelo entrenado exitosamente\n")
cat("  Número de árboles:", rfsi_model$num.trees, "\n")
cat("  Tamaño mínimo de nodo:", rfsi_model$min.node.size, "\n")

# ==================== 8. FUNCIÓN PARA PROCESAR UN DÍA ====================
procesar_dia_a_raster <- function(fecha, datos_dia, grid_base, modelo, datos_entrenamiento,
                                  r_precland, r_tmax, r_tmin, fecha_inicio) {
  
  tryCatch({
    # 1. Preparar grilla del día
    grid_dia <- grid_base
    
    # 2. Calcular el índice de la banda (día) en los rasters dinámicos
    indice_dia <- as.numeric(fecha - fecha_inicio) + 1
    
    # Verificar que el índice esté dentro del rango de bandas
    if (indice_dia < 1 | indice_dia > nlyr(r_precland)) {
      cat("  Índice fuera de rango (", indice_dia, " de ", nlyr(r_precland), ")\n", sep = "")
      return(NULL)
    }
    
    # 3. Extraer valores de los rasters dinámicos para este día
    grid_vect <- vect(grid_dia[, c("x", "y")], geom = c("x", "y"), crs = "EPSG:32718")
    
    # Extraer la banda correspondiente de cada raster dinámico
    grid_dia$precland <- terra::extract(r_precland[[indice_dia]], grid_vect)[, 2]
    grid_dia$tmax <- terra::extract(r_tmax[[indice_dia]], grid_vect)[, 2]
    grid_dia$tmin <- terra::extract(r_tmin[[indice_dia]], grid_vect)[, 2]
    
    # 4. Preparar dataframe para predicción
    grid_pred_df <- data.frame(
      staid = 1:nrow(grid_dia),
      x = grid_dia$x,
      y = grid_dia$y,
      z = as.numeric(fecha - min(datos_entrenamiento$date)),
      month = month(fecha),
      dem = grid_dia$dem,
      twi = grid_dia$twi,
      slope = grid_dia$slope,
      sin = grid_dia$sin,
      cos = grid_dia$cos,
      precland = grid_dia$precland,
      tmax = grid_dia$tmax,
      tmin = grid_dia$tmin
    )
    
    # 5. Eliminar NAs
    grid_clean <- na.omit(grid_pred_df)
    if (nrow(grid_clean) == 0) {
      cat("  Sin datos válidos después de limpieza\n")
      return(NULL)
    }
    
    # 6. Muestrear si hay muchos puntos (para eficiencia)
    max_puntos <- 10000
    if (nrow(grid_clean) > max_puntos) {
      set.seed(42)
      idx_muestra <- sample(1:nrow(grid_clean), max_puntos)
      grid_clean <- grid_clean[idx_muestra, ]
    }
    
    # 7. Predecir con RFSI
    prediccion <- suppressWarnings(
      pred.rfsi(
        model = modelo,
        data = datos_entrenamiento,
        obs.col = "prcp",
        data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
        newdata = grid_clean,
        newdata.staid.x.y.z = c("staid", "x", "y", "z"),
        output.format = "data.frame",
        cpus = 1,
        progress = FALSE,
        soil3d = FALSE
      )
    )
    
    # 8. APLICAR CORRECCIONES FÍSICAS
    # Asegurar valores no negativos primero
    pred_values <- pmax(0, prediccion$pred)
    
    # Verificar si es un día seco
    estaciones_con_lluvia <- sum(datos_dia$prcp > 0, na.rm = TRUE)
    
    if (estaciones_con_lluvia < 2) {
      pred_values[] <- 0
      cat("  Día seco (", estaciones_con_lluvia, " estaciones) → 0 mm\n", sep = "")
    } else {
      # Umbral mínimo adaptativo
      umbral_min_dia <- max(
        UMBRAL_MIN_MM,
        quantile(datos_dia$prcp, 0.10, na.rm = TRUE)
      )
      pred_values[pred_values < umbral_min_dia] <- 0
      
      # Umbral máximo dinámico (OBSERVACIONAL)
      mes_actual <- month(fecha)
      
      # Percentil mensual climatológico
      umbral_mes <- if (mes_actual %in% umbral_mensual$month) {
        umbral_mensual$p95[umbral_mensual$month == mes_actual]
      } else {
        Inf
      }
      
      # Percentil diario (solo con soporte)
      if (estaciones_con_lluvia >= 5) {
        umbral_dia <- quantile(datos_dia$prcp, 0.95, na.rm = TRUE)
      } else {
        umbral_dia <- umbral_mes
      }
      
      # Umbral final PC-RFSI
      umbral_max_final <- min(umbral_dia, umbral_mes, na.rm = TRUE)
      
      if (is.finite(umbral_max_final)) {
        pred_values[pred_values > umbral_max_final] <- umbral_max_final
      }
      
      cat(sprintf("  Corr: min=%.2f max=%.2f mm (mes %d, %d estaciones)\n",
                  umbral_min_dia, umbral_max_final, mes_actual, estaciones_con_lluvia))
    }
    
    # 9. Crear raster UTM
    grid_clean$pred <- pred_values
    
    # Crear raster UTM
    raster_dia_utm <- terra::rast(
      grid_clean[, c("x", "y", "pred")],
      type = "xyz",
      crs = "EPSG:32718"
    )
    
    # Convertir a WGS84
    raster_dia_wgs <- terra::project(raster_dia_utm, "EPSG:4326")
    
    # Nombre de la capa con la fecha
    names(raster_dia_wgs) <- format(fecha, "%Y%m%d")
    
    return(raster_dia_wgs)
    
  }, error = function(e) {
    cat("  ERROR en procesar_dia_a_raster:", e$message, "\n")
    return(NULL)
  })
}

# ==================== 9. PROCESAR POR AÑOS ====================
cat("\n8. PROCESANDO POR AÑOS...\n")

# Obtener años únicos
anios <- unique(year(data$date))
cat("Años disponibles:", toString(anios), "\n")

# Crear dataframe para estadísticas
estadisticas_anuales <- data.frame()

# Procesar cada año por separado
for (anio_actual in anios) {
  cat("\n", paste0(rep("=", 60), collapse = ""), "\n", sep = "")
  cat("PROCESANDO AÑO:", anio_actual, "\n")
  cat(paste0(rep("=", 60), collapse = ""), "\n\n", sep = "")
  
  # Filtrar fechas del año actual
  fechas_anio <- sort(unique(data$date[year(data$date) == anio_actual]))
  
  # Para prueba: procesar solo los primeros 5 días
  # Para procesar completo: cambiar a fechas_anio
  fechas_procesar <- fechas_anio[1:31]  # PRUEBA - cambiar a fechas_anio para todo el año
  
  cat("Días a procesar:", length(fechas_procesar), "de", length(fechas_anio), "\n\n")
  
  # Lista para almacenar rasters del año
  rasters_anio <- list()
  estadisticas_diarias <- data.frame()
  
  # Procesar días del año
  for (i in seq_along(fechas_procesar)) {
    fecha_actual <- fechas_procesar[i]
    datos_dia <- data %>% filter(date == fecha_actual)
    
    cat(sprintf("[%d/%d] %s: ", i, length(fechas_procesar), format(fecha_actual, "%Y-%m-%d")))
    
    if (nrow(datos_dia) >= 3) {
      raster_dia <- procesar_dia_a_raster(
        fecha_actual, 
        datos_dia, 
        grid_cells_utm, 
        rfsi_model, 
        data,
        r_precland_utm,
        r_tmax_utm,
        r_tmin_utm,
        fecha_inicio_raster
      )
      
      if (!is.null(raster_dia)) {
        rasters_anio[[format(fecha_actual, "%Y%m%d")]] <- raster_dia
        
        # Calcular estadísticas
        stats_dia <- calcular_estadisticas_dia(raster_dia)
        stats_dia$fecha <- as.character(fecha_actual)
        stats_dia$anio <- anio_actual
        estadisticas_diarias <- rbind(estadisticas_diarias, stats_dia)
        
        cat("OK\n")
      } else {
        cat("FALLÓ\n")
      }
    } else {
      cat(sprintf("Pocos datos (%d estaciones) - SKIP\n", nrow(datos_dia)))
    }
    
    # Liberar memoria cada 5 días
    if (i %% 5 == 0) {
      gc()
    }
  }
  
  # ==================== 10. CREAR Y GUARDAR RASTER MULTIBANDA ANUAL ====================
  if (length(rasters_anio) > 0) {
    cat("\nCombinando", length(rasters_anio), "rasters diarios...\n")
    
    # Combinar todos los rasters del año en un solo objeto multibanda
    raster_anual <- terra::rast(rasters_anio)
    
    # Guardar el raster multibanda anual
    nombre_archivo <- paste0(dir_salida, "prcp_", anio_actual, ".tif")
    
    cat("Guardando:", basename(nombre_archivo), "\n")
    cat("  Dimensiones:", ncol(raster_anual), "x", nrow(raster_anual), "\n")
    cat("  Número de bandas:", nlyr(raster_anual), "\n")
    
    terra::writeRaster(
      raster_anual,
      filename = nombre_archivo,
      filetype = "GTiff",
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES")
    )
    
    # Verificar que se guardó correctamente
    if (file.exists(nombre_archivo)) {
      file_size <- file.size(nombre_archivo) / 1024^2  # MB
      cat("  ✓ Guardado: ", basename(nombre_archivo), 
          sprintf("(%.1f MB)\n", file_size))
      
      # Guardar estadísticas
      if (nrow(estadisticas_diarias) > 0) {
        write.csv(estadisticas_diarias,
                  paste0(dir_salida, "estadisticas_prcp_", anio_actual, ".csv"),
                  row.names = FALSE)
        cat("  ✓ Estadísticas guardadas\n")
      }
      
      # Guardar metadatos de bandas
      info_bandas <- data.frame(
        banda = 1:length(rasters_anio),
        fecha = names(rasters_anio),
        nombre = paste0("prcp_", names(rasters_anio))
      )
      
      write.csv(info_bandas,
                paste0(dir_salida, "bandas_prcp_", anio_actual, ".csv"),
                row.names = FALSE)
      
      cat("  ✓ Metadatos de bandas guardados\n")
      
    } else {
      cat("  ✗ ERROR: No se pudo guardar el archivo\n")
    }
    
    # Agregar estadísticas anuales
    if (nrow(estadisticas_diarias) > 0) {
      stats_anual <- data.frame(
        anio = anio_actual,
        dias_procesados = nrow(estadisticas_diarias),
        prcp_media = mean(estadisticas_diarias$media, na.rm = TRUE),
        prcp_max = max(estadisticas_diarias$maximo, na.rm = TRUE),
        dias_secos = sum(estadisticas_diarias$media == 0, na.rm = TRUE)
      )
      estadisticas_anuales <- rbind(estadisticas_anuales, stats_anual)
    }
    
    # Liberar memoria
    rm(raster_anual, rasters_anio)
    gc()
    
  } else {
    cat("\n✗ No se generaron rasters para el año", anio_actual, "\n")
  }
}

# ==================== 11. GUARDAR RESUMEN FINAL ====================
cat("\n", paste0(rep("=", 60), collapse = ""), "\n", sep = "")
cat("RESUMEN DEL PROCESO\n")
cat(paste0(rep("=", 60), collapse = ""), "\n\n", sep = "")

if (nrow(estadisticas_anuales) > 0) {
  print(estadisticas_anuales)
  
  # Guardar resumen
  write.csv(estadisticas_anuales,
            paste0(dir_salida, "resumen_procesamiento.csv"),
            row.names = FALSE)
  cat("\n✓ Resumen guardado en: resumen_procesamiento.csv\n")
}

cat("\n", paste0(rep("=", 60), collapse = ""), "\n", sep = "")
cat("PROCESO COMPLETADO EXITOSAMENTE\n")
cat(paste0(rep("=", 60), collapse = ""), "\n", sep = "")
