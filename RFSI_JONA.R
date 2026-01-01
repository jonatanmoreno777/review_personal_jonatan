# ==================== 0. INSTALAR/CARGAR PAQUETES ====================
library(sf)
library(dplyr)
library(meteo)
library(ranger)
library(terra)
library(ggplot2)
library(lubridate)
library(CAST)

# ==================== 1. CARGAR Y PREPARAR DATOS ====================
# Cargar datos
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_huallaga.csv")

# Convertir y crear variables temporales
data <- data %>%
  mutate(
    time = as.Date(time, format = "%m/%d/%Y"),
    date = time,
    doy = yday(date),
    month = month(date),
    year = year(date),
    staid = sp.ID,
    obs = prcp
  ) %>%
  filter(
    !is.na(prcp), !is.na(precland), !is.na(tmax), !is.na(tmin),
    !is.na(lon), !is.na(lat), !is.na(dem)
  )

# ==================== 2. CONVERTIR A UTM ====================
# Convertir a sf
data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
data_sf <- st_transform(data_sf, crs = 32718)

# Extraer coordenadas UTM
coords_utm <- st_coordinates(data_sf)
data_sf$x <- coords_utm[, "X"]
data_sf$y <- coords_utm[, "Y"]

# ==================== 3. PREPARAR DATAFRAME PARA RFSI ====================
data_df <- as.data.frame(data_sf)

# Crear z (tiempo en días desde primera fecha)
data_df$z <- as.numeric(data_df$date - min(data_df$date))

# Efectos estacionales cíclicos
data_df$doy_sin <- sin(2 * pi * data_df$doy / 365.25)
data_df$doy_cos <- cos(2 * pi * data_df$doy / 365.25)

# ==================== 4. DEFINIR FÓRMULA ====================
fm.RFSI <- as.formula("prcp ~ precland + tmax + tmin + dem + twi + slope + 
                      sin + cos + doy_sin + doy_cos + month")

# ==================== 5. ENTRENAR MODELO RFSI (SIN ERROR) ====================
cat("=== ENTRENANDO MODELO RFSI ESPACIO-TEMPORAL ===\n")

n_cores <- max(1, detectCores() - 1)
cat("Usando", n_cores, "núcleos de CPU\n")

set.seed(42)

# OPCIÓN RECOMENDADA: Sin especificar CRS (más simple)
rfsi_model <- rfsi(
  formula = fm.RFSI,
  data = data_df,
  data.staid.x.y.z = c("staid", "x", "y", "z"),
  n.obs = 7,
  soil3d = FALSE,
  # ¡NO incluir s.crs o p.crs!
  cpus = n_cores,
  progress = TRUE,
  importance = "impurity",
  seed = 42,
  num.trees = 300,
  mtry = 5,
  splitrule = "variance",
  min.node.size = 5,
  sample.fraction = 0.8,
  quantreg = TRUE
)

# ==================== 6. VER RESULTADOS ====================
cat("\n=== RESULTADOS DEL MODELO ===\n")
print(rfsi_model)

# Importancia de variables
cat("\n=== IMPORTANCIA DE VARIABLES ===\n")
var_importance <- data.frame(
  Variable = names(rfsi_model$variable.importance),
  Importance = rfsi_model$variable.importance
) %>%
  arrange(desc(Importance))

print(var_importance)

# ==================== 7. PREDECIR Y EVALUAR (CORREGIDO) ====================
cat("\n=== PREDICCIÓN Y EVALUACIÓN ===\n")

# PREDICCIÓN CORRECTA PARA RFSI
# Necesitas usar pred.rfsi() que recalcula los vecinos

# Opción A: Predecir en los mismos datos de entrenamiento
rfsi_predictions <- pred.rfsi(
  model = rfsi_model,           # Modelo entrenado
  data = data_df,               # Datos originales
  obs.col = "prcp",             # Columna con observaciones
  data.staid.x.y.z = c("staid", "x", "y", "z"),  # Columnas ID, coord, tiempo
  newdata = data_df,            # Predecir en mismos puntos
  newdata.staid.x.y.z = c("staid", "x", "y", "z"),  # Columnas para nuevos datos
  output.format = "data.frame",  # Formato de salida
  cpus = n_cores,
  progress = TRUE,
  soil3d = FALSE
)

# Las predicciones están en el resultado
data_df$pred <- rfsi_predictions$pred

saveRDS(rfsi_model, "D:/S/Serbia1km/Interpolation/rfsi_modelo_final.rds")
saveRDS(rfsi_predictions, "predicciones.rds")


# ==================== 8. CARGAR RASTERS DIARIOS Y PREDECIR ====================
# ==================== FUNCIÓN SIMPLIFICADA PARA UN DÍA ====================
predecir_dia_simple <- function(fecha, indice_dia) {
  
  cat(format(fecha, "%Y-%m-%d"), "... ")
  
  tryCatch({
    # 1. Cargar capas climáticas para este día
    precland_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/precland_2000_2003.tif", 
                                lyrs = indice_dia)
    tmax_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/tmax_2000_2003.tif", 
                            lyrs = indice_dia)
    tmin_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/tmin_2000_2003.tif", 
                            lyrs = indice_dia)
    
    # 2. Crear dataframe con todas las variables
    newdata_df <- as.data.frame(raster_base, xy = TRUE, na.rm = FALSE)
    newdata_df$staid <- 1:nrow(newdata_df)
    
    # Extraer valores climáticos
    newdata_df$precland <- terra::extract(precland_dia, newdata_df[, c("x", "y")])[,2]
    newdata_df$tmax <- terra::extract(tmax_dia, newdata_df[, c("x", "y")])[,2]
    newdata_df$tmin <- terra::extract(tmin_dia, newdata_df[, c("x", "y")])[,2]
    
    # Variables temporales
    newdata_df$z <- as.numeric(fecha - min(data_df$date))
    newdata_df$doy_sin <- sin(2 * pi * yday(fecha) / 365.25)
    newdata_df$doy_cos <- cos(2 * pi * yday(fecha) / 365.25)
    newdata_df$month <- month(fecha)
    
    # 3. Eliminar NAs
    vars_needed <- c("staid", "x", "y", "z", "doy_sin", "doy_cos", "month",
                     "dem", "slope", "twi", "sin", "cos", "precland", "tmax", "tmin")
    
    newdata_clean <- na.omit(newdata_df[, vars_needed])
    
    if(nrow(newdata_clean) == 0) {
      cat("Sin datos\n")
      return(NULL)
    }
    
    # 4. Predecir (Muestra aleatoria si hay muchos puntos)
    if(nrow(newdata_clean) > 10000) {
      newdata_clean <- newdata_clean[sample(1:nrow(newdata_clean), 10000), ]
    }
    
    # 5. Ejecutar pred.rfsi
    prediction <- pred.rfsi(
      model = rfsi_model,
      data = data_df,
      obs.col = "prcp",
      data.staid.x.y.z = c("staid", "x", "y", "z"),
      newdata = newdata_clean,
      newdata.staid.x.y.z = c("staid", "x", "y", "z"),
      output.format = "data.frame",
      cpus = 1,
      progress = FALSE,
      soil3d = FALSE
    )
    
    # 6. Crear raster
    pred_raster <- terra::rast(raster_base[[1]])
    cells <- terra::cellFromXY(pred_raster, as.matrix(newdata_clean[, c("x", "y")]))
    pred_raster[cells] <- prediction$pred
    
    names(pred_raster) <- paste0("prcp_", format(fecha, "%Y%m%d"))
    
    # 7. Guardar
    terra::writeRaster(
      pred_raster,
      paste0("D:/S/Serbia1km/Interpolation/Huallaga/prcp_", 
             format(fecha, "%Y%m%d"), ".tif"),
      overwrite = TRUE
    )
    
    cat("OK\n")
    return(pred_raster)
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

# ==================== EJECUTAR PARA MÚLTIPLES DÍAS ====================
cat("\n=== PROCESANDO DICIEMBRE 2000 ===\n")

# Definir fechas
fechas <- seq.Date(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")

# Calcular índices (asumiendo que el raster empieza el 2000-01-01)
fecha_inicio_raster <- as.Date("2000-01-01")
indices <- as.numeric(fechas - fecha_inicio_raster) + 1

# Crear raster_base si no existe
if(!exists("raster_base")) {
  # Crear uno simple
  template <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/precland_2000_2003.tif", lyrs = 1)
  
  raster_base <- terra::rast(list(
    dem = terra::setValues(template, 500),
    slope = terra::setValues(template, 5),
    twi = terra::setValues(template, 10),
    sin = terra::setValues(template, 0),
    cos = terra::setValues(template, 0)
  ))
}

# Procesar cada día
resultados <- list()

for(i in seq_along(fechas)) {
  resultados[[as.character(fechas[i])]] <- predecir_dia_simple(fechas[i], indices[i])
}

# Crear stack final
cat("\nCreando stack final...\n")
resultados_validos <- resultados[!sapply(resultados, is.null)]

if(length(resultados_validos) > 0) {
  stack_final <- terra::rast(resultados_validos)
  terra::writeRaster(stack_final, 
                     "D:/QSWAT/Output_RFSI/prcp_año_2000.tif",
                     overwrite = TRUE)
  cat("✓ Stack final guardado\n")
}

cat("\n=== FIN DEL PROCESO ===\n")

library(raster)

# Cargar el raster multicapas de un año
r <- stack("D:/S/Serbia1km/Interpolation/Huallaga/prcp_diciembre_2000.tif")

plot(r)


# ==================== 0. INSTALAR/CARGAR PAQUETES ====================
library(sf)
library(dplyr)
library(meteo)
library(ranger)
library(terra)
library(ggplot2)
library(lubridate)
library(CAST)
library(doParallel)    # AGREGADO
library(foreach)       # AGREGADO

# ==================== 1. CARGAR Y PREPARAR DATOS ====================
# Cargar datos
data <- read.csv("D:/S/Serbia1km/Interpolation/stfdf_pt_huallaga.csv")

# Convertir y crear variables temporales
data <- data %>%
  mutate(
    time = as.Date(time, format = "%m/%d/%Y"),
    date = time,
    doy = yday(date),
    month = month(date),
    year = year(date),
    staid = sp.ID,
    obs = prcp
  ) %>%
  filter(
    !is.na(prcp), !is.na(precland), !is.na(tmax), !is.na(tmin),
    !is.na(lon), !is.na(lat), !is.na(dem)
  )

# ==================== 2. CONVERTIR A UTM ====================
# Convertir a sf
data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
data_sf <- st_transform(data_sf, crs = 32718)

# Extraer coordenadas UTM
coords_utm <- st_coordinates(data_sf)
data_sf$x <- coords_utm[, "X"]
data_sf$y <- coords_utm[, "Y"]

# ==================== 3. PREPARAR DATAFRAME PARA RFSI ====================
data_df <- as.data.frame(data_sf)

# Crear z (tiempo en días desde primera fecha)
data_df$z <- as.numeric(data_df$date - min(data_df$date))

# Efectos estacionales cíclicos
data_df$doy_sin <- sin(2 * pi * data_df$doy / 365.25)
data_df$doy_cos <- cos(2 * pi * data_df$doy / 365.25)

# ==================== 4. DEFINIR FÓRMULA ====================
fm.RFSI <- as.formula("prcp ~ precland + tmax + tmin + dem + twi + slope + 
                      sin + cos + doy_sin + doy_cos + month")

# ==================== 5. ENTRENAR MODELO RFSI ====================
cat("=== ENTRENANDO MODELO RFSI ESPACIO-TEMPORAL ===\n")

n_cores <- max(1, detectCores() - 1)
cat("Usando", n_cores, "núcleos de CPU\n")

set.seed(42)

rfsi_model <- rfsi(
  formula = fm.RFSI,
  data = data_df,
  data.staid.x.y.z = c("staid", "x", "y", "z"),
  n.obs = 7,
  soil3d = FALSE,
  cpus = n_cores,
  progress = TRUE,
  importance = "impurity",
  seed = 42,
  num.trees = 300,
  mtry = 5,
  splitrule = "variance",
  min.node.size = 5,
  sample.fraction = 0.8,
  quantreg = TRUE
)

# ==================== 6. VER RESULTADOS ====================
cat("\n=== RESULTADOS DEL MODELO ===\n")
print(rfsi_model)

# Importancia de variables
cat("\n=== IMPORTANCIA DE VARIABLES ===\n")
var_importance <- data.frame(
  Variable = names(rfsi_model$variable.importance),
  Importance = rfsi_model$variable.importance
) %>%
  arrange(desc(Importance))

print(var_importance)

# ==================== 7. PREDECIR Y EVALUAR ====================
cat("\n=== PREDICCIÓN Y EVALUACIÓN ===\n")

rfsi_predictions <- pred.rfsi(
  model = rfsi_model,
  data = data_df,
  obs.col = "prcp",
  data.staid.x.y.z = c("staid", "x", "y", "z"),
  newdata = data_df,
  newdata.staid.x.y.z = c("staid", "x", "y", "z"),
  output.format = "data.frame",
  cpus = n_cores,
  progress = TRUE,
  soil3d = FALSE
)

data_df$pred <- rfsi_predictions$pred

saveRDS(rfsi_model, "D:/S/Serbia1km/Interpolation/rfsi_modelo_final.rds")
saveRDS(rfsi_predictions, "predicciones.rds")

# ==================== 8. CONFIGURAR RASTER BASE ====================
cat("\n=== CONFIGURANDO RASTER BASE ===\n")

template <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/precland_2000_2003.tif", lyrs = 1)

raster_base <- terra::rast(list(
  dem = terra::setValues(template, 500),
  slope = terra::setValues(template, 5),
  twi = terra::setValues(template, 10),
  sin = terra::setValues(template, 0),
  cos = terra::setValues(template, 0)
))

cat("Raster base creado\n")

# ==================== 9. FUNCIÓN PARA PREDECIR UN DÍA (VERSIÓN SIMPLIFICADA) ====================
predecir_dia_simple <- function(fecha, indice_dia, rfsi_model, data_df, raster_base) {
  
  nombre_archivo <- paste0("D:/S/Serbia1km/Interpolation/Huallaga/prcp_", 
                           format(fecha, "%Y%m%d"), ".tif")
  
  # Si ya existe, no procesar
  if(file.exists(nombre_archivo)) {
    cat(format(fecha, "%Y-%m-%d"), "Ya existe\n")
    return(nombre_archivo)
  }
  
  tryCatch({
    cat(format(fecha, "%Y-%m-%d"), "... ")
    
    # 1. Cargar capas climáticas (rutas completas)
    precland_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/precland_2000_2003.tif", 
                                lyrs = indice_dia)
    tmax_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/tmax_2000_2003.tif", 
                            lyrs = indice_dia)
    tmin_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/tmin_2000_2003.tif", 
                            lyrs = indice_dia)
    
    # 2. Crear dataframe
    newdata_df <- as.data.frame(raster_base, xy = TRUE, na.rm = FALSE)
    newdata_df$staid <- 1:nrow(newdata_df)
    
    # Extraer valores
    newdata_df$precland <- terra::extract(precland_dia, newdata_df[, c("x", "y")])[,2]
    newdata_df$tmax <- terra::extract(tmax_dia, newdata_df[, c("x", "y")])[,2]
    newdata_df$tmin <- terra::extract(tmin_dia, newdata_df[, c("x", "y")])[,2]
    
    # Variables temporales
    newdata_df$z <- as.numeric(fecha - min(data_df$date))
    newdata_df$doy_sin <- sin(2 * pi * yday(fecha) / 365.25)
    newdata_df$doy_cos <- cos(2 * pi * yday(fecha) / 365.25)
    newdata_df$month <- month(fecha)
    
    # 3. Eliminar NAs
    vars_needed <- c("staid", "x", "y", "z", "doy_sin", "doy_cos", "month",
                     "dem", "slope", "twi", "sin", "cos", "precland", "tmax", "tmin")
    
    newdata_clean <- na.omit(newdata_df[, vars_needed])
    
    if(nrow(newdata_clean) == 0) {
      cat("Sin datos\n")
      return(NULL)
    }
    
    # 4. Muestra aleatoria si hay muchos puntos
    if(nrow(newdata_clean) > 10000) {
      newdata_clean <- newdata_clean[sample(1:nrow(newdata_clean), 10000), ]
    }
    
    # 5. Predecir
    prediction <- pred.rfsi(
      model = rfsi_model,
      data = data_df,
      obs.col = "prcp",
      data.staid.x.y.z = c("staid", "x", "y", "z"),
      newdata = newdata_clean,
      newdata.staid.x.y.z = c("staid", "x", "y", "z"),
      output.format = "data.frame",
      cpus = 1,
      progress = FALSE,
      soil3d = FALSE
    )
    
    # 6. Crear raster
    pred_raster <- terra::rast(raster_base[[1]])
    cells <- terra::cellFromXY(pred_raster, as.matrix(newdata_clean[, c("x", "y")]))
    pred_raster[cells] <- prediction$pred
    
    names(pred_raster) <- paste0("prcp_", format(fecha, "%Y%m%d"))
    
    # 7. Guardar
    terra::writeRaster(
      pred_raster,
      nombre_archivo,
      overwrite = TRUE
    )
    
    cat("OK\n")
    return(nombre_archivo)
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

# ==================== 10. EJECUTAR CON PARALELISMO (VERSIÓN FUNCIONAL) ====================
cat("\n=== PROCESANDO AÑO 2000 ===\n")

# Definir fechas
fechas <- seq.Date(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")

# Calcular índices
fecha_inicio_raster <- as.Date("2000-01-01")
indices <- as.numeric(fechas - fecha_inicio_raster) + 1

# PRIMERO: Crear raster_base (debe existir antes de paralelizar)
if(!exists("raster_base")) {
  template <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/precland_2000_2003.tif", lyrs = 1)
  
  raster_base <- terra::rast(list(
    dem = terra::setValues(template, 500),
    slope = terra::setValues(template, 5),
    twi = terra::setValues(template, 10),
    sin = terra::setValues(template, 0),
    cos = terra::setValues(template, 0)
  ))
}

# Guardar raster_base como archivo temporal para que los workers puedan cargarlo
raster_base_file <- "D:/S/Serbia1km/Interpolation/Huallaga/raster_base_temp.tif"
terra::writeRaster(raster_base, raster_base_file, overwrite = TRUE)

# Configurar paralelismo
n_workers <- min(detectCores() - 1, 4)
cat("Usando", n_workers, "workers paralelos\n")

# Crear cluster
cl <- makeCluster(n_workers)
registerDoParallel(cl)

# Exportar TODO lo necesario
clusterExport(cl, varlist = c("predecir_dia_simple", "rfsi_model", "data_df", 
                              "raster_base_file", "fechas", "indices",
                              "fm.RFSI"),
              envir = environment())

# Cargar paquetes y datos en workers
clusterEvalQ(cl, {
  # Cargar paquetes
  library(terra)
  library(CAST)
  library(lubridate)
  
  # Cargar modelo RFSI
  rfsi_model <- readRDS("D:/S/Serbia1km/Interpolation/rfsi_modelo_final.rds")
  
  # Cargar raster_base desde archivo
  raster_base <- terra::rast(raster_base_file)
  
  # Cargar data_df (simplificado - solo lo necesario)
  # En lugar de pasar todo data_df, pasamos un resumen o lo recreamos
  NULL
})

# Procesar en paralelo (solo primeros 10 días para prueba)
cat("\nProcesando (solo primeros 366 días para prueba)...\n")
n_dias_prueba <- min(366, length(fechas))

resultados <- foreach(i = 1:n_dias_prueba, 
                      .combine = c,
                      .errorhandling = "pass",
                      .packages = c("terra", "CAST", "lubridate")) %dopar% {
                        
                        # Dentro del worker, necesitamos recrear data_df mínima
                        # Para simplificar, usamos solo las columnas esenciales
                        fecha_actual <- fechas[i]
                        indice_actual <- indices[i]
                        
                        # Llamar a la función
                        archivo <- tryCatch({
                          predecir_dia_simple(fecha_actual, indice_actual, rfsi_model, data_df, raster_base)
                        }, error = function(e) {
                          cat("Error en worker para fecha", as.character(fecha_actual), ":", e$message, "\n")
                          return(NULL)
                        })
                        
                        return(archivo)
                      }

# Detener cluster
stopCluster(cl)

# Filtrar resultados
archivos_creados <- resultados[!sapply(resultados, is.null)]
cat("\nArchivos creados exitosamente:", length(archivos_creados), "de", n_dias_prueba, "\n")

# ==================== 11. CREAR STACK FINAL (SI HAY RESULTADOS) ====================
if(length(archivos_creados) > 0) {
  cat("\nCreando stack final...\n")
  
  # Cargar todos los rasters creados
  rasters_list <- list()
  for(archivo in archivos_creados) {
    if(file.exists(archivo)) {
      rast <- terra::rast(archivo)
      rasters_list[[basename(archivo)]] <- rast
    }
  }
  
  if(length(rasters_list) > 0) {
    # Crear stack
    stack_final <- terra::rast(rasters_list)
    
    # Guardar stack
    terra::writeRaster(stack_final, 
                       "D:/QSWAT/Output_RFSI/prcp_prueba1_2000.tif",
                       overwrite = TRUE)
    cat("Stack de prueba guardado: prcp_prueba1_2000.tif\n")
    
    # Mostrar estadísticas
    cat("\nEstadísticas de los días procesados:\n")
    for(i in seq_along(rasters_list)) {
      stats <- terra::global(rasters_list[[i]], c("mean", "min", "max"), na.rm = TRUE)
      cat(names(rasters_list)[i], ": ", 
          "Media=", round(stats$mean, 2), "mm, ",
          "Min=", round(stats$min, 2), "mm, ",
          "Max=", round(stats$max, 2), "mm\n", sep = "")
    }
  }
}

library(raster)

# Cargar el raster multicapas de un año
r <- stack("D:/QSWAT/Output_RFSI/prcp_prueba1_2000.tif")

plot(r)



##############################################################################################

# ==================== 10. EJECUTAR CON PARALELISMO (VERSIÓN CORREGIDA) ====================
cat("\n=== PROCESANDO AÑO 2000 ===\n")

# Definir fechas (366 días para 2000)
fechas <- seq.Date(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
cat("Total de días:", length(fechas), "\n")

# Calcular índices
fecha_inicio_raster <- as.Date("2000-01-01")
indices <- as.numeric(fechas - fecha_inicio_raster) + 1

# Crear y guardar una versión ligera de data_df para los workers
cat("Preparando datos para workers...\n")
data_df_light <- data_df[, c("staid", "x", "y", "z", "prcp", "precland", 
                             "tmax", "tmin", "dem", "slope", "twi", 
                             "sin", "cos", "doy_sin", "doy_cos", "month")]
saveRDS(data_df_light, "D:/S/Serbia1km/Interpolation/data_df_light.rds")

# También guardar el modelo (por si acaso)
saveRDS(rfsi_model, "D:/S/Serbia1km/Interpolation/rfsi_model.rds")

# Configurar paralelismo
n_workers <- min(detectCores() - 1, 4)
cat("Usando", n_workers, "workers paralelos\n")

# Crear cluster
cl <- makeCluster(n_workers)
registerDoParallel(cl)

# Procesar TODOS los 366 días en paralelo
cat("\nProcesando 366 días...\n")

resultados <- foreach(i = 1:366, 
                      .combine = c,
                      .errorhandling = "pass",
                      .packages = c("terra", "CAST", "lubridate", "ranger")) %dopar% {
                        
                        tryCatch({
                          # Cargar datos dentro del worker
                          data_df_worker <- readRDS("D:/S/Serbia1km/Interpolation/data_df_light.rds")
                          rfsi_model_worker <- readRDS("D:/S/Serbia1km/Interpolation/rfsi_model.rds")
                          
                          # Cargar raster base
                          raster_base_worker <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/raster_base_temp.tif")
                          
                          # Fecha actual
                          fecha_actual <- as.Date("2000-01-01") + (i - 1)
                          indice_actual <- i
                          
                          # Nombre del archivo
                          nombre_archivo <- paste0("D:/S/Serbia1km/Interpolation/Huallaga/prcp_", 
                                                   format(fecha_actual, "%Y%m%d"), ".tif")
                          
                          # Si ya existe, no procesar
                          if(file.exists(nombre_archivo)) {
                            return(nombre_archivo)
                          }
                          
                          # Cargar capas climáticas
                          precland_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/precland_2000_2003.tif", 
                                                      lyrs = indice_actual)
                          tmax_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/tmax_2000_2003.tif", 
                                                  lyrs = indice_actual)
                          tmin_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/tmin_2000_2003.tif", 
                                                  lyrs = indice_actual)
                          
                          # Crear dataframe
                          newdata_df <- as.data.frame(raster_base_worker, xy = TRUE, na.rm = FALSE)
                          newdata_df$staid <- 1:nrow(newdata_df)
                          
                          # Extraer valores
                          newdata_df$precland <- terra::extract(precland_dia, newdata_df[, c("x", "y")])[,2]
                          newdata_df$tmax <- terra::extract(tmax_dia, newdata_df[, c("x", "y")])[,2]
                          newdata_df$tmin <- terra::extract(tmin_dia, newdata_df[, c("x", "y")])[,2]
                          
                          # Variables temporales
                          newdata_df$z <- as.numeric(fecha_actual - min(data_df_worker$date))
                          newdata_df$doy_sin <- sin(2 * pi * yday(fecha_actual) / 365.25)
                          newdata_df$doy_cos <- cos(2 * pi * yday(fecha_actual) / 365.25)
                          newdata_df$month <- month(fecha_actual)
                          
                          # Eliminar NAs
                          vars_needed <- c("staid", "x", "y", "z", "doy_sin", "doy_cos", "month",
                                           "dem", "slope", "twi", "sin", "cos", "precland", "tmax", "tmin")
                          
                          newdata_clean <- na.omit(newdata_df[, vars_needed])
                          
                          if(nrow(newdata_clean) == 0) return(NULL)
                          
                          # Muestra aleatoria si hay muchos puntos
                          if(nrow(newdata_clean) > 10000) {
                            set.seed(42)
                            newdata_clean <- newdata_clean[sample(1:nrow(newdata_clean), 10000), ]
                          }
                          
                          # Predecir
                          prediction <- pred.rfsi(
                            model = rfsi_model_worker,
                            data = data_df_worker,
                            obs.col = "prcp",
                            data.staid.x.y.z = c("staid", "x", "y", "z"),
                            newdata = newdata_clean,
                            newdata.staid.x.y.z = c("staid", "x", "y", "z"),
                            output.format = "data.frame",
                            cpus = 1,
                            progress = FALSE,
                            soil3d = FALSE
                          )
                          
                          # Crear raster
                          pred_raster <- terra::rast(raster_base_worker[[1]])
                          cells <- terra::cellFromXY(pred_raster, as.matrix(newdata_clean[, c("x", "y")]))
                          pred_raster[cells] <- prediction$pred
                          
                          names(pred_raster) <- paste0("prcp_", format(fecha_actual, "%Y%m%d"))
                          
                          # Guardar
                          terra::writeRaster(pred_raster, nombre_archivo, overwrite = TRUE)
                          
                          return(nombre_archivo)
                          
                        }, error = function(e) {
                          return(NULL)
                        })
                      }

# Detener cluster
stopCluster(cl)

# Filtrar resultados
archivos_creados <- resultados[!sapply(resultados, is.null)]
cat("\nArchivos creados exitosamente:", length(archivos_creados), "de 366\n")

# ==================== 11. CREAR STACK FINAL ====================
if(length(archivos_creados) > 0) {
  cat("\nCreando stack anual de 366 días...\n")
  
  # Cargar todos los rasters
  rasters_list <- list()
  
  for(archivo in archivos_creados) {
    if(file.exists(archivo)) {
      rast <- terra::rast(archivo)
      rasters_list[[basename(archivo)]] <- rast
    }
  }
  
  if(length(rasters_list) > 0) {
    # Crear stack
    stack_final <- terra::rast(rasters_list)
    
    # Guardar stack
    terra::writeRaster(stack_final, 
                       "D:/QSWAT/Output_RFSI/prcp_2000_completo.tif",
                       overwrite = TRUE)
    cat("Stack anual guardado: prcp_2000_completo.tif\n")
    cat("Número de capas:", terra::nlyr(stack_final), "\n")
  }
}

# ==================== 12. VERSIÓN SIMPLE PARA MÚLTIPLES AÑOS ====================
cat("\n=== PROCESANDO MÚLTIPLES AÑOS (2000-2003) ===\n")

# Definir años
anios <- 2000:2001

for(anio in anios) {
  cat("\n======================================\n")
  cat("PROCESANDO AÑO", anio, "\n")
  cat("======================================\n")
  
  # Determinar si es año bisiesto
  es_bisiesto <- (anio %% 400 == 0) || (anio %% 4 == 0 && anio %% 100 != 0)
  dias_anio <- ifelse(es_bisiesto, 366, 365)
  
  cat("Días en el año:", dias_anio, "\n")
  
  # Procesar cada día del año
  archivos_creados_anio <- list()
  
  for(dia in 1:dias_anio) {
    fecha_actual <- as.Date(paste0(anio, "-01-01")) + (dia - 1)
    indice_actual <- as.numeric(fecha_actual - as.Date("2000-01-01")) + 1
    
    cat(format(fecha_actual, "%Y-%m-%d"), "... ")
    
    # Usar la función original (sin paralelismo para simplicidad)
    archivo <- predecir_dia_simple(fecha_actual, indice_actual, rfsi_model, data_df, raster_base)
    
    if(!is.null(archivo)) {
      archivos_creados_anio[[dia]] <- archivo
    }
    
    # Mostrar progreso cada 30 días
    if(dia %% 30 == 0) {
      cat("Progreso:", dia, "/", dias_anio, "\n")
    }
  }
  
  # Crear stack anual
  archivos_validos <- archivos_creados_anio[!sapply(archivos_creados_anio, is.null)]
  cat("\nArchivos creados para", anio, ":", length(archivos_validos), "\n")
  
  if(length(archivos_validos) > 0) {
    cat("Creando stack anual...\n")
    
    rasters_list <- list()
    for(archivo in archivos_validos) {
      if(file.exists(archivo)) {
        rast <- terra::rast(archivo)
        rasters_list[[basename(archivo)]] <- rast
      }
    }
    
    if(length(rasters_list) > 0) {
      stack_anual <- terra::rast(rasters_list)
      
      terra::writeRaster(stack_anual,
                         paste0("D:/QSWAT/Output_RFSI/prcp_", anio, ".tif"),
                         overwrite = TRUE)
      
      cat("Stack anual guardado: prcp_", anio, ".tif\n", sep = "")
    }
  }
}

cat("\n=== PROCESO COMPLETADO ===\n")


##########################################################################################
#######################################################################################

# ==================== 8. PROCESAR MÚLTIPLES AÑOS CON PARALELISMO ====================
cat("\n=== PROCESANDO AÑOS 2000-2003 CON PARALELISMO ===\n")

# Definir años
anios <- 2000:2003
cat("Años a procesar:", paste(anios, collapse = ", "), "\n")

# Crear y guardar una versión ligera de data_df para los workers
cat("Preparando datos para workers...\n")
data_df_light <- data_df[, c("staid", "x", "y", "z", "prcp", "precland", 
                             "tmax", "tmin", "dem", "slope", "twi", 
                             "sin", "cos", "doy_sin", "doy_cos", "month")]
saveRDS(data_df_light, "D:/S/Serbia1km/Interpolation/data_df_light.rds")

# También guardar el modelo
saveRDS(rfsi_model, "D:/S/Serbia1km/Interpolation/rfsi_model.rds")

# Guardar raster_base como archivo temporal
raster_base_file <- "D:/S/Serbia1km/Interpolation/Huallaga/raster_base_temp.tif"
terra::writeRaster(raster_base, raster_base_file, overwrite = TRUE)

# Procesar cada año
for(anio in anios) {
  
  cat("\n")
  cat("======================================\n")
  cat("PROCESANDO AÑO", anio, "CON PARALELISMO\n")
  cat("======================================\n")
  
  # Definir fechas del año
  fecha_inicio <- as.Date(paste0(anio, "-01-01"))
  fecha_fin <- as.Date(paste0(anio, "-12-31"))
  
  # Ajustar a datos disponibles
  fecha_min_data <- min(data_df$date)
  fecha_max_data <- max(data_df$date)
  
  if(fecha_inicio < fecha_min_data) fecha_inicio <- fecha_min_data
  if(fecha_fin > fecha_max_data) fecha_fin <- fecha_max_data
  
  fechas <- seq.Date(fecha_inicio, fecha_fin, by = "day")
  total_dias <- length(fechas)
  cat("Días a procesar:", total_dias, "\n")
  
  # Calcular índices
  fecha_inicio_raster <- as.Date("2000-01-01")
  indices <- as.numeric(fechas - fecha_inicio_raster) + 1
  
  # Configurar paralelismo
  n_workers <- min(detectCores() - 1, 4)
  cat("Usando", n_workers, "workers paralelos\n")
  
  # Crear cluster
  cl <- makeCluster(n_workers)
  registerDoParallel(cl)
  
  # Procesar TODOS los días del año en paralelo
  cat("Procesando días...\n")
  
  resultados <- foreach(i = 1:total_dias, 
                        .combine = c,
                        .errorhandling = "pass",
                        .packages = c("terra", "CAST", "lubridate", "ranger")) %dopar% {
                          
                          tryCatch({
                            # Cargar datos dentro del worker
                            data_df_worker <- readRDS("D:/S/Serbia1km/Interpolation/data_df_light.rds")
                            rfsi_model_worker <- readRDS("D:/S/Serbia1km/Interpolation/rfsi_model.rds")
                            
                            # Cargar raster base
                            raster_base_worker <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/raster_base_temp.tif")
                            
                            # Fecha actual
                            fecha_actual <- as.Date(paste0(anio, "-01-01")) + (i - 1)
                            indice_actual <- as.numeric(fecha_actual - as.Date("2000-01-01")) + 1
                            
                            # Nombre del archivo
                            nombre_archivo <- paste0("D:/S/Serbia1km/Interpolation/Huallaga/prcp_", 
                                                     format(fecha_actual, "%Y%m%d"), ".tif")
                            
                            # Si ya existe, no procesar
                            if(file.exists(nombre_archivo)) {
                              return(nombre_archivo)
                            }
                            
                            # Cargar capas climáticas
                            precland_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/precland_2000_2003.tif", 
                                                        lyrs = indice_actual)
                            tmax_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/tmax_2000_2003.tif", 
                                                    lyrs = indice_actual)
                            tmin_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/tmin_2000_2003.tif", 
                                                    lyrs = indice_actual)
                            
                            # Crear dataframe
                            newdata_df <- as.data.frame(raster_base_worker, xy = TRUE, na.rm = FALSE)
                            newdata_df$staid <- 1:nrow(newdata_df)
                            
                            # Extraer valores
                            newdata_df$precland <- terra::extract(precland_dia, newdata_df[, c("x", "y")])[,2]
                            newdata_df$tmax <- terra::extract(tmax_dia, newdata_df[, c("x", "y")])[,2]
                            newdata_df$tmin <- terra::extract(tmin_dia, newdata_df[, c("x", "y")])[,2]
                            
                            # Variables temporales
                            newdata_df$z <- as.numeric(fecha_actual - min(data_df_worker$date))
                            newdata_df$doy_sin <- sin(2 * pi * yday(fecha_actual) / 365.25)
                            newdata_df$doy_cos <- cos(2 * pi * yday(fecha_actual) / 365.25)
                            newdata_df$month <- month(fecha_actual)
                            
                            # Eliminar NAs
                            vars_needed <- c("staid", "x", "y", "z", "doy_sin", "doy_cos", "month",
                                             "dem", "slope", "twi", "sin", "cos", "precland", "tmax", "tmin")
                            
                            newdata_clean <- na.omit(newdata_df[, vars_needed])
                            
                            if(nrow(newdata_clean) == 0) return(NULL)
                            
                            # Muestra aleatoria si hay muchos puntos
                            if(nrow(newdata_clean) > 10000) {
                              set.seed(42)
                              newdata_clean <- newdata_clean[sample(1:nrow(newdata_clean), 10000), ]
                            }
                            
                            # Predecir
                            prediction <- pred.rfsi(
                              model = rfsi_model_worker,
                              data = data_df_worker,
                              obs.col = "prcp",
                              data.staid.x.y.z = c("staid", "x", "y", "z"),
                              newdata = newdata_clean,
                              newdata.staid.x.y.z = c("staid", "x", "y", "z"),
                              output.format = "data.frame",
                              cpus = 1,
                              progress = FALSE,
                              soil3d = FALSE
                            )
                            
                            # Crear raster
                            pred_raster <- terra::rast(raster_base_worker[[1]])
                            cells <- terra::cellFromXY(pred_raster, as.matrix(newdata_clean[, c("x", "y")]))
                            pred_raster[cells] <- prediction$pred
                            
                            names(pred_raster) <- paste0("prcp_", format(fecha_actual, "%Y%m%d"))
                            
                            # Guardar
                            terra::writeRaster(pred_raster, nombre_archivo, overwrite = TRUE)
                            
                            return(nombre_archivo)
                            
                          }, error = function(e) {
                            return(NULL)
                          })
                        }
  
  # Detener cluster
  stopCluster(cl)
  
  # Filtrar resultados
  archivos_creados <- resultados[!sapply(resultados, is.null)]
  cat("\nArchivos creados exitosamente:", length(archivos_creados), "de", total_dias, "\n")
  
  # Crear stack anual
  if(length(archivos_creados) > 0) {
    cat("Creando stack anual...\n")
    
    rasters_list <- list()
    for(archivo in archivos_creados) {
      if(file.exists(archivo)) {
        rast <- terra::rast(archivo)
        rasters_list[[basename(archivo)]] <- rast
      }
    }
    
    if(length(rasters_list) > 0) {
      stack_anual <- terra::rast(rasters_list)
      
      terra::writeRaster(stack_anual,
                         paste0("D:/QSWAT/Output_RFSI/prcp_", anio, "_paralelo.tif"),
                         overwrite = TRUE)
      
      cat("Stack anual guardado: prcp_", anio, "_paralelo.tif\n", sep = "")
      cat("Número de capas:", terra::nlyr(stack_anual), "\n")
    }
  }
  
  cat("Año", anio, "completado\n")
}

cat("\n=== PROCESO COMPLETADO ===\n")
cat("Archivos diarios: D:/S/Serbia1km/Interpolation/Huallaga/prcp_YYYYMMDD.tif\n")
cat("Stacks anuales: D:/QSWAT/Output_RFSI/prcp_YYYY_paralelo.tif\n")



#######################################################################################
################### para multiples años
######################################################################################
# ==================== 8. PROCESAR MÚLTIPLES AÑOS CON PARALELISMO ====================
cat("\n=== PROCESANDO AÑOS 2000-2019 CON PARALELISMO ===\n")

# Definir años (2000-2019 = 20 años)
anios <- 2000:2019
cat("Años a procesar:", length(anios), "años (", min(anios), "-", max(anios), ")\n", sep = "")

# Verificar que tenemos rasters
cat("\nVerificando archivos raster...\n")
rasters_base <- c(
  "precland_2000_2019.tif",
  "tmax_2000_2019.tif",
  "tmin_2000_2019.tif"
)

for(raster_file in rasters_base) {
  ruta_completa <- paste0("D:/S/Serbia1km/Interpolation/Huallaga/", raster_file)
  if(file.exists(ruta_completa)) {
    # Verificar número de capas
    raster_info <- terra::rast(ruta_completa)
    n_capas <- terra::nlyr(raster_info)
    cat("✓", raster_file, "encontrado (", n_capas, "capas)\n", sep = "")
  } else {
    cat("✗ ERROR:", raster_file, "NO encontrado\n")
    stop("Faltan archivos raster necesarios")
  }
}

# Verificar que tenemos suficientes capas (20 años * 365/366 días)
# Mínimo: 20*365 = 7300 capas, máximo: 20*366 = 7320 capas
precland_rast <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/precland_2000_2019.tif")
n_capas_total <- terra::nlyr(precland_rast)
cat("Capas en raster:", n_capas_total, "\n")

# Calcular días totales 2000-2019
dias_totales <- sum(sapply(anios, function(a) ifelse(a %% 400 == 0 || (a %% 4 == 0 && a %% 100 != 0), 366, 365)))
cat("Días totales 2000-2019:", dias_totales, "\n")

if(n_capas_total < dias_totales) {
  cat("⚠ ADVERTENCIA: Raster tiene menos capas que días totales\n")
}

# Crear y guardar una versión ligera de data_df para los workers
cat("\nPreparando datos para workers...\n")
data_df_light <- data_df[, c("staid", "x", "y", "z", "prcp", "precland", 
                             "tmax", "tmin", "dem", "slope", "twi", 
                             "sin", "cos", "doy_sin", "doy_cos", "month")]
saveRDS(data_df_light, "D:/S/Serbia1km/Interpolation/data_df_light.rds")

# También guardar el modelo
saveRDS(rfsi_model, "D:/S/Serbia1km/Interpolation/rfsi_model.rds")

# Guardar raster_base como archivo temporal
raster_base_file <- "D:/S/Serbia1km/Interpolation/Huallaga/raster_base_temp.tif"
terra::writeRaster(raster_base, raster_base_file, overwrite = TRUE)

# Procesar cada año
for(anio in anios) {
  
  cat("\n")
  cat("===============================================\n")
  cat("PROCESANDO AÑO", anio, "(", which(anios == anio), "/", length(anios), ")\n")
  cat("===============================================\n")
  
  # Determinar si es año bisiesto
  es_bisiesto <- (anio %% 400 == 0) || (anio %% 4 == 0 && anio %% 100 != 0)
  dias_anio <- ifelse(es_bisiesto, 366, 365)
  cat("Días en el año:", dias_anio, "\n")
  
  # Definir fechas del año
  fecha_inicio <- as.Date(paste0(anio, "-01-01"))
  fecha_fin <- as.Date(paste0(anio, "-12-31"))
  
  # Verificar que el año esté dentro del rango de datos
  fecha_min_data <- min(data_df$date)
  fecha_max_data <- max(data_df$date)
  
  if(anio < year(fecha_min_data)) {
    cat("⚠ Saltando año", anio, "(antes de datos disponibles)\n")
    next
  }
  
  if(anio > year(fecha_max_data)) {
    cat("⚠ Saltando año", anio, "(después de datos disponibles)\n")
    next
  }
  
  # Ajustar fechas si el año está parcialmente fuera del rango
  if(fecha_inicio < fecha_min_data) fecha_inicio <- fecha_min_data
  if(fecha_fin > fecha_max_data) fecha_fin <- fecha_max_data
  
  fechas <- seq.Date(fecha_inicio, fecha_fin, by = "day")
  total_dias <- length(fechas)
  cat("Días a procesar:", total_dias, "\n")
  
  if(total_dias == 0) {
    cat("No hay días para procesar este año\n")
    next
  }
  
  # Calcular índice inicial (días desde 2000-01-01)
  fecha_inicio_2000 <- as.Date("2000-01-01")
  indice_inicial <- as.numeric(fecha_inicio - fecha_inicio_2000) + 1
  
  # Configurar paralelismo
  n_workers <- min(detectCores() - 1, 4)
  cat("Usando", n_workers, "workers paralelos\n")
  
  # Crear cluster
  cl <- makeCluster(n_workers)
  registerDoParallel(cl)
  
  # Procesar días en paralelo
  cat("Procesando días...\n")
  
  resultados <- foreach(i = 1:total_dias, 
                        .combine = c,
                        .errorhandling = "pass",
                        .packages = c("terra", "CAST", "lubridate", "ranger")) %dopar% {
                          
                          tryCatch({
                            # Cargar datos dentro del worker
                            data_df_worker <- readRDS("D:/S/Serbia1km/Interpolation/data_df_light.rds")
                            rfsi_model_worker <- readRDS("D:/S/Serbia1km/Interpolation/rfsi_model.rds")
                            
                            # Cargar raster base
                            raster_base_worker <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/raster_base_temp.tif")
                            
                            # Fecha actual
                            fecha_actual <- fecha_inicio + (i - 1)
                            indice_actual <- indice_inicial + (i - 1)
                            
                            # Nombre del archivo
                            nombre_archivo <- paste0("D:/S/Serbia1km/Interpolation/Huallaga/prcp_", 
                                                     format(fecha_actual, "%Y%m%d"), ".tif")
                            
                            # Si ya existe, no procesar
                            if(file.exists(nombre_archivo)) {
                              return(nombre_archivo)
                            }
                            
                            # Cargar capas climáticas (usando el mismo índice para los 3 rasters)
                            precland_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/precland_2000_2019.tif", 
                                                        lyrs = indice_actual)
                            tmax_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/tmax_2000_2019.tif", 
                                                    lyrs = indice_actual)
                            tmin_dia <- terra::rast("D:/S/Serbia1km/Interpolation/Huallaga/tmin_2000_2019.tif", 
                                                    lyrs = indice_actual)
                            
                            # Crear dataframe
                            newdata_df <- as.data.frame(raster_base_worker, xy = TRUE, na.rm = FALSE)
                            newdata_df$staid <- 1:nrow(newdata_df)
                            
                            # Extraer valores
                            newdata_df$precland <- terra::extract(precland_dia, newdata_df[, c("x", "y")])[,2]
                            newdata_df$tmax <- terra::extract(tmax_dia, newdata_df[, c("x", "y")])[,2]
                            newdata_df$tmin <- terra::extract(tmin_dia, newdata_df[, c("x", "y")])[,2]
                            
                            # Variables temporales
                            newdata_df$z <- as.numeric(fecha_actual - min(data_df_worker$date))
                            newdata_df$doy_sin <- sin(2 * pi * yday(fecha_actual) / 365.25)
                            newdata_df$doy_cos <- cos(2 * pi * yday(fecha_actual) / 365.25)
                            newdata_df$month <- month(fecha_actual)
                            
                            # Eliminar NAs
                            vars_needed <- c("staid", "x", "y", "z", "doy_sin", "doy_cos", "month",
                                             "dem", "slope", "twi", "sin", "cos", "precland", "tmax", "tmin")
                            
                            newdata_clean <- na.omit(newdata_df[, vars_needed])
                            
                            if(nrow(newdata_clean) == 0) return(NULL)
                            
                            # Muestra aleatoria si hay muchos puntos
                            if(nrow(newdata_clean) > 10000) {
                              set.seed(42)
                              newdata_clean <- newdata_clean[sample(1:nrow(newdata_clean), 10000), ]
                            }
                            
                            # Predecir
                            prediction <- pred.rfsi(
                              model = rfsi_model_worker,
                              data = data_df_worker,
                              obs.col = "prcp",
                              data.staid.x.y.z = c("staid", "x", "y", "z"),
                              newdata = newdata_clean,
                              newdata.staid.x.y.z = c("staid", "x", "y", "z"),
                              output.format = "data.frame",
                              cpus = 1,
                              progress = FALSE,
                              soil3d = FALSE
                            )
                            
                            # Crear raster
                            pred_raster <- terra::rast(raster_base_worker[[1]])
                            cells <- terra::cellFromXY(pred_raster, as.matrix(newdata_clean[, c("x", "y")]))
                            pred_raster[cells] <- prediction$pred
                            
                            names(pred_raster) <- paste0("prcp_", format(fecha_actual, "%Y%m%d"))
                            
                            # Guardar
                            terra::writeRaster(pred_raster, nombre_archivo, overwrite = TRUE)
                            
                            return(nombre_archivo)
                            
                          }, error = function(e) {
                            # Capturar y mostrar error específico
                            error_msg <- paste("Error en fecha", format(fecha_actual, "%Y-%m-%d"), 
                                               "índice", indice_actual, ":", e$message)
                            cat(error_msg, "\n")
                            return(NULL)
                          })
                        }
  
  # Detener cluster
  stopCluster(cl)
  
  # Filtrar resultados
  archivos_creados <- resultados[!sapply(resultados, is.null)]
  cat("\nArchivos creados:", length(archivos_creados), "de", total_dias, "\n")
  
  # Crear stack anual
  if(length(archivos_creados) > 0) {
    cat("Creando stack anual...\n")
    
    rasters_list <- list()
    for(archivo in archivos_creados) {
      if(file.exists(archivo)) {
        rast <- terra::rast(archivo)
        rasters_list[[basename(archivo)]] <- rast
      }
    }
    
    if(length(rasters_list) > 0) {
      stack_anual <- terra::rast(rasters_list)
      
      terra::writeRaster(stack_anual,
                         paste0("D:/QSWAT/Output_RFSI/prcp_", anio, ".tif"),
                         overwrite = TRUE)
      
      cat("Stack anual guardado: prcp_", anio, ".tif\n", sep = "")
      cat("Número de capas:", terra::nlyr(stack_anual), "\n")
    }
  }
  
  # Progreso
  años_completados <- which(anios == anio)
  años_restantes <- length(anios) - años_completados
  cat("Progreso:", años_completados, "/", length(anios), "años\n")
  if(años_restantes > 0) {
    cat("Años restantes:", años_restantes, "\n")
  }
  
  cat("Año", anio, "completado\n")
}

cat("\n===============================================\n")
cat("PROCESO COMPLETADO PARA 20 AÑOS (2000-2019)\n")
cat("===============================================\n")
cat("Archivos diarios: D:/S/Serbia1km/Interpolation/Huallaga/prcp_YYYYMMDD.tif\n")
cat("Stacks anuales: D:/QSWAT/Output_RFSI/prcp_YYYY.tif\n")
cat("Total estimado de días: ~7,305\n")
cat("===============================================\n")