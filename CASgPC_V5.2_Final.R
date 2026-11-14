# ==============================================================================
# CASγPC-R V5.2 – VERSIÓN DEFINITIVA CON MAPEO Y OPTIMIZACIÓN ESPACIO-TEMPORAL
# ==============================================================================

# 0. CONFIGURACIÓN INICIAL Y LIBRERÍAS -----------------------------------------
paquetes_necesarios <- c(
  "sf", "dplyr", "meteo", "ranger", "terra", "lubridate",
  "zoo", "parallel", "ggplot2",
  "tidyr", "gstat", "sp", "CAST", "caret", "purrr"
)
for (pkg in paquetes_necesarios) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ==============================================================================
# PARÁMETROS GLOBALES
# ==============================================================================
GAMMA_MIN           <- 0.5
GAMMA_MAX           <- 1.3
CAS_MIN             <- 0.6
CAS_MAX             <- 1.3
OOB_PROP            <- 0.20
N_TREES             <- 250
N_OBS_RFSI          <- 5
UMBRAL_LLUVIA_MM    <- 1.0   # [U1] Umbral fijo de "dia con lluvia" (WMO/ETCCDI)
UMBRAL_DINAMICO_PCT <- 0.15
K_FOLDS             <- 5

dir_datos       <- "D:/S/Serbia1km/Interpolation/"
dir_salida      <- "D:/R/RFSI paper/BlockVC/Output/"
dir_raster_5km  <- "D:/R/RFSI paper/BlockVC/raster_7kmv2/"
path_region_shp <- "D:/R/RFSI paper/clima/clasif_clima_peru/region.shp"
if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)
options(warn = 1, stringsAsFactors = FALSE)

# ==============================================================================
# 1. FUNCIONES AUXILIARES
# ==============================================================================
calcular_metricas <- function(obs, sim, label, region = NULL) {
  umbral <- UMBRAL_LLUVIA_MM
  idx <- complete.cases(obs, sim)
  obs <- obs[idx]; sim <- sim[idx]
  if (length(obs) < 10) return(NULL)
  rmse  <- sqrt(mean((obs - sim)^2))
  mae   <- mean(abs(obs - sim))
  r2    <- cor(obs, sim)^2
  bias  <- (sum(sim) - sum(obs)) / sum(obs) * 100
  r_cor <- cor(obs, sim)
  alpha <- sd(sim) / sd(obs)
  beta  <- mean(sim) / mean(obs)
  kge   <- 1 - sqrt((r_cor-1)^2 + (alpha-1)^2 + (beta-1)^2)
  nse   <- 1 - sum((obs - sim)^2) / sum((obs - mean(obs))^2)
  
  hits        <- sum(obs >= umbral & sim >= umbral)
  fals        <- sum(obs < umbral & sim >= umbral)
  miss        <- sum(obs >= umbral & sim < umbral)
  correct_neg <- sum(obs < umbral & sim < umbral)
  
  pod <- if (hits+miss>0)    hits/(hits+miss)    else NA
  far <- if (hits+fals>0)    fals/(hits+fals)    else NA
  csi <- if (hits+fals+miss>0) hits/(hits+fals+miss) else NA
  acc <- (hits+correct_neg)/(hits+fals+miss+correct_neg)
  
  data.frame(Fold = label,
             Region = if (!is.null(region)) region else "ALL",
             N = length(obs), RMSE = rmse, MAE = mae, R2 = r2,
             KGE = kge, NSE = nse, BIAS = bias,
             POD = pod, FAR = far, CSI = csi, ACC = acc,
             Umbral_mm = umbral)
}

calcular_metricas_por_region <- function(df, col_pred, label) {
  regiones <- unique(df$Region)
  resultados <- lapply(regiones, function(reg) {
    sub <- df[df$Region == reg, ]
    calcular_metricas(sub$prcp, sub[[col_pred]],
                      label = paste0(label, " | ", reg), region = reg)
  })
  global <- calcular_metricas(df$prcp, df[[col_pred]],
                              label = paste0(label, " | ALL"), region = NULL)
  do.call(rbind, c(resultados, list(global)))
}

# ==============================================================================
# 2. CARGA DE DATOS Y ASIGNACIÓN DE REGIÓN
# ==============================================================================
cat("\n[1] Cargando datos...\n")
data_raw    <- read.csv(paste0(dir_datos, "RFSI777_huallaga.csv"))
zonas_clima <- st_read(path_region_shp, quiet = TRUE) %>% st_transform(32718)

data <- data_raw %>%
  mutate(date  = as.Date(time, format = "%m/%d/%Y"),
         doy   = yday(date), month = month(date), year = year(date),
         staid = sp.ID) %>%
  filter(!is.na(prcp), !is.na(precland), !is.na(dem),
         !is.na(tmax), !is.na(tmin), prcp >= 0) %>%
  distinct()

fecha_min <- min(data$date)
data      <- data %>% mutate(z = as.numeric(date - fecha_min))

data_sf    <- st_as_sf(data, coords = c("lon","lat"), crs = 4326) %>% st_transform(32718)
coords_utm <- st_coordinates(data_sf)
data       <- data %>% mutate(x_utm = coords_utm[,"X"], y_utm = coords_utm[,"Y"])

data_sf_trabajo <- st_as_sf(data, coords = c("x_utm","y_utm"), crs = 32718, remove = FALSE)
data_con_zona   <- st_join(data_sf_trabajo, zonas_clima["Region"], join = st_intersects)

data <- data_con_zona %>%
  as.data.frame() %>% select(-geometry) %>%
  mutate(doy_sin = sin(2*pi*doy/365.25),
         doy_cos = cos(2*pi*doy/365.25)) %>%
  filter(!is.na(Region))

cat(" Regiones:", paste(unique(data$Region), collapse = ", "), "\n")

# ==============================================================================
# 3. BLOQUES ESPACIALES (blockCV)
# ==============================================================================
# Extraemos las estaciones únicas del dataset (puntos UTM fijos)
pa_data <- data %>%
  group_by(staid, x_utm, y_utm) %>%
  slice(1) %>%
  ungroup() %>%
  st_as_sf(coords = c("x_utm","y_utm"), crs = 32718)

# Cargamos r_base ÚNICAMENTE con capas estáticas de relieve/paisaje
r_base <- rast(c(paste0(dir_raster_5km,
                        c("dem_7km.tif", "dem_sd_7km.tif", "slope_7km.tif",
                          "sin_7km.tif", "cos_7km.tif", "twi_7km.tif"))))
names(r_base) <- c("Demav", "Demsd", "Slope", "Aspect_sin", "Aspect_cos", "TWI")

#  cv_spatial_autocor(
#    r = r_base, # a SpatRaster object or path to files
#    num_sample = 5000, # number of cells to be used
#    plot = TRUE, raster_colors = terrain.colors(10, alpha = 0.5)
#  )
# 
# set.seed(42)
# sb2 <- cv_spatial(x = pa_data, r = r_base, k = 5, size = 137817,
#                   hexagon = TRUE, selection = "random",
#                   iteration = 100, biomod2 = TRUE)
# 
# mapa_folds <- cv_plot(cv = sb2, r = r_base, x = pa_data, raster_colors = terrain.colors(10, alpha = 0.5)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6.5))
# #ggsave(paste0(dir_salida, "Mapa_Folds_Final_Regional.png"), plot = mapa_folds, width = 7.5, height = 6, dpi = 300)
# 
# folds_ref <- data.frame(staid = pa_data$staid, fold_spatial = sb2$folds_ids)
# data      <- data %>% left_join(folds_ref, by = "staid")
# cat("\nDistribución por región y fold espacial:\n")
# print(table(data$Region, data$fold_spatial))

# Ejecutamos kNNDM usando el DEM como dominio del modelo predictivo continuo
set.seed(42)
knndm_folds <- knndm(
  tpoints = pa_data,
  modeldomain = r_base[[1]],
  k = K_FOLDS,
  clustering = "hierarchical"
)

# ECDF de distancias (diagnostico metodologico, guardar para el paper/anexo)

plot(knndm_folds)
dev.off()

# Reconstrucción del vector plano de folds
fold_vector <- numeric(nrow(pa_data))
for (fold_id in 1:length(knndm_folds$indx_test)) {
  indices_testeo_fold <- knndm_folds$indx_test[[fold_id]]
  fold_vector[indices_testeo_fold] <- fold_id
}

# Chequeo explicito: estaciones que quedaron sin fold de test asignado (bug potencial)
n_sin_fold <- sum(fold_vector == 0)
cat(sprintf("\n[CHEQUEO] Estaciones sin fold de test asignado (fold_spatial == 0): %d\n", n_sin_fold))
if (n_sin_fold > 0) {
  warning("Hay estaciones con fold_spatial == 0: nunca entraran al set de test. Revisar knndm_folds$indx_test.")
}

folds_ref <- data.frame(
  staid        = pa_data$staid,
  fold_spatial = fold_vector
)

# Inyección del fold al dataset general diario
data <- data %>% left_join(folds_ref, by = "staid")

cat("\nDistribución final por región y fold espacial de CAST (kNNDM):\n")
print(table(data$Region, data$fold_spatial, useNA = "ifany"))
# cat("\nDistribución final por región y fold espacial de CAST (kNNDM):\n")
# print(table(data$Region, data$fold_spatial, useNA = "ifany"))

# ==============================================================================
# 3.1 MAPA DE FOLDS SOBRE TOPOGRAFÍA (figura metodológica para el paper)
# ==============================================================================
cat("\n[3] Generando mapa de folds sobre topografía (Demav)...\n")

mapa_cuenca <- r_base[["Demav"]]
crs_estaciones <- sf::st_crs(pa_data)$wkt
mapa_cuenca_reproyectado <- terra::project(mapa_cuenca, crs_estaciones)
cuenca_df <- as.data.frame(mapa_cuenca_reproyectado, xy = TRUE, na.rm = TRUE)
colnames(cuenca_df) <- c("x", "y", "Elevacion")

mapas_5_folds <- map_df(1:K_FOLDS, function(f) {
  idx_test  <- knndm_folds$indx_test[[f]]
  idx_train <- knndm_folds$indx_train[[f]]
  
  pa_data %>%
    mutate(
      set = case_when(
        row_number() %in% idx_test  ~ "test",
        row_number() %in% idx_train ~ "train",
        TRUE                        ~ "exclude"
      ),
      Fold = paste("Fold", f)
    )
})

grafico_final_con_dem <- ggplot() +
  geom_raster(data = cuenca_df, aes(x = x, y = y, fill = Elevacion)) +
  scale_fill_gradientn(
    colors = terrain.colors(10, alpha = 0.5),
    na.value = "transparent",
    guide = "none"
  ) +
  geom_sf(data = mapas_5_folds, aes(color = set), size = 2.8) +
  scale_color_manual(values = c(
    "train"   = "#7570b3",
    "test"    = "#d95f02",
    "exclude" = "#1b9e77"
  )) +
  facet_wrap(~Fold, nrow = 1) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 6),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2)
  ) +
  labs(
    title = "Esquema de Validación Cruzada kNNDM (5 Folds)",
    subtitle = "Partición espacial de estaciones sobre la topografía (Demav) de la cuenca",
    color = "",
    x = NULL, y = NULL
  )

print(grafico_final_con_dem)

# ==============================================================================
# 4. FÓRMULA DEL MODELO (SIN FFS – SE USAN TODAS LAS VARIABLES CANDIDATAS)
# ==============================================================================
cat("\n[4] Definiendo formula con todas las variables candidatas (sin FFS)...\n")

predictores_finales <- c("precland", "cape", "tmax", "tmin", "swvl1",
                         "demav", "demsd", "twi", "slope", "sin", "cos",
                         "doy_sin", "doy_cos")
predictores_finales <- intersect(predictores_finales, colnames(data))

fm.RFSI <- as.formula(paste("prcp ~", paste(predictores_finales, collapse = " + ")))

cat("\n=== VARIABLES UTILIZADAS EN RFSI (todas, sin seleccion previa) ===\n")
print(predictores_finales)
cat("Fórmula del modelo:", deparse(fm.RFSI), "\n")

cols_rfsi <- c("staid", "x_utm", "y_utm", "z", "prcp", predictores_finales)

# ==============================================================================
# 5. VALIDACIÓN CRUZADA V5.2
# ==============================================================================
cat("\n[4] Validación cruzada V5.2...\n")
res_cv_raw       <- data.frame()
res_cv_cor       <- data.frame()
pred_raw_totales <- data.frame()
pred_cor_totales <- data.frame()

for (f in 1:5) {
  cat(sprintf("\n  Fold %d/5\n", f))
  train_cv <- data[data$fold_spatial != f, ]
  test_cv  <- data[data$fold_spatial == f, ] %>% filter(!is.na(prcp))
  
  if (length(intersect(unique(test_cv$staid), unique(train_cv$staid))) > 0)
    warning(sprintf("Fold %d: estaciones solapadas", f))
  
  set.seed(100 + f)
  idx_inner   <- sample(nrow(train_cv), size = floor(OOB_PROP * nrow(train_cv)))
  train_inner <- train_cv[-idx_inner, ]
  val_inner   <- train_cv[idx_inner, ]
  
  mod_rfsi_inner <- rfsi(fm.RFSI, data = train_inner,
                         data.staid.x.y.z = c("staid","x_utm","y_utm","z"),
                         n.obs = N_OBS_RFSI, num.trees = N_TREES)
  pred_val <- meteo::pred.rfsi(mod_rfsi_inner, train_inner, "prcp",
                               c("staid","x_utm","y_utm","z"),
                               newdata = val_inner,
                               newdata.staid.x.y.z = c("staid","x_utm","y_utm","z"))
  val_inner$pred_rfsi_oob <- pmax(0, pred_val$pred)
  
  mod_rfsi_full <- rfsi(fm.RFSI, data = train_cv,
                        data.staid.x.y.z = c("staid","x_utm","y_utm","z"),
                        n.obs = N_OBS_RFSI, num.trees = N_TREES)
  pred_test <- meteo::pred.rfsi(mod_rfsi_full, train_cv, "prcp",
                                c("staid","x_utm","y_utm","z"),
                                newdata = test_cv,
                                newdata.staid.x.y.z = c("staid","x_utm","y_utm","z"))
  test_cv$pred_rfsi <- pmax(0, pred_test$pred)
  
  pred_raw_totales <- rbind(pred_raw_totales,
                            test_cv %>% select(staid, date, month, year, Region, prcp) %>%
                              mutate(pred_rfsi = test_cv$pred_rfsi, fold_spatial = f))
  res_cv_raw <- rbind(res_cv_raw,
                      calcular_metricas_por_region(test_cv, "pred_rfsi", paste0("Fold ", f)))
  
  factors_train <- val_inner %>%
    group_by(Region, year, month, staid) %>%
    summarise(obs_mes = sum(prcp), pred_mes = sum(pred_rfsi_oob), .groups = "drop") %>%
    mutate(factor_estacion = ifelse(pred_mes > 0, obs_mes / pred_mes, 1.0)) %>%
    group_by(Region, year, month) %>%
    summarise(factor_regional = mean(factor_estacion, na.rm = TRUE),
              n_est = n(), .groups = "drop") %>%
    mutate(factor_regional  = ifelse(n_est < 3, 1.0, factor_regional),
           factor_regional  = pmax(CAS_MIN, pmin(CAS_MAX, factor_regional)),
           factor_suavizado = rollapply(factor_regional, width = 3, FUN = mean,
                                        fill = factor_regional,
                                        align = "center", partial = TRUE))
  
  stats_train <- val_inner %>%
    group_by(Region, month) %>%
    summarise(mean_obs  = mean(prcp,           na.rm = TRUE),
              sd_obs    = sd(prcp,             na.rm = TRUE),
              mean_pred = mean(pred_rfsi_oob,  na.rm = TRUE),
              sd_pred   = sd(pred_rfsi_oob,    na.rm = TRUE),
              .groups   = "drop") %>%
    mutate(gamma_base = pmax(GAMMA_MIN, pmin(GAMMA_MAX,
                                             ifelse(sd_pred > 0, sd_obs / sd_pred, 1.0))))
  
  estadisticas_diarias <- train_cv %>%
    group_by(Region, date) %>%
    summarise(estaciones_con_lluvia = sum(prcp > 0.1, na.rm = TRUE),
              total_estaciones_reg  = sum(!is.na(prcp)),
              umbral_inferior       = max(0.5, quantile(prcp, 0.15, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(umbral_dinamico = pmax(1L, round(UMBRAL_DINAMICO_PCT * total_estaciones_reg)))
  
  umbral_mensual_train <- train_cv %>%
    filter(prcp > 0) %>%
    group_by(Region, month) %>%
    summarise(p99 = quantile(prcp, 0.99, na.rm = TRUE), .groups = "drop")
  
  test_cv_cor <- test_cv %>%
    left_join(factors_train %>% select(Region, year, month, factor_suavizado),
              by = c("Region","year","month")) %>%
    left_join(stats_train %>% select(Region, month, mean_obs, mean_pred, gamma_base),
              by = c("Region","month")) %>%
    left_join(estadisticas_diarias, by = c("Region","date")) %>%
    left_join(umbral_mensual_train, by = c("Region","month")) %>%
    mutate(
      factor_suavizado = ifelse(is.na(factor_suavizado), 1.0, factor_suavizado),
      pred_cas         = pred_rfsi * factor_suavizado,
      mean_obs         = ifelse(is.na(mean_obs),  0, mean_obs),
      mean_pred        = ifelse(is.na(mean_pred), 0, mean_pred),
      gamma_base       = ifelse(is.na(gamma_base), 1.0, gamma_base),
      #gamma_efectivo   = 1 + (gamma_base - 1) * (1 - exp(-pred_cas / 10)),
      gamma_efectivo   = pmax(GAMMA_MIN, pmin(GAMMA_MAX, gamma_base)),
      pred_var         = mean_obs + gamma_efectivo * (pred_cas - mean_pred),
      pred_var         = pmax(0, pred_var),
      p99              = ifelse(is.na(p99), Inf, p99),
      pred_pc          = pmin(pred_var, p99),
      pred_pc          = ifelse(estaciones_con_lluvia < umbral_dinamico, 0, pred_pc),
      #pred_pc          = ifelse(estaciones_con_lluvia < umbral_dinamico, pred_pc * 0.6, pred_pc),
      pred_pc          = ifelse(estaciones_con_lluvia >= umbral_dinamico &
                                  pred_pc < umbral_inferior, 0, pred_pc),
      pred_pc          = ifelse(total_estaciones_reg > 0 &
                                  estaciones_con_lluvia == 0, 0, pred_pc),
      pred_pc          = pmax(0, pred_pc)
    )
  
  res_cv_cor <- rbind(res_cv_cor,
                      calcular_metricas_por_region(test_cv_cor, "pred_pc", paste0("Fold ", f)))
  pred_cor_totales <- rbind(pred_cor_totales,
                            test_cv_cor %>%
                              select(staid, date, month, year, Region, prcp, pred_pc) %>%
                              mutate(fold_spatial = f))
  gc()
}

# ==============================================================================
# 6. TABLAS Y GRÁFICO
# ==============================================================================
cat("\n=== Métricas crudas ===\n")
resumen_raw <- res_cv_raw %>% filter(grepl("ALL", Region)) %>%
  summarise(Fold = "AVERAGE", Region = "ALL",
            across(N:Umbral_mm, ~ mean(.x, na.rm = TRUE)))
matriz_raw  <- bind_rows(res_cv_raw, resumen_raw)

print(matriz_raw)

cat("\n=== Métricas corregidas (V5.2) ===\n")
resumen_cor <- res_cv_cor %>% filter(grepl("ALL", Region)) %>%
  summarise(Fold = "AVERAGE", Region = "ALL",
            across(N:Umbral_mm, ~ mean(.x, na.rm = TRUE)))
matriz_cor  <- bind_rows(res_cv_cor, resumen_cor)

print(matriz_cor)

# metricas_plot <- c("KGE","BIAS","POD","FAR","CSI","ACC")
# df_plot <- bind_rows(
#   resumen_raw %>% mutate(Modelo = "Raw RFSI"),
#   resumen_cor %>% mutate(Modelo = "CASγPC-R V5.2")
# ) %>%
#   select(Modelo, all_of(metricas_plot)) %>%
#   pivot_longer(cols = all_of(metricas_plot), names_to = "Metrica", values_to = "Valor")
# 
# fig <- ggplot(df_plot, aes(x = Modelo, y = Valor, fill = Modelo)) +
#   geom_bar(stat = "identity", width = 0.5, color = "black", alpha = 0.85) +
#   facet_wrap(~Metrica, scales = "free_y", ncol = 3) +
#   scale_fill_manual(values = c("Raw RFSI" = "gray60", "CASγPC-R V5.2" = "#1a7a4a")) +
#   theme_bw(base_size = 12) +
#   labs(title = "CASγPC-R V5.2 vs Raw RFSI",
#        subtitle = sprintf("Gamma variable | CAS_MAX=%.1f | GAMMA_MAX=%.1f | PC=%d%%",
#                           CAS_MAX, GAMMA_MAX, round(UMBRAL_DINAMICO_PCT * 100)),
#        x = NULL, y = "Metric value", fill = "Model") +
#   theme(legend.position = "bottom",
#         strip.background   = element_rect(fill = "gray95"),
#         strip.text         = element_text(face = "bold"),
#         panel.grid.minor   = element_blank(),
#         axis.text.x        = element_blank(),
#         axis.ticks.x       = element_blank())
# ggsave(paste0(dir_salida, "Fig_Comparativa_V5.2.png"),
#        plot = fig, width = 10, height = 6, dpi = 300)

# ==============================================================================
# 7. ENTRENAMIENTO FINAL (todo el dataset)
# ==============================================================================
cat("\n[5] Entrenando modelo RFSI final sobre todo el dataset...\n")
rfsi_final <- rfsi(fm.RFSI, data = as.data.frame(data),
                   data.staid.x.y.z = c("staid","x_utm","y_utm","z"),
                   n.obs = N_OBS_RFSI, num.trees = N_TREES)

pred_base_est <- meteo::pred.rfsi(rfsi_final, as.data.frame(data), "prcp",
                                  c("staid","x_utm","y_utm","z"),
                                  newdata = as.data.frame(data),
                                  newdata.staid.x.y.z = c("staid","x_utm","y_utm","z"))
data$pred_rfsi_base <- pmax(0, pred_base_est$pred)

# Factores CAS globales
factors_globales <- data %>%
  group_by(Region, year, month, staid) %>%
  summarise(obs_mes  = sum(prcp,           na.rm = TRUE),
            pred_mes = sum(pred_rfsi_base, na.rm = TRUE),
            .groups  = "drop") %>%
  mutate(factor_estacion = ifelse(pred_mes > 0, obs_mes / pred_mes, 1.0)) %>%
  group_by(Region, year, month) %>%
  summarise(factor_regional = mean(factor_estacion, na.rm = TRUE),
            n_est = n(), .groups = "drop") %>%
  mutate(factor_regional  = ifelse(n_est < 3, 1.0, factor_regional),
         factor_regional  = pmax(CAS_MIN, pmin(CAS_MAX, factor_regional)),
         factor_suavizado = rollapply(factor_regional, width = 3, FUN = mean,
                                      fill = factor_regional,
                                      align = "center", partial = TRUE))

# C1: Factor mensual promedio como fallback cuando year/mes no existe en factors_globales
factors_mensual_fallback <- factors_globales %>%
  group_by(Region, month) %>%
  summarise(factor_suavizado_fb = mean(factor_suavizado, na.rm = TRUE), .groups = "drop")

# Estadísticos gamma globales
stats_globales <- data %>%
  group_by(Region, month) %>%
  summarise(mean_obs  = mean(prcp,           na.rm = TRUE),
            sd_obs    = sd(prcp,             na.rm = TRUE),
            mean_pred = mean(pred_rfsi_base, na.rm = TRUE),
            sd_pred   = sd(pred_rfsi_base,   na.rm = TRUE),
            .groups   = "drop") %>%
  mutate(gamma_base = pmax(GAMMA_MIN, pmin(GAMMA_MAX,
                                           ifelse(sd_pred > 0, sd_obs / sd_pred, 1.0))))

# p99 mensual global
umbral_mensual <- data %>%
  filter(prcp > 0) %>%
  group_by(Region, month) %>%
  summarise(p99 = quantile(prcp, 0.99, na.rm = TRUE), .groups = "drop")

# Estadísticas diarias observadas (para PC en producción)
estadisticas_diarias_obs <- data %>%
  group_by(Region, date) %>%
  summarise(estaciones_con_lluvia = sum(prcp > 0.1, na.rm = TRUE),
            total_estaciones_reg  = sum(!is.na(prcp)),
            umbral_inferior       = max(0.5, quantile(prcp, 0.15, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(umbral_dinamico = pmax(1L, round(UMBRAL_DINAMICO_PCT * total_estaciones_reg)))

# ==============================================================================
# 8. GRILLA DE PREDICCIÓN (5 km)
# ==============================================================================
cat("\n[6] Preparando grilla de predicción (5 km)...\n")
grid_base_rast  <- r_base[[1]]
grid_cells      <- as.data.frame(grid_base_rast, xy = TRUE, na.rm = TRUE) %>%
  select(x, y)
grid_cells_sf   <- st_as_sf(grid_cells, coords = c("x","y"), crs = 32718, remove = FALSE)
grid_con_region <- st_join(grid_cells_sf, zonas_clima["Region"],
                           join = st_intersects, left = TRUE)
grid_cells$Region <- grid_con_region$Region

grid_values    <- terra::extract(r_base, grid_cells[, c("x","y")])
grid_values$ID <- NULL
grid_cells     <- cbind(grid_cells, grid_values) %>% filter(!is.na(Region))
cat(sprintf(" Grilla final: %d celdas\n", nrow(grid_cells)))

# Rasters de covariables diarias
r_precland_utm <- rast(paste0(dir_raster_5km, "precland.tif"))
r_tmax_utm     <- rast(paste0(dir_raster_5km, "tmax_dailyutm.tif"))
r_tmin_utm     <- rast(paste0(dir_raster_5km, "tmin_dailyutm.tif"))

path_p          <- sources(r_precland_utm)
path_tx         <- sources(r_tmax_utm)
path_tn         <- sources(r_tmin_utm)

años_procesar       <- 2000:2003
fechas_totales      <- seq(as.Date("2000-01-01"), as.Date("2017-12-31"), by = "day")
fecha_inicio_raster <- as.Date("2000-01-01")

# C3: exportar solo columnas necesarias para pred.rfsi
cols_export   <- unique(c("staid","x_utm","y_utm","z","prcp", predictores_finales))
cols_export   <- intersect(cols_export, names(data))
data_export   <- as.data.frame(data)[, cols_export]

# Serializar objeto terra para workers (C8)
grid_base_string  <- terra::wrap(grid_base_rast)
coordenadas_fijas <- as.matrix(grid_cells[, c("x","y")])

# ==============================================================================
# 9. MAPEO DIARIO EN PARALELO (NetCDF con ncdf4 - 7 DÍAS DE PRUEBA)
# ==============================================================================
n_cores <- max(1, parallel::detectCores() - 2)
cl      <- makeSOCKcluster(n_cores)
registerDoSNOW(cl)
registerDoRNG(seed = 42)

# C8: precargar librerías en cada worker (INCLUYE ncdf4)
clusterEvalQ(cl, { 
  library(terra)
  library(dplyr)
  library(lubridate)
  library(meteo)
  library(ncdf4)   # <--- IMPORTANTE: añadido para escritura NetCDF
})

cat(sprintf("\n[7] Mapeo diario (%d años, %d cores) - NetCDF con ncdf4 (PRUEBA 7 DÍAS)...\n",
            length(años_procesar), n_cores))

pb   <- txtProgressBar(max = length(años_procesar), style = 3)
opts <- list(progress = function(n) setTxtProgressBar(pb, n))

resultados <- foreach(
  a = seq_along(años_procesar),
  .packages        = c("terra","dplyr","meteo","lubridate","zoo","ncdf4"),  # ncdf4 añadido
  .export          = c("umbral_mensual","factors_globales","factors_mensual_fallback",
                       "stats_globales","estadisticas_diarias_obs",
                       "rfsi_final","data_export","grid_cells",
                       "fechas_totales","fecha_inicio_raster","dir_salida",
                       "path_p","path_tx","path_tn","fecha_min",
                       "CAS_MIN","CAS_MAX","GAMMA_MIN","GAMMA_MAX",
                       "UMBRAL_DINAMICO_PCT","predictores_finales",
                       "grid_base_string","coordenadas_fijas"),
  .options.snow    = opts,
  .errorhandling   = "stop"
) %dopar% {
  
  anio  <- años_procesar[a]
  # --- MODO PRUEBA: SOLO PRIMEROS 7 DÍAS ---
  fechas <- fechas_totales[lubridate::year(fechas_totales) == anio]
  
  # Deserializar raster plantilla
  plantilla_local <- terra::unwrap(grid_base_string)
  
  # Abrir rasters de covariables (una vez por worker/año)
  rp_core  <- terra::rast(path_p)
  rtx_core <- terra::rast(path_tx)
  rtn_core <- terra::rast(path_tn)
  
  capas <- vector("list", length(fechas))
  
  for (fi in seq_along(fechas)) {
    fecha <- fechas[fi]
    idx   <- as.numeric(fecha - fecha_inicio_raster) + 1
    if (idx < 1 || idx > terra::nlyr(rp_core)) next
    
    capas[[fi]] <- tryCatch({
      mes          <- lubridate::month(fecha)
      anio_actual  <- lubridate::year(fecha)
      
      # Extraer covariables diarias sobre la grilla
      temp_grid          <- grid_cells
      temp_grid$precland <- terra::extract(rp_core[[idx]],  coordenadas_fijas)[, 1]
      temp_grid$tmax     <- terra::extract(rtx_core[[idx]], coordenadas_fijas)[, 1]
      temp_grid$tmin     <- terra::extract(rtn_core[[idx]], coordenadas_fijas)[, 1]
      
      grid_pred <- temp_grid %>%
        mutate(staid   = seq_len(n()),
               z       = as.numeric(fecha - fecha_min),
               month   = mes,
               doy     = lubridate::yday(fecha),
               doy_sin = sin(2 * pi * doy / 365.25),
               doy_cos = cos(2 * pi * doy / 365.25)) %>%
        na.omit()
      
      if (nrow(grid_pred) == 0) return(NULL)
      
      # C4: solo columnas que pred.rfsi necesita en newdata
      cols_new <- c("staid","x","y","z", predictores_finales)
      cols_new <- intersect(cols_new, names(grid_pred))
      
      pred_rfsi_grilla <- meteo::pred.rfsi(
        model                  = rfsi_final,
        data                   = data_export,
        obs.col                = "prcp",
        data.staid.x.y.z       = c("staid","x_utm","y_utm","z"),
        newdata                = grid_pred[, cols_new],
        newdata.staid.x.y.z    = c("staid","x","y","z")
      )
      pred_raw             <- pmax(0, pred_rfsi_grilla$pred)
      pred_raw[is.na(pred_raw) | is.infinite(pred_raw)] <- 0
      
      # C1: factor CAS con fallback mensual si el año no existe
      fac_anio <- factors_globales %>%
        filter(year == anio_actual, month == mes) %>%
        select(Region, factor_suavizado)
      
      if (nrow(fac_anio) == 0) {
        fac_anio <- factors_mensual_fallback %>%
          filter(month == mes) %>%
          rename(factor_suavizado = factor_suavizado_fb)
      }
      
      # C6: vectorizar corrección (sin loop por región)
      grid_pred$pred_raw <- pred_raw
      
      grid_pred <- grid_pred %>%
        left_join(fac_anio, by = "Region") %>%
        left_join(stats_globales %>% filter(month == mes) %>%
                    select(Region, mean_obs, mean_pred, gamma_base), by = "Region") %>%
        left_join(umbral_mensual %>% filter(month == mes) %>%
                    select(Region, p99), by = "Region") %>%
        mutate(
          factor_suavizado = ifelse(is.na(factor_suavizado), 1.0, factor_suavizado),
          pred_cas         = pred_raw * factor_suavizado,
          mean_obs         = ifelse(is.na(mean_obs),  0, mean_obs),
          mean_pred        = ifelse(is.na(mean_pred), 0, mean_pred),
          gamma_base       = ifelse(is.na(gamma_base), 1.0, gamma_base),
          #gamma_efectivo   = 1 + (gamma_base - 1) * (1 - exp(-pred_cas / 10)),
          gamma_efectivo   = pmax(GAMMA_MIN, pmin(GAMMA_MAX, gamma_base)),
          pred_var         = mean_obs + gamma_efectivo * (pred_cas - mean_pred),
          pred_var         = pmax(0, pred_var),
          p99              = ifelse(is.na(p99), Inf, p99),
          pred_pc          = pmin(pred_var, p99)
        )
      
      # PC vectorizado: usando estadísticas diarias pre-calculadas
      pc_dia <- estadisticas_diarias_obs %>%
        filter(date == fecha) %>%
        select(Region, estaciones_con_lluvia, total_estaciones_reg,
               umbral_inferior, umbral_dinamico)
      
      if (nrow(pc_dia) > 0) {
        grid_pred <- grid_pred %>%
          left_join(pc_dia, by = "Region") %>%
          mutate(
            pred_pc = ifelse(!is.na(estaciones_con_lluvia) &
                               estaciones_con_lluvia < umbral_dinamico,
                             pred_pc * 0.6, pred_pc),
            pred_pc = ifelse(!is.na(estaciones_con_lluvia) &
                               estaciones_con_lluvia >= umbral_dinamico &
                               pred_pc < umbral_inferior, 0, pred_pc),
            pred_pc = ifelse(!is.na(total_estaciones_reg) &
                               total_estaciones_reg > 0 &
                               estaciones_con_lluvia == 0, 0, pred_pc),
            pred_pc = pmax(0, pred_pc)
          )
      }
      
      final_valores <- grid_pred$pred_pc
      final_valores[is.na(final_valores) | is.infinite(final_valores)] <- 0
      
      # C5: filtrar celdas válidas antes de asignar
      r_out     <- terra::rast(plantilla_local)
      terra::values(r_out) <- NA_real_
      xy_valid  <- as.matrix(grid_pred[, c("x","y")])
      celdas    <- terra::cellFromXY(r_out, xy_valid)
      ok        <- !is.na(celdas)
      r_out[celdas[ok]] <- final_valores[ok]
      names(r_out) <- paste0("d_", format(fecha, "%Y%m%d"))
      r_out
      
    }, error = function(e) {
      message(sprintf("ERROR %s: %s", format(fecha), e$message))
      NULL
    })
  }
  
  rm(rp_core, rtx_core, rtn_core, plantilla_local)
  gc()
  
  # ======================================================================
  # C7: ESCRITURA EN NetCDF CON ncdf4 (VERSIÓN QUE TÚ PUSISTE)
  # ======================================================================
  # C7: filtrar NULLs antes de consolidar el bloque NetCDF
  capas <- capas[!sapply(capas, is.null)]
  if (length(capas) > 0) {
    raster_anual <- terra::rast(capas)
    
    ncapas <- terra::nlyr(raster_anual)
    nrows  <- terra::nrow(raster_anual)
    ncols  <- terra::ncol(raster_anual)
    
    # 1. Extraer vectores de coordenadas (Ejes UTM: Este y Norte)
    lons <- terra::xFromCol(raster_anual, 1:ncols)
    lats <- terra::yFromRow(raster_anual, 1:nrows)
    
    lons <- sort(lons)
    lats <- sort(lats, decreasing = TRUE) # Orden Norte-Sur correcto para QGIS
    
    # 2. Extraer y transponer la matriz a [lon, lat, tiempo] de forma directa
    vals_terra <- terra::as.array(raster_anual) 
    vals_array <- aperm(vals_terra, c(2, 1, 3)) # Reordenar a [col, fila, tiempo]
    
    # 3. Calcular el eje de tiempo en DÍAS enteros (Produce: time=0, time=1, time=2...)
    fechas_capas <- as.Date(gsub("d_", "", names(raster_anual)), format = "%Y%m%d")
    time_days    <- as.numeric(fechas_capas - fechas_capas[1])
    
    # 4. Definir dimensiones NetCDF
    dim_lon  <- ncdf4::ncdim_def("x", "meters", vals = lons)
    dim_lat  <- ncdf4::ncdim_def("y", "meters", vals = lats)
    dim_time <- ncdf4::ncdim_def(
      "time", 
      paste0("days since ", fechas_capas[1], " 00:00:00"), # Origen temporal dinámico
      vals     = as.double(time_days),
      calendar = "proleptic_gregorian",
      unlim    = TRUE
    )
    
    # 5. Definir la variable de datos (Precipitación diaria)
    var_prcp <- ncdf4::ncvar_def(
      name          = "prcp",
      units         = "mm",
      dim           = list(dim_lon, dim_lat, dim_time),
      missval       = -9999.0,
      longname      = "Daily Precipitation",
      prec          = "float",
      compression   = 3,
      shuffle       = TRUE
    )
    
    # 6. Definir variable de control para la Proyección (UTM 18S - EPSG:32718)
    var_crs <- ncdf4::ncvar_def(name = "crs", units = "", dim = list(), prec = "integer")
    
    # 7. Crear el archivo NetCDF físico
    archivo_salida <- paste0(dir_salida, "CASgPC_V5.2_", anio, ".nc")
    ncout <- ncdf4::nc_create(archivo_salida, list(var_prcp, var_crs), force_v4 = TRUE)
    
    # Escribir la matriz de datos
    ncdf4::ncvar_put(ncout, var_prcp, vals_array)
    
    # 8. Inyectar metadatos CF y OGC para que QGIS reconozca el CRS automáticamente
    ncdf4::ncatt_put(ncout, "crs", "grid_mapping_name", "transverse_mercator")
    ncdf4::ncatt_put(ncout, "crs", "epsg_code", "EPSG:32718")
    ncdf4::ncatt_put(ncout, "crs", "spatial_ref", 'PROJCS["WGS 84 / UTM zone 18S",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-75],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",10000000],UNIT["metre",1]]')
    
    # Amarre espacial de la precipitación y asignación del standard_name para flujos
    ncdf4::ncatt_put(ncout, "prcp", "grid_mapping", "crs")
    ncdf4::ncatt_put(ncout, "prcp", "standard_name", "precipitation_flux")
    
    # Atributos de coordenadas
    ncdf4::ncatt_put(ncout, "x", "standard_name", "projection_x_coordinate")
    ncdf4::ncatt_put(ncout, "y", "standard_name", "projection_y_coordinate")
    
    # Atributos globales del archivo
    ncdf4::ncatt_put(ncout, 0, "title", "CASγPC-R Daily Precipitation for Peru")
    ncdf4::ncatt_put(ncout, 0, "institution", "Your Institution")
    ncdf4::ncatt_put(ncout, 0, "source", "RFSI + CASγPC-R post-processing")
    ncdf4::ncatt_put(ncout, 0, "reference", "Sekulić et al. (2020) and custom CASγPC-R")
    ncdf4::ncatt_put(ncout, 0, "Conventions", "CF-1.6")
    
    # Cerrar el NetCDF
    ncdf4::nc_close(ncout)
    
    paste("Año", anio, "OK (NetCDF Perfecto para QGIS) —", ncapas, "días")
  } else {
    paste("Año", anio, "SIN DATOS")
  }
}

close(pb)
stopCluster(cl)

library(netcdf4)
Pisco.prec.brick <- brick("D:/R/RFSI paper/BlockVC/Output/CASgPC_V5.2_2000.nc")# leemos archivo netcdf con brick 
nlayers(Pisco.prec.brick) 
spplot(Pisco.prec.brick[[1:7]])
# cat("\n\n=== RESUMEN DEL MAPEO ===\n")
# invisible(lapply(resultados, function(x) cat(" ", x, "\n")))
# cat("\n[FIN] Proceso completado.\n")
# cat("  Archivos en:", dir_salida, "\n")

# Control rápido del primer año
r_control <- terra::rast(paste0(dir_salida, "CASgPC_V5.2_", años_procesar[1], ".tif"))
cat(sprintf("\nRaster de control: %d capas (año %d)\n",
            terra::nlyr(r_control), años_procesar[1]))

r_control <- terra::rast(paste0(dir_salida, "CASgPC_V5.2_2000.tif"))
terra::plot(r_control[[1:7]], col = colorRampPalette(c("white", "lightblue", "blue", "purple"))(100))

# Suponiendo que tu objeto raster se llama 'precip_corregida'
# Encontramos el valor máximo global de todo el período para fijar el techo
max_global <- max(global(r_control, "max", na.rm = TRUE)$max)

# Regresando al estilo original agradable con escalas dinámicas por día
terra::plot(r_control[[1:16]], 
            col = colorRampPalette(c("white", "lightblue", "blue", "purple"))(100),
            nc = 4)



library(raster)
r <- stack("D:/R/RFSI paper/BlockVC/Output/CASgPC_V5.2_2000.tif")

plot(r)

# ======================================================================
# DEPURACIÓN: SIMULACIÓN DE UN WORKER PARA UN DÍA ESPECÍFICO PARA ANALISIS DEL CODIGO
# ======================================================================

# 1. Definir el año y día a probar
anio_prueba <- 2000
fecha_prueba <- as.Date("2000-01-15")

# 2. Preparar entorno (igual que en el worker)
# Usar los objetos que ya tienes en el entorno global
plantilla_local <- terra::unwrap(grid_base_string)

# 3. Abrir rasters de covariables (igual que en el worker)
cat("Cargando rasters de covariables...\n")
rp_core  <- terra::rast(path_p)
rtx_core <- terra::rast(path_tx)
rtn_core <- terra::rast(path_tn)

# 4. Calcular el índice de la capa para la fecha
idx <- as.numeric(fecha_prueba - fecha_inicio_raster) + 1
cat("Índice de capa:", idx, "\n")

# 5. Extraer covariables diarias sobre la grilla (¡esto es lo que quieres ver!)
cat("Extrayendo covariables dinámicas...\n")
temp_grid <- grid_cells
temp_grid$precland <- terra::extract(rp_core[[idx]],  coordenadas_fijas)[, 1]
temp_grid$tmax     <- terra::extract(rtx_core[[idx]], coordenadas_fijas)[, 1]
temp_grid$tmin     <- terra::extract(rtn_core[[idx]], coordenadas_fijas)[, 1]

# 6. Agregar variables temporales
mes <- lubridate::month(fecha_prueba)
anio_actual <- lubridate::year(fecha_prueba)

grid_pred <- temp_grid %>%
  mutate(staid   = seq_len(n()),
         z       = as.numeric(fecha_prueba - fecha_min),
         month   = mes,
         doy     = lubridate::yday(fecha_prueba),
         doy_sin = sin(2 * pi * doy / 365.25),
         doy_cos = cos(2 * pi * doy / 365.25)) %>%
  na.omit()

cat("Filas después de na.omit:", nrow(grid_pred), "\n")
cat("Columnas en grid_pred:\n")
print(names(grid_pred))

# 7. Mostrar las primeras filas de las covariables extraídas
cat("\nPrimeras 6 filas de grid_pred (solo columnas seleccionadas):\n")
print(head(grid_pred[, c("x", "y", "Region", "precland", "tmax", "tmin", "doy_sin", "doy_cos")]))

# 8. Verificar que la región esté bien asignada
cat("\nDistribución de regiones en grid_pred:\n")
print(table(grid_pred$Region))

# 9. (Opcional) Ver un resumen de los valores extraídos
cat("\nResumen de covariables dinámicas:\n")
summary(grid_pred[, c("precland", "tmax", "tmin")])

# 10. Predicción RFSI (solo para verificar, sin corrección)
cols_new <- c("staid","x","y","z", predictores_finales)
cols_new <- intersect(cols_new, names(grid_pred))

pred_rfsi_grilla <- meteo::pred.rfsi(
  model                  = rfsi_final,
  data                   = data_export,
  obs.col                = "prcp",
  data.staid.x.y.z       = c("staid","x_utm","y_utm","z"),
  newdata                = grid_pred[, cols_new],
  newdata.staid.x.y.z    = c("staid","x","y","z")
)
pred_raw <- pmax(0, pred_rfsi_grilla$pred)
grid_pred$pred_raw <- pred_raw

cat("\nResumen de pred_raw (RFSI crudo):\n")
summary(grid_pred$pred_raw)

# 11. Aplicar corrección CAS (solo para un mes/año)
fac_anio <- factors_globales %>%
  filter(year == anio_actual, month == mes) %>%
  select(Region, factor_suavizado)

if (nrow(fac_anio) == 0) {
  fac_anio <- factors_mensual_fallback %>%
    filter(month == mes) %>%
    rename(factor_suavizado = factor_suavizado_fb)
}

grid_pred <- grid_pred %>%
  left_join(fac_anio, by = "Region") %>%
  left_join(stats_globales %>% filter(month == mes) %>%
              select(Region, mean_obs, mean_pred, gamma_base), by = "Region") %>%
  left_join(umbral_mensual %>% filter(month == mes) %>%
              select(Region, p99), by = "Region") %>%
  mutate(
    factor_suavizado = ifelse(is.na(factor_suavizado), 1.0, factor_suavizado),
    pred_cas         = pred_raw * factor_suavizado,
    mean_obs         = ifelse(is.na(mean_obs),  0, mean_obs),
    mean_pred        = ifelse(is.na(mean_pred), 0, mean_pred),
    gamma_base       = ifelse(is.na(gamma_base), 1.0, gamma_base),
    gamma_efectivo   = 1 + (gamma_base - 1) * (1 - exp(-pred_cas / 10)),
    gamma_efectivo   = pmax(GAMMA_MIN, pmin(GAMMA_MAX, gamma_efectivo)),
    pred_var         = mean_obs + gamma_efectivo * (pred_cas - mean_pred),
    pred_var         = pmax(0, pred_var),
    p99              = ifelse(is.na(p99), Inf, p99),
    pred_pc          = pmin(pred_var, p99)
  )

# 12. Ver los factores regionales aplicados
cat("\nFactores CAS por región para", mes, "/", anio_actual, ":\n")
print(unique(grid_pred[, c("Region", "factor_suavizado")]))

cat("\nResumen de pred_cas, pred_var, pred_pc:\n")
summary(grid_pred[, c("pred_raw", "pred_cas", "pred_var", "pred_pc")])

# 13. Mostrar una muestra de los datos finales
cat("\nMuestra de datos finales (primeras 10 filas):\n")
print(head(grid_pred[, c("x", "y", "Region", "pred_raw", "pred_cas", "pred_var", "pred_pc")], 10))



















# guardar en tif

# ==============================================================================
# 9. MAPEO DIARIO EN PARALELO
# ==============================================================================
n_cores <- max(1, parallel::detectCores() - 2)
cl      <- makeSOCKcluster(n_cores)
registerDoSNOW(cl)
registerDoRNG(seed = 42)

# C8: precargar terra en cada worker
clusterEvalQ(cl, { library(terra); library(dplyr); library(lubridate); library(meteo) })

cat(sprintf("\n[7] Mapeo diario (%d años, %d cores)...\n",
            length(años_procesar), n_cores))

pb   <- txtProgressBar(max = length(años_procesar), style = 3)
opts <- list(progress = function(n) setTxtProgressBar(pb, n))

resultados <- foreach(
  a = seq_along(años_procesar),
  .packages  = c("terra","dplyr","meteo","lubridate","zoo"),
  .export    = c("umbral_mensual","factors_globales","factors_mensual_fallback",
                 "stats_globales","estadisticas_diarias_obs",
                 "rfsi_final","data_export","grid_cells",
                 "fechas_totales","fecha_inicio_raster","dir_salida",
                 "path_p","path_tx","path_tn","fecha_min",
                 "CAS_MIN","CAS_MAX","GAMMA_MIN","GAMMA_MAX",
                 "UMBRAL_DINAMICO_PCT","predictores_finales",
                 "grid_base_string","coordenadas_fijas"),
  .options.snow    = opts,
  .errorhandling   = "stop"
) %dopar% {
  
  anio  <- años_procesar[a]
  fechas <- fechas_totales[lubridate::year(fechas_totales) == anio][1:21]
  
  # Deserializar raster plantilla
  plantilla_local <- terra::unwrap(grid_base_string)
  
  # Abrir rasters de covariables (una vez por worker/año)
  rp_core  <- terra::rast(path_p)
  rtx_core <- terra::rast(path_tx)
  rtn_core <- terra::rast(path_tn)
  
  capas <- vector("list", length(fechas))
  
  for (fi in seq_along(fechas)) {
    fecha <- fechas[fi]
    idx   <- as.numeric(fecha - fecha_inicio_raster) + 1
    if (idx < 1 || idx > terra::nlyr(rp_core)) next
    
    capas[[fi]] <- tryCatch({
      mes          <- lubridate::month(fecha)
      anio_actual  <- lubridate::year(fecha)
      
      # Extraer covariables diarias sobre la grilla
      temp_grid        <- grid_cells
      temp_grid$precland <- terra::extract(rp_core[[idx]],  coordenadas_fijas)[, 1]
      temp_grid$tmax     <- terra::extract(rtx_core[[idx]], coordenadas_fijas)[, 1]
      temp_grid$tmin     <- terra::extract(rtn_core[[idx]], coordenadas_fijas)[, 1]
      
      grid_pred <- temp_grid %>%
        mutate(staid   = seq_len(n()),
               z       = as.numeric(fecha - fecha_min),
               month   = mes,
               doy     = lubridate::yday(fecha),
               doy_sin = sin(2 * pi * doy / 365.25),
               doy_cos = cos(2 * pi * doy / 365.25)) %>%
        na.omit()
      
      if (nrow(grid_pred) == 0) return(NULL)
      
      # C4: solo columnas que pred.rfsi necesita en newdata
      cols_new <- c("staid","x","y","z", predictores_finales)
      cols_new <- intersect(cols_new, names(grid_pred))
      
      pred_rfsi_grilla <- meteo::pred.rfsi(
        model                  = rfsi_final,
        data                   = data_export,
        obs.col                = "prcp",
        data.staid.x.y.z       = c("staid","x_utm","y_utm","z"),
        newdata                = grid_pred[, cols_new],
        newdata.staid.x.y.z    = c("staid","x","y","z")
      )
      pred_raw             <- pmax(0, pred_rfsi_grilla$pred)
      pred_raw[is.na(pred_raw) | is.infinite(pred_raw)] <- 0
      
      # C1: factor CAS con fallback mensual si el año no existe
      fac_anio <- factors_globales %>%
        filter(year == anio_actual, month == mes) %>%
        select(Region, factor_suavizado)
      
      if (nrow(fac_anio) == 0) {
        fac_anio <- factors_mensual_fallback %>%
          filter(month == mes) %>%
          rename(factor_suavizado = factor_suavizado_fb)
      }
      
      # C6: vectorizar corrección (sin loop por región)
      grid_pred$pred_raw <- pred_raw
      
      grid_pred <- grid_pred %>%
        left_join(fac_anio, by = "Region") %>%
        left_join(stats_globales %>% filter(month == mes) %>%
                    select(Region, mean_obs, mean_pred, gamma_base), by = "Region") %>%
        left_join(umbral_mensual %>% filter(month == mes) %>%
                    select(Region, p99), by = "Region") %>%
        mutate(
          factor_suavizado = ifelse(is.na(factor_suavizado), 1.0, factor_suavizado),
          pred_cas         = pred_raw * factor_suavizado,
          mean_obs         = ifelse(is.na(mean_obs),  0, mean_obs),
          mean_pred        = ifelse(is.na(mean_pred), 0, mean_pred),
          gamma_base       = ifelse(is.na(gamma_base), 1.0, gamma_base),
          gamma_efectivo   = 1 + (gamma_base - 1) * (1 - exp(-pred_cas / 10)),
          gamma_efectivo   = pmax(GAMMA_MIN, pmin(GAMMA_MAX, gamma_efectivo)),
          pred_var         = mean_obs + gamma_efectivo * (pred_cas - mean_pred),
          pred_var         = pmax(0, pred_var),
          p99              = ifelse(is.na(p99), Inf, p99),
          pred_pc          = pmin(pred_var, p99)
        )
      
      # PC vectorizado: usando estadísticas diarias pre-calculadas
      pc_dia <- estadisticas_diarias_obs %>%
        filter(date == fecha) %>%
        select(Region, estaciones_con_lluvia, total_estaciones_reg,
               umbral_inferior, umbral_dinamico)
      
      if (nrow(pc_dia) > 0) {
        grid_pred <- grid_pred %>%
          left_join(pc_dia, by = "Region") %>%
          mutate(
            pred_pc = ifelse(!is.na(estaciones_con_lluvia) &
                               estaciones_con_lluvia < umbral_dinamico,
                             pred_pc * 0.6, pred_pc),
            pred_pc = ifelse(!is.na(estaciones_con_lluvia) &
                               estaciones_con_lluvia >= umbral_dinamico &
                               pred_pc < umbral_inferior, 0, pred_pc),
            pred_pc = ifelse(!is.na(total_estaciones_reg) &
                               total_estaciones_reg > 0 &
                               estaciones_con_lluvia == 0, 0, pred_pc),
            pred_pc = pmax(0, pred_pc)
          )
      }
      
      final_valores <- grid_pred$pred_pc
      final_valores[is.na(final_valores) | is.infinite(final_valores)] <- 0
      
      # C5: filtrar celdas válidas antes de asignar
      r_out     <- terra::rast(plantilla_local)
      terra::values(r_out) <- NA_real_
      xy_valid  <- as.matrix(grid_pred[, c("x","y")])
      celdas    <- terra::cellFromXY(r_out, xy_valid)
      ok        <- !is.na(celdas)
      r_out[celdas[ok]] <- final_valores[ok]
      names(r_out) <- paste0("d_", format(fecha, "%Y%m%d"))
      r_out
      
    }, error = function(e) {
      message(sprintf("ERROR %s: %s", format(fecha), e$message))
      NULL
    })
  }
  
  rm(rp_core, rtx_core, rtn_core, plantilla_local)
  gc()
  
  # C7: filtrar NULLs antes de apilar
  capas <- capas[!sapply(capas, is.null)]
  if (length(capas) > 0) {
    raster_anual  <- terra::rast(capas)
    archivo_salida <- paste0(dir_salida, "CASgPC_V5.2_", anio, ".tif")
    terra::writeRaster(raster_anual, archivo_salida,
                       overwrite = TRUE, gdal = "COMPRESS=LZW")
    paste("Año", anio, "OK —", length(capas), "días")
  } else {
    paste("Año", anio, "SIN DATOS")
  }
}

close(pb)
stopCluster(cl)







# ============================================================
# CONVERSIÓN DE CAUDAL DIARIO A MENSUAL (solo días con dato)
# ============================================================
# ============================================================
# PREPARACIÓN DE DATOS PARA GR4J (diario) Y GR2M (mensual)
# Estación Chazuta - Caudales diarios
# ============================================================

# 1. Cargar librerías
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

# 2. Leer archivo (ajusta la ruta)
df_raw <- read_excel("D:/RFSI paper/DatosSerie.xlsx", sheet = "Datos", skip = 11)

# 3. Renombrar columnas
names(df_raw) <- c("Año", "Día", "Ene", "Feb", "Mar", "Abr", "May", "Jun",
                   "Jul", "Ago", "Set", "Oct", "Nov", "Dic")

# 4. Limpiar
df_raw <- df_raw %>% filter(!is.na(Año) & !is.na(Día))
df_raw <- df_raw %>%
  mutate(Año = as.integer(Año),
         Día = as.integer(Día))

# 5. Convertir a formato largo (fecha, caudal)
df_long <- df_raw %>%
  pivot_longer(cols = Ene:Dic, names_to = "Mes", values_to = "Caudal_texto") %>%
  mutate(
    Mes_num = match(Mes, month.abb),
    Fecha = make_date(year = Año, month = Mes_num, day = Día)
  ) %>%
  select(Fecha, Caudal_texto) %>%
  arrange(Fecha)

# 6. Convertir a numérico (los no numéricos -> NA)
df_long <- df_long %>%
  mutate(Q_m3s = as.numeric(as.character(Caudal_texto))) %>%
  select(-Caudal_texto)

# ============================================================
# 7. DATOS PARA GR4J (DIARIO)
# ============================================================

# Crear dataframe diario con placeholders para Precipitación y PET
# Deberás reemplazar estas columnas con tus datos reales
df_gr4j <- df_long %>%
  mutate(
    Precip_mm = NA,    # <--- RELLENA CON PRECIPITACIÓN DIARIA (mm)
    PET_mm = NA        # <--- RELLENA CON EVAPOTRANSPIRACIÓN DIARIA (mm)
  ) %>%
  select(Fecha, Precip_mm, PET_mm, Q_m3s)

# Guardar para GR4J (diario)
write.csv(df_gr4j, "datos_GR4J_diario.csv", row.names = FALSE)
print("✅ Archivo 'datos_GR4J_diario.csv' creado (rellena Precip y PET)")

# ============================================================
# 8. DATOS PARA GR2M (MENSUAL) - PROMEDIO DE CAUDALES DIARIOS
# ============================================================

# Agrupar por mes y calcular el caudal medio mensual (en m³/s)
df_gr2m <- df_long %>%
  mutate(AnioMes = floor_date(Fecha, "month")) %>%
  group_by(AnioMes) %>%
  summarise(
    Q_m3s_mensual = mean(Q_m3s, na.rm = TRUE),   # PROMEDIO (no suma)
    N_dias_con_dato = sum(!is.na(Q_m3s))
  ) %>%
  ungroup() %>%
  # (Opcional) Filtrar meses con al menos 25 días de dato para mayor confiabilidad
  filter(N_dias_con_dato >= 25) %>%
  mutate(
    Precip_mm = NA,   # <--- RELLENA CON PRECIPITACIÓN MENSUAL (mm)
    PET_mm = NA       # <--- RELLENA CON PET MENSUAL (mm)
  ) %>%
  select(AnioMes, Precip_mm, PET_mm, Q_m3s_mensual)

# Guardar para GR2M (mensual)
write.csv(df_gr2m, "datos_GR2M_mensual.csv", row.names = FALSE)
print("✅ Archivo 'datos_GR2M_mensual.csv' creado (rellena Precip y PET)")

# ============================================================
# 9. VER LOS PRIMEROS REGISTROS
# ============================================================
print("📊 Vista previa - GR4J (diario):")
print(head(df_gr4j, 10))

print("📊 Vista previa - GR2M (mensual):")
print(head(df_gr2m, 10))
































# RFSI CRUDO

# ==============================================================================
# VALIDACIÓN CRUZADA ESPACIAL + GENERACIÓN DE GRÁFICO DE DENSIDAD (PDF)
# ==============================================================================
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

calcular_metricas <- function(obs, sim, label) {
  rmse  <- sqrt(mean((obs - sim)^2, na.rm = TRUE))
  r2    <- cor(obs, sim, use = "complete.obs")^2
  bias  <- (sum(sim) - sum(obs)) / sum(obs) * 100
  r_cor <- cor(obs, sim, use = "complete.obs")
  alpha <- sd(sim, na.rm = TRUE) / sd(obs, na.rm = TRUE)
  beta  <- mean(sim, na.rm = TRUE) / mean(obs, na.rm = TRUE)
  kge   <- 1 - sqrt((r_cor - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
  
  u <- 0.1
  hits <- sum(obs >= u & sim >= u, na.rm = TRUE)
  fals <- sum(obs < u & sim >= u, na.rm = TRUE)
  miss <- sum(obs >= u & sim < u, na.rm = TRUE)
  pod  <- hits / (hits + miss)
  far  <- fals / (hits + fals)
  
  return(data.frame(Fold = label, RMSE = rmse, R2 = r2, KGE = kge, BIAS = bias, POD = pod, FAR = far))
}

fm.RFSI <- as.formula("prcp ~ precland + tmax + tmin + demm + demsd + twi + slope + 
                      sin + cos + doy_sin + doy_cos + month")

umbral_mensual <- data %>%
  filter(prcp > 0) %>%
  group_by(month) %>%
  summarise(p95 = quantile(prcp, 0.95, na.rm = TRUE), .groups = 'drop')

res_cv <- data.frame()

# --- NUEVO: Tabla vacía para acumular las predicciones de TODOS los Folds ---
datos_para_grafico <- data.frame()

cat("\n[4] Ejecutando Validación Cruzada Espacial...")
set.seed(42) # Tu semilla ganadora fija

# Candado de proyección para eliminar los warnings de distancia
attr(data, "crs") <- "EPSG:32718"

for (f in 1:5) {
  cat(paste0("\n>>> Procesando Fold ", f, "..."))
  
  train_cv <- as.data.frame(data[data$fold_spatial != f, ])
  test_cv  <- as.data.frame(data[data$fold_spatial == f, ]) %>% filter(!is.na(prcp))
  
  # Forzar CRS en los subsets
  attr(train_cv, "crs") <- "EPSG:32718"
  attr(test_cv, "crs") <- "EPSG:32718"
  
  mod_cv <- rfsi(formula = fm.RFSI, data = train_cv, 
                 data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"), 
                 n.obs = 5, cpus = parallel::detectCores()-2, num.trees = 250)
  
  # FUNCIÓN CORREGIDA
  pred_cv <- meteo::pred.rfsi(model = mod_cv, data = train_cv, obs.col = "prcp",
                              data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"), 
                              newdata = test_cv, 
                              newdata.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"))
  
  test_cv$pred_raw <- pmax(0, pred_cv$pred) 
  test_cv$pred_pc  <- test_cv$pred_raw     
  
  fechas_unicas <- unique(test_cv$date)
  
  for (d in seq_along(fechas_unicas)) {
    fecha_act <- fechas_unicas[d]
    datos_cuenca_dia <- train_cv[train_cv$date == fecha_act, ]
    estaciones_con_lluvia <- sum(datos_cuenca_dia$prcp > 0, na.rm = TRUE)
    
    idx_dia_test <- which(test_cv$date == fecha_act)
    if (length(idx_dia_test) == 0) next
    
    # RESTRICCIÓN 1: Día Seco Regional
    if (estaciones_con_lluvia < 2) {
      test_cv$pred_pc[idx_dia_test] <- 0
    } else {
      # RESTRICCIÓN 2: Umbral Mínimo (FAR)
      umbral_min_dia <- max(0.5, quantile(datos_cuenca_dia$prcp, 0.10, na.rm = TRUE))
      valores_pred <- test_cv$pred_pc[idx_dia_test]
      valores_pred[valores_pred < umbral_min_dia] <- 0
      
      # RESTRICCIÓN 3: Umbral Máximo Dinámico (BIAS)
      mes_act <- unique(test_cv$month[idx_dia_test])[1]
      umbral_max_clima <- umbral_mensual$p95[umbral_mensual$month == mes_act]
      
      valores_pred[valores_pred > umbral_max_clima] <- umbral_max_clima
      test_cv$pred_pc[idx_dia_test] <- valores_pred
    }
  }
  
  res_cv <- rbind(res_cv, calcular_metricas(test_cv$prcp, test_cv$pred_pc, paste0("Fold ", f)))
  
  columnas_grafico <- test_cv %>% select(prcp, pred_raw, pred_pc)
  datos_para_grafico <- rbind(datos_para_grafico, columnas_grafico)
}

# Imprimir cuadro numérico final (El de KGE 0.2491)
print(rbind(res_cv, res_cv %>% summarise(Fold = "AVERAGE", across(RMSE:FAR, mean, na.rm = TRUE))))

# ==============================================================================
# PROCESAMIENTO DEL GRÁFICO CON TODOS LOS FOLDS ACUMULADOS
# ==============================================================================
cat("\n[GRAFICANDO] Construyendo Función de Densidad de Probabilidad (PDF)...")

df_pdf <- datos_para_grafico %>%
  # Pasamos a formato largo (long format) para ggplot
  pivot_longer(cols = everything(), names_to = "Modelo", values_to = "Precipitacion") %>%
  filter(Precipitacion > 0.1) %>% # Filtro mínimo para evitar ruido en el logaritmo
  mutate(Modelo = case_when(
    Modelo == "prcp" ~ "Estaciones (Observado)",
    Modelo == "pred_raw" ~ "RFSI Crudo (Sin Filtro)",
    Modelo == "pred_pc" ~ "PC-RFSI (Con Filtro Físico)"
  ))

# Construir el gráfico estadístico de alta calidad
grafico_pdf <- ggplot(df_pdf, aes(x = Precipitacion, color = Modelo, fill = Modelo)) +
  geom_density(alpha = 0.08, linewidth = 1.2) +
  # Escala logarítmica en X
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100),
                labels = c("0.5", "1", "2", "5", "10", "20", "50", "100")) +
  # Paleta de colores elegante y académica
  scale_color_manual(values = c("Estaciones (Observado)" = "#222222", 
                                "RFSI Crudo (Sin Filtro)" = "#E41A1C", 
                                "PC-RFSI (Con Filtro Físico)" = "#377EB8")) +
  scale_fill_manual(values = c("Estaciones (Observado)" = "#222222", 
                               "RFSI Crudo (Sin Filtro)" = "#E41A1C", 
                               "PC-RFSI (Con Filtro Físico)" = "#377EB8")) +
  labs(
    title = "Función de Densidad de Probabilidad (PDF) de la Precipitación Diaria",
    subtitle = "Validación Cruzada Espacial en la Cuenca del Huallaga",
    x = "Precipitación Diaria (mm/día) - Escala Logarítmica",
    y = "Densidad de Probabilidad",
    color = "Fuente de Datos",
    fill = "Fuente de Datos"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Mostrar el gráfico en la pestaña Plots de RStudio
print(grafico_pdf)
# ==============================================================================
# ==================== RFSI VERSION 1: ORIGINAL (CRUDO) =======================
# ==============================================================================
cat("\n[INICIO] Ejecutando Proceso para RFSI Original...")

# --- FÓRMULA PRINCIPAL ---
fm.RFSI <- as.formula("prcp ~ precland + tmax + tmin + demmav + demsd + demav + twi + slope + 
                      sin + cos + doy_sin + doy_cos + month")

# --- FUNCIÓN DE MÉTRICAS ---
calcular_metricas <- function(obs, sim, label) {
  rmse  <- sqrt(mean((obs - sim)^2, na.rm = TRUE))
  r2    <- cor(obs, sim, use = "complete.obs")^2
  bias  <- (sum(sim) - sum(obs)) / sum(obs) * 100
  r_cor <- cor(obs, sim, use = "complete.obs")
  alpha <- sd(sim, na.rm = TRUE) / sd(obs, na.rm = TRUE)
  beta  <- mean(sim, na.rm = TRUE) / mean(obs, na.rm = TRUE)
  kge   <- 1 - sqrt((r_cor - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
  
  u <- 0.1
  hits <- sum(obs >= u & sim >= u, na.rm = TRUE)
  fals <- sum(obs < u & sim >= u, na.rm = TRUE)
  miss <- sum(obs >= u & sim < u, na.rm = TRUE)
  pod  <- hits / (hits + miss)
  far  <- fals / (hits + fals)
  
  return(data.frame(Fold = label, RMSE = rmse, R2 = r2, KGE = kge, BIAS = bias, POD = pod, FAR = far))
}

# --- VALIDACIÓN CRUZADA ESPACIAL (ORIGINAL) ---
res_cv_original <- data.frame()
cat("\n>>> Corriendo Validación Cruzada Espacial RFSI Original...")

set.seed(42)
for (f in 1:5) {
  cat(paste0("\nProcesando Fold ", f, "..."))
  train_cv <- as.data.frame(data[data$fold_spatial != f, ])
  test_cv  <- as.data.frame(data[data$fold_spatial == f, ]) %>% filter(!is.na(prcp))
  
  mod_cv <- rfsi(formula = fm.RFSI, data = train_cv, 
                 data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"), 
                 n.obs = 5, cpus = parallel::detectCores()-2, num.trees = 250)
  
  pred_cv <- meteo::pred.rfsi(model = mod_cv, data = train_cv, obs.col = "prcp",
                              data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"), 
                              newdata = test_cv, 
                              newdata.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"))
  
  pred_original <- pmax(0, pred_cv$pred)
  res_cv_original <- rbind(res_cv_original, calcular_metricas(test_cv$prcp, pred_original, paste0("Fold ", f)))
}


final_original <- rbind(res_cv_original, res_cv_original %>% summarise(Fold = "AVERAGE", across(RMSE:FAR, mean, na.rm = TRUE)))
#write.csv(final_original, paste0(dir_salida, "CV_Metricas_RFSI_Original.csv"), row.names = FALSE)
print(final_original)

# --- ENTRENAMIENTO MODELO FINAL ---
cat("\n>>> Entrenando modelo RFSI Original definitivo...")
set.seed(42)
rfsi_final_orig <- rfsi(formula = fm.RFSI, data = as.data.frame(data),
                        data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
                        n.obs = 5, cpus = parallel::detectCores()-2, num.trees = 250, mtry = 5)

# --- CONFIGURACIÓN PARALELA PARA MAPAS ORIGINALES ---
path_p <- sources(r_precland_utm)
path_tx <- sources(r_tmax_utm)
path_tn <- sources(r_tmin_utm)

library(doSNOW)
n_cores <- parallel::detectCores() - 2
cl <- makeSOCKcluster(n_cores)
registerDoSNOW(cl)

años_procesar <- 2000:2015 
pb <- txtProgressBar(max = length(años_procesar), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

cat(paste0("\n>>> Generando Mapas RFSI Originales...\n"))
foreach(a = 1:length(años_procesar), .packages = c("terra", "dplyr", "meteo", "lubridate"), 
        .options.snow = opts, .errorhandling = 'pass') %dopar% {
          
          anio_actual <- años_procesar[a]
          fechas_totales <- seq(as.Date(paste0(anio_actual, "-01-01")), as.Date(paste0(anio_actual, "-12-31")), by="day")
          
          rp_core  <- rast(path_p)
          rtx_core <- rast(path_tx)
          rtn_core <- rast(path_tn)
          capas_anio <- list()
          
          for(f in 1:length(fechas_totales)) {
            fecha_actual <- fechas_totales[f]
            fecha_inicio_raster <- as.Date("2000-01-01")
            idx <- as.numeric(fecha_actual - fecha_inicio_raster) + 1
            
            if (idx > 0 && idx <= nlyr(rp_core)) {
              gv <- vect(grid_cells[, c("x", "y")], geom = c("x", "y"), crs = "EPSG:32718")
              temp_grid <- grid_cells
              temp_grid$precland <- terra::extract(rp_core[[idx]], gv)[,2]
              temp_grid$tmax     <- terra::extract(rtx_core[[idx]], gv)[,2]
              temp_grid$tmin     <- terra::extract(rtn_core[[idx]], gv)[,2]
              
              grid_pred <- temp_grid %>%
                mutate(staid = 1:n(), z = as.numeric(fecha_actual - min(data$date)),
                       month = month(fecha_actual), doy = yday(fecha_actual),
                       doy_sin = sin(2 * pi * doy / 365.25), doy_cos = cos(2 * pi * doy / 365.25)) %>% na.omit()
              
              pred_rfsi <- meteo::pred.rfsi(model = rfsi_final_orig, data = as.data.frame(data), obs.col = "prcp",
                                            data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
                                            newdata = grid_pred, newdata.staid.x.y.z = c("staid", "x", "y", "z"))
              
              valores_original <- pmax(0, pred_rfsi$pred)
              r_orig <- rast(cbind(grid_pred[, c("x", "y")], valores_original), type = "xyz", crs = "EPSG:32718")
              names(r_orig) <- paste0("d_", format(fecha_actual, "%Y%m%d"))
              capas_anio[[f]] <- r_orig
            }
          }
          capas_anio <- capas_anio[!sapply(capas_anio, is.null)]
          if(length(capas_anio) > 0) {
            raster_anual <- rast(capas_anio)
            writeRaster(raster_anual, paste0(dir_salida, "rfsi_original_anual_", anio_actual, ".tif"), overwrite=TRUE, gdal=c("COMPRESS=LZW"))
          }
        }
close(pb)
stopCluster(cl)
cat("\n[FIN] RFSI Original completado.\n")


#
# ==============================================================================
# CONFIGURACIÓN DE LIBRERÍAS DE ALTO IMPACTO
# ==============================================================================
library(ggplot2)
library(dplyr)
library(patchwork) # El motor para fusionar los paneles A y B
library(terra)
library(blockCV)

# ==============================================================================
# PANEL A: IMPORTANCIA DE VARIABLES (OPTIMIZADO CON TUS NUEVAS VARIABLES)
# ==============================================================================
cat("Usando", n_cores, "núcleos de CPU\n")

set.seed(42)

# OPCIÓN RECOMENDADA: Sin especificar CRS (más simple)
rfsi_model <- rfsi(
  formula = fm.RFSI,
  data = as.data.frame(data),
  data.staid.x.y.z = c("staid", "x_utm", "y_utm", "z"),
  n.obs = 7,
  soil3d = FALSE,
  # ¡NO incluir s.crs o p.crs!
  cpus = parallel::detectCores()-2,
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
# 1. Extraer y ordenar la importancia cruda de tu modelo
# ==============================================================================
# 1. PREPARACIÓN DE DATOS (MANTENIENDO EL TOP 22 COMPLETO Y HONESTO)
# ==============================================================================
# ==============================================================================
# 1. PREPARACIÓN DE DATOS (TOP 15 ESTRICTO)
# ==============================================================================
lista_importancia <- round(rfsi_model$variable.importance)
lista_ordenada <- lista_importancia[order(unlist(lista_importancia), decreasing = TRUE)]

df_completo <- data.frame(
  covariate = names(lista_ordenada),
  importance_raw = as.numeric(lista_ordenada),
  stringsAsFactors = FALSE
)

df_mio <- head(df_completo, 15) 
df_mio <- df_mio %>% mutate(importance = importance_raw / max(importance_raw))

df_mio <- df_mio %>%
  mutate(covariate = case_when(
    covariate == "precland" ~ "ERA5LAND",
    covariate == "tmax"     ~ "TMAX",
    covariate == "tmin"     ~ "TMIN",
    covariate == "demav"    ~ "DEMAV",
    covariate == "demmav"   ~ "DEMMAV",
    covariate == "demsd"    ~ "DEMSD",
    covariate == "slope"    ~ "SLOPE",
    covariate == "twi"      ~ "TWI",
    covariate == "month"    ~ "MONTH",
    covariate == "doy_cos"  ~ "DOY_COS",
    covariate == "doy_sin"  ~ "DOY_SIN",
    TRUE ~ covariate 
  ))

variables_resaltadas <- c("DEMAV", "DEMMAV", "DEMSD", "ERA5LAND", "TMAX", "TMIN")

df_mio <- df_mio %>%
  mutate(Group = ifelse(covariate %in% variables_resaltadas, 
                        "Key Topo-Climatic Predictors", 
                        "Spatial Autocorrelation Components")) %>%
  arrange(importance) %>%
  mutate(covariate = factor(covariate, levels = unique(covariate)))

# ==============================================================================
# 2. PANEL A: IMPORTANCIA (CON MARGEN AJUSTADO PARA UNIR)
# ==============================================================================
plot_importance <- ggplot(df_mio, aes(x = covariate, y = importance, color = Group)) +
  geom_segment(aes(x = covariate, xend = covariate, y = 0, yend = importance), 
               linewidth = ifelse(df_mio$covariate %in% variables_resaltadas, 0.9, 0.4),
               show.legend = FALSE) +
  geom_point(size = ifelse(df_mio$covariate %in% variables_resaltadas, 3.5, 2.2)) +
  coord_flip() +
  scale_color_manual(values = c("Key Topo-Climatic Predictors" = "#B22222", 
                                "Spatial Autocorrelation Components" = "#2F4F4F")) + 
  labs(x = "Predictor Covariates", y = "Relative Importance Score (0.0 - 1.0)", color = "Feature Category:") +
  theme_bw(base_size = 11) +
  theme(
    axis.title = element_text(face = "bold", size = 11, color = "#111111"),
    axis.text = element_text(color = "black", size = 9, face = "bold"), 
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray96", linewidth = 0.5),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.5),
    legend.position = "bottom",
    # TRUCO 1: Reducir el margen derecho del Panel A para acercarlo al Panel B
    plot.margin = margin(t = 5, r = -10, b = 5, l = 5, unit = "pt"),
    panel.border = element_rect(color = "gray40", fill = NA, linewidth = 0.8)
  )

# ==============================================================================
# 3. PANEL B: BLOCKCV (CON MARGEN AJUSTADO PARA UNIR)
# ==============================================================================
plot_cv_raw <- cv_plot(cv = sb2, r = r_clima[["demav"]], x = pa_data)

plot_blockcv <- plot_cv_raw + 
  labs(x = "Longitude", y = "Latitude", fill = "Validation Folds:") +
  theme_bw(base_size = 11) +
  theme(
    axis.title = element_text(face = "bold", size = 11, color = "#111111"),
    axis.text = element_text(color = "black", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5), 
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray93", color = "gray40"), 
    strip.text = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    # TRUCO 2: Reducir el margen izquierdo del Panel B para pegarlo al Panel A
    plot.margin = margin(t = 5, r = -10, b = 5, l = -10, unit = "pt"),
    panel.border = element_rect(color = "gray40", fill = NA, linewidth = 0.8)
  )

# ==============================================================================
# 4. ENSAMBLE COMPACTO CON PATCHWORK
# ==============================================================================
figura_compacta <- (plot_importance + plot_blockcv) + 
  # TRUCO 3: Proporción estrecha y recolección de leyendas para evitar vacíos
  plot_layout(widths = c(1, 1.1), guides = "collect") + 
  plot_annotation(
    title = "Spatio-Temporal Model Architecture and Validation Framework",
    subtitle = "Huallaga River Basin Case Study — Precipitation Downscaling",
    tag_levels = 'A',
    theme = theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5, color = "#111111"),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40", face = "italic"),
      plot.tag = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )
  )

# Mostrar directo en la pestaña Plots de RStudio
print(figura_compacta)

cv_spatial_autocor(
  r = r_base, # a SpatRaster object or path to files
  num_sample = 5000, # number of cells to be used
  plot = TRUE
)
































library(blockCV)
library(sf) # working with spatial vector data
library(terra) # working with spatial raster data
library(tmap) # plotting maps

# 1. Rutas y archivos
input_path <- "D:/R/RFSI paper/BlockVC/raster/"
output_path <- "D:/R/RFSI paper/BlockVC/raster_5km/" # Carpeta nueva para no sobreescribir
if (!dir.exists(output_path)) dir.create(output_path)

files <- list.files(input_path, pattern = "\\.tif$", full.names = TRUE)
files_static <- files[grep("dem|slope|cos|sin|twi", files)]
ref_5km <- rast(paste0(input_path, "precland.tif"))

# 2. Procesar, Resamplear con AVERAGE y Guardar
static_list <- lapply(files_static, function(x) {
  r <- rast(x)
  nombre_archivo <- basename(x)
  
  # Si la geometría no coincide, resampleamos
  if (!compareGeom(r, ref_5km, stopOnError = FALSE)) {
    cat("Resampleando (average):", nombre_archivo, "\n")
    
    # IMPORTANTE: Usamos method = 'average' para conservar la masa/promedio
    r_5km <- resample(r, ref_5km, method = "average")
    
    # 3. Guardar el archivo físicamente
    writeRaster(r_5km, 
                filename = paste0(output_path, gsub(".tif", "_5km.tif", nombre_archivo)), 
                overwrite = TRUE)
    
    return(r_5km)
  } else {
    return(r)
  }
})

# 4. Crear el stack con los nuevos rasters
rasters <- terra::rast(static_list)
names(rasters) <- c("cos", "dem", "sin", "slope", "twi") # Asegúrate que el orden coincida
