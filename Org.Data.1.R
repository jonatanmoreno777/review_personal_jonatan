
Org.M.Data <- function(X, Y, MP,
                       start_date = "1981-01-01",# end of grilled data .nc month
                       y_from = 1991, y_to = 2012) { #  time of Obs data e.g station Huanta
  # Si MP es SpatRaster, conviértelo
  if (inherits(MP, "SpatRaster")) MP <- raster::brick(MP)
  
  # Punto
  punto <- data.frame(x = X, y = Y)
  sp::coordinates(punto) <- ~ x + y
  raster::crs(punto) <- raster::crs(MP)
  
  # EXTRAER con la versión de raster
  valores <- raster::extract(MP, punto)
  
  # Fechas del mismo largo que las capas
  fechas <- seq(as.Date(start_date), by = "month", length.out = raster::nlayers(MP))
  
  df_full <- data.frame(Time = fechas, Data = as.numeric(valores))
  df_filtrado <- subset(df_full,
                        as.numeric(format(Time, "%Y")) >= y_from &
                          as.numeric(format(Time, "%Y")) <= y_to)
  
  DataMonth1 <- df_filtrado
  Years  <- as.numeric(format(df_filtrado$Time, "%Y"))
  Meses  <- format(df_filtrado$Time, "%b")
  DataMonth2 <- data.frame(Years = Years, variable = Meses, value = df_filtrado$Data)
  DataMonth3 <- data.frame(Id = seq_len(nrow(DataMonth2)),
                           Months = DataMonth2$variable,
                           value = DataMonth2$value)
  
  list(DataMonth1, DataMonth2, DataMonth3)
}




