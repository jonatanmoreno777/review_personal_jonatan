

OrG.Data2 <- function(Data.Obs.Long, Data.Ext) {
  # Asegurarse que Time es Date
  Data.Obs.Long$Time <- as.Date(Data.Obs.Long$Time)
  Data.Ext$Time <- as.Date(Data.Ext$Time)
  
  # Fusionamos
  Data.Tot <- merge(Data.Ext, Data.Obs.Long, by = c("Time", "Months"), suffixes = c(".Ext", ".Obs"))
  
  # Asignamos temporada
  Data.Tot$season <- sapply(format(Data.Tot$Time, "%m"), function(m) {
    m <- as.numeric(m)
    if (m %in% c(12,1,2)) "Winter"
    else if (m %in% c(3,4,5)) "Spring"
    else if (m %in% c(6,7,8)) "Summer"
    else "Autumn"
  })

  # Reordenamos columnas
  Data.Tot <- Data.Tot[, c("season", "Time", "Id.Ext", "Months", "value", "P.Obs")]
  colnames(Data.Tot) <- c("season", "Time", "Id", "Months", "P.Ext", "P.Obs")
  
  return(Data.Tot)
}