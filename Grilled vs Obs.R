
library(raster)
library(rgdal)
library(sp)
library(cowplot)
library(ggpubr)
library(tidyr)
library(dplyr)
library(terra)

setwd("D:/T_JONA/TESIS_PISCO/Entrada/Pisco_Pp")
M <- brick("PISCOpm.nc", varname = "variable") 
writeRaster(M, filename = "PISCOpm.tif", format = "GTiff", overwrite = TRUE)

MP        <- brick("RAIN4PEm.tif") # brick("PISCOpm.tif")   #RAIN4PEm.tif

Data.Obs  <- read.table("Data.Obs.txt", dec = ".", header = FALSE)

# Convertion of lambert coordinates to geographic coordinates
Shp       <- readOGR("Shp/PERU.shp")
shp_utm <- spTransform(Shp, CRS("+init=epsg:32718"))

C.Station <- data.frame(x=557047.25, y=8517093.71)
coordinates(C.Station) <-   ~x + y
proj4string(C.Station) <- proj4string(shp_utm)
NC.Station             <- spTransform(C.Station, CRS(proj4string(MP)))
as.data.frame(NC.Station)

#Data preparation
X          <- -74.47306
Y          <- -13.41333

# Funtions of organize monthy precipitation data
source("D:/T_JONA/TESIS_PISCO/Entrada/Pisco_Pp/Org.Data.1.r")# run frist
source("D:/T_JONA/TESIS_PISCO/Entrada/Pisco_Pp/Org.Data.2.r")
DataMonths <-  Org.M.Data(X,Y,MP)  # of run close the window and end again
DataMonth1 <-  DataMonths[[1]]
DataMonth2 <-  DataMonths[[2]]
DataMonth3 <-  DataMonths[[3]]


rownames(Data.Obs) <- 1991:2012  # time Obs data
colnames(Data.Obs) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

Data.Obs.Long <- Data.Obs %>%
  as.data.frame() %>%
  mutate(Year = as.numeric(rownames(.))) %>%
  pivot_longer(cols = -Year, names_to = "Months", values_to = "P.Obs") %>%
  mutate(
    MonthNum = match(Months, month.abb),
    Time = as.Date(paste0(Year, "-", MonthNum, "-01")),
    Id = row_number()
  ) %>%
  select(Time, Id, Months, P.Obs)

Data.Ext   <- data.frame(Time=DataMonth1[,1], DataMonth3)
Data.Tot <- OrG.Data2(Data.Obs.Long, Data.Ext)

ggplot(Data.Tot,aes(x=P.Obs,y=P.Ext), shape=21,size=1)+
  geom_point()+
  xlab("")+
  ylab("")+
  ggtitle("")+
  theme_bw()+
  geom_smooth(metho=lm,se=T,fullranfe=T)+
  stat_cor(method = "pearson")

ggplot(Data.Tot,aes(x=P.Obs,y=P.Ext, color=Months), shape=21,size=1)+
  geom_point()+
  facet_wrap(~reorder(Months,Id),ncol = 3)+
  xlab("Obs")+
  ylab("Grilled Rain4pe uncorrected")+
  ggtitle("")+
  theme_bw()+
  geom_smooth(method=lm,se=F,fullrange=T)+
  stat_cor(method = "pearson",label.x=0, label.y=150)


ggplot(Data.Tot,aes(x=P.Obs,y=P.Ext, color=season), shape=21,size=1)+
  geom_point()+
  facet_wrap(~reorder(season,Id),ncol = 6)+
  xlab("Observed rainfall [mm]")+
  ylab("Extrated rainfall ¨[mm]")+
  ggtitle("Observed rainfall vs Extrated rainfall")+
  theme_bw()+
  geom_smooth(method=lm,se=F,fullrange=T)+
  stat_cor(method = "pearson",label.x=0, label.y=150)


