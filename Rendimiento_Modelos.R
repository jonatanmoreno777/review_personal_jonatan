######################################################################################
#######################Comparación PISCO-models-CORDEX################################

######################################################################################

setwd("D:/descarga _esgf/ObservadoPISCO_Simulado_GCM/RCM_Pisco_PT/")
getwd()
ls()
rm(list=ls())

#cargar el paquete
pacman::p_load(xts, ggplot2, hydroTSM, zoo, 
               hydroGOF)

day <- read.csv("GCM_Model_PT.csv" ,head = T,check.names = F,stringsAsFactors = F)

my.date = as.Date("1981/1/1")####para no obtener "0001-01-19"
my.date 
as.Date("1/1/1981", format="%m/%d/%Y") #########crear mi formato fecha
time = seq(as.Date("1981/1/1"), as.Date("2004/12/31"), "days")

#day
PISCO_SENAMHI = zoo(x=day$PISCO_SENAMHI, order.by=time)
SAM44_MIROC5_RCA4_v3 = zoo(x=day$SAM44_MIROC5_RCA4_v3, order.by=time)

ggof(sim=SAM44_MIROC5_RCA4_v3, obs=PISCO_SENAMHI, ftype="dm", FUN=mean,xlab = "Time",ylab=c("pr, [mm]"),
     gofs=c("MAE", "RMSE", "PBIAS","KGE","NSE","R2","r"),col = c("red", "blue")) #

r <- SAM44_MIROC5_RCA4_v3-PISCO_SENAMHI
library(hydroTSM)
smry(r)
# daily, monthly and annual plots, boxplots and histograms
hydroplot(r, FUN=mean)

ggof(sim=day$'SAM44_MIROC5_RCA4_v3', obs=day$PISCO_SENAMHI,
     pt.style = "ts", ftype = "o", FUN,
     stype="default",
     gof.leg = TRUE, digits=2,
     gofs=c("MAE", "RMSE", "PBIAS","KGE","NSE","R2", #gofs=c("ME", "MAE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE",
            "r"), #"rNSE", "d", "md", "rd", "r", "R2", "bR2", "KGE", "VE"),
     col = c("red", "black"),
     main="", #Provide the chart title here
     xlab = "Tiempo (meses)", #Change the x-axis title here
     ylab=c("pr, [mm/mes]"), #Change the y-axis title here  #ylab=c("Q, [m3/s]")
     lwd = c(2, 1), #To change the line width
     cex = c(1, 2))

