
setwd("D:/T_JONA/TESIS_PISCO/Salida/")
getwd()
ls()
rm(list=ls())

#https://rstudio-pubs-static.s3.amazonaws.com/279440_9c7091b89f07486d83e3d6b9c55280f0.html

library(pacman)#cargar el paquete
pacman::p_load(xts, ggplot2, hidroTSM, ZOO, latticeExtra, 
       laticce,dygraph,quantmod,TSstudio)

ipak <- function(pkg){
  new.pkg <- pkg[(pkg %in% installed.packages()[,"package"])]
  if (length(new.pkg))
    installed.packages(new.pkg, dependencies=T)
  sapply(pkg, require, character.only=T)
}

##mportar archvo csv
data <- read.csv("Pisco_pd.csv" , head = T,check.names = F,stringsAsFactors = F)
plot(data[,3],type="o", col= "blue",
     ylab="Prec. [mm]",
     xlab = "day", main="Prec. prom areal - Cuenca Cachi [mm]")
legend(900, 55, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8,
       box.lty=2, box.lwd=2, box.col="green")
data
dim(data)
length(data)
str(data)
summary(data)
library(xts)
# Crear objeto xts
idx <- as.Date(data[,1])
data.matrix <- data[,-1]
data.xts <- xts(data.matrix, order.by = idx )
str(data.xts)

#plot.xts
plot(data.xts)
plot(data.matrix[,1], type="l")
#grafco con dygraph
#https://nhsrcommunity.com/blog/dygraphs/
##e6ffff
library(dygraphs)
dygraph(data.xts[,2:4],main = "pp diaria de estaciones")%>% 
  dyRoller(showRoller = TRUE,rollPeriod = 20)%>% 
  dyHighlight(highlightSeriesBackgroundAlpha = 2,
              hideOnMouseOut = TRUE )%>%
  dyRangeSelector(height = 100, strokeColor = "BLUE")%>%#ALTURA BLUE
  dyAxis("y", drawGrid = T,label = "pp (mm/dia)", valueRange = c(0, 15) )%>%
  dyAxis("y2", valueRange = c(0, 15))%>%
  dyAxis("x", drawGrid = T,label = "A?os")%>%
# Add shading for the recessionary period
  dyShading(from = "1985-01-01", to = "1986-10-09", color = "#fff1e6")%>%
  dyShading(from = "2007-01-01", to = "2008-10-09", color = "#fae6ff")%>%
  # Add an event for the financial crisis. 
  dyEvent(x = "1986-03-20", label = "Evento Maximo", labelLoc = "top", color = "red")%>%
  dyEvent(x = "2008-01-20", label = "Evento Mnmo", labelLoc = "top", color = "green")%>%
  #dyRebase(value = 0.4, percent = F)
  dyEvent(x = "2000-01-20"," fn calbraton",labelLoc = "top")%>%
  dyLimit(10, label = "PP 8mm/day", color = "red")
  #dyLegend(data.xts[,2:4],show = c("huamanga","huanta","chiara"),width=250,
           #howZeroValues=T, labelsDiv = NULL,labelsSeparateLines = FALSE,
           #hideOnMouseOut = T)

#convertra un objeto zoo
data.zoo <- as.zoo(data.xts)
data.zoo 
str(data.zoo )
##plot zooo
plot(data.zoo, main = "Hstograma de precptacone de las estacones",xlab = "A?o")
#confguracon de escala de eje para toda la sere temporal
summary(data.zoo)
max(data.zoo, na.rm = T)
max(data.zoo)
plot(data.zoo, main = "Hstograma de precptacone de las estacones",
     ylim= c(0,110))

#plot xts con latcce
xyplot(data.xts, xlab="fecha", ylab="precptacon(mm/day)",
       main = "Hstograma de precptacone de las estacones",ylim= c(0,110) )
#plot con ggplot
autoplot(data.xts[,1:4]) +theme_bw()+xlab('Fecha')+ylab('Precipitaci?n [mm/dia]')+
  geom_point(shape=1,color="red",fill="red")+
  geom_bar(stat = "identity",width=5,color="light blue")+
  geom_line(size=.5,lty=2,colour="blue")

##3convertr a datos mensuales y maxmensuales

#de day a mensual

data.monthly <- apply.monthly(data.xts, FUN=sum)#fue corecta la acumlacon?
plot(data.monthly)
data.monthly <- apply.monthly(data.xts[,1], FUN=sum)#fue corecta la acumlacon?
plot(data.monthly)
data.monthly <- apply.monthly(data.xts[,1], FUN=apply, MARGIN=2, sum)#fue corecta la acumlacon?
plot(data.monthly)
str(data.monthly)
max(data.monthly)
xyplot(data.monthly, ylim=c(0,245))
##grafcos en chartsseres
ts_seasonal(data.monthly, type = "normal")
ts_seasonal(data.monthly, type = "cycle")
ts_seasonal(data.monthly, type = "box")
ts_seasonal(data.monthly, type = "all")
##grafcos en chartsseres
chartSeries(
  data.monthly,
  theme = chartTheme("white"),
  TA = c(addBBands(),addTA(RSI(data.monthly)))
)
# also easy zooming
zoomChart("1990::")

#hacer varos grafcos de data montly

SUM <- function(a,n){
  count <- sum(is.na(a))#cantdad de datos perddos
  if (count <- n){
    tot <- sum(a, n.rm=T)
  } else tot <- NA
  return(tot)
}

SUM(a=c(1:28, NA, NA), n=2)
data.monthly <- apply.monthly(data.xts, FUN=apply, MARGIN=2, sum, n=3)
xyplot(data.monthly, ylim=c(0,245))
data.monthly
write.csv((data.monthly), "Datos_mensuales.csv")

###convertr los datos de vertcal a horzontal
datos <- read.csv("Datos_mensuales.csv")
datos
str(datos)
#estaconn_ro1
dat1 <- datos[,2]#escoger la columna donde se ubca la estaco
madat1 <- t(matrix(dat1, nrow = 12))

#cambando los nombres de columnas y flas
colnames(madat1) <- c("ENE", "FEB","MAR","APR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC")
rownames(madat1) <- 1981:2016 #COLOCAR EL PERODO
write.csv(madat1, "1.PPMM_HUAMANGA.csv")

#3CALCULAMOS LOS MAX MENSUALES

data.max.mensual <- apply.monthly(data.xts, FUN=apply, 2, max)
xyplot(data.max.mensual)
data.max.mensual
write.csv((data.max.mensual), "dat_max_mensuals.csv")

###convertr los datos de vertcal a horzontal
datosmax <- read.csv("dat_max_mensuals.csv")
datosmax
str(datosmax)
#estaconn_ro1
dat1 <- datosmax[,2]#escoger la columna donde se ubca la estaco
madat1 <- t(matrix(dat1, nrow = 12))

#cambando los nombres de columnas y flas
colnames(madat1) <- c("ENE", "FEB","MAR","APR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC")
rownames(madat1) <- 1981:2016 #COLOCAR EL PERODO
write.csv(madat1, "1.PPMax_HUAMANGA.csv")

##convertmos datos mensual a anual

data.anual <- apply.yearly(data.monthly, FUN=apply, 2,sum)
xyplot(data.anual)
str(data.anual)
as.matrix.POSIXlt(data.anual)
anual <- as.matrix(data.anual)
str(anual)
row.names(anual) <- 1981:2016#colocar el perodo
anual
write.csv(anual, "data_anual.csv")

colnames(data.anual) <- c("allpachaca","huamanga")

##determnar los maxmos anuales
data.max.anual <- apply.yearly(data.xts, FUN=apply, 2,max)
xyplot(data.max.anual)
str(data.max.anual)
as.matrix.POSIXlt(data.max.anual)
anualmax <- as.matrix(data.max.anual)
str(anualmax)
row.names(anualmax) <- 1981:2016
anualmax
write.csv(anualmax, "data_anual_max.csv")

##analss de datos

#grafco de sere de tempo
max(data.xts)
xyplot(data.xts, xlab="fecha",ylab="precp(mm/day)",ylim=c(0,110))#sere day
xyplot(data.monthly,ylim=c(0,245))#mensual
max(data.anual)
xyplot(data.anual,ylim=c(0,1260))

#dagrama de boxplot
boxplot(coredata(data.xts))#datos daros de estacon
boxplot(coredata(data.monthly))

#estaconaldad de lluva
boxplot(matrix(coredata(data.monthly[,1]),nrow = nrow(data.monthly)/12, ncol = 12, byrow = T),
        col = "red", main=c(paste(names(data.monthly[,1])),"precptacon_mensual"))

#grafco de hstograma
hist(coredata(data.xts[,1]), freq = T)# cantidad de datos por clase 
histogram(coredata(data.xts[,1]))# porcentaje de datos por clase

#Grafico de datos disponibles por estacion y anio
y.count<-function(x){
  count.na <- sum(is.na(x))
  value <- 1
  if (count.na >= 1){
    value <- NA
  }
  return(value)
}

y.count(c(1,NA,3,4,5))
y.count(c(1,NA,NA,4,5))
y.count(c(1,2,3,4,5))
anios.data <- apply.yearly(data.monthly, FUN = apply, MARGIN = 2, y.count)
anios.data
xyplot(anios.data, superpose = TRUE)
for (i in 1:ncol(anios.data)){
  anios.data[,i][anios.data[,i]==1] <- i*2
}
anios.data
xyplot(anios.data, 
       superpose = TRUE,
       scales=list(y = list(at = seq(2,24,2), labels = names(anios.data)),#12estacones=24/2
                   x = list(at = index(anios.data), labels = 1981:2016,rot = 90)),
       col="black",
       type="o",
       pch=16,
       cex=2,
       auto.key = F,
       xlab="A?o",
       ylab="Estacion"
)

library(hydroTSM)
hydroplot(as.zoo(data.xts[,1]), var.type="Precipitation", main="at San Martino",
          pfreq = "dm", from="1981-01-01")

hydroplot(as.zoo(data.xts[,1]), var.type="Precipitation", pfreq = "dma", ylab = "Prec")
hydroplot(as.zoo(data.monthly[,1]), var.type="Precipitation", pfreq = "ma", ylab = "Prec")
#  ------------------------------------------------------------------------
# Completacion de datos--------------------------------------------
#  ------------------------------------------------------------------------
# Completando datos mediante cutoffR
# Revisar: Feng, L., Nowak, G., O'Neill, T.J., & Welsh, A.H. (2014). CUTOFF: A spatio-temporal imputation method. Journal of Hydrology, 519(Part D), 3591-3605.
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
library(corrplot)
library(cutoffR)
# Correlacion cruzada
cor.cruzada.mensual <- cor(as.matrix(data.monthly),use="complete")

cor.test(data.monthly[,1],data.monthly[,2])
summary(cor.cruzada.mensual)
min(cor.cruzada.mensual)
corrplot(cor.cruzada.mensual, method="circle",type = c("lower"),mar = c(4, 2, 3, 4))
corrplot(cor.cruzada.mensual, method="color",type = c("lower"),mar = c(4, 2, 3, 4))
corrplot(cor.cruzada.mensual, method="number",type = c("lower"),mar = c(4, 2, 3, 4))
data.cutoff <- data.frame(data.monthly,date=index(data.monthly),check.names = FALSE)
#data.cutoff.comp <- cutoff(data = data.cutoff)
data.cutoff.comp <- cutoff(data = data.cutoff,method = c("correlation"),corr = "spearman",cutoff = 0.7)# para tmin:cutoff = 0.8, tmax:cutoff = 0.7
data.mensual.comp <- as.xts(data.cutoff.comp,order.by = index(data.monthly))

xyplot(data.monthly,ylim=c(0,600))# mensual
xyplot(data.mensual.comp,ylim=c(0,600))# mensual
library(latticeExtra)
st.sin <- xyplot(data.monthly,ylim=c(0,600),col="red", lwd=2)# mensual
st.com <- xyplot(data.mensual.comp,ylim=c(0,600),col="blue", lwd=2)# mensual

st.com + st.sin
#Analisis de autocorrelacion
acf(data.mensual.comp[,1])
pacf(data.mensual.comp[,1])
#diagnostico
#estimacion de parametros
fit<-arima(as.ts(data.mensual.comp[,1]),order=c(1,0,1))
tsdiag(fit)
Box.test(fit$residuals,lag=1)
# prediccion
LH.pred<-predict(fit,n.ahead=10)

plot(as.ts(data.mensual.comp[,1]),xlim=c(1,220),ylim=c(0,500))
lines(LH.pred$pred,col="red")
lines(LH.pred$pred+2*LH.pred$se,col="red",lty=3)
lines(LH.pred$pred-2*LH.pred$se,col="red",lty=3)
