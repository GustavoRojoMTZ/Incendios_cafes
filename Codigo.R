library(readxl)
library(ggplot2)
library(ggfortify)
library(imputeTS)
library(strucchange)
library(fUnitRoots)
library(astsa)
library(urca)
library(tswge)
library(TSstudio)
library(forecast)
library(tseries)
library(tsoutliers)
library(fma)
library(expsmooth)
library(rriskDistributions)
library (MASS)
library( aTSA )
library(nortest) 
library(moments) 
library(fitdistrplus)
library(lmtest)
library(fBasics)
library(car)
library(tidyverse)
library(Rmisc)
library(nortest)
library(dplyr)
library(TSA)
library(DescTools)

#Importación de Datos
RPV_China <- read_excel("RPV_China.xls", 
                        range = "B11:B87")
RPV_Mexico <- read_excel("RPV_Mexico.xls", 
                        range = "B11:B87")

#Traformar a formato Serie de tiempo
China.ts <- ts(RPV_China,frequency = 4,start = c(2005,2))
Mexico.ts <- ts(RPV_Mexico,frequency = 4,start = c(2005,2))

#Primeras graficas de la introducción
plot.ts(China.ts)
plot.ts(Mexico.ts)

#Imputacion
Na.China <- statsNA(China.ts)
Na.Mexico <- statsNA(Mexico.ts)

ggplot_na_distribution(China.ts)
ggplot_na_distribution(Mexico.ts)

#Cambios estructurales
Aux <- 1:length(RPV_China$CHINA)

globtemp_brk1 <- breakpoints(China.ts ~ Aux,h = 0.1)
globtemp_brk2 <- breakpoints(Mexico.ts ~ Aux,h = 0.1)

summary(globtemp_brk1)
summary(globtemp_brk2)


#Graficas de BIC y RSS - cambios estructurales
plot(globtemp_brk1)
plot(globtemp_brk2)


#Graficas de los cambios estructurales
plot(China.ts)
lines(fitted(globtemp_brk1, breaks = 7), col = 4)
lines(confint(globtemp_brk1, breaks = 7))

plot(Mexico.ts)
lines(fitted(globtemp_brk2, breaks = 5), col = 4)
lines(confint(globtemp_brk2, breaks = 5))


#test de Dickey-Fuller (Prueba de estacionalidad)
DF1 = ur.df(RPV_China$CHINA, type="none", lags = 0)
summary(DF1)
DF2 = ur.df(RPV_Mexico$MEXICO, type="none", lags = 0)
summary(DF2)


#Diferenciación
China.wt <- artrans.wge(China.ts,phi.tr =1)
Mexico.wt <- artrans.wge(Mexico.ts,phi.tr =1)

#test de Dickey-Fuller (ya diferenciado)
DF3 = ur.df(China.wt, type="none", lags = 0)
summary(DF3)
DF4 = ur.df(Mexico.wt, type="none", lags = 0)
summary(DF4)


#Analisis de Autocorrelación y Autocorrelación parcial.
op<-par(mfrow=c(1,2))
acf(China.wt,main="ACF de China")
pacf(China.wt,main="PACF de China")

op<-par(mfrow=c(1,2))
acf(Mexico.wt,main="ACF de Mexico")
pacf(Mexico.wt,main="PACF de Mexico")


China.wt <- ts(China.wt,frequency = 4,start = c(2005,2))
Mexico.wt <- ts(Mexico.wt,frequency = 4,start = c(2005,2))

#Datos de prueba y entrenamiento
train1 <- window(China.wt,start = time(China.wt)[1],
                end = time(China.wt)[length(China.wt) - 7])
test1 <- window(China.wt,start = time(China.wt)[length(China.wt) - 7 + 1],
               end = time(China.wt)[length(China.wt)])
train2 <- window(Mexico.wt,start = time(Mexico.wt)[1],
                end = time(Mexico.wt)[length(Mexico.wt) - 7])
test2 <- window(Mexico.wt,start = time(Mexico.wt)[length(Mexico.wt) - 7 + 1],
               end = time(Mexico.wt)[length(Mexico.wt)])

#Ajuste de modelo Arima
Arima.china <- arima(China.wt,order = c(3,0,3))
Arima.mexico <- arima(Mexico.wt,order = c(3,0,3))

#Ajuste computacional de modelo arima
md1 <- auto.arima(train1, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
md2 <- auto.arima(train2, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)

#grafica de reciduales
checkresiduals(md1)
checkresiduals(md2)

#normalidad de reciduales
jarque.bera.test(md1$residuals)
jarque.bera.test(md2$residuals)


#Predicciones de los siguientes 2 años
fc1 <- forecast(md1, h = 8)
fc2 <- forecast(md2, h = 8)

test_forecast(actual = China.wt,forecast.obj = fc1,test = test1)
test_forecast(actual = Mexico.wt,forecast.obj = fc2,test = test2)

accuracy(fc1, test1)
accuracy(fc2, test2)


#Analisis de intervanción

ari.m1<- Arima(China.ts, order=c(1,1,0), seasonal=c(0,1,1),
               lambda=0)
coeftest(ari.m1)
resid <- residuals(ari.m1)
pars <- coefs2poly(ari.m1)
outliers <- locate.outliers(resid, pars)

locate.outliers(resid, pars, cval = 3.5, types = c("AO", "LS", "TC"), delta = 0.7)

#Analisis de tranferencia

#Lectura de ambas bases de datos

RPV_China <- read_excel("RPV_China.xls", 
                        range = "B11:B87")
IPC <- read_excel("Indice_de_precios_al_consumidor.xls", 
                  range = "B11:B240")

#codigo para obtener los valores cuatrimestrales de IPC
n=1
a <- numeric(76)
for (i in 1:76) {
  a[i] <- (IPC$CPI[n]+IPC$CPI[n+2]+IPC$CPI[n+3])/3
  n=n+3
}
IPC <- data.frame(CPI = a)

#Series de tiempo
China.ts <- ts(RPV_China,frequency = 4,start = c(2005,2))
IPC.ts <- ts(IPC,frequency = 4,start = c(2005,2))

#Grafico de ambas series
par(mar=c(5,4,4,4))  
plot(China.ts, col="blue", ylab="",type="o",pch=20)
mtext("RPV",side=2,line=2,col="blue")
par(new=T)   # Para sobreponer las 2 series
plot(IPC.ts, col="red", type="o", xaxt="n", yaxt="n", xlab="", ylab="", pch=20)
axis(4)  # Segundo eje Y
mtext("IPC",side=4,line=2,col="red")
grid()

#Analisis exploratorio de X1
basicStats(IPC.ts)
qqPlot(IPC.ts,dist="norm",ylab = "RPV",main = "d)")
jarque.bera.test(IPC.ts)
min(IPC.ts)
jarque.bera.test(log(IPC.ts+2)) #no hay normalidad

#Serie X1
F3<-autoplot(IPC.ts,ylab = "IPC",main="X1")
F4<-autoplot(diff(IPC.ts),ylab = "IPC",main="d=1")
multiplot(F3,F4)

acf(IPC.ts,main="a)")
acf(diff(IPC.ts),main="b)")

pacf(diff(IPC.ts),main="b)")
mod3<-ar(diff(IPC.ts))
mod3$order #11

auto.arima(diff(IPC.ts))

#HO. La serie tiene raiz unitaria
adf.test(IPC.ts) #es estacionaria


#Ajute de modelos ARIMA a xt

acf2(IPC.ts)

# mod1<-ar(diff(brentX_ts),method = "mle")
# mod1$order

m_IPC<-Arima(IPC.ts,order=c(0,1,1),include.drift = TRUE)
ari.m1<- Arima(China.ts, order=c(1,1,0), seasonal=c(0,1,1),
               lambda=0)
plot(density(IPC.ts))

summary(m_IPC) # sigma^2 estimated as 0.007222
coeftest(m_IPC)

checkresiduals(m_IPC)
plot(density(residuals(m_IPC)))

Box.test(residuals(m_IPC),type="Ljung-Box") 
mean(residuals(m_IPC))
summary(lm(residuals(m_IPC)~1))

autoplot(ari.m1, main="Estimación precio de viviendas")
autoplot(m_IPC,main="Estimación indice precios al consumidor")

### 12. Test de causalidad Granger -- Ho: X no causa a Y
# The test is simply a Wald test comparing the unrestricted
# model-in which y is explained by the lags 
# (up to order order) of y and x-
# and the restricted model-in which y is only explained 
# by the lags of y.
# X: residuals(m_brent)
# Y: residuals(m_gas)
# Las siguinetes instrucciones son equivalentes

### 12.1 Ho: X no causa a Y
grangertest(residuals(m_IPC),residuals(ari.m1),order = 3)
# Seconcluye que si 

### 13. Correlacion cruzada entre los 
#       ruidos blancos de la series
#       ccf=ccf2(Y,X)

#La correlacion debe ser entre variables estacionarias

ccf(residuals(ari.m1),residuals(m_IPC),ylab="Cross correlation",main="IPC & RPV",type = "correlation")

#b=3,s=1,r=2

ccf2(residuals(ari.m1),residuals(m_IPC),ylab="Cross correlation",main="IPC & RPV",type = "correlation")


#Identificación del modelo de tranferencia 

output_XY <- arimax(China.ts,order=c(1,1,0),
                    transfer=list(c(1,3)),
                     xtransf=IPC.ts)
summary(output_XY) # sigma^2 estimated as 0.0003982
coeftest(output_XY)
tsdisplay(residuals(output_XY))

### 16. Identificacion de outliers. library(tsoutliers)
tso(residuals(output_XY))
tso(residuals(output_XY),types = c("AO", "LS", "TC"))
# No se detectaron outliers
class(residuals(output_XY))
plot(residuals(output_XY))
plot(density(residuals(output_XY),na.rm = TRUE))
Box.test(residuals(output_XY))

### 17. Analisis de residuales del modelo de transferencia

#       Se usa DescTools::JarqueBeraTest
#       porque hay valores perdidos por la diferenciacion en Yt

### 17.1 Ho: Los residuales son normales

DescTools::JarqueBeraTest(residuals(output_XY), na.rm=TRUE)

### 17.2 Funcion de densidad de residuales

plotDistribution = function (x) {
  N = length(x)
  x <- na.omit(x)
  hist( x,col = "light blue",
        probability = TRUE)
  lines(density(x), col = "red", lwd = 3)
  rug(x)
  print(N-length(x))
}
plotDistribution(residuals(output_XY))

### 17.3 Prueba de independencia de residuales
Box.test(residuals(output_XY),type="Ljung-Box") 

#Ajuste de modelo GARCH

library(quantmod)
library(rugarch)
library(rmgarch)

#metodo de holtwinters




