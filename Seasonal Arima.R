jawa=read.csv("D:\\Kuliah\\ARW\\tugas 2\\Volume penumpang KA.csv")
jawa
jawa=ts(jawa$Jawa, start =c (2006, 1), freq=12)
ts.plot(jawa, main="ts : Volume Penumpang KA Jawa")
ts.plot

jawa.sarima1=auto.arima(jawa)
jawa.sarima1

#uji normal
jarque.bera.test(jawa)

library(forecast)
library(tseries)

#autoARIMA (gk dipake jg gpp)
jawa.arima1=auto.arima(jawa)
jawa.arima1
jawa.arima2=auto.arima(jawa,lambda=BoxCox.lambda(jawa))
jawa.arima2

#autoSARIMA
jawa.sarima1=auto.arima(jawa, d=1, D=1, lambda=BoxCox.lambda(jawa))
jawa.sarima1



