library(forecast)
inflasi=read.csv('D:\\Analisis Runtun Waktu\\Inflasi Indonesia.csv')
inflasi

#mengubah data ke time series
inflasi=ts(inflasi$Inflasi, start=c(2006,1), freq=12)
inflasi

#plot
ts.plot(inflasi,col='blue', main="ts:Inflasi",lwd = 2)
length(inflasi)
line(inflasi.hw$fitted, col)

#train dan test
inflasi.train=ts(inflasi[1:115], start=c(2006,1), frequency=12)
inflasi.test=ts(inflasi[116:142], start =c (2015,8), frequency=12)

#HW model 1
inflasi.hw=hw(inflasi.train, alpha=NULL, beta=NULL, gamma=NULL, damped=FALSE, initial = "optimal", exponential=FALSE, seasonal = "additive", h=27)
inflasi.hw$model
inflasi.hw$mean
accuracy(inflasi.hw,inflasi.test)

#Model 1
ts.plot(inflasi.hw$x,col='red', main="Data Aktual VS Fitted",lwd = 2)
lines(inflasi.hw$fitted, col='green', lwd=2)
legend("topright",c("Data Aktual","Data Fitted"), col=c("red","green"),
       cex=0.8,bty = "n", lty = 1) #bty=boxtype


#HW model 2
inflasi.hw=hw(inflasi.train, alpha=NULL, beta=NULL, gamma=NULL, phi= NULL, damped=TRUE, initial = "optimal", exponential=FALSE, seasonal = "multiplicative", h=27)
inflasi.hw$model
inflasi.hw$mean
accuracy(inflasi.hw, inflasi.test)

#SES model 1
inflasi.ses$model

#Model 2
ts.plot(inflasi.hw$x,col='red', main="Data Aktual VS Fitted",lwd = 2)
lines(inflasi.hw$fitted, col='green', lwd=2)
legend("topright",c("Data Aktual","Data Fitted"), col=c("red","green"),
       cex=0.8,bty = "n", lty = 1) #bty=boxtype

#HW model 2
inflasi.hw=hw(inflasi.train, alpha=NULL, beta=NULL, gamma=NULL, phi= NULL, damped=TRUE, initial = "optimal", exponential=FALSE, seasonal = "multiplicative", h=27)
inflasi.hw$model
inflasi.hw$mean
accuracy(inflasi.hw, inflasi.test)


