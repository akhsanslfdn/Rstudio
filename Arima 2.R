library(forecast)
library(tseries)

motor=read.csv('D:\\Kuliah\\ARW\\tugas 2\\penjualan motor.csv')
motor

Kawasaki=ts(motor$Kawasaki, start = c(2008, 1), frequency = 12)
Kawasaki

#plot
ts.plot(Kawasaki,col='blue', main="ts:Kawasaki",lwd = 2)
length(Kawasaki)

#normalitas
jarque.bera.test(Kawasaki)

Lambda = BoxCox.lambda(Kawasaki)
Lambda

#uji stasioneritas
adf.test(Kawasaki)
adf.test(diff(Kawasaki, differences = 1))

#identifikasi model
par(mfrow=c(1,2))
Acf(diff(Kawasaki, differences=1))
Pacf(diff(Kawasaki, differences=1))

#estimasi
model1=Arima(Kawasaki, order = c(4,1,1))
model1

#estimasi parameter dan overfitting mode# 
printstatarima <- function (x, digits = 4,se=T,...){
  if (length(x$coef) > 0) {
    cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
      ses <- rep(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      statt <- coef[1,]/ses
      pval  <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = FALSE)
      coef<-rbind(coef, t=round (statt,digits=digits) , sign.= round (pval,digits=digits))
      coef <- t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}
#Estimasi model
model1=Arima(Kawasaki, order = c(1,1,0))
model1
printstatarima(model1)

model1=Arima(Kawasaki, order = c(1,1,0),lambda = BoxCox.lambda(Kawasaki))
model1
printstatarima(model1)

model1=Arima(Kawasaki, order = c(2,1,0),lambda = BoxCox.lambda(Kawasaki))
model1
printstatarima(model1)

model1=Arima(Kawasaki, order = c(2,1,2),lambda = BoxCox.lambda(Kawasaki))
model1
printstatarima(model1)
tsdiag(model1)

#prediksi
kawasaki.pred=forecast(model1, h=10)
kawasaki.pred
plot(kawasaki.pred)

tsdiag(model1)
#auto ARIMA
kawasaki.arima1=auto.arima(Kawasaki)
kawasaki.arima1
kawasaki.arima2=auto.arima(Kawasaki, lambda = BoxCox.lambda(Kawasaki))
kawasaki.arima2
