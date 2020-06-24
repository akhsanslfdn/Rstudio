#Manggil Data
bensin=read.csv("D:\\Analisis Runtun Waktu\\Praktikum & Laporan\\Pertemuan 4\\gasoline.csv")
#ubah data ke time series
bensin=ts(bensin$Gasoline, start=c(2004,1),freq=12)
bensin
ts.plot(bensin, main="TS:Bensin")
install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)
#mengetahui ada apa isi dari package:tseries
ls("package:tseries")
ls("package:forecast")
adf.test(bensin)
par(mfrow=c(1,2))
Acf(bensin,lag.max = 24)
pacf(bensin,lag.max = 24)

#uji normalitas
jarque.bera.test(bensin)

#untuk mengatasi data tidak stasioner menggunakan diferensing
bensin.diff1=diff(bensin,1)
par(mfrow=c(1,1))
ts.plot(bensin.diff1, main="TS:bensin(Diferensin Order 1)")
adf.test(bensin.diff1)
par(mfrow=c(1,2))
Acf(bensin.diff1,lag.max = 24)
pacf(bensin.diff1,lag.max = 24)

#estimasi model
model1=Arima(bensin, order = c(1,1,2),include.mean = F)
model1
model2=Arima(bensin, order = c(1,1,0),include.mean = F)
model2
model3=Arima(bensin, order = c(0,1,1),include.mean = F)
model3
model4=Arima(bensin, order = c(1,1,1),include.mean = F)
model4

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
      pval  <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = F)
      coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,digits=digits))
      coef <- t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}

printstatarima(model1)
printstatarima(model2)
printstatarima(model3)
printstatarima(model4)

#model yang diperoleh untuk uji diagnostik adalah ARIMA (1,1,0)
#uji diagnostic
#uji no autokorelasi
#ljung-box diatas garis putus" artinya gagal tolak H0 = tidak 
tsdiag(model2)
tsdiag(model3)
#hasil yang didapat pilih model 2
tsdiag(model2)

#forecast
pred.bensin = predict(model2, n.ahead = 4)
pred.bensin

fitted(model2)
auto.arima(bensin)
