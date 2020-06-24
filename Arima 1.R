#Input Data
motor=read.csv("D:\\Analisis Runtun Waktu\\penjualan mobil.csv")
motor
suzuki = ts(motor$Suzuki, start=c(20018,1), freq=12)
suzuki
#Membuat Plot
ts.plot(suzuki,col="green", main="penjualan motor", lwd=1)

library(forecast)
library(tseries)

#Tes normalitas
jarque.bera.test(suzuki)

#Uji ADF(menguji stasioneritas)
adf.test(suzuki)

#Uji KPSS
kpss.test(suzuki, null="Trend")

adf.test(diff(suzuki, differences =1))

#Identitas model
par(mfrow=c(1,2))
Acf(diff(suzuki,differences=1))
Pacf(diff(suzuki,differences=1))

#estimasi parameter model ;digunakan untuk mengecek arima
model1=Arima(suzuki,order = c(0,1,0))
model1

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
auto.arima(suzuki)

#prediksi
suzuki.pred=forecast(model1, h=10)
suzuki.pred
plot(suzuki.pred)

