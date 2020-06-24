#Sarima
turis=read.csv("D:\\Analisis Runtun Waktu\\Praktikum & Laporan\\Pertemuan 5\\spain.txt")
turis=ts(turis, start = c(1970, 1), frequency = 12)
turis

#membuat plot
ts.plot(turis, col="blue", main="TS:Turis Spanyol", lwd=2)

library(forecast)
library(tseries)

#Uji stasioner
adf.test(turis) #H0 : Data mengandung unit root(tidak stasioner)

#kpss trend hampir sama dengan uji adf
#kpss level = karena datanya tidak stsioner dalam musiman maka tolak H0
kpss.test(turis, null="Level") #UJi Stasioner, H0 : data Stasioner

kpss.test(turis, null="Trend") #UJi Stasioner, H0 : data Stasioner

par(mfrow= c(1,2))
Acf(turis, lag.max = 36)
pacf(turis, lag.max = 36)

#jika data mengandung musiman maka pertama kali kita melakukan deferensi musiman
turis.dslog=diff(log(turis),differences=1, lag=12) #differensi musiman order 1 (a=12)

ts.plot(turis.dslog, col="green", main="TS:Turis dslog")
adf.test(turis.dslog)
par(mfrow= c(1,2))
Acf(turis.dslog, lag.max = 37)
pacf(turis.dslog, lag.max = 37)

#differensing non musiman
turis.ddslog=diff(turis.dslog,differences=1) #differensing non musiman order 1
adf.test(turis.ddslog)
par(mfrow= c(1,2))
Acf(turis.ddslog, lag.max = 37)
pacf(turis.ddslog, lag.max = 37)

#estimasi
model1=Arima(log(turis), order=c(0,1,1), seasonal=list(order=c(2,1,1), period=12), include.mean= F)
model2=Arima(log(turis), order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12), include.mean= F)
model3=Arima(log(turis), order=c(0,1,1), seasonal=list(order=c(0,1,2), period=12), include.mean= F)
model1
model2
model3                          

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
             printstatarima(model1.new)
             printstatarima(model2.new)
             