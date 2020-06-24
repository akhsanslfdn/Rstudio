jabodetabek=read.csv("D:\\Analisis Runtun Waktu\\Volume penumpang KA.csv")
jabodetabek
jabodetabek=ts(jabodetabek$Jabodetabek, start= c(2006,1), freq = 12)
ts.plot(jabodetabek, main="ts : Volume Penumpang KA Jabodetabek")

pt(12, 30, lower.tail = F)
pt(12, 30, lower.tail = T)

Acf(jabodetabek)
Pacf(jabodetabek)

jabodetabek.sarima1=auto.arima(jabodetabek)
jabodetabek.sarima1

#uji normal
jarque.bera.test(jabodetabek)

jabodetabek.sarima2=auto.arima(jabodetabek, d=1,D=1)
jabodetabek.sarima2


jabodetabek.sarima3=auto.arima(jabodetabek, d=1,D=1, lambda=BoxCox.lambda(jabodetabek))
jabodetabek.sarima3
