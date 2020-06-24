jabo=read.csv("D:\\Analisis Runtun Waktu\\Volume penumpang KA Jabodetabek.csv")
jabo
jabo = ts (jabo$Jabodetabek, start = c (2012,1), frequency = 12)
ts.plot(jabo,  col='blue', main="TS: PLot ", lwd=2)

#dekomposisi
jabo.decom = decompose(jabo, type = "additive")
plot(jabo.decom, lwd=2)
jabo.decom

#library
library(forecast)

#menghilangkan komponen musiman
jabo.nomusim=seasadj(jabo.decom)
jabo.nomusim

#meramalkan
jabo.holt=holt(jabo.nomusim, alpha = NULL, beta = NULL, initial = "optimal", h=10)
jabo.holt
plot(jabo.holt, lwd=2)

#peramalan
jabo.forecast=jabo.holt$mean+jabo.decom$seasonal[10:19]
jabo.forecast

jabo.musim = sindexf(jabo.decom, h=10)
jabo.musim

jabo.forecast=jabo.holt$mean+jabo.musim
jabo.forecast
