#example 1
f<-function(x) exp(-x)+x^4
curve(f, from=-1, to=4)

f.diff1<-function(x)-exp(-x)+4*x^3
f.diff2<-function(x) exp(-x)+12*x^2

x<-c(0.5, rep(NA,6))

f.nilai<-rep(NA, 7)
f.diff1.nilai<-rep(NA, 7)
f.diff2.nilai<-rep(NA, 7)

for (i in 1:6){
  f.nilai[i]<-f(x[i])
  f.diff1.nilai[i]<-f.diff1(x[i])
  f.diff2.nilai[i]<-f.diff2(x[i])
 
  x[i+1]=x[i]-(f.diff1.nilai[i]/f.diff2.nilai[i])
}

hasil=data.frame(x,f.nilai,f.diff1.nilai,f.diff2.nilai)
hasil
  