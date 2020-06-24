#Single MA
X=scan()
n=length(X)
k=5

MA=array(NA,dim=c(n))
for(i in 1:n){
  MA[i+k]=mean(X[i:(i+(k-1))])
}
MA


##MSE##
e=array(NA,dim=c(n))
for(i in 1:n){
  e[i]=(X[i]-MA[i])^2
}
e
SSE=sum(e, na.rm=TRUE)
SSE
MSE=mean(e,na.rm=TRUE)
MSE

##MAPE##
PE=array(NA, dim=c(n))
for(i in 1:n){
  PE[i]=abs((X[i]-MA[i])/X[i])
}
PE
MAPE=mean(PE,na.rm=TRUE)
MAPE

##data time series##
X=ts(X, start=1991, end=2014, freq=1)
X
MA5=ts(MA, start=1991, end=2014, freq=1)
MA5
pred=ts(MA[25], start=2015, freq=1)
pred

##plot tipe titik##
plot(X, type="p", col="red",lwd=2, xlim=c(1991,2015), ylim=c(1000000,4000000), xlab="Tahun", ylab="Rupiah", main="Plot Data Asli dan Ramalan MA(5)")
lines(MA5, col="blue", lwd=2, type="p")
limitDate=end(X)[1]+(end(X)[2]-1)/frequency(X)
abline(v=limitDate ,lty=4)
lines(pred,col="green", lwd=2, type="p")
legend("topleft", c("Asli", "Prediksi", "Ramalan"), pch=21, bty="n", lwd=2, col=c("red", "blue","green"))


##plot tipe garis#
pred=ts(MA[24:25], start=2014, end=2015, freq=1)
pred

plot(X, type="l", col="red",lwd=2, xlim=c(1991,2015), ylim=c(1000000,4000000), xlab="Tahun", ylab="Rupiah", main="Plot Data Asli dan Ramalan MA(5)")
lines(MA5, col="blue", lwd=2)
limitDate=end(X)[1]+(end(X)[2]-1)/frequency(X)
abline(v=limitDate ,lty=4)
lines(pred,col="green", lwd=2)
legend("topleft", c("Asli", "Prediksi", "Ramalan"), bty="n", lwd=2, col=c("red", "blue","green"))


