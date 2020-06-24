data.cp=read.delim("clipboard",header=TRUE)
head(data.cp)
library(minpack.lm)
wema=function(par,data.cp)
{
  #panjang data
  n=(dim(data.cp))[1];
  N=n+1
  #pembentukan matriks
  F0=matrix(0,n,1);
  #initial value
  F0[6]=(((data.cp[5,1]*5)+(data.cp[4,1]*4)+(data.cp[3,1]*3)+
            (data.cp[2,1]*2)+(data.cp[1,1]*1))/(sum(1:5)));
  for (i in 7:N)
  {
    F0[i]=par*data.cp[i-1,1]+(1-par)*F0[i-1];
  }
  return(F0)
}
forecast = wema(0.3333,data.cp)
data=as.data.frame(forecast)
head(forecast)
View(forecast)
forecast
mse_wema=function(par,data.cp)
{
  n=nrow(data.cp);
  forecast=wema(par,data.cp);
  data=as.data.frame(forecast)
  error2=(data[6:n,]-data.cp[6:n,])^2
  mse_wema=mean(error2)
  
}
mse= mse_wema(0.3333,data.cp)
mse
mapewema=function(par,data)
{
  n=nrow(data.cp);
  data.cp2=data.cp[6:n,]
  forecast=wema(par,data.cp);
  data= as.data.frame(forecast);
  error=(data[6:n,]-data.cp[6:n,])
  pei=(error/data.cp2)*-1
  mapewema=mean(abs(pei))*100
}
mape= mapewema(0.3333,data.cp)
mape

