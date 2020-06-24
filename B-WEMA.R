data.cp=read.delim("clipboard",header=TRUE)
head(data.cp)
library(minpack.lm)
bwema=function(par,data.cp)
{
  #panjang data
  n=(dim(data.cp))[1];
  N=n+1
  #pembentukan matriks
  F0=matrix(0,n,1);
  S1=matrix(0,n,1);
  S2=matrix(0,n,1);
  a=matrix(0,n,1);
  b=matrix(0,n,1);
  #initial value
  S1[5]=(((data.cp[5,1]*5)+(data.cp[4,1]*4)+(data.cp[3,1]*3)+
            (data.cp[2,1]*2)+(data.cp[1,1]*1))/(sum(1:5)));
  S2[5]=(((data.cp[5,1]*5)+(data.cp[4,1]*4)+(data.cp[3,1]*3)+
            (data.cp[2,1]*2)+(data.cp[1,1]*1))/(sum(1:5)));
  a[5]=(((data.cp[5,1]*5)+(data.cp[4,1]*4)+(data.cp[3,1]*3)+
           (data.cp[2,1]*2)+(data.cp[1,1]*1))/(sum(1:5)));
  for (i in 6:N)
  {
    S1[i]=par*data.cp[i,1]+(1-par)*S1[i-1];
    S2[i]=par*S1[i]+(1-par)*S2[i-1];
    b[i]=par/(1-par)*(S1[i]-S2[i]);
    a[i]=2*S1[i]-S2[i];
    F0[i]=a[i-1]+b[i-1];
  }
  return(F0)
}

forecast = bwema(0.1,data.cp)
data=as.data.frame(forecast)
head(forecast)

View(forecast)
forecast
msebwema=function(par,data)
{
  n=nrow(data.cp);
  forecast=bwema(par,data.cp);
  data = as.data.frame(forecast);
  error2=(data[6:n,]-data.cp[6:n,])^2
  msebwema=mean(error2)
}
mapebwema=function(par,data)
{
  n=nrow(data.cp);
  data.cp2=data.cp[6:n,]
  forecast=bwema(par,data.cp);
  data= as.data.frame(forecast);
  error=(data[6:n,]-data.cp[6:n,])
  pei=(error/data.cp2)*-1
  mapebwema=mean(abs(pei))*100
}
mse= msebwema(0.1,data.cp)
mse
mape= mapebwema(0.1,data.cp)
mape
#optimasi algoritma lm
lm=nls.lm(c(0.1),lower=NULL,upper=NULL,msebwema,data=data.cp)
lm
forecast.op= bwema(0.5014,data.cp)

data=as.data.frame(forecast)
head(forecast.op)
View(forecast.op)
forecast.op
msebwema.op=function(par,data)
{
  n=nrow(data.cp);
  forecast.op=bwema(par,data.cp);
  data = as.data.frame(forecast.op);
  error2=(data[6:n,]-data.cp[6:n,])^2
  msebwema.op=mean(error2)
}
mapebwema.op=function(par,data)
{
  n=nrow(data.cp);
  data.cp2=data.cp[6:n,]
  forecast.op=bwema(par,data.cp);
  data= as.data.frame(forecast.op);
  error=(data[6:n,]-data.cp[6:n,])
  pei=(error/data.cp2)*-1
  mapebwema.op=mean(abs(pei))*100
}
mse.op= msebwema.op(0.5014,data.cp)
mse.op
mape.op= mapebwema.op(0.5014,data.scp)
mape.op

