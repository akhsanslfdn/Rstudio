data.cp=read.delim("clipboard",header=TRUE)
data.cp
head(data.cp)
library(minpack.lm) 

bdes=function(par,data.cp) {   
  #panjang data 
  n=(dim(data.cp))[1];
  N=n+1     
  #pembentukan matriks   F0=matrix(0,n,1); 
  F0=matrix(0,n,1);
  S1=matrix(0,n,1); 
  S2=matrix(0,n,1);   
  a=matrix(0,n,1);
  b=matrix(0,n,1);   
  #initial value
  S1[5]=(mean(data.cp[1:5,])); 
  S2[5]=(mean(data.cp[1:5,])); 
  a[5]=(mean(data.cp[1:5,])); 
  for (i in 6:N)   
  {     
    S1[i]=par*data.cp[i,1]+(1-par)*S1[i-1]; 
    S2[i]=par*S1[i]+(1-par)*S2[i-1];   
    b[i]=par/(1-par)*(S1[i]-S2[i]);   
    a[i]=2*S1[i]-S2[i];  
    F0[i]=a[i-1]+b[i-1]  
  }  
  return(F0) 
  } 
  
  forecast = bdes(0.1,data.cp)
  forecast
  head(forecast)
  data=as.data.frame(forecast) 
  
  View(forecast) 
  
  
  msebdes=function(par,data.cp)
  {
    n=nrow(data.cp); 
    forecast=bdes(par,data.cp);
    data = as.data.frame(forecast);  
    error2=(data[6:n,]-data.cp[6:n,])^2 
    msebdes=mean(error2) 
    } 
  
  mapebdes=function(par,data)
  {
    n=nrow(data.cp);  
    data.cp2=data.cp[6:n,] 
    forecast=bdes(par,data.cp);
    data= as.data.frame(forecast); 
    error=(data[6:n,]-data.cp[6:n,])
    pei=(error/data.cp2)*-1 
    mapebdes=mean(abs(pei))*100
    } 
  
  mse= msebdes(0.1,data.cp) 
  mse
  mape= mapebdes(0.1,data.cp) 
  mape 
  
  #optimasi algoritma lm
  lm=nls.lm(c(0.1),lower=NULL,upper=NULL,msebdes,data=data.cp) 
  lm 
  
  forecast.op=bdes(0.5009,data.cp)
  head(forecast.op)
  data=as.data.frame(forecast.op)
  View(forecast.op) 
  forecast.op
  msebdes.op=function(par,data.cp) 
    
  
  {  
    n=nrow(data.cp);  
    forecas.opt=bdes(par,data.cp);
    data = as.data.frame(forecast.op); 
    error2=(data[6:n,]-data.cp[6:n,])^2  
    msebdes.op=mean(error2)
    } 
  
  mapebdes.op=function(par,data) 
  {
    n=nrow(data.cp);
    data.cp2=data.cp[6:n,]  
    forecast.op=bdes(par,data.cp);
    data= as.data.frame(forecast.op);
    error=(data[6:n,]-data.cp[6:n,])  
    pei=(error/data.cp2)*-1  
    mapebdes.op=mean(abs(pei))*100
    } 
  
  mse.op= msebdes.op(0.5009,data.cp)
  mse.op
  mape.op= mapebdes(0.5009,data.cp)
  mape.op
    
  