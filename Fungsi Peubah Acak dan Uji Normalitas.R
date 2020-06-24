install.packages("tseries")
install.packages("car")
install.packages("nortest")
library(tseries)
library(car)
library(nortest)

#menghitung kuantil distribusi normal
qbinom(0.25,15,0.5, lower.tail = T)
?qbinom

set.seed(1)
rnorm(100,0,1)
set.seed(123)
rnorm(100,0,1)

#membuat plot untuk suatu nilai peubah
dbinom(0:15,15,0.5)

Table.Density=data.frame(pr=dbinom(0:15,size = 15,prob = 0.5))
rownames(Table.Density)<-0:15
Table.Density
a=pbinom(0:15,20,0.5, lower.tail = T)
a
plot(a)
Tabel.Distribusi.Peluangkumulatif<-data.frame(Pr=pbinom(0:15,
                                                        size = 15,prob = 0.5, lower.tail = T))                        
rownames(Tabel.Distribusi.Peluangkumulatif)<-0:15
Tabel.Distribusi.Peluangkumulatif

#Uji Normalitas Menggunakan Uji Goodness of Fit Test
data=read.delim("clipboard")
data=scan()
data
shapiro.test(data)

library(tseries)
jarque.bera.test(data)

library(nortest) 
sf.test(data)
ad.test(data)
cvm.test(data)
lillie.test(data)
pearson.test(data)

library(car)
powerTransform(data)
jarque.bera.test(data^-1.40403)

#Transformasi Inverse Square untuk Data Bernilai Negatif
dataa=read.delim("clipboard",header=F)
dataa
n=dataa
n
data_positif=function(n,c)
{
  x=length(n)
  r=NULL
  for(i in 1:x)
    if(n[i]==0)
    {
      r[i]=1/(n[i]^2+c)
      cat(r[i],"\n")
    }
  else
  {
    r[i]=1/(n[i]^2)
    cat(r[i],"\n")
  }
  r
}
datpos2=data_positif(n,c)
datpos2
powerTransform(datpos2)
jarque.bera.test(dat^-0.6717152)
