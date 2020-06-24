#Vektor
c(T,1,-5,T,"A")
x=c(10,5,14,12,8,11,9,10,16,20)
x
x[-5]
x[-4,-6]
x[x<10]

#matriks
A<-matrix(c(1:4),2,2) 
A
dim(A)
length(A)
mode(A)

x1=1:4
x2=matrix(c(1:4),2)
x3=matrix(c(2,3,2,3),2)

x1
x2
x3

x1*x1 #perkalian elemen demi elemen
x2*x2 

x2%*%x3 #perkalianmatriks
x1%o%x1 #outer

t(x2)
invx2=solve(x2)
invx2
invx2%*%x2

crossprod(x1)
eigen(x2)  #

Nama = c("Arif","Bima","Lina","Susi","Galuh")
Asal = c("Jakarta","Semarang","Sumbawa","Samarinda","Balikpapan")
Hobi = c("Traveling","Singig","Dancing","Coding","Watching Movie")
Umur = c(21,20,19,20,18)

frame=data.frame(Nama,Asal,Hobi,Umur)
frame

list=list(no=c(1,2,3,4),status=c(T,F,T,T),nilai=data.frame(nilai=c(5,6,7,8),nama=c("A1","A2","B1","B4")))
list

