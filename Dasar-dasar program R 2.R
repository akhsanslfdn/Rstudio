square.it<-function(x)
  {square<-x*x
  return(square)} #harus ditulis
square.it(5)
square.it(c(1, 7, 9))

matrix1<-matrix(c(1:6), nrow=3)
square.it(matrix1)
#luas persegi panjang
luas<-function(p,l)
  {luas<-p*l
  return(luas)}
luas(5, 4)

#soal 1 rumus luas persegi
persegiku<-function(p, l){
  luas<-p*l
  cat("Jika sebuah persegi panjang dengan panjang=", p,"dan lebar =", l,"maka luasnya adalah",luas,"\n")}
persegiku(10, 4)

#soal 2 rumus volume balok
volumebalok<-function(p, l, t){
  volumebalok<-p*l*t
  cat("Jika sebuah persegi panjang dengan panjang=", p,"dan lebar =", l,"dan tingginya=",t,"maka volumenya adalah",volumebalok,"\n")}
volumebalok(10, 4, 6)
#Soal 3
kelilingpersegipanjang<-function(p, l){
  kelilingpersegipanjang<- p+l+p+l
  cat("Jika sebuah persegi panjang dengan panjang=", p,"dan lebar =", l,"maka kelilingnya adalah",kelilingpersegipanjang,"\n")}
kelilingpersegipanjang(8,6)

#soal 4
volumeprismasegitiga<-function(p, l, t){
  volumeprismasegitiga<-1/3*t(1/2*p*l)
  cat("Jika sebuah persegi panjang dengan panjang=", p,"dan lebar =", l,"dan tingginya=",t,"maka volumenya adalah",volumeprismasegitiga,"\n")}
volumeprismasegitiga(12, 24, 6)

#soal 5 if else
nilai<-function(nilai_angka){
  if(nilai_angka>=100){nilai="A"}
  else{nilai="Bukan A"}
return(nilai)}
nilai(20)
nilai(100)

#soal 6
nilai<-function(nilai_angka){
  ifelse(nilai_angka>=80,"A","Bukan A")}
nilai(80)
nilai(60)
#exampel for
for(johan in 1:10){
  cat("Namanya Adalah=", johan,"\n")
}

#soal 7
fibo<-numeric(12)
fibo[1]<-1
fibo[2]<-1
for(i in 3:12){
  fibo[i]<-fibo[i-2]+fibo[i-1]
}
fibo

#soal 8
nilai_angka<-matrix(c(80,35,95,55,68),nrow=5,ncol = 1)
nilai_huruf<-matrix(0, nrow=5, ncol=1)
for(i in 1:5){
  if (nilai_angka[i]>=80){
    nilai_huruf[i]="A"}
    else if (nilai_angka[i]>=65 && nilai_angka[i]<80){
    nilai_huruf[i]="B"}
    else if (nilai_angka[i]>=50 && nilai_angka[i]<65){
    nilai_huruf[i]="C"}
    else (nilai_huruf[i]="D")}
nilai_huruf    
nilai_akhir=data.frame(nilai_angka, nilai_huruf)
nilai_akhir
