#contoh 1 (memunculkan sebuah nilai)
datanya<-iris$Sepal.Length
datanya
data.ke.n <- function(data,i){
  x<-data[i]
  return (x)
}
data.ke.n(datanya,10)

#contoh 2 (memunculkan beberapa nilai sekaligus)
deskripsi<-function(data){
  
  n=length(data)
  mi=min(data)
  mx=max(data)
  
  cat('Panjang datanya',n,'\n')
  cat('Nilai Terkecil',mi,'\n')
  cat('Nilai Terbesar',mx,'\n')
}
deskripsi(datanya)

# contoh for if
nilai=c(70,85,59,45,60,65)
nilai
nilai.mhs=function(x)
  {
  n=length(x)
  hasil.akhir=NULL
  for (i in 1:n) #untuk berjalan dari satu sampai n
  
  {if (x[i]>50){hasil.akhir[i]="Lulus"} else
  {hasil.akhir[i]="Belum Lulus"}
  }
hasil.akhir
}
nilai.mhs(nilai)
nilai.mhs(nilai) [4] #data ke-4

#contoh  perulangan repeat
x=1
repeat{
  x=x+1
  print(x)
  if(x>=10){
    break
  }}
#PERINTAH IF
#1
{if(2==2)cat ("benar \n")
  else cat("salah \n")}
#2
{if(2!=3)cat ("benar \n")
  else if (2!=3) cat("salah \n")}
#3
program=function(x)
{if (x>60) cat("lulus \n")
  else if (x<=60) cat("tidak lulus \n")}
program(70)
#4
skor=50
if (skor>=80){
  print("A")
    } else if (skor>=70){
    print("B")
      }  else {print("C")}
    