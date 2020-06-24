
#RAKL

#Perlakuan : Kadar Paracetamol (A,B,C,D,E)

#respon : waktu dari perlakuan


perlakuan = c(rep('A',4),rep('B',4),rep('C',4),rep('D',4))
respon = c(2,3,3,5,5,4,5,5,8,7,10,9,6,5,5,2)
kelompok = c(rep(c(1,2,3,4),4))

bobot = data.frame(perlakuan,respon,kelompok)

hasil = aov(respon~perlakuan+kelompok, data=bobot)
summary(hasil)

