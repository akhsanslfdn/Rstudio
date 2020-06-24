
#RAL

#Perlakuan : Kadar Paracetamol (A,B,C,D,E)

#respon : waktu dari perlakuan


perlakuan = c(rep('A',4),rep('B',3),rep('C',3),rep('D',2))
respon = c(98,97,99,96,91,90,93,96,95,97,95,96)

datalat1 = data.frame(perlakuan,respon)  #menggabungkan array

#Membuat Plot

plot(respon~perlakuan, data=datalat1)

#cara 1

an2=lm(respon~perlakuan,data=datalat1)
anova(an2)

#cara 2

result=aov(respon~perlakuan,data=datalat1)
summary(result)
