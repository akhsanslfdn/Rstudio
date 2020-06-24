
#RAL

#Perlakuan : Kadar Paracetamol (A,B,C,D,E)

#respon : waktu dari perlakuan


perlakuan = c(rep('A',5),rep('B',5),rep('C',5),rep('D',5),rep('E',5))
respon = c(7,6,9,4,7,9,7,8,6,9,5,4,8,6,3,3,5,2,3,7,2,3,4,1,4)

datalat1 = data.frame(perlakuan,respon)  #menggabungkan array

# Membuat Plot

plot(respon~perlakuan, data=datalat1)

#cara 1

an1=lm(respon~perlakuan,data=datalat1)
anova(an1)

#cara 2

result=aov(respon~perlakuan,data=datalat1)
summary(result)

bartlett.test(respon~perlakuan, data=datalat1)
shapiro.test(result$residuals)
