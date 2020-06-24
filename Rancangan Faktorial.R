#Rancangan Faktorial 
respon = c(130,74,155,180,150,159,188,126,138,168,110,160,
           34,80,40,75,136,106,122,115,174,150,120,139,
           20,82,70,58,25,58,70,45,96,82,104,60)
kolom =c(rep('t15',12),rep('t70',12),rep('t125',12)) #temperatur
baris =c(rep(c(rep('1',4),rep('2',4),rep('3',4)),3)) #tipe material
baterai= data.frame(respon,baris,kolom)
baterai
hasil=aov(respon~baris*kolom, data = baterai) #sama
summary(hasil)
hasil$residuals
#hasil=aov(respon~baris+kolom+baris*kolom, data = baterai) #sama
#summary(hasil)
#(* = )
#(** = )
#(*** = )
#untuk model tetap, jika model acak dan model campuran, maka dihitung manual

#menggambar plot iterasi
kolom1= factor(kolom,levels=c("t15","t70","t125")) #mengurutkan kolom dari temperatur 15,75,125
with(baterai,
interaction.plot(kolom1,baris,
respon,type="b",pch=19,fixed=TRUE,xlab=
"Temperatur", 
ylab="Average life"))
plot.design(respon~baris*kolom,data = baterai)
shapiro.test(hasil$residuals)

