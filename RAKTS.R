# Rancangan Acak Kelompok TAk Seimbang (RAKTS)

perlakuan = c("A","C","D","A","B","C","B","C","D","A","B","D")
respon = c(73,73,75,74,75,75,67,68,72,71,72,75) 
kelompok = c(rep("1",3),rep("2",3),rep("3",3),rep("4",3))
bobot = data.frame(respon, perlakuan, kelompok) 

result = aov(respon~kelompok+perlakuan, data = bobot) #kelompok+perlakuan tidak boleh terbalik
summary(result)

hasil1 = aov(respon~kelompok+perlakuan, data = bobot)
drop1(hasil1, test = "F") #F = menggunakan uji F 
#jika menggunakan drop1 JKP dan JKT sudah terkoreksi
#kelompok dan perlakuan boleh terbolak balik

