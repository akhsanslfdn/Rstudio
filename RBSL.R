perlakuan=c('a','b','c','d','b','c','d','a','c','d','a','b','d','a','b','c')
respon=c(84,91,59,75,79,82,70,91,63,80,77,75,97,93,80,68)
baris=c(rep(c('1','2','3','4'),4)) 
kolom=c(rep('Aljabar',4),rep('geometri',4),rep('statstika',4),rep('kalkulus',4))
nilai= data.frame(perlakuan,respon,baris,kolom)
hasil=aov(respon~perlakuan+baris+kolom, data=nilai)
hasil
summary(hasil)

barlett. test(respon~perlakuan, cara = kuliah )
shapiro.test(hasil$residuals)
TukeyHSD(hasil, 'perlakuan', ordered = TRUE)
plot(TukeyHSD(hasil,'perlakuan',ordered = TRUE))
