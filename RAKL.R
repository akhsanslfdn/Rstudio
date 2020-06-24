ransum = c(rep('A',4),rep('B',4),rep('C',4),rep('D',4)) 
umur = c(rep(c('1','2','3','4'),4)) 
bobot = c(2,3,3,5,5,4,5,5,8,7,10,9,6,5,5,2) 
percobaan = data.frame(ransum, umur, bobot) 

result = aov(bobot~ransum+umur, data = percobaan) 

summary(result) 

par(mfrow=c(1,2))
plot (bobot~ransum , data=percobaan)
plot (bobot ~ umur, data=percobaan)

bartlett.test(bobot~ransum, data = percobaan) 
shapiro.test(result$residuals)

TukeyHSD(result,'ransum',  ordered=TRUE )

plot(TukeyHSD(result,'ransum',  ordered=TRUE ))

