#Input Data
softdrink = read.table("D:\\Rancangan Percobaan\\data softdrink.txt", header = TRUE)
softdrink
softdrink.df = data.frame(respon = c(softdrink$U1, softdrink$U2), rbind(softdrink[,1:3], softdrink[,1:3])) # membuat data frame data
softdrink.df[,2:4] = lapply(softdrink.df[,2:4], factor) # mengubah 
softdrink.df

#ANOVA
hasil = aov(respon ~ A*B*C, data = softdrink.df)
summary(hasil)
