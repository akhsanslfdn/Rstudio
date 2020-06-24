data=data.frame(pengeluaran=c(80,65,70.5,90,95,100,75,80.5,70,65),pendapatan=c(90,70,80,95,100,130,80,90,75,70))
data
summary(data)
var(data) #semakin besar variansi data semakin bagus
cor(data) #melihat hubangan antara 2 variabel 
sd(data$pengeluaran) #sebaran data dengan rata-rata
sd(data$pendapatan)
plot(data)
plot(data,pch=19,xlab="pengeluaran",ylab="pendapatan")
boxplot(data)
hist(data$pengeluaran,xlab = "pengeluaran",main = "Histogram Data Pengeluaran")
pie(data$pengeluaran)
library(plotrix) #package pie chart
slices=c(80,65,70.5,90,95,100,75,80.5,70,65)
lbls=c("80","65","70.5","90","95","100","75","80.5","70","65")
pie3D(slices, labels = lbls, explode=0.5,main="Pie Chart Pengeluaran")

barplot(data$pengeluaran)
dotchart(data$pengeluaran)
stripchart(data$pengeluaran)

#LEGEND
x<- 1:20
x
y1 = x*x
y1
y2 = 3*y1
y2
plot(x, y1, type = "b", pch=19, col="red", xlab="x", ylab="y")
lines(x, y2, pch=18, col="blue", type="b", lty=2)
legend(1, 200, legend = c("Line 1","Line 2"),col = c("red","blue"),lty=1:2, cex=0.8)
