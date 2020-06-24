library(arules)
library(Matrix)
library(arulesViz)
library(grid)

data=read.csv("D:\\Pengantar Data Mining\\KS01.csv")
data
head(data)
dim(data)
summary(data)
trans<-as(split(data[,"NAMA"],data[,"NO"]),"transactions")
trans
itemFrequencyPlot(trans,type="absolute", topN=10) 
tabel<-crossTable(trans,sort=TRUE)
tabel[1:4,1:4]
summary(trans)
rules<-apriori(trans,parameter = list(supp=0.01,conf=0.2))
rules
inspect(sort(rules))
crossTable(trans,measure='lift',sort=T)[1:5,1:5]
crossTable(trans,measure='chi',sort=T)[1:5,1:5]
plot(sort(rules,by="lift"),method="graph",control=list(type="items"))
rules2<-apriori(trans,parameter=list(supp=0.001,conf=0.05), 
                appearance=list(default="rhs",lhs="FILMA POUCH 1 LT"))
inspect(sort(rules2))
plot(sort(rules2,by="lift"),method="graph",control=list(type="items"))
