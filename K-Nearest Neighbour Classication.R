install.packages("class")
library(class)
tinggi<-c(175,165,166,178,174,169,168,170)
berat<-c(75,60,77,73,62,68,64,70)
size<-factor(c("L","M","L","L","M","M","M","L"))

orang<-cbind(tinggi, berat)
orang

rownames(orang)<-c("Richard","Deby","Mike","Tom","Bella","John","Ann","Jack" )
orang
plot(orang,col=ifelse(size=="L","red","blue"),cex=2,lwd=4) #cex:jenis dari titiknya, lwd:tebal titiknya
plot(orang,col=ifelse(size=="L","red","blue"),cex=0.2)
text(orang,labels = row.names(orang),cex=1.3)
#text(orang,labels = row.names(orang)(col=ifelse(size=="L","red","blue")),cex=1.3)
dev.off()

plot(orang,col=ifelse(size=="L","red","blue"),cex=2,lwd=4,asp=1) #asp:plot jadi rapat

legend("bottomright",fill =c(4.2),legend = c("M","L"),
       title="size",horiz = TRUE,bty = "n",col = grey(.7),cex=1)

ryan<-cbind(173,69)
ryan
points(ryan,cex=2,lwd=4)

klasifikasi<-knn(orang,ryan,size,k=1, prob = TRUE)
klasifikasi
str(klasifikasi)

dim(orang)
orang[8,]
ryan

jarakkuadrat<-(170-173)^2+(70-69)^2
jarakkuadrat
jarak<-sqrt(jarakkuadrat)
jarak

klasifikasi3<-knn(orang,ryan,size,k=3, prob = TRUE)
klasifikasi3

dev.off()

plot(orang,col=ifelse(size=="L","red","blue"),cex=2,lwd=4,asp=1)
text(orang,col=ifelse(size=="L","red","blue"), lwd=2, pch=3,cex=1)
klasifikasi3

#melihat jarak terjauh
circle<-function(x,y,rad=1,nvert=500,...){
  rads<-seq(0,2*pi,length.out = nvert)
  xcoord<-cos(rads)*rad+x
  ycoord<-sin(rads)*rad+y
  polygon(xcoord,ycoord,...)
}

points(ryan,cex=2,lwd=4)

circle(173,69,6.324)
klasifikasi3<-knn(orang,orang,size,k=3, prob = TRUE)
klasifikasi3

100*sum(size==klasifikasi3)/100
table(klasifikasi3, size)

klasifikasi5<-knn(orang,orang,size,k=5, prob = TRUE)
klasifikasi5
100*sum(size==klasifikasi5)/100
table(klasifikasi5, size)



