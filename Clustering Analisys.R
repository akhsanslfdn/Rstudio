##contoh kmeans clustering
belajar<-c(80,130,110,90,120,140,60,50,100,70,120,100,45)
gadget<-c(130,45,90,100,60,30,150,145,90,80,120,40,140)
anak<-cbind(belajar,gadget)
anak
rownames(anak)<-c("izan","tiara","sendy","jimmy","kiky","mega","ridha","fian","ria","dewi","sita","irfan","boki")
anak

plot(anak,cex=2,lwd=4,pch=1)
plot(anak,cex=0.1,lwd=0.1,pch=1)
text(anak,labels = row.names(anak),cex=2)
plot(anak,cex=0.1,lwd=0.1,pch=1,asp=1)
text(anak,labels = row.names(anak),cex=2)

cluster<-kmeans(anak,2)
cluster

str(cluster)
