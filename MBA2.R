dataKS01=read.csv("E:\\KULIAH AYE\\SMT 6\\DATMIN\\KS01.csv")
View(dataKS01)
data1=split(dataKS01$NAMA,dataKS01$NO)
data1
#melihat berapa pembelian
x=table(dataKS01$NO)
x
y=data.frame(x)
y
datatransaksi<-as(data1,"transactions")
datatransaksi
class(datatransaksi)
View(as(datatransaksi,"matrix")) #diurutkan menurut abjad
View(as(datatransaksi,"data.frame"))
itemFrequencyPlot(datatransaksi,type="absolute",topN=10) #mengetahui frekuensi penjualan terbanyak
dataapriori<-apriori(datatransaksi,parameter=list(supp=0.01,conf=0.2))
inspect(dataapriori)

#No 1 tgl 1/2/2006
datatgl1=split(dataKS01$HARGA,dataKS01$TANGGAL)
datatgl1
summary(dataKS01[1:1446])
sum(dataKS01$TOTAL[1:1446])
sum(dataKS01$TOTAL[1447:3076])
sum(dataKS01$TOTAL[3076:4838])
sum(dataKS01$HARGA)
tanggal=table(dataKS01$TANGGAL)
tanggal
tanggal1=data.frame(tanggal)
tanggal1
datatransaksi<-as(datatgl1,"transactions")
datatransaksi
class(datatransaksi)
View(as(datatransaksi,"matrix")) #diurutkan menurut abjad
View(as(datatransaksi,"data.frame"))
itemFrequencyPlot(datatgl1,type="absolute",topN=10) #mengetahui frekuensi penjualan terbanyak
dataapriori<-apriori(datatransaksi,parameter=list(supp=0.01,conf=0.2))
inspect(dataapriori)
