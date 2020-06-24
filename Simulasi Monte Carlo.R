#Soal No 1
set.seed(1)
x=round(runif(10),digits = 2)*100
x

kedatangan=c(0:5)
frekuensi=c(10,20,40,60,40,30)
distribusi_frek=frekuensi/(sum(frekuensi))
distribusi_kum=cumsum(distribusi_frek)
data=data.frame(kedatangan,frekuensi,distribusi_frek,distribusi_kum)
data

set.seed(1)
bil.acak=round(runif(10),digits = 2)
bil.acak

output=array(dim=c(10,2))

for (i in 1:10) {
  output[i,1]=i
  
  if(bil.acak[i]<=distribusi_kum[1])
  {output[i,2]=0
  }
  else if(bil.acak[i]<=distribusi_kum[2])
  {output[i,2]=1
  }
  else if(bil.acak[i]<=distribusi_kum[3])
  {output[i,2]=2
  }
  else if(bil.acak[i]<=distribusi_kum[4])
  {output[i,2]=3
  }
  else if(bil.acak[i]<=distribusi_kum[5])
  {output[i,2]=4
  }
  else output[i,2]=5
}

hari=c(output[,1])
prediksi_byk_kedatangan=c(output[,2])
tabel_prediksi=data.frame(hari,prediksi_byk_kedatangan)
tabel_prediksi
rata2_byk_kedatangan=mean(prediksi_byk_kedatangan)
rata2_byk_kedatangan


#Soal No 2

waktu_antar_kedatangan=c(0:5)
probabilitas_frek=c(0.10,0.35,0.25,0.15,0.10,0.05)
distribusi_kum=cumsum(probabilitas_frek)
data=data.frame(waktu_antar_kedatangan,probabilitas_frek,distribusi_kum)
data

set.seed(5)
x=round(runif(15),digits = 2)
x

output=array(dim=c(15,2))
output[,1]=c(1:15)
for(i in 1:15){
  output[i,1]=i
  if(x[i]<=distribusi_kum[1])
  {output[i,2]=0
  }
  else if(x[i]<=distribusi_kum[2])
  {output[i,2]=1
  }
  else if(x[i]<=distribusi_kum[3])
  {output[i,2]=2
  }
  else if(x[i]<=distribusi_kum[4])
  {output[i,2]=3
  }
  else if(x[i]<=distribusi_kum[5])
  {output[i,2]=4
  }
  else output[i,2]=5
}
}

hari=c(output[,1])
pred.kedatangan=c(output[,2])
tabel.prediksi=data.frame(hari,pred.kedatangan)
tabel.prediksi
rata2_kedatangan=mean(pred.kedatangan)
rata2_kedatangan



waktu_pelayanan=c(0:4)
probabilitas_frek=c(0.00,0.25,0.20,0.40,0.15)
distribusi_kum=cumsum(probabilitas_frek)
data=data.frame(waktu_pelayanan,probabilitas_frek,distribusi_kum)
data

set.seed(5)
x=round(runif(15),digits = 2)
x

output=array(dim=c(15,2))
output[,1]=c(1:15)
for(i in 1:15){
  output[i,1]=i
  if(x[i]<=distribusi_kum[1])
  {output[i,2]=0
  }
  else if(x[i]<=distribusi_kum[2])
  {output[i,2]=1
  }
  else if(x[i]<=distribusi_kum[3])
  {output[i,2]=2
  }
  else if(x[i]<=distribusi_kum[4])
  {output[i,2]=3
  }
  else output[i,2]=4
}
}


hari=c(output[,1])
pred.pelayanan=c(output[,2])
tabel.prediksi=data.frame(hari,pred.pelayanan)
tabel.prediksi
rata2_pelayanan=mean(pred.pelayanan)
rata2_pelayanan

data=data.frame(x,pred.kedatangan,pred.pelayanan)
data