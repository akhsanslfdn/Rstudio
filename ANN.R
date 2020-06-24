#input data
input = read.delim('clipboard')
input

#memisahkan variabel yg digunakan
data = input[,1:11]
str(data)

#training sample with n observations
n = round(nrow(data)*0.75);n
set.seed(12);samp=sample(1:nrow(data),n)
head(samp)

#memisahkan
train = data[samp,]
test = data[-samp,]

#mengambil nama variabel
feats = names(data[,1:10])
#concatenate strings
f = paste(feats,collapse = '+')
f = paste('Species ~',f)
#convert to formula
f = as.formula(f);f

#train neural net
library(neuralnet)
nn = neuralnet(f,train,hidden=c(4),linear.output = TRUE)
summary(nn)

#plot model
plot(nn)

#confusion matrix+prediksi model
library(caret)
pred_1= compute(nn,test[1:9])
pred_1.1= ifelse(pred_1$net.result > 0.5, 1, 0)
conf_matrix=table(pred_1.1,test$Species);conf_matrix
accuracy = (conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)
accuracy
