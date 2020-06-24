library(e1071)
library(devtools)
library(caret)
data.prak5 = read.delim('clipboard')
data.prak5

#training sample with n observation
n=round(nrow(data.prak5)*0.75)
n

#training sample with n observations
set.seed(12345)
sample=sample(seq_len(nrow(data.prak5)), size=n)
train=data.prak5[sample,]
test=data.prak5[-sample,]

##SUPPORT VECTOR MACHINE
data.svm = svm(Diagnosis ~., data=train)
data.svm

#pengujian model SVM data training
pred1 = predict(data.svm,train) #model sementara data training
pred1

confusionMatrix(pred1, train$Diagnosis)

#pengujian model SVM data testing
pred2 = predict(data.svm,test)
pred2

confusionMatrix(pred2, test$Diagnosis)