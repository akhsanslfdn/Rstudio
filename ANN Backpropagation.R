#Skripsi
# input data
set.seed(500)
library(MASS)
data=read.delim("clipboard")
# Check that no data is missing
apply(data,2,function(x) sum(is.na(x)))
# Train-test random splitting for model
index <- sample(1:nrow(data),round(0.8*nrow(data)))
train <- data[index,]
test <- data[-index,]
#Normalisasi data
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}
scaled <- as.data.frame(lapply(data, normalize))
View(scaled)
# Train-test split
train_ <- scaled[index,]
View(train_)
test_ <- scaled[-index,]
View(test_)
# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("inj ~", paste(n[!n %in% "inj"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,2), learningrate=0.01,linear.output=T)
plot(nn)
nn$startweights
nn$weights
# Visual plot of the model
print(nn)
# Predict data test_
pr.nn <- compute(nn,test_[,1:6])
pr.nn_ <- pr.nn$net.result*(max(data$inj)-min(data$inj))+min(data$inj)
test.r <- (test_$inj)*(max(data$inj)-min(data$inj))+min(data$inj)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
MSE.nn