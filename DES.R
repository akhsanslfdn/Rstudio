X=scan()
series=ts(X, start=c(2011,1), freq=12)
series
plot(series)

library(forecast)

# Double Exponential smoothing: Level and Trend components
#alpha=0.1; beta=0.1
modelDES.1 <- HoltWinters(series, alpha=0.1, beta=0.1, gamma=FALSE) 

Xhat.1 <- modelDES.1$fitted[,2] #predict from model

error.1 <- residuals(modelDES.1) #calculate error
SSE.1 <- modelDES.1$SSE 
MSE.1 <- mean(error.1^2)
RMSE.1 <- sqrt(MSE.1)

forecast.1 <- predict(modelDES.1, n.ahead=5)

#alpha=0.2; beta=0.1
modelDES.2 <- HoltWinters(series, alpha=0.2, beta=0.1, gamma=FALSE) 

Xhat.2 <- modelDES.2$fitted[,2] 

error.2 <- residuals(modelDES.2) 
SSE.2 <- modelDES.2$SSE 
MSE.2 <- mean(error.2^2)
RMSE.2 <- sqrt(MSE.2)

forecast.2 <- predict(modelDES.2, n.ahead=5)

#alpha=0.1; beta=0.2
modelDES.3 <- HoltWinters(series, alpha=0.1, beta=0.2, gamma=FALSE) 

Xhat.3 <- modelDES.3$fitted[,2] 

error.3 <- residuals(modelDES.3)
SSE.3 <- modelDES.3$SSE 
MSE.3 <- mean(error.3^2)
RMSE.3 <- sqrt(MSE.3)

forecast.3 <- predict(modelDES.3, n.ahead=5)

#alpha=0.2; beta=0.2
modelDES.4 <- HoltWinters(series, alpha=0.2, beta=0.2, gamma=FALSE) 

Xhat.4 <- modelDES.4$fitted[,2] 

error.4 <- residuals(modelDES.4)
SSE.4 <- modelDES.4$SSE 
MSE.4 <- mean(error.4^2)
RMSE.4 <- sqrt(MSE.4)

forecast.4 <- predict(modelDES.4, n.ahead=5)

##Comparing MSE and Forecast for these models
data.forecast=data.frame(Model_DES=c("alpha=0.1 dan beta=0.1","alpha=0.2 dan beta=0.1", "alpha=0.1 dan beta=0.2", "alpha=0.2 dan beta=0.2"), Nilai_MSE=c(MSE.1, MSE.2, MSE.3, MSE.4), Nilai_RMSE=c(RMSE.1, RMSE.2, RMSE.3, RMSE.4))
data.forecast

#forecasting from best model
forecast.1

##Plot Comparing Model
plot(series, main="Comparing Model", type="l", col="black")
lines(Xhat.1, type="l", col="red")
lines(Xhat.2,col="green", type="l")
lines(Xhat.3, col="blue", type="l")
lines(Xhat.4, col="yellow", type="l")

legend("bottomleft", c("Actual Data", expression(paste(DES, " ", alpha, "=0.1", " ", beta, "=0.1")),expression(paste(DES, " ", alpha,"=0.2", " ", beta, "=0.1")),expression(paste(DES, " ", alpha,"=0.1", " ", beta, "=0.2")), expression(paste(DES, " ", alpha, "=0.2", " ", beta, "=0.2"))), cex=0.6, col=c("black","red","green", "blue","yellow"), lty=1, bty="n")

##Plotting data from best model
plot(modelDES.1) #model with alpha=0.1 dan beta=0.1