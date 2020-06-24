
X=scan()
series=ts(X, start=c(2011,1), freq=12)
series
plot(series)

library(forecast)

# Single exponential smoothing: Level Only
#alpha=0.09
SES.1 <- HoltWinters(series, alpha=0.09, beta=FALSE, gamma=FALSE) 

Xhat.1 <- SES.1$fitted[,2] #predict from model

error.1 <- residuals(SES.1) #calculate error
SSE.1 <- SES.1$SSE 
MSE.1 <- mean(error.1^2)
RMSE.1 <- sqrt(MSE.1)

forecast.1 <- predict(SES.1, n.ahead=1) #forecasting one ahead

#alpha=0.5
SES.2 <- HoltWinters(series, alpha=0.5, beta=FALSE, gamma=FALSE) 

Xhat.2 <- SES.2$fitted[,2]

error.2 <- residuals(SES.2) 
SSE.2 <- SES.2$SSE 
MSE.2 <- mean(error.2^2)
RMSE.2 <- sqrt(MSE.2)

forecast.2 <- predict(SES.2, n.ahead=1)

#alpha=0.8
SES.3 <- HoltWinters(series, alpha=0.8, beta=FALSE, gamma=FALSE) 

Xhat.3 <- SES.3$fitted[,2] 

error.3 <- residuals(SES.3) 
SSE.3 <- SES.3$SSE 
MSE.3 <- mean(error.3^2)
RMSE.3 <- sqrt(MSE.3)

forecast.3 <- predict(SES.3, n.ahead=1)

##Comparing MSE and Forecast for these models
data.forecast=data.frame(Model_SES=c("Alpha=0.1","Alpha=0.2","Alpha=0.5"), Nilai_MSE=c(MSE, MSE1, MSE2), Nilai_RMSE=c(RMSE.1, RMSE.2, RMSE.3), Ramalan=c(forecast.1, forecast.2, forecast.3))
data.forecast

##Plot Comparing Model
plot(series, main="Comparing Model", type="l", col="black")
lines(Xhat.1, type="l", col="red")
lines(Xhat.2,col="green", type="l")
lines(Xhat.3, col="blue", type="l")

legend("bottomleft", c("Actual Data", expression(paste(SES, " ", alpha, "=0.1")),expression(paste(SES, " ", alpha,"=0.2")),expression(paste(SES, " ", alpha,"=0.5"))), cex=0.6, col=c("black","red","green", "blue"), lty=1)


##Plotting data from best model
plot(SES.2) #model with alpha=0.5 #disesuaikan dengan model dg MSE terkecil




