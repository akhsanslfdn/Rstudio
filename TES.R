X=scan()
series=ts(X, start=c(2011,1), freq=12)
series
plot(series)

library(forecast)

# Holt Winters: Level, Trend and Seasonality
#alpha=0.1; beta=0.1; gamma=0.1; model additive
TES.1 <- HoltWinters(series, alpha=0.1, beta=0.1, gamma=0.1, seasonal="additive")

Xhat.1 <- TES.1$fitted[,2] #predict from model

error.1 <- residuals(TES.1) #calculate error
SSE.1 <- TES.1$SSE 
MSE.1 <- mean(error.1^2)
RMSE.1 <- sqrt(MSE.1)

forecast.1 <- predict(TES.1, n.ahead=5)


#alpha=0.1; beta=0.1; gamma=0.1; model multiplicative
TES.2 <- HoltWinters(series, alpha=0.1, beta=0.1, gamma=0.1, seasonal="multiplicative")

Xhat.2 <- TES.2$fitted[,2] #predict from model

error.2 <- residuals(TES.2) #calculate error
SSE.2 <- TES.2$SSE 
MSE.2 <- mean(error.2^2)
RMSE.2 <- sqrt(MSE.2)

forecast.2 <- predict(TES.2, n.ahead=5)

##Comparing MSE and Forecast for these models
data.forecast=data.frame(Model_TES=c("alpha=0.1; beta=0.1; gamma=0.1; Additive","alpha=0.1; beta=0.1; gamma=0.1; Multiplicative"), Nilai_MSE=c(MSE.1, MSE.2), Nilai_RMSE=c(RMSE.1, RMSE.2))
data.forecast

#forecasting from best model
forecast.1

##Plot Comparing Model
plot(series, main="Comparing Model", type="l", col="black", xlim=c(2012,2016))
lines(Xhat.1, type="l", col="red")
lines(Xhat.2,col="green", type="l")


legend("bottomleft", c("Actual Data", expression(paste(TES, " ", alpha, "=0.1", " ", beta, "=0.1", " ", gamma, "=0.1", " ", "Additive")),expression(paste(TES, " ", alpha,"=0.1", " ", beta, "=0.1", " ", gamma, "=0.1", " ", "Multiplicative"))), cex=0.6, col=c("black","red","green"), lty=1, bty="n")

##Plotting data from best model
plot(TES.1) #model with alpha=0.1; beta=0.1; gamma=0.1; additive
