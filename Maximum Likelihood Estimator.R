#1
library(stats4)
xpois=rpois(n=10, lambda = 2)
xpois
lpois=function(lambda)
{
  n=length(xpois)
  x=xpois
  #minus loglikelihood
  n*lambda-sum(x)*log(lambda)+sum(log(factorial(x)))
}
estpois=mle(minuslogl=lpois, start=list(lambda=2))
estpois
summary(estpois)
mean(xpois)

#2
library(stats4)
xexp=rexp(n=30, rate = 2)
xexp
lexp=function(rate)
{
  n=length(xexp)
  x=xexp
  #minus likelihood
  -n*log(rate)+rate*sum(x)
}
estexp=mle(minuslogl=lexp, start = list(rate=2))
estexp
summary(estexp)
1/mean(xexp)


