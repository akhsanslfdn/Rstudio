an<-function (i,m,t,k)
{
  j=i/m
  n=m*t
  v=(1+j)^-1
  an=k*(1-(v^n))/(j)
  
  sn=k*((1+j)^n-1)/(j)
  return(c(an,sn))
}