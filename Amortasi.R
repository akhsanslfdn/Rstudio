amortisasi <- function (an,i,m,t,k=FALSE)
{
  #an : pokok hutang / pv
  
  n = m*t
  j = i/m
  v = (1+j)^-1
  
  if(is.null(an)==FALSE)
  {
  k = an*j/(1-v^n)
  }
  
  cat('Time  Payment    Interest        Principal       Balance')
  cat('\n')
  
  
  for(t in 1:n)
  {
  B = k*(1-v^(n-t+1))
  C = k*v^(n-t+1)
  D = k*(1-v^(n-t))/j
 
  cat (t,'\t',k,'\t',B,'\t',C,'\t',D,'\t','\t') 
  cat ('\n') #newline
  
  }
}