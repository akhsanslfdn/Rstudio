Volatilitas<-function(saham)
{
  n=length(saham)
  ret=array(0,n-1)
  
  for (i in 1 : (n-1))
  {
    ret[i]=log(saham[i+1]/saham[i])
  }
  sim_baku = sd(ret)
  vol = sim_baku*sqrt(n)
  
  return(vol)
}
