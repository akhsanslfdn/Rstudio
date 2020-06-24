amortisasi <- function (an,i,m,t)
{
  #an = pokok hutang / pv
  #t tahun
  #m banyak pembayaran bunga dalam setahun
  #i bunga
  #k cicilan,boleh diisi atau tidak;|kalau diisi NULL
  n = m*t
  j = i/m
  v = (1+j)^-1
  k = an*j/(1-v*n)
  
  for(t in i:n)
  {
  B = k*(1-v^(n-t+1))
  C = k*v^(n-t+1)
  D = k*(1-v^(n-t)/j)
 
  cat (B,C,D) 
  }
}