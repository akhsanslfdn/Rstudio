#gamma dist

gamma.NR<-function(x,alpha,betta){
  n<-length(x)
  #input turunan kedua
  Jaa=-n*trigamma(alpha)
  Jbb=-n*alpha/(betta^2)
  Jab=n/betta
  J=rbind(c(Jaa,Jab),c(Jab,Jbb))
  Jinv=solve(J)
  
  #input turunan pertama
  Ga=n*log(betta)-n*digamma(alpha)+sum(log(x))
  Gb=n*alpha/betta-sum(x)
  G=c(Ga,Gb)
  
  #input parameter
  para=c(alpha,betta)
  parabaru=para-Jinv%*%G
}


#input inisial value
set.seed(1)
x=rgamma(100,2,3)
alpha0=mean(x)^2/var(x)
betta0=mean(x)/var(x)
x
alpha0
betta0

#NR
old=c(alpha0,betta0)
iters=NULL
iterasi=rbind(old,iters)
new=c(0,0)
diff=sum(abs(new-old))
while(diff>0.000001){
  new=gamma.NR(x,old[1],old[2])
  iterasi=rbind(iterasi,c(new[1],new[2]))
  diff=sum(abs(new-old))
  old=new
}
iterasi