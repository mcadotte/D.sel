###interate the selection algorithm to calculate average 
###priority and CI


probilizer <- function(SxT, rho = 1, c = 1, fuzzy = 1,iter=999){
  first<-D.sel(SxT,rho,c,fuzzy)
  all<-first 
  
  for (i in 1:iter){
    tmp<-D.sel(SxT,rho,c,fuzzy)
    all[,i+1]<-tmp[match(rownames(tmp),rownames(first)),]
  }
  
  CI95 <- function(n, mean, sd) {
    e <- qnorm(0.975)*sd/sqrt(n)
    return(c("mean"=mean,"lower" = mean - e, "upper" = mean + e))
  }
  
  out<-apply(all,1,function(x){
    CI95(length(x),mean(x),sd(x))
  })
  out<-t(out)
  out<-data.frame(Species=row.names(out),Mean=out[,1],Lower=out[,2],Upper=out[,3])
  return(out)
}