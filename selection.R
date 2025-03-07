##trait based prioritization and selection
require(FD)
require(nnspat)
require(gawdis)


#1.prioritization function####

#SxT = species x trait matrix; species names are row names 
#rho = preference for species, with 1 as high preference and 0 as not wanted, if 
   #no preference, automatically make all 1s, in a named vector with species names same as species rown names in SxT
#c = cost of species, if no costs, no preference, automatically make all 1s,
   #in a named vector with species names same as species rown names in SxT
#fuzzy = number of significant digits for distances, e.g., 1 means 0.3675 becomes 0.4, 2 means 0.37. 
  ##this creates equivalencies allowing multiple similar selections



D.sel<-function(SxT, rho, c, fuzzy){

  dister<-function(x,rho,c,fuzzy,dc=0){
    out<-NULL
    for (i in 1:ncol(x)){
      out[i]<-round((sum(x[,i])/(ncol(x)-1)*rho[i])/((1-dc[i])*c[i]),fuzzy)
    }
    names(out)<-colnames(x)
    return(out)
  }
  
  selecter<-function(x,n=1){
    return(sample(names(x[x==max(x)]),n))
  }
  
  if (length(rho) == 1) {
    rho <- rep(1,nrow(SxT))
    names(rho)<-row.names(SxT)
  }
  
  if (length(rho) > 1){
    rho <-rho
    rho<-rho[match(rownames(SxT),names(rho))]
  } 
  
  if (length(c) == 1) {
    c <- rep(1,nrow(SxT))
    names(c)<-row.names(SxT)
  }
  
  if (length(c) > 1) {
    c <-c
    c<-c[match(rownames(SxT),names(c))]
    c<-c/max(c)
  }
   
    ds.pool <- gowdis(SxT)
    ds.pool <- dist2full(ds.pool)
    colnames(ds.pool)<-row.names(SxT)

    sp.ds<-dister(ds.pool,rho,c,fuzzy,dc=rep(0,ncol(ds.pool))) #calc distinctiveness with fuzzy rounding
    
    sp.sel<-selecter(sp.ds)#randomly select one of these species
    
    sub.SxT<-SxT[row.names(SxT)!=sp.sel,]
    
    for (i in 1:(nrow(sub.SxT)-1)){
      rho.tmp<-rho[match(row.names(sub.SxT),names(rho))]
      c.tmp<-c[match(row.names(sub.SxT),names(c))]
      
      #pool
      sub.ds.pool <- gowdis(sub.SxT)
      sub.ds.pool <- dist2full(sub.ds.pool)
      colnames(sub.ds.pool)<-row.names(sub.SxT)
      
      #comm
      comm.tmp<-SxT[match(sp.sel,row.names(SxT)),]
      
      sub.ds.comm <-gowdis(rbind(comm.tmp,sub.SxT))
      sub.ds.comm<-dist2full(sub.ds.comm)
      sub.ds.comm<-1-sub.ds.comm
      sub.ds.comm<-as.matrix(sub.ds.comm)
      colnames(sub.ds.comm)<-c(row.names(comm.tmp),row.names(sub.SxT))
      
      ###have similarity matrix, need to create Dc vector, which is average
      #of the species in comm.tmp (first n rows), looped across all species in
      #sub.SxT
      
      Dc<-sub.ds.comm[nrow(comm.tmp),(nrow(comm.tmp)+1):ncol(sub.ds.comm)]
      Dc.ave<-sapply(Dc,mean)
      
      ##calculate full weights
      sp.ds.pool<-dister(sub.ds.pool,rho.tmp,c.tmp,fuzzy,dc=Dc.ave)
      sp.sel[i+1]<-selecter(sp.ds.pool)
      
      sub.SxT<-SxT[is.na(match(row.names(SxT),sp.sel)),]         
    }
  #end of for loop  
  
 sp.sel[length(sp.sel)+1]<-rownames(sub.SxT)
  sp.out<-data.frame(row.names=sp.sel,inc.order=1:length(sp.sel))  
  return(sp.out)
}
