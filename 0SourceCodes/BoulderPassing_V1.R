
BoulderPassing<-function(Volume.Surge,Boulders)
{
  Boulder.list<-NA
  #Maximum number of boulder that may pass considering the volume of the surge
  N.max<-round(Volume.Surge/Boulders$V,0)
  
  for(Boulder.Ind in (1:length(Boulders$Diameter)))
  { if(N.max[Boulder.Ind]>0)
  {#If at least one boulder may pass (N.max>1), pick up a random number of boulder of size D 
    # according to a binomial law with N.max random sampling
    N.boulderPassing<-rbinom(1,N.max[Boulder.Ind],Boulders$P[Boulder.Ind])
    if((!is.na(N.boulderPassing) && N.boulderPassing>0)){
      #V0
      # N.Diam.boulder<-rep(Boulders$Diameter[Boulder.Ind], #Add the diameter value
      # N.boulderPassing)#of the randomly sampled positive values
      #V1
      # Add N.BoulderPassing of diameter in the range Dmin - Dmax of this category
      N.Diam.boulder<-runif(N.boulderPassing,Boulders$Dmin[Boulder.Ind],Boulders$Dmax[Boulder.Ind])
      #Reduce the surge volume by the boulders detected
      #V0 all boulders were of the same size
      # Volume.Surge<-Volume.Surge-N.boulderPassing*Boulders$V[Boulder.Ind]
      #V1 compute
      Volume.Surge<-Volume.Surge-sum(pi/6*N.Diam.boulder^3)
    }else{N.Diam.boulder<-NA}
    
    if( (is.na(Boulder.list)  && !is.na(N.Diam.boulder)) )#Initialize the boulder list
    {Boulder.list<-N.Diam.boulder
    }else{
      if( (!is.na(Boulder.list)  && !is.na(N.Diam.boulder)) ){ #Append the boulder list
        Boulder.list<-c(Boulder.list,N.Diam.boulder)}
    }
  }
  }#end of the boulder loop
  return(Boulder.list)
}
# BoulderPassing(100,Boulders)
