###Randomly generate the number of boulder passing
#V1 with constant value of boulder probability p
#V2 with value of probability p possibly varying with time

BoulderPassing<-function(Volume.Surge
                         ,Diameter_min
                         ,Diameter_max
                         ,Boulder.probabilities)
{
  Boulder.list<-data.frame(D=NA,Class=NA)
  #Maximum number of boulder that may pass considering the volume of the surge
  N.max<-round(Volume.Surge/(1/6*(0.5*(Diameter_min+Diameter_max))^3*pi),0)
  
  for(Boulder.Ind in (1:length(Diameter_min)))
  { if(N.max[Boulder.Ind]>0)
  {#If at least one boulder may pass (N.max>1), pick up a random number of boulder of size D 
    # according to a binomial law with N.max random sampling
    N.boulderPassing<-rbinom(1,N.max[Boulder.Ind],Boulder.probabilities[Boulder.Ind])
    if((!is.na(N.boulderPassing) && N.boulderPassing>0)){
      #V0
      # N.Diam.boulder<-rep(Boulders.Diameters[Boulder.Ind], #Add the diameter value
      # N.boulderPassing)#of the randomly sampled positive values
      #V1
      # Add N.BoulderPassing of diameter in the range Diameter_min - Diameter_max of this category
      N.Diam.boulder<-runif(N.boulderPassing,Diameter_min[Boulder.Ind],Diameter_max[Boulder.Ind])
      #Reduce the surge volume by the boulders detected
      #V0 all boulders were of the same size
      # Volume.Surge<-Volume.Surge-N.boulderPassing*(1/6*(0.5*(Diameter_min+Diameter_max))^3*pi)[Boulder.Ind]
      #V1 compute
      Volume.Surge<-Volume.Surge-sum(pi/6*N.Diam.boulder^3)
    }else{N.Diam.boulder<-NA}
    
    if( (is.na(Boulder.list$D[1])  && !is.na(N.Diam.boulder[1])) )#Initialize the boulder list
    {Boulder.list<-data.frame(D=N.Diam.boulder,Class=rep(Boulder.Ind,N.boulderPassing))
    }else{
      if( (!is.na(Boulder.list$D[1])  && !is.na(N.Diam.boulder[1])) ){ #Append the boulder list
        Boulder.list<-rbind(Boulder.list,data.frame(D=N.Diam.boulder,Class=rep(Boulder.Ind,N.boulderPassing)))}
    }
  }
  }#end of the boulder loop
  return(Boulder.list)
}
# BoulderPassing(100,Boulders$Diameter_min,Boulders$Diameter_max,rep(0.1,length(Boulders$Boulder_size_category_.m.)))

#Compute the number and size of boulder at a given time step
BoulderSizing<-function(Boulder.number)
{
  #Count number of boulders 
  for(i in (1:length(Boulders[,1])))
  {
    Boulders$Number[i]<-input[6+i]
  }
  Boulder.list<-data.frame(D=NA,Class=NA)
  for(Boulder.Ind in (1:length(Boulders[,1])))
  {
    Boulder.list<-rbind(Boulder.list,data.frame(D=runif(as.numeric(Boulder.number[Boulder.Ind])
                                                        ,Boulders$Diameter_min[Boulder.Ind]
                                                        ,Boulders$Diameter_max[Boulder.Ind])
                                                  ,Class=rep(Boulder.Ind,Boulder.number[Boulder.Ind])))
  }
  if(length(Boulder.list>1)){Boulder.list<-Boulder.list[2:length(Boulder.list$D),]
    return(Boulder.list)
  }
}
