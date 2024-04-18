#' ---
#' title: "Boulder Passing"
#' output: html_document
#' date: "2024-03-22"
#' Author: G. Piton, C. Misset, H. Shirra
#' ---
#' This is an annotated version of the BoulderPassing.R script prepared by H. Shirra, originally developed by G. Piton and C. Misset as part of a larger script to stochastically simulate jamming through a series of constrictions during a debris flow event. This code randomly generates the either just the diameter value (BoulderSizing function) or the number of boulders passing through constrictions as well as their diameter using the user-defined probabilities in a binomial law (see original paper Piton, Goodwin et al. 2022). The results are fed into 00_MainCode.R.
#' 
#' ### Summary of Script
#' This script creates a function to stochastically determine how many boulders pass through a structure, given the volume of debris passing on the one hand, and the size and average number of the boulders, on the other hand. For each boulder class and opening, the code first slice the volume of debris flow in a number of packets having the average size of a boulder and randomly count the number of 'true' boulders (binomial sampling) and the number of 'false' boulders that is mud or boulders of lower size. The results are recorded within a data frame. This procedure is repeated under all boulder classes have been simulated, and the entire event volume is accounted for.
#' 
#' ### Script
## ----comment= "#", echo=FALSE-------------------------------------------------
#Transfer the .Rmd file to R automatically when it is knit. 
#knitr::purl(input = "BoulderPassing.Rmd", documentation = 2) #comment this line prior to running generated R script.

#' This first sub-routine randomly generate the number and size of the boulders. It is used if we only know the probabilities of the boulders but not their actual numbers (see the next sub-routine in this case).
## -----------------------------------------------------------------------------
###Randomly generate the number of boulder passing
#V1 with constant value of boulder probability p
#V2 with value of probability p possibly varying with time

###BOULDER PASSING FUNCTION####
#This function simulates the process of boulders passing through a given volume of debris flow, calculating the number and size of boulders that can pass, based on the surge volume, and the probability of boulder passage. It is used for non-instantaneous transfer between structures where MIXING occurs. Number of boulders is recomputed as the simulation progresses and mixing occurs, probabilities of boulders is known. 

BoulderPassing<-function(Volume.Surge #Initialize function and call in parameters
                         ,Diameter_min
                         ,Diameter_max
                         ,Boulder.probabilities)
{
  Boulder.list<-data.frame(D=NA,Class=NA) #Initialize data frame where the diameter and class of boulders can be stored.
  #Maximum number of boulder that may pass considering the volume of the surge (divide the surge volume into distinct packets with the same volume as the boulder).
  N.max<-round(Volume.Surge/(1/6*(0.5*(Diameter_min+Diameter_max))^3*pi),0)

# Check if the minimum diameter of boulder in class can pass through opening. If so, randomly sample the number of boulders that pass from the user-defined binomial distribution. The range of boulder diameters are pre-defined by the user. The diameter of the boulder is randomly sampled from the pre-defined range of diameters. Repeat for all boulder classes and the entire event volume, and record results. 

  for(Boulder.Ind in (1:length(Diameter_min))) #for all boulder classes 
  { if(N.max[Boulder.Ind]>0) #If at least one boulder of a given class may pass (N.max>1), pick a random number of boulder of size D 
  {
    # According to a binomial law with N.max randomly sample
    N.boulderPassing<-rbinom(1,N.max[Boulder.Ind],Boulder.probabilities[Boulder.Ind])
    if((!is.na(N.boulderPassing) && N.boulderPassing>0)){ 
      #V0
      # N.Diam.boulder<-rep(Boulders.Diameters[Boulder.Ind], #Add the diameter value
      # N.boulderPassing)#of the randomly sampled positive values
      #V1
      #Randomly generate the diameter of the boulders that pass from the range of the minimum diameter to the maximum diameter for that class
      N.Diam.boulder<-runif(N.boulderPassing,Diameter_min[Boulder.Ind],Diameter_max[Boulder.Ind])
      #Reduce the surge volume by the boulders detected
      #V0 all boulders were of the same size
      # Volume.Surge<-Volume.Surge-N.boulderPassing*(1/6*(0.5*(Diameter_min+Diameter_max))^3*pi)[Boulder.Ind]
      #V1 compute
      Volume.Surge<-Volume.Surge-sum(pi/6*N.Diam.boulder^3)
    }else{N.Diam.boulder<-NA}#If all boulders are too large to pass, do not randomly generate boulder diameter
    
    if( (is.na(Boulder.list$D[1])  && !is.na(N.Diam.boulder[1])) )#Initialize the boulder list, with the previously generated boulder diameters and boulder classes 
    {Boulder.list<-data.frame(D=N.Diam.boulder,Class=rep(Boulder.Ind,N.boulderPassing))
    }else{
      if( (!is.na(Boulder.list$D[1])  && !is.na(N.Diam.boulder[1])) ){ #Append the boulder list, if the data frame has already been initialized
        Boulder.list<-rbind(Boulder.list,data.frame(D=N.Diam.boulder,Class=rep(Boulder.Ind,N.boulderPassing)))}
    }
  }
  }#end of the boulder loop
  return(Boulder.list)
}

# BoulderPassing(100,Boulders$Diameter_min,Boulders$Diameter_max,rep(0.1,length(Boulders$Boulder_size_category_.m.)))

#' 
#' This second sub-routine randomly generate only the diameter of the boulders knowing their numbers and the size class to which they belong. 
## -----------------------------------------------------------------------------
###BOULDER SIZING FUNCTION####
#This function computes the number and size of boulders based on user input - it is used for INSTANTANEOUS transfer between structures. Number of boulders is not recomputed in between structures.

BoulderSizing<-function(Boulder.number)
{
  #Count number of boulders 
  for(i in (1:length(Boulders[,1]))) #For all boulder classes
  {
    Boulders$Number[i]<-input[6+i] #Create vector containing number of boulders for each class
  }
  Boulder.list<-data.frame(D=NA,Class=NA)
  for(Boulder.Ind in (1:length(Boulders[,1]))) #Generate diameter of boulders for each class
  {
    Boulder.list<-rbind(Boulder.list,data.frame(D=runif(as.numeric(Boulder.number[Boulder.Ind])
                                                        ,Boulders$Diameter_min[Boulder.Ind]
                                                        ,Boulders$Diameter_max[Boulder.Ind])
                                                  ,Class=rep(Boulder.Ind,Boulder.number[Boulder.Ind])))
  }
  if(length(Boulder.list>1)){Boulder.list<-Boulder.list[2:length(Boulder.list$D),] #Remove empty first row of boulder data
    return(Boulder.list) #Return vector
  }
}

