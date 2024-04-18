#' ---
#' title: "Boulder Transfer"
#' output: html_document
#' date: "2024-03-22"
#' Author: G. Piton, C. Misset, H. Shirra
#' ---
#' This is an annotated version of the BoulderTransfer.R script prepared by H. Shirra, originally developed by G. Piton and C. Misset as part of a larger script to stochastically simulate jamming through a series of constrictions during a debris flow event. This code routes stochastically generated boulders through a series of structures. The results are fed into 00_MainCode.R.
#' 
#' ### Summary of Script
#' This script defines a function which transfers boulders between structures. Boulder transfer between structures is defined as either "instantaneous", wherein there is no possibility of deposition between structures, and therefore no change in boulder positioning, or "mixing" wherein possible deposition of a certain volume of debris flow might occur between the two structures, and thus a random sampling of the boulder number is performed at the downstream structure.The probability of each class of boulder to appear is in this case, driven by the number of boulders of each class that were discharged from structures. More detailed information can be found in the Msc Thesis of H. Shirra. 
#' 
#' ### Script
## ----comment= "#", echo=FALSE-------------------------------------------------
#Transfer the .Rmd file to R automatically when it is knit. 
# knitr::purl(input = "BoulderTransfer.Rmd", documentation = 2) #comment this line prior to running generated R script.

#' 
## -----------------------------------------------------------------------------
#Define transfer type
TransferType <- "instantaneous"
# TransferType <- "Mixing 5000"
# Vmixing <- sum(Qo$Qo)/20

Transfer_Between_Structure<-function(Qo,TransferType,Vmixing)
{
  #Count the number of classes of boulders
  N_BoulderClass<-0.5*sum(substr(names(Qo),1,5)=="Class")
  
  Qnext<- Qo %>% 
    #Keep only the data needed (time, flow, and unjammed boulders)
    select(Time,Qo,paste0("Class",(1:N_BoulderClass),".unjammed"))
  
  #Add column for probabilities that are filled by NA so fit for "instantaneous" transfer mode
  Qnext <- cbind(Qnext,data.frame(matrix(NA,nrow=length(Qnext$Time),ncol=N_BoulderClass)))
  
  #Rename the data.frame to have the same names as the inlet input
  names(Qnext)<-c("Time","Q"
                  ,paste0("N",(1:N_BoulderClass))
                  ,paste0("p",(1:N_BoulderClass)))
  
  Qnext <- Qnext %>% 
    #Compute time difference between two time step
    mutate(dT = Time-c(0,Qnext$Time[1:(length(Qnext$Time)-1)])) %>%
    #Compute cumulative volume passing
    mutate(Vo = cumsum(Q * dT)) %>%
    #Initialize index such that Vo[MixingStart:i] = Vmixing
    mutate(MixingStart = NA)

  #If mixing is needed, the probability of boulder is recomputed over the moving window 
  # such that the volume passing is Vmixing
  if(substr(TransferType,1,6) == "Mixing")
  {
    #Mean boulder size of each class used to compute the typical volume of a boulder
    Boulders$Diameter<-0.5*(Boulders$Diameter_min+Boulders$Diameter_max)
    #Elementary volume of each boulder class
    Boulders$V<-pi/6*Boulders$Diameter^3
    
    for(i in (1:length(Qnext$Time)))
    { # Screen the whole time series of volume to find the time window 
      # MixingStart : i over which the number of boulders should counted
      # considering that a volume Vmixing is discharged over this specific 
      # time window : Vo[MixingStart:i] = Vmixing
      Qnext$MixingStart[i]<-max(1,i-max(which((Qnext$Vo[i]-Vmixing-Qnext$Vo[i:1])<0)))
      
      #Compute the precise volume of debris flow passing over the mixing period
      Vpassing <-Qnext$Vo[i]-Qnext$Vo[Qnext$MixingStart[i]]
      
      
      for(j in (1:N_BoulderClass))
      {
        #Find the relevant columns for each boulder class
        Prob_ColumnIndex<-which(names(Qnext)==paste0("p",j))
        Numb_ColumnIndex<-which(names(Qnext)==paste0("N",j))
        
        #Compute the number of boulder passing during the mixing time
        Npassing <- sum(Qnext[Qnext$MixingStart[i]:i,Numb_ColumnIndex])
        
        if(i==1){
          Qnext[i,Prob_ColumnIndex]<-0
        }else{
          if(Vpassing > 0)
          {
            #For each class of boulder, compute the probability (in case of mixing)
            Qnext[i,Prob_ColumnIndex] <-min(1,Npassing*Boulders$V[j]/Vpassing)   
          }else{Qnext[i,Prob_ColumnIndex]<-0}
        }
      }
      
    }
    #Once the times series has been treated for all boulder classes, 
    #their actual number is erased
    for(j in (1:N_BoulderClass))
    {
      #Find the relevant columns for each boulder class
      Numb_ColumnIndex<-which(names(Qnext)==paste0("N",j))
      Qnext[,Numb_ColumnIndex] <-NA
    }
  }
  #Remove temporary variables
  Qnext<- Qnext %>% select(Time,Q
                           ,paste0("p",(1:N_BoulderClass))
                           ,paste0("N",(1:N_BoulderClass)))
  
  return(Qnext)
}

