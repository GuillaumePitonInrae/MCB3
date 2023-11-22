### Function routing the boulder numbers or probability to the 
#next structure depending on the user choice
#V0.1 Aug 2023 - G. Piton & C. Misset

TransferType <- "instantaneous"
# TransferType <- "Mixing 5000"
# Vmixing <- sum(Qo$Qo)/20

Transfer_Between_Structure<-function(Qo,TransferType,Vmixing)
{
  #Count the number of class of boulders
  N_BoulderClass<-0.5*sum(substr(names(Qo),1,5)=="Class")
  
  Qnext<- Qo %>% 
    #Keep only the data needed
    select(Time,Qo,paste0("Class",(1:N_BoulderClass),".unjammed"))
  
  # Add column for probabilities that are filled by NA so fit for "instantaneous" transfer mode
  Qnext <- cbind(Qnext,data.frame(matrix(NA,nrow=length(Qnext$Time),ncol=N_BoulderClass)))
  
  #rename the data.frame to have the same names than an inlet input
  names(Qnext)<-c("Time","Q"
                  ,paste0("N",(1:N_BoulderClass))
                  ,paste0("p",(1:N_BoulderClass)))
  
  Qnext <- Qnext %>% 
    #Compute time difference between two time step
    mutate(dT = Time-c(0,Qnext$Time[1:(length(Qnext$Time)-1)])) %>%
    #Compute cumulated volume passing
    mutate(Vo = cumsum(Q * dT)) %>%
    # Initialize index such that Vo[MixingStart:i] = Vmixing
    mutate(MixingStart = NA)

  #If mixing is needed, the probability of boulder is recomputed over the moving window 
  # such that the volume passing is Vmixing
  if(substr(TransferType,1,6) == "Mixing")
  {
    #Mean boulder size of each class used to compute the typucal volume of a boulder
    Boulders$Diameter<-0.5*(Boulders$Diameter_min+Boulders$Diameter_max)
    #Elementary volume of each boulder class
    Boulders$V<-pi/6*Boulders$Diameter^3
    
    for(i in (1:length(Qnext$Time)))
    { #screen the whole time series of volume to find the time window 
      # MixingStart : i over which the number of boulders should counted
      # considering that a volume Vmixing is discharged over this specific 
      # time window : Vo[MixingStart:i] = Vmixing
      Qnext$MixingStart[i]<-max(1,i-max(which((Qnext$Vo[i]-Vmixing-Qnext$Vo[i:1])<0)))
      
      #Compute the precise volume of debris flow passing over the mixing period
      Vpassing <-Qnext$Vo[i]-Qnext$Vo[Qnext$MixingStart[i]]
      
      
      for(j in (1:N_BoulderClass))
      {
        #Find the relevant columns for each boudler class
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
    # their actual number is erased
    for(j in (1:N_BoulderClass))
    {
      #Find the relevant columns for each boudler class
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
