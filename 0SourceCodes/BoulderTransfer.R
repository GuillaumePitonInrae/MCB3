### Function routing the boulder numbers or probability to the 
#next structure depending on the user choice
#V0.1 Aug 2023 - G. Piton & C. Misset

# Transfer.Type <- "Instantaneous"
# Transfer.Type <- "Mixing"
# Vmixing <- sum(Qo$Qo)/20

Transfer_Between_Structure<-function(Qo,Transfer.Type,Vmixing)
{
  #Count the number of class of boulders
  N.boulder.class<-0.5*sum(substr(names(Qo),1,5)=="Class")
  
  Qnext<- Qo %>% 
    #Keep only the data needed
    select(T,Qo,paste0("Class",(1:N.boulder.class),".unjammed"))
  
  # Add column for probabilities that are filled by NA so fit for "instantaneous" transfer mode
  Qnext <- cbind(Qnext,data.frame(matrix(NA,nrow=length(Qnext$T),ncol=N.boulder.class)))
  
  #rename the data.frame to have the same names than an inlet input
  names(Qnext)<-c("T","Q"
                  ,paste0("N",(1:N.boulder.class))
                  ,paste0("p",(1:N.boulder.class)))
  
  Qnext <- Qnext %>% 
    #Compute time difference between two time step
    mutate(dT = T-c(0,Qnext$T[1:(length(Qnext$T)-1)])) %>%
    #Compute cumulated volume passing
    mutate(Vo = cumsum(Q * dT)) %>%
    # Initialize index such that Vo[Mixing.start:i] = Vmixing
    mutate(Mixing.start = NA)

  #If mixing is needed, the probability of boulder is recomputed over the moving window 
  # such that the volume passing is Vmixing
  if(Transfer.Type == "Mixing")
  {
    #recompute the volume of the boulders
    for(j in (1:N.boulder.class))
    {
      Boulders$Dmin[j]<-as.numeric(substr(Boulders[j,1],1,(stringr::str_locate(Boulders[,1],"-")[j,1]-1)))
      Boulders$Dmax[j]<-as.numeric(substring(Boulders[j,1],(stringr::str_locate(Boulders[,1],"-")[j,1]+1)))
    }
    #Mean boulder size of each class used to compute the typucal volume of a boulder
    Boulders$Diameter<-0.5*(Boulders$Dmin+Boulders$Dmax)
    #Elementary volume of each boulder class
    Boulders$V<-pi/6*Boulders$Diameter^3
    
    for(i in (1:length(Qnext$T)))
    { #screen the whole time series of volume to find the time window 
      # Mixing.start : i over which the number of boulders should counted
      # considering that a volume Vmixing is discharged over this specific 
      # time window : Vo[Mixing.start:i] = Vmixing
      Qnext$Mixing.start[i]<-max(1,i-max(which((Qnext$Vo[i]-Vmixing-Qnext$Vo[i:1])<0)))
      
      #Compute the precise volume of debris flow passing over the mixing period
      Vpassing <-Qnext$Vo[i]-Qnext$Vo[Qnext$Mixing.start[i]]
      
      for(j in (1:N.boulder.class))
      {
        #Find the relevant columns for each boudler class
        Prob.column.index<-which(names(Qnext)==paste0("p",j))
        Numb.column.index<-which(names(Qnext)==paste0("N",j))
        
        #Compute the number of boulder passing during the mixing time
        Npassing <- sum(Qnext[Qnext$Mixing.start[i]:i,Numb.column.index])
        
        if(i==1){
          Qnext[i,Prob.column.index]<-0
        }else{
          #For each class of boulder, compute the probability (in case of mixing)
          Qnext[i,Prob.column.index] <-Npassing*Boulders$V[j]/Vpassing   
        }
      }
    }
    #Once the times series has been treated for all boulder classes, 
    # their actual number is erased
    for(j in (1:N.boulder.class))
    {
      #Find the relevant columns for each boudler class
      Numb.column.index<-which(names(Qnext)==paste0("N",j))
      Qnext[,Numb.column.index] <-NA
    }
  }
  #Remove temporary variables
  Qnext<- Qnext %>% select(T,Q
                           ,paste0("p",(1:N.boulder.class))
                           ,paste0("N",(1:N.boulder.class)))
  
  return(Qnext)
}
