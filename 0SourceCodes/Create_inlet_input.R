### Create input files with main parameters used as upstream supply
#V0 July 2023 - G. Piton & C. Misset

Create_inlet_input<-function(EventName,AdjustEventManually,Structures,Boulders)
{
  
  # Find the event in the table
  MagnitudeClass<-which(Events$Name==EventName)
  Vevent<-Events$Volume_BestEstimate[MagnitudeClass]
  
  #Define the input data for the run
  input<-c(
    #Event definition
    Vevent/10^3  #Volume * 1000 m3
    ,Events$PeakDischarge_BestEstimate[MagnitudeClass]#Qpeak m3/s,
    ,Events$TimeLag_BestEstimate[MagnitudeClass] #(s) Peak lag
    ,Events$DepositionSlope_BestEstimate[MagnitudeClass] #Deposition slope (%)
  )
  #Initial state of the basin filling and jamming
  for(Structure_Ind in (1:length(Structures$Name)))
  {
    #Initial deposit height (m)
    InitialDeposit<-Structures$InitialConditions$DepositHeight_BestEstimate[which(Structures$Rank==Structure_Ind)]
    if(is.na(InitialDeposit)){InitialDeposit<-0}
    #Jam at the slit base by large wood  or boulders(m)
    InitialJam<-Structures$InitialConditions$JammingHeight_BestEstimate[which(Structures$Rank==Structure_Ind)]
    if(is.na(InitialJam)){InitialJam<-0}
    
    input<-c(input ,InitialDeposit ,InitialJam)
  }
  
  # Adding of the number of Boulders
  for(j in (1:dim(Boulders)[1]))
  {
    input<-c(input,Boulders$Number_BestEstimate[j])
  }
  
  #Adjust values manually
  #Pop up windows to define the values
  if(AdjustEventManually){
    #Define the names for the pop-up windows
    InputDataName<-c("Volume (*1000 m3)"
                       ,"Qpeak (m3/s)"
                       ,"Peak lag (-)"
                       ,"Deposition slope (%)")
                       
    InputDataName<-c(InputDataName
                     ,paste0(c("Initial deposit height at structure "
                               ,"Initial jam height at structure ")
                             ,sort(rep(1:length(Structures$Name),2)))
                     ,paste0("Number of boulders of diameter "
                                              ,Boulders$Diameter_min,"-"
                                              ,Boulders$Diameter_max
                                              ," m in a volume of "
                                              ,Boulders$ReferenceVolume," m3 of deposit"))
    for(j in c(1:length(input)))
    {
      input[j]<-as.numeric(dlg_input(message = paste("Please provide the value of parameter \n",InputDataName[j])
                                     , default = input[j])$res)
      if(!is.na(input[j]))
         {
           if(input[j]<0)
           {input[j]<-NA}
           }
    }
    
    if(sum(is.na(input))>0)
    {
      RedefineValue_Ind<-which(is.na(input))
      
      
      for(j in RedefineValue_Ind)
      {
        input[j]<-as.numeric(dlg_input(message = paste("The value you provided for parameter",InputDataName[j]
                                                       ,"was not acceptable (positive number),"
                                                       ,"Please provide the value of parameter")
                                       , default = input[j])$res)
      }
    }
  }
  return(input)
}



