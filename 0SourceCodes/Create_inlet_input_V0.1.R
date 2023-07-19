### Create input files with main parameters used as upstream supply
#V0 July 2023 - G. Piton & C. Misset

Create_inlet_input_V0.1<-function(Event.name,Adjust.event.manually)
{
  
  # Find the event in the table
  Magnitude.class<-which(Events$Name==Event.name)
  Vevent<-Events$Volume_BestEstimate[Magnitude.class]
  
  #Define the input data for the run
  input<-c(
    #Event definition
    Vevent/10^3  #Volume * 1000 m3
    ,Events$PeakDischarge_BestEstimate[Magnitude.class]#Qpeak m3/s,
    ,Events$TimeLag_BestEstimate[Magnitude.class] #(s) Peak lag
    ,Events$DepositionSlope_BestEstimate[Magnitude.class] #Deposition slope (%)
    #Initial state of the basin filling and jamming
    ,InitialConditions$InitialDepositHeight_BestEstimate[1] #Initial deposit height (m)
    ,InitialConditions$InitialJammingHeight_BestEstimate[1]  #Jam at the slit base by large wood (m)
  )
  # Adding of the number of Boulders
  for(j in (1:length(Boulders[,1])))
  {
    input<-c(input,Boulders$Best_estimate[j])
  }
  
  #Adjust values manually
  #Pop up windows to define the values
  if(Adjust.event.manually){
    #Define the names for the pop-up windows
    input.data.name<-c("Volume (*1000 m3)"
                       ,"Qpeak (m3/s)"
                       ,"Peak lag (-)"
                       ,"Deposition slope (%)"
                       ,"Initial deposit height (m)"
                       ,"Jam at the slit base by large wood or boulders (m)")
    input.data.name<-c(input.data.name,paste0("Number of boulders of diameter "
                                              ,Boulders$Boulder_size_category_.m.
                                              ," m in a volume of "
                                              ,Boulders$Reference_Volume," m3 of deposit"))
    for(j in c(1:length(input)))
    {
      input[j]<-as.numeric(dlg_input(message = paste("Please provide the value of parameter \n",input.data.name[j])
                                     , default = input[j])$res)
    }
  }
  return(input)
}



