#' ---
#' title: "Create Inlet Input"
#' output: html_document
#' date: "2024-03-27"
#' Author: G. Piton, C. Misset, H. Shirra
#' ---
#' 
#' This is an annotated version of the Create_inlet_input.R  script prepared by H. Shirra, originally developed by G. Piton and C. Misset as part of a larger script to stochastically simulate jamming through a series of constrictions in a debris flow event. This script creates the main input parameters describing inflow to the simulated structures. The input files created by this script are called upon by the main code (00_MainCode). 
#' 
#' ### Summary of Script
#' This script creates a function to define the event input parameters.The user-specified event is matched to the frequency-magnitude data, and the volume of the event is calculated. All input variables (event volume, peak discharge, peak lag, and deposition slope) are imported, and combined into a single vector. The initial state of the basin (i.e. filling and jamming prior to the modeled event) is defined for all structures included in the analysis, and added to the input vector. The best estimate for the number of boulders is also added to the input data. Input data can be adjusted manually by the user via pop-up windows throughout the simulation. 
#' 
#' ### Script
#' 
## ----comment= "#", echo=FALSE-------------------------------------------------
#Transfer the .Rmd file to R automatically when it is knit. 
#knitr::purl(input = "Create_inlet_input.Rmd", documentation = 2) #Comment this line prior to running generated R script.

#' 
## -----------------------------------------------------------------------------
Create_inlet_input<-function(EventName,AdjustEventManually,Structures,Boulders)
{
#The user-specified event is matched to the user-defined frequency-magnitude data, and the volume of the event is calculated.

  # Find the event in the table
  MagnitudeClass<-which(Events$Name==EventName)
  Vevent<-Events$Volume_BestEstimate[MagnitudeClass]

  input<-c(
    #Event definition
    Vevent/10^3  #Volume * 1000 m3
    ,Events$PeakDischarge_BestEstimate[MagnitudeClass]#Qpeak m3/s,
    ,Events$TimeLag_BestEstimate[MagnitudeClass] #(s) Peak lag
    ,Events$DepositionSlope_BestEstimate[MagnitudeClass] #Deposition slope (%)
  )

#The initial state of the basin (i.e. filling and jamming prior to the modeled event) is defined for all structures included in the analysis, and added to the input vector.  
  
  #Initial state of the basin filling and jamming
  for(Structure_Ind in (1:length(Structures$Name)))
  {
    #Initial deposit height (m)
    InitialDeposit <- Structures$InitialConditions$DepositHeight_BestEstimate[which(Structures$Rank==Structure_Ind)]
    if(is.null(InitialDeposit)){InitialDeposit<-0}
    #Jam at the slit base by large wood  or boulders(m)
    InitialJam <- Structures$InitialConditions$JammingHeight_BestEstimate[which(Structures$Rank==Structure_Ind)]
    if(is.null(InitialJam)){InitialJam<-0}
    
    input<-c(input ,InitialDeposit ,InitialJam)
    #Remove superfluous variables from the environment
    rm(InitialDeposit,InitialJam)
  }

#The best estimate for the number of boulders is added to the input data
  
  # Adding of the number of Boulders
  for(j in (1:dim(Boulders)[1]))
  {
    input<-c(input,Boulders$Number_BestEstimate[j])
  }

#Input data can be adjusted manually. The following section of code calls for pop-up windows to appear, where this may be done. Inputted values are added to the input vector. Impossible values (e.g. <0) are discarded.


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


#' 
#' 
