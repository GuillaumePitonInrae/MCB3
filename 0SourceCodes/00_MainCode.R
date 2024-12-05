#' ---
#' title: "Main Code"
#' output: html_document
#' date: "2024-03-22"
#' Author: G. Piton, C. Misset, H. Shirra
#' ---
#' This is an annotated version of the 00_MainCode.R  script prepared by H. Shirra, originally developed by G. Piton and C. Misset to stochastically simulate jamming through a series of constrictions in a debris flow event. It is adapted from Piton et. al's (2022) paper " *Debris Flows, Boulders and Constrictions: A Simple Framework for Modeling Jamming, and Its Consequences on Outflow*" for which an R Script was developed to stochastically simulate jamming through a single constriction. This code was developed in R, and is accessible either through RStudio, or through an interface on the online platform PlatRisk (https://platrisk.ige.inrae.fr). This script calls upon various sub-routines, which simulate different aspects of jamming. 
#' 
#' ### Summary of Script
#' In this script, the workspace is first is set up by setting the working directory, and loading the subroutines and all input data. Next, the simulation is performed with user input, by calling on the various subroutines. The simulation can be summarized as follows: 
#' 
#' 1. The geometry of the structures is defined.
#' 2. The hydraulic parameters of the event (volume, peak flow, time to peak, deposition slope) are defined. 
#' 3. The passage of the event through the series of structures is computed, for each time step for the duration of the hydrograph:
#'     a. The passage of debris flow is computed using hydraulic equations (weir and orifice equations), the remaining debris material is assumed stored in the basin (mass conservation).
#'     b. The number of boulders is stochastically simulated.
#'     c. Their passage/ jamming is computed by comparing their size with the opening width and height.
#' 4. At the end of the simulation, the results are saved and plotted.
#'   
#' Further details on the simulation process are contained in the annotated versions of the sub-routine scripts. 
#' 
#' # Script
#' ### Set-up
## ----comment= "#", echo=FALSE-------------------------------------------------
#Transfer the .Rmd file to R automatically when it is knit. 
# knitr::purl(input = "00_MainCode.Rmd", documentation = 2) #comment this line prior to running generated R script.

#' 
#' Definition of an automatic online run (HEADLESS = TRUE) or local, user-driven run (HEADLESS = FALSE), loading of the packages
## ----echo=FALSE---------------------------------------------------------------
### Main code calling the sub-routines depending on the user choice

#Clean environment
rm(list=ls())
suppressPackageStartupMessages(library(jsonlite))

# Setup HEADLESS variables
HEADLESS <- !base::interactive()
if(HEADLESS) {
  args<-commandArgs(trailingOnly = TRUE)
}

#For Guillaume P. only, to emulate the headless mode under RStudio:
HEADLESS = TRUE
rootDir<-"D:/MCB3/4Simu/DFbuffering"
args = c(paste0(rootDir,"/params.json"), paste0(rootDir,"/out"))
setwd(paste0(rootDir,"/0SourceCodes"))

# For Hilary S. only, to emulate the headless mode under RStudio:
# HEADLESS = FALSE
# rootDir<-"C:/Users/shirrah/Documents/MSc_Thesis/GitHub/DFbuffering"
# args = c(paste0(rootDir,"/params.json"), paste0(rootDir,"/out"))
# setwd(paste0(rootDir,"/0SourceCodes"))

if(HEADLESS) {
  print('Running in HEADLESS mode')
  json <- jsonlite::fromJSON(args[1])
  # Transfer all first-level json props to global scope:
  if("sim" %in% names(json)){
    # the whole data may be wrapped in a "sim" object in json...
    list2env(json$sim,globalenv())
  } else {
    # ... or not.
    list2env(json,globalenv())
  }
  
  # But in the future, the env method above may not be acceptable, in which case prefer
  # using the following syntax: 
  # OnlyNormalRun <<- json$OnlyNormalRun
  # ... and so on...
}


# Load package
suppressPackageStartupMessages({
  library(lubridate) #To add date on plot
  library(stringr) #To replace string
  library(HYRISK)#Package uncertainty propagation
  library(svDialogs) #Package for popup dialog windows
  library(ggplot2) #Plots
  library(gridBase) #Plots
  library(gridExtra) #Plots
  library(grid) #Plots
  library(sets) #Plots
  library(dplyr) #for data manipulation
})
#Model version
ModelVersion <- "MCB³ (V1.1)"

#' 
#' If HEADLESS = TRUE, the user is here asked to show the path where source codes are saved. Then, the sub-routines are loaded
## ----echo=FALSE---------------------------------------------------------------
if(!HEADLESS)
{
  #Selecting the source codes repository ----
  dlg_message(message="Show me where are stored the source codes (Repository \"/0SourceCodes\")"
              , type = c("ok")) ; SourceCodeRepository<-dlg_dir(title="Show me where are stored the source codes (Repository \"/0SourceCodes\")"
                                                                ,default = getwd())$res
              
              #Set this repository as working repository
              setwd(SourceCodeRepository)
}

#Load the source codes
source("PLOT_INPUTnew.R")#To plot input distrib
source("CheckTriangleDistribution.R") #To check input data distributions
source("DischargeCapacityFunctions.R")#Load slit capacity function
source("BoulderPassing.R")#Compute the number of boulders passing through an orifice
source("Plot_BufferingModelResults.R") #For plotting results of singular runs
source("Create_inlet_input.R")#define the input data of runs
source("Create_inlet_timeseries.R")#define the times series of the inlet of the most upstream structure
source("Structure_definition.R")#define structure
source("Structure_functionning.R")#Actual buffering model
source("Cascade_of_structure_functionning.R")#Application of the buffering model to series of structures
source("BoulderTransfer.R")#Compute the transformation of the time series from one structure to another

#' 
#' If HEADLESS = TRUE, the user is here asked to show the path where data (i.e. text files defining the structures and events) are saved. Then, the event features (e.g. volume, peak discharge, boulder size and numbers), as well as the geometry of the structures (opening and basin shape) and their initial conditions, are loaded and computed. A repository named '\out' is created and the results will be stored in it.
## ----echo=FALSE---------------------------------------------------------------
## Upload the input data----
if(!HEADLESS){
  #Selecting the repository where the source codes are stored
  dlg_message(message="Show me where are stored the input data (Repository \"/1Data\")"
              , type = c("ok")) ; InputDataRepository<-dlg_dir(title="Show me where are stored the input data (Repository \"/1Data\")"
                                                               ,default = getwd())$res
              
              #Load boulder list
              Boulders<-read.csv(paste0(InputDataRepository,"/RangeOfBoulders.txt"),sep="\t")
                
              #Load event features
              Events<-read.csv(paste0(InputDataRepository,"/Events.txt"),sep="\t")
              
              #Load the structure list and organisation----
              Structure_organisation<-read.csv(paste0(InputDataRepository,"/StructureList.txt"),sep="\t")
              
              #Load initial conditions
              InitialConditions<-read.csv(paste0(InputDataRepository,"/InitialConditions.txt"),sep="\t")
              
              #Reorganize the table
              Structure_organisation<-data.frame(
                Name = Structure_organisation$Value[which(Structure_organisation$Variable=="Structure")],
                InitialCondition = Structure_organisation$Value[which(Structure_organisation$Variable=="Structure")+1],
                Transfer = Structure_organisation$Value[which(Structure_organisation$Variable=="Structure")+2])
              
              #Import the structure description
              Structures<-structure_definition(InputDataRepository)
              #define rank, transfer condition and initial conditions
              # Structures$Rank[Structures$Name %in% Structure_organisation$Name]<-match(Structure_organisation$Name,Structures$Name)
              Structures$Rank<-match(Structures$Name,Structure_organisation$Name)
              Structures$TransferDownstream<-Structure_organisation$Transfer[match(Structure_organisation$Name,Structures$Name)]
              Structures$InitialConditions<-InitialConditions[match(Structure_organisation$InitialCondition
                                                                    ,InitialConditions$Name),]
}#else{
#SourceCodeRepository<-paste0(MainRep,"/0SourceCodes/")
#}
#Set this repository as working repository
#setwd(SourceCodeRepository)

#Complete the storage elevation data of each bridge elements
for(Structure_Ind in (1:length(Structures$Name)))
{
  if(Structures$Type[[Structure_Ind]]=="bridge")
  {
    Structures$StorageElevation[[Structure_Ind]]<-define_bridgeStorageElevation(Structures$Openings[[Structure_Ind]]
                                                                                ,Structures$width[[Structure_Ind]]
                                                                                ,Structures$slope[[Structure_Ind]])
  }
  #Reorder the Storage Elevation such that they are in ascending order
  Structures$StorageElevation[[Structure_Ind]]<-Structures$StorageElevation[[Structure_Ind]][order(Structures$StorageElevation[[Structure_Ind]]$Z),]
  
  #reorder Opening by increasing base level to have the spillway as last opening
  Structures$Openings[[Structure_Ind]]<-Structures$Openings[[Structure_Ind]][order(Structures$Openings[[Structure_Ind]]$BaseLevel),]
  
  #Inform the user about the accuracy of the level computation
  #Define model level accuracy for convergence, taken as 0.01*elevation difference between crest and base level
  #or 1% of opening width if no crest level available (single weir or slit)
  Opening<-as.data.frame(Structures$Openings[[which(Structures$Rank==Structure_Ind)]])
  N_opening<-length(Opening$Number)
  if(N_opening>1){
    CrestLevel<-Opening$BaseLevel[N_opening]
    ModelLevelAccuracy <- 0.01*(CrestLevel-min(Opening$BaseLevel))
  }else{
    if(Opening$Type == "slot"){
      ModelLevelAccuracy <- 0.01*(Opening$TopLevel[1]-Opening$BaseLevel)
      CrestLevel <- Opening$TopLevel[1]
    }else{
      CrestLevel<-Opening$BaseLevel
      if(Opening$Width > 0)
      {
        ModelLevelAccuracy <- 0.01*Opening$Width  
      }else{
        ModelLevelAccuracy <- 0.01 #1 cm accuracy, arbitrary
      }
    }
  }
  print(paste0("The computation at structure: ",Structures$Name[[which(Structures$Rank==Structure_Ind)]]," is done with a level accuracy of ",ModelLevelAccuracy," m, i.e. about one percent of the structure height."))
  
}

#Order the Boulder table from bigger sizes
Boulders<-Boulders[match(sort(Boulders$Diameter_min,decreasing =TRUE), Boulders$Diameter_min),]

#Define the repository where one want to record the results
if(HEADLESS) {
  MainRep <- args[2]
  #Set this repository as working repository
  setwd(MainRep)
} else {
  # setwd(InputDataRepository)
  # dlg_message(message="Show me where you want to store the results (e.g., the parent directory where /0SourceCode and /1Data are stored)"
  #             , type = c("ok"));MainRep<-dlg_dir(title="Show me where you want to store the results (e.g., the parent directory where /0SourceCode and /1Data are stored)"
  #                                                       ,default = getwd())$res
  #Look for relevant name of repository (where are stored the input data)
  RepoistoryListName<-unlist(strsplit(x=getwd(),split=c("/")))
  RepositoryName<-paste0(RepoistoryListName[1],"/")
  for(i in (2:(length( RepoistoryListName)-1)))
  {
    RepositoryName<-paste0(RepositoryName,RepoistoryListName[i],"/")
  }
  MainRep<-RepositoryName
  rm(RepositoryName,RepoistoryListName)
  dir.create(paste0(MainRep,"/out"),showWarnings = FALSE)
  #Set this repository as working repository
  setwd(paste0(MainRep,"/out"))
}


#' 
#' ## Computation
#' ### Initial steps
#' The next code block is the actual computation. It is organized in several sub-blocks:
#' 1. The user defines if normal runs will be performed (i.e. accounting for fixed values of the input parameters but exploring the stochasticity of the boulder blockages) or if a error propagation analysis will be performed (i.e. accounting for the ranges of parameter values, in addition to the stochasticity of the boulder generation).
#' 2. The user defines the number of simulations.
#' 3. The user defines the event that will be simulated, either pre-defined or manually adjusted.
#' 4. Then the computation is actually launched with two different frameworks for normal runs versus error propagation.
#' 5. Synthetic plots aggregating the results of multiple runs are eventually automatically generated.
#' 
#' ### Normal runs
#' A simple double loop of computation is done: for each run that is launched, the debris flow is routed in each structure from upstream to downstream.
#' 
#' ### Error propagation
#' The ranges of uncertainty in each parameter is loaded, checked and prepared to fuel a possibilistic analysis (i.e. for each parameter, the user is asked to provide a lower bound value, a best estimate which is supposed by the user to be the most probable value, and a upper bound value). Then, the same double loop routing debris flow through the structures is applied.
#' 
#' ### Synthetic plots
#' If only a few runs are launched, one plot for each run is prepared (time series of discharge, clogging, flow level and volume). If many runs are launched, a few plots gather the main results in a synthetic way.
#' 
## 
#Main loop within which each set of run is performed----
PerformAnotherSimulation <- "yes"
while(PerformAnotherSimulation == "yes")
{
  
  if(!HEADLESS){
    #Select the type of approach----
    Perform_error_propagation <-dlg_message(message="Press \"Yes\" to perform a full uncertainty propagation analysis \n Or press \"No\" to run normal runs (using best estimates of the input data)"
                                            , type = c("yesno"))$res
    
    # Perform_error_propagation <-"yes"
    if(Perform_error_propagation =="yes"){ Perform_error_propagation <-TRUE}else{ Perform_error_propagation <-FALSE}
    #Define the number of simulations to run----
    N.unvalidated<-TRUE
    while(N.unvalidated)
    {
      if(Perform_error_propagation == FALSE)
      {N_runs<-as.numeric(dlg_input(message = "How many simulations to you want to run (n<10 000)"
                                    , default = "5")$res)
      }else
      {N_runs<-as.numeric(dlg_input(message = "How many simulations to you want to run (10<n<10 000)"
                                    , default = "25")$res)
      }
      if(N_runs<1001){
        N.unvalidated<-FALSE
      }else{
        if(N_runs<10001){
          Validation<-dlg_message(message="It will probably take hours! Sure?", type = c("yesno"))$res
          if(Validation=="yes"){N.unvalidated<-FALSE}else{N.unvalidated<-TRUE}
        }else{
          Validation<-dlg_message(message="It will likely take days! Sure?", type = c("yesno"))$res
          if(Validation=="yes"){N.unvalidated<-FALSE}else{N.unvalidated<-TRUE}
        }
        rm(Validation)
      }
    }
  }
  
  
  # Choices of plot and save option----
  if(Perform_error_propagation)
  {
    ComputeWithBestEstimateNumber<-FALSE
  }else{
    ComputeWithBestEstimateNumber<-TRUE
  }
  
  if(Perform_error_propagation == FALSE)
  {
    if(N_runs <= 10){ PrintFinalPlot <- TRUE }else{ PrintFinalPlot <- FALSE} #no plot if more than 10 runs
    # if(!HEADLESS){
    #Saving synthesis figure for each run?
    # PrintFinalPlot<-dlg_message(message="Do you want to print a synthesis plot for each run (hydrographs, flow level, volume stored) in a .png file?", type = c("yesno"))$res
    # if(PrintFinalPlot=="yes"){ PrintFinalPlot<-TRUE}else{ PrintFinalPlot<-FALSE}
    # }
  }else{PrintFinalPlot<-FALSE} #no plot for each run if error propagation by possibility analysis
  
  #Disabled options in this version
  PrintDataPlot<-FALSE #Plot stage - discharge Q(h) curve and stage - volume V(h) curves
  TestAbsenceBottomJam<-FALSE #remove the initial blockage level y_0 of the initial conditions
  
  #Want to save the final state of clogging to use it for the next run?
  KeepTrackOfCloggingState<-FALSE
  
  # Time step
  TimeStep<-1 #(s) Should be an integer in seconds, so no less than 1 second
  # small enough to capture the peak, the code seems to get stuck in infinite loops if set at 5 s with low slopes
  # Define the event to model----
  if(!HEADLESS) 
  {
    EventUndefined<-TRUE
    while(EventUndefined)
    {
      if(Perform_error_propagation == FALSE){ # Possible to reuse predefined values or to define the event manually
        EventName<-dlg_input(message = c("Write the name of the event you want to model, the available names are :"
                                         ,Events$Name
                                         ,"If you want to define the event manually, write \"0\" ")
                             ,default = Events$Name[1])$res
      }else{#Only possible to reuse predefined values 
        EventName<-dlg_input(message = c("Write the name of the event you want to model, the available names are :"
                                         ,Events$Name)
                             ,default = Events$Name[1])$res
        AdjustEventManually <- FALSE
      } 
      
      if(EventName != "0"){AdjustEventManually<-FALSE}else{#Define values manually
        AdjustEventManually <- TRUE
        EventName<-dlg_input(message = c("OK we will adjust an event, which one do you want to take as template?, the available names are :"
                                         ,Events$Name)
                             ,default = Events$Name[1])$res
      }
      
      if(EventName %in% Events$Name){EventUndefined<-FALSE}else{
        dlg_message(message="The name you wrote is not in the list of the available events, please provide an available event name"
                    , type = c("ok"))
        EventUndefined<-TRUE
      }
    }#end of while loop to define the event
  }else{
    EventName <- Events$Name[1]
    AdjustEventManually<-FALSE}
  
  
  # Create input data to launch runs----
  if(Perform_error_propagation == FALSE)
  {
    #If only normal runs, we only use the best estimates
    BoulderGenerationMode<-"Best estimate values"
    
    
    #Create input data according to the EventName and adjustement option
    input<-Create_inlet_input(EventName,AdjustEventManually,Structures,Boulders)
    Run_Ind<-0
    save(Run_Ind,N_runs,file = "RunInd.Rdata.tmp")
    ## Computation of isolated runs ----
    for(Run_Ind in (1:N_runs))
    {
      Qoutmax<-Cascade_of_structure_functionning(input)
      
      #Record the run results
      if(Run_Ind==1){  Qoutmax_all<-Qoutmax }else{ Qoutmax_all<-rbind(Qoutmax_all,Qoutmax)  }
      
      #print message
      # print(paste0("Run #",Run_Ind," finished at ",now()," for the whole cascade of structure, still ",N_runs-Run_Ind," run to perform"))
      # print(paste0("PROGRESS[",Run_Ind,"/",N_runs,"]"))
      
    }
    
  }else{# end of the normal run condition
    
    BoulderGenerationMode<-"Error propagation"
    # Find the event in the table
    Event_Ind<-which(Events$Name==EventName)
    
    # IMPERFECT DATA TO PROVIDE----
    
    ninput<-4+length(Structures$Name)*2+length(Boulders[,1]) #Number of input parameters
    input<-vector(mode="list", length=ninput) # Initialization#
    
    ############## Volume of debris flows, in Mm3
    #If min = best estimate = max, set the parameter as "fixed"
    if((Events$Volume_min[Event_Ind]==Events$Volume_BestEstimate[Event_Ind]
        &&
        Events$Volume_BestEstimate[Event_Ind]==Events$Volume_max[Event_Ind]))
    {
      input[[1]]=CREATE_INPUT(
        name="Volume \n [*1000m3]",
        type="fixed",
        param= Events$Volume_BestEstimate[Event_Ind]/10^3,  
        monoton = "incr"
      )
    }else{
      #Otherwise, set it as possibility distribution (triangle)
      input[[1]]=CREATE_INPUT(
        name="Volume \n [*1000m3]",
        type="possi",
        distr="triangle",
        param=CheckTriangleDistribution(Events$Volume_min[Event_Ind],
                                        Events$Volume_BestEstimate[Event_Ind],
                                        Events$Volume_max[Event_Ind],
                                        "Volume")/10^3,
        monoton = "incr"
      )
      
    }
    
    #Peak discharge in m3/s
    #If min = best estimate = max, set the paramter as "fixed"
    if((Events$PeakDischarge_min[Event_Ind]==Events$PeakDischarge_BestEstimate[Event_Ind]
        &&
        Events$PeakDischarge_BestEstimate[Event_Ind]==Events$PeakDischarge_max[Event_Ind]))
    {
      input[[2]]=CREATE_INPUT(
        name="Peak discharge \n [m3/s]",
        type="fixed",
        param= Events$PeakDischarge_BestEstimate[Event_Ind],  
        monoton = "incr"
      )
    }else{
      #Otherwise, set it as possibility distribution (triangle)
      input[[2]]=CREATE_INPUT(
        name="Peak discharge \n [m3/s]",
        type="possi",
        distr="triangle",
        param=CheckTriangleDistribution(Events$PeakDischarge_min[Event_Ind],
                                        Events$PeakDischarge_BestEstimate[Event_Ind],
                                        Events$PeakDischarge_max[Event_Ind],
                                        "Qpeak"),
        monoton = "incr"
      )  
    }
    
    #Position of the peak in the triangle: Tpeak/Ttotal
    #If min = best estimate = max, set the paramter as "fixed"
    if((Events$TimeLag_min[Event_Ind]==Events$TimeLag_BestEstimate[Event_Ind]
        &&
        Events$TimeLag_BestEstimate[Event_Ind]==Events$TimeLag_max[Event_Ind]))
    {
      input[[3]]=CREATE_INPUT(
        name="Peak lag \n [-]",
        type="fixed",
        param= Events$TimeLag_BestEstimate[Event_Ind],  
        monoton = "incr"
      )
    }else{
      #Otherwise, set it as possibility distribution (triangle)
      input[[3]]=CREATE_INPUT(
        name="Peak lag \n [-]",
        type="possi",
        distr="triangle",
        param=CheckTriangleDistribution(Events$TimeLag_min[Event_Ind],
                                        Events$TimeLag_BestEstimate[Event_Ind],
                                        Events$TimeLag_max[Event_Ind],
                                        "Peak lag"),
        monoton = "incr"
      )
    }
    
    #Deposition slope
    #If min = best estimate = max, set the paramter as "fixed"
    if((Events$DepositionSlope_min[Event_Ind]==Events$DepositionSlope_BestEstimate[Event_Ind]
        &&
        Events$DepositionSlope_BestEstimate[Event_Ind]==Events$DepositionSlope_max[Event_Ind]))
    {
      input[[4]]=CREATE_INPUT(
        name="Deposition slope \n [%]",
        type="fixed",
        param= Events$DepositionSlope_BestEstimate[Event_Ind],  
        monoton = "decr"
      )
    }else{
      #Otherwise, set it as possibility distribution (triangle)
      input[[4]]=CREATE_INPUT(
        name="Deposition slope \n [%]",
        type="possi",
        distr="triangle",
        param=CheckTriangleDistribution(Events$DepositionSlope_min[Event_Ind],
                                        Events$DepositionSlope_BestEstimate[Event_Ind],
                                        Events$DepositionSlope_max[Event_Ind],
                                        "Deposition slope"),
        monoton = "decr"
      )
    }
    
    ##Initial deposit level and initial clogging level, set to 0 if missing
    for(Structure_Ind in (1:length(Structures$Name)))
    {
      DepositHeight_BestEstimate<-Structures$InitialConditions$DepositHeight_BestEstimate[which(Structures$Rank==Structure_Ind)]
      if(is.na(DepositHeight_BestEstimate)){DepositHeight_BestEstimate<-0}
      
      DepositHeight_max<-Structures$InitialConditions$DepositHeight_max[which(Structures$Rank==Structure_Ind)]
      if(is.na(DepositHeight_max)){DepositHeight_max<-0}
      
      DepositHeight_min<-Structures$InitialConditions$DepositHeight_min[which(Structures$Rank==Structure_Ind)]
      if(is.na(DepositHeight_min)){DepositHeight_min<-0}
      
      
      #If min = best estimate = max, set the paramter as "fixed"
      if((DepositHeight_BestEstimate  == DepositHeight_max)  
         &&
         (DepositHeight_BestEstimate  == DepositHeight_min))
      {
        input[[5+(Structure_Ind-1)*2]]=CREATE_INPUT(
          name=paste0("Initial deposit [m] \n","structure:",Structures$Name[which(Structures$Rank==Structure_Ind)]),
          type="fixed",
          param= DepositHeight_BestEstimate,
          monoton = "decr"
        )
      }else{
        #Otherwise, set it as possibility distribution (triangle)
        input[[5+(Structure_Ind-1)*2]]=CREATE_INPUT(
          name=paste0("Initial deposit [m] \n","(",Structures$Name[which(Structures$Rank==Structure_Ind)],")"),
          ##### Triangular distri
          type="possi",
          distr="triangle",
          param=CheckTriangleDistribution(DepositHeight_min,
                                          DepositHeight_BestEstimate,
                                          DepositHeight_max,
                                          paste0("Initial deposit [m] \n","(",Structures$Name[which(Structures$Rank==Structure_Ind)],")")),
          monoton = "decr"
        )
      }
      
      # Height of the initial jamming in the barrier (Large wood and / or boulders), set to 0 if missing
      #If min = best estimate = max, set the paramter as "fixed"
      JammingHeight_BestEstimate <- Structures$InitialConditions$JammingHeight_BestEstimate[which(Structures$Rank==Structure_Ind)]
      if(is.na(JammingHeight_BestEstimate)){JammingHeight_BestEstimate<-0}
      
      JammingHeight_max <- Structures$InitialConditions$JammingHeight_max[which(Structures$Rank==Structure_Ind)]
      if(is.na(JammingHeight_max)){JammingHeight_max<-0}
      
      JammingHeight_min <- Structures$InitialConditions$JammingHeight_min[which(Structures$Rank==Structure_Ind)]
      if(is.na(JammingHeight_min)){JammingHeight_min<-0}
      
      # Structures$InitialConditions$JammingHeight_BestEstimate[which(Structures$Rank==Structure_Ind)])#Jam at the slit base by large wood 
      
      #If min = best estimate = max, set the paramter as "fixed"
      if((JammingHeight_BestEstimate  == JammingHeight_max)  
         &&
         (JammingHeight_BestEstimate  == JammingHeight_min))
      {
        input[[5+(Structure_Ind-1)*2+1]]=CREATE_INPUT(
          name=paste0("Initial jam height [m] \n","structure:",Structures$Name[which(Structures$Rank==Structure_Ind)]),
          type="fixed",
          param= JammingHeight_BestEstimate,
          monoton = "decr"
        )
      }else{
        #Otherwise, set it as possibility distribution (triangle)
        input[[5+(Structure_Ind-1)*2+1]]=CREATE_INPUT(
          name=paste0("Initial jam height [m] \n","structure:",Structures$Name[which(Structures$Rank==Structure_Ind)]),
          ##### Triangular distri
          type="possi",
          distr="triangle",
          param=CheckTriangleDistribution(JammingHeight_min,
                                          JammingHeight_BestEstimate,
                                          JammingHeight_max,
                                          paste0("Initial jam height [m] \n","structure:",Structures$Name[which(Structures$Rank==Structure_Ind)])),
          monoton = "decr"
        )
      }
    }
    
    ###Boulders
    for(i in (1:length(Boulders[,1])))
    {
      #Boulder class i
      #If min = best estimate = max, set the parameter as "fixed"
      if((Boulders$Number_min[i]==Boulders$Number_BestEstimate[i]
          &&
          Boulders$Number_BestEstimate[i]==Boulders$Number_max[i]))
      {
        input[[(4+length(Structures$Name)*2+i)]]=CREATE_INPUT(
          name=paste0("#Boulders \n ","D=",Boulders[i,1],"-",Boulders[i,2],"m"),
          type="fixed",
          param= Boulders$Number_BestEstimate[i],
          monoton = "decr"
        )
      }else{
        #Otherwise, set it as possibility distribution (triangle)
        input[[(4+length(Structures$Name)*2+i)]]=CREATE_INPUT(
          name=paste0("#Boulders ",Boulders[i,1],"-",Boulders[i,2],"m"),
          ##### Triangular distri
          type="possi",
          distr="triangle",
          param=CheckTriangleDistribution(Boulders$Number_min[i]
                                          ,Boulders$Number_BestEstimate[i]
                                          ,Boulders$Number_max[i]
                                          ,paste0("#Boulders \n ","D=",Boulders[i,1],"-",Boulders[i,2],"m")),
          monoton = "decr")
      }
    }
    #### Prepare input for the clogging analysis -----
    input_for_clogging<-input
    
    #Change the monotony
    ##Initial deposit level and initial clogging level, set to 0 if missing
    for(Structure_Ind in (1:length(Structures$Name)))
    {
      #Initial jam
      input_for_clogging[[5+(Structure_Ind-1)*2]]$monoton<-"incr"
      input_for_clogging[[5+(Structure_Ind-1)*2+1]]$monoton<-"incr"
    }
    
    ###Boulders
    for(i in (1:length(Boulders[,1])))
    {
      #Boulder class i
      input_for_clogging[[(4+length(Structures$Name)*2+i)]]$monoton <- "incr"
    }
    
    #     COMPUTATION----
    
    ####CREATION OF THE DISTRIBUTIONS ASSOCIATED TO THE PARAMETERS
    input=CREATE_DISTR(input)
    input_for_clogging=CREATE_DISTR(input_for_clogging)
    
    ####VISU INPUT
    png(paste0("DistributionsInputParametersPossibilityAnalysis_Evt-",EventName,".png")
        , width = 22, height = 24
        ,units="cm"
        ,res=350
    )
    {PLOT_INPUTnew(input)}
    dev.off()
    
    # ### OPTIMZATION CHOICES
    choice_opt=NULL #no optimization needed because monotony known
    param_opt=NULL
    
    
    ###HYBRID UNCERTAINTY PROPAGATION ON VOLUME AND RELEASE DISCHARGE----
    #Initialize run number
    Run_Ind<-0
    save(Run_Ind,N_runs,file = "RunInd.Rdata.tmp")
    print("Launching first part of the uncertainty propagation focusing on released volume and discharge.")
    Rslt_Propag_VandQpeak<-PROPAG(N=N_runs,input
                                  ,Cascade_of_structure_functionning
                                  ,choice_opt,param_opt,mode="IRS")
    
    # Arrange results in a table
     Rslt_Propag_VandQpeak<-data.frame(P=seq(0,1,length.out = N_runs)
                                              ,Min=sort( Rslt_Propag_VandQpeak[1,],na.last = TRUE)
                                              ,Max=sort( Rslt_Propag_VandQpeak[2,],na.last = FALSE))
    #       
    ###################Plot Pbox----
    #       
    ggplot()+theme_bw(base_size = 9)+
      geom_vline(aes(xintercept = 1))+
      geom_vline(aes(xintercept = 0))+
      geom_hline(aes(yintercept=Events$PeakDischarge_min[Event_Ind]),lty=2)+
      geom_hline(aes(yintercept=Events$PeakDischarge_BestEstimate[Event_Ind]))+
      geom_hline(aes(yintercept=Events$PeakDischarge_max[Event_Ind]),lty=2)+
      annotate(geom = "text", x = 0.5, y = Events$PeakDischarge_BestEstimate[Event_Ind]
               ,vjust=(-0.5), label = "Supply (Best. Est.)",srt=90)+
      geom_ribbon(data= Rslt_Propag_VandQpeak,aes(x =P,ymin=Min,ymax=Max),alpha=0.3,lwd=1)+
      geom_line(data= Rslt_Propag_VandQpeak,aes(y =Min ,x=P,colour="1"),lwd=1)+
      geom_line(data= Rslt_Propag_VandQpeak,aes(y =Max ,x=P,colour="2"),lwd=1)+
      scale_colour_manual(name="Bounding Cumulated Distribution Functions (CDF)"
                          ,values=c("lightblue","darkblue")
                          ,labels=c("Lower bound","Upper bound"))+
      coord_flip()+ #To have Probability as Y
      theme(legend.position = "top")+
      labs( y = "Peak discharge [m3/s]",x = "Cumulative distribution function"
            ,paste("Code version:",ModelVersion,"|",
                   "Parameters:"," uncertain values","|"
                   ,"Number of runs =",N_runs)
            ,title = paste0("Uncertainty analysis of peak discharge downstream all structures\n","(event: ",EventName,")"))
    #Save figure
    ggsave(paste0("ReleasedPeakDischarge_Evt-",EventName,"_FromErrorPropagation.png")
           , width = 16.5, height = 7,units="cm")
    
    #Save results
    # save( Rslt_Propag_VandQpeak,file=paste0("PboxDownstreamReleasedPeakDischarge_Evt-",EventName,".RData"))
  
    
    }#end of the uncertainty propagation condition on volume and peak discharge
  
  
  #Rename file names
  ListFile1<-list.files(pattern="computedOn")#Figure
  ListFile2<-list.files(pattern="ComputedOn")#Rdata
  if(Perform_error_propagation == FALSE)
  {
    if(length(ListFile1)>0)
    {
      file.rename(ListFile1,paste0("Figure",substr(ListFile1,1,nchar(ListFile1)-34),"_run",rep((1:Run_Ind),max(Structures$Rank)),".png"))  
    }
    if(length(ListFile2)>0)
    {
      file.rename(ListFile2,paste0("Rdata",substr(ListFile2,1,nchar(ListFile2)-36),"_run",rep((1:Run_Ind),max(Structures$Rank)),".Rdata"))
    }
  }
  # rm(ListFile1,ListFile2)
  
  
  #Aggregate every run results in a single data frame per structure
  # Find the event in the table
  Event_Ind<-which(Events$Name==EventName)
  
  for(Structure_Ind in 1:length(Structures$Name))
  {
    StructureName<-Structures$Name[[which(Structures$Rank==Structure_Ind)]]
    ListFileStructure<-list.files(pattern = paste0("Structure_@-",StructureName))#Rdata
    for(File_Ind in (1:length(ListFileStructure)))
    {
      #load results of the structure
      load(ListFileStructure[File_Ind])
      #Hyrisk launch first lower bound and then upper bound simulations
      Run_Ind<-unique(Qo$Run)
      Branch<-  if(Perform_error_propagation == TRUE){if(round(File_Ind/2,0)==File_Ind/2){"Upper"}else{"Lower"}}else{"Intermediate"}
      #record the indice and branch
      # Qo$Run<-File_Ind
      Qo$Branch<-Branch
      input$Run<-Run_Ind
      input$Branch<-Branch
      Result$Run<-Run_Ind
      Result$Branch<-Branch
      if(File_Ind == 1)
      {
        Qo_all <-Qo 
        input_all<-data.frame(input)
        Result_all<-Result
        
      }else{
        Qo_all <-rbind(Qo_all,Qo) 
        input_all<-rbind(input_all,data.frame(input))
        Result_all<-rbind(Result_all,Result)
      }
    }
    
    ##### Computation of the residual opening, i.e. 1-clogging ratio.----
    # Clogging of slots is systematically computed,
    # Clogging of weir is systematically  NOT   computed,
    # Slits are included only in barriers, not in bridges.
    
    #initialisation
    ResidualSection <- rep(0,length(Result_all$Run))
    for(Opening_ind in (1:N_opening))
    {
      #For bridges we count only slots as opening, i.e. below the deck, slits and weirs are for the deck and other upper side flow sections
      if(Structures$Openings[[Structure_Ind]]$Type[Opening_ind] == "slot")
      {
        #Compute residual width in the opening
        ResidualSection_openingWidth <- rep(Structures$Openings[[Structure_Ind]]$Width[Opening_ind],length(ResidualSection)) - Result_all[,which(names(Result_all) == paste0("W",Opening_ind))]
        #Compute residual height in the opening
        ResidualSection_openingHeight <- rep(Structures$Openings[[Structure_Ind]]$TopLevel[Opening_ind],length(ResidualSection)) - Result_all[,which(names(Result_all) == paste0("Z",Opening_ind))] - rep(Structures$Openings[[Structure_Ind]]$BaseLevel[Opening_ind],length(ResidualSection))
        #remove eventual negative value
        ResidualSection_openingWidth[ResidualSection_openingWidth<0]<-0
        ResidualSection_openingHeight[ResidualSection_openingHeight<0]<-0
        #Add residual section of opening i
        ResidualSection <- ResidualSection + ResidualSection_openingWidth * ResidualSection_openingHeight
      }else{
        #For barrier we also count the slits, but only those having a top level > base level
        if(Structures$Openings[[Structure_Ind]]$Type[Opening_ind] == "slit" && Structures$Type[[Structure_Ind]]=="barrier")
        {
          #Compute residual width in the opening
          ResidualSection_openingWidth <- Structures$Openings[[Structure_Ind]]$Width[Opening_ind]
          #Compute residual height in the opening
          ResidualSection_openingHeight <- rep(Structures$Openings[[Structure_Ind]]$TopLevel[Opening_ind],length(ResidualSection)) - Result_all[,which(names(Result_all) == paste0("Z",Opening_ind))] - rep(Structures$Openings[[Structure_Ind]]$BaseLevel[Opening_ind],length(ResidualSection))
          #remove eventual negative value
          ResidualSection_openingHeight[ResidualSection_openingHeight<0]<-0
          #Add residual section of opening i
          ResidualSection <- ResidualSection + ResidualSection_openingWidth * ResidualSection_openingHeight
        }
      }
    }
    
    SlotsAndSlits <- Structures$Openings[[Structure_Ind]] %>% filter(Type == "slot" | Type == "slit")
    OpeningTotalSection<-sum(SlotsAndSlits$Width*(SlotsAndSlits$TopLevel-SlotsAndSlits$BaseLevel))
    # Record the residual fration of the opening
    Result_all$ResidualOpening <- ResidualSection / OpeningTotalSection
    
    
    #Save the file that aggregate all the results
    save(Qo_all,input_all,Result_all,file=paste0("RdataResult_Evt-",EventName,"_Structure_",StructureName,".RData"))
    
    # Remove files for each singular run
    file.remove(ListFileStructure)
 }# end of the structure loop
  
  #### SECOND UNCERTAINTY PROPAGATION ON FLOW LEVEL AND CLOGGING RATE----
  ###HYBRID UNCERTAINTY PROPAGATION 
  if(Perform_error_propagation == TRUE)
  {
    print("Launching second part of the uncertainty propagation focusing on flow level and clogging rate.")
    Rslt_Propag_ZandClogging<-PROPAG(N=N_runs,input_for_clogging
                                     ,Cascade_of_structure_functionning
                                     ,choice_opt,param_opt,mode="IRS")
    
    #Rename file names
    ListFile1<-list.files(pattern="computedOn")#Figure
    ListFile2<-list.files(pattern="ComputedOn")#Rdata
    if(Perform_error_propagation == FALSE)
    {
      if(length(ListFile1)>0)
      {
        file.rename(ListFile1,paste0("Figure",substr(ListFile1,1,nchar(ListFile1)-34),"_run",rep((1:Run_Ind),max(Structures$Rank)),".png"))  
      }
      if(length(ListFile2)>0)
      {
        file.rename(ListFile2,paste0("Rdata",substr(ListFile2,1,nchar(ListFile2)-36),"_run",rep((1:Run_Ind),max(Structures$Rank)),".Rdata"))
      }
    }
    # rm(ListFile1,ListFile2)
    
    for(Structure_Ind in 1:length(Structures$Name))
    {
      StructureName<-Structures$Name[[which(Structures$Rank==Structure_Ind)]]
      ListFileStructure<-list.files(pattern = paste0("Structure_@-",StructureName))#Rdata
      for(File_Ind in (1:length(ListFileStructure)))
      {
        #load results of the structure
        load(ListFileStructure[File_Ind])
        #Hyrisk launch first lower bound and then upper bound simulations
        Branch<-  if(Perform_error_propagation == TRUE)
        {if(round(File_Ind/2,0)==File_Ind/2){"Upper"}else{"Lower"}
          }else{"Intermediate"}
        #record the indice and branch
        Qo$Run<-File_Ind
        Qo$Branch<-Branch
        input$Run<-File_Ind
        input$Branch<-Branch
        Result$Run<-File_Ind
        Result$Branch<-Branch
        if(File_Ind == 1)
        {
          Qo_all <-Qo 
          input_all<-data.frame(input)
          Result_all<-Result
          
        }else{
          Qo_all <-rbind(Qo_all,Qo) 
          input_all<-rbind(input_all,data.frame(input))
          Result_all<-rbind(Result_all,Result)
        }
      }
      
      ##### Computation of the residual opening, i.e. 1-clogging ratio.----
      # Clogging of slots is systematically computed,
      # Clogging of weir is systematically  NOT   computed,
      # Slits are included only in barriers, not in bridges.
      
      #initialisation
      ResidualSection <- rep(0,length(Result_all$Run))
      for(Opening_ind in (1:N_opening))
      {
        #For bridges we count only slots as opening, i.e. below the deck, slits and weirs are for the deck and other upper side flow sections
        if(Structures$Openings[[Structure_Ind]]$Type[Opening_ind] == "slot")
        {
          #Compute residual width in the opening
          ResidualSection_openingWidth <- rep(Structures$Openings[[Structure_Ind]]$Width[Opening_ind],length(ResidualSection)) - Result_all[,which(names(Result_all) == paste0("W",Opening_ind))]
          #Compute residual height in the opening
          ResidualSection_openingHeight <- rep(Structures$Openings[[Structure_Ind]]$TopLevel[Opening_ind],length(ResidualSection)) - Result_all[,which(names(Result_all) == paste0("Z",Opening_ind))] - rep(Structures$Openings[[Structure_Ind]]$BaseLevel[Opening_ind],length(ResidualSection))
          #remove eventual negative value
          ResidualSection_openingWidth[ResidualSection_openingWidth<0]<-0
          ResidualSection_openingHeight[ResidualSection_openingHeight<0]<-0
          #Add residual section of opening i
          ResidualSection <- ResidualSection + ResidualSection_openingWidth * ResidualSection_openingHeight
        }else{
          #For barrier we also count the slits, but only those having a top level > base level
          if(Structures$Openings[[Structure_Ind]]$Type[Opening_ind] == "slit" && Structures$Type[[Structure_Ind]]=="barrier")
          {
            #Compute residual width in the opening
            ResidualSection_openingWidth <- Structures$Openings[[Structure_Ind]]$Width[Opening_ind]
            #Compute residual height in the opening
            ResidualSection_openingHeight <- rep(Structures$Openings[[Structure_Ind]]$TopLevel[Opening_ind],length(ResidualSection)) - Result_all[,which(names(Result_all) == paste0("Z",Opening_ind))] - rep(Structures$Openings[[Structure_Ind]]$BaseLevel[Opening_ind],length(ResidualSection))
            #remove eventual negative value
            ResidualSection_openingHeight[ResidualSection_openingHeight<0]<-0
            #Add residual section of opening i
            ResidualSection <- ResidualSection + ResidualSection_openingWidth * ResidualSection_openingHeight
          }
        }
      }
      
      SlotsAndSlits <- Structures$Openings[[Structure_Ind]] %>% filter(Type == "slot" | Type == "slit")
      OpeningTotalSection<-sum(SlotsAndSlits$Width*(SlotsAndSlits$TopLevel-SlotsAndSlits$BaseLevel))
      # Record the residual fration of the opening
      Result_all$ResidualOpening <- ResidualSection / OpeningTotalSection
      
      ### record them with different name
      Qo_all_for_clogging<-Qo_all
      Result_all_for_clogging<-Result_all
      
      #load the file that aggregates all the results
      load(paste0("RdataResult_Evt-",EventName,"_Structure_",StructureName,".RData"))
      #Save the file that aggregate all the results
      save(Qo_all,Qo_all_for_clogging,
           input_all,
           Result_all,Result_all_for_clogging
           ,file=paste0("RdataResult_Evt-",EventName,"_Structure_",StructureName,".RData"))
      
      # Remove files for each singular run
      file.remove(ListFileStructure)
      file.remove(list.files(pattern="RunInd.Rdata.tmp"))
    }# end of the structure loop
    
    
  }
  
  #Plot structure loop----
  for(Structure_Ind in 1:length(Structures$Name))
  {
    StructureName<-Structures$Name[[which(Structures$Rank==Structure_Ind)]]
    #load the file that aggregates all the results
    load(paste0("RdataResult_Evt-",EventName,"_Structure_",StructureName,".RData"))
    if(Perform_error_propagation == FALSE)
      {
      Result_all_for_clogging<-Result_all
      FillLegendLabel<-"# of runs"
    }else{
      FillLegendLabel<-"# of bounding runs"
      Result_all$Branch<-paste0(Result_all$Branch," bound")
      Result_all_for_clogging$Branch<-paste0(Result_all_for_clogging$Branch," bound")
      
      
      Qo_all <- Qo_all %>% 
        mutate(Branch = case_when(Branch == "Lower" ~ "Small event, many boulders", Branch == "Upper" ~ "Big event, few boulders"))
      
      Qo_all_for_clogging <- Qo_all_for_clogging %>% 
        mutate(Run = - Run) %>% mutate(Branch = case_when(Branch == "Lower" ~ "Small event, few boulders", Branch == "Upper" ~ "Big event, many boulders"))
     
       Qo_all <- rbind(Qo_all,Qo_all_for_clogging) 
      
       Result_all <- Result_all %>% 
         mutate(Branch = case_when(Branch == "Lower bound" ~ "Small event, many boulders", Branch == "Upper bound" ~ "Big event, few boulders"))
      
      Result_all_for_clogging <- Result_all_for_clogging   %>%
        mutate(Branch = case_when(Branch == "Lower bound" ~ "Small event, few boulders", Branch == "Upper bound" ~ "Big event, many boulders"))
      
      
      }
    
    Result_all<-rbind(Result_all,Result_all_for_clogging)
    
    
    #plots of synthesis multi run figures
    if(PrintFinalPlot==FALSE)
    {
      AlphaN_runs<-min(0.3,0.3*25/max(Qo_all$Run)*4)
      ### Four panel graph for V and Qout ------
      {
        # Plot a synthesis figure on Vin
        TopTopLeftPanel<-ggplot(Result_all)+
          theme_classic(base_size = 9)+
          geom_histogram(aes(Vevent/10^3,fill=Branch)
                         ,binwidth=Events$Volume_BestEstimate[Event_Ind]/10^3/10)
        if(Perform_error_propagation == TRUE)
        {
          TopTopLeftPanel<-TopTopLeftPanel+
          scale_fill_viridis_d("Bound",option="D",direction = 1)+
            coord_cartesian(xlim=c(0,Events$Volume_max[Event_Ind]/10^3*1.25))+
            guides(fill = guide_legend(order = 1,nrow=2    ))
        }else{ TopTopLeftPanel<-TopTopLeftPanel+
          guides(fill="none")+
          coord_cartesian(xlim=c(0,Events$Volume_BestEstimate[Event_Ind]/10^3*1.25))
        }
        
        TopTopLeftPanel<-TopTopLeftPanel+
          geom_boxplot(aes(x=Vevent/10^3,y=-1))+
          geom_vline(xintercept = Events$Volume_BestEstimate[Event_Ind]/10^3,col="grey")+
          annotate(geom = "text", y = 0, adj=0, x = Events$Volume_BestEstimate[Event_Ind]/10^3
                   ,vjust=(1.2), label = "Total event \n (Best. Est.)",srt=90,col="grey",size=3.5)+
          labs(x="Supplied volume [*1000 m3]",y="# of Run"
               ,title = paste0("Debris flow volume and peak discharge \n","|Event: " ,EventName," \n"
                               ,"|Structure: ",StructureName,""))+
          theme(legend.position = "top"#c(0.2,0.5)
                ,legend.box.background = element_rect(colour = 1) ,legend.key.height = unit(0.3, 'cm'))
        
        # Plot a synthesis figure on Vout
        TopLeftPanel<-ggplot(Result_all)+
          theme_classic(base_size = 9)+
          geom_histogram(aes(Vout/10^3,fill=Branch)
                         ,binwidth=Events$Volume_BestEstimate[Event_Ind]/10^3/10)
        if(Perform_error_propagation == TRUE)
        {
          TopLeftPanel<-TopLeftPanel+
            scale_fill_viridis_d("Bound",option="D",direction = 1)+
            coord_cartesian(xlim=c(0,Events$Volume_max[Event_Ind]/10^3*1.25))
        }else{
          TopLeftPanel<-TopLeftPanel+
          coord_cartesian(xlim=c(0,Events$Volume_BestEstimate[Event_Ind]/10^3*1.25))
        }
        
        TopLeftPanel<-TopLeftPanel+
          geom_boxplot(aes(x=Vout/10^3,y=-1))+
          labs(x="Released volume [*1000 m3]",y="# of Run")+
          guides(fill="none")
        
        # Plot a synthesis figure of Qpeak out VS Vout, top right panel
        TopightPanel<-ggplot(Result_all)+
          theme_bw(base_size = 9)+
          geom_bin2d(aes(x=(Vevent-Vout)/Vevent,y=(Qp_in-Qp_out)/Qp_in),col=1,#bins=20)+
                     binwidth=c(0.1,0.1))+
          scale_fill_gradient2(FillLegendLabel,low="deeppink1", high = "deeppink4")+ 
          geom_hline(yintercept = 1,col="grey")+
          geom_vline(xintercept = 1,col="grey")+
          annotate(geom = "text", y = 0.5, adj=0.5, x = 0,vjust=(-1), label = "No effect on volume",srt=90,col="grey",size=3.5)+
          annotate(geom = "text", x = 0.5, adj=0.5, y = 0,vjust=(1.5), label ="No effect on peak discharge" ,col="grey",size=3.5)+
          geom_hline(yintercept = 0,col="grey")+
          geom_vline(xintercept = 0,col="grey")+
          annotate(geom = "text", y = 0.5, adj=0.5, x = 1*1.08,vjust=(0), label = "Total trapping",srt=90,col="grey",size=3.5)+
          annotate(geom = "text", x = 0.5, adj=0.5, y = 1*1.1,vjust=(1), label = "Total attenuation",col="grey",size=3.5)+
          coord_cartesian(ylim=c(-0.1,1.1),xlim=c(-0.1,1.1))+
          labs(x="Volume trapping [%]",y="Peak discharge attenuation [%]" )+
          theme(legend.direction = "horizontal",legend.position = "top",legend.key.height = unit(0.3, 'cm'))+
          scale_y_continuous(labels = scales::percent_format(scale = 100),breaks =c(0,0.2,0.4,0.6,0.8,1))+
          scale_x_continuous(labels = scales::percent_format(scale = 100),breaks =c(0,0.2,0.4,0.6,0.8,1))
        
        # Plot a synthesis figure of Qpeak out VS Vout, top right panel
        BottomLeftPanel<-ggplot(Result_all)+
          theme_bw(base_size = 9)
        
        if((max(Result_all$Vout,na.rm=TRUE) == min(Result_all$Vout,na.rm=TRUE))  | (max(Result_all$Qp_out,na.rm=TRUE) == min(Result_all$Qp_out,na.rm=TRUE)) )
        {
          BottomLeftPanel<-BottomLeftPanel+
            geom_point(aes(x=Vout/10^3,y=Qp_out),col=1,pch=22,fill=1,alpha=0.1)+
            theme(plot.margin = margin(t=0.1,r=0.1,b=1.5,l=0.4, "cm"))
        }else{
          BottomLeftPanel<-BottomLeftPanel+
            geom_bin2d(aes(x=Vout/10^3,y=Qp_out),col=1,#bins=20)+
                       binwidth=c(Events$Volume_BestEstimate[Event_Ind]/10^3/10,Events$PeakDischarge_BestEstimate[Event_Ind]/10))+
            scale_fill_gradient2(FillLegendLabel,low="deeppink1", high = "deeppink4")+ 
            theme(legend.direction = "horizontal",legend.position = "bottom",legend.key.height = unit(0.3, 'cm'))
        }
        
        BottomLeftPanel<-BottomLeftPanel+ 
          geom_point(aes(x=Vout/10^3,y=Qp_out,pch=Branch,col=Branch),alpha= AlphaN_runs)
        
        if(Perform_error_propagation == TRUE)
        {
          BottomLeftPanel<-BottomLeftPanel+ 
            # geom_point(data=Result_all_for_clogging,aes(x=Vout/10^3,y=Qp_out,pch="Intermediate")
                                                       # ,alpha= AlphaN_runs)+
            scale_color_viridis_d("",option = "D",direction = 1)+
            scale_shape_manual("",values=c(3,4,1,2)
                               # ,labels=c("Upper","Intermediate","Lower")
            )+
            guides(shape = guide_legend(order = 1,nrow=4,override.aes=list(size=2,alpha=1))
                   ,color=guide_legend(order = 1,nrow=4,override.aes=list(size=2,alpha=1))
                   ,fill = guide_colourbar(order = 2,title.position = "top")
                   )+
            coord_cartesian(ylim=c(0,Events$PeakDischarge_max[Event_Ind]*1.25)
                            ,xlim=c(0,Events$Volume_max[Event_Ind]/10^3*1.25))
            
        }else{
          BottomLeftPanel<-BottomLeftPanel+ 
            guides(color ="none",shape="none")+
            coord_cartesian(ylim=c(0,Events$PeakDischarge_BestEstimate[Event_Ind]*1.25)
                            ,xlim=c(0,Events$Volume_BestEstimate[Event_Ind]/10^3*1.25))
          }
        BottomLeftPanel<-BottomLeftPanel+ 
          labs(x="Released volume [*1000 m3]",y="Released Peak discharge [m3/s]")
        
        # Plot a synthesis figure on Qpeak out
        BottomRightPanel<-ggplot(Result_all)+
          theme_classic(base_size = 9)+
          geom_histogram(aes(y=Qp_out,fill=Branch)
                         ,binwidth=Events$PeakDischarge_BestEstimate[Event_Ind]/10)
        if(Perform_error_propagation == TRUE)
        {
          BottomRightPanel<-BottomRightPanel+
            scale_fill_viridis_d("Bound",option="D",direction = 1)+
            coord_cartesian(ylim=c(0,Events$PeakDischarge_max[Event_Ind])*1.25)
        }else{ BottomRightPanel<-BottomRightPanel+
          guides(fill="none")+
          coord_cartesian(ylim=c(0,Events$PeakDischarge_BestEstimate[Event_Ind])*1.25)
        }
        
        BottomRightPanel<-BottomRightPanel+
          geom_boxplot(aes(y=Qp_out,x=-1))+
          labs(y="Released peak discharge [m3/s]",x="# of Run")+
          guides(fill="none")
        
        # Plot a synthesis figure on Qpeak out
        BottomRightRightPanel<-ggplot(Result_all)+
          theme_classic(base_size = 9)+
          geom_histogram(aes(y=Qp_in,fill=Branch)
                         ,binwidth=Events$PeakDischarge_BestEstimate[Event_Ind]/10)
        if(Perform_error_propagation == TRUE)
        {
          BottomRightRightPanel<-BottomRightRightPanel+
            scale_fill_viridis_d("Bound",option="D",direction = 1)+
            coord_cartesian(ylim=c(0,Events$PeakDischarge_max[Event_Ind])*1.25)
        }else{
          BottomRightRightPanel<-BottomRightRightPanel+
            coord_cartesian(ylim=c(0,Events$PeakDischarge_BestEstimate[Event_Ind])*1.25)
        }
        
        BottomRightRightPanel<-BottomRightRightPanel+
          geom_boxplot(aes(y=Qp_in,x=-1))+
          geom_hline(yintercept = Events$PeakDischarge_BestEstimate[Event_Ind],col="grey")+
          annotate(geom = "text", x = 0, adj=0, y = Events$PeakDischarge_BestEstimate[Event_Ind]
                   ,vjust=(-0.2), label = "Total Event \n (Best. Est.)",srt=0,col="grey",size=3.5)+
          labs(y="Supplied peak discharge [m3/s]",x="# of Run")+
          guides(fill="none")

        
        if(Perform_error_propagation == FALSE)
        {
          Caption_text <-  paste("Code version:",ModelVersion,
                                 "\n",
                                 " |",
                                 "Parameters:"," best estimate values",
                                 "\n",
                                 " |",
                                 "Number of runs =",N_runs)
        }else{
          Caption_text <-  paste("Code version:",ModelVersion,
                                 "\n",
                                 " |",
                                 "Parameters:"," uncertain values",
                                 "\n",
                                 " |",
                                 "Number of runs = 4 x",N_runs)
        }
        
        
        FootNote<-ggplot(Result_all)+
          theme_void(base_size = 7)+labs(title=Caption_text)
        
        
        #Save figure
        
        png( paste0("FourPanelGraphReleasedVolume_Evt-",EventName,"_Structure_n",Structure_Ind,"-",StructureName,".png")
             , width = 17.5, height = 15,units="cm",res=350)
        {
          pushViewport(viewport(layout = grid.layout(11,12)))
          # Define region in the plot
          define_region <- function(row, col){viewport(layout.pos.row = row, layout.pos.col = col)}
          # Arrange panels
          print(TopightPanel, vp = define_region(1:5,8:12))
          print(TopTopLeftPanel+theme(plot.margin = margin(t=0.1,r=0.1,b=0.1,l=0.4, "cm"))
                , vp = define_region(1:4,1:6))
          print(TopLeftPanel+theme(plot.margin = margin(t=0.1,r=0.1,b=0.1,l=0.4, "cm"))
                , vp = define_region(5:6,1:6))
          
          print(BottomRightPanel+theme(plot.margin = margin(t=0.15,r=0.5,b=1.5,l=0.1, "cm"))#+labs(caption="")
                , vp = define_region(7:11,7:9))
          print(BottomRightRightPanel+theme(plot.margin = margin(t=0.15,r=0.5,b=1.4,l=0.1, "cm"))+
                  # labs(caption=Caption_text)+
                  theme(plot.caption =  element_text(size=8.5))
                , vp = define_region(7:11,10:12))
          print(FootNote,vp = define_region(11,9:12))
          print(BottomLeftPanel#+labs(caption="")
                , vp = define_region(7:11,1:6))
        }
        dev.off()
      }
      #Multi-run time series----
      {
          QplotIn<-ggplot(Qo_all,aes(x=Time/3600))+theme_bw(base_size = 9)+
            geom_line(aes(y=Qi,group = Run,col=Branch),alpha=AlphaN_runs)
            
            if(Perform_error_propagation == TRUE)
            {
              QplotIn<-QplotIn+
                scale_color_viridis_d(option="D",direction = 1)
            }
          
          QplotIn<-QplotIn+
            theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank()
                  ,legend.position = "top")+
            labs(y = "Inlet Discharge\n [m3/s]",
                 title = paste0("Time series of bounding runs (Event: ",EventName," at structure: ",StructureName,")"))+
            guides(linewidth="none")+
            guides(color="none")+
            theme(legend.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"))
          
          QplotOut<-ggplot(Qo_all,aes(x=Time/3600))+theme_bw(base_size = 9)+
            geom_line(aes(y=Qo,group =Run,col=Branch),alpha=AlphaN_runs)
          
          if(Perform_error_propagation == TRUE)
          {
            QplotOut<-QplotOut+
              scale_color_viridis_d(option="D",direction = 1)
          }
          
          QplotOut<-QplotOut+
            theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank())+
            labs(y = "Outlet Discharge\n [m3/s]")+
            guides(linewidth="none")+
            theme(legend.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"))+
            guides(color="none")
          
          Vplot<-ggplot(Qo_all)+theme_bw(base_size = 9)+
            geom_line(aes(x=Time/3600,y=V,group=Run,col=Branch),alpha=AlphaN_runs)
          
          if(Perform_error_propagation == TRUE)
          {
            Vplot<-Vplot+
              scale_color_viridis_d(option="D",direction = 1)
          }
          
          Vplot<-Vplot+
            theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank())+
            guides(linewidth="none")+
            labs( x = "Time [h]",y = "Stored volume\n [*1000m3]")+
            guides(color="none")
          
          Zplot<-ggplot(Qo_all)+theme_bw(base_size = 9)+
            geom_line(aes(x=Time/3600,y=Z,lty="4",group=Run,col=Branch),alpha=AlphaN_runs)+
            geom_line(aes(x=Time/3600,y=BaseLevelJam,lty="5",group=Run,col=Branch),alpha=AlphaN_runs)
          
          if(Perform_error_propagation == TRUE)
          {
            Zplot<-Zplot+
              scale_color_viridis_d(option="D",direction = 1)
          }else{Zplot<-Zplot+guides(color="none")}
          
          Zplot<-Zplot+
            scale_linetype_manual(name="Level",label=c("Flow","Basal boulder jam"),values=c(1,4))+
            theme(legend.position = "bottom"#c(0.82,0.83)
                  ,legend.direction = "horizontal")+
            guides(color = guide_legend(order = 1,nrow=2,override.aes=list(alpha=1,lwd=2))
                   ,linetype = guide_legend(order = 2,nrow=2))+
            labs( x = "Time [h]",y = "Flow level\n [m]")
          
         png(paste0("SyntheticTimeSerie_Evt-" ,EventName   ,"_Structure_n"   ,Structure_Ind,"-"       ,StructureName,".png")
              , width = 16, height = 15,units="cm",res=350)
          {
            pushViewport(viewport(layout = grid.layout(26,1) ) )
            
            # Arrange graphs
            print(QplotIn      , vp = define_region(1:6,1))
            print(QplotOut     , vp = define_region(7:11,1))
            print(Vplot        , vp = define_region(12:15,1))
            print(Zplot  +  labs(caption=Caption_text)+ 
                    theme(plot.caption =  element_text(size=8.5))        
                  , vp = define_region(16:26,1))
          }
          dev.off() 
      }
      
      ### Four panel graph for Z_max and Clogging rate ------
      {
        #Histograms and the 2Dbin plot is plotted with accuracy of 1/10 of Zmax - bottom outlet base level
        ZbinWidth<-(max(Result_all$Zmax,na.rm = TRUE)-min(Structures$Openings[[Structure_Ind]]$BaseLevel,na.rm = TRUE))/10
        
        
        TopTopLeftPanel<-ggplot(Result_all)+
          theme_classic(base_size = 9)+
          stat_ecdf(aes(1-ResidualOpening))+
          coord_cartesian(xlim=c(0,1))+
          scale_x_continuous(labels = scales::percent_format(scale = 100),breaks =c(0,0.2,0.4,0.6,0.8,1))+
          labs(x="", y="CDF",title = paste0("Maximum flow level and general clogging ratio of the structure\n",
                               "|Event: ",EventName,"\n","|Structure: ",StructureName,""))
        
        TopLeftPanel<-ggplot(Result_all)+
          theme_classic(base_size = 9)+
          geom_histogram(aes(1-ResidualOpening,fill=Branch)
                         ,binwidth=1/10)+
          geom_boxplot(aes(x=1-ResidualOpening,y=-1))+
          coord_cartesian(xlim=c(0,1))+
          scale_x_continuous(labels = scales::percent_format(scale = 100),breaks =c(0,0.2,0.4,0.6,0.8,1))+
          labs(x="General clogging of the structure [-]",y="# of Run"
               # ,title = paste0("Maximum flow level and general clogging ratio of the structure\n",
                               # "|Event: ",EventName,"\n","|Structure: ",StructureName,"")
               )+
          theme(legend.position = "top",legend.box.background = element_rect(colour = 1),legend.key.height = unit(0.3, 'cm') )
        
        
        if(Perform_error_propagation == TRUE)
        {
          TopLeftPanel<-TopLeftPanel+
            scale_fill_viridis_d("Bound",option="D",direction = 1)+
            guides(fill = guide_legend(order = 1,nrow=4    ))
        }
        

        TopRightPanel<-ggplot(Result_all)+
          theme_void(base_size = 8)+
          labs(title=paste0("Nota: The general clogging\n"
                            ,"of the structure is computed\n"
                            ,"only considering the slots for \n"
                            ,"bridges, and slot and slit\n"
                            ,"openings for barriers, i.e. \n"
                            ,"openings coded as weir are \n"
                            ,"always ignored in its comp-\n"
                            ,"utation, as well as slits \n"
                            ,"elements in bridges."))+
          theme(plot.title = element_text(hjust=0.5))
        
        
        BottomLeftPanel<-ggplot(Result_all)+
          theme_bw(base_size = 9)
        
        if((max(Result_all$ResidualOpening,na.rm = TRUE) == min(Result_all$ResidualOpening,na.rm = TRUE))  | (max(Result_all$Zmax,na.rm = TRUE) == min(Result_all$Zmax,na.rm = TRUE)) )
        {
          BottomLeftPanel<-BottomLeftPanel+
            geom_point(aes(x=1-ResidualOpening,y=Zmax),col=1,pch=22,fill=1,alpha=0.1)+
            theme(plot.margin = margin(t=0.1,r=0.1,b=1.5,l=0.4, "cm"))
        }else{
          BottomLeftPanel<-BottomLeftPanel+
            geom_bin2d(aes(x=1-ResidualOpening,y=Zmax),col=1,
                       binwidth=c(1/10,ZbinWidth))+
            scale_fill_gradient2(FillLegendLabel,low="deeppink1", high = "deeppink4")+ 
            theme(legend.direction = "horizontal",legend.position = "bottom",legend.key.height = unit(0.3, 'cm'))
        }
        
        BottomLeftPanel<-BottomLeftPanel+ 
          geom_point(aes(x=1-ResidualOpening,y=Zmax,pch=Branch,col=Branch),alpha= AlphaN_runs)
        
        if(Perform_error_propagation == TRUE)
        {
          BottomLeftPanel<-BottomLeftPanel+
            scale_color_viridis_d("",option = "D",direction = 1)+
            scale_shape_manual("",values=c(3,4,1,2) )+
            guides(shape = guide_legend(order = 1,nrow=4,override.aes=list(size=2,alpha=1))
                   ,color = guide_legend(order = 1,nrow=4,override.aes=list(size=2,alpha=1))
                   ,fill = guide_colourbar(order = 2,title.position = "top"))
            
        }else{ BottomLeftPanel<-BottomLeftPanel+guides(color="none",shape="none")    }
        
      BottomLeftPanel<-BottomLeftPanel+ 
          geom_vline(xintercept = 0,col="grey")+
          geom_vline(xintercept = 1,col="grey")+
          annotate(geom = "text", y = 0.5*(min(Structures$Openings[[Structure_Ind]]$BaseLevel)+max(Result_all$Zmax)+.5)
                   , adj=0.5, x = 0,vjust=(-0.5), label = "No clogging",srt=90,col="grey",size=3.5)+
          annotate(geom = "text", x = 1, adj=0.5, y = 0.5*(min(Structures$Openings[[Structure_Ind]]$BaseLevel)+max(Result_all$Zmax)+.5)
                   ,vjust=(1.5), label ="Total obstruction",srt=90 ,col="grey",size=3.5)+
          geom_hline(yintercept = min(Structures$Openings[[Structure_Ind]]$BaseLevel),col="grey")+
          annotate(geom = "text", x = 0.5, adj=0.5, y = min(Structures$Openings[[Structure_Ind]]$BaseLevel)
                   ,vjust=(-.5), label ="Outlet base level" ,col="grey",size=3.5)+
          coord_cartesian(xlim=c(0,1),ylim=c(min(Structures$Openings[[Structure_Ind]]$BaseLevel),max(Result_all$Zmax)+.5))+
          scale_x_continuous(labels = scales::percent_format(scale = 100),breaks =c(0,0.2,0.4,0.6,0.8,1))+
          labs(x="General clogging of the structure [-]",y="Maximum flow level [m]")
        
        
        # Plot a synthesis figure on Zmax
        BottomRightPanel<-ggplot(Result_all)+
          theme_classic(base_size = 9)+
          geom_histogram(aes(y=Zmax,fill=Branch),binwidth=ZbinWidth)+
          geom_boxplot(aes(y=Zmax,x=-1))+
          coord_cartesian(ylim=c(min(Structures$Openings[[Structure_Ind]]$BaseLevel),max(Result_all$Zmax)+.5))+
          labs(y="Maximum flow level [m]",x="# of Run")+guides(fill="none")
        
        if(Perform_error_propagation == TRUE)
        {
          BottomRightPanel<-BottomRightPanel+
            scale_fill_viridis_d("Bound",option="D",direction = 1)
        }else{ BottomRightPanel<-BottomRightPanel+guides(fill="none")    }
        
        
        #Save figure
        
        png( paste0("FourPanelGraphMaxFlowLevelAndCloggingRate_Evt-"
                    ,EventName,"_Structure_n",Structure_Ind,"-",StructureName,".png")
             , width = 17.5, height = 15,units="cm",res=350)
        {
          pushViewport(viewport(layout = grid.layout(10,12)))
          # Arrange panels
          print(TopTopLeftPanel+theme(plot.margin = margin(t=0.1,r=0.1,b=0.1,l=0.4, "cm"))
                , vp = define_region(1:3,1:9))
          if(Perform_error_propagation == TRUE)
          {
            print(TopLeftPanel+theme(plot.margin = margin(t=0.1,r=0.1,b=0.1,l=0.4, "cm"))+theme(legend.position = "right")
                  , vp = define_region(4:5,1:12))
          }else{
            print(TopLeftPanel+theme(plot.margin = margin(t=0.1,r=0.1,b=0.1,l=0.4, "cm"))+
                    guides(fill="none")
                  , vp = define_region(4:5,1:9))
          }
          
          
          print(TopRightPanel+theme(plot.margin = margin(t=0.1,r=0.1,b=0.1,l=0.0, "cm"))
                , vp = define_region(1:3,10:12))
          print(BottomLeftPanel+          labs(caption="")
                , vp = define_region(6:10,1:9))
          print(BottomRightPanel+theme(plot.margin = margin(t=0.15,r=0.5,b=1.3,l=0.1, "cm"))+
                  labs(caption=Caption_text)+
                  theme(plot.caption =  element_text(size=8.5))        
                , vp = define_region(6:10,10:12))
        }
        dev.off() 
      }
      #Boulder inventory plot----
      # 
      # ggplot(Qo_all,aes(x=Time/3600))+theme_bw(base_size = 9)+
      #   # geom_col(a♣es(y=-Class3.unjammed))+
      #   geom_col(aes(y=Class1.unjammed),alpha=1)+
      #   geom_col(aes(y=Class2.unjammed),alpha=0.3)+
      #   geom_col(aes(y=-Class1.jammed),alpha=1)+
      #   geom_col(aes(y=-Class2.jammed),alpha=0.3)#+
      #   # geom_col(aes(y=Class3.unjammed),alpha=0.5)+
      # geom_col(aes(y=Class4.unjammed))
      # # 
      # # 
      # # 
      # Test<-Qo_all %>% select(c(11:22,24)) %>%
      #   # group_by(Run) %>%
      #   summarise(across(everything(), ~ sum(., na.rm = TRUE)))
      
    }# end of the synthesis plot loop
  }# end of the plot structure loop
  
  ## Define if another run is to be launched
  if(HEADLESS){
    PerformAnotherSimulation = "no"
  } else {
    ListFile<-list.files()
    #Define where to save the results
    dlg_message(message="The simulation is finished! Show me where you want to store the results. Beware the it will overwrite existing files with same name! If you choose the /out/ repository, it will erase the previous simulation."
                , type = c("ok")); SaveRep<-dlg_dir(title="The simulation is finished! Show me where you want to store the results. Beware the it will overwrite existing files with same name! If you choose the /out/ repository, it will erase the previous simulation.",default = getwd())$res
    
    file.copy(from=ListFile,to=SaveRep,overwrite = TRUE)                                                      
    file.remove(ListFile)
    
    #Want to perform another run
    PerformAnotherSimulation<-dlg_message(message="Ok, done. Do you want to perform another set of simulations?", type = c("yesno"))$res
  }
}


