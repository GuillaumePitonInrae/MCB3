### Main code calling the sub-routines depending on the user choice

#Clean environment
rm(list=ls())
library(jsonlite)

# Setup HEADLESS variables
HEADLESS <- !base::interactive()
HEADLESS = TRUE #REMOVE !
if(HEADLESS) {
  print('Running in HEADLESS mode')
  args<-commandArgs(trailingOnly = TRUE)
  #François
  MainRep<-"/home/francois/Documents/Micro-entreprise/projets/DFBuffering/DFbuffering/"
  # Guillaume
  MainRep<-"D:/Private/05_PROJETS/2023_DFbuffering/4Simu/DFbuffering"
  args[1] <- paste0(MainRep,"/params.json") #REMOVE !
  args[2] <- paste0(MainRep,"/out") #REMOVE !
  json <- jsonlite::fromJSON(args[1])
  # Transfer all first-level json props to global scope:
  list2env(json,globalenv())

  # But in the future, the env method above may not be acceptable, in which case prefer
  # using the following syntax: 
  # OnlyNormalRun <<- json$OnlyNormalRun
  # ... and so on...
}


# Load package
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

#start the clock for computation time recording
# ptm <- proc.time()

#Model version
Model <- "CheekyeDebrisFlowBarrier V3.0"
SourceCodeRepository<-paste0(getwd(),"/0SourceCodes")
if(!HEADLESS)
{
  #Selecting the repository where the source codes are stored----
  dlg_message(message="Show me where are stored the source codes (Repository \"/0SourceCodes\")"
              , type = c("ok")) ; SourceCodeRepository<-dlg_dir(title="Show me where are stored the source codes (Repository \"/0SourceCodes\")"
                                                                ,default = getwd())$res
              
}
#Set this repository as working repository
setwd(SourceCodeRepository)

#Load the source codes
source("PLOT_INPUTnew.R")#To plot input distrib
source("CheckTriangleDistribution.R") #To check input data distributions
source("DischargeCapacityFunctions_V0.R")#Load slit capacity function
source("BoulderPassing_V2.R")#Compute the number of boulders passing through an orifice
source("Plot_BufferingModelResults_V0.R") #For plotting results of singular runs
source("Create_inlet_input_V0.1.R")#define the input data of runs
source("Create_inlet_timeseries_V0.1.R")#define the times series of the inlet of the most upstream structure
source("Structure_definition_V0.1.R")#define structure
source("Structure_functionning_V0.1.R")#Actual buffering model
source("BoulderTransfer_V0.1.R")#Compute the transformation of the time series from one structure to another

if(!HEADLESS){
  #Selecting the repository where the source codes are stored
  dlg_message(message="Show me where are stored the input data (Repository \"/1Data\")"
  , type = c("ok")) ; InputDataRepository<-dlg_dir(title="Show me where are stored the input data (Repository \"/1Data\")"
  ,default = getwd())$res

  #Load the structure list and organisation
  Structure_organisation<-read.csv(paste0(InputDataRepository,"/StructureList.txt"),sep="\t")
  #Load initial conditions
  InitialConditions<-read.csv(paste0(InputDataRepository,"/InitialConditions.txt"),sep="\t")
  #Reorganize the table
  Structure_organisation<-data.frame(
    Name = Structure_organisation$Value[which(Structure_organisation$Variable=="Structure")],
    InitialCondition = Structure_organisation$Value[which(Structure_organisation$Variable=="Structure")+1],
    Transfer = Structure_organisation$Value[which(Structure_organisation$Variable=="Structure")+2]
  )


  #Import the structure description
  Structures<-structure_definition(InputDataRepository)
  Structures$TransferDownstream<-Structure_organisation$Transfer[match(Structure_organisation$Name,Structures$Name)]
  Structures$InitialConditions<-InitialConditions[match(Structure_organisation$InitialCondition,InitialConditions$Name),]
  # FK TODO: Convert Structure_description and Structure_organisation to a global "Structures" variable, such that it matches the
  # Structures set in the json file.
}

#Selecting the repository where one want to record the results
if(HEADLESS) {
  MainRepository <- args[2]
} else {
  setwd(InputDataRepository)
  dlg_message(message="Show me where you want to store the results (e.g., the parent directory where /0SourceCode and /1Data are stored)"
              , type = c("ok"));MainRepository<-dlg_dir(title="Show me where you want to store the results (e.g., the parent directory where /0SourceCode and /1Data are stored)"
                                                        ,default = getwd())$res
}
#Set this repository as working repository
setwd(MainRepository)
dir.create("2Outputs",showWarnings = FALSE)

#Main loop within which each set of run is performed
PerformAnotherSimulation<-"yes"
while(PerformAnotherSimulation=="yes")
{
  
  if(!HEADLESS){
    #Select the type of approach----
    # OnlyNormalRun<-dlg_message(message="Press \"Yes\" to perform normal runs (using best estimates of the input data) \n Or press \"No\" to run a full uncertainty propagation analysis "
    #                            , type = c("yesno"))$res
    
    OnlyNormalRun<-"yes"
    if(OnlyNormalRun=="yes"){ OnlyNormalRun<-TRUE}else{ OnlyNormalRun<-FALSE}
    #Define the number of simulations to run----
    N.unvalidated<-TRUE
    while(N.unvalidated)
    {
      if(OnlyNormalRun)
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
  
  
    # Choices for the simulation----
  if(OnlyNormalRun)
  {
    ComputeWithBestEstimateBoulderNumber<-TRUE
    ComputeWithBestEstimateBoulderNumber<-FALSE
  }else{
    ComputeWithBestEstimateBoulderNumber<-FALSE
    ComputeWithBestEstimateBoulderNumber<-TRUE
  }
  if(!HEADLESS){
    #Saving synthesis figure for each run?
    PrintFinalPlot<-dlg_message(message="Do you want to print a synthesis plot for each run (hydrographs, flow level, volume stored) in a .png file?", type = c("yesno"))$res
    if(PrintFinalPlot=="yes"){ PrintFinalPlot<-TRUE}else{ PrintFinalPlot<-FALSE}
    #Saving details?
    SaveDetailsResults<-dlg_message(message="Do you want to save the full result details (hydrographs, boulder sizes) in .Rdata files?", type = c("yesno"))$res
    if(SaveDetailsResults=="yes"){ SaveDetailsResults<-TRUE}else{ SaveDetailsResults<-FALSE}
  }
  if(SaveDetailsResults)
  {
    SaveBoulderSize<-TRUE
    SaveHydrographs<-TRUE
    SaveRsltSingleRun<-TRUE
  }else{
    SaveBoulderSize<-FALSE
    SaveHydrographs<-FALSE
    SaveRsltSingleRun<-FALSE
  }
  
  #Disabled options in this version
  PrintDataPlot<-FALSE #Plot stage - discharge Q(h) curve and stage - volume V(h) curves
  TestAbsenceBottomJam<-FALSE #remove the initial blockage level y_0 of the initial conditions
  
  #Want to save the final state of clogging to use it for the next run?
  KeepTrackOfCloggingState<-FALSE
  
  # Time step
  TimeStep<-1 #(s) Should be an integer in seconds, so no less than 1 second
  # small enough to capture the peak, the code seems to get stuck in infinite loops if set at 5 s with low slopes
  
  if(!HEADLESS) {
    ##########################################To REMOVE --------------------
    #Load boulder list
    Boulders<-read.csv(paste0(InputDataRepository,"/RangeOfBoulders.txt"),sep="\t")
    #Load event features
    Events<-read.csv(paste0(InputDataRepository,"/Events.txt"),sep="\t")
    ##########################################To REMOVE
  }
  
  # Create input data to launch runs----
  if(OnlyNormalRun)
  {
    #If only normal runs, we only use the best estimates
    BoulderGenerationMode<-"Best estimate numbers"
    
    # Define the event to model----
    EventUndefined<-TRUE
    while(EventUndefined)
    {
      if(OnlyNormalRun){ # Possible to reuse predefined values or to define the event manually
        EventName<-dlg_input(message = c("Write the name of the event you want to model, the available names are :"
                                          ,Events$Name
                                          ,"If you want to define the event manually, write \"0\" ")
                              ,default = Events$Name[1])$res
        AdjustEventManually <- TRUE
        
      }else{#Only possible to reuse predefined values 
        EventName<-dlg_input(message = c("Write the name of the event you want to model, the available names are :"
                                          ,Events$Name)
                              ,default = Events$Name[1])$res
        AdjustEventManually <- TRUE
      } 
      
      if(EventName == "0"){#Define values manually
        AdjustEventManually <- TRUE
        EventName<-dlg_input(message = c("OK we will adjust an event, which one do you want to take as template?, the available names are :"
                                          ,Events$Name)
                              ,default = Events$Name[1])$res
      }else{AdjustEventManually<-FALSE}
      
      if(EventName %in% Events$Name){EventUndefined<-FALSE}else{
        dlg_message(message="The name you wrote is not in the list of the available events, please provide an available event name"
                    , type = c("ok"))
      }
    }
    #Create input data according to the EventName and adjustement option
    input<-Create_inlet_input_V0.1(EventName,AdjustEventManually)
    
    #Create input timesseries accordingly
    Qin<-Create_inlet_timeseries_V0.1(input,Boulders)
    
    #Computation at each structure
    for(Structure_Ind in 1:length(Structures$Name))
    {
      #This line is useless, we rather call the [Structure_Ind] element of the 
      # structure list
      # Structure <- Structures[Structure_Ind,]
            
      if(Structure_Ind > 1){Qo_all_upstream<-Qo_all}
      
      
      ## Computation of isolated runs ----
      for(Run_Ind in (1:N_runs))
      {
        if(Structure_Ind > 1)
        {
          transfer <- Structures$TransferDownstream[[Structure_Ind-1]]
          if(transfer == "Instantaneous")
          {
            Vmixing<-NA
          }else{
            Vmixing <- as.numeric(substr(transfer
                                         ,8
                                         ,nchar(transfer)))
          }
          Qin<-Transfer_Between_Structure(Qo = Qo_all_upstream %>% filter(Run == paste0("Run #",Run_Ind))
                                          ,Transfer.Type = transfer
                                          ,Vmixing = Vmixing)
        }
        #################updating of the initial conditions in input[5] et input[6]
        input[5]<-Structures$InitialConditions$InitialDepositHeight_BestEstimate[Structure_Ind]
        
        input[6]<-Structures$InitialConditions$InitialJammingHeight_BestEstimate[Structure_Ind]
        
        # NOTE/FIXME ABOUT Structure$Opening[[1]] and Structure$StorageElevation[[1]]: jsonlite seems to encaspulate json list of objects into a list of size one instead of creating directly a DF. So we must call the first list element with [[1]] in the lines below, but this weird behavior should be fixed in the future. 
        #launch computation
        Qo<-Structure_functionning_V0.1(input,Qin
                                        ,Opening=as.data.frame(Structures$Opening[[Structure_Ind]])
                                        ,StorageElevation=as.data.frame(Structures$StorageElevation[[Structure_Ind]])
        )
        Qo$Run<-paste0("Run #",Run_Ind)
        
        Result<-Synthetic_Structure_results_V0.1(Qo, Structures$Opening[[Structure_Ind]])
        print(paste0("Run #",Run_Ind," finished at, ",now(),", still ",N_runs-Run_Ind," to perform for structure ",Structures$Name[[Structure_Ind]]))
       
         #Record the run results
        if(Run_Ind==1){
          Result_all<-Result
          Qo_all<-Qo
        }else{
          Result_all<-rbind(Result_all,Result)
          Qo_all<-rbind(Qo_all,Qo)
        }
      }
      #Save a data frame with the main results of all runs as a .Rdata file
      save(Result_all,Qo_all,file=paste0("2Outputs/Result_Evt-",EventName,"_Structure_",Structures$Name[[Structure_Ind]],".RData"))
      
      #If more than 10 runs, plot histogramms and a scatter plot of Qmax and V
      if(N_runs>=10)
      {
        # Find the event in the table
        Magnitude.class<-which(Events$Name==EventName)
        
        # Plot a synthesis figure on Vout
        ggplot(Result_all)+
          theme_bw(base_size = 9)+
          geom_histogram(aes(Vout/10^3))+
          geom_boxplot(aes(x=Vout/10^3,y=-1))+
          geom_vline(xintercept = Events$Volume_BestEstimate[Magnitude.class]/10^3)+
          annotate(geom = "text", y = 0, adj=c(0,0), x = Events$Volume_BestEstimate[Magnitude.class]/10^3
                   ,vjust=(-0.5), label = "Supply (Best. Est.)",srt=90)+
          coord_cartesian(xlim=c(0,Events$Volume_BestEstimate[Magnitude.class]/10^3))+
          labs(x="Released volume [*1000 m3]",y="count"
               ,caption=paste("Code of",Model," used on", lubridate::today(),"| Number of runs N =",N_runs)
               ,title = paste("Distribution of released volume for event:",EventName))
        #Save figure
        ggsave(paste0("2Outputs/ReleasedVolume_Evt-",EventName,"_Nrun_",N_runs,"_Structure_",Structures$Name[[Structure_Ind]],"_ParametersAsBestEstimates.png")
               , width = 11, height = 7,units="cm")
        
        # Plot a synthesis figure on Qpeak out
        ggplot(Result_all)+
          theme_bw(base_size = 9)+
          geom_histogram(aes(Qp.out))+
          geom_boxplot(aes(x=Qp.out,y=-1))+
          geom_vline(xintercept = Events$PeakDischarge_BestEstimate[Magnitude.class])+
          annotate(geom = "text", y = 0, adj=c(0,0), x = Events$PeakDischarge_BestEstimate[Magnitude.class]
                   ,vjust=(-0.5), label = "Supply (Best. Est.)",srt=90)+
          coord_cartesian(xlim=c(0,Events$PeakDischarge_BestEstimate[Magnitude.class]))+
          labs(x="Peak discharge [m3/s]",y="count"
               ,caption=paste("Code of",Model," used on", lubridate::today(),"| Number of runs N =",N_runs)
               ,title = paste("Distribution of released peak dischage for event:",EventName))
        
        #Save figure
        ggsave(paste0("2Outputs/ReleasedQpeak_Evt-",EventName,"_Nrun_",N_runs,"_Structure_",Structures$Name[[Structure_Ind]],"_ParametersAsBestEstimates.png")
               , width = 11, height = 7,units="cm")
        
        # Plot a synthesis figure of Qpeak out VS Vout
        ggplot(Result_all)+
          theme_bw(base_size = 9)+
          geom_bin2d(aes(x=Vout/10^3,y=Qp.out))+
          scale_fill_continuous("# of Run")+
          geom_hline(yintercept = Events$PeakDischarge_BestEstimate[Magnitude.class])+
          geom_vline(xintercept = Events$Volume_BestEstimate[Magnitude.class]/10^3)+
          annotate(geom = "text", y = 0, adj=c(0,0), x = Events$Volume_BestEstimate[Magnitude.class]/10^3
                   ,vjust=(-0.5), label = "Supply (Best. Est.)",srt=90)+
          annotate(geom = "text", x = 0, adj=c(0,0), y = Events$PeakDischarge_BestEstimate[Magnitude.class]
                   ,vjust=(1.2), label = "Supply (Best. Est.)")+
          coord_cartesian(ylim=c(0,Events$PeakDischarge_BestEstimate[Magnitude.class])
                          ,xlim=c(0,Events$Volume_BestEstimate[Magnitude.class]/10^3))+
          labs(x="Released volume [*1000 m3]",y="Peak discharge [m3/s]"
               ,caption=paste("Code of",Model," used on", lubridate::today(),"\n Number of runs N =",N_runs)
               # ,title = paste("Released volume and released peak dischage for event:",EventName)
          )
        
        #Save figure
        ggsave(paste0("2Outputs/ReleasedVolume-VS-Qpeak_Evt-",EventName,"_Nrun_",N_runs,"_Structure_",Structures$Name[[Structure_Ind]],"_ParametersAsBestEstimates.png")
               , width = 10, height = 7,units="cm") 
      }
    }
    
    if(HEADLESS){
      PerformAnotherSimulation = "no"
    } else {
    #Want to perform another run
      PerformAnotherSimulation<-dlg_message(message="The computation is finished! \n Do you want to perform another set?"
                                          , type = c("yesno"))$res
    }
  }
}
