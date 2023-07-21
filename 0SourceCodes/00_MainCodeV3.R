### Main code calling the sub-routines depending on the user choice
#V0.1 July 2023 - G. Piton & C. Misset


#Clean environment
rm(list=ls())


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
#start the clock for computation time recording
# ptm <- proc.time()

#Model version
Model <- "CheekyeDebrisFlowBarrier V3.0"

#Selecting the repository where the source codes are stored----
dlg_message(message="Show me where are stored the source codes (Repository \"/0SourceCodes\")"
            , type = c("ok")) ; SourceCodeRepository<-dlg_dir(title="Show me where are stored the source codes (Repository \"/0SourceCodes\")"
                                                           ,default = getwd())$res


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

#Selecting the repository where the source codes are stored
dlg_message(message="Show me where are stored the input data (Repository \"/1Data\")"
, type = c("ok")) ; InputDataRepository<-dlg_dir(title="Show me where are stored the input data (Repository \"/1Data\")"
,default = getwd())$res



#Load initial conditions
InitialConditions<-read.csv(paste0(InputDataRepository,"/InitialConditions.txt"),sep="\t")

#Main loop within which each set of run is performed
Perform.Another.Simulation<-"yes"
# while(Perform.Another.Simulation=="yes")
{
  #Select the type of approach----
  # OnlyNormalRun<-dlg_message(message="Press \"Yes\" to perform normal runs (using best estimates of the input data) \n Or press \"No\" to run a full uncertainty propagation analysis "
  #                            , type = c("yesno"))$res
  
  OnlyNormalRun<-"yes"
  #Define the number of simulations to run----
  N.unvalidated<-TRUE
  while(N.unvalidated)
  {
    if(OnlyNormalRun=="yes")
    {N.Runs<-as.numeric(dlg_input(message = "How many simulations to you want to run (n<10 000)"
                                  , default = "5")$res)
    }else
    {N.Runs<-as.numeric(dlg_input(message = "How many simulations to you want to run (10<n<10 000)"
                                  , default = "25")$res)
    }
    if(N.Runs<1001){
      N.unvalidated<-FALSE
    }else{
      if(N.Runs<10001){
        Validation<-dlg_message(message="It will probably take hours! Sure?", type = c("yesno"))$res
        if(Validation=="yes"){N.unvalidated<-FALSE}else{N.unvalidated<-TRUE}
      }else{
        Validation<-dlg_message(message="It will likely take days! Sure?", type = c("yesno"))$res
        if(Validation=="yes"){N.unvalidated<-FALSE}else{N.unvalidated<-TRUE}
      }
      rm(Validation)
    }
  }
  
  #Selecting the repository where one want to record the results
  setwd(InputDataRepository)
  dlg_message(message="Show me where you want to store the results (e.g., the parent directory where /0SourceCode and /1Data are stored)"
              , type = c("ok"));MainRepository<-dlg_dir(title="Show me where you want to store the results (e.g., the parent directory where /0SourceCode and /1Data are stored)"
                                                        ,default = getwd())$res


  #Set this repository as working repository
  setwd(MainRepository)
  #Prepare subrepositories
  dir.create("2Outputs",showWarnings = FALSE)
  dir.create("2Outputs/Boulders",showWarnings = FALSE)
  dir.create("2Outputs/Buffering",showWarnings = FALSE)
  dir.create("2Outputs/Hydrographs",showWarnings = FALSE)
  dir.create("2Outputs/Pbox",showWarnings = FALSE)
  dir.create("2Outputs/Rdata",showWarnings = FALSE)
  
    # Choices for the simulation----
  if(OnlyNormalRun=="yes")
  {
    Compute.With.Best.Estimate.Boulder.Number<-TRUE
    Compute.With.Uncertain.Boulder.Number<-FALSE
  }else{
    Compute.With.Best.Estimate.Boulder.Number<-FALSE
    Compute.With.Uncertain.Boulder.Number<-TRUE
  }
  #Saving synthesis figure for each run?
  Print.Final.Plot<-dlg_message(message="Do you want to print a synthesis plot for each run (hydrographs, flow level, volume stored) in a .png file?", type = c("yesno"))$res
  if(Print.Final.Plot=="yes"){ Print.Final.Plot<-TRUE}else{ Print.Final.Plot<-FALSE}
  #Saving details?
  Save.details<-dlg_message(message="Do you want to save the full result details (hydrographs, boulder sizes) in .Rdata files?", type = c("yesno"))$res
  if(Save.details=="yes")
  {
    Save.boulder.size<-TRUE
    Save.hydrographs<-TRUE
    Save.Rslt.Single.Run<-TRUE
  }else{
    Save.boulder.size<-FALSE
    Save.hydrographs<-FALSE
    Save.Rslt.Single.Run<-FALSE
  }
  
  #Disabled options in this version
  Print.Data.Plot<-FALSE #Plot stage - discharge Q(h) curve and stage - volume V(h) curves
  Test.Absence.Bottom.Jam<-FALSE #remove the initial blocage level y_0 of the initial conditions
  Kill.boulder.transport.after.input.surge<-FALSE #no more boulders are randomly sampled after the input surge is finished. 
  #Want to save the final state of clogging to use it for the next run?
  Keep.track.of.clogging.state<-FALSE
  
  # Time step
  TimeStep<-1 #(s) Should be an integer in seconds, so no less than 1 second
  # small enough to capture the peak, the code seems to get stuck in infinite loops if set at 5 s with low slopes
  
  
  ##########################################To REMOVE --------------------
  #Load boulder list
  Boulders<-read.csv(paste0(InputDataRepository,"/RangeOfBoulders.txt"),sep="\t")
  #Load event features
  Events<-read.csv(paste0(InputDataRepository,"/Events.txt"),sep="\t")
  ##########################################To REMOVE 
  
  # Create input data to launch runs----
  if(OnlyNormalRun=="yes")
  {
    #If only normal runs, we only use the best estimates
    Boulder.Generation.Mode<-"Best estimate numbers"
    
    # Define the event to model----
    Event.undefined<-TRUE
    while(Event.undefined)
    {
      if(OnlyNormalRun=="yes"){ # Possible to reuse predefined values or to define the event manually
        Event.name<-dlg_input(message = c("Write the name of the event you want to model, the available names are :"
                                          ,Events$Name
                                          ,"If you want to define the event manually, write \"0\" ")
                              ,default = Events$Name[1])$res
        Adjust.event.manually <- TRUE
        
      }else{#Only possible to reuse predefined values 
        Event.name<-dlg_input(message = c("Write the name of the event you want to model, the available names are :"
                                          ,Events$Name)
                              ,default = Events$Name[1])$res
        Adjust.event.manually <- TRUE
      } 
      
      if(Event.name == "0"){#Define values manually
        Adjust.event.manually <- TRUE
        Event.name<-dlg_input(message = c("OK we will adjust an event, which one do you want to take as template?, the available names are :"
                                          ,Events$Name)
                              ,default = Events$Name[1])$res
      }else{Adjust.event.manually<-FALSE}
      
      if(Event.name %in% Events$Name){Event.undefined<-FALSE}else{
        dlg_message(message="The name you wrote is not in the list of the available events, please provide an available event name"
                    , type = c("ok"))
      }
    }
    #Create input data according to the event.name and adjustement option
    input<-Create_inlet_input_V0.1(Event.name,Adjust.event.manually)
    
    #Create input timesseries accordingly
    Qin<-Create_inlet_timeseries_V0.1(input,Boulders)
    
    ## Computation of isolated runs ----
    for(Run.ind in (1:N.Runs))
    {
      #launch computation
      Qo<-Structure_functionning_V0.1(input,Qin,Opening,StorageElevation)
      
      Qo$Run<-paste0("Run #",Run.ind)
      
      Result<-Synthetic_Structure_results_V0.1(Qo)
      print(paste0("Run #",Run.ind," finished at, ",now(),", still ",N.Runs-Run.ind," to perform"))
      #Record the run results
      if(Run.ind==1){
        Result.all<-Result
        Qo.all<-Qo
      }else{
          Result.all<-rbind(Result.all,Result)
          Qo.all<-rbind(Qo.all,Qo)
          }
    }
    #Save a data frame with the main results of all runs as a .Rdata file
    save(Result.all,Qo.all,file=paste0("2Outputs/Rdata/Result_Evt-",Event.name,"_Run_",Run.ind,".RData"))
    
    if(N.Runs>=10)
    {
      # Plot a synthesis figure on Vout
      ggplot(Result.all)+
        theme_bw(base_size = 9)+
        geom_histogram(aes(Vout/10^3))+
        geom_boxplot(aes(x=Vout/10^3,y=-1))+
        geom_vline(xintercept = Events$Volume_BestEstimate[Magnitude.class]/10^3)+
        annotate(geom = "text", y = 0, adj=c(0,0), x = Events$Volume_BestEstimate[Magnitude.class]/10^3
                 ,vjust=(-0.5), label = "Supply (Best. Est.)",srt=90)+
        coord_cartesian(xlim=c(0,Events$Volume_BestEstimate[Magnitude.class]/10^3))+
        labs(x="Released volume [*1000 m3]",y="count"
             ,caption=paste("Code of",Model," used on", lubridate::today(),"| Number of runs N =",N.Runs)
             ,title = paste("Distribution of released volume for event:",Event.name))
      #Save figure
      ggsave(paste0("2Outputs/Buffering/ReleasedVolume_Evt-",Event.name,"_Nrun_",N.Runs,"_ParametersAsBestEstimates.png")
             , width = 11, height = 7,units="cm")
      
      # Plot a synthesis figure on Qpeak out
      ggplot(Result.all)+
        theme_bw(base_size = 9)+
        geom_histogram(aes(Qp.out))+
        geom_boxplot(aes(x=Qp.out,y=-1))+
        geom_vline(xintercept = Events$PeakDischarge_BestEstimate[Magnitude.class])+
        annotate(geom = "text", y = 0, adj=c(0,0), x = Events$PeakDischarge_BestEstimate[Magnitude.class]
                 ,vjust=(-0.5), label = "Supply (Best. Est.)",srt=90)+
        coord_cartesian(xlim=c(0,Events$PeakDischarge_BestEstimate[Magnitude.class]))+
        labs(x="Peak discharge [m3/s]",y="count"
             ,caption=paste("Code of",Model," used on", lubridate::today(),"| Number of runs N =",N.Runs)
             ,title = paste("Distribution of released peak dischage for event:",Event.name))
      
      #Save figure
      ggsave(paste0("2Outputs/Buffering/ReleasedQpeak_Evt-",Event.name,"_Nrun_",N.Runs,"_ParametersAsBestEstimates.png")
             , width = 11, height = 7,units="cm")
      
      # Plot a synthesis figure of Qpeak out VS Vout
      ggplot(Result.all)+
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
             ,caption=paste("Code of",Model," used on", lubridate::today(),"\n Number of runs N =",N.Runs)
             # ,title = paste("Released volume and released peak dischage for event:",Event.name)
        )
      
      #Save figure
      ggsave(paste0("2Outputs/Buffering/ReleasedVolume-VS-Qpeak_Evt-",Event.name,"_Nrun_",N.Runs,"_ParametersAsBestEstimates.png")
             , width = 10, height = 7,units="cm") 
    }
  }
}
