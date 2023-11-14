### Main code calling the sub-routines depending on the user choice

#Clean environment
rm(list=ls())
library(jsonlite)

# Setup HEADLESS variables
HEADLESS <- !base::interactive()
# HEADLESS = TRUE #REMOVE !
if(HEADLESS) {
  print('Running in HEADLESS mode')
  args<-commandArgs(trailingOnly = TRUE)
  #François
  MainRep<-"/home/francois/Documents/Micro-entreprise/projets/DFBuffering/DFbuffering"
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
ModelVersion <- "CheekyeDebrisFlowBarrier V3.0"
SourceCodeRepository<-paste0(getwd(),"/0SourceCodes")
if(!HEADLESS)
{
  #Selecting the source codes repository ----
  dlg_message(message="Show me where are stored the source codes (Repository \"/0SourceCodes\")"
              , type = c("ok")) ; SourceCodeRepository<-dlg_dir(title="Show me where are stored the source codes (Repository \"/0SourceCodes\")"
                                                                ,default = getwd())$res
              
}
#Set this repository as working repository
setwd(SourceCodeRepository)

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
source("BoulderTransfer.R")#Compute the transformation of the time series from one structure to another

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
              Structures$Rank<-match(Structure_organisation$Name,Structures$Name)
              Structures$TransferDownstream<-Structure_organisation$Transfer[match(Structure_organisation$Name,Structures$Name)]
              Structures$InitialConditions<-InitialConditions[match(Structure_organisation$InitialCondition
                                                                    ,InitialConditions$Name),]
}

#Complete the storage elevation data of each bridge elements
for(Structure_Ind in (1:length(Structures$Name)))
{
  if(Structures$Type[[Structure_Ind]]=="bridge")
  {
    Structures$StorageElevation[[Structure_Ind]]<-define_bridgeStorageElevation(Structures$Opening[[Structure_Ind]]
                                                                                ,Structures$width[[Structure_Ind]]
                                                                                ,Structures$slope[[Structure_Ind]])
  }
}

#Selecting the repository where one want to record the results
if(HEADLESS) {
  MainRep <- args[2]
} else {
  setwd(InputDataRepository)
  dlg_message(message="Show me where you want to store the results (e.g., the parent directory where /0SourceCode and /1Data are stored)"
              , type = c("ok"));MainRep<-dlg_dir(title="Show me where you want to store the results (e.g., the parent directory where /0SourceCode and /1Data are stored)"
                                                        ,default = getwd())$res
}
#Set this repository as working repository
setwd(MainRep)
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
  
  
  # Choices of plot and save option----
  if(OnlyNormalRun)
  {
    ComputeWithBestEstimateBoulderNumber<-TRUE
  }else{
    ComputeWithBestEstimateBoulderNumber<-TRUE
  }
  if(!HEADLESS){
    #Saving synthesis figure for each run?
    PrintFinalPlot<-dlg_message(message="Do you want to print a synthesis plot for each run (hydrographs, flow level, volume stored) in a .png file?", type = c("yesno"))$res
    if(PrintFinalPlot=="yes"){ PrintFinalPlot<-TRUE}else{ PrintFinalPlot<-FALSE}
  }
  
  #Disabled options in this version
  PrintDataPlot<-FALSE #Plot stage - discharge Q(h) curve and stage - volume V(h) curves
  TestAbsenceBottomJam<-FALSE #remove the initial blockage level y_0 of the initial conditions
  
  #Want to save the final state of clogging to use it for the next run?
  KeepTrackOfCloggingState<-FALSE
  
  # Time step
  TimeStep<-1 #(s) Should be an integer in seconds, so no less than 1 second
  # small enough to capture the peak, the code seems to get stuck in infinite loops if set at 5 s with low slopes
  
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
    }#end of while loop to define the event
    
    #Create input data according to the EventName and adjustement option
    input<-Create_inlet_input(EventName,AdjustEventManually,Structures,Boulders)
    
    ## Computation of isolated runs ----
    for(Run_Ind in (1:N_runs))
    {
      Qoutmax<-Cascade_of_structure_functionning(input)
      
      #Record the run results
      if(Run_Ind==1){  Qoutmax_all<-Qoutmax }else{ Qoutmax_all<-rbind(Qoutmax_all,Qoutmax)  }
      
      #print message
      print(paste0("Run #",Run_Ind," finished at, ",now()," for the whole cascade of structure, still ",N_runs-Run_Ind," run to perform"))
      
    }
    
    #plots of scatterplot and histograms----
    #If more than 10 runs, plot histograms and a scatter plot of Qmax and V
    if(N_runs>=10)
    {
      # Find the event in the table
      Magnitude.class<-which(Events$Name==EventName)
     
       for(Structure_Ind in 1:length(Structures$Name))
      {
         StructureName<-Structures$Name[[which(Structures$Rank==Structure_Ind)]]
         #load results of the structure
         load(paste0("2Outputs/Result_Evt-",EventName,"_Structure_",StructureName,".RData"))
         
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
               ,caption=paste("Code of",ModelVersion," used on", lubridate::today(),"| Number of runs N =",N_runs)
               ,title = paste("Distribution of released volume for event\n Event:",EventName," & Structure:",StructureName))
        #Save figure
        ggsave(paste0("2Outputs/ReleasedVolume_Evt-",EventName,"_Nrun_",N_runs,"_Structure_",StructureName,"_ParametersAsBestEstimates.png")
               , width = 11, height = 7,units="cm")
        
        # Plot a synthesis figure on Qpeak out
        ggplot(Result_all)+
          theme_bw(base_size = 9)+
          geom_histogram(aes(Qp_out))+
          geom_boxplot(aes(x=Qp_out,y=-1))+
          geom_vline(xintercept = Events$PeakDischarge_BestEstimate[Magnitude.class])+
          annotate(geom = "text", y = 0, adj=c(0,0), x = Events$PeakDischarge_BestEstimate[Magnitude.class]
                   ,vjust=(-0.5), label = "Supply (Best. Est.)",srt=90)+
          coord_cartesian(xlim=c(0,Events$PeakDischarge_BestEstimate[Magnitude.class]))+
          labs(x="Peak discharge [m3/s]",y="count"
               ,caption=paste("Code of",ModelVersion," used on", lubridate::today(),"| Number of runs N =",N_runs)
               ,title = paste("Distribution of released peak dischage \n Event:",EventName," & Structure:",StructureName))
        
        #Save figure
        ggsave(paste0("2Outputs/ReleasedQpeak_Evt-",EventName,"_Nrun_",N_runs,"_Structure_",StructureName,"_ParametersAsBestEstimates.png")
               , width = 11, height = 7,units="cm")
        
        # Plot a synthesis figure of Qpeak out VS Vout
        ggplot(Result_all)+
          theme_bw(base_size = 9)+
          geom_bin2d(aes(x=Vout/10^3,y=Qp_out))+
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
               ,caption=paste("Code of",ModelVersion," used on", lubridate::today(),"\n Number of runs N =",N_runs)
               ,title = paste("Released volume and released peak dischage \n Event:",EventName," & Structure:",StructureName))
        
        #Save figure
        ggsave(paste0("2Outputs/ReleasedVolume-VS-Qpeak_Evt-",EventName,"_Nrun_",N_runs,"_Structure_",StructureName,"_ParametersAsBestEstimates.png")
               , width = 10, height = 7,units="cm") 
      }# en of the structure loop
      
    }# end of the if loop to plot synthesis plots
    
  }else{# end of the normal run condition
    
    # IMPERFECT DATA TO PROVIDE----
    
    ninput<-4+length(Structures$Name)*2+length(Boulders[,1]) #Number of input parameters
    input<-vector(mode="list", length=ninput) # Initialization#
    
    ############## Volume of debris flows, in Mm3
    #If min = best estimate = max, set the parameter as "fixed"
    if((Events$Volume_min[Magnitude.class]==Events$Volume_BestEstimate[Magnitude.class]
        &&
        Events$Volume_BestEstimate[Magnitude.class]==Events$Volume_max[Magnitude.class]))
    {
      input[[1]]=CREATE_INPUT(
        name="Volume [Mm3]",
        type="fixed",
        param= Events$Volume_BestEstimate[Magnitude.class]/10^6,  
        monoton = "incr"
      )
    }else{
      #Otherwise, set it as possibility distribution (triangle)
      input[[1]]=CREATE_INPUT(
        name="Volume [Mm3]",
        type="possi",
        distr="triangle",
        param=CheckTriangleDistribution(Events$Volume_min[Magnitude.class],
                                        Events$Volume_BestEstimate[Magnitude.class],
                                        Events$Volume_max[Magnitude.class],
                                        "Volume")/10^6,
        monoton = "incr"
      )
    }
    
    #Peak discharge in m3/s
    #If min = best estimate = max, set the paramter as "fixed"
    if((Events$PeakDischarge_min[Magnitude.class]==Events$PeakDischarge_BestEstimate[Magnitude.class]
        &&
        Events$PeakDischarge_BestEstimate[Magnitude.class]==Events$PeakDischarge_max[Magnitude.class]))
    {
      input[[2]]=CREATE_INPUT(
        name="Qpeak [m3/s]",
        type="fixed",
        param= Events$PeakDischarge_BestEstimate[Magnitude.class],  
        monoton = "incr"
      )
    }else{
      #Otherwise, set it as possibility distribution (triangle)
      input[[2]]=CREATE_INPUT(
        name="Qpeak [m3/s]",
        type="possi",
        distr="triangle",
        param=CheckTriangleDistribution(Events$PeakDischarge_min[Magnitude.class],
                                        Events$PeakDischarge_BestEstimate[Magnitude.class],
                                        Events$PeakDischarge_max[Magnitude.class],
                                        "Qpeak"),
        monoton = "incr"
      )  
    }
    
    #Position of the peak in the triangle: Tpeak/Ttotal
    #If min = best estimate = max, set the paramter as "fixed"
    if((Events$TimeLag_min[Magnitude.class]==Events$TimeLag_BestEstimate[Magnitude.class]
        &&
        Events$TimeLag_BestEstimate[Magnitude.class]==Events$TimeLag_max[Magnitude.class]))
    {
      input[[3]]=CREATE_INPUT(
        name="Peak lag [-]",
        type="fixed",
        param= Events$TimeLag_BestEstimate[Magnitude.class],  
        monoton = "incr"
      )
    }else{
      #Otherwise, set it as possibility distribution (triangle)
      input[[3]]=CREATE_INPUT(
        name="Peak lag [-]",
        type="possi",
        distr="triangle",
        param=CheckTriangleDistribution(Events$TimeLag_min[Magnitude.class],
                                        Events$TimeLag_BestEstimate[Magnitude.class],
                                        Events$TimeLag_max[Magnitude.class],
                                        "Peak lag"),
        monoton = "incr"
      )
    }
    
    #Deposition slope
    #If min = best estimate = max, set the paramter as "fixed"
    if((Events$DepositionSlope_min[Magnitude.class]==Events$DepositionSlope_BestEstimate[Magnitude.class]
        &&
        Events$DepositionSlope_BestEstimate[Magnitude.class]==Events$DepositionSlope_max[Magnitude.class]))
    {
      input[[4]]=CREATE_INPUT(
        name="Deposition slope [%]",
        type="fixed",
        param= Events$DepositionSlope_BestEstimate[Magnitude.class],  
        monoton = "decr"
      )
    }else{
      #Otherwise, set it as possibility distribution (triangle)
      input[[4]]=CREATE_INPUT(
        name="Deposition slope [%]",
        type="possi",
        distr="triangle",
        param=CheckTriangleDistribution(Events$DepositionSlope_min[Magnitude.class],
                                        Events$DepositionSlope_BestEstimate[Magnitude.class],
                                        Events$DepositionSlope_max[Magnitude.class],
                                        "Deposition slope"),
        monoton = "decr"
      )
    }
    
    ##Initial deposit level
    #If min = best estimate = max, set the paramter as "fixed"
    if((InitialConditions$InitialDepositHeight_min[1]==InitialConditions$InitialDepositHeight_BestEstimate[1]
        &&
        InitialConditions$InitialDepositHeight_BestEstimate[1]==InitialConditions$InitialDepositHeight_max[1]))
    {
      input[[5]]=CREATE_INPUT(
        name="Initial deposit [m]",
        type="fixed",
        param= InitialConditions$InitialDepositHeight_BestEstimate[1],
        monoton = "decr"
      )
    }else{
      #Otherwise, set it as possibility distribution (triangle)
      input[[5]]=CREATE_INPUT(
        name="Initial deposit [m]",
        ##### Triangular distri
        type="possi",
        distr="triangle",
        param=CheckTriangleDistribution(InitialConditions$InitialDepositHeight_min[1],
                                        InitialConditions$InitialDepositHeight_BestEstimate[1],
                                        InitialConditions$InitialDepositHeight_max[1],
                                        "Initial deposit"),
        monoton = "decr"
      )
    }
    
    # Height of the initial jamming in the barrier (Large wood and / or boulders)
    #If min = best estimate = max, set the paramter as "fixed"
    if((InitialConditions$InitialJammingHeight_min[1]==InitialConditions$InitialJammingHeight_BestEstimate[1]
        &&
        InitialConditions$InitialJammingHeight_BestEstimate[1]==InitialConditions$InitialJammingHeight_max[1]))
    {
      input[[6]]=CREATE_INPUT(
        name="Initial jamming height [m]",
        type="fixed",
        param= InitialConditions$InitialJammingHeight_BestEstimate[1],
        monoton = "decr"
      )
    }else{
      #Otherwise, set it as possibility distribution (triangle)
      input[[6]]=CREATE_INPUT(
        name="Initial jamming height [m]",
        ##### Triangular distri
        type="possi",
        distr="triangle",
        param=CheckTriangleDistribution(InitialConditions$InitialJammingHeight_min[1],
                                        InitialConditions$InitialJammingHeight_BestEstimate[1],
                                        InitialConditions$InitialJammingHeight_max[1],
                                        "Initial jamming height [m]"),
        monoton = "decr")
    }
    ###Boulders
    for(i in (1:length(Boulders[,1])))
    {
      #Boulder class i
      #If min = best estimate = max, set the parameter as "fixed"
      if((Boulders$BoulderNumber_min[i]==Boulders$BoulderNumber_BestEstimate[i]
          &&
          Boulders$BoulderNumber_BestEstimate[i]==Boulders$BoulderNumber_max[i]))
      {
        input[[(6+i)]]=CREATE_INPUT(
          name=paste0("#Boulders ",Boulders[i,1],"-",Boulders[i,2],"m"),
          type="fixed",
          param= Boulders$BoulderNumber_BestEstimate[i],
          monoton = "decr"
        )
      }else{
        #Otherwise, set it as possibility distribution (triangle)
        input[[(6+i)]]=CREATE_INPUT(
          name=paste0("#Boulders ",Boulders[i,1],"-",Boulders[i,2],"m"),
          ##### Triangular distri
          type="possi",
          distr="triangle",
          param=CheckTriangleDistribution(Boulders$BoulderNumber_min[i]
                                          ,Boulders$BoulderNumber_BestEstimate[i]
                                          ,Boulders$BoulderNumber_max[i]
                                          ,paste0("#Boulders ",Boulders[i,1],"-",Boulders[i,2],"m")),
          monoton = "decr")
      }
    }
    
    
    #     COMPUTATION----
    
    ####CREATION OF THE DISTRIBUTIONS ASSOCIATED TO THE PARAMETERS
    input=CREATE_DISTR(input)
    
    ####VISU INPUT
    png(paste0("2Outputs/PossibilityAnalysis_Evt-",Event.name,"_Nrun_",N.Runs,"_InputDistributions.png"), width = 22, height = 18,units="cm",res=350)
    {PLOT_INPUTnew(input)}
    dev.off()
    
    ### OPTIMZATION CHOICES
    choice_opt=NULL #no optimization needed because monotony known
    param_opt=NULL
    
    #Hybrid uncertainty propagation on released volume----
    ###HYBRID UNCERTAINTY PROPAGATION
    
    Rslt_Uncertain.Boulder.Number<-PROPAG(N=N.Runs,input
                                          ,CheekyeBufferingModel_UncertainBoulderNumber_Vtot
                                          ,choice_opt,param_opt,mode="IRS")
    Rslt_Uncertain.Boulder.Number<-data.frame(P=seq(0,1,length.out = N.Runs)
                                              ,Min=sort(Rslt_Uncertain.Boulder.Number[1,])/10^3
                                              ,Max=sort(Rslt_Uncertain.Boulder.Number[2,])/10^3)
    #       
    ###################Plot Pbox----
    #       
    ggplot()+theme_bw(base_size = 9)+
      geom_vline(aes(xintercept = 1))+
      geom_vline(aes(xintercept = 0))+
      geom_hline(aes(yintercept=Events$Volume_min[Magnitude.class]/10^3),lty=2)+
      geom_hline(aes(yintercept=Events$Volume_BestEstimate[Magnitude.class]/10^3))+
      geom_hline(aes(yintercept=Events$Volume_max[Magnitude.class]/10^3),lty=2)+
      annotate(geom = "text", x = 0.5, y = Events$Volume_BestEstimate[Magnitude.class]/10^3
               ,vjust=(-0.5), label = "Supply (Best. Est.)",srt=90)+
      geom_ribbon(data=Rslt_Uncertain.Boulder.Number,aes(x =P,ymin=Min,ymax=Max),alpha=0.3,lwd=1)+
      geom_line(data=Rslt_Uncertain.Boulder.Number,aes(y =Min ,x=P,colour="1"),lwd=1)+
      geom_line(data=Rslt_Uncertain.Boulder.Number,aes(y =Max ,x=P,colour="2"),lwd=1)+
      scale_colour_manual(name="Bounding Cumulated Distribution Functions (CDF)"
                          ,values=c("lightblue","darkblue")
                          ,labels=c("Lower bound","Upper bound"))+
      coord_flip()+ #To have Probability as Y
      theme(legend.position = "top")+
      labs( y = "Released volume [1000m3]",x = "Cumulative distribution function"
            ,caption=paste("Code of",Model," used on", lubridate::today(),"| Number of runs N =",N.Runs)
            ,title = paste("Uncertainty analysis of released volume for event:",Event.name))
    #Save figure
    ggsave(paste0("2Outputs/Pbox/ReleasedVolume_EvtClass",Magnitude.class,"_Nrun_",N.Runs,"_NboulderUncertain.png")
           , width = 16.5, height = 7,units="cm")
    
    #Save results
    save(Rslt_Uncertain.Boulder.Number,file=paste0("2Outputs/Rdata/ReleasedVolume_Evt-",Event.name,"_Nrun_",N.Runs,"_NboulderUncertain.RData"))
  }#end of the uncertainty propagation condition
  
  
  ## Define if another run is to be launched
  if(HEADLESS){
    PerformAnotherSimulation = "no"
  } else {
    #Want to perform another run
    PerformAnotherSimulation<-dlg_message(message="The computation is finished! \n Do you want to perform another set?", type = c("yesno"))$res
  }
}
