### Main code calling the sub-routines depending on the user choice

#Clean environment
rm(list=ls())
library(jsonlite)

# Setup HEADLESS variables
HEADLESS <- !base::interactive()
if(HEADLESS) {
  args<-commandArgs(trailingOnly = TRUE)
}

# For Guillaume P. only, to emulate the headless mode under RStudio:
# HEADLESS = TRUE
# rootDir<-"D:/Private/05_PROJETS/2023_DFbuffering/4Simu/DFbuffering"
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

#Model version
ModelVersion <- "ACB³ V1.0"

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
}

#Selecting the repository where one want to record the results
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

#Main loop within which each set of run is performed
PerformAnotherSimulation <- "yes"
while(PerformAnotherSimulation == "yes")
{
  
  if(!HEADLESS){
    #Select the type of approach----
    OnlyNormalRun<-dlg_message(message="Press \"Yes\" to perform normal runs (using best estimates of the input data) \n Or press \"No\" to run a full uncertainty propagation analysis "
                               , type = c("yesno"))$res
    
    # OnlyNormalRun<-"yes"
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
    ComputeWithBestEstimateNumber<-TRUE
  }else{
    ComputeWithBestEstimateNumber<-FALSE
  }
  
  if(OnlyNormalRun)
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
      if(OnlyNormalRun){ # Possible to reuse predefined values or to define the event manually
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
  if(OnlyNormalRun)
  {
    #If only normal runs, we only use the best estimates
    BoulderGenerationMode<-"Best estimate numbers"
    

    #Create input data according to the EventName and adjustement option
    input<-Create_inlet_input(EventName,AdjustEventManually,Structures,Boulders)
    
    ## Computation of isolated runs ----
    for(Run_Ind in (1:N_runs))
    {
      Qoutmax<-Cascade_of_structure_functionning(input)
      
      #Record the run results
      if(Run_Ind==1){  Qoutmax_all<-Qoutmax }else{ Qoutmax_all<-rbind(Qoutmax_all,Qoutmax)  }
      
      #print message
      # print(paste0("Run #",Run_Ind," finished at ",now()," for the whole cascade of structure, still ",N_runs-Run_Ind," run to perform"))
      print(paste0("PROGRESS[",Run_Ind,"/",N_runs,"]"))
      
    }

  }else{# end of the normal run condition
    
    BoulderGenerationMode<-"Uncertain boulder numbers"
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
    
    
    #     COMPUTATION----
    
    ####CREATION OF THE DISTRIBUTIONS ASSOCIATED TO THE PARAMETERS
    input=CREATE_DISTR(input)
    
    ####VISU INPUT
    png(paste0(MainRep,"/PossibilityAnalysis_Evt-",EventName,"_Nrun_",N_runs,"_InputDistributions.png"), width = 22, height = 24
        ,units="cm"
        ,res=350
        )
    {PLOT_INPUTnew(input)}
    dev.off()

    # ### OPTIMZATION CHOICES
    choice_opt=NULL #no optimization needed because monotony known
    param_opt=NULL

    #Hybrid uncertainty propagation on released volume----
    ###HYBRID UNCERTAINTY PROPAGATION
    Rslt_Uncertain.Boulder.Number<-PROPAG(N=N_runs,input
                                          ,Cascade_of_structure_functionning
                                          ,choice_opt,param_opt,mode="IRS")
    Rslt_Uncertain.Boulder.Number<-data.frame(P=seq(0,1,length.out = N_runs)
                                              ,Min=sort(Rslt_Uncertain.Boulder.Number[1,])
                                              ,Max=sort(Rslt_Uncertain.Boulder.Number[2,]))
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
      geom_ribbon(data=Rslt_Uncertain.Boulder.Number,aes(x =P,ymin=Min,ymax=Max),alpha=0.3,lwd=1)+
      geom_line(data=Rslt_Uncertain.Boulder.Number,aes(y =Min ,x=P,colour="1"),lwd=1)+
      geom_line(data=Rslt_Uncertain.Boulder.Number,aes(y =Max ,x=P,colour="2"),lwd=1)+
      scale_colour_manual(name="Bounding Cumulated Distribution Functions (CDF)"
                          ,values=c("lightblue","darkblue")
                          ,labels=c("Lower bound","Upper bound"))+
      coord_flip()+ #To have Probability as Y
      theme(legend.position = "top")+
      labs( y = "Peak discharge [m3/s]",x = "Cumulative distribution function"
            ,caption=paste("Model:",ModelVersion," used on"
                           , lubridate::today(),"| Number of runs N =",N_runs)
            ,title = paste("Uncertainty analysis of peak discharge downstream all structures (event:",EventName,")"))
    #Save figure
    ggsave(paste0("ReleasedPeakDischarge_Evt-",EventName,"_Nrun_",N_runs,"_NboulderUncertain.png")
           , width = 16.5, height = 7,units="cm")
    
    #Save results
    save(Rslt_Uncertain.Boulder.Number,file=paste0("ReleasedPeakDischarge_Evt-",EventName,"_Nrun_",N_runs,"_NboulderUncertain.RData"))
  }#end of the uncertainty propagation condition
  
  #Rename file names
  ListFile1<-list.files(pattern="computedOn")#Figure
  ListFile2<-list.files(pattern="ComputedOn")#Rdata
  if(OnlyNormalRun)
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
  rm(ListFile1,ListFile2)
  
  
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
      Qo$Run<-File_Ind
      Result$Run<-File_Ind
      if(File_Ind == 1)
      {
        Qo_all <-Qo 
        Result_all<-Result
        
      }else{
        Qo_all <-rbind(Qo_all,Qo) 
        Result_all<-rbind(Result_all,Result)
      }
    }
    #Save the file that aggregate all the results
    save(Qo_all,Result_all,file=paste0("RdataResult_Evt-",EventName,"_Structure_@-",StructureName,".RData"))
    # Remove files for each singular run
    file.remove(ListFileStructure)
    
    #plots of synthesis multi run figures----
    if(PrintFinalPlot==FALSE)
    {
      ### Four panel graph
      # Plot a synthesis figure on Vin
      TopTopLeftPanel<-ggplot(Result_all)+
        theme_classic(base_size = 9)+
        geom_histogram(aes(Vevent/10^3))+
        geom_boxplot(aes(x=Vevent/10^3,y=-1))+
        geom_vline(xintercept = Events$Volume_BestEstimate[Event_Ind]/10^3,col="grey")+
        annotate(geom = "text", y = 0, adj=0, x = Events$Volume_BestEstimate[Event_Ind]/10^3
                 ,vjust=(1.2), label = "Total event \n (Best. Est.)",srt=90,col="grey",size=3.5)+
        coord_cartesian(xlim=c(0,Events$Volume_BestEstimate[Event_Ind]/10^3*1.2))+
        labs(x="Supplied volume [*1000 m3]",y="# of Run"
             ,title = paste0("Debris flow volume and peak discharge \n","(Event: ",EventName,") \n","(Structure: ",StructureName,")"))
      
      # Plot a synthesis figure on Vout
      TopLeftPanel<-ggplot(Result_all)+
        theme_classic(base_size = 9)+
        geom_histogram(aes(Vout/10^3))+
        geom_boxplot(aes(x=Vout/10^3,y=-1))+
        coord_cartesian(xlim=c(0,Events$Volume_BestEstimate[Event_Ind]/10^3*1.25))+
        labs(x="Released volume [*1000 m3]",y="# of Run")
      
      # Plot a synthesis figure of Qpeak out VS Vout, top right panel
      TopightPanel<-ggplot(Result_all)+
        theme_bw(base_size = 9)+
        geom_bin2d(aes(x=(Vevent-Vout)/Vevent,y=(Qp_in-Qp_out)/Qp_in),col=1,#bins=20)+
                   binwidth=c(0.1,0.1))+
        scale_fill_gradient2("# of Run",low="palegreen1", high = "palegreen4")+ 
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
        theme_bw(base_size = 9)+
        geom_bin2d(aes(x=Vout/10^3,y=Qp_out),col=1,#bins=20)+
                   binwidth=c(Events$Volume_BestEstimate[Event_Ind]/10^3/10,Events$PeakDischarge_BestEstimate[Event_Ind]/10))+
        scale_fill_gradient2("# of Run",low="dodgerblue1", high = "dodgerblue4")+ 
        coord_cartesian(ylim=c(0,Events$PeakDischarge_BestEstimate[Event_Ind]*1.25)
                        ,xlim=c(0,Events$Volume_BestEstimate[Event_Ind]/10^3*1.25))+
        labs(x="Released volume [*1000 m3]",y="Released Peak discharge [m3/s]")+
        theme(legend.direction = "horizontal",legend.position = "bottom",legend.key.height = unit(0.3, 'cm'))
      
      # Plot a synthesis figure on Qpeak out
      BottomRightPanel<-ggplot(Result_all)+
        theme_classic(base_size = 9)+
        geom_histogram(aes(y=Qp_out))+
        geom_boxplot(aes(y=Qp_out,x=-1))+
        coord_cartesian(ylim=c(0,Events$PeakDischarge_BestEstimate[Event_Ind])*1.25)+
        labs(y="Released Peak discharge [m3/s]",x="# of Run")
      
      # Plot a synthesis figure on Qpeak out
      BottomRightRightPanel<-ggplot(Result_all)+
        theme_classic(base_size = 9)+
        geom_histogram(aes(y=Qp_in))+
        geom_boxplot(aes(y=Qp_in,x=-1))+
        geom_hline(yintercept = Events$PeakDischarge_BestEstimate[Event_Ind],col="grey")+
        annotate(geom = "text", x = 0, adj=0, y = Events$PeakDischarge_BestEstimate[Event_Ind]
                 ,vjust=(-0.2), label = "Total Event \n (Best. Est.)",srt=0,col="grey",size=3.5)+
        coord_cartesian(ylim=c(0,Events$PeakDischarge_BestEstimate[Event_Ind])*1.25)+
        labs(y="Supplied Peak discharge [m3/s]",x="# of Run")
      
      if(OnlyNormalRun)
      {
        Caption_text <-  paste("Code version:",ModelVersion,"\n",
                               "Parameters:"," Best Estimate values","\n"
                               ,"Number of runs =",N_runs)
      }else{
        Caption_text <-  paste("Code version:",ModelVersion,"\n",
                               "Parameter:"," uncertain values","\n"
                               ,"Number of runs =",N_runs)
      }
      
      #Save figure
      png( paste0("FourPanelGraphReleasedVolume_Evt-",EventName,"_Structure_n",Structure_Ind,"-",StructureName,".png"), width = 17, height = 15,units="cm",res=350)
      {
        pushViewport(viewport(layout = grid.layout(10,12)))
        # Define region in the plot
        define_region <- function(row, col){viewport(layout.pos.row = row, layout.pos.col = col)}
        # Arrange panels
        print(TopightPanel, vp = define_region(1:5,7:12))
        print(TopTopLeftPanel+theme(plot.margin = margin(t=0.1,r=0.1,b=0.1,l=0.4, "cm"))
              , vp = define_region(1:3,1:6))
        print(TopLeftPanel+theme(plot.margin = margin(t=0.1,r=0.1,b=0.1,l=0.4, "cm"))
              , vp = define_region(4:5,1:6))
        print(BottomLeftPanel
              , vp = define_region(6:10,1:6))
        print(BottomRightPanel+theme(plot.margin = margin(t=0.15,r=0.5,b=1.5,l=0.1, "cm"))
              , vp = define_region(6:10,7:9))
        print(BottomRightRightPanel+theme(plot.margin = margin(t=0.15,r=0.5,b=0.4,l=0.1, "cm"))+
                labs(caption=Caption_text)+   theme(plot.caption =  element_text(size=8.5))        
              , vp = define_region(6:10,10:12))
      }
      dev.off()
      
      #Multi-run time series
      {
       QplotIn<-ggplot(Qo_all,aes(x=Time/3600))+theme_bw(base_size = 9)+
          geom_line(aes(y=Qi,group =Run),color="black",alpha=0.3)+
          theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank())+
          labs(y = "Supplied Discharge\n [m3/s]")+
          theme(legend.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"))
        
        QplotOut<-ggplot(Qo_all,aes(x=Time/3600))+theme_bw(base_size = 9)+
          geom_line(aes(y=Qo,group =Run),alpha=0.3)+
          theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank())+
          labs(y = "Outlet Discharge\n [m3/s]")+
          theme(legend.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"))
        
        
        
        Zplot<-ggplot(Qo_all)+theme_bw(base_size = 9)+
          geom_line(aes(x=Time/3600,y=Z,lty="4",group=Run),alpha=0.3)+
          geom_line(aes(x=Time/3600,y=BaseLevelJam,lty="5",group=Run),alpha=0.3)+
          # scale_colour_grey(name="Level",label=c("Flow","Basal boulder jam"))+
          scale_linetype_manual(name="Level",label=c("Flow","Basal boulder jam"),values=c(1,4))+
          theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank())+
          theme(legend.position = c(0.82,0.83),legend.direction = "horizontal"
              ,legend.background = element_rect(colour =1),legend.box.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"))+
          labs( x = "Time [h]",y = "Flow level\n [m]")
          
        Vplot<-ggplot(Qo_all)+theme_bw(base_size = 9)+
          geom_line(aes(x=Time/3600,y=V,group=Run),col="black",alpha=0.3)+
          labs( x = "Time [h]",y = "Volume\n [*1000m3]")
        
        png(paste0("FourPanelGraphReleasedVolume_Evt-",EventName,"_Structure_n",Structure_Ind,"-",StructureName,".png"), width = 17, height = 15,units="cm",res=350)
        {
          # grid.arrange(Qplot1,Wplot1,Zplot,Vplot,nrow = 4)
          pushViewport(viewport(layout = grid.layout(22,1) ) )
          # Une fonction pour definir une region dans la mise en page
          define_region <- function(row, col){viewport(layout.pos.row = row, layout.pos.col = col)}
          # Arrange graphs
            print(QplotIn      , vp = define_region(1:5,1))
            print(QplotOut     , vp = define_region(6:11,1))
            print(Zplot        , vp = define_region(11:15,1))
            print(Vplot  +  labs(caption=Caption_text)+   theme(plot.caption =  element_text(size=8.5))        
                  , vp = define_region(16:22,1))
          
        }
        dev.off()
      }
      
      #Boulder inventory plot
      # 
      # ggplot(Qo_all,aes(x=Time/3600))+theme_bw(base_size = 9)+
      #   # geom_col(aes(y=-Class3.unjammed))+
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
  }# end of the structure loop
  
  ## Define if another run is to be launched
  if(HEADLESS){
    PerformAnotherSimulation = "no"
  } else {
    #Want to perform another run
    PerformAnotherSimulation<-dlg_message(message="The computation is finished! \n Do you want to perform another set?", type = c("yesno"))$res
  }
}

