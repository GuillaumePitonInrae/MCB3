### Main code calling the sub-routines depending on the user choice
# Version 0.1 - G. PITON, Apri. 2022

#Clean environment
rm(list=ls())

# Load package
library(lubridate) #To add date on plot
library(stringr) #To replace string
library(HYRISK)#Package uncertainty propagation
library(svDialogs) #Package for popup dialog windows
library(ggplot2) #Plots
#start the clock for computation time recording
# ptm <- proc.time()

#Model version
Model <- "CheekyeDebrisFlowBarrier V2.0"


#Selecting the repository where the source codes are stored----
dlg_message(message="Show me where are stored the source codes (Repository \"/0SourceCodes\")"
            , type = c("ok"));SourceCodeRepository<-dlg_dir(title="Show me where are stored the source codes (Repository \"/0SourceCodes\")"
                                                            ,default = getwd())$res

#Set this repository as working repository
setwd(SourceCodeRepository)

#Load the source codes
source("PLOT_INPUTnew.R")#To plot input distrib
source("CheckTriangleDistribution.R") #To check input data distributions
source("DischargeCapacityFunctions_V0.R")#Load slit capacity function
source("BoulderPassing_V1.R")#Compute the number of boulders passing through an orifice
source("Plot_BufferingModelResults_V0.R") #For plotting results of singular runs
source("BarrierBuffering_V0.3.R")#Actual buffering model and general functions calling it for error propagation


#Selecting the repository where the source codes are stored
dlg_message(message="Show me where are stored the input data (Repository \"/1Data\")"
            , type = c("ok"));InputDataRepository<-dlg_dir(title="Show me where are stored the input data (Repository \"/1Data\")"
                                                           ,default = getwd())$res
#Load boulder list
Boulders<-read.csv(paste0(InputDataRepository,"/RangeOfBoulders.txt"),sep="\t")
#Load event features
Events<-read.csv(paste0(InputDataRepository,"/Events.txt"),sep="\t")
#Load initial conditions
InitialConditions<-read.csv(paste0(InputDataRepository,"/InitialConditions.txt"),sep="\t")
#Load storage - elevation curve
StorageElevation<-read.csv(paste0(InputDataRepository,"/ElevationStorageCurves.txt"),sep="\t")
#Load the barrier definition (openings)
Opening<-read.csv(paste0(InputDataRepository,"/Opening.txt"),header = T)

#Main loop within which each set of run is performed
Perform.Another.Simulation<-"yes"
while(Perform.Another.Simulation=="yes")
{
  #Select the type of approach----
  OnlyNormalRun<-dlg_message(message="Press \"Yes\" to perform normal runs (using best estimates of the input data) \n Or press \"No\" to run a full uncertainty propagation analysis "
                             , type = c("yesno"))$res
  
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
  Print.Data.Plot<-FALSE
  Test.Absence.Bottom.Jam<-FALSE
  Kill.boulder.transport.after.input.surge<-FALSE
  #Want to save the final state of clogging to use it for the next run?
  Keep.track.of.clogging.state<-FALSE
  
  
  
  #Duration of the simulation = input hydrograph duration * FactorOfDuration
  FactorOfDuration<-5
  
  # Define the event to model----
  Event.undefined<-TRUE
  while(Event.undefined)
  {
    if(OnlyNormalRun=="yes"){ # Possible to reuse predefined values or to define the event manually
      Event.name<-dlg_input(message = c("Write the name of the event you want to model, the available names are :"
                                        ,Events$Name
                                        ,"If you want to define the event manually, write \"0\" ")
                            ,default = Events$Name[1])$res
      
    }else{#Only possible to reuse predefined values 
      Event.name<-dlg_input(message = c("Write the name of the event you want to model, the available names are :"
                                        ,Events$Name)
                            ,default = Events$Name[1])$res
    } 
    
    if(Event.name == "0"){#Define values manually
      Adjust.event.manually <- TRUE
      Event.name<-dlg_input(message = c("OK we will adjust an event, which one do you want to take as base?, the available names are :"
                                        ,Events$Name)
                            ,default = Events$Name[1])$res
    }else{Adjust.event.manually<-FALSE}
    
    # Find the event in the table
    Magnitude.class<-which(Events$Name==Event.name)
    
    if(Event.name %in% Events$Name){Event.undefined<-FALSE}else{
      dlg_message(message="The name you wrote is not in the list of the available events, please provide an available event name"
                  , type = c("ok"))
    }
  }
  
  # Launch runs----
  if(OnlyNormalRun=="yes")
  {
    #If only normal runs, we only use the best estimates
    Boulder.Generation.Mode<-"Best estimate numbers"
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
    #Pop up windows to define the values
    if(Adjust.event.manually){
      for(j in c(1:length(input)))
      {
        input[j]<-as.numeric(dlg_input(message = paste("Please provide the value of parameter \n",input.data.name[j])
                                       , default = input[j])$res)
      }
    }
    
    
    # Duration = V/Q*2
    Duration<-round(input[1]*10^3/(input[2]/2),0)
    #Pass duration in second rather than dimensionless
    input[3]<-input[3]*Duration
    
    for(Run.ind in (1:N.Runs))
    {
      #launch computation
      Result<-BarrierBufferingUncertainSlope_V0.3(input,Boulder.Generation.Mode)
      print(paste0("Run #",Run.ind," finished at, ",now(),", still ",N.Runs-Run.ind," to perform"))
      #Record the run results
      if(Run.ind==1){Result.all<-Result}else{Result.all<-rbind(Result.all,Result)}
    }
    #Save a data frame with the main results of all runs as a .Rdata file
    save(Result.all,file=paste0("2Outputs/Rdata/Result_Evt-",Event.name,"_Run_",Run.ind,".RData"))
    
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
           , width = 16.5, height = 7,units="cm")
    
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
           , width = 16.5, height = 7,units="cm")
    
    # Plot a synthesis figure of Qpeak out VS Vout
    ggplot(Result.all)+
      theme_bw(base_size = 9)+
      geom_point(aes(x=Vout/10^3,y=Qp.out,col=Vfinal/10^3))+
      scale_color_continuous("Trapped volume\n [*1000 m3]")+
      geom_hline(yintercept = Events$PeakDischarge_BestEstimate[Magnitude.class])+
      geom_vline(xintercept = Events$Volume_BestEstimate[Magnitude.class]/10^3)+
      annotate(geom = "text", y = 0, adj=c(0,0), x = Events$Volume_BestEstimate[Magnitude.class]/10^3
               ,vjust=(-0.5), label = "Supply (Best. Est.)",srt=90)+
      annotate(geom = "text", x = 0, adj=c(0,0), y = Events$PeakDischarge_BestEstimate[Magnitude.class]
               ,vjust=(1.2), label = "Supply (Best. Est.)")+
      coord_cartesian(ylim=c(0,Events$PeakDischarge_BestEstimate[Magnitude.class])
                      ,xlim=c(0,Events$Volume_BestEstimate[Magnitude.class]/10^3))+
      labs(x="Released volume [*1000 m3]",y="Peak discharge [m3/s]"
           ,caption=paste("Code of",Model," used on", lubridate::today(),"| Number of runs N =",N.Runs)
           ,title = paste("Released volume and released peak dischage for event:",Event.name))
    #Save figure
    ggsave(paste0("2Outputs/Buffering/ReleasedVolume-VS-Qpeak_Evt-",Event.name,"_Nrun_",N.Runs,"_ParametersAsBestEstimates.png")
           , width = 16.5, height = 7,units="cm")
    
  }else{
    
    
    # IMPERFECT DATA TO PROVIDE----
    
    ninput<-6+length(Boulders[,1]) #Number of input parameters
    input<-vector(mode="list", length=ninput) # Initialisation#
    
    ############## Volume of debris flows, in Mm3
    #If min = best estimate = max, set the paramter as "fixed"
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
      #If min = best estimate = max, set the paramter as "fixed"
      if((Boulders$Lower_bound[i]==Boulders$Best_estimate[i]
          &&
          Boulders$Best_estimate[i]==Boulders$Upper_bound[i]))
      {
        input[[(6+i)]]=CREATE_INPUT(
          name=paste0("#Boulders ",Boulders[i,1],"m"),
          type="fixed",
          param= Boulders$Best_estimate[i],
          monoton = "decr"
        )
      }else{
        #Otherwise, set it as possibility distribution (triangle)
        input[[(6+i)]]=CREATE_INPUT(
          name=paste0("#Boulders ",Boulders[i,1],"m"),
          ##### Triangular distri
          type="possi",
          distr="triangle",
          param=CheckTriangleDistribution(Boulders$Lower_bound[i]
                                          ,Boulders$Best_estimate[i]
                                          ,Boulders$Upper_bound[i]
                                          ,paste0("#Boulders ",Boulders[i,1],"m")),
          monoton = "decr")
      }
    }
    
    
    #     COMPUTATION----
    
    ####CREATION OF THE DISTRIBUTIONS ASSOCIATED TO THE PARAMETERS
    input=CREATE_DISTR(input)
    
    ####VISU INPUT
    png(paste0("2Outputs/Pbox/ReleasedVolume_Evt-",Event.name,"_Nrun_",N.Runs,"_InputDistributions.png"), width = 22, height = 18,units="cm",res=350)
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
  }
  
  #Want to perform another run
  Perform.Another.Simulation<-dlg_message(message="The computation is finished! \n Do you want to perform another set?"
                                          , type = c("yesno"))$res
  
}
