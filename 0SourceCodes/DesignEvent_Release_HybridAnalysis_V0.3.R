### Uncertainty propagation by hybrid method on event of classes 1 to 7
# The code basically call the algorithm used in FCheekyeBuffering_V0.R 
# a large number of time with uncertain parameters and provide p-box of 
# the variable of interest
# G. PITON, May 2021

#Clean environment
rm(list=ls())
#Working repository
MainRep<-"C:/INRAE/Private/05_PROJETS/2018_CheekyeDam/5Analysis/05BufferingModel_V2"
setwd(MainRep)

# Load package
library(lubridate) #To add date on plot
library(stringr) #To replace string
library(HYRISK)#Package uncertainty propagation
library(svDialogs) #Package for popup dialog windows
library(ggplot2) #Plots
library(gridExtra) #For multi ggplots
library(grid) #For multi ggplots
#start the clock for computation time recording
ptm <- proc.time()

source("0SourceCodes/PLOT_INPUTnew.R")#To plot input distrib
source("0SourceCodes/DischargeCapacityFunctions_V0.R")#Load slit capacity function
source("0SourceCodes/BoulderPassing_V1.R")#Compute the number of boulders passing through an orifice
source("0SourceCodes/Plot_BufferingModelResults_V0.R") #For plotting results of singular runs
# source("0SourceCodes/BarrierBuffering_V0.2.R")#Actual buffering model with boulder probability as best estimate
source("0SourceCodes/BarrierBuffering_V0.3.R")#Actual buffering model with boulder probability as best estimate
source("0SourceCodes/CheekyeBuffering_V1.R")#Adaptation for the Cheekye case with large wood clogging the base level

# Choices
Compute.With.Best.Estimate.Boulder.Number<-FALSE#TRUE#
Compute.With.Uncertain.Boulder.Number<-TRUE#FALSE#
Test.Absence.Bottom.Jam<-FALSE#TRUE#
Save.boulder.size<-TRUE#FALSE#
Save.hydrographs<-FALSE#TRUE#
Kill.boulder.transport.after.input.surge<-FALSE#TRUE

#Want to print the plots?
Print.Data.Plot<-FALSE#TRUE#
Print.Final.Plot<-FALSE#TRUE#

#Want to save the final state of clogging to use it for the next run?
Keep.track.of.clogging.state<-FALSE


# Number of runs?
#100 takes about 25 minutes for high discharges
N.random.Runs<-100

#Duration of the simulation = input hydrograph duration * FactorOfDuration
# FactorOfDuration<-3 # Sufficient for design event
# FactorOfDuration<-50 # Necessary for safety check events
FactorOfDuration<-6


SubRep<-data.frame(Rep=c("/15_Width6mNoBar_LWjams2-4-6"
                         ,"/16_Width6mNoBar_LWjams5-10-15"
                         ,"/13_Width6mFinalDesign_LWjam2-4-6"
                         ,"/14_Width6mFinalDesign_LWjam5-10-15"
                          )
                   ,Name=c("No bar - 2|4|6","No bar - 5|10|15"
                           ,"With bar - 2|4|6","With bar - 5|10|15")
                  ,LWjamheight=c("Low","High","Low","High")
                  )
Rep.Ind<-1
for(Rep.Ind in c(1:length(SubRep$Rep)))
    {
      setwd(paste0(MainRep,SubRep$Rep[Rep.Ind])) 
      

#Load boulder list
Boulders<-read.csv("./1Data/RangeOfBoulders.txt",sep="\t")
Boulder.Generation.Mode<-"Best estimate numbers"
#Load frequency magnitude data
FreqMag<-read.csv("1Data/FreqMag.txt",sep="\t")
# Ask which class to work on
Magnitude.class<-as.numeric(dlg_input(message = "Enter the class of event to model: 1, 2, 3, 4, 5, 6 or 7", default = "6", gui = .GUI)$res)
# for(Magnitude.class in c(7,2,1,4,5))#6,3))#
for(Magnitude.class in c(6,3))#
{
  Vevent<-FreqMag$Volume[Magnitude.class]*10^6
  # 
# Vevent<-2.8*10^6
# Vevent<-FreqMag$Volume[Magnitude.class]*10^6
# 
input<-c(Vevent/10^6  #Volume * 1000 m3
         ,15000#0.0188*Vevent^0.79 #Qpeak m3/s,          # formula for muddy debris flow according to Miyuzama et al. 1992         # INTERPRAEVENT http://www.interpraevent.at/palm-cms/upload_files/Publikationen/Tagungsbeitraege/1992_4_99.pdf
         ,0.2 #(s) Peak lag
         ,4.7#Deposition slope (%)
         ,0 #Initial deposit height (m)
         ,0  #Jam at the slit base by large wood (m)
         ,3000#     0.5-1
         ,400#       1-1.5
         ,150 #       1.5-2
         ,50 #         2-3
         ,1 #         3-4
         ,0.5 #Boulders 4-5
          )
#   test<-CheekyeBufferingModel_UncertainBoulderNumber_Vtot(input)
#   test<-CheekyeBufferingModel_Vtot(input)
  # proc.time() - ptm
  ################################ 
  #                                    IMPERFECT DATA TO PROVIDE----
  ################################ 
  ninput<-6+length(Boulders[,1]) #Number of input parameters
  input<-vector(mode="list", length=ninput) # Initialisation#
  
  ############## Volume of debris flows, in Mm3
  input[[1]]=CREATE_INPUT(
    name="Volume [Mm3]",
    type="fixed",
    param=Vevent/10^6,
    monoton = "incr"
  )
  
  #Peak discharge in m3/s
  input[[2]]=CREATE_INPUT(
    name="Qpeak [m3/s]",
    type="fixed",
    # type="possi",
    # distr="triangle",
    param=FreqMag$Discharge[Magnitude.class],
    monoton = "incr"
  )
  
  #Position of the peak in the triangle: Tpeak/Ttotal
  input[[3]]=CREATE_INPUT(
    name="Peak lag [-]",
    type="possi",
    distr= "trapeze",
    param=c(0.05,0.1,0.3,0.5),
    monoton = "incr"
  )
  
  #Deposition slope
  input[[4]]=CREATE_INPUT(
    name="Deposition slope [%]",
    ###Fixed value
    type="fixed",
    param=c(4.7)
    ##### Triangular distri
    # type="possi",
    # distr="triangle",
    # param=c(4.7*0.9,4.7,4.7*1.1),
    # monoton = "decr"
  )
  
  ##Initial deposit level
  input[[5]]=CREATE_INPUT(
    name="Initial deposit [m]",
    ##### Triangular distri
    type="possi",
    distr="triangle",
    param=c(0,1,2),
    monoton = "decr"
  )

  # Height of the large wood jam  
  if(SubRep$LWjamheight[Rep.Ind]=="Low"){ LWjamheight<-c(2,4,6)}else{LWjamheight<-c(5,10,15)}
  
    input[[6]]=CREATE_INPUT(
      name="Log jam height [m]",
      ##### Triangular distri
      type="possi",
      distr="triangle",
      param=LWjamheight,
      # param=c(5,10,15),
      # param=c(2,4,6),
      monoton = "decr")
  
  ###Boulders
  for(i in (1:length(Boulders[,1])))
  {
    input[[(6+i)]]=CREATE_INPUT(
      name=paste0("#Boulders ",Boulders[i,1],"m"),
      ##### Triangular distri
      type="possi",
      distr="triangle",
      param=c(Boulders$Lower_bound[i],Boulders$Best_estimate[i],Boulders$Upper_bound[i]),
      monoton = "decr")
  }
  
  ################################ 
  #                                    COMPUTATION----
  ################################ 
  ####CREATION OF THE DISTRIBUTIONS ASSOCIATED TO THE PARAMETERS
  input=CREATE_DISTR(input)
  
  ####VISU INPUT
  png("2Outputs/HUA_DesignEvent_Input.png", width = 22, height = 12*1.5,units="cm",res=350)
  {PLOT_INPUTnew(input)}
  dev.off()
  
    ### OPTIMZATION CHOICES
  choice_opt=NULL #no optimization needed because monotony known
  param_opt=NULL
  
  #Hybrid uncertainty propagation on released volume----
  {
    ###HYBRID UNCERTAINTY PROPAGATION
    if(Compute.With.Best.Estimate.Boulder.Number){
      Rslt_Best.Estimate.Boulder.Number<-PROPAG(N=N.random.Runs,input
                                                ,CheekyeBufferingModel_Vtot
                                                ,choice_opt,param_opt,mode="IRS")
      Rslt_Best.Estimate.Boulder.Number<-data.frame(P=seq(0,1,length.out = N.random.Runs)
                                                    ,Min=sort(Rslt_Best.Estimate.Boulder.Number[1,])/10^3
                                                    ,Max=sort(Rslt_Best.Estimate.Boulder.Number[2,])/10^3
                                                    ,Hyp=rep("Normal run",N.random.Runs))
      if(Test.Absence.Bottom.Jam){
        Rslt_Best.Estimate.Number.No.Bottom.Jam<-PINCHING_fun(
          which=6,## input variable 
          value=0, ##pinched at this scalar value
          N.random.Runs,input,
          CheekyeBufferingModel_Vtot,choice_opt,param_opt,mode="IRS")
        
        Rslt_Best.Estimate.Boulder.Number<-rbind(Rslt_Best.Estimate.Boulder.Number,
                                             data.frame(P=seq(0,1,length.out = N.random.Runs)
                                                        ,Min=sort(Rslt_Best.Estimate.Number.No.Bottom.Jam[1,])/10^3
                                                        ,Max=sort(Rslt_Best.Estimate.Number.No.Bottom.Jam[2,])/10^3
                                                        ,Hyp=rep("No bottom jam",N.random.Runs)))
      }
    }
    
    if(Compute.With.Uncertain.Boulder.Number){
      Rslt_Uncertain.Boulder.Number<-PROPAG(N=N.random.Runs,input
                                             ,CheekyeBufferingModel_UncertainBoulderNumber_Vtot
                                             ,choice_opt,param_opt,mode="IRS")
      Rslt_Uncertain.Boulder.Number<-data.frame(P=seq(0,1,length.out = N.random.Runs)
                                                ,Min=sort(Rslt_Uncertain.Boulder.Number[1,])/10^3
                                                ,Max=sort(Rslt_Uncertain.Boulder.Number[2,])/10^3
                                                ,Hyp=rep("Normal run",N.random.Runs))
      
      if(Test.Absence.Bottom.Jam){
        Rslt_Uncertain.Boulder.Number.No.Bottom.Jam<-PINCHING_fun(
          which=6,## input variable 
          value=0, ##pinched at this scalar value
          N.random.Runs,input,
          CheekyeBufferingModel_UncertainBoulderNumber_Vtot,choice_opt,param_opt,mode="IRS")
      
      Rslt_Uncertain.Boulder.Number<-rbind(Rslt_Uncertain.Boulder.Number,
                                           data.frame(P=seq(0,1,length.out = N.random.Runs)
                                                      ,Min=sort(Rslt_Uncertain.Boulder.Number.No.Bottom.Jam[1,])/10^3
                                                      ,Max=sort(Rslt_Uncertain.Boulder.Number.No.Bottom.Jam[2,])/10^3
                                                      ,Hyp=rep("No bottom jam",N.random.Runs)))
      }
    }
    
    
    ###################Plot----
    
    PboxPlot<-ggplot()+theme_bw(base_size = 9)+
      # geom_hline(aes(yintercept=10^3*0.2))+
      # geom_hline(aes(yintercept=10^3*0.4))+
      # geom_hline(aes(yintercept=10^3*0.8))+
      # annotate(geom = "text", x = 0.5, y = 10^3*0.2,vjust=(-0.5), label = "Event Class 1",srt=90)+
      # annotate(geom = "text", x = 0.5, y = 10^3*0.4,vjust=(-0.5), label = "Event Class 2",srt=90)+
      # annotate(geom = "text", x = 0.5, y = 10^3*0.8,vjust=(-0.5), label = "Event Class 3",srt=90)+
      geom_vline(aes(xintercept = 1))+
      geom_vline(aes(xintercept = 0))
    
    if(Compute.With.Best.Estimate.Boulder.Number && Compute.With.Uncertain.Boulder.Number==FALSE){
      PboxPlot<-PboxPlot+
        geom_ribbon(data=Rslt_Best.Estimate.Boulder.Number,aes(x =P, ymin=Min, ymax=Max,fill="1",group=Hyp),alpha=0.3,lwd=1)+
        scale_fill_manual("Probability box"
                          ,values=c("blue"),labels=c("Boulder numbers: best estimates"))+
        geom_line(data=Rslt_Best.Estimate.Boulder.Number,aes(y =Min ,x=P,colour="1",lty=Hyp),lwd=1)+
        geom_line(data=Rslt_Best.Estimate.Boulder.Number,aes(y =Max ,x=P,colour="2",lty=Hyp),lwd=1)+
        scale_linetype("Type of run")
    }
    
    if(Compute.With.Best.Estimate.Boulder.Number == FALSE && Compute.With.Uncertain.Boulder.Number){
      PboxPlot<-PboxPlot+
      geom_ribbon(data=Rslt_Uncertain.Boulder.Number,aes(x =P, ymin=Min, ymax=Max,fill="2",group=Hyp),alpha=0.3,lwd=1)+
        scale_fill_manual("Probability box"
                          ,values=c("black"),labels=c("Boulder numbers: uncertain"))+
      geom_line(data=Rslt_Uncertain.Boulder.Number,aes(y =Min ,x=P,colour="1",lty=Hyp),lwd=1)+
      geom_line(data=Rslt_Uncertain.Boulder.Number,aes(y =Max ,x=P,colour="2",lty=Hyp),lwd=1)+
        scale_linetype("Type of run")
    }
    
    if(Compute.With.Best.Estimate.Boulder.Number && Compute.With.Uncertain.Boulder.Number)
    {
      PboxPlot<-PboxPlot+
        geom_ribbon(data=Rslt_Best.Estimate.Boulder.Number,aes(x =P, ymin=Min, ymax=Max,fill="1",group=Hyp),alpha=0.3,lwd=1)+
        geom_ribbon(data=Rslt_Uncertain.Boulder.Number,aes(x =P, ymin=Min, ymax=Max,fill="2",group=Hyp),alpha=0.3,lwd=1)+
        scale_fill_manual("Probability box"
                          ,values=c("blue","black"),labels=c("Boulder numbers: best estimates","Boulder numbers: uncertain"))+
        geom_line(data=Rslt_Best.Estimate.Boulder.Number,aes(y =Min ,x=P,colour="1",lty=Hyp),lwd=1)+
        geom_line(data=Rslt_Best.Estimate.Boulder.Number,aes(y =Max ,x=P,colour="2",lty=Hyp),lwd=1)+
        geom_line(data=Rslt_Uncertain.Boulder.Number,aes(y =Min ,x=P,colour="1",lty=Hyp),lwd=1)+
        geom_line(data=Rslt_Uncertain.Boulder.Number,aes(y =Max ,x=P,colour="2",lty=Hyp),lwd=1)+
        scale_linetype("Type of run")
     
    }
  
    PboxPlot<-PboxPlot+
      geom_hline(aes(yintercept=10^3*0.2))+
      geom_hline(aes(yintercept=10^3*0.4))+
      geom_hline(aes(yintercept=10^3*0.8))+
      geom_hline(aes(yintercept=10^3*1.4))+
      geom_hline(aes(yintercept=10^3*2.2))+
      geom_hline(aes(yintercept=10^3*2.8))+
      annotate(geom = "text", x = 0.5, y = 10^3*0.2,vjust=(-0.5), label = "Event Class 1",srt=90,alpha=0.5)+
      annotate(geom = "text", x = 0.5, y = 10^3*0.4,vjust=(-0.5), label = "Event Class 2",srt=90,alpha=0.5)+
      annotate(geom = "text", x = 0.5, y = 10^3*0.8,vjust=(-0.5), label = "Event Class 3",srt=90,alpha=0.5)+
      annotate(geom = "text", x = 0.5, y = 10^3*1.4,vjust=(-0.5), label = "Event Class 4",srt=90,alpha=0.5)+
      annotate(geom = "text", x = 0.5, y = 10^3*2.2,vjust=(-0.5), label = "Event Class 5",srt=90,alpha=0.5)+
      annotate(geom = "text", x = 0.5, y = 10^3*2.8,vjust=(-0.5), label = "Event Class 6",srt=90,alpha=0.5)+
      scale_colour_manual(name="Bounding CDF",values=c("lightblue","darkblue"),labels=c("Lower","Upper"))+
      coord_flip()+ #To have Probability as Y
      # theme(legend.position = "top")+
      labs( y = "Released volume [1000m3]",x = "Cumulative distribution function"
            ,caption=paste("Buffering model V2.0 used on", lubridate::today(),"| Number of runs N =",N.random.Runs)
            ,title = paste("Uncertainty analysis of released volume for event of class ",Magnitude.class))+
      ggsave(paste0("2Outputs/Pbox/ReleasedVolume_EvtClass",Magnitude.class,"_Nrun_",N.random.Runs,"_NboulderUncertain.png"), width = 16.5, height = 7,units="cm")
  }  
  if(Compute.With.Best.Estimate.Boulder.Number){
    save(Rslt_Best.Estimate.Boulder.Number,file=paste0("2Outputs/Rdata/ReleasedVolume_EvtClass",Magnitude.class,"_Nrun_",N.random.Runs,"_NboulderBestEstimate.RData"))
  }
  if(Compute.With.Uncertain.Boulder.Number)
  {
    save(Rslt_Uncertain.Boulder.Number,file=paste0("2Outputs/Rdata/ReleasedVolume_EvtClass",Magnitude.class,"_Nrun_",N.random.Runs,"_NboulderUncertain.RData"))
  }
 

  # Stop the clock
  proc.time() - ptm
}
#
print(paste("Run",SubRep$Name[Rep.Ind],"finished at",now()))
}