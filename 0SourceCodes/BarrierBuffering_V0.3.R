#Flow routing through open check dam

#Version 0.3: add boulder probability as uncertain parameters

#Version 0.2 : reduce the surge volume by the volume of the boulders already detected
#Add crest discharge overtopping the structure.
#Compute opening width accounting for trapezoidal shape of weir.

#Version 0.1 : Add an arbitrary number of openings and code obstruction based on 
# boulder size and random passage of boulders 

#Version 0 : changed slit obstruction on width to obstruction from the base

#Version -1 : based on code HydrographBufferingV0c # G. PITON, Aug. 2019
#


#Whole code and framework of analysis described in : # Piton G, Goodwin SR, Mark E, Strouth A. 2022. 
# Debris flows, boulders and constrictions: a simple framework for modeling jamming, and its consequences on outflow. 
# Journal of Geophysical Research: Earth Surface DOI: 10.1029/2021JF006447 
# [online] Available from: https://onlinelibrary.wiley.com/doi/10.1029/2021JF006447 (Accessed 24 April 2022)


#Flow discharge through opening defined according to Piton G, Recking A. 2016. 
# Design of sediment traps with open check dams. I: hydraulic and deposition processes. 
# Journal of Hydraulic Engineering 142 : 1–23. DOI: 10.1061/(ASCE)HY.1943-7900.0001048


#Jamming of boulders: conditions defined according to : 
# Piton G, Charvet G, Kuss D, Carladous S. 2020. 
# Putting a Grill (or Not) in Slit Dams Aiming at Trapping Debris Flows? Lessons Learnt From France. 
# AGHP Technical Notes : 1–5. [online] Available from: https://hal.archives-ouvertes.fr/hal-02701076

##### 
BarrierBufferingUncertainSlope_V0.3<-function(input,Boulder.Generation.Mode)
{
# Time step
TimeStep<-1 #(s) Should be an integer in seconds, so no less than 1 second
# small enough to capture the peak, the code seems to get stuck in infinite loops if set at 5 s with low slopes


################################
#    EXTRACT INPUT RUN DATA----
################################

    Volume<-input[1]*10^3
    Qpeak<-input[2]
    #Event features computation
    Duration<-round(Volume/(Qpeak/2),0)
    PeakLag<-input[3]#*Duration
    SimulationDuration<-round(FactorOfDuration*Duration)
    #Number of time steps
    N.time.steps<-ceiling(SimulationDuration/TimeStep)
    
    #### Slope of deposition
    SlopeDep<-input[4]#
    
    #Initial deposition height
    DepositDepth.initial<-input[5] 
    
    #Base jam height 
    Base.Jam.height<-input[6]
    
    ################################
    #    OPEN INPUT DATA----
    ################################
    
    #Interpolation of storage - elevation curve
    # StorageElevation<-read.csv("./1Data/ElevationStorageCurves.txt",sep="\t")
    storageElevationCurve<-data.frame(s=0,h=StorageElevation$Z)
    #Interpolation for the slope of deposition selected
    for (i in (1:length(StorageElevation[,1])))
    {
      storageElevationCurve$s[i]<-approx(x=as.numeric(substr(names(StorageElevation[,-1]),2,6))
                                       ,y=StorageElevation[i,-1],xout = SlopeDep/100)$y/10^3
    }
    rm(StorageElevation)
    
    
    #Count number of boulders 
    # Boulders<-read.csv("./1Data/RangeOfBoulders.txt",sep="\t")
    
    for(i in (1:length(Boulders[,1])))
    {
      Boulders$Dmin[i]<-as.numeric(substr(Boulders[i,1],1,(stringr::str_locate(Boulders[,1],"-")[i,1]-1)))
      Boulders$Dmax[i]<-as.numeric(substring(Boulders[i,1],(stringr::str_locate(Boulders[,1],"-")[i,1]+1)))
      Boulders$Number[i]<-input[6+i]
    }
    Boulders$Diameter<-0.5*(Boulders$Dmin+Boulders$Dmax)
    #Elementary volume of each boulder class
    Boulders$V<-pi/6*Boulders$Diameter^3
    #Maximum number of boulder that could theoretically be observed in the reference volume
    Boulders$Nmax<-round(Boulders$Reference_Volume/Boulders$V,0)
    #PRobability of having a boulder each time a volume of debris flow = boulder volume pass
    Boulders$P<-Boulders$Number/Boulders$Nmax
    
    #Initialize a data.frame to record boulders generated
    Boulder.list.all<-data.frame(T=NULL,D=NULL,Opening=NULL,Jammed=NULL)
    
    
    #Definition of openings
    # Opening<-read.csv("./1Data/Opening.txt",header = T)
    #reorder Opening by increasing base level to have the spillway as last opening
    Opening<-Opening[rank(Opening$Base.Level),]
    
    #Number of opening
    N.opening<-length(Opening$Number)
    
    OpeningMinBaseLevel<-min(Opening$Base.Level)
    #by default, the spillway should be the opening below the top one (top one is the crest)
    SpillwayLevel<-Opening$Base.Level[(N.opening-1)] 
    #But for dams with complex crest shape, one can write in Opening.txt which open is the 'Spillway' (Case sensitive test)
    if(sum(Opening$Comment=="Spillway")>0){
      SpillwayLevel<-Opening$Base.Level[which(Opening$Comment=="Spillway")]
      }
    CrestLevel<-Opening$Base.Level[N.opening]
  
    #Clogging level and width by boulders
    Boulderclogging.level<-data.frame(matrix(Opening$Boulder.vertical.clogging, N.time.steps, N.opening,byrow=TRUE),stringsAsFactors=F)
    names(Boulderclogging.level)<-paste0("Z",(1:N.opening))
    #For initial clogging we take the maximum between the pre-existing clogging height and the base jam height arriving at the surge front
    Boulderclogging.level[,1]<-max(Boulderclogging.level[,1],Base.Jam.height)
    
    Boulderclogging.width<-data.frame(matrix(Opening$Boulder.lateral.clogging, N.time.steps, N.opening,byrow=TRUE),stringsAsFactors=F)
    names(Boulderclogging.width)<-paste0("W",(1:N.opening))
    

    #Discharge elevation curves
    h<-seq(from=OpeningMinBaseLevel, to=max(storageElevationCurve$h)+5,by=0.1) #Extend 5 meter above the crest
   
    ###Plot V vs h
    if(Print.Data.Plot){
      ggplot(storageElevationCurve)+
        geom_line(aes(x=s,y=h),col=2)+theme_bw(base_size = 9)+
        labs( x = "Storage capacity [*1000 m3]",y = "Flow level at barrier [m.a.s.l.]"
              ,title = paste("Stage - volume capacity"))+
        geom_hline(aes(yintercept = OpeningMinBaseLevel,lty="1"))+
        geom_hline(aes(yintercept = SpillwayLevel,lty="2"))+
        scale_linetype_manual("",labels=c("Opening base level","Spillway level"),values=c(2,3))+
        theme(legend.position = "top")+
        ggsave("2Outputs/StageVolumeBarrier.png", width = 8, height = 6,units="cm")
    }
    #Print stage - discharge relationship
    if(Print.Data.Plot){
      ggplot(data.frame(h=h[h<max(storageElevationCurve$h)]
                        #Capacity without any clogging
                        ,Q0=apply(Q_CompoundBarrier(Opening = Opening
                                                    ,BaseClogging = rep(0,length(Opening$Number))
                                                    ,WidthClogging=rep(0,length(Opening$Number))
                                                    ,h=h[h<max(storageElevationCurve$h)]
                        ),1,sum)
                        #Capacity with only initial clogging
                        ,Q=apply(Q_CompoundBarrier(Opening = Opening
                                                   ,BaseClogging = Boulderclogging.level[1,]
                                                   ,WidthClogging=Boulderclogging.width[1,]
                                                   ,h=h[h<max(storageElevationCurve$h)]
                        ),1,sum)))+
        geom_hline(aes(yintercept = OpeningMinBaseLevel,lty="1"))+
        
        geom_hline(aes(yintercept = SpillwayLevel,lty="2"))+
        geom_line(aes(x=Q0,y=h,lty="3"))+
        geom_line(aes(x=Q,y=h,lty="4"))+
        scale_linetype_manual("Stage - discharge VS clogging",labels=c("Opening base level"
                                                                       ,"Spillway level"
                                                                       ,"Flow level (without clogging)"
                                                                       ,"Flow level (initial clogging)")
                              ,values=c(2,3,1,4))+
        labs( x = "Outlet discharge [m3/s]",y = "Flow level at barrier [m.a.s.l.]")+
          theme_bw(base_size = 9)+guides(linetype= guide_legend(nrow=2,title.position="top"))+
        theme(legend.position = "top")+
        theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm"))+
        # ylim(min(storageElevationCurve$h),max(storageElevationCurve$h))+
        ggsave("2Outputs/StageDischargeInitial.png", width = 8, height = 6,units="cm")
    }
    #Creation of time series
    # Hydrograph<-data.frame(T=c(0,PeakLag,Duration),Q=c(0,Qpeak,0))
    
    ################################
    #    COMPUTATION----
    ################################
    
    
    #Result dataset initialisation
    Reservoir<-data.frame(T=seq(0,SimulationDuration,length.out = N.time.steps)
                          ,Qi=approx(x=c(0,PeakLag,Duration)
                                     ,y=c(0,Qpeak,0)
                                     ,xout = seq(0,SimulationDuration,length.out = N.time.steps)
                                     ,yright=0)$y #Qinlet
                          ,Qo=0 #Qoutlet
                          ,V=NA#/10^3 #stored volume
                          ,Z=DepositDepth.initial #level at barrier 
                          ,Qslit=rep(0,N.time.steps) #Discharge passing by openings only
                          ,Qspillway=rep(0,N.time.steps)) #Discharge passing by spillway only
    #First step
    i<-1
    Reservoir$V[i]<-approx(x=storageElevationCurve$h,y=storageElevationCurve$s
                           ,xout=DepositDepth.initial+OpeningMinBaseLevel
                           ,yleft=min(storageElevationCurve$s)
                           ,yright=max(storageElevationCurve$s))$y
    #For initial level we take the maximum between the pre-existing deposit height and the base jam height arriving at the surge front
    Reservoir$Z[i]<-OpeningMinBaseLevel+max(DepositDepth.initial,Base.Jam.height)+0.1 #+0.1 to avoid 0 values and null discharge
    #Sum the discharge of all orifices 
    Reservoir$Qo[i]<-sum(Q_CompoundBarrier(Opening = Opening
                                           ,BaseClogging = Boulderclogging.level[i,]
                                           ,WidthClogging = Boulderclogging.width[i,]
                                           ,h=Reservoir$Z[i])) 
    
    #Next steps
    i<-2
    #Actual computation step by step
    while(i<N.time.steps)
    { 
      #Initialize outlet discharge at step i
      Reservoir$Qo[i]<-sum(Q_CompoundBarrier(Opening = Opening
                                             ,BaseClogging = Boulderclogging.level[i,]
                                             ,WidthClogging = Boulderclogging.width[i,]
                                             ,h=Reservoir$Z[i]))
    
      #Initialize the reservoir level accuracy criteria
      dZ<-1
      Ninter<-0
      #Look for the values of reservoir level and outlet discharge that align volume conservation
      while(abs(dZ)>0.05 && Ninter<1000) #targeted reservoir level accuracy, in meters
      {   #Previous reservoir level recording
          Z<-Reservoir$Z[i]
          #Compute new reservoir volume by volume balance
          Reservoir$V[i]<-max(0,Reservoir$V[i-1]+TimeStep*(Reservoir$Qi[i]+Reservoir$Qi[i-1]-Reservoir$Qo[i]-Reservoir$Qo[i-1])/2/10^3)
          
          #Compute new reservoir level by interpolation on volume - elevation curve
          Reservoir$Z[i]<-approx(x=storageElevationCurve$s,y=storageElevationCurve$h,xout=Reservoir$V[i]
                                 ,yleft=min(storageElevationCurve$h),yright=max(storageElevationCurve$h))$y
          
          #Update outlet discharge with new reservoir level
          Reservoir$Qo[i]<-sum(Q_CompoundBarrier(Opening = Opening
                                                 ,BaseClogging = Boulderclogging.level[i,]
                                                 ,WidthClogging = Boulderclogging.width[i,]
                                                 ,h=Reservoir$Z[i]))
          #Update dZ
          dZ<-Z-Reservoir$Z[i]
          Ninter<-Ninter+1
      }
      if(Ninter>=1000) #If more than 1000 iteration does not enable to converge, we kill the run
      {print("One simulation killed")
        i<-N.time.steps
      }
      
      #Compute opening discharge, i.e., all but the last opening
      Reservoir$Qslit[i]<-sum(Q_CompoundBarrier(Opening = Opening[1:(N.opening-2),] #Top opening is the crest, before is the spillway
                                             ,BaseClogging = Boulderclogging.level[i,(1:(N.opening-2))]
                                             ,WidthClogging = Boulderclogging.width[i,(1:(N.opening-2))]
                                             ,h=Reservoir$Z[i]))
      #Compute spillway discharge, i.e., last opening
      Reservoir$Qspillway[i]<-as.numeric(Q_CompoundBarrier(Opening = Opening[(N.opening-1),] #Top opening is the crest, before is the spillway
                                                    ,BaseClogging = Boulderclogging.level[i,(N.opening-1)]
                                                    ,WidthClogging = Boulderclogging.width[i,(N.opening-1)]
                                                    ,h=Reservoir$Z[i]))
      
      #The clogging of the opening is computed
      #  during the duration of the input surge
      #                          and also later if the user decided not to kill the transport of boulder after the 
      #                          end of the input surge. 
      if((i*TimeStep < Duration | Kill.boulder.transport.after.input.surge == FALSE))
      {
        
        #At each second, compute whether some clogging occur or not
        for(TimeStep.index in (1:round(TimeStep)))
        {
          #Initialize 
          Jam.Width<-0
          ####Clogging of the openings
          #Compute the volume of the surge passing each second  through each opening, i.e., instantaneous discharges
          Volume.Surge<-as.numeric(Q_CompoundBarrier(Opening = Opening
                                                     ,BaseClogging = Boulderclogging.level[i,]
                                                     ,WidthClogging = Boulderclogging.width[i,]
                                                     ,h=Reservoir$Z[i]))
          #Alternative: integrate the volume over the full time step: assume that all boulders arrive at the same time in one 
          # time step, considered too conservative.
          # TimeStep*as.numeric(Q_CompoundBarrier(Opening = Opening,BaseClogging = Boulderclogging.level[i,],h=Reservoir$Z[i]))
          
          #Initialize the list of boulder passing through each opening
          for(Opening.Ind in (1:N.opening))
          { 
            Volume.Surge.inst<-Volume.Surge[Opening.Ind]
            Boulder.list<-BoulderPassing(Volume.Surge.inst,Boulders)
            if(!is.na(Boulder.list)[1]){
              Boulder.list<-data.frame(T=rep(i*TimeStep,length(Boulder.list))
                                       ,D=sort(Boulder.list,na.last=TRUE,decreasing =TRUE)
                                       ,Opening=rep(Opening.Ind,length(Boulder.list))
                                       ,Jammed=rep("a) Unjammed",length(Boulder.list)))
              # data.frame(T=NULL,D=NULL,Opening=NULL,Jammed=NULL)
              # if(!is.na(Boulder.list)){print(Boulder.list$D)}
              
              #Full width clogging 
              #Sum the two biggest diameters
              Jam.Width<-sum(Boulder.list$D[1:2],na.rm = TRUE)
              if(Opening$Type[Opening.Ind]=="weir")
              {
                #Account for trappezoidal shape if weir
                Opening.Remaining.Width<-2/tan(Opening$Param[Opening.Ind]/180*pi)*Boulderclogging.level[i,Opening.Ind]+Opening$Width[Opening.Ind]-Boulderclogging.width[i,Opening.Ind]
              }else{
                #Otherwise, use opening width minus clogging
                Opening.Remaining.Width<-Opening$Width[Opening.Ind]-Boulderclogging.width[i,Opening.Ind]
              }
              
              #If the jam width is larger than the opening (eventually yet partially clogged), 
              # it is jammed and the jam is as high as the biggest boulder passing
              if(max(Jam.Width,0,na.rm = T)> Opening.Remaining.Width){
                Boulderclogging.level[i:N.time.steps,Opening.Ind]<-Boulderclogging.level[i,Opening.Ind]+max(Boulder.list$D,na.rm = TRUE)
                # Record the boulders
                if(length(Boulder.list$Jammed)>1){Boulder.list$Jammed[1:2]<-"b) Laterally jammed"}else{ Boulder.list$Jammed[1]<-"b) Laterally jammed"}
                
              }
              
              #Partial width clogging
              if(Opening$Type[Opening.Ind]=="slot")
              {#Record that the opening width clogging increase by the diameters of boulders 
                # that are bigger than slot height
                Boulderclogging.width[i:N.time.steps,Opening.Ind]<-min(Opening$Width[Opening.Ind]
                                                                       ,Boulderclogging.width[i-1,Opening.Ind]+sum(Boulder.list$D[which(Boulder.list$D>(Opening$Param[Opening.Ind]-Opening$Base.Level[Opening.Ind]))]))
                Boulder.list$Jammed[which(Boulder.list$Jammed!="b) Laterally jammed" & Boulder.list$D>(Opening$Param[Opening.Ind]-Opening$Base.Level[Opening.Ind]))]<-"c) Vertically jammed"  
              }
              Boulder.list.all<-rbind(Boulder.list.all,Boulder.list)
            }
          }#end of the opening loop
        }#end of the Timestep loop  
      }
      
       
      #iterate on i to compute the next step
      i<-i+1
    }
    
    ################################
    #    RECORD THE TIME SERIES AND INDICATORS----
    ################################
    Reservoir<-cbind(Reservoir
                     ,Boulderclogging.level[,(1:(N.opening-1))] #Only until N.opening-1 because clogging is a no sense on the crest
                     ,Boulderclogging.width[,(1:(N.opening-1))])#Only until N.opening-1 because clogging is a no sense on the crest
    Reservoir$LevelClogging1<-Reservoir$Z1+OpeningMinBaseLevel
    
    #Combine the tables of clogging for later plot
    N.slot<-(Opening$Type=="slot") #which opening are slots
    WidthClogging<-VerticalClogging<-NULL
    for(i in (1:(N.opening-1))) #Only until N.opening-1 because clogging is a no sense on the crest
    {
      if(i==1){ #Initialize
        VerticalClogging<-data.frame(T=Reservoir$T,Opening="#1"
                                     ,Clogging.rate=Boulderclogging.level[,i]/(Opening$Param[i]-Opening$Base.Level[i]))
        if(N.slot[i])
        {
          WidthClogging<-data.frame(T=Reservoir$T,Opening="#1"
                                    ,Clogging.rate=Boulderclogging.width[,i]/Opening$Width[i])
        }
      }
      #Append
      VerticalClogging<-rbind(VerticalClogging,data.frame(T=Reservoir$T,Opening=paste0("#",i)
                                                          ,Clogging.rate=Boulderclogging.level[,i]/(Opening$Param[i]-Opening$Base.Level[i])))
      
      if((N.slot[i] && is.null(WidthClogging)))
      {#Initialize
        WidthClogging<-data.frame(T=Reservoir$T,Opening=paste0("#",i)
                                  ,Clogging.rate=Boulderclogging.width[,i]/Opening$Width[i])
      }else{
        if((N.slot[i] && !is.null(WidthClogging)))
        {#Append
          WidthClogging<-rbind(WidthClogging
                               ,data.frame(T=Reservoir$T,Opening=paste0("#",i)
                                           ,Clogging.rate=Boulderclogging.width[,i]/Opening$Width[i]))
        }
      }
    }
    #Set vertical clogging = 100% if boulder height higher than slot height
    if(!is.null(VerticalClogging)){
      VerticalClogging$Clogging.rate[VerticalClogging$Clogging.rate>1]<-1  
    }
    #Same on horizontal clogging
    if(!is.null(WidthClogging)){
      WidthClogging$Clogging.rate[WidthClogging$Clogging.rate>1]<-1  
    }
    
    #remove last line which is not computed
    Reservoir<-Reservoir[-N.time.steps,]

    #Extract the maximum level of the result data set.
    Zmax<-max(Reservoir$Z,na.rm = T)
    Zfinal<-Reservoir$Z[N.time.steps-1]
    
    #Final clogging status
    Boulder.z.final<-Boulderclogging.level[(N.time.steps-1),]
    Boulder.w.final<-Boulderclogging.width[(N.time.steps-1),]

    
    #update the new clogging status in the text fill to be used 
    # at the next run with the final clogging status
    Opening$Boulder.vertical.clogging<-as.numeric(Boulder.z.final)
    Opening$Boulder.lateral.clogging<-as.numeric(Boulder.w.final)
    if(Keep.track.of.clogging.state)
    {
      write.csv(Opening,file="./1Data/Opening.txt",row.names = FALSE)  
    }
    
    
    #Save boulder sizes
    if(Save.boulder.size){
      dir.create(paste0("2Outputs/Boulders/",Event.name,"_ComputedOn",lubridate::today()))
      File.Name<-paste0("2Outputs/Boulders/",Event.name
                        ,"_ComputedOn",lubridate::today(),"/Boulders",lubridate::now(),".Rdata")
      File.Name<-str_replace_all(File.Name,":","-")
      File.Name<-str_replace_all(File.Name," ","At")
      save(Boulder.list.all,file=File.Name)
    }
    
    #Save hydrograph
    if(Save.hydrographs){
      dir.create(paste0("2Outputs/Hydrographs/",Event.name,"_ComputedOn",lubridate::today()))
      File.Name<-paste0("2Outputs/Hydrographs/",Event.name
                        ,"_ComputedOn",lubridate::today(),"/Hydrographs",lubridate::now(),".Rdata")
      File.Name<-str_replace_all(File.Name,":","-")
      File.Name<-str_replace_all(File.Name," ","At")
      Hydrographs<-Reservoir[,c(1:3,6:7)]
      save(Hydrographs,file=File.Name)
      rm(Hydrographs)
    }
    
    
    #Extract the maximum Storage of the result data set.
    Vmax<-max(Reservoir$V,na.rm = T)
    Vfinal<-Reservoir$V[N.time.steps-1]
    
    #Extract maximum outlet discharge
    Qp.out<-max(Reservoir$Qo,na.rm=T)
    
    #Look for time step of level passing over and below the crest
    Nmin<-min(which(Reservoir$Z>Opening$Base.Level[N.opening]))
    Nmax<-max(which(Reservoir$Z>Opening$Base.Level[N.opening]))

    #Look for time step of level passing over and below the spillway
    Nmin.s<-min(which(Reservoir$Z>Opening$Base.Level[(N.opening-1)]))
    Nmax.s<-max(which(Reservoir$Z>Opening$Base.Level[(N.opening-1)]))
    
    #OVertopping duration
    Tover<-TimeStep*(Nmax-Nmin)
    Tover.s<-TimeStep*(Nmax.s-Nmin.s)
    
    #Released volume = sum of released discharge * timestep in Mm3
    Vout<-round(sum(Reservoir$Qo)*TimeStep/10^3,3)
    #Part passing by the slit
    VoutSlit<-round(sum(Reservoir$Qslit)*TimeStep/10^3,3)
    #Part passing over the spillway
    VoutSpillway<-sum(Reservoir$Qspillway)*TimeStep/10^3
    #Remaing part passing over the Crest
    VoutCrest<-Vout-VoutSlit-VoutSpillway
    
    #Add virtual values to extend the legend of clogging from 0 to 100%
    VerticalClogging<-rbind(VerticalClogging,data.frame(T=c(-2,-1.5,-1)*10^3
                                                        ,Opening=rep(VerticalClogging$Opening[1],3)
                                                        ,Clogging.rate=c(0,1,rep(VerticalClogging$Clogging.rate[1],1))))
    if(Opening$Type[1]=="slot")
    {
      WidthClogging<-rbind(WidthClogging,data.frame(T=c(-2,-1.5,-1)*10^3
                                                    ,Opening=rep(WidthClogging$Opening[1],3)
                                                    ,Clogging.rate=c(0,1,rep(WidthClogging$Clogging.rate[1],1))))  
    }


    if(Print.Final.Plot){
        # Create directory to save recorded buffering results
        dir.create(paste0("2Outputs/Buffering/",Event.name,"_ComputedOn",lubridate::today())
                   ,showWarnings = FALSE,recursive = TRUE)
      
      Plot_BufferingModel(Reservoir,WidthClogging,VerticalClogging
                          ,N.opening,storageElevationCurve,Volume
                          ,N.time.steps,Duration
                          ,OpeningMinBaseLevel,SpillwayLevel,CrestLevel
                          ,Boulder.Generation.Mode)
    }
    
    ###############END OF THE FUNCTION
    
    if(Ninter>=1000)
    {return(rep(NA,7))}else
    {
      RESULTS<-data.frame("Zmax"=Zmax,
                          "Zfinal"=Zfinal
                          ,"Qp.out"=Qp.out
                          ,"Vmax"=Vmax*1000,"Vout"=Vout*1000,"Voutslit"=VoutSlit*1000
                          ,"Voutsplillway"=VoutSpillway*1000,"VoutCrest"=VoutCrest*1000
                          ,"Vfinal"=Vfinal*1000
                          ,"Tover"=Tover,"Tover.spillway"=Tover.s)
      RESULTS<-cbind(RESULTS
                     ,Boulder.z.final[1:(N.opening-1)] #only until N.opening -1 because
                     ,Boulder.w.final[1:(N.opening-1)])#the top opening is the crest
      
      if(Save.Rslt.Single.Run)
      {
        dir.create(paste0("2Outputs/Rdata/",Event.name,"_ComputedOn",lubridate::today()))
        File.Name<-paste0("2Outputs/Rdata/",Event.name
                          ,"_ComputedOn",lubridate::today(),"/RunResults",lubridate::now(),".Rdata")
        File.Name<-str_replace_all(File.Name,":","-")
        File.Name<-str_replace_all(File.Name," ","At")
        save(list = c("RESULTS","input","Boulder.Generation.Mode"),file=File.Name)
      }
      
    return(RESULTS)
    }
  }#end of function




#######General function that can be called in the possibilistic analysis
CheekyeBufferingModel_UncertainBoulderNumber<-function(input)
{
  #Transform volume in *1000 m3
  input[1]<-input[1]*10^3
  # Duration = V/Q*2
  Duration<-round(input[1]*10^3/(input[2]/2),0)
  #Pass duration in second rather than dimensionless
  input[3]<-input[3]*Duration
  
  Boulder.Generation.Mode<-"Uncertain numbers"
  #V0.3 uses the uncertain data of boulder numbers
  Result<-BarrierBufferingUncertainSlope_V0.3(input,Boulder.Generation.Mode)
  
  return(Result)
}

# #Function exporting the volume flowing though and atop the barrier 
CheekyeBufferingModel_UncertainBoulderNumber_Vtot<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel_UncertainBoulderNumber(input))
return(Rslt[5])
}

