### Compute the functionning of a structure
#V0 July 2023 - G. Piton & C. Misset

#Originally inspired by BarrierBuffering_V0.3.R
# see--> https://github.com/GuillaumePitonInrae/CheekyeDebrisFlowBarrier/tree/main/0SourceCodes

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

###NOTA: AJOUTER UN BLOCAGE AUTOMATIQUE POUR ORIFICE DE TAILLE RESIDUELLE < DIAMETRE MINI DES CLASSES DE BLOCS


# ##########################################To REMOVE --------------------
# InputDataRepository <-  "D:/Private/05_PROJETS/2023_DFbuffering/4Simu/DFbuffering/1Data/structure/pdd 1"
# #Load storage - elevation curve
# StorageElevation<-read.csv(paste0(InputDataRepository,"/ElevationStorageCurves.txt"),sep="\t")
# #Load the barrier definition (openings)
# Opening<-read.csv(paste0(InputDataRepository,"/Opening.txt"),header = T)
# ##########################################To REMOVE 


Structure_functionning_V0.1<-function(input,Qin,Opening,StorageElevation)
{
  ################################
  #    OPEN INPUT DATA----
  ################################
  
  N.time.steps<-length(Qin[,1])
  #### Duration of the surge assuming a triangular hydrograph
  # Duration<-round(input[1]*10^3/(input[2]/2),0)
  Duration<-max(which(Qin$Q>max(Qin$Q/10000)))
  
  #### Slope of deposition
  SlopeDep<-input[4]#
  #Initial deposition height
  DepositDepth.initial<-input[5] 
  #Base jam height 
  Base.Jam.height<-input[6]
  
  
  #Interpolation of storage - elevation curve
  # StorageElevation<-read.csv("./1Data/ElevationStorageCurves.txt",sep="\t")
  storageElevationCurve<-data.frame(s=0,h=StorageElevation$Z)
  
  # Check that deposition slope is within the range of data
  if(min(as.numeric(substr(names(StorageElevation[,-1]),2,6)))>SlopeDep/100)
  {
    print("Deposition slope selected below the minimum values of the elevation - storage capacity!
            Used the minimum value of the table but consider adding the relevant data to the table!")
  }
  #Interpolation for the slope of deposition selected
  for (i in (1:length(StorageElevation[,1])))
  {
    storageElevationCurve$s[i]<-approx(x=as.numeric(substr(names(StorageElevation[,-1]),2,6))
                                       ,y=StorageElevation[i,-1]
                                       ,xout = SlopeDep/100
                                       ,yleft=min(StorageElevation[i,-1])
                                       ,yright=max(StorageElevation[i,-1])
    )$y/10^3
  }
  # rm(StorageElevation)
  
  #Definition of openings
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
  
  #Count number of boulders 
  for(i in (1:length(Boulders[,1])))
  {
    Boulders$Dmin[i]<-as.numeric(substr(Boulders[i,1],1,(stringr::str_locate(Boulders[,1],"-")[i,1]-1)))
    Boulders$Dmax[i]<-as.numeric(substring(Boulders[i,1],(stringr::str_locate(Boulders[,1],"-")[i,1]+1)))
    Boulders$Number[i]<-input[6+i]
  }
  #Mean boulder size of each class used to compute the typucal volume of a boulder
  Boulders$Diameter<-0.5*(Boulders$Dmin+Boulders$Dmax)
  #Elementary volume of each boulder class
  Boulders$V<-pi/6*Boulders$Diameter^3
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
  
  ################################
  #    COMPUTATION----
  ################################
  #Initialize a data.frame to record boulders generated
  Boulder.list.all<-data.frame(T=NULL,D=NULL,Class=NULL,Opening=NULL,Jammed=NULL)
  
  #Result dataset initialisation
  Reservoir<-data.frame(T=Qin$T
                        ,Qi=Qin$Q
                        ,Qo=0 #Qoutlet
                        ,V=NA#/10^3 #stored volume
                        ,Z=DepositDepth.initial #level at barrier 
                        ,Qslit=0 #Discharge passing by openings only
                        ,Qspillway=0) #Discharge passing by spillway only
  
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
    while(abs(dZ)>0.01*(SpillwayLevel-OpeningMinBaseLevel) && Ninter<1000) #targeted reservoir level accuracy,
      # 1% of the difference in elevation between the outlet and the spillway
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
        # time step, unused because considered too conservative if the time step is long.
        # TimeStep*as.numeric(Q_CompoundBarrier(Opening = Opening,BaseClogging = Boulderclogging.level[i,],h=Reservoir$Z[i]))
        
        #Initialize the list of boulder passing through each opening
        for(Opening.Ind in (1:N.opening))
        { 
          Volume.Surge.inst<-Volume.Surge[Opening.Ind]
          #Check whether the transfer from upstream is instantaneous or mixing
          if(is.na(Qin$p1[i]))  #if probability is NA, then we must directly transfer the upstream number of boulders
          {
            Boulder.list <-BoulderSizing(Qin[i,(1:length(Boulders[,1])+2+length(Boulders[,1]))])
          }else{
            #of the probability was not NA, then we randomly sample the boulder size
            Boulder.list<-BoulderPassing(Volume.Surge.inst
                                         ,Boulders$Dmin,Boulders$Dmax
                                         ,Boulder.probabilities=as.numeric(Qin[i,(1:length(Boulders[,1])+2)]))
            
          }
          
          if(!is.na(Boulder.list$D)[1])
          {
            Boulder.list<-data.frame(T=rep(Reservoir$T[i],length(Boulder.list$D))
                                     ,D=sort(Boulder.list$D,na.last=TRUE,decreasing =TRUE)
                                     ,Class=sort(Boulder.list$Class,na.last=TRUE,decreasing =TRUE)
                                     ,Opening=rep(Opening.Ind,length(Boulder.list$D))
                                     ,Jammed=rep("a) Unjammed",length(Boulder.list$D)))
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
   #Add the jamming state of the openings
  Reservoir<-cbind(Reservoir
                   ,Boulderclogging.level[,(1:(N.opening-1))] #Only until N.opening-1 because clogging is a no sense on the crest
                   ,Boulderclogging.width[,(1:(N.opening-1))])#Only until N.opening-1 because clogging is a no sense on the crest
  Reservoir$LevelClogging1<-Reservoir$Z1+OpeningMinBaseLevel
  
  
  #Add the number of boulders of each class to reservoir
  for(i in c(1:length(Boulders$Dmin)))
  {
    Boulder.N.jammed<-Boulder.list.all %>% 
      filter(Class==i) %>%
      filter(Jammed!="a) Unjammed") %>%
      group_by(T) %>%
      summarise(N=n())
    
    Boulder.N.unjammed<-Boulder.list.all %>% 
      filter(Class==i) %>%
      filter(Jammed=="a) Unjammed") %>%
      group_by(T) %>%
      summarise(N=n())
    
    Reservoir$X<-Reservoir$Y<-0
    Reservoir$X[match(Boulder.N.jammed$T,Reservoir$T)]<-Boulder.N.jammed$N
    names(Reservoir)[which(names(Reservoir)=="X")]<-paste0("Class",i,".jammed")
    
    Reservoir$Y[match(Boulder.N.unjammed$T,Reservoir$T)]<-Boulder.N.unjammed$N
    names(Reservoir)[which(names(Reservoir)=="Y")]<-paste0("Class",i,".unjammed")
    
  }
  
  #Combine the tables of clogging for later plot and analysis
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
                        ,N.opening,storageElevationCurve,input[1]*1000
                        ,N.time.steps,Duration
                        ,OpeningMinBaseLevel,SpillwayLevel,CrestLevel
                        ,Boulder.Generation.Mode)
  }
  
  ###############END OF THE FUNCTION
  if(Ninter>=1000)
  {return(rep(NA,dim(Reservoir)[2]))}else
  {return(Reservoir)}
}

Synthetic_Structure_results_V0.1<-function(Reservoir, Opening)
{  #Number of opening
  N.opening<-length(Opening$Number)
  N.time.steps<-length(Reservoir$T)
  
  #Extract the maximum level of the result data set.
  Zmax<-max(Reservoir$Z,na.rm = T)
  Zfinal<-Reservoir$Z[N.time.steps-1]
  
  #Extract the maximum Storage of the result data set.
  Vmax<-max(Reservoir$V,na.rm = T)
  Vfinal<-Reservoir$V[N.time.steps-1]
  
  
  #Record peak discharge at inlet
  Qp.in<-max(Reservoir$Qi,na.rm=T)
  
  #Extract maximum outlet discharge
  Qp.out<-max(Reservoir$Qo,na.rm=T)
  
  #Look for time step of level passing over and below the crest
  # Nmin<-min(which(Reservoir$Z>Opening$Base.Level[N.opening]))
  # Nmax<-max(which(Reservoir$Z>Opening$Base.Level[N.opening]))
  
  #Look for time step of level passing over and below the spillway
  # Nmin.s<-min(which(Reservoir$Z>Opening$Base.Level[(N.opening-1)]))
  # Nmax.s<-max(which(Reservoir$Z>Opening$Base.Level[(N.opening-1)]))
  
  #OVertopping duration
  # Tover<-TimeStep*(Nmax-Nmin)
  # Tover.s<-TimeStep*(Nmax.s-Nmin.s)
  
  #Released volume = sum of released discharge * timestep in Mm3
  Vout<-round(sum(Reservoir$Qo)*TimeStep/10^3,3)
  #Part passing by the slit
  VoutSlit<-round(sum(Reservoir$Qslit)*TimeStep/10^3,3)
  #Part passing over the spillway
  VoutSpillway<-sum(Reservoir$Qspillway)*TimeStep/10^3
  #Remaing part passing over the Crest
  VoutCrest<-Vout-VoutSlit-VoutSpillway
  
  if(is.na(Reservoir$T[1]))
  {return(NA)}else
  {
    RESULTS<-data.frame("Zmax"=Zmax,
                        "Zfinal"=Zfinal
                        ,"Qp.out"=Qp.out
                        ,"Vmax"=Vmax*1000,"Vout"=Vout*1000,"Voutslit"=VoutSlit*1000
                        ,"Voutsplillway"=VoutSpillway*1000,"VoutCrest"=VoutCrest*1000
                        ,"Vfinal"=Vfinal*1000
                        # ,"Tover"=Tover,"Tover.spillway"=Tover.s
                        ,"Qp.in"=Qp.in)
    RESULTS<-cbind(RESULTS
                   ,Reservoir[dim(Reservoir)[1],(7+1:(N.opening-1))]                    #only until N.opening -1 because
                   ,Reservoir[dim(Reservoir)[1],(7+(N.opening-1)+1:(N.opening-1))]      #the top opening is the crest
                   )
    
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
}
  