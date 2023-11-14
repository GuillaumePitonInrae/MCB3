### Compute the functionning of a structure, extract synthetic results and compute the whole cascade of structure
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

Structure_functionning<-function(ModelVersion,StructureName,input,Qin,Opening,StorageElevation)
{
  ################################
  #    OPEN INPUT DATA----
  ################################
  
  N_TimeSteps<-length(Qin[,1])
  #### Duration of the surge assuming a triangular hydrograph
  # Duration<-round(input[1]*10^3/(input[2]/2),0)
  Duration<-max(which(Qin$Q>max(Qin$Q/10000)))
  
  #### Slope of deposition
  SlopeDep<-input[4]#
  
  
  StructureRank<-Structures$Rank[which(Structures$Name == StructureName)]
  #Initial deposition height
  DepositDepthInitial<-input[5+(StructureRank-1)*2] 
  #Base jam height 
  BaseJamHeight<-input[5+(StructureRank-1)*2+1] 
  
  
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
  Opening<-Opening[rank(Opening$BaseLevel),]
  
  #Number of opening
  N_opening<-length(Opening$Number)
  
  OpeningMinBaseLevel<-min(Opening$BaseLevel)
  #by default, the spillway should be the opening below the top one (top one is the crest)
  SpillwayLevel<-Opening$BaseLevel[(N_opening-1)] 
  #But for dams with complex crest shape, one can write in Opening.txt which open is the 'Spillway' (Case sensitive test)
  if(sum(Opening$Comment=="Spillway")>0){
    SpillwayLevel<-Opening$BaseLevel[which(Opening$Comment=="Spillway")]
  }
  CrestLevel<-Opening$BaseLevel[N_opening]
  
  #Count number of boulders 
  for(i in (1:dim(Boulders)[1]))
  {
    Boulders$Number[i]<-input[6+i]
  }
  #Mean boulder size of each class used to compute the typucal volume of a boulder
  Boulders$Diameter<-0.5*(Boulders$BoulderDiameter_min+Boulders$BoulderDiameter_max)
  #Elementary volume of each boulder class
  Boulders$V<-pi/6*Boulders$Diameter^3
  #Clogging level and width by boulders
  BoulderClogging_level<-data.frame(matrix(Opening$BoulderVerticalClogging, N_TimeSteps, N_opening,byrow=TRUE),stringsAsFactors=F)
  names(BoulderClogging_level)<-paste0("Z",(1:N_opening))
  #For initial clogging we take the maximum between the pre-existing clogging height and the base jam height arriving at the surge front
  BoulderClogging_level[,1]<-max(BoulderClogging_level[,1],BaseJamHeight)
  
  BoulderClogging_width<-data.frame(matrix(Opening$BoulderLateralClogging, N_TimeSteps, N_opening,byrow=TRUE),stringsAsFactors=F)
  names(BoulderClogging_width)<-paste0("W",(1:N_opening))
  
  
  #Discharge elevation curves
  h<-seq(from=OpeningMinBaseLevel, to=max(storageElevationCurve$h)+5,by=0.1) #Extend 5 meter above the crest
  
  ###Plot V vs h
  if(PrintDataPlot){
    ggplot(storageElevationCurve)+
      geom_line(aes(x=s,y=h),col=2)+theme_bw(base_size = 9)+
      labs( x = "Storage capacity [*1000 m3]",y = "Flow level at barrier [m.a.s.l.]"
            ,title = paste("Stage - volume capacity"))+
      geom_hline(aes(yintercept = OpeningMinBaseLevel,lty="1"))+
      geom_hline(aes(yintercept = SpillwayLevel,lty="2"))+
      scale_linetype_manual("",labels=c("Opening base level","Spillway level"),values=c(2,3))+
      theme(legend.position = "top")
      ggsave("2Outputs/StageVolumeBarrier.png", width = 8, height = 6,units="cm")
  }
  #Print stage - discharge relationship
  if(PrintDataPlot){
    ggplot(data.frame(h=h[h<max(storageElevationCurve$h)]
                      #Capacity without any clogging
                      ,Q0=apply(Q_CompoundBarrier(Opening = Opening
                                                  ,BaseClogging = rep(0,length(Opening$Number))
                                                  ,WidthClogging=rep(0,length(Opening$Number))
                                                  ,h=h[h<max(storageElevationCurve$h)]
                      ),1,sum)
                      #Capacity with only initial clogging
                      ,Q=apply(Q_CompoundBarrier(Opening = Opening
                                                 ,BaseClogging = BoulderClogging_level[1,]
                                                 ,WidthClogging=BoulderClogging_width[1,]
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
      # ylim(min(storageElevationCurve$h),max(storageElevationCurve$h))+
      theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm"))
      
      ggsave("2Outputs/StageDischargeInitial.png", width = 8, height = 6,units="cm")
  }
  
  ################################
  #    COMPUTATION----
  ################################
  #Initialize a data.frame to record boulders generated
  Boulder_list_all<-data.frame(T=NULL,D=NULL,Class=NULL,Opening=NULL,Jammed=NULL)
  
  #Result dataset initialisation
  Reservoir<-data.frame(T=Qin$T
                        ,Qi=Qin$Q
                        ,Qo=0 #Qoutlet
                        ,V=NA#/10^3 #stored volume
                        ,Z=DepositDepthInitial #level at barrier 
                        ,Qoutlet=0 #Discharge passing by openings only
                        ,Qspillway=0) #Discharge passing by spillway only
  
  #First step
  i<-1
  Reservoir$V[i]<-approx(x=storageElevationCurve$h
                         ,y=storageElevationCurve$s
                         ,xout=DepositDepthInitial+OpeningMinBaseLevel
                         ,yleft=min(storageElevationCurve$s)
                         ,yright=max(storageElevationCurve$s))$y
  #For initial level we take the maximum between the pre-existing deposit height and the base jam height arriving at the surge front
  Reservoir$Z[i]<-OpeningMinBaseLevel+max(DepositDepthInitial,BaseJamHeight)+0.001 #+0.001 to avoid 0 values and null discharge
  #Sum the discharge of all orifices 
  Reservoir$Qo[i]<-sum(Q_CompoundBarrier(Opening = Opening
                                         ,BaseClogging = BoulderClogging_level[i,]
                                         ,WidthClogging = BoulderClogging_width[i,]
                                         ,h=Reservoir$Z[i])) 
  
  #Next steps
  i<-2
  #Actual computation step by step
  while(i<N_TimeSteps)
  { 
    #Initialize outlet discharge at step i
    Reservoir$Qo[i]<-sum(Q_CompoundBarrier(Opening = Opening
                                           ,BaseClogging = BoulderClogging_level[i,]
                                           ,WidthClogging = BoulderClogging_width[i,]
                                           ,h=Reservoir$Z[i]))
    
    #Initialize the reservoir level accuracy criteria
    dZ<-10^6
    Ninter<-0
    #Look for the values of reservoir level and outlet discharge that align volume conservation
    while(abs(dZ)>0.01*(CrestLevel-OpeningMinBaseLevel) && Ninter<1000) 
      #targeted reservoir level accuracy, 1% of the difference in elevation between the outlet and the spillway
    {   #Previous reservoir level recording
      Z<-Reservoir$Z[i]
      #Compute new reservoir volume by volume balance
      Reservoir$V[i]<-max(0,Reservoir$V[i-1]+TimeStep*(Reservoir$Qi[i]+Reservoir$Qi[i-1]-Reservoir$Qo[i]-Reservoir$Qo[i-1])/2/10^3)
      
      #Compute new reservoir level by interpolation on volume - elevation curve
      Reservoir$Z[i]<-approx(x=storageElevationCurve$s,y=storageElevationCurve$h,xout=Reservoir$V[i]
                             ,yleft=min(storageElevationCurve$h),yright=max(storageElevationCurve$h))$y
      
      #Update outlet discharge with new reservoir level
      Reservoir$Qo[i]<-sum(Q_CompoundBarrier(Opening = Opening
                                             ,BaseClogging = BoulderClogging_level[i,]
                                             ,WidthClogging = BoulderClogging_width[i,]
                                             ,h=Reservoir$Z[i]))
      #Update dZ
      dZ<-Z-Reservoir$Z[i]
      Ninter<-Ninter+1
    }
    if(Ninter>=1000) #If more than 1000 iteration does not enable to converge, we kill the run
    {print("One simulation killed (no convergence after 1000 iteration)")
      i<-N_TimeSteps
    }
    
    #Compute opening discharge, i.e., all but the last opening
    Reservoir$Qoutlet[i]<-sum(Q_CompoundBarrier(Opening = Opening[1:(N_opening-2),] #Top opening is the crest, before is the spillway
                                              ,BaseClogging = BoulderClogging_level[i,(1:(N_opening-2))]
                                              ,WidthClogging = BoulderClogging_width[i,(1:(N_opening-2))]
                                              ,h=Reservoir$Z[i]))
    #Compute spillway discharge, i.e., last opening
    Reservoir$Qspillway[i]<-as.numeric(Q_CompoundBarrier(Opening = Opening[(N_opening-1),] #Top opening is the crest, before is the spillway
                                                         ,BaseClogging = BoulderClogging_level[i,(N_opening-1)]
                                                         ,WidthClogging = BoulderClogging_width[i,(N_opening-1)]
                                                         ,h=Reservoir$Z[i]))
    
    
      #At each second, compute whether some clogging occur or not
      for(TimeStep.index in (1:round(TimeStep)))
      {
        #Initialize 
        Jam.Width<-0
        ####Clogging of the openings
        #Compute the volume of the surge passing each second  through each opening, 
        # i.e., instantaneous discharges
        Volume.Surge<-as.numeric(Q_CompoundBarrier(Opening = Opening
                                                   ,BaseClogging = BoulderClogging_level[i,]
                                                   ,WidthClogging = BoulderClogging_width[i,]
                                                   ,h=Reservoir$Z[i]))
        #Alternative: integrate the volume over the full time step: assume that all boulders arrive at the same time in one 
        # time step, unused because considered too conservative if the time step is long.
        # TimeStep*as.numeric(Q_CompoundBarrier(Opening = Opening,BaseClogging = BoulderClogging_level[i,],h=Reservoir$Z[i]))
        
        #Initialize the list of boulder passing through each opening
        for(Opening_Ind in (1:N_opening))
        { 
          VolumeSurge_inst<-Volume.Surge[Opening_Ind]
          #Check whether the transfer from upstream is instantaneous or mixing
          if(is.na(Qin$p1[i]))  #if probability is NA, then we must directly transfer the upstream number of boulders
          {
            Boulder_list <-BoulderSizing(Qin[i,(1:dim(Boulders)[1])+2+dim(Boulders)[1]])
          }else{
            #of the probability was not NA, then we randomly sample the boulder size
            Boulder_list<-BoulderPassing(VolumeSurge_inst
                                         ,Boulders$BoulderDiameter_min,Boulders$BoulderDiameter_max
                                         ,Boulder.probabilities=as.numeric(Qin[i,(1:dim(Boulders)[1])+2]))
            
          }
          
          if(!is.na(Boulder_list$D)[1])
          {
            Boulder_list<-data.frame(T=rep(Reservoir$T[i],length(Boulder_list$D))
                                     ,D=sort(Boulder_list$D,na.last=TRUE,decreasing =TRUE)
                                     ,Class=sort(Boulder_list$Class,na.last=TRUE,decreasing =TRUE)
                                     ,Opening=rep(Opening_Ind,length(Boulder_list$D))
                                     ,Jammed=rep("a) Unjammed",length(Boulder_list$D)))
            # data.frame(T=NULL,D=NULL,Opening=NULL,Jammed=NULL)
            # if(!is.na(Boulder_list)){print(Boulder_list$D)}
            
            #Full width clogging 
            #Sum the two biggest diameters
            Jam.Width<-sum(Boulder_list$D[1:2],na.rm = TRUE)
            if(Opening$Type[Opening_Ind]=="weir")
            {
              #Account for trappezoidal shape if weir
              Opening.Remaining.Width<-2/tan(Opening$Param[Opening_Ind]/180*pi)*BoulderClogging_level[i,Opening_Ind]+Opening$Width[Opening_Ind]-BoulderClogging_width[i,Opening_Ind]
            }else{
              #Otherwise, use opening width minus clogging
              Opening.Remaining.Width<-Opening$Width[Opening_Ind]-BoulderClogging_width[i,Opening_Ind]
            }
            
            #If the jam width is larger than the opening (eventually yet partially clogged), 
            # it is jammed and the jam is as high as the biggest boulder passing
            if(max(Jam.Width,0,na.rm = T)> Opening.Remaining.Width)
              {
              BoulderClogging_level[i:N_TimeSteps,Opening_Ind]<-BoulderClogging_level[i,Opening_Ind]+max(Boulder_list$D,na.rm = TRUE)
              # Record the boulders
              if(length(Boulder_list$Jammed)>1){Boulder_list$Jammed[1:2]<-"b) Laterally jammed"}else{Boulder_list$Jammed[1]<-"b) Laterally jammed"}
            }
            
            #Partial width clogging
            if(Opening$Type[Opening_Ind]=="slot")
            {
              #Record that the opening width clogging increase by the diameters of boulders that are bigger than slot height
              BoulderClogging_width[i:N_TimeSteps,Opening_Ind]<-min(Opening$Width[Opening_Ind]
                                                                     ,BoulderClogging_width[i-1,Opening_Ind]+sum(Boulder_list$D[which(Boulder_list$D>(Opening$Param[Opening_Ind]-Opening$BaseLevel[Opening_Ind]))]))
              
              # Record the boulders
              Boulder_list$Jammed[which(Boulder_list$Jammed!="b) Laterally jammed" & Boulder_list$D>(Opening$Param[Opening_Ind]-Opening$BaseLevel[Opening_Ind]))]<-"c) Vertically jammed"  
              
              #Clog the eventual residual small holes smaller than minimum boulder size (thus not clogged later)
              if(BoulderClogging_width[i,Opening_Ind]-Opening$Width[Opening_Ind] < min(Boulders$BoulderDiameter_min))
              {
                BoulderClogging_width[i:N_TimeSteps,Opening_Ind]<-Opening$Width[Opening_Ind]
              }
              if(BoulderClogging_level[i,Opening_Ind]-Opening$Param[Opening_Ind] < min(Boulders$BoulderDiameter_min))
              {
                BoulderClogging_level[i:N_TimeSteps,Opening_Ind]<-Opening$Param[Opening_Ind]
              }
            }
            Boulder_list_all<-rbind(Boulder_list_all,Boulder_list)
          }
        }#end of the opening loop
      }#end of the Timestep loop  
    #iterate on i to compute the next step
    i<-i+1
  }
  #Check for max flow level and whether it reached the available data
  if(max(Reservoir$Z)==max(storageElevationCurve$h))
    {
    print(paste0("Level reached the maximum storage elevation level of structure:"
  ,StructureName,".Consider providing storage volume at higher level if it is a barrier, otherwise it means that the highest bridge deck level was overtopped by more than 5 m, the buffering and discharge computed by the model are thus most probably wrong!"))
    }
  
    
  ################################
  #    RECORD THE TIME SERIES AND INDICATORS----
  ################################
   #Add the jamming state of the openings
  Reservoir<-cbind(Reservoir
                   ,BoulderClogging_level[(1:(N_opening-1))] #Only until N_opening-1 because clogging is a no sense on the crest
                   ,BoulderClogging_width[(1:(N_opening-1))])#Only until N_opening-1 because clogging is a no sense on the crest
  Reservoir$LevelClogging1<-Reservoir$Z1+OpeningMinBaseLevel
  
  
  #Add the number of boulders of each class to reservoir
  for(i in c(1:dim(Boulders)[1]))
  {
    Boulder_N_jammed<-Boulder_list_all %>% 
      filter(Class==i) %>%
      filter(Jammed!="a) Unjammed") %>%
      group_by(T) %>%
      summarise(N=n())
    
    Boulder_N_unjammed<-Boulder_list_all %>% 
      filter(Class==i) %>%
      filter(Jammed=="a) Unjammed") %>%
      group_by(T) %>%
      summarise(N=n())
    
    Reservoir$X<-Reservoir$Y<-0
    Reservoir$X[match(Boulder_N_jammed$T,Reservoir$T)]<-Boulder_N_jammed$N
    names(Reservoir)[which(names(Reservoir)=="X")]<-paste0("Class",i,".jammed")
    
    Reservoir$Y[match(Boulder_N_unjammed$T,Reservoir$T)]<-Boulder_N_unjammed$N
    names(Reservoir)[which(names(Reservoir)=="Y")]<-paste0("Class",i,".unjammed")
    
  }
  
  #Combine the tables of clogging for later plot and analysis
  N_slot<-(Opening$Type=="slot") #which opening are slots
  WidthClogging<-VerticalClogging<-NULL
  for(i in (1:(N_opening-1))) #Only until N_opening-1 because clogging is a no sense on the crest
  {
    if(i==1){ #Initialize
      VerticalClogging<-data.frame(T=Reservoir$T,Opening="#1"
                                   ,CloggingRate=BoulderClogging_level[,i]/(Opening$Param[i]-Opening$BaseLevel[i]))
      if(N_slot[i])
      {
        WidthClogging<-data.frame(T=Reservoir$T,Opening="#1"
                                  ,CloggingRate=BoulderClogging_width[,i]/Opening$Width[i])
      }
    }
    #Append
    VerticalClogging<-rbind(VerticalClogging,data.frame(T=Reservoir$T,Opening=paste0("#",i)
                                                        ,CloggingRate=BoulderClogging_level[,i]/(Opening$Param[i]-Opening$BaseLevel[i])))
    
    if((N_slot[i] && is.null(WidthClogging)))
    {#Initialize
      WidthClogging<-data.frame(T=Reservoir$T,Opening=paste0("#",i)
                                ,CloggingRate=BoulderClogging_width[,i]/Opening$Width[i])
    }else{
      if((N_slot[i] && !is.null(WidthClogging)))
      {#Append
        WidthClogging<-rbind(WidthClogging
                             ,data.frame(T=Reservoir$T,Opening=paste0("#",i)
                                         ,CloggingRate=BoulderClogging_width[,i]/Opening$Width[i]))
      }
    }
  }
  #Set vertical clogging = 100% if boulder height higher than slot height
  if(!is.null(VerticalClogging)){
    VerticalClogging$CloggingRate[VerticalClogging$CloggingRate>1]<-1  
  }
  #Same on horizontal clogging
  if(!is.null(WidthClogging)){
    WidthClogging$CloggingRate[WidthClogging$CloggingRate>1]<-1  
  }
  
  
  #remove last line which is not computed
  Reservoir<-Reservoir[-N_TimeSteps,]
 
  #Final clogging status
  BoulderZ_final<-BoulderClogging_level[(N_TimeSteps-1),]
  BoulderW_final<-BoulderClogging_width[(N_TimeSteps-1),]
  
  
  #update the new clogging status in the text fill to be used 
  # at the next run with the final clogging status
  Opening$BoulderVerticalClogging<-as.numeric(BoulderZ_final)
  Opening$BoulderLateralClogging<-as.numeric(BoulderW_final)
  if( KeepTrackOfCloggingState)
  {
    write.csv(Opening,file="./1Data/Opening.txt",row.names = FALSE)  
  }
  
  #Add virtual values to extend the legend of clogging from 0 to 100%
  VerticalClogging<-rbind(VerticalClogging,data.frame(T=c(-2,-1.5,-1)*10^3
                                                      ,Opening=rep(VerticalClogging$Opening[1],3)
                                                      ,CloggingRate=c(0,1,rep(VerticalClogging$CloggingRate[1],1))))
  if(Opening$Type[1]=="slot")
  {
    WidthClogging<-rbind(WidthClogging,data.frame(T=c(-2,-1.5,-1)*10^3
                                                  ,Opening=rep(WidthClogging$Opening[1],3)
                                                  ,CloggingRate=c(0,1,rep(WidthClogging$CloggingRate[1],1))))  
  }
  
  
  if(PrintFinalPlot){
    # Create directory to save recorded buffering results
    # dir.create(paste0("2Outputs/",EventName,"_ComputedOn",lubridate::today())
               # ,showWarnings = FALSE,recursive = TRUE)
    
    Plot_BufferingModel(ModelVersion,StructureName,Reservoir,WidthClogging,VerticalClogging
                        ,N_opening,storageElevationCurve,input[1]*1000
                        ,N_TimeSteps,Duration
                        ,OpeningMinBaseLevel,SpillwayLevel,CrestLevel
                        ,BoulderGenerationMode)
  }
  
  ###############END OF THE FUNCTION
  if(Ninter>=1000)
  {return(rep(NA,dim(Reservoir)[2]))}else{return(Reservoir)}
}


Synthetic_Structure_results<-function(Reservoir, Opening)
{  #Number of opening
  N_opening<-length(Opening$Number)
  N_TimeSteps<-length(Reservoir$T)
  
  #Extract the maximum level of the result data set.
  Zmax<-max(Reservoir$Z,na.rm = T)
  Zfinal<-Reservoir$Z[N_TimeSteps-1]
  
  #Extract the maximum Storage of the result data set.
  Vmax<-max(Reservoir$V,na.rm = T)
  Vfinal<-Reservoir$V[N_TimeSteps-1]
  
  
  #Record peak discharge and volume at inlet
  Qp_in<-max(Reservoir$Qi,na.rm=T)
  Vevent<-round(sum(Reservoir$Qi)*TimeStep/10^3,3)
  
  #Extract maximum outlet discharge
  Qp_out<-max(Reservoir$Qo,na.rm=T)
  
  #Look for time step of level passing over and below the crest
  # Nmin<-min(which(Reservoir$Z>Opening$BaseLevel[N_opening]))
  # Nmax<-max(which(Reservoir$Z>Opening$BaseLevel[N_opening]))
  
  #Look for time step of level passing over and below the spillway
  # Nmin.s<-min(which(Reservoir$Z>Opening$BaseLevel[(N_opening-1)]))
  # Nmax.s<-max(which(Reservoir$Z>Opening$BaseLevel[(N_opening-1)]))
  
  #OVertopping duration
  # Tover<-TimeStep*(Nmax-Nmin)
  # Tover.s<-TimeStep*(Nmax.s-Nmin.s)
  
  #Released volume = sum of released discharge * timestep in Mm3
  Vout<-round(sum(Reservoir$Qo)*TimeStep/10^3,3)
  #Part passing by the slit
  VoutSlit<-round(sum(Reservoir$Qoutlet)*TimeStep/10^3,3)
  #Part passing over the spillway
  VoutSpillway<-sum(Reservoir$Qspillway)*TimeStep/10^3
  #Remaning part passing over the Crest
  VoutCrest<-Vout-VoutSlit-VoutSpillway
  
  if(is.na(Reservoir$T[1]))
  {return(NA)}else
  {
    RESULTS<-data.frame("Qp_in"=Qp_in
                        ,"Vevent"=Vevent*1000
                        ,"Zmax"=Zmax
                        ,"Zfinal"=Zfinal
                        ,"Qp_out"=Qp_out
                        ,"Vmax"=Vmax*1000,"Vout"=Vout*1000,"Voutslit"=VoutSlit*1000
                        ,"Voutsplillway"=VoutSpillway*1000,"VoutCrest"=VoutCrest*1000
                        ,"Vfinal"=Vfinal*1000
                        # ,"Tover"=Tover,"Tover.spillway"=Tover.s
                        ,BoulderGenerationMode=BoulderGenerationMode
                        )
    RESULTS<-cbind(RESULTS
                   ,Reservoir[dim(Reservoir)[1],(7+1:(N_opening-1))]                    #only until N_opening -1 because
                   ,Reservoir[dim(Reservoir)[1],(7+(N_opening-1)+1:(N_opening-1))]      #the top opening is the crest
                   )
    return(RESULTS)
  }
}

Cascade_of_structure_functionning<-function(input)
{
  #Create input timesseries accordingly
  Qin<-Create_inlet_timeseries(input,Boulders)
  
  #Computation at each structure
  for(Structure_Ind in 1:length(Structures$Name))
  {
    #If not the first structure, then compute first the transferred inlet discharge and boulder
    if(Structure_Ind > 1)
    { 
      #record input 
      Qo_all_upstream<-Qo_all
      #Check type of transfert condition
      transfer <- Structures$TransferDownstream[[which(Structures$Rank==Structure_Ind-1)]]
      if(transfer == "Instantaneous")
      {
        Vmixing<-NA
      }else{
        Vmixing <- as.numeric(substr(transfer
                                     ,8
                                     ,nchar(transfer)))
      }
      #update the input accordingly
      Qin<-Transfer_Between_Structure(Qo = Qo_all_upstream %>% filter(Run == paste0("Run #",Run_Ind)) %>% filter(StructureRank == (Structure_Ind-1))
                                      ,Transfer.Type = transfer
                                      ,Vmixing = Vmixing)
    }
    
    #Compute the actual structure functionning
    Qo<-Structure_functionning(ModelVersion=ModelVersion
                               ,StructureName=Structures$Name[[which(Structures$Rank==Structure_Ind)]]
                               ,input=input,Qin=Qin
                               ,Opening=as.data.frame(Structures$Opening[[which(Structures$Rank==Structure_Ind)]])
                               ,StorageElevation=as.data.frame(Structures$StorageElevation[[which(Structures$Rank==Structure_Ind)]])
                                )
    
    Result<-Synthetic_Structure_results(Qo, Structures$Opening[[which(Structures$Rank==Structure_Ind)]])
    Result$StructureRank<-Structure_Ind
    
    #record the run ID (general variable) and structure name
    Qo$Run<-paste0("Run #",Run_Ind)
    Qo$StructureRank<-Structure_Ind
    
    #Record the run results
    if(Run_Ind == 1){
      Result_all<-Result
      Qo_all<-Qo
    }else{
      load(paste0("2Outputs/Result_Evt-",EventName,"_Structure_",Structures$Name[[which(Structures$Rank==Structure_Ind)]],".RData"))
      Result_all<-rbind(Result_all,Result)
      Qo_all<-rbind(Qo_all,Qo)
    }
    #Save a data frame with the main results of all runs as a .Rdata file
    save(Result_all,Qo_all,file=paste0("2Outputs/Result_Evt-",EventName,"_Structure_",Structures$Name[[which(Structures$Rank==Structure_Ind)]],".RData"))
    
  } #end of the for loop on structures
  
  #Return max outlet discharge at last structure
  return(max(Qo$Qo))
}
  