#' ---
#' title: "Structure Functioning"
#' output: html_document
#' date: "2024-03-22"
#' Author: G. Piton, C. Misset, H. Shirra
#' ---
#' This is an annotated version of the Structure_functionning.R script prepared by H. Shirra, originally developed by G. Piton and C. Misset (V0 July 2023) as part of a larger script to stochastically simulate jamming through a series of constrictions in a debris flow event. This code computes the passage of a debris flow through a structure using hydraulics and mass conservation equations, as well as jamming through openings. The results are fed into, Cascade_of_structure_functioning.R, and 00_MainCode.R. 
#' 
#' The script was originally inspired by BarrierBuffering_V0.3.R (https://github.com/GuillaumePitonInrae/CheekyeDebrisFlowBarrier/tree/main/0SourceCodes). The whole code and framework of analysis is described in Piton G, Goodwin SR, Mark E, Strouth A. 2022 (https://doi.org/10.1029/2021JF006447). Within this script, flow discharge through openings is defined according to Piton G, Recking A. 2016 (https://doi.org/10.1061/(ASCE)HY.1943-7900.0001048). Jamming of boulders is defined according to Piton G, Charvet G, Kuss D, Carladous S. 2020 (available from https://hal.archives-ouvertes.fr/hal-02701076).
#' 
#' ### Summary of Script
#' This sub-routine is the actual core function of the model where for a single structure, the hydraulic laws and mass conservation equations are resolved and the sub-routine randomly generating the boulders and computing the clogging are called at each time step.
#' 
#' Within the script, the initial deposit depth, and the height of any jamming at the base of the structures is imported from user-defined input. As well, the height of all openings within the structure is imported, and parsed based on the opening type. Storage-elevation curves are imported and interpolated. The initial deposition slope is imported and compared to the inputted storage-elevation data. If the user-defined slope is below the minimum possible slope defined by the storage-elevation curve, then the minimum slope from the storage-elevation curve is selected. The simulation progresses, but a warning message is displayed, prompting the user to update their input data.
#' 
#' For each structure within the cascade, the order of the openings is defined starting at the lowest elevation, increasing to the spillway, and then finally the crest (except in the case of dams, where the spillway is also the crest).The accuracy of the model regarding the flow level is typically 1% of the dam height (see details in code for structures with only one opening and no spillway). 
#' 
#' The number of boulders within an event is counted, and the average boulder diameter and volume is calculated for each class. The initial level of clogging is defined for each structure, using the pre-existing clogging data, and the base jam height. The data frame to track clogging as the simulation progresses is initialized. With all variables inputted and defined, computation is initiated.
#' 
#' The volume of the reservoir upstream of each structure is calculated at each time step by interpolating the storage capacity at the current flow level from the volume-elevation curve. Discharge and jamming is computed for each time step. At the end of each time step, clogging is recorded in the Reservoir data frame. The number of jammed and un-jammed boulders is recorded for each class. The results are plotted, and used to calculate the inflow hydrograph for the structure immediately downstream. The number of boulders within the event is updated based on the number of transferred boulders and upstream jamming. This process is repeated for the entire event volume.
#' 
#' Note: within the following code, an orifice is referred to as a "slot", and a weir with vertical side walls is referred to as a "slit". Before it is fully submerged, an orifice-like opening behaves as a weir, and is therefore considered a "slit".
#' 
#' ### Script
## ----comment= "#", echo=FALSE-------------------------------------------------
#Transfer the .Rmd file to R automatically when it is knit. 
#knitr::purl(input = "Structure_functionning.Rmd", documentation = 2) #Comment this line prior to running generated R script.

#' 
## -----------------------------------------------------------------------------
Structure_functionning<-function(ModelVersion,StructureName,input,Qin,Opening,StorageElevation)
{
  ################################
  #    OPEN INPUT DATA----
  ################################
  
  N_TimeSteps<-length(Qin[,1])
  #### Duration of the surge assuming a triangular hydrograph
  # Duration<-round(input[1]*10^3/(input[2]/2),0)
  Duration<-max(which(Qin$Q>(max(Qin$Q,na.rm=TRUE)/100)))
  
  #### Slope of deposition
  SlopeDep<-input[4]#
  
  
  StructureRank<-Structures$Rank[which(Structures$Name == StructureName)]
  #Initial deposition height
  DepositDepthInitial<-input[5+(StructureRank-1)*2] 
  if(is.na(DepositDepthInitial)){DepositDepthInitial<-0}
  #Base jam height 
  BaseJamHeight<-input[5+(StructureRank-1)*2+1] 
  if(is.na(BaseJamHeight)){BaseJamHeight<-0}
  
  #Extract the height of the openings
  Opening$Height<-Opening$TopLevel-Opening$BaseLevel
 
  #Interpolation of storage - elevation curve
  # StorageElevation<-read.csv("./1Data/ElevationStorageCurves.txt",sep="\t")
  storageElevationCurve<-data.frame(s=0,h=StorageElevation$Z)
  
  # Check that deposition slope is within the range of data
  if(min(as.numeric(substr(names(StorageElevation[,-1]),2,6)))>SlopeDep)#/100)
  {
    print(paste0("Deposition slope selected below the minimum values of the elevation - storage capacity! \n",
                 "Used the minimum value of the table but consider adding the relevant data to the table!"))
  }
  #Interpolation for the slope of deposition selected
  for (i in (1:length(StorageElevation[,1])))
  {
    storageElevationCurve$s[i]<-approx(x=as.numeric(substr(names(StorageElevation[,-1]),2,6))
                                       ,y=StorageElevation[i,-1]
                                       ,xout = SlopeDep#/100
                                       ,yleft=min(StorageElevation[i,-1])
                                       ,yright=max(StorageElevation[i,-1])
    )$y/10^3
  }
  # rm(StorageElevation)
  
  #Definition of openings
  #reorder Opening by increasing base level to have the spillway as last opening
  Opening<-Opening[order(Opening$BaseLevel),]
  
  #Number of opening
  N_opening<-length(Opening$Number)
  
  OpeningMinBaseLevel<-min(Opening$BaseLevel)
  #by default, the spillway should be the opening below the top one (top one is the crest)
  SpillwayLevel<-Opening$BaseLevel[max(N_opening-1,1)] 
  #But for dams with complex crest shape, one can write in Opening.txt which open is the 'Spillway' (Case sensitive test)
  if(sum(Opening$Comment=="Spillway")>0){
    SpillwayLevel<-Opening$BaseLevel[which(Opening$Comment=="Spillway")]
  }
  
  #Define model level accuracy for convergence, taken as 0.01*elevation difference between crest and base level
  #or 1% of opening width if no crest level available (single weir or slit)
  if(N_opening>1){
    CrestLevel<-Opening$BaseLevel[N_opening]
    ModelLevelAccuracy <- 0.01*(CrestLevel-min(Opening$BaseLevel))
  }else{
    if(Opening$Type == "slot"){
      ModelLevelAccuracy <- 0.01*(Opening$TopLevel[1]-Opening$BaseLevel)
    }else{
      if(Opening$Width > 0)
      {
        ModelLevelAccuracy <- 0.01*Opening$Width  
      }else{
        ModelLevelAccuracy <- 0.01 #1 cm accuracy, arbitrary
      }
    }
  }
  # print(paste0("The computation at structure: ",StructureName," is done with a level accuracy of ",ModelLevelAccuracy," m"))
  
  #Count number of boulders 
  for(i in (1:dim(Boulders)[1]))
  {
    Boulders$Number[i]<-input[(4+length(Structures$Name)*2)+i]
  }
  #Mean boulder size of each class used to compute the typucal volume of a boulder
  Boulders$Diameter<-0.5*(Boulders$Diameter_min+Boulders$Diameter_max)
  #Elementary volume of each boulder class
  Boulders$V<-pi/6*Boulders$Diameter^3
  #Clogging level and width by boulders
  # BoulderClogging_level<-data.frame(matrix(Opening$VerticalClogging, N_TimeSteps, N_opening,byrow=TRUE),stringsAsFactors=F) #Former version where we kept track of values
  # BoulderClogging_width<-data.frame(matrix(Opening$LateralClogging, N_TimeSteps, N_opening,byrow=TRUE),stringsAsFactors=F)#Former version where we kept track of values
  BoulderClogging_width<-BoulderClogging_level<-data.frame(matrix(0, N_TimeSteps, N_opening,byrow=TRUE),stringsAsFactors=F)
  names(BoulderClogging_level)<-paste0("Z",(1:N_opening))
  names(BoulderClogging_width)<-paste0("W",(1:N_opening))
  
  #For initial clogging we take the maximum between the pre-existing clogging height and the base jam height arriving at the surge front, which is computed as compared to the base level of the whole structure
  for(Opening_Ind in (1:N_opening))
  {
    BoulderClogging_level[,Opening_Ind] <- max(0,max(BoulderClogging_level[,Opening_Ind],BaseJamHeight) + OpeningMinBaseLevel - Opening$BaseLevel[Opening_Ind])
  } 
  
  #Discharge elevation curves
  h<-seq(from=OpeningMinBaseLevel, to=max(storageElevationCurve$h)+5,by=5*ModelLevelAccuracy) #Extend 5 meter above the crest
  
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
    ggsave("StageVolumeBarrier.png", width = 8, height = 6,units="cm")
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
    
    ggsave("StageDischargeInitial.png", width = 8, height = 6,units="cm")
  }
  
  ################################
  #    COMPUTATION----
  ################################
  #Initialize a data.frame to record boulders generated
  Boulder_list_all<-data.frame(Time=NULL,D=NULL,Class=NULL,Opening=NULL,Jammed=NULL)
  
  #Result dataset initialisation
  Reservoir<-data.frame(Time=Qin$Time
                        ,Qi=Qin$Q
                        ,Qo=0 #Qoutlet
                        ,V=NA#/10^3 #stored volume
                        ,Z=OpeningMinBaseLevel+DepositDepthInitial #level at barrier 
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
  Reservoir$Z[i]<-OpeningMinBaseLevel+max(DepositDepthInitial,BaseJamHeight)+0.0001 #+0.0001 to avoid 0 values and null discharge
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
    while(abs(dZ)>ModelLevelAccuracy && Ninter<1000) 
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
    {
      print(paste("One simulation killed (no convergence after 1000 iteration) at time step number",i))
      i<-N_TimeSteps
    }
    
    if(N_opening>2)#case with opening(s) + spillway + crest
    {
      #Compute opening discharge, i.e., all but the last opening
      Reservoir$Qoutlet[i]<-sum(Q_CompoundBarrier(Opening = Opening[1:(N_opening - 2),] #Top opening is the crest, before is the spillway
                                                  ,BaseClogging = BoulderClogging_level[i,(1:(N_opening - 2))]
                                                  ,WidthClogging = BoulderClogging_width[i,(1:(N_opening - 2))]
                                                  ,h=Reservoir$Z[i]))
      #Compute spillway discharge, i.e., opening below the crest
      Reservoir$Qspillway[i]<-as.numeric(Q_CompoundBarrier(Opening = Opening[(N_opening-1),] #Top opening is the crest, before is the spillway
                                                           ,BaseClogging = BoulderClogging_level[i,(N_opening-1)]
                                                           ,WidthClogging = BoulderClogging_width[i,(N_opening-1)]
                                                           ,h=Reservoir$Z[i]))  
    }else{
      if(N_opening == 2)#case with one opening + crest
      {
        #Compute opening discharge, i.e., all but the last opening
        Reservoir$Qoutlet[i]<-sum(Q_CompoundBarrier(Opening = Opening[1:(N_opening - 1),] #Top opening is the crest, before is the spillway
                                                    ,BaseClogging = BoulderClogging_level[i,(1:(N_opening - 1))]
                                                    ,WidthClogging = BoulderClogging_width[i,(1:(N_opening - 1))]
                                                    ,h=Reservoir$Z[i]))
        #Compute spillway discharge, i.e., last opening
        Reservoir$Qspillway[i]<-as.numeric(Q_CompoundBarrier(Opening = Opening[N_opening,] #Top opening is the crest, before is the spillway
                                                             ,BaseClogging = BoulderClogging_level[i,N_opening]
                                                             ,WidthClogging = BoulderClogging_width[i,N_opening]
                                                             ,h=Reservoir$Z[i]))
      }else{#case with one opening only
        Reservoir$Qoutlet[i]<-Reservoir$Qo[i]
        Reservoir$Qspillway[i]<-0
      }
      
    }
    
    
    
    #At each second, compute whether some clogging occur or not
    for(TimeStep.index in (1:round(TimeStep)))
    {
      #Initialize 
      JamWidth<-0
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
      
      #If instantaneous transfer, record the number of boulders arriving at the structure
      if(is.na(Qin$p1[i]))  #if probability is NA, then we must directly transfer the upstream number of boulders
      {
        #Count the number of boulders arriving
        Boulder_list_wholeStructure <-BoulderSizing(Qin[i,(1:dim(Boulders)[1])+2+dim(Boulders)[1]])
        
        if(!is.na( Boulder_list_wholeStructure$D[1]))
        {
          # if(sum(Volume.Surge == 0))
          # {
          #   Boulder_list<-data.frame(Time=rep(Reservoir$Time[i],length(Boulder_list$D))
          #                            ,D=sort(Boulder_list_wholeStructure$D,na.last=TRUE,decreasing =TRUE)
          #                            ,Class=sort(Boulder_list_wholeStructure$Class,na.last=TRUE,decreasing =TRUE)
          #                            ,Opening=rep(NA,length(Boulder_list$D))
          #                            ,Jammed=rep("d) jammed behind other boulders",length(Boulder_list$D)))
          # }
          # Compute the relative discharge passing in each Opening
          Volume.Surge.loc<-Volume.Surge
          Volume.Surge.loc[Volume.Surge.loc<sum(Volume.Surge.loc)/10^4] <- sum(Volume.Surge.loc)/10^4
          #Assign a opening number using a random computation with the probability of each opening being proportional
          # to the discharge passing through the said opening
          if(sum(Volume.Surge.loc)>0)
          {
            Boulder_list_wholeStructure$Opening<-floor(approx(c(0,cumsum(Volume.Surge.loc)/sum(Volume.Surge.loc)),
                                                              c(1:N_opening,N_opening),
                                                              runif(dim(Boulder_list_wholeStructure)[1]))$y)
          }
          rm(Volume.Surge.loc)
        }
      }
      
      #Initialize the list of boulder passing through each opening
      for(Opening_Ind in (1:N_opening))
      { 
        VolumeSurge_inst<-Volume.Surge[Opening_Ind]
        #Check whether the transfer from upstream is instantaneous or mixing
        if(is.na(Qin$p1[i]))  #if probability is NA, then we must directly transfer the upstream number of boulders
        {
          if(sum(Boulder_list_wholeStructure$Opening==Opening_Ind)>0 & VolumeSurge_inst > 0)#(!is.na( Boulder_list_wholeStructure$D[1]))#(length(Boulder_list_wholeStructure$D)>0)
          {
            Boulder_list <-Boulder_list_wholeStructure[Boulder_list_wholeStructure$Opening==Opening_Ind,]
            }else{Boulder_list<-data.frame(D=NA,Class=NA)}
          
        }else{
          #of the probability was not NA, then we randomly sample the boulder size
          Boulder_list<-BoulderPassing(VolumeSurge_inst
                                       ,Boulders$Diameter_min,Boulders$Diameter_max
                                       ,Boulder.probabilities=as.numeric(Qin[i,(1:dim(Boulders)[1])+2]))
        }
        
        if(!is.na(Boulder_list$D)[1])
        {
          Boulder_list<-data.frame(Time=rep(Reservoir$Time[i],length(Boulder_list$D))
                                   ,D=sort(Boulder_list$D,na.last=TRUE,decreasing =TRUE)
                                   ,Class=sort(Boulder_list$Class,na.last=TRUE,decreasing =TRUE)
                                   ,Opening=rep(Opening_Ind,length(Boulder_list$D))
                                   ,Jammed=rep("a) Unjammed",length(Boulder_list$D)))
          # data.frame(Time=NULL,D=NULL,Opening=NULL,Jammed=NULL)
          # if(!is.na(Boulder_list)){print(Boulder_list$D)}
          
          #Full width clogging, named horizontal clogging in the original paper 
          #Sum the two biggest diameters
          JamWidth<-sum(Boulder_list$D[1:2],na.rm = TRUE)
          #compute the re;aining opening width
          if(Opening$Type[Opening_Ind]=="weir")
          {
            #Account for trappezoidal shape if weir
            OpeningRemainingWidth <- 2 / tan(Opening$SideAngle[Opening_Ind] / 180 * pi) * BoulderClogging_level[i,Opening_Ind] + Opening$Width[Opening_Ind] - BoulderClogging_width[i,Opening_Ind]
          }else{
            #Otherwise, use opening width minus clogging
            OpeningRemainingWidth <- Opening$Width[Opening_Ind] - BoulderClogging_width[i,Opening_Ind]
          }
          
          #If the jam width is larger than the opening (eventually yet partially clogged), 
          # it is jammed and the jam is as high as the biggest boulder passing
          if(max(JamWidth,0,na.rm = T) > OpeningRemainingWidth & (Opening$BaseLevel[Opening_Ind] + BoulderClogging_level[i,Opening_Ind]) < Opening$TopLevel[Opening_Ind] )
          {
            BoulderClogging_level[i:N_TimeSteps,Opening_Ind] <- BoulderClogging_level[i,Opening_Ind] + max(Boulder_list$D,na.rm = TRUE)
            # Record the boulders
            if(length(Boulder_list$Jammed)>1){Boulder_list$Jammed[1:2]<-"b) Laterally jammed"}else{Boulder_list$Jammed[1]<-"b) Laterally jammed"}
          }
          
          #Partial width clogging, named vertical clogging in the original paper
          if(Opening$Type[Opening_Ind]=="slot")
          {
            #Record that the opening width clogging increase by the diameters of boulders that are bigger than slot height
            BoulderClogging_width[i:N_TimeSteps,Opening_Ind]<-min(Opening$Width[Opening_Ind]
                                                                  ,BoulderClogging_width[i-1,Opening_Ind] + sum(Boulder_list$D[which(Boulder_list$D > (Opening$Height[Opening_Ind]))]))
            
            # Record the boulders
            Boulder_list$Jammed[which(Boulder_list$Jammed!="b) Laterally jammed" & Boulder_list$D>(Opening$Height[Opening_Ind]))]<-"c) Vertically jammed"  
            
            #Clog the eventual residual small holes smaller than minimum boulder size (thus not clogged later)
            if(Opening$Width[Opening_Ind] - BoulderClogging_width[i,Opening_Ind] < min(Boulders$Diameter_min))
            {
              BoulderClogging_width[i:N_TimeSteps,Opening_Ind]<-Opening$Width[Opening_Ind]
            }
            # if remaining space is smaller than the smallest boulder size, we will never compute its clogging because such small boulders
            # are not randomly generated. We thus immediately clogg it assuming many such small boulders are present in the debris flow
            if(Opening$Height[Opening_Ind] - BoulderClogging_level[i,Opening_Ind] < min(Boulders$Diameter_min))
            {
              BoulderClogging_level[i:N_TimeSteps,Opening_Ind]<-Opening$Height[Opening_Ind]
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
                 ,StructureName))
          print(". Consider providing storage volume at higher level if it is a barrier, otherwise it means that the highest bridge deck level was overtopped by more than 5 m, the buffering and discharge computed by the model are thus most probably wrong!")
  }
  
  
  ################################
  #    RECORD THE TIME SERIES AND INDICATORS----
  ################################
  #Add the jamming state of the openings
  Reservoir<-cbind(Reservoir
                   ,BoulderClogging_level[(1:max(N_opening-1,1))] #Only until N_opening-1 because clogging is a no sense on the crest
                   ,BoulderClogging_width[(1:max(N_opening-1,1))])#Only until N_opening-1 because clogging is a no sense on the crest
  Reservoir$BaseLevelJam<-Reservoir$Z1+OpeningMinBaseLevel
  
  #Add the number of boulders of each class to reservoir
  #With a for loop
  for(i in c(1:dim(Boulders)[1]))
  {
    Reservoir$X<-Reservoir$Y<-0

    Boulder_N_jammed<-table(Boulder_list_all[(Boulder_list_all$Class == i & Boulder_list_all$Jammed != "a) Unjammed"),1])
    Boulder_N_unjammed<-table(Boulder_list_all[(Boulder_list_all$Class == i & Boulder_list_all$Jammed == "a) Unjammed"),1])

    Reservoir$X[match(names(Boulder_N_jammed),Reservoir$Time)]<-Boulder_N_jammed
    Reservoir$Y[match(names(Boulder_N_unjammed),Reservoir$Time)]<-Boulder_N_unjammed

    names(Reservoir)[which(names(Reservoir)=="X")]<-paste0("Class",i,".jammed")
    names(Reservoir)[which(names(Reservoir)=="Y")]<-paste0("Class",i,".unjammed")
  }

  #with DPLYR
  # for(i in c(1:dim(Boulders)[1]))
  # {
  # 
  #   Boulder_N_jammed<-Boulder_list_all %>%
  #     filter(Class == i) %>%
  #     filter(Jammed != "a) Unjammed") %>%
  #     group_by(Time) %>%
  #     summarise(N=n())
  # 
  #   Boulder_N_unjammed<-Boulder_list_all %>%
  #     filter(Class == i) %>%
  #     filter(Jammed == "a) Unjammed") %>%
  #     group_by(Time) %>%
  #     summarise(N=n())
  # 
  #   Reservoir$X<-Reservoir$Y<-0
  #   Reservoir$X[match(Boulder_N_jammed$Time,Reservoir$Time)]<-Boulder_N_jammed$N
  #   names(Reservoir)[which(names(Reservoir)=="X")]<-paste0("Class",i,".jammed")
  # 
  #   Reservoir$Y[match(Boulder_N_unjammed$Time,Reservoir$Time)]<-Boulder_N_unjammed$N
  #   names(Reservoir)[which(names(Reservoir)=="Y")]<-paste0("Class",i,".unjammed")
  # 
  # }

  #Combine the tables of clogging for later plot and analysis
  N_slot<-(Opening$Type=="slot") #which opening are slots
  WidthClogging<-VerticalClogging<-NULL
  for(i in (1:max(1,(N_opening-1)))) #Only until N_opening-1 because clogging is a no sense on the crest
  {
    if(i==1){ #Initialize
      VerticalClogging<-data.frame(Time=Reservoir$Time,Opening="#1"
                                   ,CloggingRate=BoulderClogging_level[,i]/Opening$Height[i])
      if(N_slot[i])
      {
        WidthClogging<-data.frame(Time=Reservoir$Time,Opening="#1"
                                  ,CloggingRate=BoulderClogging_width[,i]/Opening$Width[i])
      }
    }
    #Append
    VerticalClogging<-rbind(VerticalClogging,data.frame(Time=Reservoir$Time,Opening=paste0("#",i)
                                                        ,CloggingRate=BoulderClogging_level[,i]/Opening$Height[i]))
    
    if((N_slot[i] && is.null(WidthClogging)))
    {#Initialize
      WidthClogging<-data.frame(Time=Reservoir$Time,Opening=paste0("#",i)
                                ,CloggingRate=BoulderClogging_width[,i]/Opening$Width[i])
    }else{
      if((N_slot[i] && !is.null(WidthClogging)))
      {#Append
        WidthClogging<-rbind(WidthClogging
                             ,data.frame(Time=Reservoir$Time,Opening=paste0("#",i)
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
  Opening$VerticalClogging<-as.numeric(BoulderZ_final)
  Opening$LateralClogging<-as.numeric(BoulderW_final)
  if( KeepTrackOfCloggingState)
  {
    write.csv(Opening,file="./1Data/Opening.txt",row.names = FALSE)  
  }
  
  #Add virtual values to extend the legend of clogging from 0 to 100%
  VerticalClogging<-rbind(VerticalClogging,data.frame(Time=c(-2,-1.5,-1)*10^3
                                                      ,Opening=rep(VerticalClogging$Opening[1],3)
                                                      ,CloggingRate=c(0,1,rep(VerticalClogging$CloggingRate[1],1))))
  if(Opening$Type[1]=="slot")
  {
    WidthClogging<-rbind(WidthClogging,data.frame(Time=c(-2,-1.5,-1)*10^3
                                                  ,Opening=rep(WidthClogging$Opening[1],3)
                                                  ,CloggingRate=c(0,1,rep(WidthClogging$CloggingRate[1],1))))  
  }
  #Add a virtual value of negative time to extend the plotting of the initial condition before the event
  Reservoir$Time[1]<-(-3600)
  
  
  if(PrintFinalPlot){
    # Create directory to save recorded buffering results
    # dir.create(paste0("",EventName,"_ComputedOn",lubridate::today())
    # ,showWarnings = FALSE,recursive = TRUE)
    
    Plot_BufferingModel(ModelVersion,StructureName,Reservoir,WidthClogging,VerticalClogging
                        ,N_opening,storageElevationCurve,input[1]*1000
                        ,N_TimeSteps,Duration
                        ,OpeningMinBaseLevel,SpillwayLevel,CrestLevel
                        ,BoulderGenerationMode)
  }
  
  #remove a virtual value of negative time to extend the plotting of the initial condition before the event
  Reservoir$Time[1]<-1
  
  ###############END OF THE FUNCTION
  if(Ninter>=1000)
  {return(rep(NA,dim(Reservoir)[2]))}else{return(Reservoir)}
}


Synthetic_Structure_results<-function(Qo, Opening)
{  #Number of opening
  N_opening<-length(Opening$Number)
  N_TimeSteps<-length(Qo$Time)
  
  #Extract the maximum level of the result data set.
  Zmax<-max(Qo$Z,na.rm = T)
  Zfinal<-Qo$Z[N_TimeSteps-1]
  
  #Extract the maximum Storage of the result data set.
  Vmax<-max(Qo$V,na.rm = T)
  Vfinal<-Qo$V[N_TimeSteps-1]
  
  
  #Record peak discharge and volume at inlet
  Qp_in<-max(Qo$Qi,na.rm=T)
  Vevent<-round(sum(Qo$Qi)*TimeStep/10^3,3)
  
  #Extract maximum outlet discharge
  Qp_out<-max(Qo$Qo,na.rm=T)
  
  #Look for time step of level passing over and below the crest
  # Nmin<-min(which(Qo$Z>Opening$BaseLevel[N_opening]))
  # Nmax<-max(which(Qo$Z>Opening$BaseLevel[N_opening]))
  
  #Look for time step of level passing over and below the spillway
  # Nmin.s<-min(which(Qo$Z>Opening$BaseLevel[max(N_opening-1,1)]))
  # Nmax.s<-max(which(Qo$Z>Opening$BaseLevel[max(N_opening-1,1)]))
  
  #OVertopping duration
  # Tover<-TimeStep*(Nmax-Nmin)
  # Tover.s<-TimeStep*(Nmax.s-Nmin.s)
  
  #Released volume = sum of released discharge * timestep in Mm3
  Vout<-round(sum(Qo$Qo)*TimeStep/10^3,3)
  #Part passing by the slit
  VoutSlit<-round(sum(Qo$Qoutlet)*TimeStep/10^3,3)
  #Part passing over the spillway
  VoutSpillway<-sum(Qo$Qspillway)*TimeStep/10^3
  #Remaning part passing over the Crest
  VoutCrest<-Vout-VoutSlit-VoutSpillway
  
  if(is.na(Qo$Time[1]))
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
                   ,Qo[dim(Qo)[1],(7+1:max(N_opening-1,1))]                    #only until N_opening -1 because
                   ,Qo[dim(Qo)[1],(7+max(N_opening-1,1)+1:max(N_opening-1,1))]      #the top opening is the crest
    )
    names(RESULTS)[(12+1:max(N_opening-1,1))]<-names(Qo)[(7+1:max(N_opening-1,1))]
    names(RESULTS)[(12+max(N_opening-1,1))+1:max(N_opening-1,1)]<-names(Qo)[(7+max(N_opening-1,1)+1:max(N_opening-1,1))]
    return(RESULTS)
  }
}


