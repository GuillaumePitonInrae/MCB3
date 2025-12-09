# Plot results of a simulation with the buffering model
# G. PITON, and H. SHIRRA May 2024

Plot_BufferingModel<-function(ModelVersion,StructureName
                              ,Reservoir,WidthClogging,VerticalClogging
                              ,Boulder_list_all
                              ,N_opening
                              ,storageElevationCurve,Volume
                              ,N_TimeSteps,Duration
                              ,OpeningMinBaseLevel,SpillwayLevel,CrestLevel
                              ,BoulderGenerationMode)
{
  ################################
  #    SYNTHESIS PLOT----
  ################################
  Qplot<-ggplot(Reservoir,aes(x=Time/3600))+theme_bw(base_size = 9)+
    geom_line(aes(y=Qi,colour="Inlet"),lwd=1.2)+
    geom_line(aes(y=Qo,colour="Outlet"),lwd=1.5)+
    theme(legend.position = "top")+
    scale_colour_grey(name="Discharge")+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank())+
    # annotate(geom = "text", x = 1*Duration/3600, y = max(Reservoir$Qi)*0.5
    #          ,vjust=1,hjust=1
    #          , label = paste0('Boulder generation mode: ',BoulderGenerationMode)
    #          ,colour="grey50")+
    annotate(geom = "text", x = 1*Duration/3600, y = max(Reservoir$Qi)
             ,vjust=1,hjust=1
             , label = paste("Max discharge released =",round(max(Reservoir$Qo),0)
                             ,"m3/s | Buffering:",round((1-max(Reservoir$Qo)/max(Reservoir$Qi))*100,0),"%")
    )+
    coord_cartesian(xlim = c(0,1*Duration/3600))+
    labs(y = "Discharge [m3/s]"#caption=paste("(a)") ,
         # ,subtitle = paste("Max discharge released =",round(max(Reservoir$Qo),0)
         #               ,"m3/s | Buffering:",round((1-max(Reservoir$Qo)/max(Reservoir$Qi))*100,0),"%")
    )+
    theme(legend.margin = margin(0.1,0.1,0.1,0.1, "cm"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),  # Remove text from x-axis
          axis.title.x = element_blank(),
          axis.ticks = element_line()) + 
    
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))
  
  if(!is.null(WidthClogging))
  {
    WclogPlot<-ggplot(WidthClogging,aes(x=Time/3600,y=Opening))+theme_bw(base_size = 9)+
      geom_line(aes(col=CloggingRate*100),lwd=7/N_opening*4)+
      scale_color_continuous(name="Rate of clogging in slots (Boulders higher than orifice)"
                             ,breaks = c(0, 50,100),labels=c("0%","50%","100%"),palette = "Greys")+
      theme(legend.position = "top",legend.margin = margin(0.1,0.1,0.1,0.1, "cm")
            ,axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank()
            ,legend.key.height = unit(15, 'pt')
      )+
      coord_cartesian(xlim = c(0,1*Duration/3600))+
      labs(y = "Opening")
  }
  
  VclogPlot<-ggplot(VerticalClogging,aes(x=Time/3600,y=Opening))+theme_bw(base_size = 9)+
    geom_line(aes(col=CloggingRate*100),lwd=7/N_opening*4)+
    scale_color_continuous(name="Rate of vertical clogging (Boulders jamming laterally)"
                           , breaks = c(0, 50,100),labels=c("0%","50%","100%"),palette = "Greys")+
    theme(legend.position = "top",legend.margin = margin(0.1,0.1,0.1,0.1, "cm")
          ,axis.line=element_blank()
          ,axis.text.x=element_blank(),axis.title.x=element_blank()
          ,legend.key.height = unit(15, 'pt')
    )+
    coord_cartesian(xlim = c(0,1*Duration/3600))+
    labs(y = "Opening")
  
  Zplot<-ggplot(Reservoir,aes(x=Time/3600))+
    theme_bw(base_size = 9)+ 
    geom_hline(aes(yintercept = OpeningMinBaseLevel,colour="1",lty="1"))+
    geom_hline(aes(yintercept = SpillwayLevel,colour="2",lty="2"))+
    geom_hline(aes(yintercept = CrestLevel,colour="3",lty="3"))+
    geom_line(aes(x=Time/3600,y=Z,colour="4",lty="4"),lwd=1)+
    geom_line(aes(x=Time/3600,y=BaseLevelJam,colour="5",lty="5"))+
    geom_ribbon(aes(x=Time/3600,ymax=BaseLevelJam,ymin=OpeningMinBaseLevel)
                ,lwd=1,col="transparent",alpha=0.2)+
    theme(legend.position = "top")+
    scale_colour_grey(name="Level",label=c("Opening base","Spillway","Crest","Flow","Boulder jam at opening #1"))+
    scale_linetype_manual(name="Level",label=c("Opening base"
                                               ,"Spillway"
                                               ,"Crest"
                                               ,"Flow"
                                               ,"Boulder jam at opening #1")
                          ,values=c(2,2,3,1,4))+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank())+
    labs( x = " ",y = "Flow level [m]")+
    annotate(geom = "text", x = 1*Duration/3600, y = max(storageElevationCurve$h,na.rm=TRUE)
             ,vjust=1,hjust=1
             , label = paste("Max level reached =",round(max(Reservoir$Z),1),"m.a.s.l.")
    )+
    coord_cartesian(xlim = c(0,1*Duration/3600),ylim=c(OpeningMinBaseLevel,max(storageElevationCurve$h,na.rm=TRUE)))+
    labs( x = "Time [h]",y = "Flow level [m]"
          # ,caption=paste("(d)")
          # ,subtitle = paste("Max level at barrier =",round(max(Reservoir$Z),1),"m.a.s.l.")
    )+
    # coord_cartesian(xlim = c(0, max(Reservoir$Time) / 3600), ylim = c(OpeningMinBaseLevel, max(storageElevationCurve$h, na.rm = TRUE))) +
    theme(legend.margin = margin(0.1,0.1,0.1,0.1, "cm")
          # ,plot.margin = margin(0.1,0.1,0.1,0.1, "cm")
    )
  
  Vplot<-ggplot(Reservoir)+theme_bw(base_size = 9)+
    geom_area(aes(x=Time/3600,y=V),lwd=1,col="black",alpha=0.9)+
    coord_cartesian(xlim = c(0,1*Duration/3600),ylim=c(0,Volume/1000)*1.15)+
    annotate(geom = "text", x = 1*Duration/3600, y = Volume/1000*1.15
             ,vjust=1,hjust=1
             , label = paste("Surge:",round(Volume/10^6,2),"Mm3"
                             ,"| Trapped:",round(Reservoir$V[(N_TimeSteps-1)],0)/1000,"Mm3"
                             ,"| Released:",round(Volume/10^6-Reservoir$V[(N_TimeSteps-1)]/1000,3),"Mm3"
                             ,"| Buffering:",round(100-(Volume/10^6-Reservoir$V[(N_TimeSteps-1)]/1000)/(Volume/10^6)*100,0),"%")
    )+ 
    labs( x = "",y = "Volume [*1000m3]" 
    )+
    theme(panel.border = element_rect(color = "black", fill = NA),
          line= element_line(color = "black"), legend.margin = margin(0.1,0.1,0.1,0.1, "cm")
          ,axis.text.x=element_blank(),axis.title.x=element_blank())
  
  # #Boulder passing plot
  # Define variables
  BoulderPlot<-data.frame(Number_Passing=NA, Number_Total = NA, Class_boulder=NA,time=Boulder_list_all$Time/3600)  #initialize dataframe
  # Loop over each row of the Boulder_list_all dataframe
  for (i in 1:nrow(Boulder_list_all)) {
    
    BoulderPlot[i, "Class_boulder"] <- paste0(Boulders$Diameter_min[Boulder_list_all$Class[i]]
                                              ,"-"
                                              ,Boulders$Diameter_max[Boulder_list_all$Class[i]]
                                              ,"m")
    BoulderPlot[i, "Number_Total"] <- 1
    # Check if the boulder is "unjammed"
    if (Boulder_list_all$Jammed[i] == "a) Unjammed") {
      # Record 1 under Number_Passing and the class
      BoulderPlot[i, "Number_Passing"] <- 1
    }
  }
  # 
  # ###Boulder Plot
  # # Create the bar chart
  BoulderPlot$Class_boulder <- factor(BoulderPlot$Class_boulder)
  BoulderPlot$Number_Passing[is.na(BoulderPlot$Number_Passing)]<-0
  # BoulderPlot_filtered <- BoulderPlot[complete.cases(BoulderPlot), ]
  
  Bplot<-
    ggplot(BoulderPlot, aes(x = time, fill = Class_boulder)) +theme_bw(base_size = 9)+
    geom_bar(aes(y=-(Number_Total-Number_Passing)),stat = "identity", width=1/Duration/10) +
    geom_bar(aes(y = Number_Passing),stat = "identity", width=1/Duration/10) +
    geom_text(x=Duration/3600,y=0,vjust=-0.5,hjust=1
              ,label=paste0("Boulders passing downstream (>0)"))+
    geom_text(x=Duration/3600,y=0,vjust=1.5,hjust=1
              ,label=paste0(" Boulders jamming the opening (<0) "))+
    geom_text(x=Duration/3600,y=-0.5,vjust=1.5,hjust=1
              ,label=paste0("Note: Boulders trapped in the basin are not counted"))+
    geom_hline(yintercept = 0)+
    scale_fill_viridis_d(direction=-1) +  # Use the colorblind-friendly viridis color palette
    labs(x = "Time [h]", y = "# Boulders ", fill="Boulder Class") +
    coord_cartesian(xlim = c(0,1*Duration/3600))+
    theme(legend.position = "bottom")
  
  
  
  #Plot name definition with date and hour
  Plot.Name<-paste0("TimeSeriesOfEvent_",EventName
                    ,"_forStructure_",StructureName
                    ,"-computedOn",lubridate::now())
  Plot.Name<-str_replace_all(Plot.Name, "[^[:alnum:]]", "_")
  Plot.Name<-paste0(Plot.Name,".png")
  
  png(Plot.Name, width = 17, height = 28,units="cm",res=350)
  {
    pushViewport(viewport(layout = grid.layout(28,1) ) )
    # define layout
    define_region <- function(row, col){
      viewport(layout.pos.row = row, layout.pos.col = col)
    }
    # Arrange graphs
    if(is.null(WidthClogging))
    {
      print(Qplot+labs(title=paste0("Modelling of structure: ",StructureName,", for event: ",EventName,"\n"
                                    ,"Model version: ",ModelVersion,"\n"
                                    ,"Input parameters: ",BoulderGenerationMode))+ 
              theme(plot.title = element_text(size=8.5))
            , vp = define_region(1:6,1))
      print(VclogPlot, vp = define_region(7:10,1))
      print(Vplot, vp = define_region(11:15,1))
      print(Zplot, vp = define_region(16:22,1))
      print(Bplot, vp = define_region(23:28, 1))
      
    }else{
      print(Qplot+labs(title=paste0("Modelling of structure: ",StructureName,", for event: ",EventName,"\n"
                                    ,"Model version: ",ModelVersion,"\n"
                                    ,"Input paramters: ",BoulderGenerationMode))
            , vp = define_region(1:5,1))
      print(VclogPlot, vp = define_region(6:9,1))
      print(WclogPlot, vp = define_region(10:13,1))
      print(Vplot, vp = define_region(14:16,1))
      print(Zplot, vp = define_region(17:22,1))
      print(Bplot, vp = define_region(23:28, 1))
    }
  }
  dev.off()
}


# For tests
# Plot_BufferingModel(ModelVersion,StructureName
#                     ,Reservoir
#                     ,WidthClogging,VerticalClogging
#                     ,Boulder_list_all
#                     ,N_opening,storageElevationCurve,input[[1]]*1000
#                     ,N_TimeSteps,Duration
#                     ,OpeningMinBaseLevel,SpillwayLevel,CrestLevel
#                     ,BoulderGenerationMode)
