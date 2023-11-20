# Plot results of a simulation with the buffering model
# G. PITON, May 2021

Plot_BufferingModel<-function(ModelVersion,StructureName
                              ,Reservoir,WidthClogging,VerticalClogging
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
    theme(legend.margin = unit(c(0.1,0.1,0.1,0.1), "cm")
          # ,plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")
          )
    
  if(!is.null(WidthClogging))
  {
      WclogPlot<-ggplot(WidthClogging,aes(x=Time/3600,y=Opening))+theme_bw(base_size = 9)+
        geom_line(aes(alpha=CloggingRate*100),col="black",lwd=7/N_opening*4)+
        theme(legend.position = "top")+
        guides(col="none")+
        scale_alpha_continuous("Rate of clogging in slots (Boulders higher than opening) [%]",range = c(0, 1))+
        guides(alpha = guide_legend(order = 1,nrow=1))+
        theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank()
              ,legend.key.size = unit(0.5, 'cm'),legend.key=element_rect(colour = "black"))+
        coord_cartesian(xlim = c(0,1*Duration/3600))+
        labs(y = "Opening"#,caption=paste("(c)")
        )+
        theme(legend.margin = unit(c(0.1,0.1,0.1,0.1), "cm")
              # ,plot.margin = unit(c(0.1,0.1,0.1,0.5), "cm")
              )
  }
  
  VclogPlot<-ggplot(VerticalClogging,aes(x=Time/3600,y=Opening))+theme_bw(base_size = 9)+
    geom_line(aes(alpha=CloggingRate*100),col="black",lwd=9/N_opening*4)+
    theme(legend.position = "top")+
    guides(col="none")+
    scale_alpha_continuous("Rate of vertical clogging (Boulder jamming laterally) [%]",range = c(0, 1))+
    guides(alpha = guide_legend(order = 1,nrow=1))+
    coord_cartesian(xlim = c(0,1*Duration/3600))+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank()
          ,legend.key.size = unit(0.5, 'cm'),legend.key=element_rect(colour = "black"))+
    labs(y = "Opening"
         # ,caption=paste("(b)")
         )+
    theme(legend.margin = unit(c(0.1,0.1,0.1,0.1), "cm")
          # ,plot.margin = unit(c(0.1,0.1,0.1,0.5), "cm")
          )
  
  Zplot<-ggplot(Reservoir)+theme_bw(base_size = 9)+
    geom_hline(aes(yintercept = OpeningMinBaseLevel,colour="1",lty="1"))+
    geom_hline(aes(yintercept = SpillwayLevel,colour="2",lty="2"))+
    geom_hline(aes(yintercept = CrestLevel,colour="3",lty="3"))+
    geom_line(aes(x=Time/3600,y=Z,colour="4",lty="4"),lwd=1)+
    geom_line(aes(x=Time/3600,y=LevelClogging1,colour="5",lty="5"))+
    geom_ribbon(aes(x=Time/3600,ymax=LevelClogging1,ymin=OpeningMinBaseLevel)
                ,lwd=1,col="transparent",alpha=0.2)+
    theme(legend.position = "top")+
    scale_colour_grey(name="Level",label=c("Opening base","Spillway","Crest","Flow","Boulder jam at opening #1"))+
    scale_linetype_manual(name="Level",label=c("Opening base"
                                               ,"Spillway"
                                               ,"Crest"
                                               ,"Flow"
                                               ,"Boulder jam at opening #1")
                          ,values=c(2,2,3,1,4))+
    annotate(geom = "text", x = 1*Duration/3600, y = max(storageElevationCurve$h,na.rm=TRUE)
             ,vjust=1,hjust=1
             , label = paste("Max level reached =",round(max(Reservoir$Z),1),"m.a.s.l.")
    )+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank())+
    coord_cartesian(xlim = c(0,1*Duration/3600),ylim=c(min(storageElevationCurve$h,na.rm=TRUE),max(storageElevationCurve$h,na.rm=TRUE)))+
    labs( x = "Time [h]",y = "Flow level [m]"
          # ,caption=paste("(d)")
          # ,subtitle = paste("Max level at barrier =",round(max(Reservoir$Z),1),"m.a.s.l.")
          )+
    theme(legend.margin = unit(c(0.1,0.1,0.1,0.1), "cm")
          # ,plot.margin = unit(c(0.1,0.1,0.1,0.4), "cm")
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
    labs( x = "Time [h]",y = "Volume [*1000m3]"
          #,caption=paste("(e)")
          # ,subtitle = paste("Surge:",round(Volume/10^6,2),"Mm3"
          #               ,"| Trapped:",round(Reservoir$V[(N_TimeSteps-1)],0)/1000,"Mm3"
          #               ,"| Released:",round(Volume/10^6-Reservoir$V[(N_TimeSteps-1)]/1000,3),"Mm3"
          #               ,"| Buffering:",round(100-(Volume/10^6-Reservoir$V[(N_TimeSteps-1)]/1000)/(Volume/10^6)*100,0),"%")
          )+
    theme(legend.margin = unit(c(0.1,0.1,0.1,0.1), "cm")
          # ,plot.margin = unit(c(0.1,0.1,0.1,0.3), "cm")
          )
  
  #Plot name definition with date and hour
  Plot.Name<-paste0("2Outputs/TimeSeriesOfEvent_",EventName
                    ,"_forStructure_",StructureName
                    ,"-computedOn",lubridate::now(),".png")
  Plot.Name<-str_replace_all(Plot.Name,":","-")
  Plot.Name<-str_replace_all(Plot.Name," ","_")
  
  png(Plot.Name, width = 17, height = 20,units="cm",res=350)
  {
    # grid.arrange(Qplot1,Wplot1,Zplot,Vplot,nrow = 4)
    pushViewport(viewport(layout = grid.layout(22,1) ) )
    # Une fonction pour definir une region dans la mise en page
    define_region <- function(row, col){
      viewport(layout.pos.row = row, layout.pos.col = col)
    }
    # Arranger les graphiques
    if(is.null(WidthClogging))
    {
      print(Qplot+labs(title=paste0("Modelling of structure: ",StructureName," for event:",EventName,"\n"
                                    ,"Model version: ",ModelVersion,"\n"
                                    ,"Boulder generation mode: ",BoulderGenerationMode))
            , vp = define_region(1:6,1))
      print(VclogPlot, vp = define_region(7:10,1))
      print(Zplot, vp = define_region(11:16,1))
      print(Vplot, vp = define_region(17:22,1))
    }else{
      print(Qplot+labs(title=paste0("Modelling of structure: ",StructureName," for event: ",EventName,"\n"
                                    ,"Model version: ",ModelVersion,"\n"
                                    ,"Boulder generation mode: ",BoulderGenerationMode))
            , vp = define_region(1:5,1))
      print(VclogPlot, vp = define_region(6:9,1))
      print(WclogPlot, vp = define_region(10:13,1))
      print(Zplot, vp = define_region(14:17,1))
      print(Vplot, vp = define_region(18:22,1))
    }
  }
  dev.off()
}
# 
# Plot_BufferingModel(Reservoir,WidthClogging,VerticalClogging
#                     ,N_opening,storageElevationCurve,Volume
#                     ,N_TimeSteps,Duration
#                     ,OpeningMinBaseLevel,SpillwayLevel,CrestLevel
#                     ,BoulderGenerationMode)