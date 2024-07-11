
# clean environment
rm(list=ls())
library(ggplot2)
library(dplyr)
setwd("C:/Users/shirrah/Documents/MSc_Thesis/GitHub/DFbuffering/out/Parametric Analysis/100_Iterations")

ListOfDir<-list.dirs()
ListOfDir<-ListOfDir[3:length(ListOfDir)]
i<-3
# for(i in (1:length(ListOfDir)))
for(i in (1:6))
{
  load(paste0(ListOfDir[i],"/RdataResult_Evt-rare_Structure_@-pdd 1.Rdata"))
  rm(Qo_all)
  
  Result_all <- Result_all %>% mutate(CloggingRate = max((7.25-Z1)/7.25,1))
  
  Parameter<-read.csv(paste0(ListOfDir[i],"/Parameter.txt"),header=FALSE,col.names=c("Name","Value"))
  
  Result_all$Parameter<-Parameter$Name
  Result_all$Value<-Parameter$Value
  
  if(i==1){BigTable<-Result_all}else{BigTable<-rbind(BigTable,Result_all)}
  
}


ggplot(BigTable,aes(y=Vout/1000,x=Value))+theme_bw(base_size = 9)+
  geom_boxplot(aes(group=Value)) +
  # geom_violin(aes(group=Value)) +
  # geom_jitter()+
  labs(y="Released volume (*1000m3)")+
  facet_wrap(~ Parameter, scales = "free",strip.position = "bottom")+
  # coord_cartesian(ylim=c(0,80000))+
  geom_smooth(method = "lm")

ggsave("MultiPanelParametricPlotVout.png",width=16,height=20,units="cm")


ggplot(BigTable,aes(y=Qp_out,x=Value))+theme_bw(base_size = 9)+
  geom_boxplot(aes(group=Value)) +
  labs(y="Peak discharge (m3/s")+
  facet_wrap(~ Parameter, scales = "free",strip.position = "bottom")+
  # coord_cartesian(ylim=c(0,80000))+
  geom_smooth(method = "lm")

ggsave("MultiPanelParametricPlotQout.png",width=16,height=20,units="cm")

ggplot(BigTable,aes(y=CloggingRate,x=Value))+theme_bw(base_size = 9)+
  geom_boxplot(aes(group=Value)) +
  scale_y_continuous(labels = scales::percent)+
  labs(y="Clogging rate (-)")+
  facet_wrap(~ Parameter, scales = "free",strip.position = "bottom")+
  coord_cartesian(ylim=c(0,1))+
  geom_smooth(method = "lm")

ggsave("MultiPanelParametricPlotCloggingRate.png",width=16,height=20,units="cm")


ggplot(BigTable,aes(y=Zmax,x=Value))+theme_bw(base_size = 9)+
  geom_boxplot(aes(group=Value)) +
  labs(y="Maximum flow level (m.asl)")+
  facet_wrap(~ Parameter, scales = "free",strip.position = "bottom")+
  # coord_cartesian(ylim=c(0,1))+
  geom_smooth(method = "lm")

ggsave("MultiPanelParametricPlotZmax.png",width=16,height=20,units="cm")