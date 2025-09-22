#' ---
#' title: "Discharge Capacity Functions"
#' output: html_document
#' date: "2024-03-22"
#' Author: G. Piton, C. Misset, H. Shirra
#' ---
#' This is an annotated version of the DischargeCapacityFunctions.R script, originally developed by G. Piton and C. Misset as part of a larger script to stochastically simulate jamming through a series of constrictions in a debris flow event. This script defines the hydraulic discharge equations (including the orifice, weir and compound barrier equations) which calculate the passing of water through a constriction. The results are fed into 00_MainCode.R.
#' 
#' ### Summary of Script
#' Discharge is calculated using either the orifice equation, the weir equation, or the compound barrier equation. The equation is selected based on the type of opening, as defined by the user. The basis of these hydraulic equations is well-documents and is freely available online and in textbooks. Variable definitions are available in the Msc Thesis of H. Shirra. 
#' 
#' ### Script
## ----comment= "#", echo=FALSE-------------------------------------------------
#Transfer the .Rmd file to R automatically when it is knit. 
#knitr::purl(input = "DischargeCapacityFunctions.Rmd", documentation = 2) #Comment this line prior to running generated R script.

#' Discharge through an orifice is calculated as follows:
## -----------------------------------------------------------------------------
g<-9.81
# #Basic hydraulic functions
# 
Q_GOrifice<-function(h,a,b,mu=0.7) #Upstream water depth, orifice height, orifice width, weir coefficient
{ 
  Q<-rep(0,length(h))
  Q[h>a]<-(2/3*b*mu)*(2*g)^0.5*(h[h>a]^1.5-(h[h>a]-a)^1.5)
  Q[h<=a]<-mu*2/3*b*(2*g*h[h<=a]^3)^0.5#Weir hypothesis for h<a
  Q[h<=0]<-0
  return(Q)
}
  #test
# Q_GOrifice(0.1,0.02,0.15,0.65)

#' Discharge through a check dam is calculated as follows:
## -----------------------------------------------------------------------------
#check dam discharge
#Equation after Deymier et al. 1995 Conception et calcul de barrages de correction torrentielle, Edition PEGR
  Q_Weir<-function(h,b_spillway,mu=0.7,Phi=90)#Upstream water depth, spillway width,weir coefficient,  Phi side angle of the spillway,
  { Q<-rep(NA,length(h))
  for(i in 1:length(h))
  {
    if(h[i]>0){Q[i]<-mu*2/3*(b_spillway*(2*g*h[i]^3)^0.5+0.8*(2*g*h[i]^5)^0.5/tan(Phi/180*pi))}
    
    else{Q[i]<-0}
  }
  return(Q)
  }
  # Q_Weir(1,6,0.7,45)

###

#' 
#' Discharge through a compound barrier (i.e. some combination of slots, slits, and weirs) is calculated as follows:
## -----------------------------------------------------------------------------
 Q_CompoundBarrier<-function(Opening,BaseClogging,WidthClogging,h)
  {
    BaseClogging<-as.numeric(BaseClogging)
    WidthClogging<-as.numeric(WidthClogging)
    Q<-data.frame(matrix(0, length(h), length(!is.na(Opening$Number))),stringsAsFactors=F)
    
    for(Ind in 1:length(h))
    { 
      Q.Ind<-rep(0,length(!is.na(Opening$Number)))
      for(i in (1:length(Opening$Number)))
      {
        if(Opening$Type[i]=="slot")
        {
          if((Opening$TopLevel[i]>BaseClogging[i]+Opening$BaseLevel[i] & Opening$Width[i]>WidthClogging[i] )){
            Q.Ind[i]<-Q_GOrifice(h=(h[Ind]-Opening$BaseLevel[i] - BaseClogging[i])
                                 ,a=Opening$TopLevel[i]-Opening$BaseLevel[i] - BaseClogging[i]
                                 ,b=Opening$Width[i] - WidthClogging[i]
                                 ,mu=0.65)
            }else{Q.Ind[i]<-0}
          
        }
        if(Opening$Type[i]=="slit")
        {
          Q.Ind[i]<-Q_GOrifice(h=(h[Ind] - Opening$BaseLevel[i] - BaseClogging[i])
                               ,a=(Opening$BaseLevel[i]+10^4) #+10^4 because slits are just slots infinitely high
                               ,b=(Opening$Width[i]-WidthClogging[i])
                               ,mu=0.65) 
        } 
        if(Opening$Type[i]=="weir")
        {
          if(BaseClogging[i] > Opening$TopLevel[i] - Opening$BaseLevel[i])
          {# case when the trapezoid part is fully clogged
            Q.Ind[i]<-Q_Weir(h=(h[Ind]-Opening$BaseLevel[i]-BaseClogging[i])
                             ,b_spillway=(Opening$Width[i]-WidthClogging[i]+2/tan(Opening$SideAngle[i]/180*pi)*BaseClogging[i])
                             ,mu = 0.65
                             ,Phi=90)
          }else{
            if(h[Ind] > Opening$TopLevel[i])
            {# case with trapezoid base shape and then vertical walls above top level, computed as a full trapezoid shape minus discharge
              # of a triangle weir starting at top level
              Q.Ind[i]<-Q_Weir(h=(h[Ind]-Opening$BaseLevel[i]-BaseClogging[i])
                               ,b_spillway=(Opening$Width[i]-WidthClogging[i]+2/tan(Opening$SideAngle[i]/180*pi)*BaseClogging[i])
                               ,mu = 0.65
                               ,Phi=Opening$SideAngle[i]) - Q.Ind[i] - Q_Weir(h=(h[Ind]-Opening$TopLevel[i])
                                                                             ,b_spillway=0
                                                                             ,mu = 0.65
                                                                             ,Phi=Opening$SideAngle[i])
            }else{
              Q.Ind[i]<-Q_Weir(h=(h[Ind]-Opening$BaseLevel[i]-BaseClogging[i])
                               ,b_spillway=(Opening$Width[i]-WidthClogging[i]+2/tan(Opening$SideAngle[i]/180*pi)*BaseClogging[i])
                               ,mu = 0.65
                               ,Phi=Opening$SideAngle[i])
            }
          }
        }
      }
    Q[Ind,]<-Q.Ind
    }
  return(Q)  
  }
  
  # Q_CompoundBarrier(Opening,rep(1,length(Opening$Number)),rep(0,length(Opening$Number)),738)
  
  

