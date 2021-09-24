g<-9.81
# #Basic hydraulic fonctions
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
          if((Opening$Param[i]>BaseClogging[i]+Opening$Base.Level[i] & Opening$Width[i]>WidthClogging[i] )){
            Q.Ind[i]<-Q_GOrifice(h=(h[Ind]-Opening$Base.Level[i]-BaseClogging[i])
                                 ,a=Opening$Param[i]
                                 ,b=Opening$Width[i]-WidthClogging[i]
                                 ,mu=0.65)
            }else{Q.Ind[i]<-0}
          
        }
        if(Opening$Type[i]=="slit")
        {
          Q.Ind[i]<-Q_GOrifice(h=(h[Ind]-Opening$Base.Level[i]-BaseClogging[i])
                               ,a=(Opening$Base.Level[i]+10^4) #+10^4 because slits are just slots infinitely high
                               ,b=(Opening$Width[i]-WidthClogging[i])
                               ,mu=0.65) 
        } 
        if(Opening$Type[i]=="weir")
        {
          Q.Ind[i]<-Q_Weir(h=(h[Ind]-Opening$Base.Level[i]-BaseClogging[i])
                           ,b_spillway=(Opening$Width[i]-WidthClogging[i]+2/tan(Opening$Param[i]/180*pi)*BaseClogging[i])
                           ,mu = 0.65
                           ,Phi=Opening$Param[i])
        }
      }
    Q[Ind,]<-Q.Ind
    }
  return(Q)  
  }
  
  # Q_CompoundBarrier(Opening,c(3,0,0),c(2,0,0),696)
  