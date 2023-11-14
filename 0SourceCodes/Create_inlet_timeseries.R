### Create time series of discharge and boulders
#V0 July 2023 - G. Piton & C. Misset

Create_inlet_timeseries<-function(input,Boulders)
{
################################
#    EXTRACT INPUT RUN DATA----
################################

Volume<-input[1]*10^3
Qpeak<-input[2]
#Event features computation
Duration<-round(Volume/(Qpeak/2),0)
PeakLag<-input[3]*Duration
# #Duration of the simulation = input hydrograph duration * FactorOfDuration
FactorOfDuration<-5
SimulationDuration<-round(FactorOfDuration*Duration)

#Number of time steps
N_TimeSteps<-ceiling(SimulationDuration/TimeStep)

#### Slope of deposition
SlopeDep<-input[4]#

#Count number of boulders 
IndexOfLastInitialCondition<-length(input)-length(Boulders[,1])
for(i in (1:length(Boulders[,1])))
{
  Boulders$Number[i]<-input[IndexOfLastInitialCondition+i]
}
#Mean boulder size of each class used to compute the typucal volume of a boulder
Boulders$Diameter<-0.5*(Boulders$BoulderDiameter_min+Boulders$BoulderDiameter_max)
#Elementary volume of each boulder class
Boulders$V<-pi/6*Boulders$Diameter^3
#Maximum number of boulder that could theoretically be observed in the reference volume
Boulders$Nmax<-round(Boulders$ReferenceVolume/Boulders$V,0)
#PRobability of having a boulder each time a volume of debris flow = boulder volume pass
Boulders$P<-Boulders$Number/Boulders$Nmax

#Result dataset initialisation
Times.series.inlet<-data.frame(T=seq(0,SimulationDuration,length.out = N_TimeSteps)
                               #triangular hydrograph 
                               ,Q=approx(x=c(0,PeakLag,Duration)
                                         ,y=c(0,Qpeak,0)
                                         ,xout = seq(0,SimulationDuration,length.out = N_TimeSteps)
                                         ,yright=0)$y #Qinlet
                              )
#Create columns of probability p for each class J of boulders according to the values provided in input
for(i in (1:length(Boulders[,1])))
{
  if(i==1){
    Boulder.p<-data.frame(p1=rep(Boulders$P[i],length.out = N_TimeSteps))
    }else{
      Boulder.p<-cbind(Boulder.p,data.frame(rep(Boulders$P[i],length.out = N_TimeSteps)))
    
  }
}
names(Boulder.p)<-paste0("p",(1:length(Boulders[,1])))
#Add also columns of number of boulders N as NA to mimick the structure of output files downstream of a structure
Boulder.N<-data.frame(matrix(data=NA,nrow=N_TimeSteps,ncol=length(Boulders[,1])))
names(Boulder.N)<-paste0("N",(1:length(Boulders[,1])))

#Merge tables of time, discharge, p and N
Times.series.inlet<-cbind(Times.series.inlet,Boulder.p,Boulder.N)

return(Times.series.inlet)
}
