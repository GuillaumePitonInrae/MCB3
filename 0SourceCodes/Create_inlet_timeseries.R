#' ---
#' title: "Create Inlet Timeseries"
#' output: html_document
#' date: "2024-03-27"
#' Author: G. Piton, C. Misset, H. Shirra
#' ---
#' 
#' This is an annotated version of the Create_inlet_timeseries.R  script, originally developed by G. Piton and C. Misset  as part of a larger script to stochastically simulate jamming through a series of constrictions in a debris flow event. This script creates the time series of boulder and water inflow to the structures. The input files created by this script are called upon by 00_MainCode.
#' 
#' ### Summary of Script
#' 
#' This script creates a function to combine input variables into a time series, which is applied to structure inlets. The input data created in the Create_inlet_input.R script is called upon and used to calculate the inflow hydrograph for each structure. 
#' 
#' The duration of the event is approximated by dividing the volume of the event by half of the peak flow. The lag of the peak flow is calculated by multiplying the user defined lag time by the calculated event duration. The duration of the simulation is set equal to the input hydrograph duration multiplied by the factor of duration. The Number of time steps in the simulation is calculated by dividing the duration of the simulation by the time step interval. 
#' 
#' The deposition slope and the number of boulders  is imported from a database. The average volume of a boulder in each boulder class is computed using the average boulder diameter within that class. Each boulder is assumed to be spherical. The boulder volumes are used to calculate the maximum number of boulders from a given class that can be observed within a reference volume of debris flow. The probability that a boulder of a given class will be observed in each reference volume is calculated, by dividing the total number of boulders by the maximum number of boulders possible within a reference volume. 
#' 
#' A dataframe is initialized that contains the hydrograph, from the beginning of the simulation (t=1) to the end (t=SimulationDuration), incrementing by the selected time step. The hydrograph is assumed to be linear and triangular, rising from 0 m3/s to the user-defined peak flow, then back down to 0 m3/s. The peak flow is reached at the user-defined peak lag time. Finally, another dataframe is created, wherein each boulder class is assigned a probability, according to the user-defined input values. All dataframes are combined to be called upon later by the main code. 
#' 
#' ### Script
## ----comment= "#", echo=FALSE-------------------------------------------------
#Transfer the .Rmd file to R automatically when it is knit. 
#knitr::purl(input = "Create_inlet_timeseries.Rmd", documentation = 2) #comment this line prior to running generated R script.

#' 
## -----------------------------------------------------------------------------
Create_inlet_timeseries<-function(input,Boulders)
{
#Extract volume and peak flow from input data
Volume<-input[1]*10^3
Qpeak<-input[2]

#The duration of the event is approximated by dividing the volume of the event by half of the peak flow.
Duration<-round(Volume/(Qpeak/2),0)

#The lag of the peak flow is calculated by multiplying the user defined lag time by the calculated duration.
PeakLag<-input[3]*Duration

#The duration of the simulation is set equal to the input hydrograph duration multiplied by the factor of duration. 
FactorOfDuration<-3
SimulationDuration<-round(FactorOfDuration*Duration)

#The Number of time steps in the simulation is calculated by dividing the duration of the simulation by the time step interval. 
N_TimeSteps<-ceiling(SimulationDuration/TimeStep)

#The deposition slope is called from the input variables compiled in the Create_inlet_input.R script.
SlopeDep<-input[4]#

#The number of boulders is determined from the input compiled in the Create_inlet_input.R script, and added to the Boulders data set.
IndexOfLastInitialCondition<-length(input)-length(Boulders[,1])
for(i in (1:length(Boulders[,1])))
{
  Boulders$Number[i]<-input[IndexOfLastInitialCondition+i]
}

#Mean boulder size of each class used to compute the typical volume of a boulder.
Boulders$Diameter<-0.5*(Boulders$Diameter_min+Boulders$Diameter_max)

#Elementary volume of each boulder class. Each boulder is assumed to be spherical.
Boulders$V<-pi/6*Boulders$Diameter^3
#Maximum number of boulders that could theoretically be observed in the reference volume.
Boulders$Nmax<-round(Boulders$ReferenceVolume/Boulders$V,0)
#Probability of having a boulder each time a volume of debris flow equal to boulder volume passes.
# V0: consider probabilities independantly of other boulders
Boulders$P<-Boulders$Number/Boulders$Nmax
#Addition for boulders of class >1 of the consideration of the bigger boulders in the remaining volume
for(i in (2:length(Boulders[,1])))
{
  Boulders$P[i]<-Boulders$Number[i]/(round((Boulders$ReferenceVolume[i]-sum(Boulders$Number[1:(i-1)]*Boulders$ReferenceVolume[1:(i-1)]/Boulders$ReferenceVolume[i]*Boulders$V[1:(i-1)]))/Boulders$V[i],0))
}

#Result dataset initialization
Times.series.inlet<-data.frame(Time=seq(1,SimulationDuration,by = TimeStep)
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

#Add also columns of number of boulders N as NA to mimic the structure of output files downstream of a structure
Boulder.N<-data.frame(matrix(data=NA,nrow=N_TimeSteps,ncol=length(Boulders[,1])))
names(Boulder.N)<-paste0("N",(1:length(Boulders[,1])))

#Tables of time, discharge, p and N are merged.
Times.series.inlet<-cbind(Times.series.inlet,Boulder.p,Boulder.N)

return(Times.series.inlet)
}

