#V0 call for BarrierBuffering_V0.2 (for best estimate numbers of boulders) and BarrierBufferingV0.3
#V1 calls only for BarrierBufferingV0.3 but rewrite the inputs based on the best estimate numbers of boulders


CheekyeBufferingModel<-function(input)
{
  
  #Transform volume in *1000 m3
  input[1]<-input[1]*10^3
  # Duration = V/Q*2
  Duration<-round(input[1]*10^3/(input[2]/2),0)
  #Pass duration in second rather than dimensionless
  input[3]<-input[3]*Duration
  
  Boulders<-read.csv("./1Data/RangeOfBoulders.txt",sep="\t")
  #Consider only the best estimate of number of boulders
    input[7:12]<-Boulders$Best_estimate
  
    Boulder.Generation.Mode<-"Best estimate numbers"
    
  Result<-BarrierBufferingUncertainSlope_V0.3(input,Boulder.Generation.Mode)
  
  return(Result)
}

#Function exporting the maximum level at barrier 
CheekyeBufferingModel_Z<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel(input))
return(Rslt[1])
}
# CheekyeBufferingModel_Z(input)

# #Function exporting the volume flowing though and atop the barrier 
CheekyeBufferingModel_Vtot<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel(input))
return(Rslt[5])
}

# #Function exporting the volume flowing though the SLIT 
CheekyeBufferingModel_Vslit<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel(input))
return(Rslt[6])
}

# #Function exporting the volume flowing over the SPILLWAY 
CheekyeBufferingModel_Vspillway<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel(input))
return(Rslt[7])
}

# #Function exporting the volume flowing over the CREST until flow level drop down below the spillway
CheekyeBufferingModel_VCrest<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel(input))
return(Rslt[8])
}
# #Function exporting the time of overtopping on the spillway
CheekyeBufferingModel_tOverSpillway<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel(input))
return(Rslt[11])
}


# #Function exporting the time of overtopping on the Crest
CheekyeBufferingModel_tOverCrest<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel(input))
return(Rslt[10])
}



#######With uncertain model input of boulder numbers


CheekyeBufferingModel_UncertainBoulderNumber<-function(input)
{
  #Transform volume in *1000 m3
  input[1]<-input[1]*10^3
  # Duration = V/Q*2
  Duration<-round(input[1]*10^3/(input[2]/2),0)
  #Pass duration in second rather than dimensionless
  input[3]<-input[3]*Duration
  
  Boulder.Generation.Mode<-"Uncertain numbers"
  #V0.3 uses the uncertain data of boulder numbers
  Result<-BarrierBufferingUncertainSlope_V0.3(input,Boulder.Generation.Mode)

  return(Result)
}

#Function exporting the maximum level at barrier 
CheekyeBufferingModel_UncertainBoulderNumber_Z<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel_UncertainBoulderNumber(input))
return(Rslt[1])
}

# #Function exporting the volume flowing though and atop the barrier 
CheekyeBufferingModel_UncertainBoulderNumber_Vtot<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel_UncertainBoulderNumber(input))
return(Rslt[5])
}

# #Function exporting the volume flowing though the SLIT 
CheekyeBufferingModel_UncertainBoulderNumber_Vslit<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel_UncertainBoulderNumber(input))
return(Rslt[6])
}

# #Function exporting the volume flowing over the SPILLWAY 
CheekyeBufferingModel_UncertainBoulderNumber_Vspillway<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel_UncertainBoulderNumber(input))
return(Rslt[7])
}

# #Function exporting the volume flowing over the CREST until flow level drop down below the spillway
CheekyeBufferingModel_UncertainBoulderNumber_VCrest<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel_UncertainBoulderNumber(input))
return(Rslt[8])
}

# #Function exporting the time of overtopping on the crest
CheekyeBufferingModel_UncertainBoulderNumber_tOverCrest<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel_UncertainBoulderNumber(input))
return(Rslt[10])
}

# #Function exporting the time of overtopping on the spillway
CheekyeBufferingModel_UncertainBoulderNumber_tOverSpillway<-function(input)
{Rslt<-as.numeric(CheekyeBufferingModel_UncertainBoulderNumber(input))
return(Rslt[11])
}


