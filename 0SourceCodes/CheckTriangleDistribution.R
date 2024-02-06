CheckTriangleDistribution<-function(Min,BestEstimate,Max,Name)
{
  #Check that the data are indeed with min < best estimate < max
  if((Min<BestEstimate && BestEstimate<Max))
  {
    return(c(Min,BestEstimate,Max))
  }else
  {
    #if min >= best estimate, set min = 0.999 best estimate
    if(Min>BestEstimate)
    {
      Min<-0.999*BestEstimate
      # dlg_message(message=paste("In the input data, on parameter"
                                # ,Name
                                # ,", min >= best estimate, \n I changed it to min = 0.999*best estimate for the simulation, consider correcting the input file if necessary.")
                  # , type = c("ok"))
      print(paste("In the input data, on parameter" ,Name ,", min > best estimate,"
      ,"I changed it to min = 0.999*best estimate for the simulation, consider correcting the input file if necessary."))
    }
    
    #if max <= best estimate, set max = 1.001 best estimate
    if(BestEstimate>Max)
    {
      Max<-1.001*BestEstimate
      # dlg_message(message=paste("In the input data, on parameter"
      #                           ,Name
      #                           ,", best estimate >= max, \n I changed it to max = 1.001*best estimate for the simulation, consider correcting the input file if necessary")
      #             , type = c("ok"))
      print(paste("In the input data, on parameter",Name,", best estimate > max,",
                  " I changed it to max = 1.001*best estimate for the simulation, consider correcting the input file if necessary"))
    }
  }
  
  return(c(Min,BestEstimate,Max))
}
