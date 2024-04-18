#' ---
#' title: "Check Triangle Distribution"
#' output: html_document
#' date: "2024-03-22"
#' Author: G. Piton, C. Misset, H. Shirra
#' ---
#' This is an annotated version of the CheckTriangleDistribution.R scriptprepared by H. Shirra, originally developed by G. Piton and C. Misset as part of a larger script to stochastically simulate jamming through a series of constrictions in a debris flow event. This code confirms that the best estimate of various input parameters is between the minimum and maximum estimates. The results are fed into 00_MainCode.R.
#' 
#' ### Summary of Script
#' The minimum, maximum, and best estimate input parameters are imported from user-defined input. A function is defined that checks if the best estimate value is between the minimum and maximum estimates. If this is not the case, then the minimum and maximum estimates are adjusted, and the user is alerted to the adjustment. 
#' 
#' 
#' ### Script
## ----comment= "#", echo=FALSE-------------------------------------------------
#Transfer the .Rmd file to R automatically when it is knit. 
#knitr::purl(input = "CheckTriangleDistribution.Rmd", documentation = 2) #comment this line prior to running generated R script.

#' 
## -----------------------------------------------------------------------------
CheckTriangleDistribution<-function(Min,BestEstimate,Max,Name)
{
  #Check that the data are indeed within min < best estimate < max
  if((Min<BestEstimate && BestEstimate<Max))
  {
    return(c(Min,BestEstimate,Max))
  }else
  {
    #If min >= best estimate, set min = 0.999 best estimate
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
    
    #If max <= best estimate, set max = 1.001 best estimate
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

