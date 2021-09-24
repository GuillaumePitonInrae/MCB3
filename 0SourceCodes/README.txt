The main code is DesignEvent_Release_HybridAnalysis_V0.3.R

0SourceCodes/PLOT_INPUTnew.R is used to plot input distributions
0SourceCodes/DischargeCapacityFunctions_V0.R is used to compute slit, slot and weir hydraulique capacity functions
0SourceCodes/BoulderPassing_V1.R computes the number of boulders passing through an orifice for a given volume of debris flow (binomial distribution)
0SourceCodes/Plot_BufferingModelResults_V0.R is used for plotting results of singular runs (times series)
0SourceCodes/BarrierBuffering_V0.3.R is doing the core computation, loading data, resolving mass conservation at each time step and computes all times series
0SourceCodes/CheekyeBuffering_V1.R creates functions that call BarrierBuffering_V0.3 and records some of its results, it is adapted for the Cheekye case with large wood clogging the base level
