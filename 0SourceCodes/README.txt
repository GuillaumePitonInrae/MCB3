The main code is 01_MainCode.R

0SourceCodes/00_InstallPackages.R install all packages required to run the simulations
0SourceCodes/01_MainCode.R is the main code to be used. It help the user to load the source codes, to define where are the input data, to define the type of simulation to be run and where to store the results

Subroutines (alphabetical order)
0SourceCodes/BoulderPassing_V1.R computes the number of boulders passing through an orifice for a given volume of debris flow (binomial distribution)
0SourceCodes/BarrierBuffering_V0.3.R is doing the core computation, loading data, resolving mass conservation at each time step and computes all times series
0SourceCodes/CheckTriangleDistribution.R check the input data to control if triangle are indeed triangles, i.e., min < best estimate < max
0SourceCodes/CheekyeBuffering_V1.R creates functions that call BarrierBuffering_V0.3 and records some of its results, it is adapted for the Cheekye case with large wood clogging the base level
0SourceCodes/DesignEvent_Release_HybridAnalysis_V0.3.R is the main code used in Piton, Goodwin et al. (2022) Debris flows, boulders and constrictions: a simple framework for modeling jamming, and its consequences on outflow. Journal of Geophysical Research: Earth Surface DOI: 10.1029/2021JF006447 [online] Available from: https://onlinelibrary.wiley.com/doi/10.1029/2021JF006447 (Accessed 24 April 2022)
0SourceCodes/DischargeCapacityFunctions_V0.R is used to compute slit, slot and weir hydraulique capacity functions
0SourceCodes/Plot_BufferingModelResults_V0.R is used for plotting results of singular runs (times series)
0SourceCodes/PLOT_INPUTnew.R is used to plot input distributions

