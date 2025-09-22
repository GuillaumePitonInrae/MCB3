The main code is 00_MainCodeV3.R

0SourceCodes/00_InstallPackages.R install all packages required to run the simulations
0SourceCodes/00_MainCode.R is the main code to be used. It help the user to load the source codes, to define where are the input data, to define the type of simulation to be run and where to store the results

Subroutines (alphabetical order)
0SourceCodes/BoulderPassing.R computes the number of boulders passing through an orifice for a given volume of debris flow (binomial distribution)
0SourceCodes/BoulderTransfer.R contains a function that takes the output data of a structure to prepare the input data to another located downstream
0SourceCodes/Cascade_of_structure_functionning.R this function call the Structure_functionning subroutines over the series of structures.
0SourceCodes/CheckTriangleDistribution.R check the input data to control if triangle are indeed triangles, i.e., min < best estimate < max
0SourceCodes/Create_inlet_input.R create a vector of input data 
0SourceCodes/Create_inlet_timeseries.R create a timeseries to feed Structure_functionning_VX.X from the input vector
0SourceCodes/DischargeCapacityFunctions.R is used to compute slit, slot and weir hydraulique capacity functions
0SourceCodes/Plot_BufferingModelResults.R is used for plotting results of singular runs (times series)
0SourceCodes/PLOT_INPUTnew.R is used to plot input distributions
0SourceCodes/Structure_definition.R define structures 
0SourceCodes/Structure_functionning.R is the core code routing input timeseries through a structure

