These files provide the input data for the codes

In ElevationStorageCurves.txt, 
-The first column must be the altitude [m.a.s.l] and 
-The second column are for each value of slope [SLOPE], the title is S[SLOPE] and below are,
 for each altitude, the storage capacity of the basin in cubic meters.

In Events.txt, one must define the features of the events to model
-The first column defines the event names
-The second, third and fourth columns define the event volume (min, best estimate, max) [m3]
-The fifth, sixth and seventh columns define the peak discharge  (min, best estimate, max) [m3/s]
-Column 8, 9 and 10 define the peak lag = time to peak / total duration(min, best estimate, max) [-]
-The columns 11, 12 and 13 define the deposition slope (min, best estimate, max) [%]

The file InitialCOnditions.txt describe the initial state in the basin and barrier:
-The three first columns describe the initial deposit height (min, best estimate, max) [m], it is used to define a deposit existing in the basin at the beginning of the simulation
-The columns 4, 5 and 6 describe the height of the jam in the barrier. This is what was used to describe the bottom jam made of large wood in Piton, Goodwin et al. (2022, JGR). 

The file FreqMag.txt is used only to the code DesignEvent_Release_HybridAnalysis_V0.3.R, it provided the supply of debris flow at the Cheekye barrier as in the paper Piton, Goodwin et al. (2022, JGR: Earth Surface),
-Class	is the class of the event, e.g., 1, 2...
-Volume	is the debris flow volume supply in cubic meters
-Discharge	is the debris flow peak discharge in cubic meters / seconds
-Return_Period_min	and Return_Period_max are the estimated return period of this event, in years

 In Opening.txt
 -The first column is the number of the opening
 -The second is the type : "slit", "slot" or "weir"
 -The third is the width in meters
 -The fourth is the base level in altitude
 -The fifth is Param and is type-dependent:
	- for slot it is the top level of the slot in altitude
	- for slit it does not matter, it is not used
	- for weir it is the angle between the wing and the horizontal in degrees. A weir with Param = 90 is actually a slit
 -The sixth is the vertical clogging of the opening by boulders in meters at the beggining of the run
 -The seventh is the lateral clogging of the opening by boulders in meters at the beggining of the run
 -The heighth is a comment column to keep track of what are these openings, e.g., "crest", "spillway", "bottom slit"
 
 
In RangeOfBoulders.txt, the column titles must be: Boulder_size_category_(m)	Lower_bound	Upper_bound	Best_estimate	Reference_Volume
-Boulder_size_category_(m)	is the range of diameter D of the boulder class in meters, it must be separeted by "-", e.g., 0.5-1;
-the three next columns are "Lower_bound"	"Upper_bound"	"Best_estimate"	are the typical number N of such boulders that can be found in a reference volume (that is written in the next column), it is asked to provide a lower bound and upper bound (N being out of this range is considered impossible) and a best estimate (which mathematically is assumed to be the mode of the distribution, i.e., the most probable value);
-The fifth column is Reference_Volume, it is the reference volume in which typically the above mentionned numbers of boulders of class D can be found, expressed in cubic meters.
 
