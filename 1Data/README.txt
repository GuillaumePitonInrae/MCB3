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
-BoulderDiameter_min & BoulderDiameter_max	are the lower and upper range of diameter D of the boulder class in meters;
-the three next columns are BoulderNumber_BestEstimate, BoulderNumber_min & BoulderNumber_max are the typical number N of such boulders that can be found in a reference volume (that is written in the next column), it is asked to provide a lower bound and upper bound (N being out of this range is considered impossible) and a best estimate (which mathematically is assumed to be the mode of the distribution, i.e., the most probable value);
-The fifth column is ReferenceVolume, it is the reference volume in which typically the above mentionned numbers of boulders of class D can be found, expressed in cubic meters.
 
