# CheekyeDebrisFlowBarrier

Large boulders can jam in constrictions, both for natural geological features (e.g. canyons) and man-made structures (e.g. slit-dams). Boulder jamming occurs stochastically, and partially governs the outflow rate of geophysical material. Explicit hydro-mechanical models of flows and boulders are unsuitable for studying this stochasticity given their prohibitive computational demands.

This software is a novel computationally-frugal statistical-hydro-mechanical program. This new program applies expert estimates of a multi-phase flows upstream of a constriction to a simple model of fluid and boulders interacting with the constriction. This constriction can comprise multiple arbitrarily-sized and arbitrarily-placed sub-openings. The model uses a novel and simple statistical method to stochastically compute the presence of boulders that might obstruct the opening(s). The program then gives simple descriptions of flow characteristics near the constriction, including the flow level over time, and the outlet discharge rate and volume. The model runs remarkably quickly (5-30 s per run), allowing uncertainty propagation analyses of interactions between flows, boulders and constrictions.

Interaction between both low- and high-risk flows can be quickly evaluated for different canyon cross-sections or details of slit-dam design. 

For uncertainty propagation, we use possibility theory, a framework complementary to probabilities but more suitable for accommodating uncertainties relevant for geophysical flows (e.g. boulder size distributions and hydrograph shape). This gives practitioners a much-needed generalised tool for understanding the long-term behavior of boulder-laden flows through canyons, or for the detailed design of engineered structures. 
 
Data regarding the boulder number, magnitude of the event, design of the barrier and stage - volume capacity of the basin come from a field case - the design of the proposed Cheekye slit-dam - thence elucidating the design requirements for effective flow control. 
