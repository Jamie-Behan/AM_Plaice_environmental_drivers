# AM_Plaice_environmental_drivers

## This repository houses the data used to build and run the exploratory GAM analyses that were used to identify potential ecosystem drivers on American plaice population dynamics.

### From "Plaice_Ecosystem_Drivers.PDF":

#### **1.	BACKGROUND**

Over the last 40 years, the waters of the northwest Atlantic have warmed at a rate more than three times the global average and the recent decadal warming in this region is among the fastest in the world (Pershing et al. 2015, 2018, NEFSC 2022b). Fish population dynamics are strongly influenced by these changing ocean conditions and Northeast groundfish have exhibited sensitivity to changing thermal conditions with associated changes in productivity and distribution (Brodziak and O’Brien 2005, Nye et al. 2009, Hare et al. 2016, Pershing et al. 2021). American plaice is a cold-water demersal flatfish species native to the North Atlantic and Arctic oceans (NEFMC 1985). Changes in ocean conditions have been documented to affect key life history processes, including recruitment, distribution, and growth of American plaice (see detailed description in ToR 1 section of WG report).

The goal of this work was to conduct exploratory modeling to examine the relationship between key aspects of American plaice stock dynamics (i.e., recruitment, distribution, and growth) and ocean climate variables. The literature review, combined with fishermen’s ecosystem knowledge informed the selection of environmental drivers to explore in these analyses. Time series of relevant environmental variables included sea surface (SST) and bottom temperature anomalies, Atlantic Multidecadal Oscillation (AMO), North Atlantic Oscillation (NAO), and the Gulf Stream Index (GSI) and were related to time series of stock variables using generalized additive models (GAMs). These analyses were used to inform the environmental covariates considered in performance testing of climate-integrated stock assessment modeling of American plaice (see ToR 4 section of WG report). 

  
### **Included in this Repository is the following:**
  
  **1. "Plaice_Ecosystem_Drivers.PDF"** - A technical report detailing a short introduction, as well as detailed methods and results from these exploratory analyses
  
  **2. "Plaice_Ecosystem_Drivers.html"** - An RMarkdown document containing a slightly more summarized version of the methods and results compared to the technical report.
  
  **3. "Code" folder containing R code used to run the analyses:**
  
- "Plaice_recruitment_age1_cleanup.R" - .R file used to run exploratory recruitment analyses as described in "Plaice_Ecosystem_Drivers.PDF"
        
- "Plaice_distribution_cleanup.R" - .R file used to run exploratory distribution analyses as described in "Plaice_Ecosystem_Drivers.PDF"
        
- "Plaice_growth_GAMs.R" - .R file used to run exploratory growth analyses as described in "Plaice_Ecosystem_Drivers.PDF"
        
  **4. "Environmental Data" folder containing Environmental data used in analyses:**
   
- "fall_env.df.csv" - Fall environmental data"

- "spring_env.df.csv" - Spring environmental data"

- original data sources for all data used are discussed in the technical report, "Plaice_Ecosystem_Drivers.PDF".

      Any further questions can be directed to Jamie Behan: jbehan@gmri.org
   
