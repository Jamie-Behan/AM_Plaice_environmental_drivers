# AM_Plaice_environmental_drivers

## This repository houses the data used to build and run the exploratory GAM analyses that were used to identify potential ecosystem drivers on American plaice population dynamics.

### From "Plaice_Ecosystem_Drivers.PDF":

#### **1.	Introduction**

  Fish population dynamics are strongly dependent upon the environment in which the populations exist (Pershing et al. 2021; Nye et al. 2009; Brodziak and O’Brien 2005). Environmental conditions such as bottom water temperature, sea surface temperature (SST), salinity, current direction, and strength, etc. are known to affect fish survival rates, recruitment strength, growth, and distribution, amongst other population dynamics. However, the environmental drivers determined to have a significant or strong effect may vary across species and/or spatial locations because such impacts are specific to each ecosystem and are difficult to generalize (Brodziak and O’Brien 2005).
  To help identify ecosystem and climate influences on American plaice stock dynamics, generalized additive models (GAMs) were used to examine associations between potential environmental drivers (identified through a literature review) and American plaice recruitment, distribution, and growth in the Gulf of Maine (GOM) region. Tested environmental effects include (1) bottom water and sea surface temperature anomalies, (2) the Atlantic Multidecadal Oscillation (AMO), (3) the North Atlantic Oscillation (NAO), (4) the Gulf Stream Index (GSI), and (5) density-dependent effects (SSB). If incorporated into future assessments, identified ecosystem and climate influences on recruitment. Distribution, and growth may reduce uncertainty in stock assessments of GOM American plaice.
  
### **Included in this Repository is the following:**
  
  **1. "Plaice_Ecosystem_Drivers.PDF"** - A technical report detailing a short introduction, as well as detailed methods and results from these exploratory analyses
  
  **2. "Plaice_Ecosystem_Drivers.html"** - An RMarkdown document containing a slightly more summarized version of the methods and results compared to the technical report.
  
  **3. "Code" folder containing R code used to run the analyses:**
  
- "Plaice_recruitment_age1_cleanup.R" - .R file used to run exploratory recruitment analyses as described in "Plaice_Ecosystem_Drivers.PDF"
        
- "Plaice_distribution_cleanup.R" - .R file used to run exploratory distribution analyses as described in "Plaice_Ecosystem_Drivers.PDF"
        
- "Plaice_growth_GAMs.R" - .R file used to run exploratory growth analyses as described in "Plaice_Ecosystem_Drivers.PDF"
        
        Any further questions can be directed to Jamie Behan: jbehan@gmri.org
