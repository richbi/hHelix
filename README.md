# hHelix
Functions and vignette for creating hierarchical helices for displaying timed event of nested periodicity 




# TropicalMoon

This repository contains data and code accompanying article "A self-similar helix as a metaphor for continuous time with nested1
periodicity" (Bischof 2025, DOI: XXXXXXXXX). 

The file vignette_LunarDielAnalysis.html provides step-by-step explanations and code for creating and projecting timed data onto hierarchical helical strcutures (up 3rd order) as described in the article.
The following additional files are provided in this repository and are required to run the code in the vignette:


# functions.R
# 
# InputData1.RData
# 
# InputData2.RData
# 
# InputData3.RData
# 
# The data have been processed and are set up as inputs for 3 Bayesian multinomial models described in the article. Each InputData file (in native RData format) contains data objects (lists) required by the NIMBLE models run in the study. These objects are a data object (nimData), a constants objects (nimConstants), an initial values objects (nimInits), and a species information object (species.info). The core elements in the nimData object are counts of 15-minute intervals with unique detections (species at a camera trap). These are aggregated by protected area and species. Detections are further segregated into the diel and lunar periods they are associated with, depending on the analysis. The nimConstants objects contain values of constants (e.g., covariate values, dimensions, etc.), organized as list elements. The nimInits objects are empty: the models sample initial values from the prior distributions. The species.info objects include a data frame with taxonomic information for each species, alligned (rows) with the observation data in nimData objects associated with the respective analysis (1, 2, and 3).


