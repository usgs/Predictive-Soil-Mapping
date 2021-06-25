# Development and Mapping of Ecological Site Groups

This folder is a repository for code used in development of Ecological Site Groups (ESGs) for the for rangelands of the Upper Colorado River Basin. This work is documented in the following paper. 

Nauman, T.W., Burch, S., Humphries, J.T., Knight, A.C., Duniway, M.C., In Revision. A quantitative soil-geomorphic framework for developing and mapping Ecological Site Groups. Submitted to Rangeland Ecology and Management 12/15/20. 

Please note that this software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

## Overview

<p align="left">
  <img src="./data/Fig_FlowChart_white_repo.png" width="1000" title="Development Overview">
</p>
Figure 1. Flow chart showing the variety of data sources and analyses employed for both creation and mapping of soil geomorphic units (SGUs) and Ecological Site Groups (ESGs). Development started with Ecological Site Description (ESD) data queried from the Ecosystem Dynamics Interpretative Tool (EDIT), the Soil Survey Geographic (SSURGO) database, and previous work (Duniway et al., 2016) to initially create a SGU key by optimizing reference production differentiation through evaluating different candidate non-parametric MANOVA (NPMANOVA) models. The developed key was used to classify National Soil Information System soil pedon field data for use in producing a digital soil map of SGUs. Final Ecological Site Groups (ESGs) were determined by testing combinations of thresholds in gridded climate data with SGU classification of ESDs in SSURGO.  Gridded climate data was averaged by SGU-linked SSURGO map units to then test for optimal breaks using a NPMANOVA model optimization process that considered both reference production data and documented ecological states from a generalized state and transition model (STM) table. The chosen combination of climate thresholds with SGU classes was then utilized in combining the SGU map with gridded climate data to create a final ESG map. Finally, rangeland monitoring vegetation cover data from the Bureau of Land Management Assessment Inventory and Monitoring (AIM) and Natural Resources Conservation Service Natural Resource Inventory (NRI) were overlaid on the maps of SGUs and ESGs to qualitatively test and assess the final classification.

### Reference
Duniway, M. C., Nauman, T. W., Johanson, J. K., Green, S., Miller, M. E., Williamson, J. C., and Bestelmeyer, B. T., 2016, Generalizing Ecological Site Concepts of the Colorado Plateau for Landscape-Level Applications: Rangelands, v. 38, no. 6, p. 342-349.

## Steps and associated scripts

### The SGU development
[SGU_climate_PERMANOVA_SSURGO.r](https://github.com/usgs/Predictive-Soil-Mapping/blob/537f3b5e6d8628c13874e22dedc61f0c1f57a3b8/ESGs/SGU_climate_PERMANOVA_SSURGO.r)

### Climate Optimization
[ClimateOptimization_v2b_nested.R](https://github.com/usgs/Predictive-Soil-Mapping/blob/4184b3a8e6d3134b2c4807b92ac48111668f3435/ESGs/ClimateOptimization_v2b_nested.R)

[ClimateOptimization_v1b_simultaneous.R](https://github.com/usgs/Predictive-Soil-Mapping/blob/4184b3a8e6d3134b2c4807b92ac48111668f3435/ESGs/ClimateOptimization_v1b_simultaneous.R)

[ESG_STM_ClimateOptimization_b.R](https://github.com/usgs/Predictive-Soil-Mapping/blob/4184b3a8e6d3134b2c4807b92ac48111668f3435/ESGs/ESG_STM_ClimateOptimization_b.R)

### Mapping SGUs and ESGs
[Digital Soil Mapping of SGUs](https://github.com/usgs/Predictive-Soil-Mapping/blob/4184b3a8e6d3134b2c4807b92ac48111668f3435/ESGs/RFforeach_categor_with_pt_extract_Opti_parallel_rank123_ADJsmote_InfSp_sgu_30mINT.r)

[sgu_to_esg.R](https://github.com/usgs/Predictive-Soil-Mapping/blob/4184b3a8e6d3134b2c4807b92ac48111668f3435/ESGs/sgu_to_esg.R)

### Figures for publication and exploration
[ESG_UCRB_figs_tabs.r](https://github.com/usgs/Predictive-Soil-Mapping/blob/4184b3a8e6d3134b2c4807b92ac48111668f3435/ESGs/ESG_UCRB_figs_tabs.r)

### Support
Travis Nauman, PhD (he/him/his)

Soil Scientist

US Geological Survey

Southwest Biological Science Center

Moab, UT

tnauman@usgs.gov



