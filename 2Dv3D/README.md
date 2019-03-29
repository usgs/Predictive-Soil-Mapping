# 2D versus 3D Predictive Soil Property Mapping

This folder includes R statistical programing language code for approaches of predictive mapping of soil properties that vary in how soil depth is dealt with in modeling. The 2D approach is one where each depth of interest in modeled separately in the spatial (2D) realm. The 3D approach refers to the model including depth as a independent covariate data used to build one model that can predict soil properties at any depth across a landscape (3D). The models used for predictions are quantile regression forest (QRF) implementions of the random forest machine learning model. The QRF models help discern the links between environmental raster data presumed to represent soil forming factors (e.g. DEMs, Landsat) to soil property variation at field observations. 

This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

Two scripts are provided that develop a full worflow taking field soil observations, extracting environmental covariates, building a random forest regression models, creating prediction and uncertainty rasters, and validating the model. The workflows allow automated prediction of each soil property for what ever depths a user may choose by looping through desired depths. Scripts were developed for both a workflow where the depth of the soil is included as a covariate in model building (3D approach), and for a workflow where a different model is built for each depth (2D) workflow. The scripts include outputs for cross validation accuracy and global prediction interval summarization. The scripts are set up to run pH models as are, but can be modified following internal commenting to creat models for soil organic carbon, % fine sand + very fine sand, and 1:2 H2O soil electrical conductivity as documentation for the following paper submitted to the journal Geoderma:

Nauman, T.W., Duniway, M.C., In Press. Relative prediction intervals reveal larger uncertainty in 3D approaches to predictive digital soil mapping of soil properties with legacy data. For: Geoderma.

Files:

Scripts:

QuantRFmodel_2D.R: 2D approach workflow script

QuantRFmodel_3D.R: 3D approach workflow script

Data:

cop_ncss17SOC__covarsc.txt: Training points for soil organic carbon model with environmental covariates already extracted.

cop_ncss17_FS_VFS_pct_covarsc.txt: Training points for % wt fine sand + very fine sand model with environmental covariates already extracted.

cop_ncss17pH_h20_covarsc.txt: Training points for soil 1:1 H2O pH with environmental covariates already extracted.

cop_ncss17wLIMS_ec12_covarsc.txt: Training points for soil 1:2 H2O electrical conductivity with environmental covariates already extracted.

The USGS data release (soil maps) citation

Nauman, T.W. and Duniway, M.C., 2019, Predictive maps of 2D and 3D surface soil properties and associated uncertainty for the Upper Colorado River Basin, USA: U.S. Geological Survey data release, https://doi.org/10.5066/P9YBAKC2.

Beta maps used for peer review produced from these scripts for the paper are also available at the following repository:

http://doi.org/10.5281/zenodo.2545882

Includes updated 2D EC RPI raster after correcting an error with the 95% interquantile range used in calculations
Travis Nauman, PhD, Soil Scientist, Moab, UT tnauman@usgs.gov, naumi421@gmail.com
