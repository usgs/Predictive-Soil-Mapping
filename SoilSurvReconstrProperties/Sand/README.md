# Sand Content

This folder catalogues models and inputs for sand maps

Files included in this folder include:

Files with "Qsoiclass" are the quantile regression forest R objects for each depth.

Files with "cvlm_preds" are the cross validation (CV) instances that include all training data, extracted covariate values, CV predictions, CV prediction intervals and relative prediction intervals. Since the NASIS pedon data is private, we could not provide locational data for those instances, but the points are identified as from the soil characterization database (SCD) laboratory or from the NASIS database in the 'tid' field.

Files with "rflmadj" are the regression forest averaging bias linear adjustment model R objects for each depth.

The QuantRFmodel_LmAdj_ File is the R script with the full prediction workflow.
