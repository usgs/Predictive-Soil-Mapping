######################
## Random Forest script that includes:
## Extraction of covariates to points
## Confustion matrix creation
## Kappa calculation
## Prediction interval creation
## Cross Validation
## Most steps parallelized
######################
## Rock adjusted erodibility


# Workspace setup
# Install packages if not already installed

required.packages <- c("raster", "sp", "rgdal", "randomForest", "snow", "snowfall", "quantregForest","dplyr", "ggplot2","hexbin","mgcv")# might need snowfall
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
## Increase actuve memory useable by raster package: Windows only
memory.limit(500000)
rasterOptions(maxmemory = 1e+09, chunksize = 1e+08)


## Key Folder Locations
predfolder <- "/home/tnaum/data/BLMsoils/kw_2D_SSURGO_NASIS_SCD"
covfolder <- "/home/tnaum/data/UCRB_Covariates"

######## Load shapefile ##############
#setwd("C:/Models_active_work/UpCo/ECmodel_wLIMS")## FOlder with points
#shp.pts <-readOGR(".", "ec_12pre_ncss_LIMS_UPCO")
#point.proj <- projection(shp.pts)
## If not prj file and you know proj, can specify by name
#shp.proj <- CRS("+proj=longlat +datum=WGS84")

######## Get points for extraction if in table form ###########
# setwd("V:/PROJECTS/TRAVIS_NAUMAN/GIS_Archive/gSSURGO18/UCRB_gSSURGO18_mupolys_nasis")
# #pts <- read.delim("NCSS17_PSDA_rkFrags_ttab.txt") # If in delimited file other than csv
# pts <- readRDS("nasispts_gSSURGO18hor_ucrb_final.rds")
# ### Weed out points with imprecise coordinates ###
# pts$latnchar = nchar(abs(pts$ywgs84))
# pts$longnchar = nchar(abs(pts$xwgs84))
# ptsc = subset(pts, pts$latnchar > 5 & pts$longnchar > 6)
# ### Turn into spatial file
# shp.pts <- ptsc
# coordinates(shp.pts) <- ~ xwgs84 + ywgs84
# temp.proj <- CRS("+proj=longlat +datum=WGS84") ## specify projection
# projection(shp.pts) <- temp.proj


######## Load map clip boundary (if needed) ###########
setwd("/home/tnaum/Dropbox/USGS/BLM_projects/Utah_BLM_Salinity/Huc6_boundary")
polybound <- readOGR(".", "CO_River_watershed_Meade_alb")
#polybound <- spTransform(polybound, temp.proj)
## Now clip points and check with visualization
#shp.pts = shp.pts[polybound,]#clip by outer extent of all polybound features
#plot(polybound)
#plot(shp.pts, add=TRUE)
#shp.pts$depth = (shp.pts$hzdepb_r+shp.pts$hzdept_r)/2

######### Grid Prep #################
## Make list of grids
setwd(covfolder)
cov.grids <- list.files(pattern=".tif$")
## If points need to be matched up to grids ###
projgrid = raster(cov.grids[1])
## Or Make a stack of grids to extract all at once (for smaller datasets)
#cov.stack <- stack()
cov.proj <- projection(projgrid)
#shp.pts <- spTransform(shp.pts, CRS(cov.proj)) # project to match rasters

## Plot to ensure alignment bw points and rasters
# plot(projgrid)
# plot(shp.pts, add=TRUE)

## Parallelized extract: (larger datasets)
# rasterOptions(maxmemory = 1e+08)
# cpus = 30
# sfInit(parallel=TRUE, cpus=cpus)
# sfExport("shp.pts", "cov.grids")
# sfLibrary(raster)
# sfLibrary(rgdal)
# ov.lst <- sfLapply(cov.grids, function(i){try( raster::extract(raster(i), shp.pts) )}) 
# snowfall::sfStop()
# ov.lst <- as.data.frame(ov.lst)
# names(ov.lst) = tools::file_path_sans_ext(basename(cov.grids))
# ov.lst$DID <- seq.int(nrow(ov.lst))
# shp.pts$DID <- seq.int(nrow(shp.pts))
# pts.ext <- merge(as.data.frame(shp.pts),ov.lst, by="DID")

## Save points
#setwd("/home/tnaum/data/BLMsoils/EC_SSURGO_NASIS_2D")
#saveRDS(pts.ext, "UCRB_nasis_SSURGO_ec_r_ART_SG100_covarsc.rds")
## Updated extract for UCRB
pts.ext <- readRDS("/home/tnaum/data/gSSURGO18/UCRB_gSSURGO18_mupolys_nasis/UCRB_nasis_SSURGO_ART_SG100_covarsc_final.rds")

## Prep nasis training data for Random Forest
pts.ext$prop <- as.numeric(pts.ext$kwfact) ## UPDATE EVERY TIME
prop <- "kwfact" ## Dependent variable
pts.ext$tid <- "nasis"

## Remove lab pedons from nasis data to allow for evaluation at end
scd.pts <- read.delim("/home/tnaum/data/BLMsoils/kw_2D_SSURGO_NASIS_SCD/NCSS17_kf_calcs_ttab.txt")
pts.ext$LocID <- paste(pts.ext$xwgs84, pts.ext$ywgs84, sep = "")
nasislocs <- unique(pts.ext$LocID)
scd.pts$LocID <- paste(scd.pts$longitude_decimal_degrees, scd.pts$latitude_decimal_degrees, sep = "")
scdlocs <- unique(scd.pts$LocID)
alllocs <- c(nasislocs,scdlocs)
locdups <- subset(alllocs, duplicated(alllocs))
pts.ext <- subset(pts.ext, !(pts.ext$LocID %in% locdups))
### SCD prep: Weed out points with imprecise coordinates ###
scd.pts$latnchar <- nchar(abs(scd.pts$latitude_decimal_degrees))
scd.pts$longnchar <- nchar(abs(scd.pts$longitude_decimal_degrees))
scd.pts <- subset(scd.pts, scd.pts$latnchar > 5 & scd.pts$longnchar > 6)
### Turn into spatial file
coordinates(scd.pts) <- ~ longitude_decimal_degrees + latitude_decimal_degrees
temp.proj <- CRS("+proj=longlat +datum=WGS84") ## specify projection
projection(scd.pts) <- temp.proj
######## Load map clip boundary (if needed) ###########
cov.proj <- projection(projgrid)
polybound <- spTransform(polybound, cov.proj)
scd.pts <- spTransform(scd.pts, cov.proj)
## Now clip points and check with visualization
scd.pts <- scd.pts[polybound,]#clip by outer extent of all polybound features
plot(polybound)
plot(scd.pts, add=TRUE)
## Further SCD prep
## Extract covariates for prediction onto SCD points
# setwd(covfolder)
# rasterOptions(maxmemory = 1e+08)
# cpus = 30
# sfInit(parallel=TRUE, cpus=cpus)
# sfExport("scd.pts", "cov.grids")
# sfLibrary(raster)
# sfLibrary(rgdal)
# ov.lst <- sfLapply(cov.grids, function(i){
#   #rasterOptions(maxmemory = 1e+08,chunksize = 1e+07)
#   try(raster::extract(raster(i), scd.pts) )})
# snowfall::sfStop()
# ov.lst <- as.data.frame(ov.lst)
# names(ov.lst) = tools::file_path_sans_ext(basename(cov.grids))
# ov.lst$DID <- seq.int(nrow(ov.lst))
# scd.pts$DID <- seq.int(nrow(scd.pts))
# scd.pts.ext <- merge(as.data.frame(scd.pts),ov.lst, by="DID")
setwd(predfolder)
# saveRDS(scd.pts.ext, paste("SCD", prop, "extracted_nasisSSURGO_ART_SG100.rds",sep="_"))
scd.pts.ext <- readRDS(paste("SCD", prop, "extracted_nasisSSURGO_ART_SG100.rds",sep="_"))

## Raster Sample locations for RelPI stats
pred.pts.ext <- readRDS("/home/tnaum/data/BLMsoils/UCRB_Summary_pts/UCRB_predpts_ART_SG100_covarsc.rds") ## prediction summary locations

## SCD prep for RF
scd.pts.ext$kf_s <- as.character(scd.pts.ext$kf_s)
scd.pts.ext$kf_s <- as.numeric(scd.pts.ext$kf_s)
## Bring in table to adjust kf to kw
kadjtab <- read.delim("/home/tnaum/data/BLMsoils/kw_2D_SSURGO_NASIS_SCD/kw_correction_ttab.txt")
# GAM fit and check
x <- seq(1,100)
x1 <- seq(1,100)
y1 <- seq(0.01,1,by=0.01)
xgamexh = kadjtab$rkvol
ygamexh = kadjtab$kwadj
b1 <- gam(ygamexh~s(xgamexh), method="REML",select=TRUE)
y <- predict(b1, data.frame(xgamexh = x))
plot(ygamexh~xgamexh, ylim = c(0,1), xlim = c(0,100), xlab = "% Rock by vol", ylab = "Kf adjustment to kw")
lines(x,y)
scd.pts.ext$kadj <- predict(b1, data.frame(xgamexh = scd.pts.ext$vfg2))
scd.pts.ext$kw_s <- scd.pts.ext$kadj * scd.pts.ext$kf_s
scd.pts.ext$prop <- scd.pts.ext$kw_s ## UPDATE everytime!
scd.pts.ext$tid <- "scd"

##### Loop to train and predict properties for all depths
depths <- c(0,5,15,30,60,100,200)
for(d in depths){
pts.extc <- subset(pts.ext, as.numeric(pts.ext$hzdept_r) <= d & as.numeric(pts.ext$hzdepb_r) > d) # subset to chosen depth
pedonLocations <- unique(pts.extc$LocID) # if length differs from # of rows in pts, there are duplicates
pts.extc <- subset(pts.extc, !duplicated(pts.extc[c("LocID")])) #removes duplicates
ptspred.list <- gsub(".tif","", cov.grids)# Take .tif off of the grid list to just get the variable names
ptspred.list <- c(ptspred.list,"prop","tid") #Add dependent variable
pts.extc <- pts.extc[c(ptspred.list)]## Or create a specific list of dependent variable and covariate names to use
pts.extc <- na.omit(pts.extc)# Remove any record with NA's (in any column - be careful)
scd.pts.ext.dcm <- subset(scd.pts.ext, as.numeric(scd.pts.ext$hzn_top) <= d & as.numeric(scd.pts.ext$hzn_bot) > d)
# Remove any duplicates
scd.pts.ext.dcm <- subset(scd.pts.ext.dcm, !duplicated(scd.pts.ext.dcm[c("LocID")])) #removes duplicates
scd.pts.ext.dcm <- scd.pts.ext.dcm[c(ptspred.list)]
scd.pts.ext.dcm <- na.omit(scd.pts.ext.dcm)
pts.extcc <- rbind(pts.extc, scd.pts.ext.dcm)
xtrain <- as.matrix(pts.extcc[c(gsub(".tif","", cov.grids))])
ytrain <- c(as.matrix(pts.extcc[c("prop")]))
#logytrain <- log(ytrain+1)
# sqrtytrain <- sqrt(ytrain)
varrange <- as.numeric(quantile(pts.extcc$prop, probs=c(0.975), na.rm=T)-quantile(pts.extcc$prop, probs=c(0.025),na.rm=T)) ## TRANSFORM IF NEEDED!

############### Build quantile Random Forest
# Qsoiclass <- quantregForest(x=xtrain, y=ytrain, importance=TRUE, ntree=100, keep.forest=TRUE, nthreads = 30)
# #soiclass = randomForest(ec_12pre ~ ., data = ptsc, importance=TRUE, proximity=FALSE, ntree=100, keep.forest=TRUE)
# soiclass <- Qsoiclass
# class(soiclass) <- "randomForest"
# #soiclass## Get oob error
# ## Linear Adjustment for bias
# pts.extcc$trainpreds <- predict(soiclass, newdata=xtrain)
# pts.extcc$prop_t <- pts.extcc$prop ## TRANSFORM IF NEEDED
# attach(pts.extcc)
# rf_lm_adj <- lm(prop_t ~ trainpreds)
# detach(pts.extcc)
# pts.extcc$trainpredsadj <- predict(rf_lm_adj, newdata=pts.extcc)
## plot model performance stuff
# plot(ytrain~predict(soiclass)) ## OOB plot
# x1 <-c(-100,0,100,10000,100000000)
# y1 <-c(-100,0,100,10000,100000000)
# lines(x1,y1, col = 'red')#1:1 line
# plot(ytrain~predict(soiclass, newdata=xtrain)) #Fit plot
# lines(x1,y1, col = 'red')#1:1 line
# plot(ytrain~pts.extcc$trainpredsadj) #Fit plot
# lines(x1,y1, col = 'red')#1:1 line
# varImpPlot(soiclass)
# setwd(predfolder)
# # saveRDS(Qsoiclass, paste("Qsoiclass_RFmodel", prop, d, "cm_nasisSSURGO_ART_SG100.rds",sep="_"))
# # saveRDS(rf_lm_adj, paste("rflmadj_RFmodel",prop, d, "cm_nasisSSURGO_ART_SG100.rds",sep="_"))
# Qsoiclass <- readRDS(paste("Qsoiclass_RFmodel", prop, d, "cm_nasisSSURGO_ART_SG100.rds",sep="_"))
# rf_lm_adj <- readRDS(paste("rflmadj_RFmodel",prop, d, "cm_nasisSSURGO_ART_SG100.rds",sep="_"))


## Reference covar rasters to use in prediction
# setwd(covfolder)
# rasterOptions(maxmemory = 1e+09,chunksize = 2e+08)# maxmemory = 1e+09,chunksize = 1e+08 for soilmonster
# rasters=stack(cov.grids)
# #rasters = setMinMax(brick(rasters))
# #names(rasters)
# 
# ## Predict onto covariate grid
# setwd(predfolder)
# ## Parallelized predict
# beginCluster(30,type='SOCK')
# predl <- clusterR(rasters, predict, args=list(model=Qsoiclass,what=c(0.025)),progress="text")
# predh <- clusterR(rasters, predict, args=list(model=Qsoiclass,what=c(0.975)),progress="text")
# Sys.time()
# pred <- clusterR(rasters, predict, args=list(model=soiclass),progress="text")
# Sys.time()
# names(pred) <- "trainpreds"
# ## Linear Adjustment
# predlm <- clusterR(pred, predict, args=list(model=rf_lm_adj),progress="text")
# ## PI widths
# s <- stack(predh,predl)
# PIwidth.fn <- function(a,b) {
#   ind <- a-b
#   return(ind)
# }
# PIwidth <- clusterR(s, overlay, args=list(fun=PIwidth.fn),progress = "text")
# # Determine 95% interquantile range of original training data for horizons that include the depth being predicted
# PIrelwidth.fn <- function(a,b) {
#   ind <- (a-b)/varrange
#   return(ind)
# }
# PIrelwidth <- clusterR(s, overlay, args=list(fun=PIrelwidth.fn),progress = "text",export='varrange')
# ## Back transformation Stuff
# # bt.fn <- function(x) {
# #   ind <- (exp(x))-1 #If a backtransform is needed 10^(x) or exp(x) or ^2
# #   return(ind)
# # }
# # predh_bt <- clusterR(predh, calc, args=list(fun=bt.fn),progress='text')
# # predl_bt <- clusterR(predl, calc, args=list(fun=bt.fn),progress='text')
# # pred_bt <- clusterR(pred, calc, args=list(fun=bt.fn),progress='text')
# # s_bt <- stack(predh_bt,predl_bt)
# # PIwidth_bt.fn <- function(a,b) {
# #   ind <- a-b
# #   return(ind)
# # }
# # PIwidth_bt <- clusterR(s_bt, overlay, args=list(fun=PIwidth_bt.fn),progress = "text")
# # ## If transformed, use the following code for PI width prep steps
# # PIrelwidth_bt.fn <- function(a,b) {
# #   ind <- (a-b)/varrange
# #   return(ind)
# # }
# # PIrelwidth_bt <- clusterR(s_bt, overlay, args=list(fun=PIrelwidth_bt.fn),progress = "text", export='varrange')
# 
# endCluster()
# ## Write new geotiff files
# setwd(predfolder)
# ## Untranformed code block
# writeRaster(predlm, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# writeRaster(predl, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_l.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# writeRaster(predh, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_h.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# writeRaster(PIrelwidth, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_relwidth.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# # writeRaster(PIwidth, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_width.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# # ## Transformed code block
# # writeRaster(pred_bt, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_bt_ART_SG100covs.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# # writeRaster(predl_bt, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_l_bt.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# # writeRaster(predh_bt, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_h_bt.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# # writeRaster(PIwidth_bt, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_width_bt.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# # writeRaster(PIrelwidth_bt, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_relwidth_bt.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")



################### Manual Cross validation ################################
pts.extcvm <- pts.extcc
nfolds <- 10
pts.extcvm$folds <- sample.int(nfolds,size =length(pts.extcvm[,1]),replace=T)
pts.extcvm$prop_t <- pts.extcvm$prop ## UPDATE: tranform if needed else just create new version of prop
formulaStringCVm <- as.formula(paste('prop_t ~', paste(gsub(".tif","", cov.grids), collapse="+")))
#for (g in seq(nfolds)){
CV_factorRF <- function(g,pts.extcvm, formulaStringCVm){
  traindf <- subset(pts.extcvm, pts.extcvm$folds != g)
  testdf <- subset(pts.extcvm, pts.extcvm$folds == g)
  xtrain.t <- as.matrix(traindf[c(gsub(".tif","", cov.grids))])
  ytrain.t <- c(as.matrix(traindf$prop_t))
  rf.pcv <- quantregForest(x=xtrain.t, y=ytrain.t, importance=TRUE, ntree=100, keep.forest=TRUE)
  rf.pcvc <- rf.pcv
  class(rf.pcvc) <- "randomForest"
  traindf$pcvpredpre <- predict(rf.pcvc, newdata=traindf)
  testdf$pcvpredpre <- predict(rf.pcvc, newdata=testdf)
  #traindf$pcvpredpre <- predict(rf.pcv, newdata=traindf, what=c(0.5)) ## If median is desired
  #testdf$pcvpredpre <- predict(rf.pcv, newdata=testdf,, what=c(0.5)) ## If median is desired
  testdf$pcvpredpre.025 <- predict(rf.pcv, newdata=testdf, what=c(0.025))
  testdf$pcvpredpre.975 <- predict(rf.pcv, newdata=testdf, what=c(0.975))
  attach(traindf)
  lm.pcv <- lm(prop_t~pcvpredpre)
  detach(traindf)
  testdf$pcvpred <- predict(lm.pcv, newdata=testdf)
  return(testdf)
}
snowfall::sfInit(parallel=TRUE, cpus=nfolds)
snowfall::sfExport("pts.extcvm","formulaStringCVm","CV_factorRF","cov.grids")
snowfall::sfLibrary(randomForest)
snowfall::sfLibrary(quantregForest)
pts.extpcv <- snowfall::sfLapply(1:nfolds, function(g){CV_factorRF(g, pts.extcvm=pts.extcvm,formulaStringCVm=formulaStringCVm)})
snowfall::sfStop()
pts.extpcv <- plyr::rbind.fill(pts.extpcv)
pts.extpcv$pcvpred <- as.numeric(pts.extpcv$pcvpred)
## PCV statistics
cvp.RMSE = sqrt(mean((pts.extpcv$prop_t - pts.extpcv$pcvpred)^2, na.rm=TRUE))
cvp.Rsquared = 1-var(pts.extpcv$prop_t - pts.extpcv$pcvpred, na.rm=TRUE)/var(pts.extpcv$prop_t, na.rm=TRUE)
## Back transformed: create pcvpred_bt even if not tranformed for cv.depth function
pts.extpcv$pcvpred_bt <- pts.extpcv$pcvpred
cvp.RMSE_bt = sqrt(mean((pts.extpcv$prop - pts.extpcv$pcvpred_bt)^2, na.rm=TRUE))
cvp.Rsquared_bt = 1-var(pts.extpcv$prop - pts.extpcv$pcvpred_bt, na.rm=TRUE)/var(pts.extpcv$prop, na.rm=TRUE)
## PCV stats for scd points
pts.extpcv.scd <- subset(pts.extpcv, pts.extpcv$tid == "scd")
cvp.RMSE.scd <- sqrt(mean((pts.extpcv.scd$prop_t - pts.extpcv.scd$pcvpred)^2, na.rm=TRUE))
cvp.Rsquared.scd <- 1-var(pts.extpcv.scd$prop_t - pts.extpcv.scd$pcvpred, na.rm=TRUE)/var(pts.extpcv.scd$prop_t, na.rm=TRUE)
## PCV stats for scd points: backtransformed
cvp.RMSE.scd_bt <- sqrt(mean((pts.extpcv.scd$prop - pts.extpcv.scd$pcvpred_bt)^2, na.rm=TRUE))
cvp.Rsquared.scd_bt <- 1-var(pts.extpcv.scd$prop - pts.extpcv.scd$pcvpred_bt, na.rm=TRUE)/var(pts.extpcv.scd$prop, na.rm=TRUE)
## Number of SCD samples
n_scd <- length(pts.extpcv.scd[,1])
## RPI
pts.extpcv$prop_bt <- pts.extpcv$prop_t # UPDATE: backtransform if necessary. Used for PICP and to characterize backtransformation bias
pts.extpcv$pcvpredpre.025_bt <- pts.extpcv$pcvpredpre.025 # UPDATE: backtransform if necessary
pts.extpcv$pcvpredpre.975_bt <- pts.extpcv$pcvpredpre.975 # UPDATE: backtransform if necessary
pts.extpcv$abs.resid <- abs(pts.extpcv$prop - pts.extpcv$pcvpred_bt)
pts.extpcv$RPI <- (pts.extpcv$pcvpredpre.975_bt - pts.extpcv$pcvpredpre.025_bt)/varrange
# plot(pts.extpcv$abs.resid~pts.extpcv$RPI) # Quick look at relationship
## Summarize RPI and residuals
pts.extpcv$rel.abs.resid <- pts.extpcv$abs.resid/varrange
RPI.cvave <- mean(pts.extpcv$RPI)
RPI.cvmed <- median(pts.extpcv$RPI)
rel.abs.res.ave <- mean(pts.extpcv$rel.abs.resid)
rel.abs.res.med <- median(pts.extpcv$rel.abs.resid)
pts.extpcv$BTbias <- pts.extpcv$prop_bt - pts.extpcv$prop
BTbias.abs.max <- max(abs(pts.extpcv$BTbias))
BTbias.ave <- mean(pts.extpcv$BTbias)
PICP <- sum(ifelse(pts.extpcv$prop_bt <= pts.extpcv$pcvpredpre.975_bt & pts.extpcv$prop_bt >= pts.extpcv$pcvpredpre.025_bt,1,0))/length(pts.extpcv[,1])
## Create PCV table
CVdf <- data.frame(cvp.RMSE, cvp.Rsquared, cvp.RMSE_bt, cvp.Rsquared_bt, cvp.RMSE.scd, cvp.Rsquared.scd, cvp.RMSE.scd_bt, cvp.Rsquared.scd_bt,n_scd,RPI.cvave,RPI.cvmed,PICP,rel.abs.res.ave,rel.abs.res.med,BTbias.abs.max,BTbias.ave)
names(CVdf) <- c("cvp.RMSE","cvp.Rsquared","cvp.RMSE_bt", "cvp.Rsquared_bt", "cvp.RMSE.scd", "cvp.Rsquared.scd", "cvp.RMSE.scd_bt", "cvp.Rsquared.scd_bt","n_scd","RPI.CVave","RPI.CVmed","PICP","rel.abs.res.ave","rel.abs.res.med","BTbias.abs.max","BTbias.ave")
setwd(predfolder)
write.table(CVdf, paste("PCVstats", prop, d, "cm_nasisSSURGO_ART_SG100.txt",sep="_"), sep = "\t", row.names = FALSE)
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt)
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,10),ylim=c(0,10))
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,5),ylim=c(0,5))
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,1),ylim=c(0,1))
# plot(pts.extpcv$prop_t~pts.extpcv$pcvpred)
#lines(x1,y1, col = 'red')#1:1 line
## CV plots
# viri <- c("#440154FF", "#39568CFF", "#1F968BFF", "#73D055FF", "#FDE725FF") # color ramp
# gplt.dcm.2D.CV <- ggplot(data=pts.extpcv, aes(prop, pcvpred_bt)) +
#   stat_binhex(bins = 30) + geom_abline(intercept = 0, slope = 1,lwd=1)  + #xlim(0,100) + ylim(0,100) +
#   theme(axis.text=element_text(size=8), legend.text=element_text(size=10), axis.title=element_text(size=10),plot.title = element_text(size=10,hjust=0.5)) +
#   xlab("Measured") + ylab("CV Prediction") + scale_fill_gradientn(name = "log(Count)", trans = "log", colours = rev(viri)) +
#   ggtitle(paste("Cross val", prop, d, "cm",sep=" "))
# gplt.dcm.2D.CV
## Save Cross validation graph and data for future plotting
saveRDS(pts.extpcv, paste(prop, "cvlm_preds_2D", d, "cm_nasisSSURGO_ART_SG100.rds", sep="_"))

##### Relative PI Interval statistics for different depths
## Add new packages for foreach work
# required.packages <- c( "doSNOW","foreach", "itertools")# might need snowfall
# new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# lapply(required.packages, require, character.only=T)
# rm(required.packages, new.packages)
# pred.pts.extdf <- as.data.frame(pred.pts.ext)
# pred.pts.extdf <- na.omit(pred.pts.extdf)
# depthPI <- d
# relPI.depth <- function(d, pts.ext, pred.pts.extdf,Qsoiclass){
#   num_splits <- 30 # number of cpus to use
#   cl <- makeCluster(num_splits)
#   registerDoSNOW(cl)
#   pred.pts.extdf$depth<-d
#   DF_pred_l<-
#     foreach(splt=isplitRows(pred.pts.extdf, chunks=num_splits),.combine=c, .packages=c("quantregForest", "randomForest")) %dopar% {
#       predict(Qsoiclass,newdata=splt, what=c(0.025))
#     }
#   DF_pred_h<-
#     foreach(splt=isplitRows(pred.pts.extdf, chunks=num_splits),.combine=c, .packages=c("quantregForest", "randomForest")) %dopar% {
#       predict(Qsoiclass,newdata=splt, what=c(0.975))
#     }
#   stopCluster(cl)
#   ## Now compute stats for PI intervals
#   ## Untransform
#   DF_pred_l_bt <- DF_pred_l # Update for all runs, just don't use back transform fn if it wasn't used
#   DF_pred_h_bt <- DF_pred_h # Update for all runs, just don't use back transform fn if it wasn't used
#   df.PIwidth <- DF_pred_h_bt-DF_pred_l_bt
#   df.varrange <- as.numeric(quantile(pts.extcc$prop, probs=c(0.975),na.rm=T)-quantile(pts.extcc$prop, probs=c(0.025),na.rm=T))##Change soil factor name
#   df.PIrelwidth <- (DF_pred_h_bt-DF_pred_l_bt)/df.varrange
#   ## Stats (untransformed)
#   PIdepth <- d
#   mean <- mean(df.PIwidth)
#   median <- median(df.PIwidth)
#   q25 <- unname(quantile(df.PIwidth, probs=c(0.25),na.rm=T))
#   q75 <- unname(quantile(df.PIwidth, probs=c(0.75),na.rm=T))
#   sd <-sd(df.PIwidth)
#   max <- max(df.PIwidth)
#   min <- min(df.PIwidth)
#   relmean <- mean(df.PIrelwidth)
#   relmedian <- median(df.PIrelwidth)
#   relq25 <- unname(quantile(df.PIrelwidth, probs=c(0.25),na.rm=T))
#   relq75 <- unname(quantile(df.PIrelwidth, probs=c(0.75),na.rm=T))
#   relsd <-sd(df.PIrelwidth)
#   relmax <- max(df.PIrelwidth)
#   relmin <- min(df.PIrelwidth)
#   PIdf <- data.frame(PIdepth, mean, median, q25,q75,sd,max,min,relmean, relmedian, relq25,relq75,relsd,relmax,relmin)
#   names(PIdf) <- c("PIdepth","mean","median", "q25","q75","sd","max","min","relmean","relmedian", "relq25","relq75","relsd","relmax","relmin")
#   return(PIdf)
# }
# relPI.list <- lapply(depthPI, function(d){try(relPI.depth(d,pts.extcc, pred.pts.extdf,Qsoiclass))})
# relPI.df <- relPI.list[[1]]
# relPI.df <- relPI.df[FALSE,]
# for(i in seq(1:length(relPI.list))){
#   newrow <- relPI.list[[i]]
#   if(class(newrow)=="data.frame"){
#     relPI.df <- rbind(relPI.df, newrow)
#   }
#   print(paste("Done with ", i, sep=""))
# }
# ## detach parallel related packages that can interfere with clusterR
# detach(package:doSNOW)
# detach(package:foreach)
# detach(package:itertools)
# # Save table to folder
# setwd(predfolder)
# write.table(relPI.df, paste("relPI", prop, d, "cm_nasisSSURGO_ART_SG100.txt",sep="_"), sep = "\t", row.names = FALSE)
# ## Print out loop status
print(paste("Done with depth", d, sep=" "))
gc()
} # End of depth loop


############# Masking water pixels out ############
# nlcd <- raster("/home/tnaum/data/UCRB_Covariates/NLCDcl.tif")
# beginCluster(30,type='SOCK')
## Make a mask raster
# mask_fn <- function(nlcd){ind <- ifelse(nlcd!=11,1,NA)
#   return(ind)
# }
# mask <- clusterR(nlcd, calc, args=list(fun=mask_fn),progress='text')
# endCluster()
# plot(mask)
# writeRaster(mask, overwrite=TRUE,filename="/home/tnaum/data/BLMsoils/nlcd_watermask.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text",datatype='INT1U')
# rm(mask)
## Now set up a list of rasters and function to mask out water
rasterOptions(maxmemory = 1e+09,chunksize = 1e+08)
setwd("/home/tnaum/data/BLMsoils/kw_2D_SSURGO_NASIS_SCD")
grids <- list.files(pattern=".tif$")
mskfn <- function(rast,mask){
  ind <- rast*mask
  ind[ind<0]<-0 # to bring the slighly negative predictions back to zero
  return(ind)
}
## par list apply fn
watermask_fn <- function(g){
  setwd("/home/tnaum/data/BLMsoils/kw_2D_SSURGO_NASIS_SCD")
  rast <- raster(g)
  names(rast) <- "rast"
  setwd("/home/tnaum/data/BLMsoils")
  mask <- raster("/home/tnaum/data/BLMsoils/nlcd_watermask.tif")
  h2ostk <- stack(rast,mask)
  setwd("/home/tnaum/data/BLMsoils/kw_2D_SSURGO_NASIS_SCD/masked")
  overlay(h2ostk,fun=mskfn,progress='text',filename=g, options=c("COMPRESS=DEFLATE", "TFW=YES"))
  gc()
}
snowfall::sfInit(parallel=TRUE, cpus=30)
snowfall::sfExport("watermask_fn","mskfn","grids")
snowfall::sfLibrary(raster)
Sys.time()
snowfall::sfLapply(grids, function(g){watermask_fn(g)})
Sys.time()
snowfall::sfStop()

