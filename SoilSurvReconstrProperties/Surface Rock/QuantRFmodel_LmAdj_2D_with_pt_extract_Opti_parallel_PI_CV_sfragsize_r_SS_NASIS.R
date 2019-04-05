######################
## Random Forest script that includes:
## Extraction of covariates to points
## Prediction interval creation
## Cross Validation
## Most steps parallelized
######################
## Surface rock size (dominant)


# Workspace setup
# Install packages if not already installed

required.packages <- c("raster", "sp", "rgdal", "randomForest", "snow", "snowfall", "quantregForest","dplyr", "ggplot2","hexbin")# might need snowfall
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
## Increase actuve memory useable by raster package: Windows only
memory.limit(500000)
rasterOptions(maxmemory = 1e+09, chunksize = 1e+08)


## Key Folder Locations
predfolder <- "/home/tnaum/data/BLMsoils/Surf_Frags_SSURGO_NASIS"
covfolder <- "/home/tnaum/data/UCRB_Covariates"

######## Get points for extraction if in table form ###########
setwd("/home/tnaum/data/gSSURGO18/ESGs_Restr_SurfFrags_gSSURGO18_NASIS/SurfFrags")
pts <- readRDS("UCRB_surfFrags_NASIS_SSURGO18.rds")
### Weed out points with imprecise coordinates ###
pts$latnchar = nchar(abs(pts$ywgs84))
pts$longnchar = nchar(abs(pts$xwgs84))
ptsc = subset(pts, pts$latnchar > 5 & pts$longnchar > 6)
### Turn into spatial file
shp.pts <- ptsc
coordinates(shp.pts) <- ~ xwgs84 + ywgs84
temp.proj <- CRS("+proj=longlat +datum=WGS84") ## specify projection
projection(shp.pts) <- temp.proj

######## Load map clip boundary (if needed) ###########
setwd("/home/tnaum/Dropbox/USGS/BLM_projects/Utah_BLM_Salinity/Huc6_boundary")
polybound <- readOGR(".", "CO_River_watershed_Meade_alb")
polybound <- spTransform(polybound, temp.proj)
# Now clip points and check with visualization
shp.pts = shp.pts[polybound,]#clip by outer extent of all polybound features
plot(polybound)
plot(shp.pts, add=TRUE)

######### Grid Prep #################
## Make list of grids
setwd(covfolder)
cov.grids <- list.files(pattern=".tif$")
## If points need to be matched up to grids ###
projgrid = raster(cov.grids[1])
## Or Make a stack of grids to extract all at once (for smaller datasets)
cov.proj <- projection(projgrid)
shp.pts <- spTransform(shp.pts, CRS(cov.proj)) # project to match rasters

## Plot to ensure alignment bw points and rasters
plot(projgrid)
plot(shp.pts, add=TRUE)

## Parallelized extract: (larger datasets)
rasterOptions(maxmemory = 1e+08)
cpus = 30
sfInit(parallel=TRUE, cpus=cpus)
sfExport("shp.pts", "cov.grids")
sfLibrary(raster)
sfLibrary(rgdal)
ov.lst <- sfLapply(cov.grids, function(i){try( raster::extract(raster(i), shp.pts) )})
snowfall::sfStop()
ov.lst <- as.data.frame(ov.lst)
names(ov.lst) = tools::file_path_sans_ext(basename(cov.grids))
ov.lst$DID <- seq.int(nrow(ov.lst))
shp.pts$DID <- seq.int(nrow(shp.pts))
pts.ext <- merge(as.data.frame(shp.pts),ov.lst, by="DID")

## Save points
setwd(predfolder)
saveRDS(pts.ext, "UCRB_surfFrags_NASIS_SSURGO18_ART_SG100_covs.rds")
## Or use pre-prepared table
# pts.ext <- readRDS("/home/tnaum/data/gSSURGO18/ESGs_Restr_SurfFrags_gSSURGO18_NASIS/SurfFrags/UCRB_surfFrags_NASIS_SSURGO18_ART_SG100_covs.rds")

## Prep nasis training data for Random Forest
pts.ext$prop <- pts.ext$sfragsize_r ## UPDATE EVERY TIME
prop <- "sfragsize_r" ## Dependent variable
pts.ext$tid <- "nasis"
pts.ext$LocID <- paste(pts.ext$xwgs84.x, pts.ext$ywgs84.y, sep = "")
nasislocs <- unique(pts.ext$LocID)
polybound <- spTransform(polybound, CRS(cov.proj))

## Raster Sample locations for RelPI stats
pred.pts.ext <- readRDS("/home/tnaum/data/BLMsoils/UCRB_Summary_pts/UCRB_predpts_ART_SG100_covarsc.rds") ## prediction summary locations

## Prep for random forest
pts.extc <- subset(pts.ext, !duplicated(pts.ext[c("LocID")])) #removes duplicates
ptspred.list <- gsub(".tif","", cov.grids)# Take .tif off of the grid list to just get the variable names
ptspred.list <- c(ptspred.list,"prop","tid") #Add dependent variable
pts.extc <- pts.extc[c(ptspred.list)]## Or create a specific list of dependent variable and covariate names to use 
pts.extc <- na.omit(pts.extc)# Remove any record with NA's (in any column - be careful)
xtrain <- as.matrix(pts.extc[c(gsub(".tif","", cov.grids))])
ytrain <- c(as.matrix(pts.extc[c("prop")]))
logytrain <- log(ytrain+1)
# sqrtytrain <- sqrt(ytrain)

############### Build quantile Random Forest
Qsoiclass <- quantregForest(x=xtrain, y=logytrain, importance=TRUE, ntree=100, keep.forest=TRUE, nthreads = 30)
#soiclass = randomForest(ec_12pre ~ ., data = ptsc, importance=TRUE, proximity=FALSE, ntree=100, keep.forest=TRUE)
soiclass <- Qsoiclass
class(soiclass) <- "randomForest"
#soiclass## Get oob error
## Linear Adjustment for bias
pts.extc$trainpreds <- predict(soiclass, newdata=xtrain)
pts.extc$prop_t <- log(pts.extc$prop+1) ## TRANSFORM IF NEEDED
attach(pts.extc)
rf_lm_adj <- lm(prop_t ~ trainpreds)
detach(pts.extc)
pts.extc$trainpredsadj <- predict(rf_lm_adj, newdata=pts.extc)
# plot model performance stuff
plot(logytrain~predict(soiclass)) ## OOB plot
x1 <-c(-100,0,100,10000,100000000)
y1 <-c(-100,0,100,10000,100000000)
lines(x1,y1, col = 'red')#1:1 line
plot(logytrain~predict(soiclass, newdata=xtrain)) #Fit plot
lines(x1,y1, col = 'red')#1:1 line
plot(logytrain~pts.extc$trainpredsadj) #Fit plot
lines(x1,y1, col = 'red')#1:1 line
varImpPlot(soiclass)
setwd(predfolder)
# saveRDS(Qsoiclass, paste("Qsoiclass_RFmodel", prop, "cm_nasisSSURGO_ART_SG100.rds",sep="_"))
# saveRDS(rf_lm_adj, paste("rflmadj_RFmodel",prop,  "cm_nasisSSURGO_ART_SG100.rds",sep="_"))
Qsoiclass <- readRDS(paste("Qsoiclass_RFmodel", prop,  "cm_nasisSSURGO_ART_SG100.rds",sep="_"))
soiclass <- Qsoiclass
class(soiclass) <- "randomForest"
rf_lm_adj <- readRDS(paste("rflmadj_RFmodel",prop,  "cm_nasisSSURGO_ART_SG100.rds",sep="_"))


## Reference covar rasters to use in prediction
setwd(covfolder)
rasterOptions(maxmemory = 1e+09,chunksize = 2.5e+08)# maxmemory = 1e+09,chunksize = 1e+08 for soilmonster
rasters <- stack(cov.grids)
#rasters = setMinMax(brick(rasters))
#names(rasters)

## Predict onto covariate grid
setwd(predfolder)
## Parallelized predict
beginCluster(25,type='SOCK')
predl <- clusterR(rasters, predict, args=list(model=Qsoiclass,what=c(0.025)),progress="text")
predh <- clusterR(rasters, predict, args=list(model=Qsoiclass,what=c(0.975)),progress="text")
Sys.time()
pred <- clusterR(rasters, predict, args=list(model=soiclass),progress="text")
Sys.time()
names(pred) <- "trainpreds"
## Linear Adjustment
predlm <- clusterR(pred, predict, args=list(model=rf_lm_adj),progress="text")
# Determine 95% interquantile range of original training data for horizons that include the depth being predicted
varrange <- as.numeric(quantile(pts.extc$prop, probs=c(0.975), na.rm=T)-quantile(pts.extc$prop, probs=c(0.025),na.rm=T)) ## TRANSFORM IF NEEDED!
## PI widths
# s <- stack(predh,predl)
# PIwidth.fn <- function(a,b) {
#   ind <- a-b
#   return(ind)
# }
# PIwidth <- clusterR(s, overlay, args=list(fun=PIwidth.fn),progress = "text")
# PIrelwidth.fn <- function(a,b) {
#   ind <- (a-b)/varrange
#   return(ind)
# }
# PIrelwidth <- clusterR(s, overlay, args=list(fun=PIrelwidth.fn),progress = "text",export='varrange')
## Back transformation Stuff
bt.fn <- function(x) {
  ind <- (exp(x))-1 #If a backtransform is needed 10^(x) or exp(x) or ^2
  return(ind)
}
predh_bt <- clusterR(predh, calc, args=list(fun=bt.fn),progress='text')
predl_bt <- clusterR(predl, calc, args=list(fun=bt.fn),progress='text')
pred_bt <- clusterR(pred, calc, args=list(fun=bt.fn),progress='text')
s_bt <- stack(predh_bt,predl_bt)
PIwidth_bt.fn <- function(a,b) {
  ind <- a-b
  return(ind)
}
PIwidth_bt <- clusterR(s_bt, overlay, args=list(fun=PIwidth_bt.fn),progress = "text")
## If transformed, use the following code for PI width prep steps
PIrelwidth_bt.fn <- function(a,b) {
  ind <- (a-b)/varrange
  return(ind)
}
PIrelwidth_bt <- clusterR(s_bt, overlay, args=list(fun=PIrelwidth_bt.fn),progress = "text", export='varrange')
endCluster()
## Write new geotiff files
setwd(predfolder)
## Untranformed code block
# writeRaster(predlm, overwrite=TRUE,filename=paste(prop,"cm_2D_QRF.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# writeRaster(predl, overwrite=TRUE,filename=paste(prop,"cm_2D_QRF_95PI_l.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# writeRaster(predh, overwrite=TRUE,filename=paste(prop,"cm_2D_QRF_95PI_h.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# writeRaster(PIrelwidth, overwrite=TRUE,filename=paste(prop,"cm_2D_QRF_95PI_relwidth.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# # writeRaster(PIwidth, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_width.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# ## Transformed code block
writeRaster(pred_bt, overwrite=TRUE,filename=paste(prop,"QRF_bt_ART_SG100covs.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
writeRaster(predl_bt, overwrite=TRUE,filename=paste(prop,"QRF_95PI_l_bt.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
writeRaster(predh_bt, overwrite=TRUE,filename=paste(prop,"QRF_95PI_h_bt.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
writeRaster(PIrelwidth_bt, overwrite=TRUE,filename=paste(prop,"QRF_95PI_relwidth_bt.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")



################### Manual Cross validation ################################
pts.extcvm <- pts.extc
nfolds <- 10
pts.extcvm$folds <- sample.int(nfolds,size =length(pts.extcvm[,1]),replace=T)
pts.extcvm$prop_t <- log(pts.extcvm$prop+1) ## UPDATE: tranform if needed else just create new version of prop
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
cvp.RMSE <- sqrt(mean((pts.extpcv$prop_t - pts.extpcv$pcvpred)^2, na.rm=TRUE))
cvp.Rsquared <- 1-var(pts.extpcv$prop_t - pts.extpcv$pcvpred, na.rm=TRUE)/var(pts.extpcv$prop_t, na.rm=TRUE)
## Back transformed: create pcvpred_bt even if not tranformed for cv.depth function
pts.extpcv$pcvpred_bt <- exp(pts.extpcv$pcvpred)-1
cvp.RMSE_bt <- sqrt(mean((pts.extpcv$prop - pts.extpcv$pcvpred_bt)^2, na.rm=TRUE))
cvp.Rsquared_bt <- 1-var(pts.extpcv$prop - pts.extpcv$pcvpred_bt, na.rm=TRUE)/var(pts.extpcv$prop, na.rm=TRUE)
## RPI
pts.extpcv$prop_bt <- exp(pts.extpcv$prop_t)-1 # UPDATE: backtransform if necessary. Used for PICP and to characterize backtransformation bias
pts.extpcv$pcvpredpre.025_bt <- exp(pts.extpcv$pcvpredpre.025)-1 # UPDATE: backtransform if necessary
pts.extpcv$pcvpredpre.975_bt <- exp(pts.extpcv$pcvpredpre.975)-1 # UPDATE: backtransform if necessary
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
CVdf <- data.frame(cvp.RMSE, cvp.Rsquared, cvp.RMSE_bt, cvp.Rsquared_bt,RPI.cvave,RPI.cvmed,PICP,rel.abs.res.ave,rel.abs.res.med,BTbias.abs.max,BTbias.ave)
names(CVdf) <- c("cvp.RMSE","cvp.Rsquared","cvp.RMSE_bt", "cvp.Rsquared_bt","RPI.CVave","RPI.CVmed","PICP","rel.abs.res.ave","rel.abs.res.med","BTbias.abs.max","BTbias.ave")
setwd(predfolder)
write.table(CVdf, paste("PCVstats", prop, "nasisSSURGO_ART_SG100.txt",sep="_"), sep = "\t", row.names = FALSE)
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt)
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,10),ylim=c(0,10))
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,5),ylim=c(0,5))
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,1),ylim=c(0,1))
# plot(pts.extpcv$prop_t~pts.extpcv$pcvpred)
#lines(x1,y1, col = 'red')#1:1 line
## CV plots
# viri <- c("#440154FF", "#39568CFF", "#1F968BFF", "#73D055FF", "#FDE725FF") # color ramp
# gplt.dcm.2D.CV <- ggplot(data=pts.extpcv, aes(prop_t, pcvpred)) +
#   stat_binhex(bins = 30) + geom_abline(intercept = 0, slope = 1,lwd=1)  + #xlim(0,100) + ylim(0,100) +
#   theme(axis.text=element_text(size=8), legend.text=element_text(size=10), axis.title=element_text(size=10),plot.title = element_text(size=10,hjust=0.5)) +
#   xlab("Measured") + ylab("CV Prediction") + scale_fill_gradientn(name = "log(Count)", trans = "log", colours = rev(viri)) +
#   ggtitle(paste("Cross val", prop,sep=" "))
# gplt.dcm.2D.CV
## Save Cross validation graph and data for future plotting
saveRDS(pts.extpcv, paste(prop, "cvlm_preds_2D", "nasisSSURGO_ART_SG100.rds", sep="_"))


# ##### Relative PI Interval statistics for different depths
# ## Add new packages for foreach work
required.packages <- c( "doSNOW","foreach", "itertools")# might need snowfall
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
pred.pts.extdf <- as.data.frame(pred.pts.ext)
pred.pts.extdf <- na.omit(pred.pts.extdf)
num_splits <- 30 # number of cpus to use
cl <- makeCluster(num_splits)
registerDoSNOW(cl)
DF_pred_l<-
  foreach(splt=isplitRows(pred.pts.extdf, chunks=num_splits),.combine=c, .packages=c("quantregForest", "randomForest")) %dopar% {
    predict(Qsoiclass,newdata=splt, what=c(0.025))
  }
DF_pred_h<-
  foreach(splt=isplitRows(pred.pts.extdf, chunks=num_splits),.combine=c, .packages=c("quantregForest", "randomForest")) %dopar% {
    predict(Qsoiclass,newdata=splt, what=c(0.975))
  }
stopCluster(cl)
## Now compute stats for PI intervals
## Untransform
DF_pred_l_bt <- exp(DF_pred_l)-1 # Update for all runs, just don't use back transform fn if it wasn't used
DF_pred_h_bt <- exp(DF_pred_h)-1 # Update for all runs, just don't use back transform fn if it wasn't used
df.PIwidth <- DF_pred_h_bt-DF_pred_l_bt
df.varrange <- as.numeric(quantile(pts.extc$prop, probs=c(0.975),na.rm=T)-quantile(pts.extc$prop, probs=c(0.025),na.rm=T))##Change soil factor name
df.PIrelwidth <- (DF_pred_h_bt-DF_pred_l_bt)/df.varrange
## Stats (untransformed)
mean <- mean(df.PIwidth)
median <- median(df.PIwidth)
q25 <- unname(quantile(df.PIwidth, probs=c(0.25),na.rm=T))
q75 <- unname(quantile(df.PIwidth, probs=c(0.75),na.rm=T))
sd <-sd(df.PIwidth)
max <- max(df.PIwidth)
min <- min(df.PIwidth)
relmean <- mean(df.PIrelwidth)
relmedian <- median(df.PIrelwidth)
relq25 <- unname(quantile(df.PIrelwidth, probs=c(0.25),na.rm=T))
relq75 <- unname(quantile(df.PIrelwidth, probs=c(0.75),na.rm=T))
relsd <-sd(df.PIrelwidth)
relmax <- max(df.PIrelwidth)
relmin <- min(df.PIrelwidth)
PIdf <- data.frame(mean, median, q25,q75,sd,max,min,relmean, relmedian, relq25,relq75,relsd,relmax,relmin)
names(PIdf) <- c("mean","median", "q25","q75","sd","max","min","relmean","relmedian", "relq25","relq75","relsd","relmax","relmin")
## detach parallel related packages that can interfere with clusterR
detach(package:doSNOW)
detach(package:foreach)
detach(package:itertools)
# Save table to folder
setwd(predfolder)
write.table(PIdf, paste("relPI", prop, "nasisSSURGO_ART_SG100.txt",sep="_"), sep = "\t", row.names = FALSE)
gc()



############# Masking water pixels out ############
nlcd <- raster("/home/tnaum/data/UCRB_Covariates/NLCDcl.tif")
beginCluster(30,type='SOCK')
# Make a mask raster
mask_fn <- function(nlcd){ind <- ifelse(nlcd!=11,1,NA)
  return(ind)
}
mask <- clusterR(nlcd, calc, args=list(fun=mask_fn),progress='text')
endCluster()
plot(mask)
writeRaster(mask, overwrite=TRUE,filename="/home/tnaum/data/BLMsoils/nlcd_watermask.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text",datatype='INT1U')
rm(mask)
## Now set up a list of rasters and function to mask out water
rasterOptions(maxmemory = 1e+09,chunksize = 1e+08)
setwd("/home/tnaum/data/BLMsoils/Surf_Frags_SSURGO_NASIS")
grids <- list.files(pattern=".tif$")
grids <- grids[grep("size",grids)]
RPIgrid <- grids[grep("relwid",grids)]
grids <- grids[!grep("relwid",grids)]
mskfn <- function(rast,mask){
  ind <- rast*mask
  ind[ind<2]<-2 # to bring the predicted sizes slightly less than 2mm (due to model), back to 2mm
  return(ind)
}
mskfnRPI <- function(rast,mask){
  ind <- rast*mask
  ind[ind<0]<-0 # to bring the predicted sizes slightly less than 2mm (due to model), back to 2mm
  return(ind)
}
## Separate for RPI
rast <- raster(RPIgrid)
names(rast) <- "rast"
mask <- raster("/home/tnaum/data/BLMsoils/nlcd_watermask.tif")
h2ostk <- stack(rast,mask)
setwd("/home/tnaum/data/BLMsoils/Surf_Frags_SSURGO_NASIS/masked")
overlay(h2ostk,fun=mskfnRPI,progress='text',filename="sfragsize_r_QRF_95PI_relwidth_bt.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"))
## par list apply fn
watermask_fn <- function(g){
  setwd("/home/tnaum/data/BLMsoils/Surf_Frags_SSURGO_NASIS")
  rast <- raster(g)
  names(rast) <- "rast"
  setwd("/home/tnaum/data/BLMsoils")
  mask <- raster("/home/tnaum/data/BLMsoils/nlcd_watermask.tif")
  h2ostk <- stack(rast,mask)
  setwd("/home/tnaum/data/BLMsoils/Surf_Frags_SSURGO_NASIS/masked")
  overlay(h2ostk,fun=mskfn,progress='text',filename=g, options=c("COMPRESS=DEFLATE", "TFW=YES"))
  gc()
}
snowfall::sfInit(parallel=TRUE, cpus=4)
snowfall::sfExport("watermask_fn","mskfn","grids")
snowfall::sfLibrary(raster)
Sys.time()
snowfall::sfLapply(grids, function(g){watermask_fn(g)})
Sys.time()
snowfall::sfStop()

