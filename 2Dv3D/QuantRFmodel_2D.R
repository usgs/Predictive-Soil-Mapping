######################
## Random Forest script that includes:
## Extraction of covariates to points
## Prediction interval creation
## Cross Validation
## Most steps parallelized
######################



# Workspace setup
# Install packages if not already installed

required.packages <- c("raster", "sp", "rgdal", "randomForest", "snow", "snowfall", "quantregForest","dplyr", "ggplot2")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
## Increase actuve memory useable by raster package
memory.limit(500000) # Windows only
rasterOptions(maxmemory = 1e+10, chunksize = 1e+09)
#options(scipen = 999)
#par(mar=c(0.3,0.3,0.3,0.3))

## Key Folder Locations
predfolder <- "/home/tnaum/data/BLMsoils/pH_h2o_2D_CV"
covfolder <- "/home/tnaum/data/UCRB_Covariates"


######## Get points for extraction if in table form ###########
setwd(predfolder)
pts <- read.delim("NCSS17_pH_carbonate_gyp_ttab.txt") # Read in table pulled from Lab database
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
## Now clip points and check with visualization
shp.pts = shp.pts[polybound,]#clip by outer extent of all polybound features
plot(polybound)
plot(shp.pts, add=TRUE)

######### Grid Prep #################
## Make list of grids
setwd(covfolder)
cov.grids <- list.files(pattern=".tif$")
## If points need to be matched up to grids ###
projgrid = raster(cov.grids[1])
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
write.table(pts.ext, "cop_ncss17pH_h20_covarsc.txt", sep = "\t", row.names = FALSE)
## Points with extracted covariates already made
pts.ext <- read.delim("cop_ncss17pH_h20_covarsc.txt") ## Files available on Github repository

## Prep training data for Random Forest
pts.ext$prop <- pts.ext$ph_h2o ## UPDATE EVERY TIME
prop <- "ph_h2o" ## Dependent variable

##### Loop to train and predict properties for all depths
depths <- c(0,15,30,60,100)
for(d in depths){
pts.extc <- subset(pts.ext, as.numeric(pts.ext$hzdept_r) <= d & as.numeric(pts.ext$hzdepb_r) > d) # subset to chosen depth
ptspred.list <- gsub(".tif","", cov.grids)# Take .tif off of the grid list to just get the variable names
ptspred.list <- c(ptspred.list,"prop") #Add dependent variable
pts.extc <- pts.extc[c(ptspred.list)]## Or create a specific list of dependent variable and covariate names to use 
pts.extc <- na.omit(pts.extc)# Remove any record with NA's (in any column - be careful)
xtrain <- as.matrix(pts.extc[c(gsub(".tif","", cov.grids))])
ytrain <- c(as.matrix(pts.extc[c("prop")]))
## Transformations - if needed
# logytrain <- log(ytrain+0.1)
# sqrtytrain <- sqrt(ytrain)

############### Build quantile Random Forest
Qsoiclass <- quantregForest(x=xtrain, y=ytrain, importance=TRUE, ntree=100, keep.forest=TRUE)
#soiclass = randomForest(ec_12pre ~ ., data = ptsc, importance=TRUE, proximity=FALSE, ntree=100, keep.forest=TRUE)
soiclass = Qsoiclass
class(soiclass) = "randomForest"
#soiclass## Get oob error
## plot model performance stuff
# plot(ytrain~predict(soiclass)) ## OOB plot
# x1 <-c(-100,0,100,10000,100000000)
# y1 <-c(-100,0,100,10000,100000000)
# lines(x1,y1, col = 'red')#1:1 line
# plot(ytrain~predict(soiclass, newdata=xtrain)) #Fit plot
# lines(x1,y1, col = 'red')#1:1 line
# varImpPlot(soiclass)
setwd(predfolder)
saveRDS(Qsoiclass, paste("Qsoiclass_RFmodel", prop, d, "cm.rds",sep="_"))
#Qsoiclass <- readRDS("Qsoiclass_RFmodel_claytot_r_00cm.rds")


## Reference covar rasters to use in prediction
setwd(covfolder)
rasterOptions(maxmemory = 2e+08,chunksize = 3e+07)
rasters<-stack(cov.grids)
names(rasters)

## Predict onto covariate grid
setwd(predfolder)
## Parallelized predict
beginCluster(30,type='SOCK')
predl <- clusterR(rasters, predict, args=list(model=Qsoiclass,what=c(0.025)),progress="text")
predh <- clusterR(rasters, predict, args=list(model=Qsoiclass,what=c(0.975)),progress="text")
Sys.time()
pred <- clusterR(rasters, predict, args=list(model=soiclass),progress="text")
Sys.time()
## PI widths
s <- stack(predh,predl)
PIwidth.fn <- function(a,b) {
  ind <- a-b
  return(ind)
}
PIwidth <- clusterR(s, overlay, args=list(fun=PIwidth.fn),progress = "text")
# Determine 95% interquantile range of original training data for horizons that include the depth being predicted
pts.ext.pred.depth <- subset(pts.ext, as.numeric(pts.ext$hzn_top) <= d & as.numeric(pts.ext$hzn_bot) > d)
varrange <- as.numeric(quantile(pts.ext.pred.depth$prop, probs=c(0.975), na.rm=T)-quantile(pts.ext.pred.depth$prop, probs=c(0.025),na.rm=T)) ## TRANSFORM IF NEEDED!
PIrelwidth.fn <- function(a,b) {
  ind <- (a-b)/varrange
  return(ind)
}
PIrelwidth <- clusterR(s, overlay, args=list(fun=PIrelwidth.fn),progress = "text",export='varrange')
## Back transformation Code if needed
# bt.fn <- function(x) {
#   ind <- (exp(x))-0.1 #If a backtransform is needed 10^(x) or exp(x) or ^2
#   return(ind) 
# }
# predh_bt <- clusterR(predh, calc, args=list(fun=bt.fn),progress='text')
# predl_bt <- clusterR(predl, calc, args=list(fun=bt.fn),progress='text')
# pred_bt <- clusterR(pred, calc, args=list(fun=bt.fn),progress='text')
# s_bt <- stack(predh_bt,predl_bt)
# PIwidth_bt.fn <- function(a,b) {
#   ind <- a-b
#   return(ind)
# }
# PIwidth_bt <- clusterR(s_bt, overlay, args=list(fun=PIwidth_bt.fn),progress = "text")
# ## If transformed, use the following code for PI width prep steps
# PIrelwidth_bt.fn <- function(a,b) {
#   ind <- (a-b)/varrange
#   return(ind)
# }
# PIrelwidth_bt <- clusterR(s_bt, overlay, args=list(fun=PIrelwidth_bt.fn),progress = "text", export='varrange')

endCluster()
## Write new geotiff files
setwd(predfolder)
## Untranformed code block
writeRaster(pred, overwrite=TRUE,filename=paste(prop,"2D",d,"cm_QRF.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
writeRaster(predl, overwrite=TRUE,filename=paste(prop,"2D",d,"cm_QRF_95PI_l.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
writeRaster(predh, overwrite=TRUE,filename=paste(prop,"2D",d,"cm_2D_QRF_95PI_h.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
writeRaster(PIrelwidth, overwrite=TRUE,filename=paste(prop,"2D",d,"cm_2D_QRF_95PI_relwidth.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
writeRaster(PIwidth, overwrite=TRUE,filename=paste(prop,"2D",d,"cm_2D_QRF_95PI_width.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# ## Transformed code block
# writeRaster(pred_bt, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_bt_ART_SG100covs.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# writeRaster(predl_bt, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_l_bt.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# writeRaster(predh_bt, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_h_bt.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# writeRaster(PIwidth_bt, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_width_bt.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
# writeRaster(PIrelwidth_bt, overwrite=TRUE,filename=paste(prop,d,"cm_2D_QRF_95PI_relwidth_bt.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")

################### Manual Cross validation - RPI ################################
pts.extcvm <- pts.ext
pts.extcvm <- subset(pts.extcvm, as.numeric(pts.extcvm$hzn_top) <= 00 & as.numeric(pts.extcvm$hzn_bot) > 00)
pts.extcvm <- pts.extcvm[c(ptspred.list)]
pts.extcvm <- na.omit(pts.extcvm)# Remove any record with NA's (in any column - be careful)
pts.extcvm  <- subset(pts.extcvm, pts.extcvm$sand_f_vf_psa != "NA")
nfolds <- 10
pts.extcvm$folds <- sample.int(nfolds,size =length(pts.extcvm[,1]),replace=T)
pts.extcvm$prop <- pts.extcvm$sand_f_vf_psa
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
  traindf$pcvpred <- predict(rf.pcvc, newdata=traindf)
  testdf$pcvpred <- predict(rf.pcvc, newdata=testdf)
  #traindf$pcvpredpre <- predict(rf.pcv, newdata=traindf, what=c(0.5)) ## If median is desired
  #testdf$pcvpredpre <- predict(rf.pcv, newdata=testdf, what=c(0.5)) ## If median is desired
  testdf$pcvpredpre.025 <- predict(rf.pcv, newdata=testdf, what=c(0.025))
  testdf$pcvpredpre.975 <- predict(rf.pcv, newdata=testdf, what=c(0.975))
  return(testdf)
}
snowfall::sfInit(parallel=TRUE, cpus=3)
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
pts.extpcv$pcvpred_bt <- pts.extpcv$pcvpred  ## UPDATE: backtranform if needed else just create new version of prop
cvp.RMSE_bt = sqrt(mean((pts.extpcv$prop - pts.extpcv$pcvpred_bt)^2, na.rm=TRUE))
cvp.Rsquared_bt = 1-var(pts.extpcv$prop - pts.extpcv$pcvpred_bt, na.rm=TRUE)/var(pts.extpcv$prop, na.rm=TRUE)
## Number of SCD samples
n_scd <- length(pts.extpcv[,1])
## RPI
pts.extpcv$pcvpredpre.025_bt <- pts.extpcv$pcvpredpre.025 # UPDATE: backtransform if necessary
pts.extpcv$pcvpredpre.975_bt <- pts.extpcv$pcvpredpre.975 # UPDATE: backtransform if necessary
pts.extpcv$abs.resid <- abs(pts.extpcv$prop - pts.extpcv$pcvpred_bt)
varrange <- as.numeric(quantile(pts.extcvm$prop, probs=c(0.975), na.rm=T)-quantile(pts.extcvm$prop, probs=c(0.025),na.rm=T))
pts.extpcv$RPI <- (pts.extpcv$pcvpredpre.975_bt - pts.extpcv$pcvpredpre.025_bt)/varrange
plot(pts.extpcv$abs.resid~pts.extpcv$RPI)
## Summarize RPI and residuals
pts.extpcv$rel.abs.resid <- pts.extpcv$abs.resid/varrange
RPI.cvave <- mean(pts.extpcv$RPI)
RPI.cvmed <- median(pts.extpcv$RPI)
rel.abs.res.ave <- mean(pts.extpcv$rel.abs.resid)
rel.abs.res.med <- median(pts.extpcv$rel.abs.resid)
## prediction interval coverage probability (PICP)
PICP <- sum(ifelse(pts.extpcv$prop <= pts.extpcv$pcvpredpre.975_bt & pts.extpcv$prop >= pts.extpcv$pcvpredpre.025_bt,1,0))/length(pts.extpcv[,1])
## Create PCV table
CVdf <- data.frame(cvp.RMSE, cvp.Rsquared, cvp.RMSE_bt, cvp.Rsquared_bt,RPI.cvave,RPI.cvmed,PICP,rel.abs.res.ave,rel.abs.res.med, n_scd)
names(CVdf) <- c("cvp.RMSE","cvp.Rsquared","cvp.RMSE_bt", "cvp.Rsquared_bt","RPI.CVave","RPI.CVmed","PICP","abs.res.ave","rel.abs.res.med","n_scd")
setwd(predfolder)
write.table(CVdf, paste("PCVstats", prop, d, "cm_wRPI.txt",sep="_"), sep = "\t", row.names = FALSE)
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt)
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,10),ylim=c(0,10))
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,5),ylim=c(0,5))
# plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,1),ylim=c(0,1))
# plot(pts.extpcv$prop_t~pts.extpcv$pcvpred)
#lines(x1,y1, col = 'red')#1:1 line
## CV plots
#viri <- c("#440154FF", "#39568CFF", "#1F968BFF", "#73D055FF", "#FDE725FF") # color ramp
## Observed vs Measured
# gplt.dcm.2D.CV <- ggplot(data=pts.extpcv, aes(prop_t, pcvpred)) +
#   stat_binhex(bins = 30) + geom_abline(intercept = 0, slope = 1,lwd=1)  + #xlim(0,100) + ylim(0,100) +
#   theme(axis.text=element_text(size=8), legend.text=element_text(size=10), axis.title=element_text(size=10),plot.title = element_text(size=10,hjust=0.5)) +
#   xlab("Measured") + ylab("CV Prediction") + scale_fill_gradientn(name = "log(Count)", trans = "log", colours = rev(viri)) +
#   ggtitle(paste("Cross val", prop, d, "cm",sep=" "))
# gplt.dcm.2D.CV
## Residuals vs RPI
# gplt.dcm.2D.RPI <- ggplot(data=pts.extpcv, aes(RPI, rel.abs.resid)) +
#  stat_binhex(bins = 30)  + #xlim(0,2) + #ylim(0,2) +
#  theme(axis.text=element_text(size=8), legend.text=element_text(size=10), axis.title=element_text(size=10),plot.title = element_text(size=10,hjust=0.5)) +
#  xlab("RPI") + ylab("95% IQR Relative Absolute CV Residuals") + scale_fill_gradientn(name = "Count", colours = rev(viri)) +
#  ggtitle(paste("RPI", "f+vf Sands", "0", "cm",sep=" "))
# gplt.dcm.2D.RPI
## Save Cross validation graph and data for future plotting
saveRDS(pts.extpcv, paste("UCRB",prop, d, "cm_CV_pts_RPI.rds", sep="_"))



##### Relative PI Interval statistics for different depths
## Add new packages for foreach work
required.packages <- c( "doSNOW","foreach", "itertools")# might need snowfall
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
pred.pts.ext <- read.delim("/home/tnaum/data/BLMsoils/UCRB_Summary_pts/UCRB_ncss17_sample_subset_covarsc.txt") ## prediction summary locations
pred.pts.extdf <- as.data.frame(pred.pts.ext)
rm(pred.pts.ext)
pred.pts.extdf <- na.omit(pred.pts.extdf)
relPI.depth <- function(d, pts.ext, pred.pts.extdf,Qsoiclass){
  num_splits <- 30 # number of cpus to use
  cl <- makeCluster(num_splits)
  registerDoSNOW(cl)
  pred.pts.extdf$depth<-d
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
  pts.ext.depth <- subset(pts.ext, as.numeric(pts.ext$hzdept_r) <= d & as.numeric(pts.ext$hzdepb_r) > d)
  ## Untransform
  DF_pred_l_bt <- DF_pred_l # Update for all runs, just don't use back transform fn if it wasn't used 
  DF_pred_h_bt <- DF_pred_h # Update for all runs, just don't use back transform fn if it wasn't used 
  df.PIwidth=DF_pred_h_bt-DF_pred_l_bt
  df.varrange=as.numeric(quantile(pts.ext.depth$prop, probs=c(0.975),na.rm=T)-quantile(pts.ext.depth$prop, probs=c(0.025),na.rm=T))##Change soil factor name
  df.PIrelwidth=(DF_pred_h_bt-DF_pred_l_bt)/df.varrange
  ## Stats (untransformed)
  PIdepth <- d
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
  PIdf <- data.frame(PIdepth, mean, median, q25,q75,sd,max,min,relmean, relmedian, relq25,relq75,relsd,relmax,relmin)
  names(PIdf) <- c("PIdepth","mean","median", "q25","q75","sd","max","min","relmean","relmedian", "relq25","relq75","relsd","relmax","relmin")
  return(PIdf)
}
relPI.list <- lapply(depths, function(d){try(relPI.depth(d,pts.ext, pred.pts.extdf,Qsoiclass))})
relPI.df <- relPI.list[[1]] 
relPI.df <- relPI.df[FALSE,]
for(i in seq(1:length(relPI.list))){
  newrow <- relPI.list[[i]]
  if(class(newrow)=="data.frame"){
    relPI.df <- rbind(relPI.df, newrow)
  }
  print(paste("Done with ", i, sep=""))
}
## detach parallel related packages that can interfere with clusterR
detach(package:doSNOW)
detach(package:foreach)
detach(package:itertools)
# Save table to folder
setwd(predfolder)
write.table(relPI.df, paste("relPI", prop, d, "cm.txt",sep="_"), sep = "\t", row.names = FALSE)

## Finish off loop
print(paste("Done with depth", d, sep=" "))
gc()
} # End of depth loop
