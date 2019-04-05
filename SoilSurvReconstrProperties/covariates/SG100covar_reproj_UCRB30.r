required.packages <- c("raster", "sp", "rgdal","snow", "snowfall","parallel", "itertools","doParallel")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
## Increase actuve memory useable by raster package
memory.limit(500000)
rasterOptions(maxmemory = 1e+08,chunksize = 1e+06)

## Make list of grids to reproject
setwd("O:/Models_active_work/covars_sg100")
cov.grids <- list.files(pattern=".tif$")

## Reproject files and put into new folder
mask <- raster("O:/Models_active_work/UCRB_Covariates/GAP.tif")
newpath <- "O:/Models_active_work/covars_sg100_ucrb30/"
beginCluster(30,type='SOCK')
for(covgrid in cov.grids){
projectRaster(raster(covgrid),mask, progress='text', filename=paste(newpath,covgrid,sep=""), options=c("COMPRESS=DEFLATE", "TFW=YES"),method="ngb")
print(paste("Done with ", covgrid, sep=""))
}
endCluster()