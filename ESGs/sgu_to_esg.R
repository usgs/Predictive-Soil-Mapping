####################
### Workflow to combine
### soil geomorphic map
### with climate maps
### based on identified climate breaks
#####################
## Travis Nauman, USGS, tnauman@usgs.gov
## 8/12/2020

### Packages
required.packages <- c("plyr","raster","snow","snowfall","sp","rgdal","sf","dplyr", "latticeExtra") 
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

## Increase actuve memory useable by raster package
rasterOptions(maxmemory = 3e+10, chunksize = 5e+09, memfrac = 0.9)

### Important folders
scriptfolder <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/ESG_map"

### rasters
sgu_rast <- raster("C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/soil_gmrph_mapping/smote_ssurgo/maskedtifs/finallayers/sgu_1stClass.tif")
ai_rast <- raster("V:/PROJECTS/TRAVIS_NAUMAN/GIS_Archive/Global_Aridity_Index/ai_et0/ai_et0.tif")
maxt_rast <- raster("V:/PROJECTS/TRAVIS_NAUMAN/GIS_Archive/WorldClim/wc2.0_30s_bio/wc2.0_bio_30s_05.tif")

### Reproject climate rasters
rasterOptions(maxmemory = 5e+08, chunksize = 8e+07)
beginCluster(20,type='SOCK')
ai_rastp <- projectRaster(ai_rast, sgu_rast, method='bilinear',progress='text')
maxt_rastp <- projectRaster(maxt_rast, sgu_rast, method='bilinear',progress='text')
endCluster()
rasterOptions(maxmemory = 4e+09, chunksize = 5e+08)

### Pull in SGU lookup table to use in reclassification
sgutab <- read.delim("C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/soil_gmrph_mapping/smote_ssurgo/UCRB_BLM_ESGs_lookup_rf_soilgmrph_smote_30mINT.txt",stringsAsFactors = F)

### Create Climate zone raster
## 1 - Arid_Warm, 2 - SemiArid_Warm, 3 - SemiArid_Cool
climZn_fn <- function(ai,maxt,sgu){
  ind <- ifelse((ai/10000)<0.144&maxt>25.02,1,ifelse(maxt<25.02&(ai/10000)>=0.144,3,2))
  ind2 <- ifelse(sgu>0,1,NA)
  ind <- ind*ind2
  return(ind)
}
climstack <- stack(ai_rastp,maxt_rastp,sgu_rast)
beginCluster(30,type='SOCK')
clim_rast <- clusterR(climstack,overlay, args=list(fun=climZn_fn),progress='text')
endCluster()
gc()
setwd(scriptfolder)
writeRaster(clim_rast, overwrite=TRUE,filename="Clim_zones.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")

### Now combine climate zones with SGUs
ESG_fn <- function(clim,sgus){
  ind <- ifelse(sgus==8,1,NA) # Outcrops
  ind <- ifelse(clim==1&sgus==2,2,ind)  #arid warm breaks
  ind <- ifelse(clim==1&sgus==11,3,ind)  #arid warm saline hills
  ind <- ifelse(clim==1&sgus==6,4,ind)  #arid warm gypsum
  ind <- ifelse(clim==1&sgus==16,5,ind)  #arid warm very shallow
  ind <- ifelse(clim==1&sgus==12,6,ind)  #arid warm Saline uplands
  ind <- ifelse(clim==1&sgus==15,7,ind)  #arid warm shallow
  ind <- ifelse(clim==1&sgus==4,8,ind)  #arid warm deep rocky
  ind <- ifelse(clim==1&(sgus==14|sgus==7),9,ind)  #arid warm sandy uplands and Loamy Uplands
  ind <- ifelse(clim==1&(sgus==5|sgus==3),10,ind) # arid warm finer uplands and clay uplands
  ind <- ifelse(clim==1&sgus==13,11,ind)  #arid warm sandy bottoms
  ind <- ifelse(clim==1&(sgus==10|sgus==1),12,ind)  #arid warm saline bottoms and bottoms
  ind <- ifelse(clim==2&sgus==2,13,ind)  # semiarid warm breaks
  ind <- ifelse(clim==2&sgus==11,14,ind)  # semiarid warm saline hills
  ind <- ifelse(clim==2&sgus==6,15,ind)  # semiarid warm gypsum
  ind <- ifelse(clim==2&sgus==16,16,ind)  # semiarid warm very shallow
  ind <- ifelse(clim==2&sgus==12,17,ind)  # semiarid warm saline uplands
  ind <- ifelse(clim==2&(sgus==15|sgus==4),18,ind)  # semiarid warm shallow and deep rocky
  ind <- ifelse(clim==2&(sgus==14|sgus==7),19,ind)  # semiarid warm sandy uplands and loamy uplands
  ind <- ifelse(clim==2&sgus==5,20,ind)  # semiarid warm finer uplands
  ind <- ifelse(clim==2&sgus==3,21,ind)  # semiarid warm clay uplands
  ind <- ifelse(clim==2&(sgus==13|sgus==1),22,ind)  # semiarid warm sandy bottoms and bottoms
  ind <- ifelse(clim==2&sgus==10,23,ind)  # semiarid warm saline bottoms
  ind <- ifelse(clim==3&sgus==2,24,ind)  # semiarid cool breaks ## THIS HAD NO ESDs IN ANALYSIS BUT CAME AS BLANK IN MAP
  ind <- ifelse(clim==3&sgus==11,25,ind)  # semiarid cool saline hills
  ind <- ifelse(clim==3&sgus==6,26,ind)  # semiarid cool gypsum ## This had no esds in this climate zone as well in analysis, but had blanks in map
  ind <- ifelse(clim==3&sgus==16,27,ind)  # semiarid cool very shallow: ## This had no esds in this climate zone as well in analysis, but had blanks in map
  ind <- ifelse(clim==3&(sgus==12|sgus==14|sgus==7|sgus==5),28,ind)  # semiarid cool saline uplands, sandy uplands, loamy uplands, and finer uplands
  ind <- ifelse(clim==3&sgus==15,29,ind)  # semiarid cool shallow
  ind <- ifelse(clim==3&sgus==4,30,ind)  # semiarid cool deep rocky
  ind <- ifelse(clim==3&sgus==3,31,ind)  # semiarid cool clay uplands
  ind <- ifelse(clim==3&sgus==13,32,ind)  # semiarid cool sandy bottoms ## This had no esds in this climate zone as well in analysis, but had blanks in map
  ind <- ifelse(clim==3&sgus==10,33,ind)  # semiarid cool saline bottoms
  ind <- ifelse(clim==3&sgus==1,34,ind)  # semiarid cool bottoms
  ind <- ifelse(sgus==9,35,ind) # Riparian
  return(ind)
}
esg_stk <- stack(clim_rast,sgu_rast)
beginCluster(30,type='SOCK')
esg_rast <- clusterR(esg_stk,overlay,args=list(fun=ESG_fn),progress='text')
pal <- colorRampPalette(c("blue","cyan","green","yellow","red"))
plot(clim_rast)
plot(esg_rast,col=pal(20))
endCluster()
gc()
writeRaster(esg_rast, overwrite=TRUE,filename="ESGs_final.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")


### Evaluate esgs and sgus by climate zone (climate x sgu hypothesis)
esd_final <- readRDS("C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/esd_final_v2b.rds")
summary.factor(esd_final$v2_m4_26)
## Create climate classes for esds
esd_final$climzone <- ifelse(esd_final$aimean<0.144&esd_final$maxtempmean>25.02,"Arid_Warm",ifelse(esd_final$maxtempmean<=25.02,"Semiarid_Cool","Semiarid_Warm"))
plot(as.factor(esd_final$climzone)~as.factor(esd_final$sgu),xlab="",ylab="",yaxt="n",las=1)







