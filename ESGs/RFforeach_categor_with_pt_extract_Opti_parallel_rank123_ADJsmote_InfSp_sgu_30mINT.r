######################
## Random Forest script that includes:
## Extraction of covariates to points
## Confustion matrix creation
## Kappa calculation
## rank123
######################
## Workflow has been built out for mapping 
## soil geomorphic units as the first step
## for developing Ecological Site Groups
######################
# AU: Travis Nauman, US Geological SUrvey, tnauman@usgs.gov
# 6/2021


# Workspace setup
# Install packages if not already installed
required.packages <- c("raster", "sp", "rgdal", "randomForest", "snow", "snowfall","fmsb","reshape", "ggplot2", "parallel", "mgcv","UBL","itertools","doParallel","sf","dplyr","soiltexture")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
## Increase actuve memory useable by raster package
rasterOptions(maxmemory = 6e+09,  memfrac = 0.9) # tmpdir = "/home/tnaum/data/temp/",
# memory.limit(140000)

# modelfolder <- "/home/tnaum/data/ESGs_work/soilgmrph_mapping"
datafolder <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/soil_gmrph_mapping/smote_ssurgo"
covfolder <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/ESG_30mCovsInt"
ssurgo_fgdb <- "/ped/GIS_Archive/gSSURGO18/gSSURGO_CONUS.gdb/gSSURGO_CONUS.gdb"
ssurgo_result_fldr <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/soil_gmrph_mapping/ssurgo_intermediates"
testfldr <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/soil_gmrph_mapping/smote_ssurgo/test"
# maskfolder <- "E:/Models_active_work/ESGs/strawman_map/maskrasts"

######### Grid Prep #################
## Make list of grids
setwd(covfolder)
cov.grids <- list.files(pattern=".tif$")
## If points need to be matched up to grids ###
projgrid <- raster(cov.grids[1])
## Or Make a stack of grids to extract all at once (for smaller datasets)
#cov.stack <- stack()
cov.proj <- projection(projgrid)
  
######## Load points ##############
## NASIS pedons
# polys <- readRDS("/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/pt_data/NASIS_pts_Jon_20190930/ssurgo18_ext/UCRB_gSSURGO18_mupolys.rds")
# polys.proj <- projection(polys)
# #nasis <- readRDS("/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/pt_data/NASIS_pts_Jon_20190930/Jons_NASIS_All_Pts.rds")
# nasis <- readRDS("/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/pt_data/NASIS_pts_Jon_20190930/ssurgo18_ext/Jons_NASIS_All_Pts_ssurgo18.rds")
# coordinates(nasis) <- ~ coords.x1 + coords.x2
# projection(nasis) <- polys.proj
# rm(polys)
# gc()
# nasis@data$LocID <- paste(nasis@coords[,1],nasis@coords[,2],sep="_")
# nasis@data$project <- "nasis"
# nasis <- spTransform(nasis, CRS(cov.proj)) # project to match rasters
# ## List of unique components from the nasis pts
# cokeys <- unique(nasis@data$cokey)
# ## Link nasis pts to eco site table
# # eco.df <- sf::st_read(dsn = ssurgo_fgdb, layer = "coecoclass")
# # eco.df[] <- lapply(eco.df, function(x) if (is.factor(x)) as.character(x) else {x})
# # eco.df <- eco.df[eco.df$cokey %in% cokeys,]
# # ecoids <- eco.df[,c('ecoclassid','cokey')]
# setwd(ssurgo_result_fldr)
# # saveRDS(ecoids, "ecoids.rds")
# ecoids <- readRDS("ecoids.rds")
# nasis@data <- cbind(ecoids[match(nasis@data$cokey, ecoids$cokey),], nasis@data)
# nasis@data[,2] <- NULL # get rid of duplicate cokey field
# ## Update to latest correlation
# corrtab <- read.delim("/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/UCRB_BLM_esd18_SGU_correlations.txt",stringsAsFactors = F)
# corrtab <- corrtab[,c("ecoclassid","ESG")]
# corrtab_mtn <- read.delim("/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/mtn_data/UCRB_pts_mtnCO_esd18_sgu_correlations.txt",stringsAsFactors = F)
# corrtab_mtn$ecoclassid <- corrtab_mtn$esdid
# corrtab_mtn <- corrtab_mtn[,c("ecoclassid","ESG")]
# corrtab_all <- rbind(corrtab,corrtab_mtn)
# corrtab_all <- corrtab_all[corrtab_all$ESG!="",]
# nasis@data <- cbind(corrtab_all[match(nasis@data$ecoclassid, corrtab_all$ecoclassid),], nasis@data) #attaches to pedtab
# nasis@data[,1] <- NULL # Remove duplicate ecoclassid
# ################ Attach SSURGO data for final SGU correlation
# setwd(ssurgo_result_fldr)
# ## Depth table query: large tables processed first time and saved in intermediate folders for subsequent analysis
# # restr.df <- sf::st_read(dsn = ssurgo_fgdb, layer = "corestrictions")
# # restr.df[] <- lapply(restr.df, function(x) if (is.factor(x)) as.character(x) else {x})
# # restrs <- restr.df[restr.df$cokey %in% cokeys,]
# # restrs.all <- restrs[with(restrs,order(cokey,resdept_r)),]
# # restrs.depth <- subset(restrs.all, restrs.all$reskind == "Lithic bedrock"|restrs.all$reskind == "Paralithic bedrock"|restrs.all$reskind == "Densic bedrock")
# # restrs.depth <- subset(restrs.depth, !duplicated(restrs.depth[c("cokey")])) #removes duplicates
# # restrs.depth <- restrs.depth[,c("cokey","resdept_r")]
# # saveRDS(restrs.depth, "restrs.depth.rds")
# restrs.depth <- readRDS("restrs.depth.rds")
# nasis@data <- dplyr::left_join(nasis@data,restrs.depth, by="cokey")
# nasis@data$resdept_r <- ifelse(is.na(nasis@data$resdept_r), 201, nasis@data$resdept_r)
# nasis@data$resdept_r <- ifelse(nasis@data$resdept_r > 201, 201, nasis@data$resdept_r) # Reclass a couple >201 to 201 for consistent right censorship rules in SSURGO
# ## SSURGO horizon data: large tables processed first time and saved in intermediate folders for subsequent analysis
# # horizon.df <- sf::st_read(dsn = ssurgo_fgdb, layer = "chorizon")
# # horizon.df[] <- lapply(horizon.df, function(x) if (is.factor(x)) as.character(x) else {x})
# # horizs <- horizon.df[horizon.df$cokey %in% cokeys,]
# # saveRDS(horizs,"horizons_ucrb.rds")
# horizs <- readRDS("horizons_ucrb.rds")
# chkeys <- unique(horizs$chkey) # Unique horizon keys for future queries
# ## Now bring in rock fragment volume table
# # chfrags <- sf::st_read(dsn = ssurgo_fgdb, layer = "chfrags")
# # chfrags[] <- lapply(chfrags, function(x) if (is.factor(x)) as.character(x) else {x})
# # chfrags <- chfrags[chfrags$chkey %in% chkeys,]
# # saveRDS(chfrags,"chfrags_ucrb.rds")
# # chfrags <- readRDS("chfrags_ucrb.rds")
# ## Now summarize rock fragments for each horizon
# # frags_chkey <- plyr::ddply(chfrags,~chkey,summarise,rocksum = sum(fragvol_r)) # Sum different rock sizes for each horizon
# # chfrags <- left_join(chfrags,frags_chkey, by="chkey")
# # chfrags$fragvolwt <- chfrags$fragvol_r / chfrags$rocksum
# # chfrags$fragsize_r_wt <- chfrags$fragvolwt * chfrags$fragsize_r
# # ## Representative rock size weighted by % cover of each size class for each component
# # fragsz_ave_chkey <- plyr::ddply(chfrags,~chkey,summarise,fragsz_ave = sum(fragsize_r_wt))
# # fragsz_max_chkey <- plyr::ddply(chfrags,~chkey,summarise,fragsz_max = max(fragsize_r))
# # saveRDS(frags_chkey,"frags_chkey.rds")
# # saveRDS(fragsz_ave_chkey,"fragsz_ave_chkey.rds")
# # saveRDS(fragsz_max_chkey,"fragsz_max_chkey.rds")
# frags_chkey <- readRDS("frags_chkey.rds")
# fragsz_ave_chkey <- readRDS("fragsz_ave_chkey.rds")
# fragsz_max_chkey <- readRDS("fragsz_max_chkey.rds")
# horizs <- left_join(horizs,frags_chkey,by="chkey")
# horizs <- left_join(horizs,fragsz_ave_chkey,by="chkey")
# horizs <- left_join(horizs,fragsz_max_chkey,by="chkey")
# horizs$rocksum <- ifelse(is.na(horizs$rocksum), 0, horizs$rocksum) # Fill in 0s for horizons without rock
# horizs$fragsz_max <- ifelse(is.na(horizs$fragsz_max), 2, horizs$fragsz_max)
# horizs$fragsz_ave <- ifelse(is.na(horizs$fragsz_ave), 2, horizs$fragsz_ave)
# ## Component table
# # comp.df <- sf::st_read(dsn = ssurgo_fgdb, layer = "component")
# # comp.df[] <- lapply(comp.df, function(x) if (is.factor(x)) as.character(x) else {x})
# # comp.df <- comp.df[comp.df$cokey %in% cokeys,]
# # saveRDS(comp.df,"compdf_ucrb.rds")
# comp.df <- readRDS("compdf_ucrb.rds")
# ## Comonth table: Flooding and ponding
# # comonth <- sf::st_read(dsn = ssurgo_fgdb, layer = "comonth")
# # comonth[] <- lapply(comonth, function(x) if (is.factor(x)) as.character(x) else {x})
# # comonth <- comonth[comonth$cokey %in% cokeys,]
# # saveRDS(comonth,"comonth_ucrb.rds")
# comonth <- readRDS("comonth_ucrb.rds")
# comonth_keys <- unique(comonth$comonthkey)
# comonth$floodnum <- ifelse(comonth$flodfreqcl == 'Occasional'|comonth$flodfreqcl == 'Rare',1,NA)
# comonth$floodnum <- ifelse(comonth$flodfreqcl == 'Frequent'|comonth$flodfreqcl == 'Very frequent',2,comonth$floodnum)
# comonth$floodnum <- ifelse(is.na(comonth$floodnum),0,comonth$floodnum)
# flood_annual <- plyr::ddply(comonth,~cokey,summarise,floodmax = max(floodnum))
# comonth$pondnum <- ifelse(comonth$pondfreqcl == 'None'|comonth$pondfreqcl == 'Rare',0,NA)
# comonth$pondnum <- ifelse(comonth$pondfreqcl == 'Occasional'|comonth$pondfreqcl == 'Frequent',1,comonth$pondnum)
# comonth$pondnum <- ifelse(is.na(comonth$pondnum),0,comonth$pondnum)
# pond_annual <- plyr::ddply(comonth,~cokey,summarise,pondmax = sum(pondnum))
# ## Component Geomorphic Description Table
# # cogeomordesc <- sf::st_read(dsn = ssurgo_fgdb, layer = "cogeomordesc")
# # cogeomordesc[] <- lapply(cogeomordesc, function(x) if (is.factor(x)) as.character(x) else {x})
# # cogeomordesc <- cogeomordesc[cogeomordesc$cokey %in% cokeys,]
# # saveRDS(cogeomordesc,"cogeomordesc_ucrb.rds")
# cogeomordesc <- readRDS("cogeomordesc_ucrb.rds")
# # cogeomordesc <- cogeomordesc[cogeomordesc$rvindicator=='Yes',]
# # landforms <- cogeomordesc[cogeomordesc$geomftname=='Landform',c('cokey','geomfname')]
# # landforms <- plyr::ddply(landforms,~cokey,summarise,landform=list(geomfname))
# # landscapes <- cogeomordesc[cogeomordesc$geomftname=='Landscape',c('cokey','geomfname')]
# # landscapes <- plyr::ddply(landscapes,~cokey,summarise,landscape=list(geomfname))
# # saveRDS(landforms,"landforms_ucrb.rds")
# # saveRDS(landscapes,"landscapes_ucrb.rds")
# landforms <- readRDS("landforms_ucrb.rds")
# landscapes <- readRDS("landscapes_ucrb.rds")
# ## Join data to nasis pts
# nasis@data <- left_join(nasis@data,flood_annual,by='cokey')
# nasis@data[is.na(nasis@data$floodmax),c('floodmax')] <- 0
# nasis@data <- left_join(nasis@data,pond_annual,by='cokey')
# nasis@data[is.na(nasis@data$pondmax),c('pondmax')] <- 0
# nasis@data$h2otable <- ifelse(nasis@data$drainagecl=='Excessively drained'|nasis@data$drainagecl=='Somewhat excessively drained'|nasis@data$drainagecl=='Well drained',0,1)
# nasis@data <- left_join(nasis@data,landforms,by='cokey')
# nasis@data <- left_join(nasis@data,landscapes,by='cokey')
# 
# ## Prep horizon weights to a depth interval: topsoil
# ud <- 0 # upper depth
# ld <- 30 # lower depth
# horiz_surf <- subset(horizs, (as.numeric(horizs$hzdept_r) <= ud & as.numeric(horizs$hzdepb_r) > ud) |
#                        (as.numeric(horizs$hzdepb_r) >= ld & as.numeric(horizs$hzdept_r) < ld) ) # subset to horizon overlapping range
# horiz_surf$indepthupper <- ifelse(as.numeric(horiz_surf$hzdept_r) <= ud & as.numeric(horiz_surf$hzdepb_r) < ld ,
#                                   as.numeric(horiz_surf$hzdepb_r) - ud, 0)
# horiz_surf$indepthlower <- ifelse(as.numeric(horiz_surf$hzdept_r) >= ud & as.numeric(horiz_surf$hzdepb_r) >= ld,
#                                   ld - as.numeric(horiz_surf$hzdept_r),0)
# horiz_surf$indepthoverlap <- ifelse(as.numeric(horiz_surf$hzdept_r) < ud & as.numeric(horiz_surf$hzdepb_r) > ld,
#                                     ld - ud,0)
# horiz_surf$indepth <- horiz_surf$indepthupper + horiz_surf$indepthlower + horiz_surf$indepthoverlap
# horiz_surf$depthwt <- horiz_surf$indepth / (ld-ud)
# props <- c('claytotal_r','sandtotal_r','ec_r','rocksum','gypsum_r','sar_r','caco3_r','silttotal_r','fragsz_max','fragsz_ave')
# horiz_surf <- horiz_surf[,c(props, "cokey","depthwt")]
# horiz_surf <- na.omit(horiz_surf)
# horiz_surf_depthwt <- horiz_surf[,c("cokey","depthwt")]
# horiz_surf_depthwt <- plyr::ddply(horiz_surf_depthwt,~cokey,summarise,depthwtsum=sum(depthwt)) # determine total depth weight to allow for post NA normalization
# horiz_surf <- merge(horiz_surf, horiz_surf_depthwt,by="cokey")
# horiz_surf$depthwtnorm <- horiz_surf$depthwt / horiz_surf$depthwtsum
# ## Calculate depth weighted contibution of all relevant horizons
# for(p in props){horiz_surf[,paste(p,"depthwt",sep="_")] <- horiz_surf$depthwtnorm * horiz_surf[,p]}
# ## Summarize depth weighted property values for each component
# comp_surf_fn <- function(p){
#   horiz_surfc <- horiz_surf
#   newpropnm <- paste(p,"surf",sep="_")
#   horiz_surfc$sumprop <- horiz_surfc[,paste(p,"depthwt",sep="_")]
#   comp_sum <- plyr::ddply(horiz_surfc,~cokey,summarise, newprop = sum(sumprop))
#   comp_sum[,newpropnm] <- comp_sum$newprop
#   comp_sum$newprop <- NULL
#   rm(horiz_surfc)
#   return(comp_sum)
# }
# ## Parallel function implementation
# cpus <- ifelse((detectCores() - 1) > length(props), length(props), detectCores() - 1)
# cl <- makeCluster(cpus, type="FORK")
# registerDoParallel(cl)
# comp_surf_sum <- parLapply(cl,props,comp_surf_fn)
# stopCluster(cl)
# ## Put list into dataframe
# comp_surf <- comp_surf_sum[[1]]
# for(i in seq(length(comp_surf_sum))){
#   newcol <- comp_surf_sum[[i]][,2]
#   newcolnm <- colnames(comp_surf_sum[[i]])[2]
#   #comp_surf <- cbind(comp_surf, newcol)
#   comp_surf[,newcolnm] <- newcol
#   print(paste("Done with ", i, sep=""))
# }
# rm(comp_surf_sum)
# 
# ## Prep horizon weights to a depth interval: subsoil
# ud <- 30 # upper depth
# ld <- 100 # lower depth
# horiz_sub <- subset(horizs, (as.numeric(horizs$hzdept_r) <= ud & as.numeric(horizs$hzdepb_r) > ud) |
#                       (as.numeric(horizs$hzdepb_r) >= ld & as.numeric(horizs$hzdept_r) < ld) ) # subset to horizon overlapping range
# horiz_sub$indepthupper <- ifelse(as.numeric(horiz_sub$hzdept_r) <= ud & as.numeric(horiz_sub$hzdepb_r) < ld ,
#                                  as.numeric(horiz_sub$hzdepb_r) - ud, 0)
# horiz_sub$indepthlower <- ifelse(as.numeric(horiz_sub$hzdept_r) >= ud & as.numeric(horiz_sub$hzdepb_r) >= ld,
#                                  ld - as.numeric(horiz_sub$hzdept_r),0)
# horiz_sub$indepthoverlap <- ifelse(as.numeric(horiz_sub$hzdept_r) < ud & as.numeric(horiz_sub$hzdepb_r) > ld,
#                                    ld - ud,0)
# horiz_sub$indepth <- horiz_sub$indepthupper + horiz_sub$indepthlower + horiz_sub$indepthoverlap
# horiz_sub$depthwt <- horiz_sub$indepth / (ld-ud)
# props <- c('claytotal_r','sandtotal_r','ec_r','rocksum','gypsum_r','sar_r','caco3_r','silttotal_r','fragsz_max','fragsz_ave')
# horiz_sub <- horiz_sub[,c(props, "cokey","depthwt")]
# horiz_sub <- na.omit(horiz_sub)
# horiz_sub_depthwt <- horiz_sub[,c("cokey","depthwt")]
# horiz_sub_depthwt <- plyr::ddply(horiz_sub_depthwt,~cokey,summarise,depthwtsum=sum(depthwt)) # determine total depth weight to allow for post NA normalization
# horiz_sub <- merge(horiz_sub, horiz_sub_depthwt,by="cokey")
# horiz_sub$depthwtnorm <- horiz_sub$depthwt / horiz_sub$depthwtsum
# ## Calculate depth weighted contibution of all relevant horizons
# for(p in props){horiz_sub[,paste(p,"depthwt",sep="_")] <- horiz_sub$depthwtnorm * horiz_sub[,p]}
# ## Summarize depth weighted property values for each component
# comp_sub_fn <- function(p){
#   horiz_subc <- horiz_sub
#   newpropnm <- paste(p,"sub",sep="_")
#   horiz_subc$sumprop <- horiz_subc[,paste(p,"depthwt",sep="_")]
#   comp_sum <- plyr::ddply(horiz_subc,~cokey,summarise, newprop = sum(sumprop))
#   comp_sum[,newpropnm] <- comp_sum$newprop
#   comp_sum$newprop <- NULL
#   rm(horiz_subc)
#   return(comp_sum)
# }
# ## Parallel function implementation
# cpus <- ifelse((detectCores() - 1) > length(props), length(props), detectCores() - 1)
# cl <- makeCluster(cpus, type="FORK")
# registerDoParallel(cl)
# comp_sub_sum <- parLapply(cl,props,comp_sub_fn)
# stopCluster(cl)
# ## Put list into dataframe
# comp_sub <- comp_sub_sum[[1]]
# for(i in seq(length(comp_sub_sum))){
#   newcol <- comp_sub_sum[[i]][,2]
#   newcolnm <- colnames(comp_sub_sum[[i]])[2]
#   #comp_surf <- cbind(comp_surf, newcol)
#   comp_sub[,newcolnm] <- newcol
#   print(paste("Done with ", i, sep=""))
# }
# rm(comp_sub_sum)
# 
# ## Now join depthwise properties to the points
# nasis@data <- dplyr::left_join(nasis@data,comp_surf, by="cokey")
# nasis@data <- dplyr::left_join(nasis@data,comp_sub, by="cokey")
# nasis@data$ESG <- gsub(" ","", nasis@data$ESG) # Remove spaces in SGU names
# 
# ## Extract some key values from rasters
# # slope <- raster("/home/tnaum/data/UCRB_Covariates/SLOPE.tif")
# # names(slope) <- "slope_deg" ## In degrees
# # ai <- raster("/ped/GIS_Archive/Global_Aridity_Index/ai_et0/ai_et0.tif")
# # names(ai) <- "AI"
# # nasis <- extract(slope, nasis, df=TRUE, sp=TRUE, progress='text')
# # nasis@data$slope_deg <- nasis@data$slope_deg/100 ## original units
# # ai.proj <- projection(ai)
# # nasis <- spTransform(nasis, CRS(ai.proj))
# # nasis <- extract(ai, nasis, df=TRUE, sp=TRUE, progress='text')
# # nasis <- spTransform(nasis, CRS(cov.proj))
# # ## Update units of extractions
# # nasis@data$AI <- nasis@data$AI/10000 # To original units (precip/pet)
# # nasis@data$slope_dem <- (tan(nasis@data$slope_deg*(pi/180)))*100 # To percent
# 
# ## Now use properties to assign data driven SGUs
# nasis@data$sgu <- NA
# nasis@data$sgu <- as.character(nasis@data$sgu)
# ## Keep bottoms, outcrops, and riparian and adjust salinity
# nasis@data$sgu <- ifelse(nasis@data$ESG=="Bottoms"|nasis@data$ESG=='SandyBottoms'|nasis@data$ESG=='SalineBottoms',nasis@data$ESG,nasis@data$sgu)
# nasis@data$sgu <- ifelse(nasis@data$ESG=="Outcrops"|nasis@data$ESG=='Riparian',nasis@data$ESG,nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$floodmax > 1, 'Bottoms',nasis@data$sgu)
# nasis@data$sgu <- ifelse((nasis@data$sgu=="Bottoms"|nasis@data$sgu=='SandyBottoms') & nasis@data$ec_r_sub > 4, 'SalineBottoms',nasis@data$sgu)
# nasis@data$sgu <- ifelse(nasis@data$sgu=="SalineBottoms" & nasis@data$ec_r_sub < 4, 'Bottoms',nasis@data$sgu)
# nasis@data$sgu <- ifelse(nasis@data$sgu=='Bottoms' & nasis@data$sandtotal_r_surf > 50 & nasis@data$sandtotal_r_sub > 50, 'SandyBottoms',nasis@data$sgu)
# ## Separate out upland units with salinity and gypsum influence
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$sar_r_surf > 8, 'SalineHills',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$gypsum_r_surf > 5, 'Gypsum',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$gypsum_r_sub > 10, 'Gypsum',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$ec_r_surf > 4, 'SalineHills',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$ec_r_sub > 8, 'SalineHills',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$ec_r_surf > 1.5, 'SalineUplands',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$ec_r_sub > 2, 'SalineUplands',nasis@data$sgu)
# ## Now break out shallow units
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$slope_r > 35 & nasis@data$rocksum_surf > 40, 'Breaks',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$resdept_r < 30, 'VeryShallow',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$resdept_r < 55, 'Shallow',nasis@data$sgu)
# ## Rocky Sites
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$rocksum_surf > 30, 'DeepRocky',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$rocksum_sub > 30, 'DeepRocky',nasis@data$sgu)
# ## Texture breaks
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$claytotal_r_surf > 30, 'ClayUplands',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$claytotal_r_sub > 35, 'ClayUplands',nasis@data$sgu)
# ## Create texture classes to help
# pt_text_surf <- data.frame(CLAY=nasis@data$claytotal_r_surf,SILT=nasis@data$silttotal_r_surf,SAND=nasis@data$sandtotal_r_surf, pid = nasis@data$pid,stringsAsFactors = F)
# pt_text_surf$sum <- pt_text_surf$CLAY + pt_text_surf$SAND + pt_text_surf$SILT
# pt_text_surf$sum<- ifelse(pt_text_surf$sum<75,NA,pt_text_surf$sum)
# pt_text_surf <- na.omit(pt_text_surf)
# pt_text_surf$txtcls_surf <- TT.points.in.classes(tri.data = pt_text_surf, class.sys   = "USDA.TT", PiC.type = 't',text.tol = 0.15)
# pt_text_sub <- data.frame(CLAY=nasis@data$claytotal_r_sub,SILT=nasis@data$silttotal_r_sub,SAND=nasis@data$sandtotal_r_sub, pid = nasis@data$pid, stringsAsFactors = F)
# pt_text_sub$sum <- pt_text_sub$CLAY + pt_text_sub$SAND + pt_text_sub$SILT
# pt_text_sub$sum<- ifelse(pt_text_sub$sum<75,NA,pt_text_sub$sum)
# pt_text_sub <- na.omit(pt_text_sub)
# pt_text_sub$txtcls_sub <- TT.points.in.classes(tri.data = pt_text_sub, class.sys   = "USDA.TT", PiC.type = 't',text.tol = 0.21)
# pt_text_sub <- pt_text_sub[,c('pid','txtcls_sub')]
# pt_text_surf <- pt_text_surf[,c('pid','txtcls_surf')]
# nasis@data <- left_join(nasis@data,pt_text_sub, by = "pid")
# nasis@data <- left_join(nasis@data,pt_text_surf, by = "pid")
# ## Use texture classes
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & (nasis@data$txtcls_surf=="Sa"|nasis@data$txtcls_surf=="LoSa")
#                         & (nasis@data$txtcls_sub=="Sa"|nasis@data$txtcls_sub=="LoSa"), 'SandyUplands',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$sandtotal_r_sub > 75 & nasis@data$sandtotal_r_surf > 75, 'SandyUplands',nasis@data$sgu) # 85
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & (nasis@data$txtcls_surf=="Sa"|nasis@data$txtcls_surf=="LoSa"|nasis@data$txtcls_surf=="SaLo"),
#                         'LoamyUplands',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & nasis@data$claytotal_r_surf < 20, 'LoamyUplands',nasis@data$sgu)
# nasis@data$sgu <- ifelse(is.na(nasis@data$sgu) & !is.na(nasis@data$claytotal_r_sub), 'FinerUplands',nasis@data$sgu)

# ## Save nasis pts feature class
# setwd(testfldr)
# # saveRDS(nasis, 'nasis_sgus.rds')
# nasis <- readRDS('nasis_sgus.rds')
# 
# ## Riparian points digitized by Sam Burch, USGS
# setwd("/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/pt_data/Riparian_Sample_Points")
# rip_pts34A <- readOGR(".", "Riparian_Points_MLRA34A_points")
# rip_pts34A@data[] <- lapply(rip_pts34A@data, function(x) if (is.factor(x)) as.character(x) else {x})
# rip_pts34B <- readOGR(".", "Riparian_Points_MLRA34B_points")
# rip_pts34B@data[] <- lapply(rip_pts34B@data, function(x) if (is.factor(x)) as.character(x) else {x})
# rip_pts35 <- readOGR(".", "Riparian_Points_MLRA35_points")
# rip_pts35@data[] <- lapply(rip_pts35@data, function(x) if (is.factor(x)) as.character(x) else {x})
# rip_pts36 <- readOGR(".", "Riparian_Points_MLRA36_points")
# rip_pts36@data[] <- lapply(rip_pts36@data, function(x) if (is.factor(x)) as.character(x) else {x})
# rip_pts <- do.call("rbind", list(rip_pts34A,rip_pts34B,rip_pts35,rip_pts36)) # combine all riparian points
# rip_pts@data$ESG <- "Riparian"
# rip_pts@data$LocID <- paste(rip_pts@data$X, rip_pts@data$Y, sep = "")
# rip_pts@data$project <- "riparian"
# rip_pts <- spTransform(rip_pts, CRS(cov.proj))
# ## Outcrop points from Colby Brungard
# outcr_pts <- read.delim("/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/pt_data/Colby_depth/regMatrix_7.1.19.csv", stringsAsFactors = F, sep=",")
# outcr_pts <- subset(outcr_pts, outcr_pts$DepthClass == 'BR')
# outcr_pts <- subset(outcr_pts, outcr_pts$source == 'CaseBasedReasoning')
# # outcr_pts <- subset(outcr_pts, outcr_pts$ELEVm_m < 2600)
# outcr_pts$LocID <-  paste(outcr_pts$X, outcr_pts$Y, sep = "")
# outcr_pts$coords.x1 <- outcr_pts$X
# outcr_pts$coords.x2 <- outcr_pts$Y
# coordinates(outcr_pts) <- ~ coords.x1 + coords.x2
# outcr.proj <- CRS("+init=epsg:5070") # From Colby Brungard
# projection(outcr_pts) <- outcr.proj
# outcr_pts <- spTransform(outcr_pts, CRS(cov.proj))
# outcr_pts@data$project <- "depth"
# outcr_pts@data$ESG <- "Outcrops"
# ## Outcrop points from Sam Burch
# setwd("/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/pt_data/Outcrops_Training_Points")
# outcr35 <- readOGR(".", "Outcrops35")
# outcr34A <- readOGR(".", "Outcrops34A")
# # outcr34B <- readOGR(".", "Outcrops34B")
# outcr36 <- readOGR(".", "Outcrops36")
# outcr_pts_B <- do.call("rbind", list(outcr35,outcr34A,outcr36)) # combine all riparian points except outcr34B,
# outcr_pts_B@data$sgu <- "Outcrops"
# outcr_pts_B@data$LocID <- paste(outcr_pts_B@coords[,1], outcr_pts_B@coords[,2], sep = "")
# outcr_pts_B@data$project <- "outcropB"
# outcr_pts_B <- spTransform(outcr_pts_B, CRS(cov.proj))
# ## Now merge different point
# nasism <- nasis[,c("project","LocID","sgu")]
# projection(nasism) <- cov.proj
# rip_pts$sgu <- rip_pts$ESG
# rip_ptsm <- rip_pts[,c("project","LocID","sgu")]
# projection(rip_ptsm) <- cov.proj
# outcr_pts$sgu <- outcr_pts$ESG
# outcr_ptsm <- outcr_pts[,c("project","LocID","sgu")]
# outcr_ptsbm <- outcr_pts_B[,c("project","LocID","sgu")]
# outcr_ptsbm@coords <- outcr_ptsbm@coords[,1:2] ## Take out Z coord to match settings of other layers
# shp.pts <- do.call("rbind", list(nasism,rip_ptsm,outcr_ptsm,outcr_ptsbm))
# 
# ## Extract real elevation points to constrain psuedopoints
# ELEVm <- raster("/home/tnaum/data/UCRB_Covariates/ELEVm.tif")
# shp.pts <- extract(ELEVm, shp.pts, df=TRUE, sp=TRUE, progress='text')
# shp.pts <- subset(shp.pts, shp.pts$ELEVm < 2750)
# # shp.pts.pts <- subset(shp.pts, shp.pts$project != "depth")
# # maxelev <- max(shp.pts.pts@data$ELEVm, na.rm=T)
# # shp.pts <- subset(shp.pts, shp.pts$project != "depth" | (shp.pts$project =="depth" & shp.pts$ELEVm < 2350))
# shp.pts@data$ELEVm <- NULL
# 
# ##Clip points outside of study area
# setwd("/home/tnaum/OneDrive/USGS/BLM_projects/Utah_BLM_Salinity/Huc6_boundary")
# polybound <- readOGR(".", "CO_River_watershed_Meade_alb")
# polybound <- spTransform(polybound, cov.proj)
# shp.pts <- shp.pts[polybound,]
# shp.pts <- subset(shp.pts, shp.pts$sgu != "")
# 
# ## Plot to ensure alignment bw points and rasters
# plot(projgrid)
# plot(shp.pts, add=TRUE)
# 
# 
# ## Parallelized extract: (larger datasets)
# setwd(covfolder)
# cpus <- detectCores(logical=TRUE)-2
# rasterOptions(maxmemory = 2e+09)
# sfInit(parallel=TRUE, cpus=cpus)
# sfExport("shp.pts", "cov.grids")
# sfLibrary(raster)
# sfLibrary(rgdal)
# ov.lst <- sfLapply(cov.grids, function(i){try( raster::extract(raster(i), shp.pts) )})
# snowfall::sfStop()
# #detach(package:snowfall, unload=TRUE)
# ov.lst <- as.data.frame(ov.lst)
# names(ov.lst) <- tools::file_path_sans_ext(basename(cov.grids))
# ov.lst$DID <- seq.int(nrow(ov.lst))
# shp.pts$DID <- seq.int(nrow(shp.pts))
# pts <- merge(as.data.frame(shp.pts),ov.lst, by="DID")
# pts[] <- lapply(pts, function(x) if (is.factor(x)) as.character(x) else {x})

## Save points and lookup table
#setwd(datafolder)
setwd(testfldr)
# saveRDS(pts, "UCRB_NASIS_BLM_sgu_pts_ext_soilgmrph_30mINT.rds")
# write.table(pts, "UCRB_NASIS_BLM_sgu_pts_ext_soilgmrph_30mINT.txt", sep = "\t", row.names = FALSE)
pts <- readRDS("UCRB_NASIS_BLM_sgu_pts_ext_soilgmrph_30mINT.rds")

## Standardize Point data classes
classname <- "sgu"
pts$Class <- pts$sgu ## UPDATE EVERY TIME1
Class <- "sgu" ## Dependent variable

## Prep for Random Forest
formulaStringRF <- as.formula(paste('Class ~', paste(gsub(".tif","", cov.grids), collapse="+")))# put in dep variable name
ptsc <- subset(pts, pts$Class != "NA")
ptsc <- subset(ptsc, ptsc$Class != "")
ptsc <- na.omit(ptsc)# Remove any record with NA's (in any column - be careful)

## Examine class balaance
summary(as.factor(ptsc$Class),maxsum=200)
ptscc <- ptsc
## Pull out duplicate locations
ptscc <- subset(ptscc, !duplicated(ptscc[c("LocID")])) #removes duplicates
## Clean up class names for use in lists
ptscc$Class <- as.character(ptscc$Class)
ptscc$Class <- gsub(" ","", ptscc$Class)
ptscc$Class <- gsub("&","", ptscc$Class)
ptscc$Class <- as.factor(ptscc$Class)
## Split into train/test
nfolds <- 5
ptscc$sets <- sample.int(nfolds,size =length(ptscc[,1]),replace=T)
pts_rf <- subset(ptscc, ptscc$sets == 1 | ptscc$sets == 2 | ptscc$sets==3 | ptscc$sets == 4)
pts_test <- subset(ptscc, ptscc$sets == 5)
pts_test$Class <- as.character(pts_test$Class)
summary.factor(pts_test$Class)
## Pull out just regression matrix for SMOTE
covnames <- gsub(".tif","",cov.grids)
regmxnames <- c("Class",covnames)
pts_rgmtx <- pts_rf[,regmxnames]
pts_rgmtx$Class <- as.character(pts_rgmtx$Class)
## Smote percentage calculations
classnumb.rf <- summary(as.factor(as.character(pts_rgmtx$Class)), maxsum=200)
classnumb.rf
## Set up class weights for RF or synthetic oversampling
classwts <- max(classnumb.rf)/classnumb.rf # to get a fully balance oversampling
classwts <- (1 - (classnumb.rf/max(classnumb.rf))+1)^2.5 # conservative oversampling
# classwts <- 1-(classnumb.rf^(1.2))/nrow(pts_rgmtx) ## For using classwts in RF
classwts
classwtslst <- lapply(split(classwts, names(classwts)), unname)
## Adjust weights based on expert reasoning
classwtslst$Riparian <- 0.74
classwtslst$Outcrops <- 6.5
classwtslst$LoamyUplands <- 1.75
classwtslst$SalineHills <- 2.4
classwtslst$DeepRocky <- 3.6
classwtslst$SalineBottoms <- 8.25
classwtslst$Gypsum <- 7.5
classwtslst$SandyBottoms <- 12.5
classwtslst$Breaks <- 3.7
classwtslst$SandyUplands <- 2.25
classwtslst$Bottoms <- 3.9
classwtslst$VeryShallow <- 2.5
classwtslst$Shallow <- 2.55
classwtslst$FinerUplands <- 2.3
classwtslst$ClayUplands <- 1.25
classwtslst$SalineUplands <- 3.35
## Now create new balanced training set
pts_rgmtx$Class <- as.factor(pts_rgmtx$Class)
balpts <- SmoteClassif(formulaStringRF, pts_rgmtx, C.perc = classwtslst)
summary(balpts$Class,maxsum=200)

############### Build Random Forest
detach(package:dplyr) # Interferes with RF foreach combine
cpus <- detectCores() - 2
cl <- makeCluster(getOption("cl.cores", cpus))
# cl <- makeCluster(cpus, type="FORK")
registerDoParallel(cl)
soiclass <- foreach(ntree=rep(floor(400/cpus), cpus), .combine=combine, .multicombine=TRUE, .verbose = T,
                    .packages='randomForest') %dopar% {
                      rf <- randomForest(formulaStringRF, balpts,ntree=ntree, importance=TRUE, proximity=FALSE, keep.forest=TRUE, nodesize=3) #,mtry=15,classwt=classwts)
                    } ## Does not return OOB statistics
stopCluster(cl)
class(soiclass) <- "randomForest"
#soiclass = randomForest(formulaStringRF, data = ptsc, importance=TRUE, proximity=FALSE, ntree=100, keep.forest=TRUE, nodesize=7,mtry=15,classwt=classwts)
soiclass
### Calculate OOB error rate
balpts$predOOB = predict(soiclass)
balpts$oobmatch = ifelse(balpts$Class == balpts$predOOB, 1, 0)
rf.err.rate = 1 - sum(balpts$oobmatch)/(length(balpts[,1])) # OOB Error rate
varImpPlot(soiclass)
#setwd(datafolder)
setwd(testfldr)
saveRDS(balpts, "pts_train_rf_soilgmrph_smote_30mINT.rds")
saveRDS(pts_test, "pts_test_rf_soilgmrph_smote_30mINT.rds")
saveRDS(soiclass,"rf_model_soilgmrph_smote_30mINT.rds")
saveRDS(pts_rf, "pts_rf_soilgmrph_smote_30mINT.rds")
## For re-runs
soiclass <- readRDS("rf_model_soilgmrph_smote_30mINT.rds")
pts_test <- readRDS("pts_test_rf_soilgmrph_smote_30mINT.rds")
balpts <- readRDS("pts_train_rf_soilgmrph_smote_30mINT.rds")
pts_rf <- readRDS("pts_rf_soilgmrph_smote_30mINT.rds")
############### Create Confusion matrix (for categorical models)
## Need to strip last column e.g. confusion[1:9,1:10] in rf object would be confusion[1:9,1:9]
obsclasses <- rownames(as.data.frame(summary(as.factor(as.character(balpts$Class)))))
oobclasses <- rownames(as.data.frame(summary(as.factor(as.character(balpts$predOOB)))))
comboclasses <- unique(append(obsclasses,oobclasses))
confmatrix <- as.data.frame(balpts[,c("Class","predOOB")])
confmatrix <- confmatrix[,c("Class","predOOB")]
oob_confmatx <- table(lapply(confmatrix, factor, levels = as.factor(comboclasses)))#levels need to come from field with most classes
OOBkappa <- Kappa.test(oob_confmatx, conf.level = 0.95)
write.table(oob_confmatx, file = "UCRB_NASIS_BLM_ESG_confmatrix_rf_soilgmrph_smote_30mINT.txt", sep = "\t", row.names = TRUE) ## needs work to save rigtht
## Create lookup table (for categorical predictions)
lookup_tab <- as.data.frame(cbind(seq(1:length(soiclass$classes)),soiclass$classes))
colnames(lookup_tab) <- c("value","SGU")
lookup_tab$value <- as.character(lookup_tab$value)
lookup_tab$ESG <- as.character(lookup_tab$SGU)
write.table(lookup_tab, file <- "UCRB_BLM_ESGs_lookup_rf_soilgmrph_smote_30mINT.txt", sep = "\t", row.names = FALSE)

## Look at test set statistics
pts_test$newpred <- as.character(predict(soiclass, newdata=pts_test))
pts_test$spredmatch <- ifelse(pts_test$newpred == pts_test$Class,1,0)
testsetaccur <- sum(pts_test$spredmatch, na.rm = T)/length(na.omit(pts_test$newpred))
## Confusion matrix and Kappa of test set validation
refclasses <- rownames(as.data.frame(summary(as.factor(as.character(pts_test$Class)))))
valclasses <- rownames(as.data.frame(summary(as.factor(as.character(pts_test$newpred)))))
allclasses <- unique(append(refclasses,valclasses))
conf_mat <- as.data.frame(pts_test[,c("Class","newpred")])
conf_mat <- conf_mat[,c("Class","newpred")]
val_confmatx <- table(lapply(conf_mat, factor, levels = as.factor(allclasses)))#levels need to come from field with most classes
valKappa <- Kappa.test(val_confmatx, conf.level = 0.95)
setwd(datafolder)
setwd(testfldr)
write.table(val_confmatx, file = "UCRB_ESG_val_confmatrix_rf_soilgmrph_smote_30m.txt", sep = "\t", row.names = TRUE) ## needs work to save rigtht
## Now calculate accuracy if considering top 2 classes
pts_test_probs <- predict(soiclass,newdata=pts_test,type="prob")
pts_test_probs <- as.data.frame(pts_test_probs)
pts_test$predclass1 <- names(pts_test_probs)[apply(pts_test_probs,1,which.max)]
pts_test$predclass2 <- colnames(pts_test_probs)[apply(pts_test_probs[,1:16], 1, function(x)
  which(x == sort(x, decreasing = TRUE)[2])[1])]
pts_test$predclass3 <- colnames(pts_test_probs)[apply(pts_test_probs[,1:16], 1, function(x)
  which(x == sort(x, decreasing = TRUE)[3])[1])]
pts_test$mtchtest <- ifelse(pts_test$newpred == pts_test$predclass1,1,0)
sum(pts_test$mtchtest) # should equal the length of the data frame
pts_test$spred2match <- ifelse(pts_test$predclass2 == pts_test$Class,1,0)
pts_test$spred3match <- ifelse(pts_test$predclass3 == pts_test$Class,1,0)
pts_test$top2match <- pts_test$spred2match + pts_test$spredmatch
pts_test$top3match <- pts_test$spred2match + pts_test$spredmatch + pts_test$spred3match
Top2testsetaccur <- sum(pts_test$top2match, na.rm = T)/length(na.omit(pts_test$newpred))
Top3testsetaccur <- sum(pts_test$top3match, na.rm = T)/length(na.omit(pts_test$newpred))
# predclass <- names(pts_test_probs)[apply(pts_test_probs,1,which.max)]
# predclass2 <- colnames(pts_test_probs)[apply(pts_test_probs[,1:15], 1, function(x)
#   which(x == sort(x, decreasing = TRUE)[2])[1])]
# maxprob <- apply(pts_test_probs,1,max)
# testprepprob <- data.frame(predclass,maxprob,Class=pts_test$Class,stringsAsFactors = F)

## detach other packages that might interfere with prediction
detach(package:itertools)
detach(package:doParallel)
detach(package:sf)
detach(package:UBL)

## Reference covar rasters to use in prediction
setwd(covfolder)
rasters <- stack(cov.grids)
#rasters = setMinMax(brick(rasters))
names(rasters)


## Predict onto covariate grid
setwd(datafolder)
cpus <- detectCores(logical=TRUE)-2
# memfree <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*0.9 # available RAM in kb
# memfree <- 235000000 #Avail RAM in kb
# maxmem <- ((memfree/(cpus))*0.95)*1000 # max mem per core in bytes: could be little higher
# chunksz <- maxmem*0.35
#rasterOptions(maxmemory = maxmem, chunksize = chunksz)
rasterOptions(maxmemory = 7e+09, chunksize = 5e+09)# 5e+09, chunksize = 2e+08)
## Parallelized predict
beginCluster(cpus,type='SOCK')
Sys.time()
pred <- clusterR(rasters, predict, args=list(model=soiclass),progress="text")
Sys.time()
## predprob makes a huge temp file on c drive (137+GB for COP), make sure there's room...
Sys.time()
predprob <- clusterR(rasters, predict, args=list(model=soiclass, type="prob", index = 1:length(soiclass$classes)),progress="text")
Sys.time()
setwd(testfldr)
writeRaster(predprob, overwrite=TRUE,filename="UCRB_BLM_SGUs_rf_initial_strawman_probmatrix_flt_30m.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
endCluster()
gc()
rasterOptions(maxmemory = 3e+09, chunksize = 3e+08)
beginCluster(cpus,type='SOCK')
predprobstk <- brick("UCRB_BLM_SGUs_rf_initial_strawman_probmatrix_flt_30m.tif")
## Rank function to get 2nd and 3rd most probable classes
# rank function creates a rasterstack with each layer representing one class and each pixel representing the prediction rank of that class
# The ranks go from highest number (most likely class) to lowest number (least likely class)
probrank <- clusterR(predprobstk, overlay, args=list(fun=function(x) (rank(x,ties.method= "random"))),progress = "text")
endCluster()
gc()
writeRaster(probrank, overwrite=TRUE,filename="UCRB_BLM_SGUs_rf_rank_30m.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
probrank <- stack("/home/tnaum/data/ESGs_strawman/UCRB_BLM_SGUs_rf_rank_30m.tif")
rasterOptions(maxmemory = 5e+09, chunksize = 2e+09)
beginCluster(cpus,type='SOCK')
zero <- clusterR(pred, calc, args=list(fun=function(x) (x*0)),progress = "text")
pred2 <- zero
pred3 <- zero
## Now loop through each class to fill top2 and top3 raster by referencing rank raster
for(no in lookup_tab$value){
  num <- as.numeric(no)
  rank2num <- as.numeric(length(lookup_tab$value))-1
  rank3num <- as.numeric(length(lookup_tab$value))-2
  rast <- probrank[[num]]
  rast2_stk <- stack(rast,pred2)
  pred2_fn <- function(rast,pred2) {
    ind <- ifelse((rast==rank2num),num,pred2)
    return(ind)
  }
  rast3_stk <- stack(rast,pred3)
  pred3_fn <- function(rast,pred3) {
    ind <- ifelse((rast==rank3num),num,pred3)
    return(ind)
  }
  pred2 <- clusterR(rast2_stk, overlay, args=list(fun=pred2_fn,forcefun=T), progress='text',export=c('rank2num','num'))
  pred3 <- clusterR(rast3_stk, overlay, args=list(fun=pred3_fn,forcefun=T), progress='text',export=c('rank3num','num'))
  print(paste(no, "is done", sep=" "))
}
# ## Now mask to original extent
mask_fn <-function(pred,predn) {
  ind <- ifelse(pred > 0,predn,NA)
  return(ind)
}
pred2m_stk <- stack(pred,pred2)
pred2 <- clusterR(pred2m_stk, overlay, args=list(fun=mask_fn,forcefun=T), progress='text')
pred3m_stk <- stack(pred,pred3)
pred3 <- clusterR(pred3m_stk, overlay, args=list(fun=mask_fn,forcefun=T), progress='text')
writeRaster(pred2, overwrite=TRUE, filename = paste(classname, "2ndclass.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
writeRaster(pred3, overwrite=TRUE, filename = paste(classname, "3rdclass.tif",sep="_"), options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")

## Now produce max probability surfaces
probmax <- clusterR(predprobstk, overlay, args=list(fun=function(x) (max(x))),progress = "text")
names(probmax) <- "probmax"
names(probmax)
probmaxint <- clusterR(probmax, calc, args=list(fun=function(x) (x*100)),progress = "text")
# predprobint <- clusterR(predprobstk, calc, args=list(fun=function(x) (x*100)),progress = "text")
## Shannon's Entropy uncertainty measure
f.entropy <- function(i, na.rm=TRUE) { # Function from D. Beaudette via email
  # add a very small fudge factor to remove 0
  i <- i + 1e-15
  # shannon entropy
  return(-sum(i * log(i), na.rm=na.rm))
}
Ent <- clusterR(predprobstk, overlay, args=list(fun=f.entropy),progress = "text")
endCluster()
gc()
setwd(datafolder)
setwd(testfldr)
writeRaster(pred, overwrite=TRUE,filename="UCRB_BLM_SGUs_rf_initial_soilgmrph_ADJsmote_30mINT.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
# writeRaster(predprobint, overwrite=TRUE,filename="UCRB_BLM_SGUs_rf_initial_strawman_probmatrix_int_30m.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
writeRaster(probmaxint, overwrite=TRUE,filename="UCRB_SGUs_probmax_int_30m.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
writeRaster(Ent, overwrite=TRUE,filename="UCRB_SGUs_ShanEntropy_int_30m.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='FLT4S', progress="text")
# pred <- raster("UCRB_BLM_SGUs_rf_initial_soilgmrph_ADJsmote_30mINT.tif")





################# Inference space mask
# vars <- names(sort(varImpPlot(soiclass)[,1],decreasing = T)[0:5]) # pick top n variables in model to mask by
# vars <- c("ELEVm", "ppt_ann","temp_ann") # Or pick yourself
# samp <- ptsc
# ## set how far outside of min and max you would like to let in mask
# min_margin <- 0.02
# max_margin <- 0.02
# ## list apply to compute dummy mask rasters for each variable to then combine in a min function
# mask_fn <- function(rast){
#   setwd(covfolder)
#   rastfile <- paste(rast,'.tif', sep="")
#   rastobj <- raster(rastfile)
#   lowbound <- min(samp[c(rast)]) - abs((min(samp[c(rast)]))*min_margin)
#   highbound <- max(samp[c(rast)]) + abs((max(samp[c(rast)]))*max_margin)
#   newmask <-  calc(rastobj, fun=function(x){ifelse(x>lowbound & x<highbound,1,0)})
#   setwd(maskfolder)
#   writeRaster(newmask, overwrite=TRUE,filename=paste(rast,"mask.tif",sep=""), options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
#   gc()
# }
# ## Setup up parallel list apply
# rasterOptions(maxmemory = 1e+07, chunksize = 5e+06) # Adjust to keep RAM from blowing up
# snowfall::sfInit(parallel=TRUE, cpus=16) ## Choose number of cpus available
# snowfall::sfExport("vars","samp", "covfolder","mask_fn","maskfolder","min_margin","max_margin")
# snowfall::sfLibrary(rgdal)
# snowfall::sfLibrary(raster)
# Sys.time()
# snowfall::sfLapply(vars, function(rast){mask_fn(rast)})
# Sys.time()
# snowfall::sfStop()
# ## Now combine all variable raster masks to get an overall mask
# rasterOptions(maxmemory = 7e+10, chunksize = 7e+09)
# setwd(maskfolder)
# # mask.grids <- list.files(pattern=".tif$")
# mask.grids <- paste(vars, "mask", ".tif", sep="")
# mask.rasts <- stack(mask.grids)
# maskoverlay.fn <- function(mask.rasts) {
#   ind <- min(mask.rasts)
#   return(ind)
# }
# beginCluster(30,type='SOCK')
# mask.overlay <- clusterR(mask.rasts, overlay, args=list(fun=maskoverlay.fn),progress = "text")
# setwd(modelfolder)
# writeRaster(mask.overlay, overwrite=TRUE,filename="strawman_mask_climate_overlay.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U',progress="text")
# ## Now combine a water mask and the inference mask to create final layers
# f_mask <- function(a,b,c) {
#   ind <- a*b*c
#   return(ind)
# }
# h2omask <- raster("E:/Models_active_work/UpCo/nlcd_watermask.tif")
# msk_stk <- stack(pred,h2omask,mask.overlay)
# pred_msk <- clusterR(msk_stk, overlay, args=list(fun=f_mask),progress = "text")
# writeRaster(pred_msk, overwrite=TRUE,filename="ESGs_strawman_masked_climate.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U',progress="text")
# endCluster()

