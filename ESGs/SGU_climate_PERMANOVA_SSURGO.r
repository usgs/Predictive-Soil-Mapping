##########################################
## PerMANOVA analysis of soil geomorphic units (SGUs)
## to test different key thresholds and designs
## to develop quantitative SGU definitions
## and climate variables for soil geomorphic groups based on
## reference state production data from NRCS EDIT.
##########################################
# AU: Travis Nauman, US Geological SUrvey, tnauman@usgs.gov
# 6/2021

## Check/install required packages
required.packages <- c("plyr","tidyverse","gridExtra","reshape2","vegan","doParallel","qpcR","sf","dplyr","soiltexture") 
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

# ## Key folders
# prodfolder <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/EDIT_prod"
# climfolder <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/shps_SSURGO18"
# scriptfolder <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize"
# ssurgo_fgdb <- "/ped/GIS_Archive/gSSURGO18/gSSURGO_CONUS.gdb/gSSURGO_CONUS.gdb"
# ssurgo_result_fldr <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/ssurgo_results"
# 
# ########### Summarize species production data
# setwd(prodfolder)
# Species <- read.delim("Duniway_qry_range_plant_composition.txt",stringsAsFactors = F)
# SpeciesFG_corr <- read.delim("Unique_Species_Symbols_Corrected_12192019.txt",stringsAsFactors = F)
# # Filter data to State 1 (reference state) production values 
# Species <- filter(Species, Species$STATE_SEQUENCE == 1)
# # Create new column "PRODUCTION_AVG" taking the average of the low and high production values
# Species$PRODUCTION_AVG <- (Species$PRODUCTION_LOW + Species$PRODUCTION_HIGH)/2
# # Correct functional group erros
# Species <- join(Species, SpeciesFG_corr, by = "PLANT_SYMBOL", type = "left", match = "first")
# # Summarize each specie's production accross communities within reference state for each ESD
# esd_species <- Species %>%
#   dplyr::group_by(ECOLOGICAL_SITE,PLANT_SYMBOL,COMMON_NAME,NEW_FG) %>%
#   dplyr::summarize(SPECIES_MEAN = mean(PRODUCTION_AVG))%>%
#   dplyr::ungroup() #to make sure groupings don't carry through to other dplyr calls
# # Create unique species name list for aggregation
# Species_Names <- unique(dplyr::select(Species, c(PLANT_SYMBOL, COMMON_NAME, SCIENTIFIC_NAME)))
# # Link table with photosynthetic pathway info
# SpecPP <- read.csv("SpeciesList_WesternUS_AcceptedSymbols_2020-01-06.csv",stringsAsFactors = F)
# SpecPP$PFG <- paste(SpecPP$PhotosyntheticPathway,SpecPP$Duration,SpecPP$GrowthHabitSub,sep="_")
# SpecPPc <- SpecPP[,c("SpeciesCode","PFG")]
# SpecPPc$PLANT_SYMBOL <- SpecPPc$SpeciesCode
# SpecPPc$SpeciesCode <- NULL
# esd_species <- dplyr::left_join(esd_species,SpecPPc, by="PLANT_SYMBOL")
# esd_species$PFG <-ifelse(is.na(esd_species$PFG),"",esd_species$PFG) # Make NAs blank so grouping function works
# # Aggregate Sagebrush
# esd_spec_summary <- esd_species %>%
#   dplyr::group_by(ECOLOGICAL_SITE) %>%
#   dplyr::summarise(bigsage = sum(SPECIES_MEAN[PLANT_SYMBOL=="ARTR2"|PLANT_SYMBOL=="ARTRW8"|PLANT_SYMBOL=="ARTRW"]),
#                    mtnsage = sum(SPECIES_MEAN[PLANT_SYMBOL=="ARTRV"]),
#                    basinsage = sum(SPECIES_MEAN[PLANT_SYMBOL=="ARTRT"]),
#                    cora = sum(SPECIES_MEAN[PLANT_SYMBOL=="CORA"]),
#                    quga = sum(SPECIES_MEAN[PLANT_SYMBOL=="QUGA"]),
#                    pinion = sum(SPECIES_MEAN[PLANT_SYMBOL=="PIED"|PLANT_SYMBOL=="PIMO"]),
#                    juniper = sum(SPECIES_MEAN[PLANT_SYMBOL=="JUNIP"|PLANT_SYMBOL=="JUCO6"|PLANT_SYMBOL=="JUDE2"|PLANT_SYMBOL=="JUMO"|PLANT_SYMBOL=="JUOS"|PLANT_SYMBOL=="JUSC2"]),
#                    aspen = sum(SPECIES_MEAN[PLANT_SYMBOL=="POTR5"]),
#                    ponderosa = sum(SPECIES_MEAN[PLANT_SYMBOL=="PIPO"]),
#                    greasewd = sum(SPECIES_MEAN[PLANT_SYMBOL=="SAVE4"]),
#                    saltbush = sum(SPECIES_MEAN[PLANT_SYMBOL=="ATCO4"|PLANT_SYMBOL=="ATCU"|PLANT_SYMBOL=="ATGA"|PLANT_SYMBOL=="ATOB"]),
#                    shadscale = sum(SPECIES_MEAN[PLANT_SYMBOL=="ATCO"]),
#                    spai = sum(SPECIES_MEAN[PLANT_SYMBOL=="SPAI"]),
#                    c3PerGr = sum(SPECIES_MEAN[PFG=="C3_Perennial_Graminoid"]),
#                    c4PerGr = sum(SPECIES_MEAN[PFG=="C4_Perennial_Graminoid"])) %>%
#   dplyr::ungroup() #to make sure groupings don't carry through to other dplyr calls
# 
# ################## Now summarize functional group production data
# setwd(prodfolder)
# Prod <- read.delim("Duniway_qry_annual_production.txt",stringsAsFactors = F) # EDIT Annual production data
# Prod <- filter(Prod, Prod$STATE_SEQUENCE == 1) # Filter to reference state communities
# ## Change Low and High production values to numeric
# Prod$ANNUAL_PRODUCTION_LOW <- as.numeric(Prod$ANNUAL_PRODUCTION_LOW)
# Prod$ANNUAL_PRODUCTION_HIGH <- as.numeric(Prod$ANNUAL_PRODUCTION_HIGH)
# ## Average production values (Low and High) for each ESD/Community/Functional Group
# Prod$ANNUAL_PRODUCTION_AVG <- (Prod$ANNUAL_PRODUCTION_LOW+Prod$ANNUAL_PRODUCTION_HIGH)/2
# ## Convert long format production data to wide format, this line also takes the average functional group prod values over all reference state communities for each ESD
# Prod_Wide <- dcast(Prod, ECOLOGICAL_SITE ~ PLANT_TYPE_LABEL, value.var = "ANNUAL_PRODUCTION_AVG", fun.aggregate = mean)
# ## Rename functional group names
# Prod_Wide <- Prod_Wide %>%
#   rename(
#     Grass = `Grass/Grasslike`,
#     Shrub = `Shrub/Vine`
#   )
# ## Remove rows with NA's in all Functional Group columns
# Prod_Wide <- Prod_Wide[rowSums(is.na(Prod_Wide[,2:5]))!=4,]
# ## Change NA's in missing functional groups data to 0's
# Prod_Wide$Forb[is.na(Prod_Wide$Forb)] <- 0
# Prod_Wide$Grass[is.na(Prod_Wide$Grass)] <- 0
# Prod_Wide$Shrub[is.na(Prod_Wide$Shrub)] <- 0
# Prod_Wide$Tree[is.na(Prod_Wide$Tree)] <- 0
# ## Create a total production value from the 4 functional groups for each ESD
# Prod_Wide$TOTAL_PROD <- rowSums(Prod_Wide[, c("Forb", "Grass", "Shrub", "Tree")])
# 
# ######## Summarize SSURGO based climate summaries and other soil properties by ESD
# setwd(climfolder)
# ai <- readRDS("./SSURGO18_polys_AI_UCRB/ssurgo18_polys_AI_ucrb_AttTab_ESDs.rds")
# pptratio <- readRDS("./SSURGO18_polys_pptratio_UCRB/ssurgo18_polys_pptratio_ucrb_AttTab_ESDs.rds")
# max_temp <- readRDS("./SSURGO18_polys_maxtemp_UCRB/ssurgo18_polys_maxtemp_ucrb_AttTab_ESDs.rds")
# min_temp <- readRDS("./SSURGO18_polys_mintemp_UCRB/ssurgo18_polys_mintemp_ucrb_AttTab_ESDs.rds")
# ## SSURGO sdvattribute table to guide data prep
# setwd(ssurgo_result_fldr)
# # sdvattribute <- sf::st_read(dsn = ssurgo_fgdb, layer = "sdvattribute")
# # sdvattribute[] <- lapply(sdvattribute, function(x) if (is.factor(x)) as.character(x) else {x})
# # write.table(sdvattribute,"sdvattribute.txt",sep="\t",row.names = T)
# ## SSURGO depth table
# cokeys <- ai$cokey
# restr.df <- sf::st_read(dsn = ssurgo_fgdb, layer = "corestrictions")
# restr.df[] <- lapply(restr.df, function(x) if (is.factor(x)) as.character(x) else {x})
# restrs <- restr.df[restr.df$cokey %in% cokeys,]
# restrs.all <- restrs[with(restrs,order(cokey,resdept_r)),]
# restrs.depth <- subset(restrs.all, restrs.all$reskind == "Lithic bedrock"|restrs.all$reskind == "Paralithic bedrock"|restrs.all$reskind == "Densic bedrock")
# restrs.depth <- subset(restrs.depth, !duplicated(restrs.depth[c("cokey")])) #removes duplicates
# depth <- dplyr::left_join(ai,restrs.depth, by="cokey")
# depth$resdept_r <- ifelse(is.na(depth$resdept_r), 201, depth$resdept_r)
# depth$resdept_r <- ifelse(depth$resdept_r > 201, 201, depth$resdept_r) # Reclass a couple >201 to 201 for consistent right censorship rules in SSURGO
# ## SSURGO horizon data: large tables processed first time and saved in intermediate folders for subsequent analysis
# # horizon.df <- sf::st_read(dsn = ssurgo_fgdb, layer = "chorizon")
# # horizon.df[] <- lapply(horizon.df, function(x) if (is.factor(x)) as.character(x) else {x})
# # horizs <- horizon.df[horizon.df$cokey %in% cokeys,]
# # saveRDS(horizs,"horizons_ucrb.rds")
# horizs <- readRDS("horizons_ucrb.rds")
# ## Now bring in rock fragment volume table
# # chfrags <- sf::st_read(dsn = ssurgo_fgdb, layer = "chfrags")
# # chfrags[] <- lapply(chfrags, function(x) if (is.factor(x)) as.character(x) else {x})
# chkeys <- horizs$chkey
# # chfrags <- chfrags[chfrags$chkey %in% chkeys,]
# # saveRDS(chfrags,"chfrags_ucrb.rds")
# chfrags <- readRDS("chfrags_ucrb.rds")
# ## Now summarize rock fragments for each horizon
# frags_chkey <- ddply(chfrags,~chkey,summarise,rocksum = sum(fragvol_r)) # Sum different rock sizes for each horizon
# chfrags <- left_join(chfrags,frags_chkey, by="chkey")
# chfrags$fragvolwt <- chfrags$fragvol_r / chfrags$rocksum
# chfrags$fragsize_r_wt <- chfrags$fragvolwt * chfrags$fragsize_r
# ## Representative rock size weighted by % cover of each size class for each component
# fragsz_ave_chkey <- ddply(chfrags,~chkey,summarise,fragsz_ave = sum(fragsize_r_wt))
# fragsz_max_chkey <- ddply(chfrags,~chkey,summarise,fragsz_max = max(fragsize_r))
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
# flood_annual <- ddply(comonth,~cokey,summarise,floodmax = max(floodnum))
# comonth$pondnum <- ifelse(comonth$pondfreqcl == 'None'|comonth$pondfreqcl == 'Rare',0,NA)
# comonth$pondnum <- ifelse(comonth$pondfreqcl == 'Occasional'|comonth$pondfreqcl == 'Frequent',1,comonth$pondnum)
# comonth$pondnum <- ifelse(is.na(comonth$pondnum),0,comonth$pondnum)
# pond_annual <- ddply(comonth,~cokey,summarise,pondmax = sum(pondnum))
# ## Component Geomorphic Description Table
# # cogeomordesc <- sf::st_read(dsn = ssurgo_fgdb, layer = "cogeomordesc")
# # cogeomordesc[] <- lapply(cogeomordesc, function(x) if (is.factor(x)) as.character(x) else {x})
# # cogeomordesc <- cogeomordesc[cogeomordesc$cokey %in% cokeys,]
# # saveRDS(cogeomordesc,"cogeomordesc_ucrb.rds")
# # cogeomordesc <- readRDS("cogeomordesc_ucrb.rds")
# # cogeomordesc <- cogeomordesc[cogeomordesc$rvindicator=='Yes',]
# # landforms <- cogeomordesc[cogeomordesc$geomftname=='Landform',c('cokey','geomfname')]
# # landforms <- ddply(landforms,~cokey,summarise,landform=geomfname[1])
# # landscapes <- cogeomordesc[cogeomordesc$geomftname=='Landscape',c('cokey','geomfname')]
# # landscapes <- ddply(landscapes,~cokey,summarise,landscape=geomfname[1])
# # saveRDS(landforms,"landforms_ucrb.rds")
# # saveRDS(landscapes,"landscapes_ucrb.rds")
# landforms <- readRDS("landforms_ucrb.rds")
# landscapes <- readRDS("landscapes_ucrb.rds")
# ## Component soil moisture table: soil moisture and water table by month
# # cosoilmoist <- sf::st_read(dsn = ssurgo_fgdb, layer = "cosoilmoist")
# # cosoilmoist[] <- lapply(cosoilmoist, function(x) if (is.factor(x)) as.character(x) else {x})
# # cosoilmoist <- cosoilmoist[cosoilmoist$comonthkey %in% comonth_keys,]
# # saveRDS(cosoilmoist,"cosoilmoist_ucrb.rds")
# # cosoilmoist <- readRDS("cosoilmoist_ucrb.rds")
# ## Component summary
# comps <- comp.df[,c('cokey','slope_r','runoff','hydgrp','hydricrating','drainagecl')]
# comps <- left_join(comps,flood_annual,by='cokey')
# comps[is.na(comps$floodmax),c('floodmax')] <- 0
# comps <- left_join(comps,pond_annual,by='cokey')
# comps[is.na(comps$pondmax),c('pondmax')] <- 0
# comps$h2otable <- ifelse(comps$drainagecl=='Excessively drained'|comps$drainagecl=='Somewhat excessively drained'|comps$drainagecl=='Well drained',0,1)
# comps_ssurgo <- left_join(ai,comps,by='cokey')
# comps_ssurgo <- left_join(comps_ssurgo,landforms,by='cokey')
# comps_ssurgo <- left_join(comps_ssurgo,landscapes,by='cokey')
# 
# ## Prep horizon weights to a depth interval: topsoil
# ud <- 0 # upper depth 
# ld <- 30 # lower depth
# horiz_surf <- subset(horizs, (as.numeric(horizs$hzdept_r) <= ud & as.numeric(horizs$hzdepb_r) > ud) | 
#                     (as.numeric(horizs$hzdepb_r) >= ld & as.numeric(horizs$hzdept_r) < ld) ) # subset to horizon overlapping range
# horiz_surf$indepthupper <- ifelse(as.numeric(horiz_surf$hzdept_r) <= ud & as.numeric(horiz_surf$hzdepb_r) < ld ,
#                                as.numeric(horiz_surf$hzdepb_r) - ud, 0)
# horiz_surf$indepthlower <- ifelse(as.numeric(horiz_surf$hzdept_r) >= ud & as.numeric(horiz_surf$hzdepb_r) >= ld,
#                                ld - as.numeric(horiz_surf$hzdept_r),0)
# horiz_surf$indepthoverlap <- ifelse(as.numeric(horiz_surf$hzdept_r) < ud & as.numeric(horiz_surf$hzdepb_r) > ld,
#                                  ld - ud,0)
# horiz_surf$indepth <- horiz_surf$indepthupper + horiz_surf$indepthlower + horiz_surf$indepthoverlap
# horiz_surf$depthwt <- horiz_surf$indepth / (ld-ud)
# props <- c('claytotal_r','sandtotal_r','ec_r','rocksum','gypsum_r','sar_r','caco3_r','silttotal_r','fragsz_max','fragsz_ave')
# horiz_surf <- horiz_surf[,c(props, "cokey","depthwt")]
# horiz_surf <- na.omit(horiz_surf)
# horiz_surf_depthwt <- horiz_surf[,c("cokey","depthwt")]
# horiz_surf_depthwt <- ddply(horiz_surf_depthwt,~cokey,summarise,depthwtsum=sum(depthwt)) # determine total depth weight to allow for post NA normalization 
# horiz_surf <- merge(horiz_surf, horiz_surf_depthwt,by="cokey")
# horiz_surf$depthwtnorm <- horiz_surf$depthwt / horiz_surf$depthwtsum
# ## Calculate depth weighted contibution of all relevant horizons
# for(p in props){horiz_surf[,paste(p,"depthwt",sep="_")] <- horiz_surf$depthwtnorm * horiz_surf[,p]}
# ## Summarize depth weighted property values for each component
# comp_surf_fn <- function(p){
#   horiz_surfc <- horiz_surf
#   newpropnm <- paste(p,"surf",sep="_")
#   horiz_surfc$sumprop <- horiz_surfc[,paste(p,"depthwt",sep="_")]
#   comp_sum <- ddply(horiz_surfc,~cokey,summarise, newprop = sum(sumprop))
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
#                        (as.numeric(horizs$hzdepb_r) >= ld & as.numeric(horizs$hzdept_r) < ld) ) # subset to horizon overlapping range
# horiz_sub$indepthupper <- ifelse(as.numeric(horiz_sub$hzdept_r) <= ud & as.numeric(horiz_sub$hzdepb_r) < ld ,
#                                   as.numeric(horiz_sub$hzdepb_r) - ud, 0)
# horiz_sub$indepthlower <- ifelse(as.numeric(horiz_sub$hzdept_r) >= ud & as.numeric(horiz_sub$hzdepb_r) >= ld,
#                                   ld - as.numeric(horiz_sub$hzdept_r),0)
# horiz_sub$indepthoverlap <- ifelse(as.numeric(horiz_sub$hzdept_r) < ud & as.numeric(horiz_sub$hzdepb_r) > ld,
#                                     ld - ud,0)
# horiz_sub$indepth <- horiz_sub$indepthupper + horiz_sub$indepthlower + horiz_sub$indepthoverlap
# horiz_sub$depthwt <- horiz_sub$indepth / (ld-ud)
# props <- c('claytotal_r','sandtotal_r','ec_r','rocksum','gypsum_r','sar_r','caco3_r','silttotal_r','fragsz_max','fragsz_ave')
# horiz_sub <- horiz_sub[,c(props, "cokey","depthwt")]
# horiz_sub <- na.omit(horiz_sub)
# horiz_sub_depthwt <- horiz_sub[,c("cokey","depthwt")]
# horiz_sub_depthwt <- ddply(horiz_sub_depthwt,~cokey,summarise,depthwtsum=sum(depthwt)) # determine total depth weight to allow for post NA normalization 
# horiz_sub <- merge(horiz_sub, horiz_sub_depthwt,by="cokey")
# horiz_sub$depthwtnorm <- horiz_sub$depthwt / horiz_sub$depthwtsum
# ## Calculate depth weighted contibution of all relevant horizons
# for(p in props){horiz_sub[,paste(p,"depthwt",sep="_")] <- horiz_sub$depthwtnorm * horiz_sub[,p]}
# ## Summarize depth weighted property values for each component
# comp_sub_fn <- function(p){
#   horiz_subc <- horiz_sub
#   newpropnm <- paste(p,"sub",sep="_")
#   horiz_subc$sumprop <- horiz_subc[,paste(p,"depthwt",sep="_")]
#   comp_sum <- ddply(horiz_subc,~cokey,summarise, newprop = sum(sumprop))
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
# ## Now join depthwise properties to the ssurgo table
# comp_surf_ssurgo <- dplyr::left_join(ai,comp_surf, by="cokey")
# comp_sub_ssurgo <- dplyr::left_join(ai,comp_sub, by="cokey")
# 
# # Remove NAs
# ai <- subset(ai, ai$AI > 0)
# pptratio <- subset(pptratio, pptratio$ppt_ratio > 0)
# max_temp <- subset(max_temp, max_temp$maxtemp > 0)
# min_temp <- subset(min_temp, min_temp$mintemp < 5)
# # Summarize climate and depth by ESD
# ai_mean <- ai %>%
#   dplyr::group_by(ecoclassid) %>%
#   dplyr::summarize(aimean = mean(AI))%>%
#   dplyr::ungroup()
# pptratio_mean <- pptratio %>%
#   dplyr::group_by(ecoclassid) %>%
#   dplyr::summarize(pptrtmean = mean(ppt_ratio))%>%
#   dplyr::ungroup()
# maxtemp_mean <- max_temp %>%
#   dplyr::group_by(ecoclassid) %>%
#   dplyr::summarize(maxtempmean = mean(maxtemp))%>%
#   dplyr::ungroup()
# mintemp_mean <- min_temp %>%
#   dplyr::group_by(ecoclassid) %>%
#   dplyr::summarize(mintempmean = mean(mintemp))%>%
#   dplyr::ungroup()
# depth_mean <- depth %>%
#   dplyr::group_by(ecoclassid) %>%
#   dplyr::summarize(depthmean = mean(resdept_r))%>%
#   dplyr::ungroup()
# surfsoil_mean <- comp_surf_ssurgo %>%
#   dplyr::group_by(ecoclassid) %>%
#   dplyr::summarize(clay_surf = mean(claytotal_r_surf,na.rm = T),
#       silt_surf = mean(silttotal_r_surf,na.rm = T),
#       sand_surf = mean(sandtotal_r_surf,na.rm = T),
#       ec_surf = mean(ec_r_surf,na.rm = T),
#       rock_surf = mean(rocksum_surf,na.rm = T),
#       gypsum_surf = mean(gypsum_r_surf,na.rm = T),
#       sar_surf = mean(sar_r_surf,na.rm = T),
#       rockszmax_surf = mean(fragsz_max_surf,na.rm = T),
#       rockszave_surf = mean(fragsz_ave_surf,na.rm = T),
#       caco3_surf = mean(caco3_r_surf,na.rm = T))%>%
#   dplyr::ungroup()
# subsoil_mean <- comp_sub_ssurgo %>%
#   dplyr::group_by(ecoclassid) %>%
#   dplyr::summarize(clay_sub = mean(claytotal_r_sub,na.rm = T),
#                    silt_sub = mean(silttotal_r_sub,na.rm = T),
#                    sand_sub = mean(sandtotal_r_sub,na.rm = T),
#                    ec_sub = mean(ec_r_sub,na.rm = T),
#                    rock_sub = mean(rocksum_sub,na.rm = T),
#                    gypsum_sub = mean(gypsum_r_sub,na.rm = T),
#                    sar_sub = mean(sar_r_sub,na.rm = T),
#                    rockszmax_sub = mean(fragsz_max_sub,na.rm = T),
#                    rockszave_sub = mean(fragsz_ave_sub,na.rm = T),
#                    caco3_sub = mean(caco3_r_sub,na.rm = T))%>%
#   dplyr::ungroup()
# comps_mean <- comps_ssurgo %>%
#   dplyr::group_by(ecoclassid) %>%
#   dplyr::summarize(slope = mean(slope_r,na.rm = T),
#                    landfrms = paste(names(sort(table(landform),decreasing=TRUE)[1:10]),collapse = ","),
#                    landscpes = paste(names(sort(table(landscape),decreasing=TRUE)[1:10]),collapse = ","),
#                    ponding = mean(pondmax,na.rm = T),
#                    flooding = mean(floodmax,na.rm = T),
#                    h2otable = mean(h2otable,na.rm = T))%>%
#   dplyr::ungroup()
# 
# ##### Now join all the data back together by ecosite ID
# Prod_Wide$ecoclassid <- Prod_Wide$ECOLOGICAL_SITE
# Prod_Wide$ECOLOGICAL_SITE <- NULL
# esd_spec_summary$ecoclassid <- esd_spec_summary$ECOLOGICAL_SITE
# esd_spec_summary$ECOLOGICAL_SITE <- NULL
# esd_final <- dplyr::inner_join(Prod_Wide,esd_spec_summary,by="ecoclassid")
# esd_final <- dplyr::inner_join(esd_final,ai_mean,by="ecoclassid")
# esd_final <- dplyr::inner_join(esd_final,pptratio_mean,by="ecoclassid")
# esd_final <- dplyr::inner_join(esd_final,mintemp_mean,by="ecoclassid")
# esd_final <- dplyr::inner_join(esd_final,maxtemp_mean,by="ecoclassid")
# esd_final <- dplyr::inner_join(esd_final,depth_mean,by="ecoclassid")
# esd_final <- dplyr::inner_join(esd_final,surfsoil_mean,by="ecoclassid")
# esd_final <- dplyr::inner_join(esd_final,subsoil_mean,by="ecoclassid")
# esd_final <- dplyr::inner_join(esd_final,comps_mean,by="ecoclassid")
# 
# ## Now join to soil geomorphic groups from correlation table
# corrtab <- read.delim("/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/UCRB_BLM_esd18_SGU_correlations.txt",stringsAsFactors = F)
# corrtabc <- subset(corrtab, corrtab$ESG != "")
# corrtabc <- corrtabc[,c("ecoclassid","ESG")]
# esd_final <- dplyr::inner_join(esd_final,corrtabc,by="ecoclassid")
# esd_final <- subset(esd_final, esd_final$ESG != "Outcrops")
# esd_final <- subset(esd_final, esd_final$ESG != "Riparian")
# esd_final$ESG <- gsub(" ","", esd_final$ESG) # Remove spaces in SGU names
# 
# ## Now use properties to assign data drive SGUs
# esd_final$sgu <- NA
# esd_final$sgu <- as.character(esd_final$sgu)
# ## Keep bottoms and adjust salinity
# esd_final$sgu <- ifelse(esd_final$ESG=="Bottoms"|esd_final$ESG=='SandyBottoms'|esd_final$ESG=='SalineBottoms',esd_final$ESG,esd_final$sgu)
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$flooding > 1, 'Bottoms',esd_final$sgu) 
# esd_final$sgu <- ifelse(!is.na(esd_final$sgu) & esd_final$ec_sub > 4, 'SalineBottoms',esd_final$sgu) 
# esd_final$sgu <- ifelse(!is.na(esd_final$sgu) & esd_final$ec_sub < 4, 'Bottoms',esd_final$sgu) 
# esd_final$sgu <- ifelse(esd_final$sgu=='Bottoms' & esd_final$sand_surf > 50 & esd_final$sand_sub > 50, 'SandyBottoms',esd_final$sgu)
# ## Separate out upland units with salinity and gypsum influence
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$sar_surf > 8, 'SalineHills',esd_final$sgu) 
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$gypsum_surf > 5, 'Gypsum',esd_final$sgu) 
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$gypsum_sub > 10, 'Gypsum',esd_final$sgu) 
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$ec_surf > 4, 'SalineHills',esd_final$sgu) 
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$ec_sub > 8, 'SalineHills',esd_final$sgu) 
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$ec_surf > 1.5, 'SalineUplands',esd_final$sgu) 
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$ec_sub > 2, 'SalineUplands',esd_final$sgu) 
# ## Now break out shallow units
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$slope > 35 & esd_final$rock_surf > 40, 'Breaks',esd_final$sgu) 
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$depthmean < 30, 'VeryShallow',esd_final$sgu)
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$depthmean < 55, 'Shallow',esd_final$sgu)
# ## Rocky Sites
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$rock_surf > 30, 'DeepRocky',esd_final$sgu) 
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$rock_sub > 30, 'DeepRocky',esd_final$sgu) 
# ## Texture breaks
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$clay_surf > 30, 'ClayUplands',esd_final$sgu) 
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$clay_sub > 35, 'ClayUplands',esd_final$sgu) 
# ## Create texture classes to help
# esd_text_surf <- data.frame(CLAY=esd_final$clay_surf,SILT=esd_final$silt_surf,SAND=esd_final$sand_surf, ecoclassid = esd_final$ecoclassid,stringsAsFactors = F)
# esd_text_surf <- na.omit(esd_text_surf)
# esd_text_surf$txtcls_surf <- TT.points.in.classes(tri.data = esd_text_surf, class.sys   = "USDA.TT", PiC.type = 't',text.tol = 0.04)
# esd_text_sub <- data.frame(CLAY=esd_final$clay_sub,SILT=esd_final$silt_sub,SAND=esd_final$sand_sub, ecoclassid = esd_final$ecoclassid, stringsAsFactors = F)
# esd_text_sub <- na.omit(esd_text_sub)
# esd_text_sub$txtcls_sub <- TT.points.in.classes(tri.data = esd_text_sub, class.sys   = "USDA.TT", PiC.type = 't',text.tol = 0.21)
# esd_text_sub <- esd_text_sub[,4:5]
# esd_text_surf <- esd_text_surf[,4:5]
# esd_final <- left_join(esd_final,esd_text_sub, by = "ecoclassid")
# esd_final <- left_join(esd_final,esd_text_surf, by = "ecoclassid")
# ## Use texture classes
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & (esd_final$txtcls_surf=="Sa"|esd_final$txtcls_surf=="LoSa")
#                         & (esd_final$txtcls_sub=="Sa"|esd_final$txtcls_sub=="LoSa"), 'SandyUplands',esd_final$sgu)
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$sand_sub > 75 & esd_final$sand_surf > 75, 'SandyUplands',esd_final$sgu) # 85
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & (esd_final$txtcls_surf=="Sa"|esd_final$txtcls_surf=="LoSa"|esd_final$txtcls_surf=="SaLo"),
#                         'LoamyUplands',esd_final$sgu)
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$clay_surf < 20, 'LoamyUplands',esd_final$sgu)
# esd_final$sgu <- ifelse(is.na(esd_final$sgu) & !is.na(esd_final$clay_sub), 'FinerUplands',esd_final$sgu)
# esd_final_orig <- esd_final
# esd_final <- esd_final[!is.na(esd_final$sgu),]
# 
# ## Save other useful objects
# depth_comps <-  depth[!duplicated(depth$cokey),]
# saveRDS(depth_comps,paste(ssurgo_result_fldr,"/depth_comps.rds",sep=""))
# saveRDS(restrs.all, paste(ssurgo_result_fldr,"/restrs_all.rds",sep=""))
# saveRDS(comp_surf, paste(ssurgo_result_fldr,"/comp_surf.rds",sep=""))
# saveRDS(comp_sub, paste(ssurgo_result_fldr,"/comp_sub.rds",sep=""))
# saveRDS(comps, paste(ssurgo_result_fldr,"/comp_drainage.rds",sep=""))

## Save esd df
setwd(scriptfolder)
# saveRDS(esd_final,"esd_final.rds")
esd_final <- readRDS("esd_final.rds") # Available in data folder in github

## PerManova to test SGU strength in explaining reference production data
daty <- esd_final[,c(colnames(esd_spec_summary),colnames(Prod_Wide))]
#daty <- esd_final[,c('Tree','Shrub','Grass','Forb','TOTAL_PROD','bigsage','mtnsage','basinsage','juniper',"greasewd","saltbush","quga","spai")]
daty$ecoclassid <- NULL
daty$ecoclassid.1 <- NULL
datx <- data.frame(sgu=as.factor(esd_final$sgu),ai=esd_final$aimean,maxt=esd_final$maxtempmean,mint=esd_final$mintempmean,pptrt=esd_final$pptrtmean)
## Quick retrieval of F stat and Rsq for model selection, pvalue checked in post rerun when perm will be raised to 1000
npmanmod_sgu <- adonis2(daty ~ sgu, data=datx,method="bray",permutations = 1, by = 'margin')
esd_final$sgumtch <- ifelse(esd_final$sgu==esd_final$ESG,1,0)

## Evaluate SGUs against various indicators in boxplots
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(Grass~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(Forb~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(Shrub~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(Tree~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(TOTAL_PROD~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(bigsage~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(mtnsage~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(basinsage~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(cora~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(quga~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(pinion~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(juniper~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(aspen~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(ponderosa~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(greasewd~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(saltbush~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(shadscale~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(spai~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(c3PerGr~sgu, data=esd_final,las=1, horizontal=T,ylab="")
boxplot(c4PerGr~sgu, data=esd_final,las=1, horizontal=T,ylab="")

############### Create Confusion matrix (for categorical models)
obsclasses <- rownames(as.data.frame(summary(as.factor(as.character(esd_final$ESG)))))
sguclasses <- rownames(as.data.frame(summary(as.factor(as.character(esd_final$sgu)))))
comboclasses <- unique(append(obsclasses,sguclasses))
confmatrix <- as.data.frame(esd_final[,c("ESG","sgu")])
confmatrix <- confmatrix[,c("ESG","sgu")]
confmatx <- table(lapply(confmatrix, factor, levels = as.factor(comboclasses)))#levels need to come from field with most classes
OOBkappa <- Kappa.test(confmatx, conf.level = 0.95)
write.table(confmatx, file = "ESG_SGU_confmatrix.txt", sep = "\t", row.names = TRUE) ## needs work to save right

## Full perManova runs: to see relative influence of various independent variables
cpus <- detectCores(logical=TRUE)-1
npmanmod_all <- adonis2(daty ~ sgu+ai+maxt+mint+pptrt, data=datx,method="bray",parallel = cpus,permutations = 999, by = 'margin')
# npmanmod_all_ints <- adonis2(daty ~ sgu*ai*maxt*mint*pptrt, data=datx,method="bray",parallel = cpus,permutations = 999, by = 'margin') # all interactions
# npmanmod_2x_ints <- adonis2(daty ~ .^2, data=datx,method="bray",parallel = cpus,permutations = 999, by = 'margin') # two way interactions
# setwd(scriptfolder)
# saveRDS(npmanmod_all,"Full_perMANOVA_model.rds")
# saveRDS(npmanmod_all_ints,"Full_interactions_perMANOVA_model.rds")
# saveRDS(npmanmod_2x_ints, "perMANOVA_model_2xInteractions.rds")
  
## Test for heterogenuity of dispersion among SGUs
braydist <- vegdist(daty, method="bray")
dispgrp <- as.factor(esd_final$sgu)
sgu_bdisp <- betadisper(braydist,dispgrp, bias.adjust = T, sqrt.dist = FALSE)
## Plot dispersions among groups
plot(sgu_bdisp)
boxplot(sgu_bdisp,horizontal = TRUE, varwidth = TRUE, xlab="", ylab="", cex.axis = .75,las=1)
## Permutation test to get more accurate Type 1 errors
sgu_bdisp_ptest <- permutest(sgu_bdisp, pairwise = T,permutations = 999, parallel = 63)
## Visualizing permutation distributions
# pstat <- permustats(sgu_bdisp_ptest)
# densityplot(pstat, scales = list(x = list(relation = "free")))

## SGU ordered look up table for reference to aggregation splits
## Create ordered table of SGUs to keep aggregation of groups limited to realistic groupings
SGUs <- data.frame(sgu=c("Breaks","SalineHills","Gypsum","VeryShallow","SalineUplands","Shallow","DeepRocky","SandyUplands","LoamyUplands","FinerUplands","ClayUplands","SandyBottoms","SalineBottoms","Bottoms"),
                   sgulev=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),stringsAsFactors = F, sguleg = c("1 - Breaks","2 - SalineHills","3 - Gypsum","4 - VeryShallow","5 - SalineUplands","6 - Shallow","7 - DeepRocky","8 - SandyUplands","9 - LoamyUplands","10 - FinerUplands","11 - ClayUplands","12 - SandyBottoms","13 - SalineBottoms","14 - Bottoms"))
SGUlevs <- SGUs$sgulev

esd_final <- dplyr::left_join(esd_final, SGUs, by="sgu")

## Ordination to examine grouping (vs dispersion?)
NMDS.scree<-function(x) { #x is the name of the data frame
  plot(rep(1,2),
       replicate(2,metaMDS(x,k=1)$stress),
       xlim=c(1,9),
       ylim=c(0,1),
       xlab="# of Dimensions",
       ylab="Stress",
       main="NMDS stress plot")
  for (i in 1:4) {
    points(rep(i+1,2),
           replicate(2,metaMDS(x,distance="bray",k=i+1)$stress))
  }}
NMDS.scree(daty)
#run NMS ordination 
set.seed(1)
ord <- metaMDS(daty,
               k=3, # number of dimensions
               trymax = 30) 
# save(ord, file = "UNR/Thesis/Analysis/PostRSAnalysis/FullModel/Ordination_allWSs_20190610.Rdata")
stressplot(ord) #stress plot. nonmetric R^2 = 1 - (stress^2)
ord$stress #0.134

## Plot ordination
spec_scores <- scores(ord, display = "species") %>%
  as.data.frame()
spec_scores$SpeciesCode <- rownames(spec_scores)
library(RColorBrewer)
library(viridisLite)
# create a palette of 22 colors for all the sgus
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
set.seed(10) #7,5 were best
pal_ecosite <- viridisLite::plasma(length(levels(as.factor(esd_final$sguleg))))
## Make plot
par(mfrow=c(2,2))
plot(ord, choices = c(1,2), type = "n", 
     xlim = c(-0.75, 0.6),
     ylim = c(-0.75, 0.75))
# points(ord, choices = c(1,2), display = "sites",
#        col=pal_ecosite[as.factor(esd_final$SGU)],
#        pch = 21, cex = .8, bg=pal_ecosite[as.factor(esd_final$SGU)])
text(ord, choices = c(1,2),labels=esd_final$sgulev,col = pal_ecosite[fct_reorder(as.factor(esd_final$sguleg),esd_final$sgulev,min)])

plot(ord, choices = c(1,3), type = "n", 
     xlim = c(-0.75, 0.6),
     ylim = c(-0.75, 0.75))
# points(ord, choices = c(1,2), display = "sites",
#        col=pal_ecosite[as.factor(esd_final$SGU)],
#        pch = 21, cex = .8, bg=pal_ecosite[as.factor(esd_final$SGU)])
text(ord, choices = c(1,3),labels=esd_final$sgulev,col = pal_ecosite[fct_reorder(as.factor(esd_final$sguleg),esd_final$sgulev,min)])

plot(ord, choices = c(2,3), type = "n", 
     xlim = c(-0.75, 0.6),
     ylim = c(-0.75, 0.75))
# points(ord, choices = c(1,2), display = "sites",
#        col=pal_ecosite[as.factor(esd_final$SGU)],
#        pch = 21, cex = .8, bg=pal_ecosite[as.factor(esd_final$SGU)])
text(ord, choices = c(2,3),labels=esd_final$sgulev,col = pal_ecosite[fct_reorder(as.factor(esd_final$sguleg),esd_final$sgulev,min)])

# Legend
plot(ord, type = "n", axes=FALSE,
     display = "sites",
     #col=pal_ecosite[envi_vars$GeomorphSoil],
     col=pal_ecosite[as.factor(esd_final$sguleg)],
     xlab = "",
     ylab = "")
legend(x="center",
       #legend = levels(envi_vars$GeomorphSoil),
       legend = levels(fct_reorder(as.factor(esd_final$sguleg),esd_final$sgulev,min)),
       fill = pal_ecosite,
       title = "Soil Geomorphic Units",
       cex = 0.7)

par(mfrow=c(1,1))
