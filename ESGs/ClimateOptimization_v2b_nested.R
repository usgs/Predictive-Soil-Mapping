##########################################
## Workflow to optimize soil geomorphic & climate breaks
## for ecological site groups based on
## reference state production data from NRCS EDIT
##### Nested combination of climate zones and SGUs
## This approach allows for aggregation of SGUs specific to each climate zone.
## This allows SGU aggregation tp be different for each climate zone.
##########################################
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
# loopfolder <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/loopind2bm1"
# resultfolder <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/results2b"
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
# 
# # ## SSURGO sdvattribute table to guide data prep
# # setwd(ssurgo_result_fldr)
# # # sdvattribute <- sf::st_read(dsn = ssurgo_fgdb, layer = "sdvattribute")
# # # sdvattribute[] <- lapply(sdvattribute, function(x) if (is.factor(x)) as.character(x) else {x})
# # # write.table(sdvattribute,"sdvattribute.txt",sep="\t",row.names = T)
# # ## SSURGO depth table
# # cokeys <- ai$cokey
# # restr.df <- sf::st_read(dsn = ssurgo_fgdb, layer = "corestrictions")
# # restr.df[] <- lapply(restr.df, function(x) if (is.factor(x)) as.character(x) else {x})
# # restrs <- restr.df[restr.df$cokey %in% cokeys,]
# # restrs.all <- restrs[with(restrs,order(cokey,resdept_r)),]
# # restrs.depth <- subset(restrs.all, restrs.all$reskind == "Lithic bedrock"|restrs.all$reskind == "Paralithic bedrock"|restrs.all$reskind == "Densic bedrock")
# # restrs.depth <- subset(restrs.depth, !duplicated(restrs.depth[c("cokey")])) #removes duplicates
# # depth <- dplyr::left_join(ai,restrs.depth, by="cokey")
# # depth$resdept_r <- ifelse(is.na(depth$resdept_r), 201, depth$resdept_r)
# # depth$resdept_r <- ifelse(depth$resdept_r > 201, 201, depth$resdept_r) # Reclass a couple >201 to 201 for consistent right censorship rules in SSURGO
# # ## SSURGO horizon data: large tables processed first time and saved in intermediate folders for subsequent analysis
# # # horizon.df <- sf::st_read(dsn = ssurgo_fgdb, layer = "chorizon")
# # # horizon.df[] <- lapply(horizon.df, function(x) if (is.factor(x)) as.character(x) else {x})
# # # horizs <- horizon.df[horizon.df$cokey %in% cokeys,]
# # # saveRDS(horizs,"horizons_ucrb.rds")
# # horizs <- readRDS("horizons_ucrb.rds")
# # ## Now bring in rock fragment volume table
# # # chfrags <- sf::st_read(dsn = ssurgo_fgdb, layer = "chfrags")
# # # chfrags[] <- lapply(chfrags, function(x) if (is.factor(x)) as.character(x) else {x})
# # chkeys <- horizs$chkey
# # # chfrags <- chfrags[chfrags$chkey %in% chkeys,]
# # # saveRDS(chfrags,"chfrags_ucrb.rds")
# # chfrags <- readRDS("chfrags_ucrb.rds")
# # frags_chkey <- ddply(chfrags,~chkey,summarise,rocksum = sum(fragvol_r)) # Sum different rock sizes for each horizon
# # chfrags <- left_join(chfrags,frags_chkey, by="chkey")
# # chfrags$fragvolwt <- chfrags$fragvol_r / chfrags$rocksum
# # chfrags$fragsize_r_wt <- chfrags$fragvolwt * chfrags$fragsize_r
# # ## Representative rock size weighted by % cover of each size class for each component
# # fragsz_ave_chkey <- ddply(chfrags,~chkey,summarise,fragsz_ave = sum(fragsize_r_wt))
# # fragsz_max_chkey <- ddply(chfrags,~chkey,summarise,fragsz_max = max(fragsize_r))
# # horizs <- left_join(horizs,frags_chkey,by="chkey")
# # horizs <- left_join(horizs,fragsz_ave_chkey,by="chkey")
# # horizs <- left_join(horizs,fragsz_max_chkey,by="chkey")
# # horizs$rocksum <- ifelse(is.na(horizs$rocksum), 0, horizs$rocksum) # Fill in 0s for horizons without rock
# # horizs$fragsz_max <- ifelse(is.na(horizs$fragsz_max), 2, horizs$fragsz_max)
# # horizs$fragsz_ave <- ifelse(is.na(horizs$fragsz_ave), 2, horizs$fragsz_ave)
# # ## Component table
# # # comp.df <- sf::st_read(dsn = ssurgo_fgdb, layer = "component")
# # # comp.df[] <- lapply(comp.df, function(x) if (is.factor(x)) as.character(x) else {x})
# # # comp.df <- comp.df[comp.df$cokey %in% cokeys,]
# # # saveRDS(comp.df,"compdf_ucrb.rds")
# # comp.df <- readRDS("compdf_ucrb.rds")
# # ## Comonth table: Flooding and ponding
# # # comonth <- sf::st_read(dsn = ssurgo_fgdb, layer = "comonth")
# # # comonth[] <- lapply(comonth, function(x) if (is.factor(x)) as.character(x) else {x})
# # # comonth <- comonth[comonth$cokey %in% cokeys,]
# # # saveRDS(comonth,"comonth_ucrb.rds")
# # comonth <- readRDS("comonth_ucrb.rds")
# # comonth_keys <- unique(comonth$comonthkey)
# # comonth$floodnum <- ifelse(comonth$flodfreqcl == 'Occasional'|comonth$flodfreqcl == 'Rare',1,NA)
# # comonth$floodnum <- ifelse(comonth$flodfreqcl == 'Frequent'|comonth$flodfreqcl == 'Very frequent',2,comonth$floodnum)
# # comonth$floodnum <- ifelse(is.na(comonth$floodnum),0,comonth$floodnum)
# # flood_annual <- ddply(comonth,~cokey,summarise,floodmax = max(floodnum))
# # comonth$pondnum <- ifelse(comonth$pondfreqcl == 'None'|comonth$pondfreqcl == 'Rare',0,NA)
# # comonth$pondnum <- ifelse(comonth$pondfreqcl == 'Occasional'|comonth$pondfreqcl == 'Frequent',1,comonth$pondnum)
# # comonth$pondnum <- ifelse(is.na(comonth$pondnum),0,comonth$pondnum)
# # pond_annual <- ddply(comonth,~cokey,summarise,pondmax = sum(pondnum))
# # ## Component Geomorphic Description Table
# # # cogeomordesc <- sf::st_read(dsn = ssurgo_fgdb, layer = "cogeomordesc")
# # # cogeomordesc[] <- lapply(cogeomordesc, function(x) if (is.factor(x)) as.character(x) else {x})
# # # cogeomordesc <- cogeomordesc[cogeomordesc$cokey %in% cokeys,]
# # # saveRDS(cogeomordesc,"cogeomordesc_ucrb.rds")
# # # cogeomordesc <- readRDS("cogeomordesc_ucrb.rds")
# # # cogeomordesc <- cogeomordesc[cogeomordesc$rvindicator=='Yes',]
# # # landforms <- cogeomordesc[cogeomordesc$geomftname=='Landform',c('cokey','geomfname')]
# # # landforms <- ddply(landforms,~cokey,summarise,landform=geomfname[1])
# # # landscapes <- cogeomordesc[cogeomordesc$geomftname=='Landscape',c('cokey','geomfname')]
# # # landscapes <- ddply(landscapes,~cokey,summarise,landscape=geomfname[1])
# # # saveRDS(landforms,"landforms_ucrb.rds")
# # # saveRDS(landscapes,"landscapes_ucrb.rds")
# # landforms <- readRDS("landforms_ucrb.rds")
# # landscapes <- readRDS("landscapes_ucrb.rds")
# # ## Component soil moisture table: soil moisture and water table by month
# # # cosoilmoist <- sf::st_read(dsn = ssurgo_fgdb, layer = "cosoilmoist")
# # # cosoilmoist[] <- lapply(cosoilmoist, function(x) if (is.factor(x)) as.character(x) else {x})
# # # cosoilmoist <- cosoilmoist[cosoilmoist$comonthkey %in% comonth_keys,]
# # # saveRDS(cosoilmoist,"cosoilmoist_ucrb.rds")
# # # cosoilmoist <- readRDS("cosoilmoist_ucrb.rds")
# # ## Component summary
# # comps <- comp.df[,c('cokey','slope_r','runoff','hydgrp','hydricrating','drainagecl')]
# # comps <- left_join(comps,flood_annual,by='cokey')
# # comps[is.na(comps$floodmax),c('floodmax')] <- 0
# # comps <- left_join(comps,pond_annual,by='cokey')
# # comps[is.na(comps$pondmax),c('pondmax')] <- 0
# # comps$h2otable <- ifelse(comps$drainagecl=='Excessively drained'|comps$drainagecl=='Somewhat excessively drained'|comps$drainagecl=='Well drained',0,1)
# # comps_ssurgo <- left_join(ai,comps,by='cokey')
# # comps_ssurgo <- left_join(comps_ssurgo,landforms,by='cokey')
# # comps_ssurgo <- left_join(comps_ssurgo,landscapes,by='cokey')
# # 
# # ## Prep horizon weights to a depth interval: topsoil
# # ud <- 0 # upper depth 
# # ld <- 30 # lower depth
# # horiz_surf <- subset(horizs, (as.numeric(horizs$hzdept_r) <= ud & as.numeric(horizs$hzdepb_r) > ud) | 
# #                        (as.numeric(horizs$hzdepb_r) >= ld & as.numeric(horizs$hzdept_r) < ld) ) # subset to horizon overlapping range
# # horiz_surf$indepthupper <- ifelse(as.numeric(horiz_surf$hzdept_r) <= ud & as.numeric(horiz_surf$hzdepb_r) < ld ,
# #                                   as.numeric(horiz_surf$hzdepb_r) - ud, 0)
# # horiz_surf$indepthlower <- ifelse(as.numeric(horiz_surf$hzdept_r) >= ud & as.numeric(horiz_surf$hzdepb_r) >= ld,
# #                                   ld - as.numeric(horiz_surf$hzdept_r),0)
# # horiz_surf$indepthoverlap <- ifelse(as.numeric(horiz_surf$hzdept_r) < ud & as.numeric(horiz_surf$hzdepb_r) > ld,
# #                                     ld - ud,0)
# # horiz_surf$indepth <- horiz_surf$indepthupper + horiz_surf$indepthlower + horiz_surf$indepthoverlap
# # horiz_surf$depthwt <- horiz_surf$indepth / (ld-ud)
# # props <- c('claytotal_r','sandtotal_r','ec_r','rocksum','gypsum_r','sar_r','caco3_r','silttotal_r','fragsz_max','fragsz_ave')
# # horiz_surf <- horiz_surf[,c(props, "cokey","depthwt")]
# # horiz_surf <- na.omit(horiz_surf)
# # horiz_surf_depthwt <- horiz_surf[,c("cokey","depthwt")]
# # horiz_surf_depthwt <- ddply(horiz_surf_depthwt,~cokey,summarise,depthwtsum=sum(depthwt)) # determine total depth weight to allow for post NA normalization 
# # horiz_surf <- merge(horiz_surf, horiz_surf_depthwt,by="cokey")
# # horiz_surf$depthwtnorm <- horiz_surf$depthwt / horiz_surf$depthwtsum
# # ## Calculate depth weighted contibution of all relevant horizons
# # for(p in props){horiz_surf[,paste(p,"depthwt",sep="_")] <- horiz_surf$depthwtnorm * horiz_surf[,p]}
# # ## Summarize depth weighted property values for each component
# # comp_surf_fn <- function(p){
# #   horiz_surfc <- horiz_surf
# #   newpropnm <- paste(p,"surf",sep="_")
# #   horiz_surfc$sumprop <- horiz_surfc[,paste(p,"depthwt",sep="_")]
# #   comp_sum <- ddply(horiz_surfc,~cokey,summarise, newprop = sum(sumprop))
# #   comp_sum[,newpropnm] <- comp_sum$newprop
# #   comp_sum$newprop <- NULL
# #   rm(horiz_surfc)
# #   return(comp_sum)
# # }
# # ## Parallel function implementation
# # cpus <- ifelse((detectCores() - 1) > length(props), length(props), detectCores() - 1)
# # cl <- makeCluster(cpus, type="FORK")
# # registerDoParallel(cl)
# # comp_surf_sum <- parLapply(cl,props,comp_surf_fn)
# # stopCluster(cl)
# # ## Put list into dataframe
# # comp_surf <- comp_surf_sum[[1]]
# # for(i in seq(length(comp_surf_sum))){
# #   newcol <- comp_surf_sum[[i]][,2]
# #   newcolnm <- colnames(comp_surf_sum[[i]])[2]
# #   #comp_surf <- cbind(comp_surf, newcol)
# #   comp_surf[,newcolnm] <- newcol
# #   print(paste("Done with ", i, sep=""))
# # }
# # rm(comp_surf_sum)
# # 
# # ## Prep horizon weights to a depth interval: subsoil
# # ud <- 30 # upper depth 
# # ld <- 100 # lower depth
# # horiz_sub <- subset(horizs, (as.numeric(horizs$hzdept_r) <= ud & as.numeric(horizs$hzdepb_r) > ud) | 
# #                       (as.numeric(horizs$hzdepb_r) >= ld & as.numeric(horizs$hzdept_r) < ld) ) # subset to horizon overlapping range
# # horiz_sub$indepthupper <- ifelse(as.numeric(horiz_sub$hzdept_r) <= ud & as.numeric(horiz_sub$hzdepb_r) < ld ,
# #                                  as.numeric(horiz_sub$hzdepb_r) - ud, 0)
# # horiz_sub$indepthlower <- ifelse(as.numeric(horiz_sub$hzdept_r) >= ud & as.numeric(horiz_sub$hzdepb_r) >= ld,
# #                                  ld - as.numeric(horiz_sub$hzdept_r),0)
# # horiz_sub$indepthoverlap <- ifelse(as.numeric(horiz_sub$hzdept_r) < ud & as.numeric(horiz_sub$hzdepb_r) > ld,
# #                                    ld - ud,0)
# # horiz_sub$indepth <- horiz_sub$indepthupper + horiz_sub$indepthlower + horiz_sub$indepthoverlap
# # horiz_sub$depthwt <- horiz_sub$indepth / (ld-ud)
# # props <- c('claytotal_r','sandtotal_r','ec_r','rocksum','gypsum_r','sar_r','caco3_r','silttotal_r','fragsz_max','fragsz_ave')
# # horiz_sub <- horiz_sub[,c(props, "cokey","depthwt")]
# # horiz_sub <- na.omit(horiz_sub)
# # horiz_sub_depthwt <- horiz_sub[,c("cokey","depthwt")]
# # horiz_sub_depthwt <- ddply(horiz_sub_depthwt,~cokey,summarise,depthwtsum=sum(depthwt)) # determine total depth weight to allow for post NA normalization 
# # horiz_sub <- merge(horiz_sub, horiz_sub_depthwt,by="cokey")
# # horiz_sub$depthwtnorm <- horiz_sub$depthwt / horiz_sub$depthwtsum
# # ## Calculate depth weighted contibution of all relevant horizons
# # for(p in props){horiz_sub[,paste(p,"depthwt",sep="_")] <- horiz_sub$depthwtnorm * horiz_sub[,p]}
# # ## Summarize depth weighted property values for each component
# # comp_sub_fn <- function(p){
# #   horiz_subc <- horiz_sub
# #   newpropnm <- paste(p,"sub",sep="_")
# #   horiz_subc$sumprop <- horiz_subc[,paste(p,"depthwt",sep="_")]
# #   comp_sum <- ddply(horiz_subc,~cokey,summarise, newprop = sum(sumprop))
# #   comp_sum[,newpropnm] <- comp_sum$newprop
# #   comp_sum$newprop <- NULL
# #   rm(horiz_subc)
# #   return(comp_sum)
# # }
# # ## Parallel function implementation
# # cpus <- ifelse((detectCores() - 1) > length(props), length(props), detectCores() - 1)
# # cl <- makeCluster(cpus, type="FORK")
# # registerDoParallel(cl)
# # comp_sub_sum <- parLapply(cl,props,comp_sub_fn)
# # stopCluster(cl)
# # ## Put list into dataframe
# # comp_sub <- comp_sub_sum[[1]]
# # for(i in seq(length(comp_sub_sum))){
# #   newcol <- comp_sub_sum[[i]][,2]
# #   newcolnm <- colnames(comp_sub_sum[[i]])[2]
# #   #comp_surf <- cbind(comp_surf, newcol)
# #   comp_sub[,newcolnm] <- newcol
# #   print(paste("Done with ", i, sep=""))
# # }
# # rm(comp_sub_sum)
# # 
# # ## Now join depthwise properties to the ssurgo table
# # comp_surf_ssurgo <- dplyr::left_join(ai,comp_surf, by="cokey")
# # comp_sub_ssurgo <- dplyr::left_join(ai,comp_sub, by="cokey")
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
# # depth_mean <- depth %>%
# #   dplyr::group_by(ecoclassid) %>%
# #   dplyr::summarize(depthmean = mean(resdept_r))%>%
# #   dplyr::ungroup()
# # surfsoil_mean <- comp_surf_ssurgo %>%
# #   dplyr::group_by(ecoclassid) %>%
# #   dplyr::summarize(clay_surf = mean(claytotal_r_surf,na.rm = T),
# #                    silt_surf = mean(silttotal_r_surf,na.rm = T),
# #                    sand_surf = mean(sandtotal_r_surf,na.rm = T),
# #                    ec_surf = mean(ec_r_surf,na.rm = T),
# #                    rock_surf = mean(rocksum_surf,na.rm = T),
# #                    gypsum_surf = mean(gypsum_r_surf,na.rm = T),
# #                    sar_surf = mean(sar_r_surf,na.rm = T),
# #                    rockszmax_surf = mean(fragsz_max_surf,na.rm = T),
# #                    rockszave_surf = mean(fragsz_ave_surf,na.rm = T),
# #                    caco3_surf = mean(caco3_r_surf,na.rm = T))%>%
# #   dplyr::ungroup()
# # subsoil_mean <- comp_sub_ssurgo %>%
# #   dplyr::group_by(ecoclassid) %>%
# #   dplyr::summarize(clay_sub = mean(claytotal_r_sub,na.rm = T),
# #                    silt_sub = mean(silttotal_r_sub,na.rm = T),
# #                    sand_sub = mean(sandtotal_r_sub,na.rm = T),
# #                    ec_sub = mean(ec_r_sub,na.rm = T),
# #                    rock_sub = mean(rocksum_sub,na.rm = T),
# #                    gypsum_sub = mean(gypsum_r_sub,na.rm = T),
# #                    sar_sub = mean(sar_r_sub,na.rm = T),
# #                    rockszmax_sub = mean(fragsz_max_sub,na.rm = T),
# #                    rockszave_sub = mean(fragsz_ave_sub,na.rm = T),
# #                    caco3_sub = mean(caco3_r_sub,na.rm = T))%>%
# #   dplyr::ungroup()
# # comps_mean <- comps_ssurgo %>%
# #   dplyr::group_by(ecoclassid) %>%
# #   dplyr::summarize(slope = mean(slope_r,na.rm = T),
# #                    landfrms = paste(names(sort(table(landform),decreasing=TRUE)[1:10]),collapse = ","),
# #                    landscpes = paste(names(sort(table(landscape),decreasing=TRUE)[1:10]),collapse = ","),
# #                    ponding = mean(pondmax,na.rm = T),
# #                    flooding = mean(floodmax,na.rm = T),
# #                    h2otable = mean(h2otable,na.rm = T))%>%
# #   dplyr::ungroup()
# 
# ##### Now join all the data back together by ecosite ID
# Prod_Wide$ecoclassid <- Prod_Wide$ECOLOGICAL_SITE
# Prod_Wide$ECOLOGICAL_SITE <- NULL
# esd_spec_summary$ecoclassid <- esd_spec_summary$ECOLOGICAL_SITE
# esd_spec_summary$ECOLOGICAL_SITE <- NULL
# ## Pull all data together in df
# # esd_final <- dplyr::inner_join(Prod_Wide,esd_spec_summary,by="ecoclassid")
# # esd_final <- dplyr::inner_join(esd_final,ai_mean,by="ecoclassid")
# # esd_final <- dplyr::inner_join(esd_final,pptratio_mean,by="ecoclassid")
# # esd_final <- dplyr::inner_join(esd_final,mintemp_mean,by="ecoclassid")
# # esd_final <- dplyr::inner_join(esd_final,maxtemp_mean,by="ecoclassid")
# # esd_final <- dplyr::inner_join(esd_final,depth_mean,by="ecoclassid")
# # esd_final <- dplyr::inner_join(esd_final,surfsoil_mean,by="ecoclassid")
# # esd_final <- dplyr::inner_join(esd_final,subsoil_mean,by="ecoclassid")
# # esd_final <- dplyr::inner_join(esd_final,comps_mean,by="ecoclassid")
# 
# ## Now join to soil geomorphic groups from correlation table
# corrtab <- read.delim("/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/UCRB_BLM_esd18_SGU_correlations.txt",stringsAsFactors = F)
# corrtabc <- subset(corrtab, corrtab$ESG != "")
# corrtabc <- corrtabc[,c("ecoclassid","ESG")]
# # esd_final <- dplyr::inner_join(esd_final,corrtabc,by="ecoclassid")
# # esd_final <- subset(esd_final, esd_final$ESG != "Outcrops")
# # esd_final <- subset(esd_final, esd_final$ESG != "Riparian")
# # esd_final$ESG <- gsub(" ","", esd_final$ESG) # Remove spaces in SGU names
# 
# ## Now use properties to assign data drive SGUs
# # esd_final$sgu <- NA
# # esd_final$sgu <- as.character(esd_final$sgu)
# # ## Keep bottoms and adjust salinity
# # esd_final$sgu <- ifelse(esd_final$ESG=="Bottoms"|esd_final$ESG=='SandyBottoms'|esd_final$ESG=='SalineBottoms',esd_final$ESG,esd_final$sgu)
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$flooding > 1, 'Bottoms',esd_final$sgu) 
# # esd_final$sgu <- ifelse(!is.na(esd_final$sgu) & esd_final$ec_sub > 4, 'SalineBottoms',esd_final$sgu) 
# # esd_final$sgu <- ifelse(!is.na(esd_final$sgu) & esd_final$ec_sub < 4, 'Bottoms',esd_final$sgu) 
# # esd_final$sgu <- ifelse(esd_final$sgu=='Bottoms' & esd_final$sand_surf > 50 & esd_final$sand_sub > 50, 'SandyBottoms',esd_final$sgu)
# # ## Separate out upland units with salinity and gypsum influence
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$sar_surf > 8, 'SalineHills',esd_final$sgu) 
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$gypsum_surf > 5, 'Gypsum',esd_final$sgu) 
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$gypsum_sub > 10, 'Gypsum',esd_final$sgu) 
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$ec_surf > 4, 'SalineHills',esd_final$sgu) 
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$ec_sub > 8, 'SalineHills',esd_final$sgu) 
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$ec_surf > 1.5, 'SalineUplands',esd_final$sgu) 
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$ec_sub > 2, 'SalineUplands',esd_final$sgu) 
# # ## Now break out shallow units
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$slope > 35 & esd_final$rock_surf > 40, 'Breaks',esd_final$sgu) 
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$depthmean < 30, 'VeryShallow',esd_final$sgu)
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$depthmean < 55, 'Shallow',esd_final$sgu)
# # ## Rocky Sites
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$rock_surf > 30, 'DeepRocky',esd_final$sgu) 
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$rock_sub > 30, 'DeepRocky',esd_final$sgu) 
# # ## Texture breaks
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$clay_surf > 30, 'ClayUplands',esd_final$sgu) 
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$clay_sub > 35, 'ClayUplands',esd_final$sgu) 
# # ## Create texture classes to help
# # esd_text_surf <- data.frame(CLAY=esd_final$clay_surf,SILT=esd_final$silt_surf,SAND=esd_final$sand_surf, ecoclassid = esd_final$ecoclassid,stringsAsFactors = F)
# # esd_text_surf <- na.omit(esd_text_surf)
# # esd_text_surf$txtcls_surf <- TT.points.in.classes(tri.data = esd_text_surf, class.sys   = "USDA.TT", PiC.type = 't',text.tol = 0.04)
# # esd_text_sub <- data.frame(CLAY=esd_final$clay_sub,SILT=esd_final$silt_sub,SAND=esd_final$sand_sub, ecoclassid = esd_final$ecoclassid, stringsAsFactors = F)
# # esd_text_sub <- na.omit(esd_text_sub)
# # esd_text_sub$txtcls_sub <- TT.points.in.classes(tri.data = esd_text_sub, class.sys   = "USDA.TT", PiC.type = 't',text.tol = 0.21)
# # esd_text_sub <- esd_text_sub[,4:5]
# # esd_text_surf <- esd_text_surf[,4:5]
# # esd_final <- left_join(esd_final,esd_text_sub, by = "ecoclassid")
# # esd_final <- left_join(esd_final,esd_text_surf, by = "ecoclassid")
# # ## Use texture classes
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & (esd_final$txtcls_surf=="Sa"|esd_final$txtcls_surf=="LoSa")
# #                         & (esd_final$txtcls_sub=="Sa"|esd_final$txtcls_sub=="LoSa"), 'SandyUplands',esd_final$sgu)
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$sand_sub > 75 & esd_final$sand_surf > 75, 'SandyUplands',esd_final$sgu) # 85
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & (esd_final$txtcls_surf=="Sa"|esd_final$txtcls_surf=="LoSa"|esd_final$txtcls_surf=="SaLo"),
# #                         'LoamyUplands',esd_final$sgu)
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & esd_final$clay_surf < 20, 'LoamyUplands',esd_final$sgu)
# # esd_final$sgu <- ifelse(is.na(esd_final$sgu) & !is.na(esd_final$clay_sub), 'FinerUplands',esd_final$sgu)
# # esd_final_orig <- esd_final
# # esd_final <- esd_final[!is.na(esd_final$sgu),]

# ## Save esd df
# setwd(scriptfolder)
# # saveRDS(esd_final,"esd_final.rds")
esd_final <- readRDS("esd_final.rds") # In github data folder

## SGU ordered look up table for reference to aggregation splits
## Create ordered table of SGUs to keep aggregation of groups limited to realistic groupings
SGUs <- data.frame(sgu=c("Breaks","SalineHills","Gypsum","VeryShallow","SalineUplands","Shallow","DeepRocky","SandyUplands","LoamyUplands","FinerUplands","ClayUplands","SandyBottoms","SalineBottoms","Bottoms"),
                   sgulev=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),stringsAsFactors = F, sguleg = c("1 - Breaks","2 - SalineHills","3 - Gypsum","4 - VeryShallow","5 - SalineUplands","6 - Shallow","7 - DeepRocky","8 - SandyUplands","9 - LoamyUplands","10 - FinerUplands","11 - ClayUplands","12 - SandyBottoms","13 - SalineBottoms","14 - Bottoms"))
SGUlevs <- SGUs$sgulev
# setwd(scriptfolder)
# write.table(SGUs, "UCRB_SGUs_v2.txt", sep = "\t", row.names = FALSE)

##### Create all potential climate SGU breaks and index to test in multivariate optimization
ai.brks <-  unname(quantile(esd_final$aimean,c(0.075,0.2,0.4,0.5,0.6,0.925)))
nrcs.aibrks <- c(0.126,0.1749) # 0.2179 is already represented by the 0.925 quantile
ai.brks <- sort(append(ai.brks,nrcs.aibrks))
min.t.brks <- unname(quantile(esd_final$mintempmean,c(0.075,0.2,0.35,0.5,0.65,0.8,0.925)))
max.t.brks <- unname(quantile(esd_final$maxtempmean,c(0.075,0.2,0.35,0.5,0.65,0.8,0.925)))
pptrt.brks <- unname(quantile(esd_final$pptrtmean,c(0.075,0.2,0.35,0.5,0.65,0.8,0.925)))

## Create all possible combinations allowed for each variable
aix <- seq(1,3)
aifun <- function(x){combn(ai.brks,x,simplify = T)}
ai.brks.all <- lapply(aix,FUN = aifun)
# Add endpoints and create indexable linear lists
ai.brks.long <- list(paste(min(esd_final$aimean)-0.01,max(esd_final$aimean)+0.01,sep=","))
for(l in 1:length(ai.brks.all)){
  lst <- ai.brks.all[l]
  for(i in 1:dim(lst[[1]])[2]){
    vect <- lst[[1]][,i]
    vect <- prepend(vect,(min(esd_final$aimean)-0.01))
    vect <- append(vect,(max(esd_final$aimean)+0.01))
    newvct <- c()
    vectints <- for(v in 1:(length(vect)-1)){
      ind <- v
      interv <- paste(vect[v],vect[v+1],sep=",")
      newvct <- append(newvct,interv)
    }
    newlst <- list(newvct)
    ai.brks.long <- append(ai.brks.long,newlst)
  }
}
# Ppt ratio
pptrtx <- seq(1,2)
pptrtfun <- function(x){combn(pptrt.brks,x,simplify = T)}
pptrt.brks.all <- lapply(pptrtx,FUN = pptrtfun)
# Add endpoints and create indexable linear lists
pptrt.brks.long <- list(paste(min(esd_final$pptrtmean)-0.01,max(esd_final$pptrtmean)+0.01,sep=","))
for(l in 1:length(pptrt.brks.all)){
  lst <- pptrt.brks.all[l]
  for(i in 1:dim(lst[[1]])[2]){
    vect <- lst[[1]][,i]
    vect <- prepend(vect,(min(esd_final$pptrtmean)-0.01))
    vect <- append(vect,(max(esd_final$pptrtmean)+0.01))
    newvct <- c()
    vectints <- for(v in 1:(length(vect)-1)){
      ind <- v
      interv <- paste(vect[v],vect[v+1],sep=",")
      newvct <- append(newvct,interv)
    }
    newlst <- list(newvct)
    pptrt.brks.long <- append(pptrt.brks.long,newlst)
  }
}
# Min temp
mintx <- seq(1,2)
mintfun <- function(x){combn(min.t.brks,x,simplify = T)}
mint.brks.all <- lapply(mintx,FUN = mintfun)
# Add endpoints and create indexable linear lists
mint.brks.long <- list(paste(min(esd_final$mintempmean)-0.01,max(esd_final$mintempmean)+0.01,sep=","))
for(l in 1:length(mint.brks.all)){
  lst <- mint.brks.all[l]
  for(i in 1:dim(lst[[1]])[2]){
    vect <- lst[[1]][,i]
    vect <- prepend(vect,(min(esd_final$mintempmean)-0.01))
    vect <- append(vect,(max(esd_final$mintempmean)+0.01))
    newvct <- c()
    vectints <- for(v in 1:(length(vect)-1)){
      ind <- v
      interv <- paste(vect[v],vect[v+1],sep=",")
      newvct <- append(newvct,interv)
    }
    newlst <- list(newvct)
    mint.brks.long <- append(mint.brks.long,newlst)
  }
}
# Max temp
maxtx <- seq(1,2)
maxtfun <- function(x){combn(max.t.brks,x,simplify = T)}
maxt.brks.all <- lapply(maxtx,FUN = maxtfun)
# Add endpoints and create indexable linear lists
maxt.brks.long <- list(paste(min(esd_final$maxtempmean)-0.01,max(esd_final$maxtempmean)+0.01,sep=","))
for(l in 1:length(maxt.brks.all)){
  lst <- maxt.brks.all[l]
  for(i in 1:dim(lst[[1]])[2]){
    vect <- lst[[1]][,i]
    vect <- prepend(vect,(min(esd_final$maxtempmean)-0.01))
    vect <- append(vect,(max(esd_final$maxtempmean)+0.01))
    newvct <- c()
    vectints <- for(v in 1:(length(vect)-1)){
      ind <- v
      interv <- paste(vect[v],vect[v+1],sep=",")
      newvct <- append(newvct,interv)
    }
    newlst <- list(newvct)
    maxt.brks.long <- append(maxt.brks.long,newlst)
  }
}

## Clear out GIS folders to free up RAM
rm(pptratio,ai,max_temp,min_temp)
gc()

#### Combine all potential combinations of climate breaks
## Use expand or expand.grid to create dfs of all possible combinations?
all.brks <- expand.grid(ai=ai.brks.long,mint=mint.brks.long,maxt=maxt.brks.long,pptrt=pptrt.brks.long,KEEP.OUT.ATTRS = F,stringsAsFactors = F)
# setwd(scriptfolder)
# saveRDS(all.brks, "all.breaks.rds")
# all.brks <- readRDS("all.breaks.rds")

######################### Mutivariate distance optimization fucntion
brks_list <-c(1:nrow(all.brks))
all.brks$modid <- 1:nrow(all.brks)
opt_fn <- function(x){
  brks <- all.brks[x,]
  aibrks <- brks[,c("ai")]
  aibrksall <- as.numeric(vector())
  for(ain in 1:length(aibrks[[1]])){aibrksall <- c(aibrksall,as.numeric(strsplit(aibrks[[1]][ain],",")[[1]]))}
  aibrksall <- unique(aibrksall) # vector of break values for ai
  mintbrks <- brks[,c("mint")]
  mintbrksall <- as.numeric(vector())
  for(mt in 1:length(mintbrks[[1]])){mintbrksall <- c(mintbrksall,as.numeric(strsplit(mintbrks[[1]][mt],",")[[1]]))}
  mintbrksall <- unique(mintbrksall) # vector of break values for min temp
  maxtbrks <- brks[,c("maxt")]
  maxtbrksall <- as.numeric(vector())
  for(mt in 1:length(maxtbrks[[1]])){maxtbrksall <- c(maxtbrksall,as.numeric(strsplit(maxtbrks[[1]][mt],",")[[1]]))}
  maxtbrksall <- unique(maxtbrksall) # vector of break values for max temp
  pptrtbrks <- brks[,c("pptrt")]
  pptrtbrksall <- as.numeric(vector())
  for(mt in 1:length(pptrtbrks[[1]])){pptrtbrksall <- c(pptrtbrksall,as.numeric(strsplit(pptrtbrks[[1]][mt],",")[[1]]))}
  pptrtbrksall <- unique(pptrtbrksall) # vector of break values for ppt ratio
  # Break out groups in df
  esd_final_fn <- esd_final
  #esd_final_fn$sgu <- esd_final_fn$ESG
  esd_final_fn <- dplyr::left_join(esd_final_fn, SGUs, by="sgu")
  esd_final_fn$ai_ind <- cut(esd_final_fn$aimean,aibrksall,labels=F,include.lowest=T)
  esd_final_fn$maxt_ind <- cut(esd_final_fn$maxtempmean,maxtbrksall,labels=F,include.lowest=T)
  esd_final_fn$mint_ind <- cut(esd_final_fn$mintempmean,mintbrksall,labels=F,include.lowest=T)
  esd_final_fn$pptrt_ind <- cut(esd_final_fn$pptrtmean,pptrtbrksall,labels=F,include.lowest=T)
  esd_final_fn$groups <- paste(esd_final_fn$ai_ind,esd_final_fn$maxt_ind,esd_final_fn$mint_ind,esd_final_fn$pptrt_ind,esd_final_fn$sgu,sep="_")
  ## Test how many groups: If more than 60, don't use
  groupnum <- length(unique(esd_final_fn$groups))
  # Empty dataframe to fill or return as an empty row 
  npmanovadf <- data.frame(modid=x,numgrps = groupnum, Rsq=as.numeric(""),F=as.numeric(""),SSgrp=as.numeric(""),SSres=as.numeric(""))
  ## if else block to limit analysis to scenarios with less than 60 groups
  if (groupnum > 60){
    return(npmanovadf)
    rm(esd_final_fn,brks,aibrks,aibrksall,mintbrks,mintbrksall,maxtbrks,maxtbrksall,pptrtbrks,pptrtbrksall,groupnum,npmanovadf)
    gc()
  } else {
    ## NPMNOVA using Bray dist in vegan package with adonis to look at class distictiveness
    daty <- esd_final_fn[,c(colnames(esd_spec_summary),colnames(Prod_Wide))]
    daty$ecoclassid <- NULL
    daty$ecoclassid.1 <- NULL
    datx <- data.frame(group=as.factor(esd_final_fn$groups))
    ## Quick retrieval of F stat and Rsq for model selection, pvalue checked in post rerun when perm will be raised to 1000
    npmanmod <- adonis(daty ~ group, data=datx,method="bray",parallel = 1,permutations = 1)
    npRsq <- npmanmod$aov.tab$R2[1]
    npF <- npmanmod$aov.tab$F.Model[1]
    npSSgrp <- npmanmod$aov.tab$SumsOfSqs[1]
    npSSres <- npmanmod$aov.tab$SumsOfSqs[2]
    npmanovadf$Rsq <- npRsq
    npmanovadf$F <- npF
    npmanovadf$SSgrp <- npSSgrp
    npmanovadf$SSres <- npSSres
    return(npmanovadf)
    rm(esd_final_fn,brks,aibrks,aibrksall,mintbrks,mintbrksall,maxtbrks,maxtbrksall,pptrtbrks,pptrtbrksall,groupnum,npmanovadf,daty,datx,npmanmod,npRsq,npF)
    gc()
  }
  ## Now clear memory and return df
  #gc()
}

## Parallel function implementation
cpus <- detectCores() - 1
# cpus <- 60
cl <- makeCluster(cpus, type="FORK")
registerDoParallel(cl)
pretime <- Sys.time()
npmanova_sum <- parLapply(cl,brks_list,opt_fn)
posttime <- Sys.time()
runtime <- posttime - pretime
# 0.0042 per row with mixed set (31 cpus)
stopCluster(cl)
## Put list into dataframe
pretime <- Sys.time()
npmanova_sumdf <- do.call("rbind", npmanova_sum)
posttime <- Sys.time()
runtime <- posttime - pretime
npmanova_sumdf <- na.omit(npmanova_sumdf)
rm(npmanova_sum)
gc()
setwd(scriptfolder)
# saveRDS(npmanova_sumdf, paste("npmanova_sumdf",".rds",sep=""))
# npmanova_sumdf <- readRDS("npmanova_sumdf.rds")

## Join to original rulesets and calculate AICc
trialsjn <- dplyr::left_join(npmanova_sumdf,all.brks,by="modid")
trialsjn$AICc <- nrow(esd_final)*log(trialsjn$SSres/nrow(esd_final)) + (2*(trialsjn$numgrps+2)) + ((2*(trialsjn$numgrps+2)*(trialsjn$numgrps+3))/(nrow(esd_final)-(trialsjn$numgrps+2)-1))
trialsjn <- trialsjn[order(trialsjn$AICc),]
trialsjn$AICcWt <- akaike.weights(trialsjn$AICc)$weights
trialsjn$AICcrelLL <- akaike.weights(trialsjn$AICc)$rel.LL
saveRDS(trialsjn, "trialsjn_v2.rds")
trialsjn <- readRDS("trialsjn_v2.rds")
## Narrow down to top n for SGU aggregation trial
trialsjn_sgu <- trialsjn[1:5,]
### Models 1 had distinctly better AICc with 
### wt being 0.15 and the next highest 0.084 out of 3145 models
### Min AICc is -876.68. The next few classes are still viable (wts 0.08-0.05)

############### Now investigate aggregation of SGUs within climate zones

# Soil Geomorph Units combinations to try
SGUx <- seq(1,3)
# Expert driven potential pairings of SGUs allowed referencing SGUlevs
sgucomblst <- c('1,2','1,3','1,4','2,3','2,4','2,5','3,4','3,5','4,5','4,6','4,7','5,6','5,7','5,8','5,9','6,7','7,8','8,12','8,9','9,10','9,12','10,11','12,13','12,14','13,14') # Other possible groupings: '1,3','1,5','8,10',
# Combinations written out: Breaks-Saline Hills, Breaks-Gypsum, Breaks-Very Shallow, Saline Hills-Gypsum, Saline Hills-Very Shallow, Saline Hills-Saline Uplands, 
#                           Gypsum-Very Shallow, Gypsum-Saline Uplands, Very Shallow-Saline Uplands, Very Shallow-Shallow, Very Shallow-Deep Rocky, Saline Uplands-Shallow, Saline Uplands-Deep Rocky, 
#                           Saline Uplands-Sandy Uplands, Saline Uplands-Loamy Uplands, Shallow-Deep Rocky, Deep Rocky-Sandy Uplands, Sandy Uplands-Sandy Bottoms, Sandy Uplands-Loamy Uplands, 
#                           Loamy Uplands-Finer Uplands, Loamy Uplands-Sandy Bottoms, Finer Uplands-Clay Uplands, Sandy Bottoms-Saline Bottoms, Sandy Bottoms-Bottoms, Saline Bottoms-Bottoms
sgufun <- function(x){combn(sgucomblst,x,simplify = T)} ## factor version
SGU.brks.all <- lapply(SGUx,FUN = sgufun)
# Add endpoints and create indexable linear lists
SGU.brks.long <- list()
for(l in 1:length(SGU.brks.all)){
  lst <- SGU.brks.all[l]
  for(i in 1:dim(lst[[1]])[2]){
    vect <- lst[[1]][,i]
    #vectlen <- length(vect)*2
    vectstr <- str_c(vect,sep="",collapse = ",")
    vectnum <- as.numeric(strsplit(vectstr, ",")[[1]])
    dups <- vectnum[duplicated(vectnum)]
    dups <- unique(dups)
    newrules <- vector()
    dgrpdf <- data.frame(dgrps = dups, dgrpsgus = as.character(dups),stringsAsFactors = F)
    for(t in vect){
      tnum <- as.numeric(strsplit(t, ",")[[1]])
      if(TRUE %in% (tnum %in% dups)){
        for(d in dups){
          #dgrp <- as.character(d)
          if(TRUE %in% (tnum %in% d)){
            newnum <- tnum[tnum != d]
            #dgrp <- append(dgrp,newnum)
            ## Add SGUs to each duplicate ruleset if there is a match
            for(s in 1:length(dgrpdf$dgrpsgus)){
              try(dgrpdf[as.numeric(strsplit(dgrpdf$dgrpsgus, ",")[[s]]) %in% d,]$dgrpsgus[s] <- paste(dgrpdf[as.numeric(strsplit(dgrpdf$dgrpsgus, ",")[[s]]) %in% d,]$dgrpsgus[s],as.character(newnum),sep=","),silent = T)
            }
          } else {
            next
            # dgrprule <- str_c(dgrp,sep="",collapse = ",")
            # newrules <- ifelse(!is.na(dgrprule), append(newrules,dgrprule),newrules)
          }
        }
      } else {
        newrules <- append(newrules,t)
      }
    }
    ## Remove any duplicates
    dgrpdf$dgrpsguslst <- strsplit(dgrpdf$dgrpsgus, ",")
    #try(dgrpdf$dgrpsgusnum <- sapply(1:nrow(dgrpdf),function(x){as.numeric(dgrpdf$dgrpsguslst[[x]])}),silent = T)
    try(dgrpdf$dgrpsgusnum <- lapply(dgrpdf$dgrpsguslst,function(x) as.numeric(x)),silent = T)
    #try(dgrpdf$dgrpsgusnum <- sapply(1:nrow(dgrpdf),function(x){sort(dgrpdf$dgrpsgusnum[[x]])}),silent = T)
    try(dgrpdf$dgrpsgusnum <- lapply(dgrpdf$dgrpsgusnum,function(x) sort(x)),silent = T)
    #try(dgrpdf$dgrpsgusnum <- sapply(1:nrow(dgrpdf),function(x){ifelse(!duplicated(dgrpdf$dgrpsgusnum[[x]]),dgrpdf$dgrpsgusnum[[x]],NA)}),silent = T)
    try(dgrpdf$dgrpsgusnum <- lapply(dgrpdf$dgrpsgusnum,function(x) ifelse(!duplicated(x),x,NA)),silent = T)
    try(dgrpdf$dgrpsgusnum <- lapply(dgrpdf$dgrpsgusnum,function(x) x[!is.na(x)]),silent = T)
    duperulelst <- dgrpdf$dgrpsgusnum
    duperulelst <- unique(duperulelst)
    duprulvect <- as.character()
    for(r in duperulelst){
      try(rchar <- str_c(r,sep="",collapse = ","),silent = T)
      try(duprulvect <- append(duprulvect,rchar),silent = T)
    }
    newrules <- append(newrules,duprulvect)
    newlst <- list(newrules)
    SGU.brks.long <- append(SGU.brks.long,newlst)
  }
  #rm(vect,vectnum,d,t,i,l,dgrp,newrules,dgrprule,dups,lst,tnum,newlst)
}
## Add option of no groupings
SGU.brks.long <- prepend(SGU.brks.long,"")
SGU.brks.long <- unique(SGU.brks.long) ## Remove duplicates that arise from class overlaps causing same groupings

## Combine climate breaks with nested SGU breaks
climmodids <- trialsjn_sgu$modid
## Now create all esd level climate breaks to expand sgu set
climbrkind <- list()
modind <- list()
for(t in 1:nrow(trialsjn_sgu)){
  brks <- trialsjn_sgu[t,]
  aibrks <- brks[,c("ai")]
  aibrksall <- as.numeric(vector())
  for(ain in 1:length(aibrks[[1]])){aibrksall <- c(aibrksall,as.numeric(strsplit(aibrks[[1]][ain],",")[[1]]))}
  aibrksall <- unique(aibrksall) # vector of break values for ai
  mintbrks <- brks[,c("mint")]
  mintbrksall <- as.numeric(vector())
  for(mt in 1:length(mintbrks[[1]])){mintbrksall <- c(mintbrksall,as.numeric(strsplit(mintbrks[[1]][mt],",")[[1]]))}
  mintbrksall <- unique(mintbrksall) # vector of break values for min temp
  maxtbrks <- brks[,c("maxt")]
  maxtbrksall <- as.numeric(vector())
  for(mt in 1:length(maxtbrks[[1]])){maxtbrksall <- c(maxtbrksall,as.numeric(strsplit(maxtbrks[[1]][mt],",")[[1]]))}
  maxtbrksall <- unique(maxtbrksall) # vector of break values for max temp
  pptrtbrks <- brks[,c("pptrt")]
  pptrtbrksall <- as.numeric(vector())
  for(mt in 1:length(pptrtbrks[[1]])){pptrtbrksall <- c(pptrtbrksall,as.numeric(strsplit(pptrtbrks[[1]][mt],",")[[1]]))}
  pptrtbrksall <- unique(pptrtbrksall) # vector of break values for ppt ratio
  ## Break out groups in df
  esd_final_fn <- esd_final
  #esd_final_fn$sgu <- esd_final_fn$ESG
  esd_final_fn <- dplyr::left_join(esd_final_fn, SGUs, by="sgu")
  esd_final_fn$ai_ind <- cut(esd_final_fn$aimean,aibrksall,labels=F,include.lowest=T)
  esd_final_fn$maxt_ind <- cut(esd_final_fn$maxtempmean,maxtbrksall,labels=F,include.lowest=T)
  esd_final_fn$mint_ind <- cut(esd_final_fn$mintempmean,mintbrksall,labels=F,include.lowest=T)
  esd_final_fn$pptrt_ind <- cut(esd_final_fn$pptrtmean,pptrtbrksall,labels=F,include.lowest=T)
  esd_final_fn$groups <- paste(t,esd_final_fn$ai_ind,esd_final_fn$maxt_ind,esd_final_fn$mint_ind,esd_final_fn$pptrt_ind,sep="_")
  climbrkind <- append(climbrkind,unique(esd_final_fn$groups))
  grpnum <- length(unique(esd_final_fn$groups))
  modindvect <- as.numeric(vector())
  modindvect <- rep.int(t,grpnum)
  modind <- append(modind,modindvect)
}
## Expand to every combination of climate break and SGU break
sgubrks <- expand.grid(clim=climbrkind,sgu=SGU.brks.long,KEEP.OUT.ATTRS = F,stringsAsFactors = F) #mod=modind,
## Label each row by model
sgubrks$mod <- 1
for(l in 1:nrow(sgubrks)){
  sgubrks$mod[l] <- as.numeric(strsplit(sgubrks$clim[[l]],split="_")[[1]][1])
}
## Split out breaks by model for finding all unique combinations of rules
uniqclim <- unique(sgubrks$clim)
uniqclimv <- c()
for(l in 1:length(uniqclim)){uniqclimv <- c(uniqclimv,uniqclim[[l]][1])}
sgubrksc <- tibble(clim=as.character(""),sgu=as.list(""),mod=as.numeric(0))
sgubrksc <- sgubrksc[0,]
## Loop through sgubrks to create a clean dataframe
for(r in 1:nrow(sgubrks)){
  clim <- sgubrks$clim[[r]]
  sgu <- sgubrks$sgu[r]
  mod <- sgubrks$mod[[r]]
  #for(s in 2:length(sgubrks$sgu[[r]])) {sgu <- paste(sgu,sgubrks$sgu[[r]][s],sep=",")}
  newdf <- tibble(clim=clim,sgu=sgu,mod=mod)
  sgubrksc <- rbind(sgubrksc,newdf)
}
# Now break into list by model and climate combos
modsuniq <- as.character(unique(sgubrks$mod))
sgubrkssm <- list()
for(m in 1:length(modsuniq)){
  modm <- as.character(modsuniq[m])
  climuniq <- unique(sgubrksc$clim[sgubrksc$mod==modm])
  for(c in 1:length(climuniq)){
    clim <- climuniq[c]
    climset <- sgubrksc[sgubrksc$clim==clim,]$sgu
    sgubrkssm[[modm]][[clim]] <- climset
  }
}


############ Test all combinations of SGU aggregation for top model ###########################
## Function for computing PerManova parameters for AICc based on all combinations of SGU for each climate
## group within each model
sgu_opt_fn <- function(x){
  brks <- trialsjn_sgu[modelnum,] # 1 referencing top model
  aibrks <- brks[,c("ai")]
  aibrksall <- as.numeric(vector())
  for(ain in 1:length(aibrks[[1]])){aibrksall <- c(aibrksall,as.numeric(strsplit(aibrks[[1]][ain],",")[[1]]))}
  aibrksall <- unique(aibrksall) # vector of break values for ai
  mintbrks <- brks[,c("mint")]
  mintbrksall <- as.numeric(vector())
  for(mt in 1:length(mintbrks[[1]])){mintbrksall <- c(mintbrksall,as.numeric(strsplit(mintbrks[[1]][mt],",")[[1]]))}
  mintbrksall <- unique(mintbrksall) # vector of break values for min temp
  maxtbrks <- brks[,c("maxt")]
  maxtbrksall <- as.numeric(vector())
  for(mt in 1:length(maxtbrks[[1]])){maxtbrksall <- c(maxtbrksall,as.numeric(strsplit(maxtbrks[[1]][mt],",")[[1]]))}
  maxtbrksall <- unique(maxtbrksall) # vector of break values for max temp
  pptrtbrks <- brks[,c("pptrt")]
  pptrtbrksall <- as.numeric(vector())
  for(mt in 1:length(pptrtbrks[[1]])){pptrtbrksall <- c(pptrtbrksall,as.numeric(strsplit(pptrtbrks[[1]][mt],",")[[1]]))}
  pptrtbrksall <- unique(pptrtbrksall) # vector of break values for ppt ratio
  # Break out groups in df
  esd_final_fn <- esd_final
  #esd_final_fn$sgu <- esd_final_fn$ESG
  esd_final_fn <- dplyr::left_join(esd_final_fn, SGUs, by="sgu")
  esd_final_fn$ai_ind <- cut(esd_final_fn$aimean,aibrksall,labels=F,include.lowest=T)
  esd_final_fn$maxt_ind <- cut(esd_final_fn$maxtempmean,maxtbrksall,labels=F,include.lowest=T)
  esd_final_fn$mint_ind <- cut(esd_final_fn$mintempmean,mintbrksall,labels=F,include.lowest=T)
  esd_final_fn$pptrt_ind <- cut(esd_final_fn$pptrtmean,pptrtbrksall,labels=F,include.lowest=T)
  esd_final_fn$groups <- paste(modelnum,esd_final_fn$ai_ind,esd_final_fn$maxt_ind,esd_final_fn$mint_ind,esd_final_fn$pptrt_ind,sep="_")
  # index climate groups
  groupind <- unique(esd_final_fn$groups)
  ## Now use group to pick the SGU breaks for each group
  #esd_final_fn$sgu_ind <- as.integer(NA)
  esd_final_fn$sgu_ind <- paste(esd_final_fn$groups,esd_final_fn$sgulev,sep="_")
  for(g in 1:length(groupind)){
    grp <- groupind[g]
    sgurules <- modbrks[,c(grp)][x][[1]] # indexing both the climate group and the xth SGU ruleset
    for(rl in sgurules){
      rlnum <- as.numeric(strsplit(rl, ",")[[1]])
      #esd_final_fn[esd_final_fn$sgulev %in% rlnum,]$sgu_ind <- rl # Put in group obj
      esd_final_fn$sgu_ind <- ifelse(esd_final_fn$sgulev %in% rlnum & esd_final_fn$groups==grp & rl != "", paste(esd_final_fn$groups,rl,sep="_"),esd_final_fn$sgu_ind)
      rm(rlnum)
    }
  }
  ## Test how many groups: If more than 60, don't use
  groupnum <- length(unique(esd_final_fn$sgu_ind))
  # Empty dataframe to fill or return as an empty row 
  npmanovadf <- data.frame(modid=x,numgrps = groupnum, Rsq=as.numeric(""),F=as.numeric(""),SSgrp=as.numeric(""),SSres=as.numeric(""))
  ## if else block to limit analysis to scenarios with less than 60 groups
  if (groupnum > 60){
    return(npmanovadf)
    rm(esd_final_fn,brks,aibrks,aibrksall,mintbrks,mintbrksall,maxtbrks,maxtbrksall,pptrtbrks,pptrtbrksall,groupnum,npmanovadf)
    gc()
  } else {
    ## NPMNOVA using Bray dist in vegan package with adonis to look at class distictiveness
    daty <- esd_final_fn[,c(colnames(esd_spec_summary),colnames(Prod_Wide))]
    daty$ecoclassid <- NULL
    daty$ecoclassid.1 <- NULL
    datx <- data.frame(group=as.factor(esd_final_fn$sgu_ind))
    ## Quick retrieval of F stat and Rsq for model selection, pvalue checked in post rerun when perm will be raised to 1000
    npmanmod <- adonis(daty ~ group, data=datx,method="bray",parallel = 1,permutations = 1)
    npRsq <- npmanmod$aov.tab$R2[1]
    npF <- npmanmod$aov.tab$F.Model[1]
    npSSgrp <- npmanmod$aov.tab$SumsOfSqs[1]
    npSSres <- npmanmod$aov.tab$SumsOfSqs[2]
    npmanovadf$Rsq <- npRsq
    npmanovadf$F <- npF
    npmanovadf$SSgrp <- npSSgrp
    npmanovadf$SSres <- npSSres
    return(npmanovadf)
    rm(esd_final_fn,brks,aibrks,aibrksall,mintbrks,mintbrksall,maxtbrks,maxtbrksall,pptrtbrks,pptrtbrksall,groupnum,npmanovadf,daty,datx,npmanmod,npRsq,npF)
    gc()
  }
  ## Now clear memory and return df
  #gc()
}

##### Now run sgu_opt_fn for top n models
# 1st model
modelnum <- 1 # Set which of the top n models to evaluate
modbrks <- expand.grid(sgubrkssm[[modelnum]],KEEP.OUT.ATTRS = F,stringsAsFactors = F) # must set index i.e. [[n]]
sguoptfunIND <- 1:nrow(modbrks)
modbrks$modid <- 1:nrow(modbrks)
ninst <- 2000000 # number of models to run in one parLapply
indexincr <- ceiling(nrow(modbrks)/ninst)
indcnt <- 0
setwd(loopfolder)
## Loop to split up full list of potential models into bit sized chunks
for(l in 1:indexincr){
  newbeg <- indcnt + 1
  if(l == indexincr){
    indcnt <- indcnt+(nrow(modbrks)-indcnt)
    brktup <- newbeg:indcnt
  } else {
    indcnt <- indcnt+ninst
    brktup <- newbeg:indcnt
  }
  lch <- as.character(l)
  saveRDS(brktup, paste("brktup",l,".rds",sep=""))
  print(paste("Done with ", lch))
}
## Now loop through for running batches of potential models
for(b in 1:indexincr){
  pretime <- Sys.time()
  setwd(loopfolder)
  brks_list_for <- readRDS(paste("brktup",b,".rds",sep=""))
  cpus <- detectCores() - 2
  # cpus <- 60
  cl <- makeCluster(cpus, type="FORK")
  registerDoParallel(cl)
  #pretime <- Sys.time()
  npmanova_sum_m2 <- parLapply(cl,brks_list_for,sgu_opt_fn)
  #posttime <- Sys.time()
  #runtime <- posttime - pretime
  # 0.0042 per row with mixed set (31 cpus)
  stopCluster(cl)
  ## Put list into dataframe
  npmanova_sumdf_m2 <- do.call("rbind", npmanova_sum_m2)
  npmanova_sumdf_m2 <- na.omit(npmanova_sumdf_m2)
  setwd(resultfolder)
  saveRDS(npmanova_sumdf_m2, paste("npmanova_sumdf_m2_",b,".rds",sep=""))
  rm(npmanova_sum_m2,npmanova_sumdf_m2)
  gc()
  posttime <- Sys.time()
  runtime <- posttime - pretime
  print(paste(b, "was done at", posttime,"in",runtime, sep=" "))
}
## Now loop through folder to bring together all trials
setwd(resultfolder)
optfiles <- list.files(pattern="rds$")
trialsm1agg3 <- readRDS(optfiles[1])[,1:6]
for(t in 2:length(optfiles)){
  newtrial <- readRDS(optfiles[t])[,1:6]
  trialsm1agg3 <- rbind(trialsm1agg3,newtrial)
  rm(newtrial)
  print(paste("done with ",t))
}

## Now join to models
setwd(scriptfolder)
trialsjnm1 <- dplyr::left_join(trialsm1agg3,modbrks,by="modid")
trialsjnm1$AICc <- nrow(esd_final)*log(trialsjnm1$SSres/nrow(esd_final)) + (2*(trialsjnm1$numgrps+2)) + ((2*(trialsjnm1$numgrps+2)*(trialsjnm1$numgrps+3))/(nrow(esd_final)-(trialsjnm1$numgrps+2)-1))
trialsjnm1 <- trialsjnm1[order(trialsjnm1$AICc),]
trialsjnm1$AICcWt <- akaike.weights(trialsjnm1$AICc)$weights
trialsjnm1$AICcrelLL <- akaike.weights(trialsjnm1$AICc)$rel.LL
saveRDS(trialsjnm1, "trialsjn2b_m1.rds")
trialsjnm1 <- readRDS("trialsjn2b_m1.rds")
## Subset to just models with relLL > 0.5
trialsjnm1l50 <- trialsjnm1[trialsjnm1$AICcrelLL > 0.5,]
saveRDS(trialsjnm1l50, "trialsjn2b_m1l50.rds")
trialsjnm1l50 <- readRDS("trialsjn2b_m1l50.rds")

######## Need to narrow down sgu groupings: too many to run a 3 climate break model
## Summarize all sgu groups used in the 2 climate model for relative likelihood greater than 0.75
sguvect <- as.character()
for(r in seq(1:nrow(trialsjnm1l50))){
#for(r in seq(1:100)){
  newstr <- trialsjnm1l50$`1_1_1_1_1`[[r]]
  for(s in newstr){
    sguvect <- append(sguvect,s)
  }
  rm(s,newstr)
  newstr2 <- trialsjnm1l50$`1_2_1_1_1`[[r]]
  for(s in newstr2){
    sguvect <- append(sguvect,s)
  }
}
sgufact <- as.factor(sguvect)
## Use summary of the breaks to narrow down for the next run
summary(sgufact)


##### Climate model 2: Now run sgu_opt_fn for #2 climate model
loopfolder2 <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/loopind2bm2"
resultfolder2 <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/results2bm2"
# Soil Geomorph Units combinations to try
SGUx <- seq(1,3) # All models from climate scenario 1 have at least 2 combos...
## Including breaks that occurred in any of the models with relative likelihood > 0.5 that occured in a least 
## 5 percent of climate strata (56 occurances) from climate model 1
sgucomblst <- c('1,3','1,4','3,4','5,9','6,7','7,8','8,9','9,10','10,11','12,13','12,14','13,14')
sgufun <- function(x){combn(sgucomblst,x,simplify = T)} ## factor version
SGU.brks.all <- lapply(SGUx,FUN = sgufun)
# Add endpoints and create indexable linear lists
SGU.brks.long <- list()
for(l in 1:length(SGU.brks.all)){
  lst <- SGU.brks.all[l]
  for(i in 1:dim(lst[[1]])[2]){
    vect <- lst[[1]][,i]
    #vectlen <- length(vect)*2
    vectstr <- str_c(vect,sep="",collapse = ",")
    vectnum <- as.numeric(strsplit(vectstr, ",")[[1]])
    dups <- vectnum[duplicated(vectnum)]
    dups <- unique(dups)
    newrules <- vector()
    dgrpdf <- data.frame(dgrps = dups, dgrpsgus = as.character(dups),stringsAsFactors = F)
    for(t in vect){
      tnum <- as.numeric(strsplit(t, ",")[[1]])
      if(TRUE %in% (tnum %in% dups)){
        for(d in dups){
          #dgrp <- as.character(d)
          if(TRUE %in% (tnum %in% d)){
            newnum <- tnum[tnum != d]
            #dgrp <- append(dgrp,newnum)
            ## Add SGUs to each duplicate ruleset if there is a match
            for(s in 1:length(dgrpdf$dgrpsgus)){
              try(dgrpdf[as.numeric(strsplit(dgrpdf$dgrpsgus, ",")[[s]]) %in% d,]$dgrpsgus[s] <- paste(dgrpdf[as.numeric(strsplit(dgrpdf$dgrpsgus, ",")[[s]]) %in% d,]$dgrpsgus[s],as.character(newnum),sep=","),silent = T)
            }
          } else {
            next
            # dgrprule <- str_c(dgrp,sep="",collapse = ",")
            # newrules <- ifelse(!is.na(dgrprule), append(newrules,dgrprule),newrules)
          }
        }
      } else {
        newrules <- append(newrules,t)
      }
    }
    ## Remove any duplicates
    dgrpdf$dgrpsguslst <- strsplit(dgrpdf$dgrpsgus, ",")
    #try(dgrpdf$dgrpsgusnum <- sapply(1:nrow(dgrpdf),function(x){as.numeric(dgrpdf$dgrpsguslst[[x]])}),silent = T)
    try(dgrpdf$dgrpsgusnum <- lapply(dgrpdf$dgrpsguslst,function(x) as.numeric(x)),silent = T)
    #try(dgrpdf$dgrpsgusnum <- sapply(1:nrow(dgrpdf),function(x){sort(dgrpdf$dgrpsgusnum[[x]])}),silent = T)
    try(dgrpdf$dgrpsgusnum <- lapply(dgrpdf$dgrpsgusnum,function(x) sort(x)),silent = T)
    #try(dgrpdf$dgrpsgusnum <- sapply(1:nrow(dgrpdf),function(x){ifelse(!duplicated(dgrpdf$dgrpsgusnum[[x]]),dgrpdf$dgrpsgusnum[[x]],NA)}),silent = T)
    try(dgrpdf$dgrpsgusnum <- lapply(dgrpdf$dgrpsgusnum,function(x) ifelse(!duplicated(x),x,NA)),silent = T)
    try(dgrpdf$dgrpsgusnum <- lapply(dgrpdf$dgrpsgusnum,function(x) x[!is.na(x)]),silent = T)
    duperulelst <- dgrpdf$dgrpsgusnum
    duperulelst <- unique(duperulelst)
    duprulvect <- as.character()
    for(r in duperulelst){
      try(rchar <- str_c(r,sep="",collapse = ","),silent = T)
      try(duprulvect <- append(duprulvect,rchar),silent = T)
    }
    newrules <- append(newrules,duprulvect)
    newlst <- list(newrules)
    SGU.brks.long <- append(SGU.brks.long,newlst)
  }
  #rm(vect,vectnum,d,t,i,l,dgrp,newrules,dgrprule,dups,lst,tnum,newlst)
}
## Add option of no groupings
SGU.brks.long <- prepend(SGU.brks.long,"")
SGU.brks.long <- unique(SGU.brks.long) ## Remove duplicates that arise from class overlaps causing same groupings

## Combine climate breaks with nested SGU breaks
climmodids <- trialsjn_sgu$modid
## Now create all esd level climate breaks to expand sgu set
climbrkind <- list()
modind <- list()
for(t in 1:nrow(trialsjn_sgu)){
  brks <- trialsjn_sgu[t,]
  aibrks <- brks[,c("ai")]
  aibrksall <- as.numeric(vector())
  for(ain in 1:length(aibrks[[1]])){aibrksall <- c(aibrksall,as.numeric(strsplit(aibrks[[1]][ain],",")[[1]]))}
  aibrksall <- unique(aibrksall) # vector of break values for ai
  mintbrks <- brks[,c("mint")]
  mintbrksall <- as.numeric(vector())
  for(mt in 1:length(mintbrks[[1]])){mintbrksall <- c(mintbrksall,as.numeric(strsplit(mintbrks[[1]][mt],",")[[1]]))}
  mintbrksall <- unique(mintbrksall) # vector of break values for min temp
  maxtbrks <- brks[,c("maxt")]
  maxtbrksall <- as.numeric(vector())
  for(mt in 1:length(maxtbrks[[1]])){maxtbrksall <- c(maxtbrksall,as.numeric(strsplit(maxtbrks[[1]][mt],",")[[1]]))}
  maxtbrksall <- unique(maxtbrksall) # vector of break values for max temp
  pptrtbrks <- brks[,c("pptrt")]
  pptrtbrksall <- as.numeric(vector())
  for(mt in 1:length(pptrtbrks[[1]])){pptrtbrksall <- c(pptrtbrksall,as.numeric(strsplit(pptrtbrks[[1]][mt],",")[[1]]))}
  pptrtbrksall <- unique(pptrtbrksall) # vector of break values for ppt ratio
  # Break out groups in df
  esd_final_fn <- esd_final
  #esd_final_fn$sgu <- esd_final_fn$ESG
  esd_final_fn <- dplyr::left_join(esd_final_fn, SGUs, by="sgu")
  esd_final_fn$ai_ind <- cut(esd_final_fn$aimean,aibrksall,labels=F,include.lowest=T)
  esd_final_fn$maxt_ind <- cut(esd_final_fn$maxtempmean,maxtbrksall,labels=F,include.lowest=T)
  esd_final_fn$mint_ind <- cut(esd_final_fn$mintempmean,mintbrksall,labels=F,include.lowest=T)
  esd_final_fn$pptrt_ind <- cut(esd_final_fn$pptrtmean,pptrtbrksall,labels=F,include.lowest=T)
  esd_final_fn$groups <- paste(t,esd_final_fn$ai_ind,esd_final_fn$maxt_ind,esd_final_fn$mint_ind,esd_final_fn$pptrt_ind,sep="_")
  climbrkind <- append(climbrkind,unique(esd_final_fn$groups))
  grpnum <- length(unique(esd_final_fn$groups))
  modindvect <- as.numeric(vector())
  modindvect <- rep.int(t,grpnum)
  modind <- append(modind,modindvect)
}
## Expand to every combination of climate break and SGU break
sgubrks <- expand.grid(clim=climbrkind,sgu=SGU.brks.long,KEEP.OUT.ATTRS = F,stringsAsFactors = F) #mod=modind,
## Label each row by model
sgubrks$mod <- 1
for(l in 1:nrow(sgubrks)){
  sgubrks$mod[l] <- as.numeric(strsplit(sgubrks$clim[[l]],split="_")[[1]][1])
}
## Split out breaks by model for finding all unique combinations of rules
uniqclim <- unique(sgubrks$clim)
uniqclimv <- c()
for(l in 1:length(uniqclim)){uniqclimv <- c(uniqclimv,uniqclim[[l]][1])}
sgubrksc <- tibble(clim=as.character(""),sgu=as.list(""),mod=as.numeric(0))
sgubrksc <- sgubrksc[0,]
## Loop through sgubrks to create a clean dataframe
for(r in 1:nrow(sgubrks)){
  clim <- sgubrks$clim[[r]]
  sgu <- sgubrks$sgu[r]
  mod <- sgubrks$mod[[r]]
  #for(s in 2:length(sgubrks$sgu[[r]])) {sgu <- paste(sgu,sgubrks$sgu[[r]][s],sep=",")}
  newdf <- tibble(clim=clim,sgu=sgu,mod=mod)
  sgubrksc <- rbind(sgubrksc,newdf)
}
## Now break into list by model and climate combos
modsuniq <- as.character(unique(sgubrks$mod))
sgubrkssm <- list()
for(m in 1:length(modsuniq)){
  modm <- as.character(modsuniq[m])
  climuniq <- unique(sgubrksc$clim[sgubrksc$mod==modm])
  for(c in 1:length(climuniq)){
    clim <- climuniq[c]
    climset <- sgubrksc[sgubrksc$clim==clim,]$sgu
    sgubrkssm[[modm]][[clim]] <- climset
  }
}


####### Now run sgu_opt_fn for #2 climate model
# 2st model
modelnum <- 2 # Set which of the top n models to evaluate
modbrks <- expand.grid(sgubrkssm[[modelnum]],KEEP.OUT.ATTRS = F,stringsAsFactors = F) # must set index i.e. [[n]]
modbrks$modid <- 1:nrow(modbrks)
ninst <- 2000000 # number of models to run in one parLapply
indexincr <- ceiling(nrow(modbrks)/ninst)
indcnt <- 0
setwd(loopfolder2)
for(l in 1:indexincr){
  newbeg <- indcnt + 1
  if(l == indexincr){
    indcnt <- indcnt+(nrow(modbrks)-indcnt)
    brktup <- newbeg:indcnt
  } else {
    indcnt <- indcnt+ninst
    brktup <- newbeg:indcnt
  }
  lch <- as.character(l)
  saveRDS(brktup, paste("brktup",l,".rds",sep=""))
  print(paste("Done with ", lch))
}

for(b in 1:indexincr){
  pretime <- Sys.time()
  setwd(loopfolder2)
  brks_list_for <- readRDS(paste("brktup",b,".rds",sep=""))
  cpus <- detectCores() - 2
  # cpus <- 60
  cl <- makeCluster(cpus, type="FORK")
  registerDoParallel(cl)
  npmanova_sum_m2 <- parLapply(cl,brks_list_for,sgu_opt_fn)
  #posttime <- Sys.time()
  #runtime <- posttime - pretime
  # 0.0042 per row with mixed set (31 cpus)
  stopCluster(cl)
  ## Put list into dataframe
  npmanova_sumdf_m2 <- do.call("rbind", npmanova_sum_m2)
  npmanova_sumdf_m2 <- na.omit(npmanova_sumdf_m2)
  setwd(resultfolder2)
  saveRDS(npmanova_sumdf_m2, paste("npmanova_sumdf_m2_",b,".rds",sep=""))
  rm(npmanova_sum_m2,npmanova_sumdf_m2)
  gc()
  posttime <- Sys.time()
  runtime <- posttime - pretime
  print(paste(b, "was done at", posttime,"in",runtime, sep=" "))
}
## Now loop through folder to bring together all trials
setwd(resultfolder2)
optfiles <- list.files(pattern="rds$")
trialsm2 <- readRDS(optfiles[1])[,1:6]
for(t in 2:length(optfiles)){
  newtrial <- readRDS(optfiles[t])[,1:6]
  trialsm2 <- rbind(trialsm2,newtrial)
  rm(newtrial)
}

## Now join to models
setwd(scriptfolder)
trialsjnm2 <- dplyr::left_join(trialsm2,modbrks,by="modid")
trialsjnm2$AICc <- nrow(esd_final)*log(trialsjnm2$SSres/nrow(esd_final)) + (2*(trialsjnm2$numgrps+2)) + ((2*(trialsjnm2$numgrps+2)*(trialsjnm2$numgrps+3))/(nrow(esd_final)-(trialsjnm2$numgrps+2)-1))
trialsjnm2 <- trialsjnm2[order(trialsjnm2$AICc),]
trialsjnm2$AICcWt <- akaike.weights(trialsjnm2$AICc)$weights
trialsjnm2$AICcrelLL <- akaike.weights(trialsjnm2$AICc)$rel.LL
saveRDS(trialsjnm2, "trialsjn2b_m2.rds")
trialsjnm2 <- readRDS("trialsjn2b_m2.rds")
## Subset models with RelLL >0.5 for STM testing
trialsjnm2l50 <- trialsjnm2[trialsjnm2$AICcrelLL > 0.5,]
saveRDS(trialsjnm2l50,"trialsjn2b_m2l50.rds")
trialsjnm2l50 <- readRDS("trialsjn2b_m2l50.rds")


####### Now run sgu_opt_fn for #3 climate model
loopfolder3 <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/loopind2bm3"
resultfolder3 <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/results2bm3"
# 3rd model
modelnum <- 3 # Set which of the top n models to evaluate
modbrks <- expand.grid(sgubrkssm[[modelnum]],KEEP.OUT.ATTRS = F,stringsAsFactors = F) # must set index i.e. [[n]]
modbrks$modid <- 1:nrow(modbrks)
ninst <- 2000000 # number of models to run in one parLapply
indexincr <- ceiling(nrow(modbrks)/ninst)
indcnt <- 0
setwd(loopfolder3)
for(l in 1:indexincr){
  newbeg <- indcnt + 1
  if(l == indexincr){
    indcnt <- indcnt+(nrow(modbrks)-indcnt)
    brktup <- newbeg:indcnt
  } else {
    indcnt <- indcnt+ninst
    brktup <- newbeg:indcnt
  }
  lch <- as.character(l)
  saveRDS(brktup, paste("brktup",l,".rds",sep=""))
  print(paste("Done with ", lch))
}

for(b in 1:indexincr){
  pretime <- Sys.time()
  setwd(loopfolder3)
  brks_list_for <- readRDS(paste("brktup",b,".rds",sep=""))
  cpus <- detectCores() - 2
  # cpus <- 60
  cl <- makeCluster(cpus, type="FORK")
  registerDoParallel(cl)
  npmanova_sum_m3 <- parLapply(cl,brks_list_for,sgu_opt_fn)
  #posttime <- Sys.time()
  #runtime <- posttime - pretime
  # 0.0042 per row with mixed set (31 cpus)
  stopCluster(cl)
  ## Put list into dataframe
  npmanova_sumdf_m3 <- do.call("rbind", npmanova_sum_m3)
  npmanova_sumdf_m3 <- na.omit(npmanova_sumdf_m3)
  setwd(resultfolder3)
  saveRDS(npmanova_sumdf_m3, paste("npmanova_sumdf_m3_",b,".rds",sep=""))
  rm(npmanova_sum_m3,npmanova_sumdf_m3)
  gc()
  posttime <- Sys.time()
  runtime <- posttime - pretime
  print(paste(b, "was done at", posttime,"in",runtime, sep=" "))
}
## Now loop through folder to bring together all trials
setwd(resultfolder3)
optfiles <- list.files(pattern="rds$")
trialsm3 <- readRDS(optfiles[1])[,1:6]
for(t in 2:length(optfiles)){
  newtrial <- readRDS(optfiles[t])[,1:6]
  trialsm3 <- rbind(trialsm3,newtrial)
  rm(newtrial)
}

## Now join to models
setwd(scriptfolder)
trialsjnm3 <- dplyr::left_join(trialsm3,modbrks,by="modid")
trialsjnm3$AICc <- nrow(esd_final)*log(trialsjnm3$SSres/nrow(esd_final)) + (2*(trialsjnm3$numgrps+2)) + ((2*(trialsjnm3$numgrps+2)*(trialsjnm3$numgrps+3))/(nrow(esd_final)-(trialsjnm3$numgrps+2)-1))
trialsjnm3 <- trialsjnm3[order(trialsjnm3$AICc),]
trialsjnm3$AICcWt <- akaike.weights(trialsjnm3$AICc)$weights
trialsjnm3$AICcrelLL <- akaike.weights(trialsjnm3$AICc)$rel.LL
saveRDS(trialsjnm3, "trialsjn2b_m3.rds")
trialsjnm3 <- readRDS("trialsjn2b_m3.rds")
## Subset models with RelLL >0.5 for STM testing
trialsjnm3l50 <- trialsjnm3[trialsjnm3$AICcrelLL > 0.5,]
saveRDS(trialsjnm3l50,"trialsjn2b_m3l50.rds")
trialsjnm3l50 <- readRDS("trialsjn2b_m3l50.rds")


####### Now run sgu_opt_fn for #3 climate model
loopfolder4 <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/loopind2bm4"
resultfolder4 <- "/home/tnaum/OneDrive/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/results2bm4"
# 3rd model
modelnum <- 4 # Set which of the top n models to evaluate
modbrks <- expand.grid(sgubrkssm[[modelnum]],KEEP.OUT.ATTRS = F,stringsAsFactors = F) # must set index i.e. [[n]]
modbrks$modid <- 1:nrow(modbrks)
ninst <- 2000000 # number of models to run in one parLapply
indexincr <- ceiling(nrow(modbrks)/ninst)
indcnt <- 0
setwd(loopfolder4)
for(l in 1:indexincr){
  newbeg <- indcnt + 1
  if(l == indexincr){
    indcnt <- indcnt+(nrow(modbrks)-indcnt)
    brktup <- newbeg:indcnt
  } else {
    indcnt <- indcnt+ninst
    brktup <- newbeg:indcnt
  }
  lch <- as.character(l)
  saveRDS(brktup, paste("brktup",l,".rds",sep=""))
  print(paste("Done with ", lch))
}

for(b in 1:indexincr){
  pretime <- Sys.time()
  setwd(loopfolder4)
  brks_list_for <- readRDS(paste("brktup",b,".rds",sep=""))
  cpus <- detectCores() - 2
  # cpus <- 60
  cl <- makeCluster(cpus, type="FORK")
  registerDoParallel(cl)
  npmanova_sum_m4 <- parLapply(cl,brks_list_for,sgu_opt_fn)
  #posttime <- Sys.time()
  #runtime <- posttime - pretime
  # 0.0042 per row with mixed set (31 cpus)
  stopCluster(cl)
  ## Put list into dataframe
  npmanova_sumdf_m4 <- do.call("rbind", npmanova_sum_m4)
  npmanova_sumdf_m4 <- na.omit(npmanova_sumdf_m4)
  setwd(resultfolder4)
  saveRDS(npmanova_sumdf_m4, paste("npmanova_sumdf_m4_",b,".rds",sep=""))
  rm(npmanova_sum_m4,npmanova_sumdf_m4)
  gc()
  posttime <- Sys.time()
  runtime <- posttime - pretime
  print(paste(b, "was done at", posttime,"in",runtime, sep=" "))
}
## Now loop through folder to bring together all trials
setwd(resultfolder4)
optfiles <- list.files(pattern="rds$")
trialsm4 <- readRDS(optfiles[1])[,1:6]
for(t in 2:length(optfiles)){
  newtrial <- readRDS(optfiles[t])[,1:6]
  trialsm4 <- rbind(trialsm4,newtrial)
  rm(newtrial)
}

## Now join to models
setwd(scriptfolder)
trialsjnm4 <- dplyr::left_join(trialsm4,modbrks,by="modid")
trialsjnm4$AICc <- nrow(esd_final)*log(trialsjnm4$SSres/nrow(esd_final)) + (2*(trialsjnm4$numgrps+2)) + ((2*(trialsjnm4$numgrps+2)*(trialsjnm4$numgrps+3))/(nrow(esd_final)-(trialsjnm4$numgrps+2)-1))
trialsjnm4 <- trialsjnm4[order(trialsjnm4$AICc),]
trialsjnm4$AICcWt <- akaike.weights(trialsjnm4$AICc)$weights
trialsjnm4$AICcrelLL <- akaike.weights(trialsjnm4$AICc)$rel.LL
saveRDS(trialsjnm4, "trialsjn2b_m4.rds")
trialsjnm4 <- readRDS("trialsjn2b_m4.rds")
## Subset models with RelLL >0.5 for STM testing
trialsjnm4l50 <- trialsjnm4[trialsjnm4$AICcrelLL > 0.5,]
saveRDS(trialsjnm4l50,"trialsjn2b_m4l50.rds")
trialsjnm4l50 <- readRDS("trialsjn2b_m4l50.rds")


####### Add classes from top models to the esd final matrix
## Loops through and append the top groupings from each model to the original ESD table
esd_final_v2 <- esd_final
modellist <- list(trialsjnm1l50,trialsjnm2l50,trialsjnm3l50,trialsjnm4l50)
for(m in 1:length(modellist)){
  modeltrials <- modellist[[m]]
  for(x in 1:nrow(modeltrials)){
    brks <- trialsjn_sgu[m,] # referencing climate model
    aibrks <- brks[,c("ai")]
    aibrksall <- as.numeric(vector())
    for(ain in 1:length(aibrks[[1]])){aibrksall <- c(aibrksall,as.numeric(strsplit(aibrks[[1]][ain],",")[[1]]))}
    aibrksall <- unique(aibrksall) # vector of break values for ai
    mintbrks <- brks[,c("mint")]
    mintbrksall <- as.numeric(vector())
    for(mt in 1:length(mintbrks[[1]])){mintbrksall <- c(mintbrksall,as.numeric(strsplit(mintbrks[[1]][mt],",")[[1]]))}
    mintbrksall <- unique(mintbrksall) # vector of break values for min temp
    maxtbrks <- brks[,c("maxt")]
    maxtbrksall <- as.numeric(vector())
    for(mt in 1:length(maxtbrks[[1]])){maxtbrksall <- c(maxtbrksall,as.numeric(strsplit(maxtbrks[[1]][mt],",")[[1]]))}
    maxtbrksall <- unique(maxtbrksall) # vector of break values for max temp
    pptrtbrks <- brks[,c("pptrt")]
    pptrtbrksall <- as.numeric(vector())
    for(mt in 1:length(pptrtbrks[[1]])){pptrtbrksall <- c(pptrtbrksall,as.numeric(strsplit(pptrtbrks[[1]][mt],",")[[1]]))}
    pptrtbrksall <- unique(pptrtbrksall) # vector of break values for ppt ratio
    # Break out groups in df
    esd_final_fn <- esd_final
    esd_final_fn <- dplyr::left_join(esd_final_fn, SGUs, by="sgu")
    esd_final_fn$ai_ind <- cut(esd_final_fn$aimean,aibrksall,labels=F,include.lowest=T)
    esd_final_fn$maxt_ind <- cut(esd_final_fn$maxtempmean,maxtbrksall,labels=F,include.lowest=T)
    esd_final_fn$mint_ind <- cut(esd_final_fn$mintempmean,mintbrksall,labels=F,include.lowest=T)
    esd_final_fn$pptrt_ind <- cut(esd_final_fn$pptrtmean,pptrtbrksall,labels=F,include.lowest=T)
    esd_final_fn$groups <- paste(m,esd_final_fn$ai_ind,esd_final_fn$maxt_ind,esd_final_fn$mint_ind,esd_final_fn$pptrt_ind,sep="_")
    # index climate groups
    groupind <- unique(esd_final_fn$groups)
    ## Now use group to pick the SGU breaks for each group
    #esd_final_fn$sgu_ind <- as.integer(NA)
    esd_final_fn$sgu_ind <- paste(esd_final_fn$groups,esd_final_fn$sgulev,sep="_")
    for(g in 1:length(groupind)){
      grp <- groupind[g]
      sgurules <- modeltrials[,c(grp)][x][[1]] # indexing both the climate group and the xth SGU ruleset
      for(rl in sgurules){
        rlnum <- as.numeric(strsplit(rl, ",")[[1]])
        #esd_final_fn[esd_final_fn$sgulev %in% rlnum,]$sgu_ind <- rl # Put in group obj
        esd_final_fn$sgu_ind <- ifelse(esd_final_fn$sgulev %in% rlnum & esd_final_fn$groups==grp & rl != "", paste(esd_final_fn$groups,rl,sep="_"),esd_final_fn$sgu_ind)
        rm(rlnum)
      }
    }
    esd_final_v2[,c(paste("v2",paste("m",as.character(m),sep=""),as.character(x),sep="_"))] <- esd_final_fn$sgu_ind
    print(paste("done with number ",x, sep=" "))
  }
  print(paste("done with model ", m, sep=" "))
}

## Now save esd matrix
setwd(scriptfolder)
saveRDS(esd_final_v2,"esd_final_v2b.rds")
esd_final_v2 <- readRDS("esd_final_v2b.rds")

## Now combine all top groupwise models together for an AIC analysis
setwd(scriptfolder)
trialsv1 <- readRDS("trialsjn_v1bl10.rds")
trialsv1$vermod <- "v1_m1"
trialsv1$rank <- 1:nrow(trialsv1)
trialsv1$vermodrank <- paste(trialsv1$vermod,trialsv1$rank,sep="_")
trialsjnm4l50$vermod <- "v2_m4"
trialsjnm4l50$rank <- 1:nrow(trialsjnm4l50)
trialsjnm4l50$vermodrank <- paste(trialsjnm4l50$vermod,trialsjnm4l50$rank,sep="_")
trialsjnm3l50$vermod <- "v2_m3"
trialsjnm3l50$rank <- 1:nrow(trialsjnm3l50)
trialsjnm3l50$vermodrank <- paste(trialsjnm3l50$vermod,trialsjnm3l50$rank,sep="_")
trialsjnm2l50$vermod <- "v2_m2"
trialsjnm2l50$rank <- 1:nrow(trialsjnm2l50)
trialsjnm2l50$vermodrank <- paste(trialsjnm2l50$vermod,trialsjnm2l50$rank,sep="_")
trialsjnm1l50$vermod <- "v2_m1"
trialsjnm1l50$rank <- 1:nrow(trialsjnm1l50)
trialsjnm1l50$vermodrank <- paste(trialsjnm1l50$vermod,trialsjnm1l50$rank,sep="_")
## Now put all models together with fields just relevant to AIC
modellist <- list(trialsv1,trialsjnm1l50,trialsjnm2l50,trialsjnm3l50,trialsjnm4l50)
alltopmods <- data.frame(vermodrank=as.character(""),vermod=as.character(""),numgrps = as.numeric(""), Rsq=as.numeric(""),AICc=as.numeric(""),stringsAsFactors = F)
for(m in modellist){
  newmods <- m[,c('vermodrank',"vermod","numgrps","Rsq","AICc")]
  alltopmods <- rbind(alltopmods,newmods)
}
## Order and compute AIC criteria
alltopmods <- na.omit(alltopmods) # Removes one row of NAs from original df creation
alltopmods <- alltopmods[order(alltopmods$AICc),]
alltopmods$AICcWt <- akaike.weights(alltopmods$AICc)$weights
alltopmods$AICcrelLL <- akaike.weights(alltopmods$AICc)$rel.LL
alltopmods$rank <- 1:nrow(alltopmods)
boxplot(alltopmods$rank ~ alltopmods$vermod)

