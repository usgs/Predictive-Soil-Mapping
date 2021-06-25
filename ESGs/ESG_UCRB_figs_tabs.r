#####################
# Figures and Table prep
# for ESG manuscript
#
######################
# AU: Travis Nauman, US Geological SUrvey, tnauman@usgs.gov
# 6/2021

# Workspace setup
# Install packages if not already installed
required.packages <- c("raster","rgdal", "rasterVis","maptools","RColorBrewer","ggplot2","gridExtra","classInt","RStoolbox","hexbin","dplyr","Hmisc","viridisLite","reshape2","vegan","ztable","stringr","qpcR")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
## Increase actuve memory useable by raster package
# memory.limit(100000)
rasterOptions(maxmemory = 5e+09)

## Folders
figfolder <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/ESG_UCRB_manuscript/figures"
tabfolder <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/ESG_UCRB_manuscript/tables"

### Reference data from NRCS
# esd_final <- readRDS("C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/esd_final_v2b.rds")
# summary.factor(esd_final$v2_m4_26) # Final ESGs model
# esgcol <- esd_final[,c("v2_m4_26")]
# ## Trim down df to relevant fields
# esd_final <- esd_final[,1:56]
# esd_final$sgu_prelim <- esd_final$ESG ## Cleaning up old correlation steps
# esd_final$ESG <- NULL
# ## Create climate classes for esds
# esd_final$climzone <- ifelse(esd_final$aimean<0.1442684&esd_final$maxtempmean>25.01905,"Arid_Warm",ifelse(esd_final$maxtempmean<=25.01905,"Semiarid_Cool","Semiarid_Warm"))
# esd_final$clim <- ifelse(esd_final$aimean<0.1442684&esd_final$maxtempmean>25.01905,1,ifelse(esd_final$maxtempmean<=25.01905,3,2))
# ## Lookup tables
# sgu_lookup <- read.delim("C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/soil_gmrph_mapping/smote_ssurgo/UCRB_BLM_ESGs_lookup_rf_soilgmrph_smote_30mINT.txt",stringsAsFactors = F)
# esg_lookup <- read.delim("C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/ESG_map/ESG_name_lookup_ttab.txt",stringsAsFactors = F)
# sgu_lookup$ESG <- NULL
# sgu_lookup$sgu <- sgu_lookup$SGU
# sgu_lookup$SGU <- NULL
# sgu_lookup$sguid <- sgu_lookup$value
# sgu_lookup$value <- NULL
# esd_final <- left_join(esd_final,sgu_lookup,by="sgu")
# ## Now create final ESG numeric ID for joining to lookup table
# esd_final$ESGid <- ifelse(esd_final$sguid==8,1,NA) # Outcrops
# esd_final$ESGid <- ifelse(esd_final$clim==1&esd_final$sguid==2,2,esd_final$ESGid)  #arid warm breaks
# esd_final$ESGid <- ifelse(esd_final$clim==1&esd_final$sguid==11,3,esd_final$ESGid)  #arid warm saline hills
# esd_final$ESGid <- ifelse(esd_final$clim==1&esd_final$sguid==6,4,esd_final$ESGid)  #arid warm gypsum
# esd_final$ESGid <- ifelse(esd_final$clim==1&esd_final$sguid==16,5,esd_final$ESGid)  #arid warm very shallow
# esd_final$ESGid <- ifelse(esd_final$clim==1&esd_final$sguid==12,6,esd_final$ESGid)  #arid warm Saline uplands
# esd_final$ESGid <- ifelse(esd_final$clim==1&esd_final$sguid==15,7,esd_final$ESGid)  #arid warm shallow
# esd_final$ESGid <- ifelse(esd_final$clim==1&esd_final$sguid==4,8,esd_final$ESGid)  #arid warm deep rocky
# esd_final$ESGid <- ifelse(esd_final$clim==1&(esd_final$sguid==14|esd_final$sguid==7),9,esd_final$ESGid)  #arid warm sandy uplands and Loamy Uplands
# esd_final$ESGid <- ifelse(esd_final$clim==1&(esd_final$sguid==5|esd_final$sguid==3),10,esd_final$ESGid) # arid warm finer uplands and clay uplands
# esd_final$ESGid <- ifelse(esd_final$clim==1&esd_final$sguid==13,11,esd_final$ESGid)  #arid warm sandy bottoms
# esd_final$ESGid <- ifelse(esd_final$clim==1&(esd_final$sguid==10|esd_final$sguid==1),12,esd_final$ESGid)  #arid warm saline bottoms and bottoms
# esd_final$ESGid <- ifelse(esd_final$clim==2&esd_final$sguid==2,13,esd_final$ESGid)  # semiarid warm breaks
# esd_final$ESGid <- ifelse(esd_final$clim==2&esd_final$sguid==11,14,esd_final$ESGid)  # semiarid warm saline hills
# esd_final$ESGid <- ifelse(esd_final$clim==2&esd_final$sguid==6,15,esd_final$ESGid)  # semiarid warm gypsum
# esd_final$ESGid <- ifelse(esd_final$clim==2&esd_final$sguid==16,16,esd_final$ESGid)  # semiarid warm very shallow
# esd_final$ESGid <- ifelse(esd_final$clim==2&esd_final$sguid==12,17,esd_final$ESGid)  # semiarid warm saline uplands
# esd_final$ESGid <- ifelse(esd_final$clim==2&(esd_final$sguid==15|esd_final$sguid==4),18,esd_final$ESGid)  # semiarid warm shallow and deep rocky
# esd_final$ESGid <- ifelse(esd_final$clim==2&(esd_final$sguid==14|esd_final$sguid==7),19,esd_final$ESGid)  # semiarid warm sandy uplands and loamy uplands
# esd_final$ESGid <- ifelse(esd_final$clim==2&esd_final$sguid==5,20,esd_final$ESGid)  # semiarid warm finer uplands
# esd_final$ESGid <- ifelse(esd_final$clim==2&esd_final$sguid==3,21,esd_final$ESGid)  # semiarid warm clay uplands
# esd_final$ESGid <- ifelse(esd_final$clim==2&(esd_final$sguid==13|esd_final$sguid==1),22,esd_final$ESGid)  # semiarid warm sandy bottoms and bottoms
# esd_final$ESGid <- ifelse(esd_final$clim==2&esd_final$sguid==10,23,esd_final$ESGid)  # semiarid warm saline bottoms
# esd_final$ESGid <- ifelse(esd_final$clim==3&esd_final$sguid==2,24,esd_final$ESGid)  # semiarid cool breaks ## THIS HAD NO ESDs IN ANALYSIS BUT CAME AS BLANK IN MAP
# esd_final$ESGid <- ifelse(esd_final$clim==3&esd_final$sguid==11,25,esd_final$ESGid)  # semiarid cool saline hills
# esd_final$ESGid <- ifelse(esd_final$clim==3&esd_final$sguid==6,26,esd_final$ESGid)  # semiarid cool gypsum ## This had no esds in this esd_final$climate zone as well in analysis, but had blanks in map
# esd_final$ESGid <- ifelse(esd_final$clim==3&esd_final$sguid==16,27,esd_final$ESGid)  # semiarid cool very shallow: ## This had no esds in this esd_final$climate zone as well in analysis, but had blanks in map
# esd_final$ESGid <- ifelse(esd_final$clim==3&(esd_final$sguid==12|esd_final$sguid==14|esd_final$sguid==7|esd_final$sguid==5),28,esd_final$ESGid)  # semiarid cool saline uplands, sandy uplands, loamy uplands, and finer uplands
# esd_final$ESGid <- ifelse(esd_final$clim==3&esd_final$sguid==15,29,esd_final$ESGid)  # semiarid cool shallow
# esd_final$ESGid <- ifelse(esd_final$clim==3&esd_final$sguid==4,30,esd_final$ESGid)  # semiarid cool deep rocky
# esd_final$ESGid <- ifelse(esd_final$clim==3&esd_final$sguid==3,31,esd_final$ESGid)  # semiarid cool clay uplands
# esd_final$ESGid <- ifelse(esd_final$clim==3&esd_final$sguid==13,32,esd_final$ESGid)  # semiarid cool sandy bottoms ## This had no esds in this esd_final$climate zone as well in analysis, but had blanks in map
# esd_final$ESGid <- ifelse(esd_final$clim==3&esd_final$sguid==10,33,esd_final$ESGid)  # semiarid cool saline bottoms
# esd_final$ESGid <- ifelse(esd_final$clim==3&esd_final$sguid==1,34,esd_final$ESGid)  # semiarid cool bottoms
# esd_final$ESGid <- ifelse(esd_final$sguid==9,35,esd_final$ESGid) # Riparian
# ## Now join to ESG name table
# esg_lookup$ESGid <- esg_lookup$Value
# esg_lookup <- esg_lookup[,c("ESG","ESGid")]
# esd_final <- left_join(esd_final,esg_lookup,by="ESGid")
# # STM data
# stmtab <- read.delim("C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/STM/STM_matrix_expanded_SSB_for_R_ttab_twn.txt",stringsAsFactors = F)
# stmtab$ecoclassid <- stmtab$esdid
# stmtab$esdid <- NULL
# esd_stm <- dplyr::inner_join(esd_final,stmtab,by="ecoclassid")
# ## Summary by new ESGs
# stm_sum <- esd_stm %>% dplyr::group_by(ESG) %>%
#   dplyr::summarize(r.dune = mean(r.dune),
#                    r.grass = mean(r.grass),
#                    r.wl = mean(r.wl),
#                    r.wet = mean(r.wet),
#                    r.shrub = mean(r.shrub),
#                    i.bromus = mean(i.bromus),
#                    i.salsola = mean(i.salsola),
#                    i.halo = mean(i.halo),
#                    i.wheat = mean(i.wheat),
#                    i.blue = mean(i.blue),
#                    i.oth = mean(i.oth),
#                    inon.bun = mean(inon.bun),
#                    inon.oth = mean(inon.oth),
#                    e.sna = mean(e.sna),
#                    e.rab = mean(e.rab),
#                    e.shad = mean(e.shad),
#                    e.grea = mean(e.grea),
#                    e.tam = mean(e.tam),
#                    e.rus = mean(e.rus),
#                    e.opt = mean(e.opt),
#                    e.pin = mean(e.pin),
#                    e.jun = mean(e.jun),
#                    e.sage = mean(e.sage),
#                    e.shrub = mean(e.shrub),
#                    e.tree = mean(e.tree),
#                    e.oth = mean(e.oth),
#                    er.dune = mean(er.dune),
#                    er.rill = mean(er.rill),
#                    er.oth = mean(er.oth),
#                    b.dist = mean(b.dist),
#                    b.AnnBare = mean(b.AnnBare),
#                    b.bsc = mean(b.bsc),
#                    b.oth = mean(b.oth),
#                    pl.grass = mean(pl.grass),
#                    pl.c4grass = mean(pl.c4grass),
#                    pl.c3grass = mean(pl.c3gras),
#                    pl.sage = mean(pl.sage),
#                    pl.grswd = mean(pl.grswd),
#                    pl.other = mean(pl.other))%>%
#   dplyr::ungroup()
# stm_sum$n <- esd_stm %>% dplyr::group_by(ESG) %>% tally() %>%
#   dplyr::ungroup()
# stm_sum$n$ESG <- NULL
# stm_sum$ESDn <- stm_sum$n$n
# stm_sum$n <- NULL
# stm_sum_df <- as.data.frame(stm_sum)
# ## Now for SGUs
# stm_sum_sgu <- esd_stm %>% dplyr::group_by(sgu) %>%
#   dplyr::summarize(r.dune = mean(r.dune),
#                    r.grass = mean(r.grass),
#                    r.wl = mean(r.wl),
#                    r.wet = mean(r.wet),
#                    r.shrub = mean(r.shrub),
#                    i.bromus = mean(i.bromus),
#                    i.salsola = mean(i.salsola),
#                    i.halo = mean(i.halo),
#                    i.wheat = mean(i.wheat),
#                    i.blue = mean(i.blue),
#                    i.oth = mean(i.oth),
#                    inon.bun = mean(inon.bun),
#                    inon.oth = mean(inon.oth),
#                    e.sna = mean(e.sna),
#                    e.rab = mean(e.rab),
#                    e.shad = mean(e.shad),
#                    e.grea = mean(e.grea),
#                    e.tam = mean(e.tam),
#                    e.rus = mean(e.rus),
#                    e.opt = mean(e.opt),
#                    e.pin = mean(e.pin),
#                    e.jun = mean(e.jun),
#                    e.sage = mean(e.sage),
#                    e.shrub = mean(e.shrub),
#                    e.tree = mean(e.tree),
#                    e.oth = mean(e.oth),
#                    er.dune = mean(er.dune),
#                    er.rill = mean(er.rill),
#                    er.oth = mean(er.oth),
#                    b.dist = mean(b.dist),
#                    b.AnnBare = mean(b.AnnBare),
#                    b.bsc = mean(b.bsc),
#                    b.oth = mean(b.oth),
#                    pl.grass = mean(pl.grass),
#                    pl.c4grass = mean(pl.c4grass),
#                    pl.c3grass = mean(pl.c3gras),
#                    pl.sage = mean(pl.sage),
#                    pl.grswd = mean(pl.grswd),
#                    pl.other = mean(pl.other))%>%
#   dplyr::ungroup()
# stm_sum_sgu$n <- esd_stm %>% dplyr::group_by(sgu) %>% tally() %>%
#   dplyr::ungroup()
# stm_sum_sgu$n$sgu <- NULL
# stm_sum_sgu$ESDn <- stm_sum_sgu$n$n
# stm_sum_sgu$n <- NULL
# stm_sum_sgu_df <- as.data.frame(stm_sum_sgu)
# ## Save STM tables
# setwd(tabfolder)
# write.table(stm_sum_df,"ESG_STM_summary.txt",sep = "\t", row.names = FALSE)
# write.table(stm_sum_sgu_df,"SGU_STM_summary.txt",sep = "\t", row.names = FALSE)
# 
# 
# ##################### PerManovas on reference production
# esd_final$PerGr <- esd_final$c3PerGr + esd_final$c4PerGr
# # Four Shallow ESDs did not have soils data except for depth (all are shallow), one site (R034BY117UT) with high mat saltbush may be in 'wrong' SGU (Shallow instead of Saline HIlls or uplands)
# # The R034BY221UT, R034BY131UT, R035XE510AZ sites check out in the original ESDs.
# esd_final_soil <- subset(esd_final, !is.na(esd_final$rock_surf))
# daty_soil <- esd_final_soil[,c('Tree','Shrub','Grass','Forb','TOTAL_PROD','bigsage','mtnsage','basinsage','cora',"quga",'pinion','juniper','aspen',
#                      'ponderosa',"greasewd","saltbush","shadscale","spai",'PerGr')]
# ## All variables
# esd_final_soil$runon <- ifelse(esd_final_soil$sgu=="Bottoms"|esd_final_soil$sgu=="SalineBottoms"|esd_final_soil$sgu=="SandyBottoms",1,0)
# datx_soil <- data.frame(runon=as.factor(esd_final_soil$runon),sar=esd_final_soil$sar_surf,gypsum=esd_final_soil$gypsum_surf,ec=esd_final_soil$ec_surf,depth=esd_final_soil$depthmean,
#                    slope=esd_final_soil$slope, rock=esd_final_soil$rock_surf,clay=esd_final_soil$clay_surf,sand=esd_final_soil$sand_surf,
#                    ai=esd_final_soil$aimean,maxt=esd_final_soil$maxtempmean,mint=esd_final_soil$mintempmean,
#                    pptrt=esd_final_soil$pptrtmean,stringsAsFactors = F)
# npmanmod_all <- adonis2(daty_soil ~ runon+sar+gypsum+ec+depth+slope+rock+clay+sand+ai+maxt+mint+pptrt, data=datx_soil,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_all <- npmanmod_all[,1:5]
# PerMan_tab_all$var <- rownames(PerMan_tab_all)
# write.table(PerMan_tab_all, "PerMan_ref_all.txt",sep="\t",row.names=F)
# ## Just soil and geomorph variables
# npmanmod_soil_geom <- adonis2(daty_soil ~ runon+sar+gypsum+ec+depth+slope+rock+clay+sand, data=datx_soil,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_soil_geom <- npmanmod_soil_geom[,1:5]
# PerMan_tab_soil_geom$var <- rownames(PerMan_tab_soil_geom)
# write.table(PerMan_tab_soil_geom, "PerMan_ref_soil_geom.txt",sep="\t",row.names=F)
# 
# ## SGU and climate variables
# daty <- esd_final[,c('Tree','Shrub','Grass','Forb','TOTAL_PROD','bigsage','mtnsage','basinsage','cora',"quga",'pinion','juniper','aspen',
#                      'ponderosa',"greasewd","saltbush","shadscale","spai",'PerGr')]
# datx <- data.frame(esg=esd_final$ESG,sgu=as.factor(esd_final$sgu),ai=esd_final$aimean,maxt=esd_final$maxtempmean,mint=esd_final$mintempmean,pptrt=esd_final$pptrtmean)
# ## Testing just SGUs
# npmanmod_sgu <- adonis2(daty ~ sgu, data=datx,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_sgu <- npmanmod_sgu[,1:5]
# PerMan_tab_sgu$var <- rownames(PerMan_tab_sgu)
# write.table(PerMan_tab_sgu, "PerMan_ref_sgu.txt",sep="\t",row.names=F)
# ## Testing just climate variables
# npmanmod_clim <- adonis2(daty ~ ai+maxt+mint+pptrt, data=datx,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_clim <- npmanmod_clim[,1:5]
# PerMan_tab_clim$var <- rownames(PerMan_tab_clim)
# write.table(PerMan_tab_clim, "PerMan_ref_clim.txt",sep="\t",row.names=F)
# ## Testing SGU and climate variables
# npmanmod_sgu_clim <- adonis2(daty ~ sgu+ai+maxt+mint+pptrt, data=datx,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_sgu_clim <- npmanmod_sgu_clim[,1:5]
# PerMan_tab_sgu_clim$var <- rownames(PerMan_tab_sgu_clim)
# write.table(PerMan_tab_sgu_clim, "PerMan_ref_sgu_clim.txt",sep="\t",row.names=F)
# ## Testing 2x interactions of SGU with climate variables
# npmanmod_2x_sgu_clim_ints <- adonis2(daty ~ sgu+sgu:ai+sgu:maxt+sgu:mint+sgu:pptrt, data=datx,method="bray",parallel = 15,permutations = 999, by = 'margin') # two way SGU interactions
# PerMan_tab_2x_sgu_clim <- npmanmod_2x_sgu_clim_ints[,1:5]
# PerMan_tab_2x_sgu_clim$var <- rownames(PerMan_tab_2x_sgu_clim)
# write.table(PerMan_tab_2x_sgu_clim, "PerMan_ref_2x_sgu_clim.txt",sep="\t",row.names=F)
# ## Permanova Test final ESG groups
# npmanmod_esg <- adonis2(daty ~ esg, data=datx,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_esg <- npmanmod_esg[,1:5]
# PerMan_tab_esg$var <- rownames(PerMan_tab_esg)
# write.table(PerMan_tab_esg, "PerMan_ref_esg.txt",sep="\t",row.names=F)
# 
# ############ Permanovas on STM data
# esd_stm$runon <- ifelse(esd_stm$sgu=="Bottoms"|esd_stm$sgu=="SalineBottoms"|esd_stm$sgu=="SandyBottoms",1,0)
# esd_stm_soil <- esd_stm[!is.na(esd_stm$ec_surf),]
# daty_stm_soil <- esd_stm_soil[,c(colnames(stmtab))]
# daty_stm_soil$ecoclassid <- NULL
# datx_stm_soil <- data.frame(runon=as.factor(esd_stm_soil$runon),sar=esd_stm_soil$sar_surf,gypsum=esd_stm_soil$gypsum_surf,ec=esd_stm_soil$ec_surf,depth=esd_stm_soil$depthmean,
#                        slope=esd_stm_soil$slope, rock=esd_stm_soil$rock_surf,clay=esd_stm_soil$clay_surf,sand=esd_stm_soil$sand_surf,
#                        esg=esd_stm_soil$ESG,sgu=as.factor(esd_stm_soil$sgu),ai=esd_stm_soil$aimean,maxt=esd_stm_soil$maxtempmean,mint=esd_stm_soil$mintempmean,
#                        pptrt=esd_stm_soil$pptrtmean,stringsAsFactors = F)
# ## All variables
# npmanmod_stm_all <- adonis2(daty_stm_soil ~ runon+sar+gypsum+ec+depth+slope+rock+clay+sand+ai+maxt+mint+pptrt, data=datx_stm_soil,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_stm_all <- npmanmod_stm_all[,1:5]
# PerMan_tab_stm_all$var <- rownames(PerMan_tab_stm_all)
# write.table(PerMan_tab_stm_all, "PerMan_stm_all.txt",sep="\t",row.names=F)
# ## Soils and Geomorph variables
# npmanmod_stm_soil_geom <- adonis2(daty_stm_soil ~ runon+sar+gypsum+ec+depth+slope+rock+clay+sand, data=datx_stm_soil,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_stm_soil_geom <- npmanmod_stm_soil_geom[,1:5]
# PerMan_tab_stm_soil_geom$var <- rownames(PerMan_tab_stm_soil_geom)
# write.table(PerMan_tab_stm_soil_geom, "PerMan_stm_soil_geom.txt",sep="\t",row.names=F)
# ## SGU, ESG and climate
# daty_stm <- esd_stm[,c(colnames(stmtab))]
# daty_stm$ecoclassid <- NULL
# datx_stm <- data.frame(esg=esd_stm$ESG,sgu=as.factor(esd_stm$sgu),ai=esd_stm$aimean,maxt=esd_stm$maxtempmean,mint=esd_stm$mintempmean,
#                        pptrt=esd_stm$pptrtmean,stringsAsFactors = F)
# ## Testing just SGUs
# npmanmod_stm_sgu <- adonis2(daty_stm ~ sgu, data=datx_stm,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_stm_sgu <- npmanmod_stm_sgu[,1:5]
# PerMan_tab_stm_sgu$var <- rownames(PerMan_tab_stm_sgu)
# write.table(PerMan_tab_stm_sgu, "PerMan_stm_sgu.txt",sep="\t",row.names=F)
# ## Testing just climate variables
# npmanmod_stm_clim <- adonis2(daty_stm ~ ai+maxt+mint+pptrt, data=datx_stm,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_stm_clim <- npmanmod_stm_clim[,1:5]
# PerMan_tab_stm_clim$var <- rownames(PerMan_tab_stm_clim)
# write.table(PerMan_tab_stm_clim, "PerMan_stm_clim.txt",sep="\t",row.names=F)
# ## Testing SGU and climate variables
# npmanmod_stm_sgu_clim <- adonis2(daty_stm ~ sgu+ai+maxt+mint+pptrt, data=datx_stm,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_stm_sgu_clim <- npmanmod_stm_sgu_clim[,1:5]
# PerMan_tab_stm_sgu_clim$var <- rownames(PerMan_tab_stm_sgu_clim)
# write.table(PerMan_tab_stm_sgu_clim, "PerMan_stm_sgu_clim.txt",sep="\t",row.names=F)
# ## Testing 2x interactions of SGU with climate variables
# npmanmod_stm_2x_sgu_clim_ints <- adonis2(daty_stm ~ sgu+sgu:ai+sgu:maxt+sgu:mint+sgu:pptrt, data=datx_stm,method="bray",parallel = 15,permutations = 999, by = 'margin') # two way SGU interactions
# PerMan_tab_stm_2x_sgu_clim <- npmanmod_stm_2x_sgu_clim_ints[,1:5]
# PerMan_tab_stm_2x_sgu_clim$var <- rownames(PerMan_tab_stm_2x_sgu_clim)
# write.table(PerMan_tab_stm_2x_sgu_clim, "PerMan_stm_2x_sgu_clim.txt",sep="\t",row.names=F)
# ## Permanova Test final ESG groups
# npmanmod_stm_esg <- adonis2(daty_stm ~ esg, data=datx_stm,method="bray",permutations = 999, by = 'margin',parallel = 15)
# PerMan_tab_stm_esg <- npmanmod_stm_esg[,1:5]
# PerMan_tab_stm_esg$var <- rownames(PerMan_tab_stm_esg)
# write.table(PerMan_tab_stm_esg, "PerMan_stm_esg.txt",sep="\t",row.names=F)

##### Save processed data for other figure scripting
setwd(figfolder)
# saveRDS(esd_final, "esd_final.rds")
# saveRDS(esd_stm, "esd_stm.rds")
esd_final <- readRDS("esd_final.rds")
esd_stm <- readRDS("esd_stm.rds")

### Graphing % variation explained for all models
setwd(tabfolder)
Perman_tabs <- list.files(pattern="PerMan") # Tables for production and refs
model_var_tab <- data.frame(modtype=as.character(),modname=as.character(),ExplVar=as.numeric(),stringsAsFactors=F)
for(t in Perman_tabs){
  modeltype <- ifelse(grepl("ref",t),"reference","STM")
  modelname <- gsub("PerMan_","",t)
  modelname <- gsub("ref_","",modelname)
  modelname <- gsub("stm_","",modelname)
  modelname <- gsub(".txt","",modelname)
  vartab <- read.delim(t,stringsAsFactors = F)
  varexplained <- 1 - vartab[vartab$var=="Residual",]$R2
  newrow <- data.frame(modtype=as.character(modeltype),modname=as.character(modelname),ExplVar=as.numeric(varexplained),stringsAsFactors=F)
  model_var_tab <- rbind(model_var_tab,newrow)
}
## Now incorporate plot-based models
setwd("C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/plot_analysis")
Perman_plot_tabs <- list.files(pattern="npmanmod")
Perman_plot_tabsno <- Perman_plot_tabs[grep("fnpmanmod",Perman_plot_tabs)]
Perman_plot_tabs <- subset(Perman_plot_tabs, !Perman_plot_tabs %in% Perman_plot_tabsno)
for(t in Perman_plot_tabs){
  modeltype <- "cover"
  modelname <- gsub("npmanmod_","",t)
  modelname <- gsub(".rds","",modelname)
  vartab <- readRDS(t)
  vartab$var <- rownames(vartab)
  vartab <- vartab[,1:6]
  varexplained <- 1 - vartab[vartab$var=="Residual",]$R2
  newrow <- data.frame(modtype=as.character(modeltype),modname=as.character(modelname),ExplVar=as.numeric(varexplained),stringsAsFactors=F)
  model_var_tab <- rbind(model_var_tab,newrow)
}
## Prep sgu_clim models
sgu_clim_cov <- readRDS("npmanmod_sgu_clim.rds")
sgu_clim_cov$var <- rownames(sgu_clim_cov)
setwd(tabfolder)
sgu_clim_prod <- read.delim("PerMan_ref_sgu_clim.txt",stringsAsFactors = F)
sgu_clim_stm <- read.delim("PerMan_stm_sgu_clim.txt",stringsAsFactors = F)

## Render Rsq plots
setwd(figfolder)
refmods <- model_var_tab[model_var_tab$modtype=="reference",]
refmods <- refmods[order(refmods$ExplVar),]
refmods <- subset(refmods, refmods$modname != "soil_geom" & refmods$modname != "all")
stmmods <- model_var_tab[model_var_tab$modtype=="STM",]
stmmods <- stmmods[order(stmmods$ExplVar),]
stmmods <- subset(stmmods, stmmods$modname!="soil_geom" & stmmods$modname!="all")
plotmods <- model_var_tab[model_var_tab$modtype=="cover",]
plotmods <- plotmods[order(plotmods$ExplVar),]
plotmods <- subset(plotmods, plotmods$modname!="ai")
dev.off() # Clear plot
tiff("NpManova_nrcs_Rsq_models.tif",width = 6.5, height = 5.5, units = 'in', res = 600 )
par(mfrow=c(3,2),mar = c(2,6,2,0.5),mgp=c(2,1,0))
barplot(refmods$ExplVar, names.arg=refmods$modname, horiz=T,las=1,col = "darkblue", xlim=c(0,0.6),main="Reference Production NPMANOVA Models", cex.main=1)
par(mar = c(2,3.5,2,1))
barplot(sgu_clim_prod$R2[1:5], names.arg=sgu_clim_prod$var[1:5], horiz=T,las=1,col = "darkblue", xlim=c(0,0.25),main="Reference SGU_Clim NPMANOVA", cex.main=1)
par(mar = c(2,6,2,0.5))
barplot(stmmods$ExplVar, names.arg=stmmods$modname, horiz=T,las=1, xlim=c(0,0.6),col="grey",main="STM-based NPMANOVA Models", cex.main=1)
par(mar = c(2,3.5,2,1))
barplot(sgu_clim_stm$R2[1:5], names.arg=sgu_clim_stm$var[1:5], horiz=T,las=1,col = "grey", xlim=c(0,0.25),main="STM-based SGU_Clim NPMANOVA", cex.main=1)
par(mar = c(4,6,2,0.5))
barplot(plotmods$ExplVar, names.arg=plotmods$modname, horiz=T,las=1, xlim=c(0,0.6),col="yellow",main="Cover-based NPMANOVA Models",xlab="Variation Explained", cex.main=1)
par(mar = c(4,3.5,2,1))
barplot(sgu_clim_cov$R2[1:5], names.arg=sgu_clim_cov$var[1:5], horiz=T,las=1,col = "yellow", xlim=c(0,0.25),main="Cover-based SGU_Clim NPMANOVA", xlab="Marginal Variation Explained", cex.main=1)
dev.off()

########## Graph some key functional groups and/or species to show similarity between NRCS ref production and plot based cover data
coverplts <- readRDS("C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/plot_analysis/stats_plots.rds")
coverplt_df <- melt(coverplts[,c("Forb","Grass","Shrub","Tree","SGU")],id.vars = "SGU")
refplt_df <- melt(esd_final[,c("Forb","Grass","Shrub","Tree","sgu")],id.vars = "sgu")
esd_final$TOTAL_PROD_SI <- esd_final$TOTAL_PROD * 0.892179 # Convert to kg/ha (SI units)
dev.off() # Clear plot
tiff("SGU_TotalProd.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(TOTAL_PROD_SI~sgu, data=esd_final,las=1, horizontal=T,ylab="",xlab="Reference Production (kg/ha)")
dev.off() # Clear plot
tiff("SGU_TotalCov.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(TotCover~SGU, data=coverplts,las=1, horizontal=T,ylab="",xlab="% Total Foliar Cover")#yaxt="n",
dev.off() # Clear plot

## Now graph total production by MLRA
library(stringr)
# Pick out MLRA from ESD code
esd_final$mlra <- str_sub(esd_final$ecoclassid,start=3, end=5)
dev.off() # Clear plot
tiff("SGU_TotalProd_MLRA.tif",width = 7, height = 6.5, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0),mfrow=c(2,2))
boxplot(TOTAL_PROD_SI~sgu, data=esd_final[esd_final$mlra=="35X",],las=1, horizontal=T,ylab="",xlab="35X Reference Production (kg/ha)", ylim=c(0,2700))
boxplot(TOTAL_PROD_SI~sgu, data=esd_final[esd_final$mlra=="34B",],las=1, horizontal=T,ylab="",xlab="34B Reference Production (kg/ha)", ylim=c(0,2700))
boxplot(TOTAL_PROD_SI~sgu, data=esd_final[esd_final$mlra=="34A",],las=1, horizontal=T,ylab="",xlab="34A Reference Production (kg/ha)", ylim=c(0,2700))
boxplot(TOTAL_PROD_SI~sgu, data=esd_final[esd_final$mlra=="36X",],las=1, horizontal=T,ylab="",xlab="36X Reference Production (kg/ha)", ylim=c(0,2700))
dev.off() # Clear plot


## Grass Production and Cover
esd_final$Grass_SI <- esd_final$Grass * 0.892179 # Convert to kg/ha (SI units)
dev.off() # Clear plot
tiff("SGU_Grass_Prod.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(Grass_SI~sgu, data=esd_final,las=1, horizontal=T,ylab="",xlab="Grass Production (kg/ha)")
dev.off() # Clear plot
tiff("SGU_Grass_Cover.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(Grass~SGU, data=coverplts,las=1, horizontal=T,ylab="",xlab="% Grass Cover (Any Hit)")#yaxt="n",
dev.off() # Clear plot

## Tree Production and Cover
esd_final$Tree_SI <- esd_final$Tree * 0.892179 # Convert to kg/ha (SI units)
dev.off() # Clear plot
tiff("SGU_Tree_Prod.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(Tree_SI~sgu, data=esd_final,las=1, horizontal=T,ylab="",xlab="Tree Production (kg/ha)")
dev.off() # Clear plot
tiff("SGU_Tree_Cover.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(Tree~SGU, data=coverplts,las=1, horizontal=T,ylab="",xlab="% Tree Cover (Any Hit)")#yaxt="n",
dev.off() # Clear plot

## Shrub Production and Cover
esd_final$Shrub_SI <- esd_final$Shrub * 0.892179 # Convert to kg/ha (SI units)
dev.off() # Clear plot
tiff("SGU_Shrub_Prod.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(Shrub_SI~sgu, data=esd_final,las=1, horizontal=T,ylab="",xlab="Shrub Production (kg/ha)")
dev.off() # Clear plot
tiff("SGU_Shrub_Cover.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(Shrub~SGU, data=coverplts,las=1, horizontal=T,ylab="",xlab="% Shrub Cover (Any Hit)")#yaxt="n",
dev.off() # Clear plot

## Big Sage Production and Cover
esd_final$bigsage_SI <- esd_final$bigsage * 0.892179 # Convert to kg/ha (SI units)
dev.off() # Clear plot
tiff("SGU_BigSage_Prod.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(bigsage_SI~sgu, data=esd_final,las=1, horizontal=T,ylab="",xlab="Big Sage Production (kg/ha)")
dev.off() # Clear plot
tiff("SGU_BigSage_Cover.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(bigsage~SGU, data=coverplts,las=1, horizontal=T,ylab="",xlab="% Big Sage Cover (Any Hit)")#yaxt="n",
dev.off() # Clear plot

## Basin Sage Production and Cover
esd_final$basinsage_SI <- esd_final$basinsage * 0.892179 # Convert to kg/ha (SI units)
dev.off() # Clear plot
tiff("SGU_BasinSage_Prod.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(basinsage_SI~sgu, data=esd_final,las=1, horizontal=T,ylab="",xlab="Basin Sage Production (kg/ha)")
dev.off() # Clear plot
tiff("SGU_BasinSage_Cover.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(basinsage~SGU, data=coverplts,las=1, horizontal=T,ylab="",xlab="% Basin Sage Cover (Any Hit)")#yaxt="n",
dev.off() # Clear plot

## Mountain Sage Production and Cover
esd_final$mtnsage_SI <- esd_final$mtnsage * 0.892179 # Convert to kg/ha (SI units)
dev.off() # Clear plot
tiff("SGU_MtnSage_Prod.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(mtnsage_SI~sgu, data=esd_final,las=1, horizontal=T,ylab="",xlab="Mtn Sage Production (kg/ha)")
dev.off() # Clear plot
tiff("SGU_MtnSage_Cover.tif",width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,7,1,0.5),mgp=c(2,1,0))
boxplot(mtnsage~SGU, data=coverplts,las=1, horizontal=T,ylab="",xlab="% Mtn Sage Cover (Any Hit)")#yaxt="n",
dev.off() # Clear plot


########### To look at full cover set with Outcrops and Riparian
fcoverplts <- readRDS("C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/plot_analysis/final_plots.rds")

## Summarize cover by ESG
esg_lpi_summary <- fcoverplts %>%
  dplyr::group_by(ESG) %>%
  dplyr::summarise(bigsage = mean(bigsage),
                   basinsage = mean(basinsage),
                   mtnsage = mean(mtnsage),
                   cora = mean(cora),
                   quga = mean(quga),
                   pinion = mean(pinion),
                   juniper = mean(juniper),
                   aspen = mean(aspen),
                   ponderosa = mean(ponderosa),
                   greasewd = mean(greasewd),
                   saltbush = mean(saltbush),
                   shadscale = mean(shadscale),
                   spai = mean(spai),
                   c3PerGr = mean(c3PerGr),
                   c4PerGr = mean(c4PerGr),
                   Forb = mean(Forb),
                   Grass = mean(Grass),
                   Shrub = mean(Shrub),
                   Tree = mean(Tree),
                   TotCover = mean(TotCover)) %>%
  dplyr::ungroup()
esg_lpi_summary$n <- fcoverplts %>% dplyr::group_by(ESG) %>% tally() %>%
  dplyr::ungroup()
esg_lpi_summary$n$ESG <- NULL
esg_lpi_summary$n_plots <- esg_lpi_summary$n$n
esg_lpi_summary$n <- NULL
esg_lpi_summary_df <- as.data.frame(esg_lpi_summary)

## Save table for ESG summary in paper
write.csv(esg_lpi_summary_df,paste0(tabfolder,"/ESG_LPI_summary.csv"),row.names = F)

### Summarize cover by SGU
sgu_lpi_summary <- fcoverplts %>%
  dplyr::group_by(SGU) %>%
  dplyr::summarise(bigsage = mean(bigsage),
                   basinsage = mean(basinsage),
                   mtnsage = mean(mtnsage),
                   cora = mean(cora),
                   quga = mean(quga),
                   pinion = mean(pinion),
                   juniper = mean(juniper),
                   aspen = mean(aspen),
                   ponderosa = mean(ponderosa),
                   greasewd = mean(greasewd),
                   saltbush = mean(saltbush),
                   shadscale = mean(shadscale),
                   spai = mean(spai),
                   c3PerGr = mean(c3PerGr),
                   c4PerGr = mean(c4PerGr),
                   Forb = mean(Forb),
                   Grass = mean(Grass),
                   Shrub = mean(Shrub),
                   Tree = mean(Tree),
                   TotCover = mean(TotCover)) %>%
  dplyr::ungroup()
sgu_lpi_summary$n <- fcoverplts %>% dplyr::group_by(SGU) %>% tally() %>%
  dplyr::ungroup()
sgu_lpi_summary$n$SGU <- NULL
sgu_lpi_summary$n_plots <- sgu_lpi_summary$n$n
sgu_lpi_summary$n <- NULL
sgu_lpi_summary_df <- as.data.frame(sgu_lpi_summary)

## Save table for ESG summary in paper
write.csv(sgu_lpi_summary_df,paste0(tabfolder,"/SGU_LPI_summary.csv"),row.names = F)



par(mar = c(4,15,1,0.5))
boxplot(TotCover~SGU, data=fcoverplts,las=1, horizontal=T,ylab="",xlab="% Total Foliar Cover")
boxplot(TotCover~ESG, data=fcoverplts,las=1, horizontal=T,ylab="",xlab="% Total Foliar Cover")
boxplot(ai~SGU, data=fcoverplts,las=1, horizontal=T,ylab="",xlab="Aridity Index")
boxplot(bigsage~SGU, data=fcoverplts,las=1, horizontal=T,ylab="",xlab=)
boxplot(bigsage~ESG, data=fcoverplts,las=1, horizontal=T,ylab="",xlab=)
boxplot(basinsage~SGU, data=fcoverplts,las=1, horizontal=T,ylab="",xlab=)
boxplot(basinsage~ESG, data=fcoverplts,las=1, horizontal=T,ylab="",xlab=)
boxplot(mtnsage~SGU, data=fcoverplts,las=1, horizontal=T,ylab="",xlab=)
boxplot(mtnsage~ESG, data=fcoverplts,las=1, horizontal=T,ylab="",xlab=)
boxplot(Grass~ESG, data=fcoverplts,las=1, horizontal=T,ylab="",xlab=)
boxplot(Grass~SGU, data=fcoverplts,las=1, horizontal=T,ylab="",xlab=)

##### Graphs by structural groups (probably too much for in text?)
# reference production
ref_bxplt <- ggplot(refplt_df, aes(value, sgu, fill=factor(variable))) +
  geom_boxplot()+ theme(legend.position = c(0.8, 0.2))+
  theme(axis.title.y=element_blank())+ guides(fill=guide_legend(title="Struct. Group"))+xlab("Reference Production (lbs/Ac)")
  #axis.text.y=element_blank(),
# Plot cover
cov_bxplt <- ggplot(coverplt_df, aes(value, SGU, fill=factor(variable))) +
  geom_boxplot()+ theme(legend.position = "none")+
  theme(axis.title.y=element_blank())+xlab("% Total Any-Hit Cover")
veg.plots <- grid.arrange(cov_bxplt,ref_bxplt,ncol=2)
ggsave('Supp_StructGroup_Prod_Cov.tif', plot = veg.plots, device = "tiff", dpi = 600, limitsize = TRUE, width = 6.5, height = 6, units = 'in',compression = c("lzw"))



######################### Climate optimization
climfolder <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize"
## Read in trials tables
setwd(climfolder)
# Climate breaks for v2
trials <- readRDS("trialsjn_v2.rds")
# v1: both climate and SGU simultaneously (no nesting of SGU aggregations)
trialsjn1 <- readRDS("trialsjn_v1b.rds")
# v2 model one: first climate break set tested
trialsjnm1 <- readRDS("trialsjn2b_m1.rds")
# v2 model two
trialsjnm2 <- readRDS("trialsjn2b_m2.rds")
# v2 model three
trialsjnm3 <- readRDS("trialsjn2b_m3.rds")
# v2 model four
trialsjnm4 <- readRDS("trialsjn2b_m4.rds")
## STM trials
npmanova_sumdf <- readRDS("esd_stm_opt_df_b.rds")

## Selected models from each trial
esd_final_v1 <- readRDS("esd_final_v1b.rds") # Top models from v1: down to >10% relLL
esd_final_v2 <- readRDS("esd_final_v2b.rds") # All models from v2 with > 0.5 relLL
esd_final_copt <- cbind(esd_final_v1,esd_final_v2[,57:length(esd_final_v2)]) ## Add v2 groups to those of v1
# Selected models for comparison between frameworks
## Combine all models
trialsjnm1l50 <- readRDS("trialsjn2b_m1l50.rds")
trialsjnm2l50 <- readRDS("trialsjn2b_m2l50.rds")
trialsjnm3l50 <- readRDS("trialsjn2b_m3l50.rds")
trialsjnm4l50 <- readRDS("trialsjn2b_m4l50.rds")
trialsv1 <- readRDS("trialsjn_v1bl10.rds")
trialsv1$vermod <- "simultaneous"
trialsv1$rank <- 1:nrow(trialsv1)
trialsv1$vermodrank <- paste(trialsv1$vermod,trialsv1$rank,sep="_")
trialsjnm4l50$vermod <- "nested_m4"
trialsjnm4l50$rank <- 1:nrow(trialsjnm4l50)
trialsjnm4l50$vermodrank <- paste(trialsjnm4l50$vermod,trialsjnm4l50$rank,sep="_")
trialsjnm3l50$vermod <- "nested_m3"
trialsjnm3l50$rank <- 1:nrow(trialsjnm3l50)
trialsjnm3l50$vermodrank <- paste(trialsjnm3l50$vermod,trialsjnm3l50$rank,sep="_")
trialsjnm2l50$vermod <- "nested_m2"
trialsjnm2l50$rank <- 1:nrow(trialsjnm2l50)
trialsjnm2l50$vermodrank <- paste(trialsjnm2l50$vermod,trialsjnm2l50$rank,sep="_")
trialsjnm1l50$vermod <- "nested_m1"
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

## Look at STM trials by version and model
npmanova_sumdf$rank <-  1:nrow(npmanova_sumdf)
npmanova_sumdf$version <- as.character(NA)
npmanova_sumdf$model <- as.character(NA)
npmanova_sumdf$modelnum <- as.character(NA)
npmanova_sumdf$vermod <- as.character(NA)
for(r in 1:nrow(npmanova_sumdf)){
  trial <- npmanova_sumdf[r,c('modid')]
  trsplt <- str_split(trial,"_")[[1]]
  if(length(trsplt)>2){
    npmanova_sumdf[r,c('version')] <- trsplt[1]
    npmanova_sumdf[r,c('model')] <- trsplt[2]
    npmanova_sumdf[r,c('modelnum')] <- trsplt[3]
    npmanova_sumdf[r,c('vermod')] <- paste(trsplt[1], trsplt[2], sep="_")
  } else {
    npmanova_sumdf[r,c('version')] <- trsplt[1]
    npmanova_sumdf[r,c('model')] <- 'm1'
    npmanova_sumdf[r,c('modelnum')] <- trsplt[2]
    npmanova_sumdf[r,c('vermod')] <- paste(trsplt[1], 'm1', sep="_")
  }
  #print(paste('done with ', r, sep=""))
}
# Correct vermod labeling for plotting
npmanova_sumdf$versionnm <- ifelse(npmanova_sumdf$version=="v2","nested","simultaneous")
npmanova_sumdf$vermod <- paste(npmanova_sumdf$versionnm,npmanova_sumdf$model, sep="_")
npmanova_sumdf$vermod <- ifelse(npmanova_sumdf$vermod=="simultaneous_m1", "simultaneous",npmanova_sumdf$vermod)

## Create trial supporting tables and figures
setwd(tabfolder)
trialstab <- trials[,c("numgrps","Rsq","AICc","AICcWt","AICcrelLL")]
write.csv(trialstab[1:10,],"climtrialstab.csv", row.names = F)

## AICc rank plots
setwd(figfolder)
dev.off() # Clear plot
tiff("ClimOpt_ranks.tif",width = 6.5, height = 3.5, units = 'in', res = 600 )
par(mfrow=c(1,2),mar = c(5.5,4,2,0.5))
boxplot(alltopmods$rank ~ alltopmods$vermod,cex.axis=0.85, ylab="AICc rank of trial", xlab="", main="Reference Prod. Based Ranking",las=2,cex.main=0.8, cex=0.85)# horizontal=T)
par(mar = c(5.5,3,2,0.5))
boxplot(npmanova_sumdf$rank ~ npmanova_sumdf$vermod,cex.axis=0.85,ylab="", xlab="", main="STM-Based Ranking",las=2,cex.main=0.8,cex=0.85)#, horizontal=T)
dev.off()

## Create unified Reference and STM table for final comparison of ranks
allmods_unified <-  alltopmods[,c("vermod","vermodrank","rank","AICcrelLL")]
allmods_unified$refrank <- allmods_unified$rank
allmods_unified$refAICcrelLL <- allmods_unified$AICcrelLL
allmods_unified$rank <- NULL
allmods_unified$AICcrelLL <- NULL
npmanova_sumdf$vermodrank <- paste(npmanova_sumdf$vermod,npmanova_sumdf$modelnum,sep="_")
allmods_unified <- left_join(allmods_unified, npmanova_sumdf[,c("modid","rank","vermodrank","AICcrelLL")],by = "vermodrank")
allmods_unified$rankave <- (allmods_unified$rank + allmods_unified$refrank)/2
allmods_unified$AICcrelLLave <- (allmods_unified$AICcrelLL + allmods_unified$refAICcrelLL)/2
summary(allmods_unified$rankave) # summarize averged ranks
summary(allmods_unified$AICcrelLLave) # summarize averged relative likelihoods
allmods_unified[allmods_unified$rankave==min(allmods_unified$rankave),] ## print the top model by ranks
allmods_unified[allmods_unified$AICcrelLLave==min(allmods_unified$AICcrelLLave),] ## print the top model by ranks

#################### ESG production summary
## Prepare heatmap tables
esg_prod_sum <- esd_final %>% dplyr::group_by(ESG) %>%
  dplyr::summarize(bigsage = mean(bigsage),
                   mtnsage = mean(mtnsage),
                   basinsage = mean(basinsage),
                   cora = mean(cora),
                   quga=mean(quga),
                   pinion = mean(pinion),
                   juniper= mean(juniper),
                   aspen=mean(aspen),
                   ponderosa=mean(ponderosa),
                   greasewd=mean(greasewd),
                   saltbush=mean(saltbush),
                   shadscale=mean(shadscale),
                   spai=mean(spai),
                   c3PerGr=mean(c3PerGr),
                   c4PerGr=mean(c4PerGr),
                   Forb=mean(Forb),
                   Grass=mean(Grass),
                   Shrub=mean(Shrub),
                   Tree=mean(Tree),
                   Total=mean(TOTAL_PROD))%>%
  dplyr::ungroup()
esg_prod_sum$n <- esd_final %>% dplyr::group_by(ESG) %>% tally() %>%
  dplyr::ungroup()
esg_prod_sum$n$ESG <- NULL
esg_prod_sum$ESDn <- esg_prod_sum$n$n
esg_prod_sum$n <- NULL
esg_prod_sum_df <- as.data.frame(esg_prod_sum)
setwd(tabfolder)
write.csv(esg_prod_sum_df,"ESG_Production.csv",row.names = F)

##################### SGU production table
SGU_prod_sum <- esd_final %>% dplyr::group_by(sgu) %>%
  dplyr::summarize(bigsage = mean(bigsage),
                   mtnsage = mean(mtnsage),
                   basinsage = mean(basinsage),
                   cora = mean(cora),
                   quga=mean(quga),
                   pinion = mean(pinion),
                   juniper= mean(juniper),
                   aspen=mean(aspen),
                   ponderosa=mean(ponderosa),
                   greasewd=mean(greasewd),
                   saltbush=mean(saltbush),
                   shadscale=mean(shadscale),
                   spai=mean(spai),
                   c3PerGr=mean(c3PerGr),
                   c4PerGr=mean(c4PerGr),
                   Forb=mean(Forb),
                   Grass=mean(Grass),
                   Shrub=mean(Shrub),
                   Tree=mean(Tree),
                   Total=mean(TOTAL_PROD))%>%
  dplyr::ungroup()
SGU_prod_sum$n <- esd_final %>% dplyr::group_by(sgu) %>% tally() %>%
  dplyr::ungroup()
SGU_prod_sum$n$sgu <- NULL
SGU_prod_sum$ESDn <- SGU_prod_sum$n$n
SGU_prod_sum$n <- NULL
SGU_prod_sum_df <- as.data.frame(SGU_prod_sum)
setwd(tabfolder)
write.csv(SGU_prod_sum_df,"SGU_Production.csv",row.names = F)


###################### Climate Breaks Figure

## Breaks being tested
ai.brks <-  unname(quantile(esd_final$aimean,c(0.075,0.2,0.4,0.5,0.6,0.925)))
nrcs.aibrks <- c(0.126,0.1749) # 0.2179 is already represented by the 0.925 quantile
ai.brks <- sort(append(ai.brks,nrcs.aibrks))
min.t.brks <- unname(quantile(esd_final$mintempmean,c(0.075,0.2,0.35,0.5,0.65,0.8,0.925)))
max.t.brks <- unname(quantile(esd_final$maxtempmean,c(0.075,0.2,0.35,0.5,0.65,0.8,0.925)))
pptrt.brks <- unname(quantile(esd_final$pptrtmean,c(0.075,0.2,0.35,0.5,0.65,0.8,0.925)))
yvalues.ai <- c(0,0,0,0,0,0,0,0)
yvalues.oth <- c(0,0,0,0,0,0,0)

## Plot climate variable histograms
setwd(figfolder)
dev.off() # Clear plot
tiff("Climate_breaks.tif",width = 6.5, height = 3.5, units = 'in', res = 600 )
par(mfrow=c(2, 2),mar = c(3, 4, 2, 0.5),cex = 0.7,cex.main=1)
hist(esd_final$aimean, main="Aridity Index",xlab = "")
points(ai.brks,yvalues.ai, col="red", pch = 24, bg="red")
hist(esd_final$maxtempmean, main="Maximum Temperature (C)",xlab = "")
points(max.t.brks,yvalues.oth, col="red", pch = 24, bg="red")
hist(esd_final$mintempmean, main="Minimum Temperature (C)",xlab = "")
points(min.t.brks,yvalues.oth, col="red", pch = 24, bg="red")
hist(esd_final$pptrtmean, main="Summer Precipitation Ratio",xlab = "")
points(pptrt.brks,yvalues.oth, col="red", pch = 24, bg="red")
dev.off()


############# Revision 1 ################
r1fldr <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/ESG_UCRB_manuscript/1strevision/Figs_R1"

## Redo Fig 8 (cover vs production) for ESGs in one climate zone
## Nanrrow down to one climate zone
esd_final$Tree_SI <- esd_final$Tree * 0.892179 # Convert to kg/ha (SI units)
esd_final_SW <- esd_final[esd_final$climzone=="Semiarid_Warm",]
esd_final_SW$sgugrps <- gsub("Semiarid_Warm_","",esd_final_SW$ESG)
coverplts_SW <- coverplts[coverplts$climzone=="Semiarid_Warm",]
coverplts_SW$sgugrps <- gsub("Semiarid_Warm_","",coverplts_SW$ESG)

## Tree Production and Cover
dev.off() # Clear plot
tiff(paste(r1fldr,"/ESG_Tree_Prod_SW.tif",sep=""),width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,13,1,0.5),mgp=c(2,1,0),cex=0.65)
boxplot(Tree_SI~sgugrps, data=esd_final_SW,las=1, horizontal=T,ylab="",xlab="Tree Production (kg/ha)")
dev.off() # Clear plot
tiff(paste(r1fldr,"/ESG_Tree_Cover_SW.tif",sep=""),width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,13,1,0.5),mgp=c(2,1,0),cex=0.65)
boxplot(Tree~sgugrps, data=coverplts_SW,las=1, horizontal=T,ylab="",xlab="% Tree Cover (Any Hit)")#yaxt="n",
dev.off() # Clear plot

## Total production and cover
dev.off() # Clear plot
tiff(paste(r1fldr,"/ESG_Tot_Prod_SW.tif",sep=""),width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,13,1,0.5),mgp=c(2,1,0),cex=0.65)
boxplot(TOTAL_PROD_SI~sgugrps, data=esd_final_SW,las=1, horizontal=T,ylab="",xlab="Reference Production (kg/ha)")
dev.off() # Clear plot
tiff(paste(r1fldr,"/ESG_Tot_Cover_SW.tif",sep=""),width = 3.85, height = 4, units = 'in', res = 600 )
par(mar = c(4,13,1,0.5),mgp=c(2,1,0),cex=0.65)
boxplot(TotCover~sgugrps, data=coverplts_SW,las=1, horizontal=T,ylab="",xlab="% Total Foliar Cover")#yaxt="n",
dev.off() # Clear plot

## Export ESD table for Supplementary Table S6
r1supfldr <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/ESG_UCRB_manuscript/1strevision/SuppMats_R1"
## Table to join to get ESD name
corrtab <- read.delim("C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/UCRB_BLM_esd18_SGU_correlations.txt",stringsAsFactors = F)
corrtab <- corrtab[,c("ecoclassid","ecoclassname")]
esd_final_s6 <- left_join(esd_final,corrtab, by="ecoclassid")
write.csv(esd_final_s6,paste(r1supfldr,"/ESD_Table.csv",sep=""),row.names = F)
