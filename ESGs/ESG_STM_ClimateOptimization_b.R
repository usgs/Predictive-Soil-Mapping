##########################################
## Workflow to optimize soil geomorphic & climate breaks
## for ecological site groups based on
## STM state data from NRCS EDIT
##########################################
# AU: Travis Nauman, US Geological SUrvey, tnauman@usgs.gov
# 6/2021

## Check/install required packages
required.packages <- c("plyr","tidyverse","gridExtra","reshape2","vegan","qpcR", "snow", "snowfall") ## ,"snow", "snowfall","parallel"
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

## Key folders
stmfolder <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/STM"
scriptfolder <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize"

## Read in esd model tables
setwd(scriptfolder)
esd_final_v1 <- readRDS("esd_final_v1b.rds")
esd_final_v2 <- readRDS("esd_final_v2b.rds")
esd_final <- cbind(esd_final_v1,esd_final_v2[,57:length(esd_final_v2)]) ## Add v2 groups to those of v1
modellist <- colnames(esd_final)[57:length(esd_final)]

## Read in STM state table
setwd(stmfolder)
stmtab <- read.delim("STM_matrix_expanded_SSB_for_R_ttab_twn.txt",stringsAsFactors = F)
stmtab$ecoclassid <- stmtab$esdid
stmtab$esdid <- NULL
daty_names <- colnames(stmtab)[1:39]

## Join esd models to STM table
esd_stm <- dplyr::inner_join(esd_final,stmtab,by="ecoclassid")
daty <- esd_stm[,daty_names]
summary(daty)
## Remove empty columns (from loss of esds in join)
daty$inon.oth <- NULL

##### Loop through models to create PerManova models 
##### and associated AICc using stm states as response variables
#for(m in modellist){
opt_fn <- function(m){
  datx <- data.frame(group=as.factor(esd_stm[,m]))
  groupnum <- length(unique(as.character(datx$group)))
  # Empty dataframe to fill or return as an empty row 
  npmanovadf <- data.frame(modid=m,numgrps = groupnum, Rsq=as.numeric(""),F=as.numeric(""),SSgrp=as.numeric(""),SSres=as.numeric(""), stringsAsFactors = F)
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
}

## Parallel list apply
cpus <- 15
sfInit(parallel=TRUE, cpus=cpus)
sfExport("modellist", "esd_stm", "daty_names","daty","opt_fn")
sfLibrary(dplyr)
sfLibrary(vegan)
npmanova_sum <- sfLapply(modellist, function(m){try(opt_fn(m))})
# 0.0042 per row with mixed set (31 cpus)
snowfall::sfStop()
## Put list into dataframe
npmanova_sumdf <- do.call("rbind", npmanova_sum)
npmanova_sumdf <- na.omit(npmanova_sumdf)
npmanova_sumdf$AICc <- nrow(esd_stm)*log(npmanova_sumdf$SSres/nrow(esd_stm)) + (2*(npmanova_sumdf$numgrps+2)) + ((2*(npmanova_sumdf$numgrps+2)*(npmanova_sumdf$numgrps+3))/(nrow(esd_stm)-(npmanova_sumdf$numgrps+2)-1))
npmanova_sumdf <- npmanova_sumdf[order(npmanova_sumdf$AICc),]
npmanova_sumdf$AICcWt <- akaike.weights(npmanova_sumdf$AICc)$weights
npmanova_sumdf$AICcrelLL <- akaike.weights(npmanova_sumdf$AICc)$rel.LL

## Save file
setwd(scriptfolder)
saveRDS(npmanova_sumdf,"esd_stm_opt_df_b.rds")
npmanova_sumdf <- readRDS("esd_stm_opt_df_b.rds")


## Look at by version and model
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
  print(paste('done with ', r, sep=""))
}
## Now plot models versions against rank to see how different groups performed.
boxplot(npmanova_sumdf$rank ~ npmanova_sumdf$vermod)

#### Now run perManova and Beta Dispersion tests on chosen model
topmod <- 'v2_m4_26'
## One group only has one esd - must remove for overall tests
esd_stm_v2 <-  esd_stm[esd_stm[,topmod]!= '4_2_2_1_1_3',] ## group with only one instance
daty <- esd_stm_v2[,daty_names]
datx <- data.frame(group=as.factor(esd_stm_v2[,topmod]))
## Adonis works for one factor problem
npmanmod <- adonis(daty ~ group, data=datx,method="bray",parallel = 15,permutations = 999)
## Test for heterogenuity of dispersion among groups
braydist <- vegdist(daty, method="bray")
dispgrp <- as.factor(esd_stm_v2[,topmod])
group_bdisp <- betadisper(braydist,dispgrp, bias.adjust = T, sqrt.dist = F)
## Plot dispersions among groups
plot(group_bdisp)
boxplot(group_bdisp,horizontal = TRUE, varwidth = TRUE, xlab="", ylab="", cex.axis = .75,las=1)
## Permutation test to get more accurate Type 1 errors
sgu_bdisp_ptest <- permutest(group_bdisp, pairwise = F,permutations = 999, parallel = 14)




