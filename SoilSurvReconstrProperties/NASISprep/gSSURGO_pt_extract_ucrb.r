###################################
### Script to extract property dat from gSSURGO
### to NASIS series transect observations

## Packages
required.packages <- c("raster", "sp", "rgdal","snow", "snowfall","parallel", "itertools","doParallel", "plyr", "ncdf4","maptools", "rgeos","stats","spdep","randomForest","sf")# maybe need dplyr??
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
##Raster settings
rasterOptions(maxmemory = 1e+09, chunksize = 1e+08)

### Explore gSSURGO GDB and open necessary files
# Vignettes to read all GDB files
setwd("/home/tnaum/data/gSSURGO18")
gdb_contents <- st_layers(dsn = file.path(getwd(), 'gSSURGO_CONUS.gdb'), do_count = TRUE) 
gdb_neat_deets <- data.frame('Name' = gdb_contents[['name']], 
                             'Geomtype' = unlist(gdb_contents[['geomtype']]), 
                             'features' = gdb_contents[['features']], 
                             'fields' = gdb_contents[['fields']]) 
# so the non-spatial tables have geometry == NA, lets get their names 
gbd_nonspatial <- as.list(gdb_neat_deets[is.na(gdb_neat_deets$Geomtype), 'Name']) 
# names attrib is useful here 
names(gbd_nonspatial) <- gdb_neat_deets[is.na(gdb_neat_deets$Geomtype), 'Name'] 
# Now read in spatial file with rgdal to get sp object
fgdb <- "/home/tnaum/data/gSSURGO18/gSSURGO_CONUS.gdb"
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb) # list layers
polys <- readOGR(fgdb, "MUPOLYGON") # Takes 24 hrs to load with SSURGO for all USA
polys.proj <- projection(polys)
### Clip polys to study area ###########
# Load map clip boundary (if needed) 
setwd("/home/tnaum/Dropbox/USGS/BLM_projects/Utah_BLM_Salinity/Huc6_boundary")
polybound <- readOGR(".", "CO_River_watershed_Meade_alb")
polybound <- spTransform(polybound, polys.proj)
#polys <- polys[polybound,] # Clipping down to study area polys (done first time)
setwd("/home/tnaum/data/gSSURGO18/UCRB_gSSURGO18_mupolys_nasis")
#saveRDS(polys,"UCRB_gSSURGO18_mupolys.rds")
polys <- readRDS("UCRB_gSSURGO18_mupolys.rds")
polys.proj <- projection(polys)


# Also load necessary data tables and subset to study area
setwd("/home/tnaum/data/gSSURGO18")
comp.df <- sf::st_read(dsn = fgdb, layer = "component")
comp.df$compname = toupper(comp.df$compname)
horizon.df <- sf::st_read(dsn = fgdb, layer = "chorizon")
# subset tables
mukeys <- polys$MUKEY
mukeys <- as.character(mukeys)
comps <- comp.df[comp.df$mukey %in% mukeys,]
cokeys <- comps$cokey
cokeys <- as.character(cokeys)
horizs <- horizon.df[horizon.df$cokey %in% cokeys,]
horiz_comps <- merge(comps,horizs, by="cokey")
horiz_comps$chkey <- as.character(horiz_comps$chkey)
horiz_comps$cokey <- as.character(horiz_comps$cokey)

#### Load in NASIS points
setwd("/media/tnaum/D/GIS_Archive/NRCS_pedons/NASIS_Pedons_20160301/tables_20160503_join_script/series")
shp.pts <- readOGR(".", "nasispts16_series_L48")
shp.pts$upedonid <- as.character(shp.pts$upedonid) 
shp.pts$pid <- as.character(shp.pts$pid) 
shp.pts$soilseries <- as.character(shp.pts$soilseries) 
shp.pts <- spTransform(shp.pts, polys.proj)
pts.proj <- projection(shp.pts)
## Now clip points and check with visualization
shp.pts <- shp.pts[polybound,]#clip by outer extent of all polybound features
save.image("~/data/gSSURGO18/UCRB_gSSURGO18_mupolys_nasis/UCRB__gSSURGO_mupolys_nasis_workspace.RData")
## Some cleanup to turn factos to characters to avoid subsetting issues in memory
polys@data[] <- lapply(polys@data, function(x) if (is.factor(x)) as.character(x) else {x})
horiz_comps[] <- lapply(horiz_comps, function(x) if (is.factor(x)) as.character(x) else {x})
shp.pts@data[] <- lapply(shp.pts@data, function(x) if (is.factor(x)) as.character(x) else {x})
## Save horiz_comps
setwd("/home/tnaum/data/gSSURGO18/UCRB_gSSURGO18_mupolys_nasis")
saveRDS(horiz_comps,"horiz_comps_ucrb.rds")

#### Plot all to check projections
plot(polybound)
#plot(polys, add=TRUE) # takes a while
plot(shp.pts, add=TRUE)

## Create list of observation point ids (pid) for all observations
pids <- shp.pts$pid
## Poly IDs
polys$polyid <- seq_len(nrow(polys))

spatial.extfn <- function(pid){ ### list apply function using pedon pid to parallelize
  ## other potential fn args: ,polys,horiz_comps,shp.pts
  obs <- shp.pts[shp.pts$pid %in% pid,]
  obs_df <- as.data.frame(obs)
  polyint <- sp::over(obs,polys)
  polyint <- polyint$polyid
  poly1 <- polys[polys$polyid %in% polyint,]
  poly1$MUKEY <- as.character(poly1$MUKEY)
  poly1$MUSYM <- as.character(poly1$MUSYM)
  polyneigh_mtx <- gTouches(polys, poly1, byid = TRUE, returnDense = FALSE)
  neigh.list <- c()
  for(i in seq(1:length(polyneigh_mtx))){
    neigh <- polyneigh_mtx[i]
    if(length(neigh[[1]])>0){
      neigh.list <- append(neigh.list,names(neigh), after = length(neigh.list))
      #print(i)
    }
  }
  polys2 <- polys[rownames(polys@data) %in% neigh.list,]
  mukeys <- c(poly1$MUKEY,polys2$MUKEY)
  comps_chk <- horiz_comps[horiz_comps$mukey %in% mukeys,] # Maybe just use comps to minimize memory in future, can merge horizon after
  ## Stip off extra nomenclature to get nearest possible component
  comps_chk$compname = gsub(" VARIANT","", comps_chk$compname)
  comps_chk$compname = gsub(" TAXADJUNCT","", comps_chk$compname)
  comps_chk$compname = gsub(" TAXAJUNCT","", comps_chk$compname)
  comps_chk$compname = gsub(" FAMILY","", comps_chk$compname)
  comps_chk$compname = gsub(" LIKE","", comps_chk$compname)
  comps_chk$compname = gsub("-LIKE","", comps_chk$compname)
  comps_chk$compname = gsub("-SIMILAR","", comps_chk$compname)
  comps_chk$compname = gsub(" SIMILAR","", comps_chk$compname)
  comps_chk$compname = gsub(" TAX.","", comps_chk$compname)
  comps_chk$compname = gsub(" TAX","", comps_chk$compname)
  comps_chk$compname = gsub(" ERODED","", comps_chk$compname)
  comps_chk$compname = gsub(", ERODED","", comps_chk$compname)
  comp_mtch <- comps_chk[comps_chk$compname %in% obs_df$soilseries,][1,] ## This step picks 1 horizon from each component = ISSUE
  # A fix to include all horizons from the pts is at bottom of script
  obs_df <- cbind(obs_df,comp_mtch)
  return(obs_df)
  rm(polys2,comps_chk, polyneigh_mtx, poly1)
  gc()
}

## Setup up parallel list apply
snowfall::sfInit(parallel=TRUE, cpus=30) ## Choose number of cpus available
snowfall::sfExport("pids","polys","horiz_comps","shp.pts", "spatial.extfn")
snowfall::sfLibrary(plyr)
snowfall::sfLibrary(rgdal)
snowfall::sfLibrary(maptools)
snowfall::sfLibrary(sp)
snowfall::sfLibrary(rgeos)
Sys.time()
pts.SSls <- snowfall::sfLapply(pids, function(pid){spatial.extfn(pid)}) # Took 24hrs+ for UCRB, 340GB ram
Sys.time()
snowfall::sfStop()
## Pull out of list into dataframe
pts.SSls_df <- pts.SSls[[1]] ## must be data.frame
pts.SSls_df <- pts.SSls_df[FALSE,]
for(i in seq(1:length(pts.SSls))){
  newrow <- pts.SSls[[i]]
  if(class(newrow)=="data.frame"){
    pts.SSls_df <- rbind(pts.SSls_df, newrow)
  }
  print(paste("Done with ", i, sep=""))
}
## Save
setwd("/home/tnaum/data/gSSURGO18/UCRB_gSSURGO18_mupolys_nasis")
saveRDS(pts.SSls_df, "nasispts_gSSURGO18hor_ucrb.rds")



