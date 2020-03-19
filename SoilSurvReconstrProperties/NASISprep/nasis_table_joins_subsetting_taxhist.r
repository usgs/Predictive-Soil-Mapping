### Data manipulation script for NASIS pedon data prep for contiguous United States
library(rgdal)

## Subsetting and Joining tables
setwd("D:/GIS_Archive/NRCS_pedons/NASIS_Pedons_20160301")
pedtab = read.delim("dbo_pedon_tab.txt", header=TRUE)
sitetab = read.delim("dbo_site_tab.txt", header=TRUE)
seriestab = read.delim("dbo_soilseries_tab.txt", header=TRUE)
pscs = read.delim("pscs_lup_tab.txt", header=TRUE)
pscs$pscs = toupper(pscs$pscs)
gglup = read.delim("greatgrp_lup_tab.txt", header=TRUE)
gglup$grtgrp_nm = toupper(gglup$grtgrp_nm)
subgrplup = read.delim("subgrp_lup_tab.txt", header=TRUE)
subgrplup$subgrp = toupper(subgrplup$subgrp)
subordlup = read.delim("subord_lup_ttab.txt", header=TRUE)
subordlup$subord = toupper(subordlup$subord)
orderlup = read.delim("order_lup_ttab.txt", header=TRUE)
orderlup$order = toupper(orderlup$order)
taxhist = read.delim("mostrec_taxhist_ttab.txt", header=TRUE) # Queried most recent tax record for each pedon peiid

## join tables, can also use 'merge' command, but it was problematic
nasispedons = cbind(sitetab[match(pedtab$upedonid, sitetab$usiteid),], pedtab) #attaches to pedtab

## Subset to get lats higher than 25 N to allow for better use of decimal lengths for precision
## Code will still work for other latitudes, but might exclude some sites unnecessarily
## by using the nchar of 7 sig digits to weed out coordinates with low precision
nasispedons = subset(nasispedons, latstddecimaldegrees > 25 | latdegrees > 25)


## Select most detailed Geo coordinates
# Precision of decimal lat/long fields
nasispedons$latdeclen = nchar(abs(nasispedons$latstddecimaldegrees))
nasispedons$longdeclen = nchar(abs(nasispedons$longstddecimaldegrees))
#nasispedons$decyxlendiff = nasispedons$latdeclen - nasispedons$longdeclen

## Start filling new x & y fields with best data ####NEED to fix
nasispedons$ywgs84 = ifelse(nasispedons$latdeclen > 7, nasispedons$latstddecimaldegrees, 9999)
nasispedons$xwgs84 = ifelse(nasispedons$longdeclen > 7, nasispedons$longstddecimaldegrees, 9999)
nasispedons$latcalc = as.numeric(nasispedons$latdegrees) + as.numeric(nasispedons$latminutes)/60 + as.numeric(nasispedons$latseconds)/3600
nasispedons$longcalc = as.numeric(nasispedons$longdegrees) + as.numeric(nasispedons$longminutes)/60 + as.numeric(nasispedons$longseconds)/3600
nasispedons$longcalc = ifelse(nasispedons$longdir == 2, -nasispedons$longcalc, nasispedons$longcalc)
# Again, narrowing down to USA lower 48, other longitudes closer to prime meridian may be unnecessarily excluded by
# the precision standard use: at least 7 signif. digits in this case
nasispedons = subset(nasispedons, longstddecimaldegrees < -60 | longcalc < -60)
nasispedons['pid'] = rownames(nasispedons)
#Reproject pts with nad83 and nad27 datums
#nad83
pedsnad83 = subset(nasispedons, horizdatnm == 2, select = c(pid, longcalc, latcalc, latseconds, longseconds))
pedsnad83$newlat = ""
pedsnad83$newlong = ""
pedsnad83$ynad83 = ifelse(pedsnad83$latseconds != "NA", pedsnad83$latcalc, 9999)
pedsnad83$ynad83[is.na(pedsnad83$ynad83)]<-9999
pedsnad83 = subset(pedsnad83, pedsnad83$ynad83 != 9999)
pedsnad83$xnad83 = ifelse(pedsnad83$longseconds != "NA", pedsnad83$longcalc, 9999)
pedsnad83$xnad83[is.na(pedsnad83$xnad83)]<-9999
pedsnad83 = subset(pedsnad83, pedsnad83$xnad83 != 9999)
coordinates(pedsnad83) = c("xnad83", "ynad83")
proj4string(pedsnad83) = CRS("+proj=longlat +ellps=GRS80")
nadtowgs = spTransform(pedsnad83, CRS("+proj=longlat +datum=WGS84"))
nadtowgs$newlat = as.numeric(nadtowgs$ynad83)
nadtowgs$newlong = as.numeric(nadtowgs$xnad83)
nadtowgs = subset(nadtowgs, select = c(pid, newlat, newlong))
nasispedons = merge(x=nasispedons, y=nadtowgs, by = "pid", all.x = TRUE)
nasispedons$xwgs84 = ifelse(nasispedons$xwgs84 == 9999, nasispedons$newlong, nasispedons$xwgs84)
nasispedons$ywgs84 = ifelse(nasispedons$ywgs84 == 9999, nasispedons$newlat, nasispedons$ywgs84)
nasispedons$newlong = ""
nasispedons$newlat = ""
## nad27 tranformation
pedsnad27 = subset(nasispedons, horizdatnm == 1, select = c(pid, longcalc, latcalc, latseconds, longseconds))
pedsnad27$newlat = ""
pedsnad27$newlong = ""
pedsnad27$ynad27 = ifelse(pedsnad27$latseconds != "NA", pedsnad27$latcalc, 9999)
pedsnad27$ynad27[is.na(pedsnad27$ynad27)]<-9999
pedsnad27 = subset(pedsnad27, pedsnad27$ynad27 != 9999)
pedsnad27$xnad27 = ifelse(pedsnad27$longseconds != "NA", pedsnad27$longcalc, 9999)
pedsnad27$xnad27[is.na(pedsnad27$xnad27)]<-9999
pedsnad27 = subset(pedsnad27, pedsnad27$xnad27 != 9999)
# Need new data frame for projections with ability to link back...
coordinates(pedsnad27) = c("xnad27", "ynad27")
proj4string(pedsnad27) = CRS("+proj=longlat +ellps=GRS80")
nadtowgs = ""
nadtowgs = spTransform(pedsnad27, CRS("+proj=longlat +datum=WGS84"))
nadtowgs$newlat = as.numeric(nadtowgs$ynad27)
nadtowgs$newlong = as.numeric(nadtowgs$xnad27)
nadtowgs = subset(nadtowgs, select = c(pid, newlat, newlong))
nasispedons = subset(nasispedons, select = -c(newlat,newlong))
nasispedons = merge(x=nasispedons, y=nadtowgs, by = "pid", all.x = TRUE)
nasispedons$xwgs84 = ifelse(nasispedons$xwgs84 == 9999, nasispedons$newlong, nasispedons$xwgs84)
nasispedons$ywgs84 = ifelse(nasispedons$ywgs84 == 9999, nasispedons$newlat, nasispedons$ywgs84)
nasispedons = subset(nasispedons, ywgs84 != "NA" & ywgs84 != 9999)# remove all points 
nasispedons = subset(nasispedons, xwgs84 != "NA" & xwgs84 != 9999)# without coordinates

### Now create subtables at different levels of soil taxonomy###

## Join to taxhist table
# Remove duplicate fields
taxhist$taxonnameh = toupper(taxhist$taxonname)
taxhist$taxclnameh = toupper(taxhist$taxclname)
taxhist$taxclname_sh = toupper(taxhist$taxclname_s)
taxhist = subset(taxhist, select = -c(taxonname, taxclname, taxclname_s))
nasispedons = cbind(taxhist[match(nasispedons$peiid, taxhist$peiidref),], nasispedons) #attaches to nasispedons

#Now join to tax lookup tables
nasispedons = cbind(pscs[match(nasispedons$taxpartsize, pscs$pscs_id),], nasispedons) #attaches to nasispedons
nasispedons = cbind(gglup[match(nasispedons$taxgrtgroup, gglup$grtgrp_id),], nasispedons)
nasispedons = cbind(subgrplup[match(nasispedons$taxsubgrp, subgrplup$subgrp_id),], nasispedons)
nasispedons = cbind(subordlup[match(nasispedons$taxsuborder, subordlup$subord_id),], nasispedons)
nasispedons = cbind(orderlup[match(nasispedons$taxorder, orderlup$order_id),], nasispedons)

# Convert NA's into term that can be cleaned
nasispedons$order[is.na(nasispedons$order)]<-"NULL"
nasispedons$pscs[is.na(nasispedons$pscs)]<-"NULL"
nasispedons$subgrp[is.na(nasispedons$subgrp)]<-"NULL"
nasispedons$subord[is.na(nasispedons$subord)]<-"NULL"
nasispedons$grtgrp_nm[is.na(nasispedons$grtgrp_nm)]<-"NULL"
nasispedons$fam = as.character(nasispedons$taxclnameh)
nasispedons$fam[is.na(nasispedons$fam)]<-"NULL"
nasispedons$famlen = nchar(nasispedons$fam)
nasispedons$fam = ifelse(nasispedons$famlen > 25, nasispedons$fam, "NULL")

# Capitalize taxoname for cleanup
nasispedons$seriescap = toupper(nasispedons$taxonname)

#Now clean up PSCS: include Psamments and Rock outcrops
nasispedons$pscs = ifelse(nasispedons$pscs == "NULL" & grepl("ROCK OUTCROP", nasispedons$seriescap), "ROCK OUTCROP", nasispedons$pscs)
nasispedons$pscs = ifelse(nasispedons$pscs == "NULL" & nasispedons$subord == "PSAMMENTS", "PSAMMENTS", nasispedons$pscs)

#Now add "Rock Outcrop" instances to orders, suborders, great groups, and subgroups fields
nasispedons$grtgrp_nm = ifelse(nasispedons$grtgrp_nm == "" & grepl("ROCK OUTCROP", nasispedons$seriescap), "ROCK OUTCROP", nasispedons$grtgrp_nm)
nasispedons$order = ifelse(nasispedons$order == "NULL" & grepl("ROCK OUTCROP", nasispedons$seriescap), "ROCK OUTCROP", nasispedons$order)
nasispedons$subord = ifelse(nasispedons$subord == "NULL" & grepl("ROCK OUTCROP", nasispedons$seriescap), "ROCK OUTCROP", nasispedons$subord)
nasispedons$subgrp = ifelse(nasispedons$subgrp == "NULL" & grepl("ROCK OUTCROP", nasispedons$seriescap), "ROCK OUTCROP", nasispedons$subgrp)
nasispedons$fam = ifelse(nasispedons$fam == "NULL" & grepl("ROCK OUTCROP", nasispedons$seriescap), "ROCK OUTCROP", nasispedons$fam)

#### Now match to nearest soil series to link to other table ####

## Join series table to taxalookup tables
seriestab = cbind(pscs[match(seriestab$taxpartsize, pscs$pscs_id),], seriestab) #attaches to seriestab
seriestab = cbind(gglup[match(seriestab$taxgrtgroup, gglup$grtgrp_id),], seriestab)
seriestab = cbind(subgrplup[match(seriestab$taxsubgrp, subgrplup$subgrp_id),], seriestab)
seriestab = cbind(subordlup[match(seriestab$taxsuborder, subordlup$subord_id),], seriestab)
seriestab = cbind(orderlup[match(seriestab$taxorder, orderlup$order_id),], seriestab)

# Subset seriestab to just have needed fields
seriestabs = subset(seriestab, select = c(soilseriesname,order,subord,subgrp,grtgrp_nm,pscs,taxclname))

#Need a different series table taxclname for join
seriestabs$soilseriesnames = toupper(seriestabs$soilseriesname)
seriestabs = subset(seriestabs, select = -c(soilseriesname))
seriestabs$orders = toupper(seriestabs$order)
seriestabs = subset(seriestabs, select = -c(order))
seriestabs$subords = toupper(seriestabs$subord)
seriestabs = subset(seriestabs, select = -c(subord))
seriestabs$grtgrp_nms = toupper(seriestabs$grtgrp_nm)
seriestabs = subset(seriestabs, select = -c(grtgrp_nm))
seriestabs$subgrps = toupper(seriestabs$subgrp)
seriestabs = subset(seriestabs, select = -c(subgrp))
seriestabs$pscss = toupper(seriestabs$pscs)
seriestabs = subset(seriestabs, select = -c(pscs))
seriestabs$taxclnames = toupper(seriestabs$taxclname)
seriestabs = subset(seriestabs, select = -c(taxclname))

## Strip common series name modifiers off tax class name
nasispedons$taxonnameh = gsub(" VARIANT","", nasispedons$taxonnameh)
nasispedons$taxonnameh = gsub(" TAXADJUNCT","", nasispedons$taxonnameh)
nasispedons$taxonnameh = gsub(" TAXAJUNCT","", nasispedons$taxonnameh)
nasispedons$taxonnameh = gsub(" FAMILY","", nasispedons$taxonnameh)
nasispedons$taxonnameh = gsub(" LIKE","", nasispedons$taxonnameh)
nasispedons$taxonnameh = gsub("-LIKE","", nasispedons$taxonnameh)
nasispedons$taxonnameh = gsub("-SIMILAR","", nasispedons$taxonnameh)
nasispedons$taxonnameh = gsub(" SIMILAR","", nasispedons$taxonnameh)
nasispedons$taxonnameh = gsub(" TAX.","", nasispedons$taxonnameh)
nasispedons$taxonnameh = gsub(" TAX","", nasispedons$taxonnameh)
nasispedons$taxonnameh = gsub(" ERODED","", nasispedons$taxonnameh)
nasispedons$taxonnameh = gsub(", ERODED","", nasispedons$taxonnameh)

## Strip common series name modifiers off tax class name from taxhist
nasispedons$seriescap = gsub(" VARIANT","", nasispedons$seriescap)
nasispedons$seriescap = gsub(" TAXADJUNCT","", nasispedons$seriescap)
nasispedons$seriescap = gsub(" TAXAJUNCT","", nasispedons$seriescap)
nasispedons$seriescap = gsub(" FAMILY","", nasispedons$seriescap)
nasispedons$seriescap = gsub(" LIKE","", nasispedons$seriescap)
nasispedons$seriescap = gsub("-LIKE","", nasispedons$seriescap)
nasispedons$seriescap = gsub("-SIMILAR","", nasispedons$seriescap)
nasispedons$seriescap = gsub(" SIMILAR","", nasispedons$seriescap)
nasispedons$seriescap = gsub(" TAX.","", nasispedons$seriescap)
nasispedons$seriescap = gsub(" TAX","", nasispedons$seriescap)
nasispedons$seriescap = gsub(" ERODED","", nasispedons$seriescap)
nasispedons$seriescap = gsub(", ERODED","", nasispedons$seriescap)

#Add in any possible extra hits from taxhist
nasispedons$seriescap = ifelse(nasispedons$seriescap != nasispedons$taxonnameh, nasispedons$taxonnameh,nasispedons$seriescap)

## Join nasispedons and series table
#Joins
nasispedonsjn = cbind(seriestabs[match(nasispedons$seriescap, seriestabs$soilseriesnames),], nasispedons)


## now fill in the taxa where the series table can add in
# Make new fields cleanable
nasispedonsjn$orders[is.na(nasispedonsjn$orders)]<-"NULL"
nasispedonsjn$pscss[is.na(nasispedonsjn$pscss)]<-"NULL"
nasispedonsjn$subgrps[is.na(nasispedonsjn$subgrps)]<-"NULL"
nasispedonsjn$subords[is.na(nasispedonsjn$subords)]<-"NULL"
nasispedonsjn$grtgrp_nms[is.na(nasispedonsjn$grtgrp_nms)]<-"NULL"
nasispedonsjn$taxclnames[is.na(nasispedonsjn$taxclnames)]<-"NULL"
nasispedonsjn$soilseriesnames[is.na(nasispedonsjn$soilseriesnames)]<- "NULL"
# Now add to the original taxa classes
nasispedonsjn$pscs = ifelse(nasispedonsjn$pscs == "NULL" & nasispedonsjn$pscss != "NULL", nasispedonsjn$pscss, nasispedonsjn$pscs)
nasispedonsjn$order = ifelse(nasispedonsjn$order == "NULL" & nasispedonsjn$orders != "NULL", nasispedonsjn$orders, nasispedonsjn$order)
nasispedonsjn$subord = ifelse(nasispedonsjn$subord == "NULL" & nasispedonsjn$subords != "NULL", nasispedonsjn$subords, nasispedonsjn$subord)
nasispedonsjn$subgrp = ifelse(nasispedonsjn$subgrp == "NULL" & nasispedonsjn$subgrps != "NULL", nasispedonsjn$subgrps, nasispedonsjn$subgrp)
nasispedonsjn$grtgrp_nm = ifelse(nasispedonsjn$grtgrp_nm == "NULL" & nasispedonsjn$grtgrp_nms != "NULL", nasispedonsjn$grtgrp_nms, nasispedonsjn$grtgrp_nm)
nasispedonsjn$spodic = ifelse(nasispedonsjn$order == "SPODOSOLS" | grepl("SPODIC", nasispedonsjn$subgrp), "SPODIC", "NON_SPODIC")
nasispedonsjn$taxclnameslen = nchar(nasispedonsjn$taxclnames)
nasispedonsjn$taxclnames = ifelse(nasispedonsjn$taxclnames > 25, nasispedonsjn$taxclnames, "NULL")
nasispedonsjn$fam = ifelse(nasispedonsjn$fam == "NULL" & nasispedonsjn$taxclnames != "NULL", nasispedonsjn$taxclnames, nasispedonsjn$fam)
nasispedonsjn$soilseriesnames = ifelse(nasispedonsjn$soilseriesnames == "NULL" & grepl("ROCK OUTCROP", nasispedons$seriescap), "ROCK OUTCROP", nasispedonsjn$soilseriesnames)
nasispedonsjn$pscsmod = ifelse(nasispedonsjn$pscs == "PSAMMENTS" & grepl("LITHIC", nasispedonsjn$subgrps), "LITHIC SANDY", nasispedonsjn$pscs)
nasispedonsjn$pscsmodorg = ifelse(nasispedonsjn$pscs == "PSAMMENTS", "SANDY", nasispedonsjn$pscs)
nasispedonsjn$pscsmodorg = ifelse(nasispedonsjn$order == "HISTOSOLS", "ORGANIC", nasispedonsjn$pscsmodorg)
nasispedonsjn$pscsmodorg = ifelse((nasispedonsjn$pscsmodorg != "NULL") & (nasispedonsjn$pscsmodorg != "LOAMY") & (nasispedonsjn$pscsmodorg != "CLAYEY") & grepl("LITHIC", nasispedonsjn$subgrps), paste("LITHIC", nasispedonsjn$pscsmodorg, sep = " "), nasispedonsjn$pscsmodorg)




## Subset table for each taxa level of interest
nasispedonsjn_pscs = subset(nasispedonsjn, nasispedonsjn$pscs != "NULL", select = c(pid, upedonid, xwgs84, ywgs84, pscs))
nasispedonsjn_gg = subset(nasispedonsjn, nasispedonsjn$grtgrp_nm != "NULL", select = c(pid, upedonid, xwgs84, ywgs84, grtgrp_nm))
nasispedonsjn_ord = subset(nasispedonsjn, nasispedonsjn$order != "NULL", select = c(pid, upedonid, xwgs84, ywgs84, order))
nasispedonsjn_subord = subset(nasispedonsjn, nasispedonsjn$subord != "NULL", select = c(pid, upedonid, xwgs84, ywgs84, subord))
nasispedonsjn_subgrp = subset(nasispedonsjn, nasispedonsjn$subgrp != "NULL", select = c(pid, upedonid, xwgs84, ywgs84, subgrp))
nasispedonsjn_spodic = subset(nasispedonsjn, nasispedonsjn$subgrp != "NULL", select = c(pid, upedonid, xwgs84, ywgs84, spodic))
nasispedonsjn_series = subset(nasispedonsjn, nasispedonsjn$soilseriesnames != "NULL", select = c(pid, upedonid, xwgs84, ywgs84, soilseriesnames))
nasispedonsjn_fam = subset(nasispedonsjn, nasispedonsjn$fam != "NULL", select = c(pid, upedonid, xwgs84, ywgs84, fam))
nasispedonsjn_pscsmod = subset(nasispedonsjn, nasispedonsjn$pscsmod != "NULL", select = c(pid, upedonid, xwgs84, ywgs84, pscsmod))
nasispedonsjn_pscsmodorg = subset(nasispedonsjn, nasispedonsjn$pscsmodorg != "NULL", select = c(pid, upedonid, xwgs84, ywgs84, pscsmodorg))

## Set workspace to location to save
setwd("G:/GIS_Archive/NRCS_pedons/NASIS_Pedons_20160301/tables_20160503_join_script")
write.table(nasispedonsjn_pscs, file = "nasispts_pscs_ttab.txt", sep = "\t")
write.table(nasispedonsjn_gg, file = "nasispts_gg_ttab.txt", sep = "\t")
write.table(nasispedonsjn_ord, file = "nasispts_ord_ttab.txt", sep = "\t")
write.table(nasispedonsjn_subord, file = "nasispts_subord_ttab.txt", sep = "\t")
write.table(nasispedonsjn_subgrp, file = "nasispts_subgrp_ttab.txt", sep = "\t")
write.table(nasispedonsjn_spodic, file = "nasispts_spodic_ttab.txt", sep = "\t")
write.table(nasispedonsjn_series, file = "nasispts_series_ttab.txt", sep = "\t")
write.table(nasispedonsjn_fam, file = "nasispts_fam_ttab.txt", sep = "\t")
write.table(nasispedonsjn_pscsmod, file = "nasispts_pscsmod_ttab.txt", sep = "\t")
write.table(nasispedonsjn_pscsmodorg, file = "nasispts_pscsmodorg_ttab.txt", sep = "\t")


