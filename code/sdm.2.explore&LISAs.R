# SDM 2 SDM data explore
# #######################################################################################################################
# Housekeeping ----

setwd("E:/R/sdm_R")

ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
                  if (length(new.pkg))install.packages(new.pkg, dependencies = TRUE)
                  sapply(pkg, require, character.only = TRUE)}
ipak(c("stringr","usdm", "biomod2", "raster", "rgdal", "scales", "grid", "foreign","dplyr","magrittr","tidyr",
       "magrittr","ggplot2","gridExtra","raster","rasterVis","dismo","sdm","installr","knitr","parallel"))
installAll() # installing all of 'sdm''s dependencies.

# load data
heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

b.raster.list.names = list("Rain", "Jant", "Jult","TWet", "Slop", "Soil")
s.raster.list.names = list("Rain", "Jant", "Jult", "TWet", "Slop", "Soil", "Vegt")

b.raster.list     = readRDS(paste0(heavies.rds.path,"b.raster.list.rds"))
s.raster.list     = readRDS(paste0(heavies.rds.path,"s.raster.list.rds"))
b.preds           = readRDS(paste0(heavies.rds.path,"b.preds.rds")) # raster stack
s.preds           = readRDS(paste0(heavies.rds.path,"s.preds.rds")) # raster stack

borders              = readRDS("./rds_objects/borders.rds")
beershebensis.buffer = readRDS("./rds_objects/beershebensis.buffer_incWB.rds"); plot(beershebensis.buffer)
schreiberi.buffer    = readRDS("./rds_objects/schreiberi.buffer.rds"); plot(schreiberi.buffer)

xlims_b = c(34.267142, 35.397332);   saveRDS(xlims_b, "./rds_objects/xlims_b.rds")
ylims_b = c(30.507978, 31.720564);   saveRDS(ylims_b, "./rds_objects/ylims_b.rds")
xlims_s = c(34.271733, 35.324798);   saveRDS(xlims_s, "./rds_objects/xlims_s.rds")  
ylims_s = c(31.126732, 33.107418);   saveRDS(ylims_s, "./rds_objects/ylims_s.rds")

major.cities = readRDS("./rds_objects/major.cities.rds")
small.cities = readRDS("./rds_objects/small.cities.rds")
towns        = readRDS("./rds_objects/towns.rds")
villages     = readRDS("./rds_objects/villages.rds")
groads       = readRDS("./rds_objects/groads.rds")

nat.res   = readRDS(paste0(heavies.rds.path,"nat.res.rds"));  plot(nat.res,  col="darkgreen",  border="darkgreen")
nat.park  = readRDS(paste0(heavies.rds.path,"nat.park.rds")); plot(nat.park, col="lightgreen", border="lightgreen",add=T)
kkl.forestry   = readRDS(paste0(heavies.rds.path,"kkl.forestry.rds"))
kkl.plans.a    = readRDS(paste0(heavies.rds.path,"kkl.plans.a.rds")) # provided by Alexandra in South region
kkl.plans.b    = readRDS(paste0(heavies.rds.path,"kkl.plans.b.rds")) # provided by Yehudah in Central region
kkl.plans      = readRDS(paste0(heavies.rds.path,"kkl.plans.rds")) 

landuse.unsimplified          = readRDS(paste0(heavies.rds.path,"landuse.unsimplified.rds"))
landuse                       = readRDS(paste0(heavies.rds.path,"landuse.simplified.rds"))
landuse_b                     = readRDS(paste0(heavies.rds.path,"landuse_b.rds"))
landuse_s                     = readRDS(paste0(heavies.rds.path,"landuse_s.rds"))
natreserve_no_firing          = readRDS(paste0(heavies.rds.path,"natreserve_no_firing.rds"))
natreserve_firing_intersect   = readRDS(paste0(heavies.rds.path,"natreserve_firing_intersect.rds"))
natpark_no_firing             = readRDS(paste0(heavies.rds.path,"natpark_no_firing.rds"))
natpark_firing_intersect      = readRDS(paste0(heavies.rds.path,"natpark_firing_intersect.rds"))
firing_no_natcons             = readRDS(paste0(heavies.rds.path,"firing_no_natcons.rds"))
firing_excl_dist_l            = readRDS(paste0(heavies.rds.path,"firing_excl_dist_l.rds"))
firing_excl_alllanduse        = readRDS(paste0(heavies.rds.path,"firing_excl_alllanduse.rds"))

bs.full  = readRDS("./rds_objects/bs.full.rds")    # b = beershebensis, s = surveys
bsp.full = readRDS("./rds_objects/bsp.full.rds")  # presences
bsa.full = readRDS("./rds_objects/bsa.full.rds")  # absences

bc.full  = readRDS("./rds_objects/bc.full.rds")   # b = beershebensis, c = collections

ss.full  = readRDS("./rds_objects/ss.full.rds")   # all schreiberi survey data (errors removed)
ssp.full = readRDS("./rds_objects/ssp.full.rds")  # presences
ssa.full = readRDS("./rds_objects/ssa.full.rds")  # absences

sc.full  = readRDS("./rds_objects/sc.full.rds")   # s = schreiberi, c = collections

#########################################################################################################################
# Plot layers ----

plot(b.preds, main = b.raster.list.names)
plot(s.preds, main = s.raster.list.names)

# Test for multicollinearity ----
# Get variance inflation Factor and test for multicollinearity:
# Zuur: Some statisticians suggest that VIF values higher then 5 or 10 are too high. In ecology vif larger than 3 is considered too much
vif(b.preds) # all ok now that i've removed the overlap of jantemps and DEM
vif(s.brick) # rain and soil are over 3, but still below 4.

vifcor(b.preds, th=0.9) # no collinearity problem (I already excluded collinear variables previously)
vifcor(s.preds, th=0.9) # no collinearity problem, though a slight issue between soil and rain

#vifstep(bs.brick, th=10, ...) # identify collinear variables that should be excluded
#vifstep(bs.brick, th=10, ...)

# Get local indicators of spatial association, for sensitivity to positional uncertainty ----

# LISA explanation ----
# The LISA function 'lisa(x,d1,d2,statistic,...)' can calculate different LISA statistics at each grid cell in Raster object. The statistics, implemented in this function, include local Moran’s I ("I"), local Geary’s c ("c"), local G and G* ("G" and "G*"), and local K1 statistics. This function returns standardized value (Z) for Moran, G and G*, and K1 statistics. If a SpatialPoints or a vector of numbers is deﬁned for y or cell, the LISA is calculated only for the speciﬁed locations by points or cells.

# this function calculates local indicators of spatial association for a continuous variable at each location in a raster layer, or a spatialpointsdataframe if specified.

# d1 = numeric lower bound of local distance (default=0), or an object of class neighbours created by dneigh when x is SpatialPoints or SpatialPolygons
# d2 = numeric upper bound of local distance, not needed if d1 is a neighbours object
# statistic = a character string specifying the LISA statistic that should be calculated. This can be one of "I" (or "localmoran" or "moran"), "c" (or "localgeary" or "geary"), "G" (or "localG"), "G*" (or "localG*")
# additional arguments include writing the output to a file, getting raw or standardized Moran's I statistic, longlat (logical, only when x is a Spatial* object specifies whether the coordinate system is geographic); drop (logical, only when x is a Spatial* object, if TRUE, the original data structure (Spatial* object) is returned, otherwise a numeric vector is returned)

# LISAs give a measure of correlation between a single location and its neighbours up to a specified distance (Getis and Ord 1996). It has been shown that spatial autocorrelation in predictors can be linked to SDM robustness (Naimi et al. 2011). For this purpose, using LISAs may be more insightful because it may lead to identification of the specific occurrence records that cause the largest drop in SDM performance.

# LISA in predictors at species occurrence locations

# Given a level of positional uncertainty (defined as a distance), this function calculates different statistics of local indicator of spatial association (LISA) in predictors (explanatory variables, defined as a raster object) at each species occurrence location (defined as a SpatialPoints object). According to Naimi et al. 2012, this can be used to understand whether positional uncertainty at which species locations are likely to affect predictive performance of species distribution models.

# the speciesLisa function uses the K statistic to measure the level of local spatial association at each location. The positive values imply low spatial association and the negative values high spatial association. Therefore, you can consider the locations with the positive values as the problematic locations.

speciesLisa(x, y, uncertainty, statistic="K1",weights)
# Arguments 
# x           = explanatory variables (predictors), defined as a raster object (RasterLayer or RasterStack or RasterBrick)
# y           = species occurrence points, defined as a SpatialPoints or SpatialPointsDataFrame object
# uncertainty = level of positional uncertainty, defined as a number (distance), in the units of the input data (e.g metres for ITM, proportions of a degree for WGS1984.)
# statistic   = a character string specifying the LISA statistic that should be calculated. This can be one of "I", "c", "G", "G*", and "K1". Default is "K1"
# weights     = a numeric vector specifying the relative importance of explanatory variables in species distribution models (the first value in the weights, is the importance of the first variable in x, etc ...). These values will be used as weights to aggregate the LISAs in predictors at each location and calculate a single measure. The length of weights should be equal to the number of raster layers in x

# Note: I will be using K statistic (a Geary-type statistic) as per Naimi et al 2014. Measures of spatial autocorrelation are either global or local, as reviewed by Getis (2010). This statistic was selected over more commonly used indicators such as local Moran's 'I' and local Geary's 'c' as the more common indicators are influenced by the presence of global statial autocorrelation (Geti and Ord 1996), and therefore can only be interpreted relative to the degree of spatial autocorrelation in the data, making them less suitable for assessing local spatial association between points, averaged over multiple layers with different degrees of global spatial autocorrelation. 

# Worked example:
file <- system.file("external/predictors.grd", package="usdm")
r <- brick(file) # reading a RasterBrick object including 4 rasters in the Netherlands
r 
plot(r) # visualize the raster layers
files.path <- system.file("external", package="usdm") # path to location of example files
require(rgdal)
sp <- readOGR(dsn=files.path,layer="species_nl") # reading species data (shapefile)
w = c(0.22,0.2,0.38,0.2)
splisa <- speciesLisa(x=r, y=sp, uncertainty=15000, weights=w)
splisa
plot(splisa)
bnd <- readOGR(dsn=files.path,layer="boundary") # reading the boundary map
plot(splisa)
plot(bnd, add=T)
# end of worked example

# Calculating LISA for Beershebensis ----
summary(bc)
mean(min(bc$lat), max(bc$lat)) # 30.77854 getting average latitude for converting uncertainty measures to uncertainty degrees
# as uncertainty is distance measured in the format of the input raster (here: degrees. in the examples: metres)
deg = 110865.64762119074 # from http://www.csgnetwork.com/degreelenllavcalc.html, 1 degree is equal to ~111,000 metres

# The following takes ages to run. objects saved at end, and retreivable with code that follows:
# w = c(0.1666667,0.1666667,0.1666667,0.1666667,0.1666667,0.1666667)
# bc.lisa100m   = speciesLisa(x= b.brick, y= bc, uncertainty= 100/deg,   statistic = "K1", weights = w )
# bc.lisa1000m  = speciesLisa(x= b.brick, y= bc, uncertainty= 1000/deg,  statistic = "K1", weights = w )
# bc.lisa10000m = speciesLisa(x= b.brick, y= bc, uncertainty= 10000/deg, statistic = "K1", weights = w )
# # save the objects:
# saveRDS(bc.lisa100m,   "bc.lisa100m.rds")
# saveRDS(bc.lisa1000m,  "bc.lisa1000m.rds")
# saveRDS(bc.lisa10000m, "bc.lisa10000m.rds")

bc.lisa100m          = readRDS("bc.lisa100m.rds")
bc.lisa1000m         = readRDS("bc.lisa1000m.rds")
bc.lisa10000m        = readRDS("bc.lisa10000m.rds")
bc$lisa.100 = bc.lisa100m@LISA
bc$lisa.1000 = bc.lisa1000m@LISA
bc$lisa.10000 = bc.lisa10000m@LISA
# Remeber: speciesLisa function uses K statistic. Positive/higher values imply low spatial association; negative/low values high spatial association. Therefore, you can consider the locations with the positive values as the problematic locations.

# plot LISA for high accuracy observations (<100 m error)
par(mar=c(2,1.5,2.5,1.5)) # sets the bottom, left, top and right margins respectively
hist(bc$lisa.100)

plot(bc.lisa100m,levels=c(-4,-2,0),  main= "Impact of positional uncertainty based on local indicators of spatial association")

png(filename = "bc.lisa.100mB.png", width = 22, height = 16, units = 'cm', res = 600)
plot(bc, pch=21, bg='red') # red will be top quartile of K values, ie low spatial association; 'problematic'
lines(borders, lty=5, lwd=2, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=22, col='black', bg='yellow')
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
points(bc[bc$lisa.100 < quantile(bc$lisa.100, 0.75),], bg='orange', pch=21) 
   # orange is 2nd-top quartile K, ie lowish autocorr, somewhat 'problematic'
points(bc[bc$lisa.100 < quantile(bc$lisa.100, 0.5),],  bg='blue', pch=21)   
   # blue is 2nd lowest quartile K, ie highish autocorr, not 'problematic'
points(bc[bc$lisa.100 < quantile(bc$lisa.100, 0.25),], bg='green', pch=21)  
   # green is lowest quartile K, highest autocorr, def not 'problematic'
title(main= "Local indicators of spatial association at 100 m radius")
legend("left", c("very low spatial association, K>-3.06", "low spatial association, K<-3.06",
                    "high spatial association, K<-3.22", "very high spatial association, K<-3.53"), 
       col=c("red","orange","blue","green"),pch=16, cex=0.8, text.font=1)
dev.off()
# frustrating issue with the legent placement! off, can't get it good.

# plot LISA for medium accuracy points (<1 km error)
hist(bc$lisa.1000)
plot(bc, pch=21, bg='red')                                            # red will be top quartile of K values, ie low spatial association; 'problematic'
lines(borders, lty=5, lwd=2, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=22, col='black', bg='yellow')
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))
points(bc[bc$lisa.1000 < quantile(bc$lisa.1000, 0.75),], bg='orange', pch=21) # orange is 2nd-top quartile K, ie lowish autocorr, somewhat 'problematic'
points(bc[bc$lisa.1000 < quantile(bc$lisa.1000, 0.5),],  bg='blue', pch=21)   # blue is 2nd lowest quartile K, ie highish autocorr, not 'problematic'
points(bc[bc$lisa.1000 < quantile(bc$lisa.1000, 0.25),], bg='green', pch=21)  # green is lowest quartile K, highest autocorr, def not 'problematic'
title(main= "Local indicators of spatial association at 1 km radius")
legend("bottomleft", c("very low spatial association", "low spatial association",
                       "high spatial association", "very high spatial association"), 
       col=c("red","orange","blue","green"),pch=16, cex=0.9, text.font=1)
# dev.copy(device = png, filename = "bc.lisa.1km.png", width = 22, height = 16, units = 'cm', res = 300); dev.off()

# plot LISA for low accuracy points (up to 10 km error)
hist(bc$lisa.10000)

plot(bc, pch=21, bg='red') # red will be top quartile of K values, ie low spatial association; 'problematic'
lines(borders, lty=5, lwd=2, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=22, col='black', bg='yellow')
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.8, offset=0.3))
points(bc[bc$lisa.10000 < quantile(bc$lisa.10000, 0.75),], bg='orange', pch=21) 
# orange is 2nd-top quartile K, lowish autocorr, somewhat 'problematic'
points(bc[bc$lisa.10000 < quantile(bc$lisa.10000, 0.5),],  bg='blue', pch=21)   # blue is 2nd lowest quartile K, ie highish autocorr, not 'problematic'
points(bc[bc$lisa.10000 < quantile(bc$lisa.10000, 0.25),], bg='green', pch=21)
title(main= "Local indicators of spatial association at 10 km radius")
legend("bottomright", c("very low spatial association", "low spatial association",
                       "high spatial association", "very high spatial association"), 
                      col=c("red","orange","blue","green"),pch=16, cex=0.9, text.font=1)
# dev.copy(device = png, filename = "bc.lisa.10km.png", width = 22, height = 16, units = 'cm', res = 300); dev.off()

# Calculating LISA for Schreiberi ----
summary(sc)
plot(sc, pch=21, bg='orange')
mean(31.13379, 33.09036) #31.13379 getting average latitude for converting uncertainty measures to uncertainty degrees
# as uncertainty is distance measured in the format of the input raster (here: degrees. in the examples: metres)
# put the result of the above mean into: http://www.csgnetwork.com/degreelenllavcalc.html, get length in metres.
sdeg = 110871.74786726199 # 1 degree is equal to ~111,000 metres. Getting exact value.

# The following takes ages to run. objects saved at end, and retreivable with code that follows:
# sw = c(0.1666667,0.1666667,0.1666667,0.1666667,0.1666667,0.1666667,0.1666667)
# sc.lisa100m   = speciesLisa( x=s.brick, y=sc, uncertainty= 100/sdeg,   statistic = "K1", weights = sw )
# sc.lisa1000m  = speciesLisa( x=s.brick, y=sc, uncertainty= 1000/sdeg,  statistic = "K1", weights = sw )
# sc.lisa10000m = speciesLisa( x=s.brick, y=sc, uncertainty= 10000/sdeg, statistic = "K1", weights = sw )
# 
# # save the objects:
# saveRDS(sc.lisa100m,   "sc.lisa100m.rds")
# saveRDS(sc.lisa1000m,  "sc.lisa1000m.rds")
# saveRDS(sc.lisa10000m, "sc.lisa10000m.rds")

sc.lisa100m            = readRDS("sc.lisa100m.rds")
sc.lisa1000m           = readRDS("sc.lisa1000m.rds")
sc.lisa10000m          = readRDS("sc.lisa10000m.rds")

# plot LISA for high accuracy Schreiberi observations (<100 m error)

# few lines to explore NAs. This issue has since been resolved.
sc$lisa.100 = sc.lisa100m@LISA 
sc$lisa.100                               # includes NAs. why?
sc$lisa.100[sc$lisa.100=='NA'] <- NA      # just reassigning the NAs in case they originally appeared as charachter strings.
sc.nas <- subset(sc,is.na(sc$lisa.100))   # creating a subset of the records that returned NA
plot(sc.nas, pch=21, bg='red', cex=1.5)   # plotting these...
lines(borders, lty=5, lwd=2, col="grey15")
lines(groads, col="grey73")
points(cities, pch=22, col='black', bg='yellow', cex = 0.5)
with(cities, text(cities$lat~cities$lon, labels = cities$name, pos = 4, cex=0.8))
# it looks like all three NAs are off the coast. The observation near Rosh Hanikra is off-coast; 475 m from closest bit of stable land.The one near Furedeis is in the sea, 554m from stable land. The one near Hadera is in the sea, 1300 m from land.
# end of NA issue

# back to plotting LISA for high-accuracy Schreiberi observations (<100 m error)
sc$lisa.100 = sc.lisa100m@LISA 
par(mar=c(2,1.5,2.5,1.5)) # sets the bottom, left, top and right margins respectively
hist(sc$lisa.100)

plot(sc, pch=21, bg='grey')
lines(borders, lty=5, lwd=1.5, col="grey15")
lines(groads, col="grey73")
plot(major.cities, pch=22, bg='yellow', cex=1.6, add=TRUE)
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.8, offset=0.3))
plot(sc, pch=21, bg='red', add=TRUE)
points(sc[sc$lisa.100 < quantile(sc$lisa.100, 0.75),], bg='orange', pch=21)
points(sc[sc$lisa.100 < quantile(sc$lisa.100, 0.5),],  bg='blue', pch=21)
points(sc[sc$lisa.100 < quantile(sc$lisa.100, 0.25),], bg='green', pch=21)
title(main= "Local indicators of spatial association at 100 m radius")
legend("topleft",inset=0.02, c("Very low spatial association", "Low spatial association",
                       "High spatial association", "Very high spatial association"), 
       col=c("red","orange","blue","green"),pch=16, cex=0.9, text.font=1)
dev.copy(device = png, filename = "sc.lisa.100mB.png", width = 15, height = 20, units = 'cm', res = 600); dev.off()
#order should be: high to low K values, ie low to high association; ie high to low risk: red, orange, blue, green

# plot LISA for medium accuracy Schreiberi observations (<1 km error)
sc$lisa.1000 = sc.lisa1000m@LISA
hist(sc$lisa.1000)

dev.new(width = 15, height = 20, unit = "cm")
plot(sc, pch=21, bg='grey', cex=0.1)
lines(borders, lty=5, lwd=1.6, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=22, col='black', bg='yellow', cex=1.6)
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.8, offset=0.3))
points(sc, bg='red', pch=21)
points(sc[sc$lisa.1000 < quantile(sc$lisa.1000, 0.75),], bg='orange', pch=21)
points(sc[sc$lisa.1000 < quantile(sc$lisa.1000, 0.5),],  bg='blue',   pch=21)
points(sc[sc$lisa.1000 < quantile(sc$lisa.1000, 0.25),], bg='green',  pch=21)
title(main= "Local indicators of spatial association at 1 km radius")
legend("topleft",inset=0.02, c("very low spatial association", "low spatial association",
                       "high spatial association", "very high spatial association"), 
       col=c("red","orange","blue","green"), pch=16, cex=0.85, text.font=1)
dev.copy(device = png, filename = "sc.lisa.1kmB.png", width = 15, height = 20, units = 'cm', res = 600); dev.off()

# plot LISA for low accuracy Schreiberi observations (up to 10 km error)
sc$lisa.10000 = sc.lisa10000m@LISA
hist(sc$lisa.10000)

plot(sc, pch=21, bg='grey', cex=0.1)
lines(borders, lty=5, lwd=1.6, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=22, col='black', bg='yellow')
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.8, offset=0.3))
points(sc, pch=21, bg='red')
points(sc[sc$lisa.10000 < quantile(sc$lisa.10000, 0.75),], bg='orange', pch=21)
points(sc[sc$lisa.10000 < quantile(sc$lisa.10000, 0.5),],  bg='blue', pch=21)
points(sc[sc$lisa.10000 < quantile(sc$lisa.10000, 0.25),], bg='green', pch=21)
title(main= "Local indicators of spatial association at 10 km radius")
legend("topleft", inset=0.02, c("Very high spatial association", "High spatial association",
                       "Low spatial association", "Very low spatial association"), 
       col=c("red","orange","blue","green"), pch=16, cex=0.9, text.font=1)
# dev.copy(device = png, filename = "sc.lisa.10km.png", width = 15, height = 20, units = 'cm', res = 600); dev.off()

#####
# Create Beershebensis subsets of data with only higher-LISA and/or higher certainty ----
# i.e. remove cases where LISA is below median and positional accuracy is low, for each level of uncertainty, as these are the points where spatial error will most impact SDMs. 

names(bc)
summary(bc$positional_accuracy)
# low accuracy means up to 10 km error
# medium accuracty means up to 1 km error
# high accuracy means up to 100 m error.

bc$positional_accuracy <- factor(bc$positional_accuracy, levels = c("low accuracy", "moderate accuracy", "high accuracy"))
par(mar=c(2,2,2.5,1.5)) # sets the bottom, left, top and right margins respectively

plot  (bc$lisa.1000,  bg=bc$positional_accuracy, col=bc$positional_accuracy, pch = 21)
legend("bottomright",inset=0.02, c("low accuracy", "moderate accuracy","high accuracy"), col=1:3, pch=16, cex=0.9, text.font=1)

# change quartile specs in the two locations in this next set, to reflect the proportional threshold I'm using to determine 'problematic'.
# remember, LISA values are K values; low value means high autocorrelation and therefore better reliability. High values are problematic.
bc.reliable.pos.highs     = subset(bc, positional_accuracy == "high accuracy") ; length(bc.reliable.pos.highs) # all 13 high accuracy included

bc.reliable.pos.moderates = subset(bc, bc$positional_accuracy == "moderate accuracy" & bc$lisa.1000 < quantile(bc$lisa.1000, 0.75))
length(bc[bc$positional_accuracy == "moderate accuracy",]); length(bc.reliable.pos.moderates)       # ie all 31 moderates are considered reliable

bc.reliable.pos.lows = subset(bc, bc$positional_accuracy == "low accuracy" & bc$lisa.10000 < quantile(bc$lisa.10000, 0.75))
length(bc.reliable.pos.lows); length(bc[bc$positional_accuracy=="low accuracy",]) # ie reliables 140/270 (threshold=median),or 199/270 (th= quart)

plot(bc.reliable.pos.lows, col='red'); points(bc.reliable.pos.moderates, col='green'); points(bc.reliable.pos.highs, col='blue')

bc.r = rbind(bc.reliable.pos.highs, bc.reliable.pos.moderates, bc.reliable.pos.lows)
length(bc.r); length(bc) 
points(bc.r, pch=21, col='black', cex=0.6)
# ie 112 out of 314 reliable if bottom half excluded; 238/314 if bottom quartile excluded.
saveRDS(bc.r, "b.collections.reliables.rds")
bc.r = readRDS("b.collections.reliables.rds")
bc.r$occurrence

# select the appropriate title and filename below, according to quartile selected above (0.5 = median; 0.25 = lower quartile excl)
plot(bc, pch=21, bg='purple') # purple shows all collections records
lines(borders, lty=5, lwd=2, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=22, col='black', bg='yellow')
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))
points(bc.r, bg='pink', pch=21)
legend("bottomright", c("Reliable record", "Unreliable record"), 
       col=c("purple","pink"),pch=16, cex=0.9, text.font=1)
# title(main= "Reliable and unreliable collections obs (above median K & low accuracy)")
title(main= "Reliable and unreliable collections obs (upper quartile K & low accuracy)")
# dev.copy(device = png, filename = "bc.reliables and unreliables (lower quartile LISA & low accuracy).png", width = 22, height = 16, units = 'cm', res = 600); dev.off()
# dev.copy(device = png, filename = "bc.reliables and unreliables (below median LISA & low accuracy).png", width = 22, height = 16, units = 'cm', res = 600); dev.off() 

#####
# Create Beershebensis subsets of reliable and questionable survey absence data ----
bs
names(bs)
summary(bsa$questionableX)
bsa.r = bsa[bsa$questionableX =="N",]
length(bsa.r); length(bsa)
saveRDS(bsa.r, "b.surveys.absence.reliables.rds")

#####
# Trying elsa package ----
library(devtools)
require(elsa) # for errors with this, refer to RTools installation and workaround in 'maintainR.R' script.

# worked example ----
file <- system.file('external/dem_example.grd',package='elsa')
r <- raster(file)

plot(r,main='a continuous raster map')

e <- elsa(r,d=2000,categorical=FALSE)

plot(e)
# end of worked example

# elsa for beershebensis predictors
options("scipen"=100, "digits"=4) # prevent scientific notation
deg = 110865.64762119074
uncertainty = 10000/110865.64762119074

bc.cells = cellFromXY(b.raster.list[[1]], bc)

e2 = elsa(b.raster.list[[1]], d=0.09019927, categorical = FALSE, cells = bc.cells) # this line takes forever to run
plot(e2)
Sys.time()

e3 = elsa(b.raster.list, d=0.09019927, categorical = FALSE, cells = bc.cells) # this line takes forever to run

