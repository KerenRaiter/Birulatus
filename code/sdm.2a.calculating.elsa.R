# Housekeeping and load required data ----

if("devtools" %in% rownames(installed.packages()) == FALSE) {install.packages("devtools")}; library(devtools)
if("elsa"  %in% rownames(installed.packages()) == FALSE) {install_github("babaknaimi/elsa")} ; library(elsa)
if("rgdal" %in% rownames(installed.packages()) == FALSE) {install.packages("rgdal")} ; library(rgdal)
if("beepr" %in% rownames(installed.packages()) == FALSE) {install.packages("beepr")} ; library(beepr)

x<-c("sdm","usdm","raster","rgdal","tidyverse","png","maptools","elsa","beepr")
lapply(x, require, character.only = TRUE)
installAll() # installs all the packages that 'sdm' relied on, if they aren't already installed.

# Install 'elsa' 
{
# 
# # 1.	You need to install RTools from https://cran.r-project.org/bin/windows/Rtools/ but this doesn’t solve the issue alone.
# # 2.	However, if you try to install it in R using install.packages("Rtools"), you may get an error saying “Warning in install.packages : package ‘Rtools’ is not available (for R version 3.5.2)”
# # 3.	I used a workaround from https://github.com/r-lib/devtools/issues/1772: 
# 
install.packages("devtools")                         # make sure you have the latest version from CRAN
 library(devtools)                                             # load package
 devtools::install_github("r-lib/pkgbuild", force = TRUE)  # install updated version of pkgbuild from GitHub
 library(pkgbuild)                                             # load package
 find_rtools()                                                    # should be TRUE, assuming you have Rtools 3.5
#
 # now installing elsa should work:
 install_github("babaknaimi/elsa")
}

# Set up heavies pathways
heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

# Install data
# bc.full             = readRDS("./rds_objects/bc.full.rds"); class(bc.full)
# b.preds             = readRDS("./rds_objects/b.preds.rds") 
# b_package_names     = readRDS("./rds_objects/b_package_names.rds")
# b.raster.list       = readRDS("./rds_objects/b.raster.list.rds")
# b.raster.list.names = list("Rain", "Jant", "Jult", "DEM","TWet", "Slop", "Soil")

# sc.full             = readRDS("./rds_objects/sc.full.rds")
# s.raster.list       = readRDS("./rds_objects/s.raster.list.rds")
# s.raster.list.names =  list("Rain", "Jant", "Jult", "Topo", "Slop", "Soil", "Vegt")

# border and reference info:
borders       = readRDS("./rds_objects/borders.rds")  
major.cities  = readRDS("./rds_objects/major.cities.rds")
groads        = readRDS("./rds_objects/groads.rds")

# memory.limit(30000)

#######################################################################################################################
# Datasets I prepared earlier in this script -----

sc.nodups = readRDS(paste0(heavies.rds.path,"sc.nodups.rds"))
sc.r      = readRDS(paste0(heavies.rds.path,"s.collections.reliables.rds"))

#######################################################################################################################
# Elsa calcs for beershebensis ----

b.elsas = list() # these are for high (10 km) uncertainty
deg = 110865.64762119074 # from http://www.csgnetwork.com/degreelenllavcalc.html, 1 degree is equal to ~111,000 metres
uncertainty = 10000/deg
Sys.time()

# The main loop:
for (i in 1:(length(b.raster.list-1))) { # length minus one as soil (the last one) is categorical; processed differently.
  start.time = Sys.time()
  print(b.raster.list.names[[i]])
  bc.cells = cellFromXY(b.raster.list[[i]], bc.full)
  b.elsas[[i]] = elsa(b.raster.list[[i]], d = uncertainty, categorical = FALSE, cells = bc.cells)
  print(paste(b.raster.list.names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes")); beep() 
  beep()   
  bc.full[[paste0(b.raster.list.names[[i]],".elsa")]] = b.elsas[[i]]   
  }
  
  # partial loop
  b.elsas = list() # these are for high (10 km) uncertainty
  deg = 110865.64762119074 # from http://www.csgnetwork.com/degreelenllavcalc.html, 1 degree is equal to ~111,000 metres
  uncertainty = 10000/deg
  Sys.time()
  for (i in c(2,3,5)) { # making up the missing ones
  start.time = Sys.time(); start.time
  print(b.raster.list.names[[i]]); filename = paste0(b.raster.list.names[[i]],".elsa.rds")
  bc.cells = cellFromXY(b.raster.list[[i]], bc.full)
  b.elsas[[i]] = elsa(b.raster.list[[i]], d = uncertainty, categorical = FALSE, cells = bc.cells) # now running
  saveRDS(b.elsas[[i]], filename)
  print(paste(b.raster.list.names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes")); beep() 
  bc.full[[paste0(b.raster.list.names[[i]],".elsa")]] = b.elsas[[i]]           }

  # one-off 'loop' for DEM (newly added at the time):
  i=4 # DEM is no. 4 in the raster list
  deg = 110865.64762119074 # from http://www.csgnetwork.com/degreelenllavcalc.html, 1 degree is equal to ~111,000 metres
  uncertainty = 10000/deg
  start.time = Sys.time(); start.time
  print(b.raster.list.names[[i]]) # this should match what's written this loop is for!
  bc.cells = cellFromXY(b.raster.list[[i]], bc.full)
  dem.elsa = elsa(b.raster.list[[i]], d = uncertainty, categorical = FALSE, cells = bc.cells)
  saveRDS(dem.elsa, "./rds_objects/dem.elsa.rds")
  bc.full[[paste0(b.raster.list.names[[i]],".elsa")]] = b.elsas[[i]]   
  names(bc.full)
  saveRDS(bc.full, "./rds_objects/bc.full.wdem.elsa.rds")
  print(paste(b.raster.list.names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes")); beep() 

  # special loop for slope (to examine elsa near hilly areas closely... and see if affected by WB inclusion)
  i=6 # slope is no. 6 in the raster list
  deg = 110865.64762119074 # from http://www.csgnetwork.com/degreelenllavcalc.html, 1 degree is equal to ~111,000 metres
  uncertainty = 10000/deg
  start.time = Sys.time(); start.time
  print(b.raster.list.names[[i]])
  bc.cells = cellFromXY(b.raster.list[[i]], bc.full)
  slope.elsa = elsa(b.raster.list[[i]], d = uncertainty, categorical = FALSE, cells = bc.cells)
  saveRDS(slope.elsa, "./rds_objects/slope.elsa.rds")
  print(paste(b.raster.list.names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes")); beep() 
  
  # special 'loop' for soil type (CATEGORICAL):
  i=7 # soil is no. 7 in the raster list
  deg = 110865.64762119074 # from http://www.csgnetwork.com/degreelenllavcalc.html, 1 degree is equal to ~111,000 metres
  uncertainty = 10000/deg
  start.time = Sys.time(); start.time
  print(b.raster.list.names[[i]])
  bc.cells = cellFromXY(b.raster.list[[i]], bc.full)
  soil.elsa = elsa(b.raster.list[[i]], d = uncertainty, categorical = TRUE, cells = bc.cells)
  saveRDS(soil.elsa, "./rds_objects/soil.elsa.rds")
  print(paste(b.raster.list.names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes")); beep() 

  # Extra special loop for soil with moderate uncertainty:
  i=7
  deg = 110865.64762119074 # from http://www.csgnetwork.com/degreelenllavcalc.html, 1 degree is equal to ~111,000 metres
  uncertainty = 10000/deg
  start.time = Sys.time(); start.time
  print(paste(b.raster.list.names[[i]],"moderate uncertainty"))
  bc.cells = cellFromXY(b.raster.list[[i]], bc.full)
  soil.mod.elsa = elsa(b.raster.list[[i]], d=0.009019927, categorical = TRUE, cells = bc.cells) 
  saveRDS(soil.mod.elsa, "./rds_objects/soil.mod.elsa.rds")
  
# saveRDS(b.elsas, "./rds_objects/b.elsas.rds")
# b.elsas = readRDS("./rds_objects/b.elsas.rds")
Sys.time() # each iteration took ~3 hours. Whole loop processed in ~27 hours. Might take till tomorrow late morning.

# # getting elsa values into appropriately named columns in points layer:
# for (i in 1:length(b.raster.list.names))                               {
#   bc.full[[paste0(b.raster.list.names[[i]],".elsa")]] = b.elsas[[i]]    }
# 
# # now doing the soil one separately as it is categorical:
# bc.cells = cellFromXY(b.raster.list[[i]], bc.full)
# bc.full$Soil.elsa = elsa(b.raster.list[[7]], d=0.09019927, categorical = TRUE, cells = bc.cells) 

# before going further, check how different each are. bc.full holds the previously-calculated elsa values.
# NOTE: i compared old and new versions of elsa values and deleted old version for conciseness sake, given only negligible differences.

# rain
i=1
b.raster.list.names[[i]]
rain.elsa = readRDS("./rds_objects/rain.elsa.rds")
rain.elsa
bc.full[[paste0(b.raster.list.names[[i]],".elsa")]] = rain.elsa; bc.full$Rain.elsa
hist(bc.full$Rain.elsa, main = b.raster.list.names[[i]])

# Jant
i=2
b.raster.list.names[[i]]
b.elsas[[i]]
bc.full[[paste0(b.raster.list.names[[i]],".elsa")]] = b.elsas[[i]]; bc.full$Jant.elsa
hist(b.elsas[[i]], main = b.raster.list.names[[i]])
bc.full$Jant.elsa.new = NULL

# Jult
i=3
b.raster.list.names[[i]]
b.elsas[[i]]
bc.full[[paste0(b.raster.list.names[[i]],".elsa")]] = b.elsas[[i]]; bc.full$Jult.elsa
hist(b.elsas[[i]], main = b.raster.list.names[[i]])
bc.full$Jult.elsa.new = NULL

# DEM
i=4
b.raster.list.names[[i]]
dem.elsa
bc.full[[paste0(b.raster.list.names[[i]],".elsa")]] = dem.elsa; bc.full$DEM.elsa
hist(bc.full$DEM.elsa, main = b.raster.list.names[[i]])
bc.full$dem.elsa = NULL

# Twet
i=5
b.raster.list.names[[i]]
bc.full$Topo.elsa = NULL # TWet was previously named 'topo' for 'topographic (wetness) index'.
b.elsas[[i]]
bc.full[[paste0(b.raster.list.names[[i]],".elsa")]] = b.elsas[[i]]; bc.full$TWet.elsa
hist(b.elsas[[i]], main = b.raster.list.names[[i]])
bc.full$Jult.elsa.new = NULL

# Slope
i=6
b.raster.list.names[[i]]
slope.elsa = readRDS("./rds_objects/slope.elsa.rds")
slope.elsa
bc.full[[paste0(b.raster.list.names[[i]],".elsa")]] = slope.elsa; bc.full$Slop.elsa
hist(bc.full$Slop.elsa, main = b.raster.list.names[[i]])
bc.full$Slop.elsa.new = NULL

# Soil
i=7
b.raster.list.names[[i]]
soil.elsa = readRDS("./rds_objects/soil.elsa.rds")
soil.elsa
bc.full[[paste0(b.raster.list.names[[i]],".elsa")]] = soil.elsa; bc.full$Soil.elsa
hist(bc.full$Soil.elsa, main = b.raster.list.names[[i]])
bc.full$soil.elsa.new = NULL

# Soil moderate
soil.mod.elsa = readRDS("./rds_objects/soil.mod.elsa.rds")
     bc.full$Soil.mod.elsa = soil.mod.elsa
hist(bc.full$Soil.mod.elsa)  

# save and export:
names(bc.full)
saveRDS(bc.full, "./rds_objects/bc.w.elsa.new.rds")
ogrDrivers()
writeOGR(obj = bc.full, dsn="E:/GIS working/layers/shnuniot", layer="bc_w_new_elsas", driver = "ESRI Shapefile")

# At this stage, elsa values for each predictor variable are examined in ArcGIS, in association with the relevant raster layers.
# visual examination was used to determine thresholds for ELSA values, based on the following rules:
# - elsa threshold is capped at 0.4 (threshold rule of thumb from Babak Naimi)
# - no more than 50% of data points can be excluded for predictor (ie threshold may not be lower than elsa value median)
# - no more than 50% of data points can be excluded overall 
# it is noted that for all variables moderate to drastic changes in predictor variables did exist within the 10km neighbourhood zone for most observation points; thus the selection of thresholds represented a compromise between 
# a) maximising model robustness by excluding observations that have both moderate to high levels of positional uncertainty AND where such uncertainty is likely to have large impacts on model outcomes (i.e. due to low levels of spatial association) from 'reliable' models; and
# b) limiting the degradation of model robustness by maximising the number of input data points, subject to (a).
# ELSA values are scaled from 0 to 1, and the distribution of values reflects the number of categories of the predictor variable (or the number into which the predictor variable has been binned, for continuous datasets), as well as the level of spatial autocorrelation in the predictor. 
# c) Visual examination of elsa values in ArcGIS 10.5.1 indicated that lower threshold values would be appropriate for the predictor variables that have lower spatial autocorrelation at the ~ 10km circumference neighbourhood scale (this may reflect the finding of Naimi et al 2019 that "The power of the test goes up when the level of spatial autocorrelation increases", p74). 

# Thus relatively high thresholds of 0.4 (based on Naimi rule of thumb) were selected for precipitation, Jant, and Jult;
# A moderate threshold of 0.1 was set for topographic wetness. While spatial autocorrelation exists in this predictor, it acts on much smaller scales than the existing level of positional uncertainty; At the 10 km scale, spatial autocorrelation is low and positional uncertainty is likely to strongly affect model accuracy (despite ELSA values being quite low). However, excluding all observations where positional uncertaintyis likely to affect accurate response curve formation for TWet would result in all observations being excluded. The threshold of 0.1 was selected as a compromise here. 
# relatively low thresholds of 0.02 were selected for DEM and slope. These were identified by visual examination to represent appropriate cutoff thresholds to reduce the variation in the underlying predictor within the zone of positional uncertainty (e.g. elevation differences of ~550m, slope differences of ~35 degrees) while minimising the overall number of observations excluded from the reliable dataset.
# a relatively high threshold of 0.4 was also selected for soil type as ELSA values for this predictor were high (indicating low spatial correlation) and setting the threshold any lower would have resulted in an excess of observations excluded.

par(mfrow=c(2,4), mar=c(2,1.5,1.5,1.5))
hist(bc.full$Rain.elsa, main = "rain elsa")
  length(subset(bc.full$Rain.elsa, bc.full$Rain.elsa > 0.4));  length(bc.full$Rain.elsa)  # elsa 0.4; 0 out.
hist(bc.full$Jant.elsa, main = "jant elsa")
  length(subset(bc.full$Jant.elsa, bc.full$Jant.elsa > 0.4)); length(bc.full$Jant.elsa)  # elsa 0.4; 0 out.
hist(bc.full$Jult.elsa, main = "jult elsa")
  length(subset(bc.full$Jult.elsa, bc.full$Jult.elsa > 0.4));  length(bc.full$Jult.elsa)  # elsa 0.02; 33 out.
hist(bc.full$DEM.elsa,      main = "DEM elsa")
  length(subset(bc.full$DEM.elsa,  bc.full$DEM.elsa  > 0.02)); length(bc.full$DEM.elsa)   # elsa 0.02; 55 out.
hist(bc.full$TWet.elsa,     main = "twet elsa")
  length(subset(bc.full$TWet.elsa, bc.full$TWet.elsa > 0.1 )); length(bc.full$TWet.elsa)  # elsa 0.02; 42 out.
hist(bc.full$Slop.elsa, main = "slope elsa")
  length(subset(bc.full$Slop.elsa, bc.full$Slop.elsa > 0.02)); length(bc.full$Slop.elsa) # elsa 0.02; 68 out.
hist(bc.full$soil.elsa, main = "soil elsa") 
  length(subset(bc.full$Soil.elsa, bc.full$Soil.elsa > 0.4)); length(bc.full$Soil.elsa) # 123, now down to 111.
hist(bc.full$Soil.mod.elsa, main = "soil mod")
  length(subset(bc.full$Soil.mod.elsa, bc.full$Soil.mod.elsa > 0.1));  length(bc.full$Soil.mod.elsa)  # 20 out of 314.

max(bc.full$Rain.elsa) # 0.0592717
max(bc.full$Jant.elsa) # 0.1054068
max(bc.full$Jult.elsa) # 0.06928181
max(bc.full$Topo.elsa) # 0.2090323
max(bc.full$Slop.elsa) # 0.08835533
max(bc.full$Soil.elsa) # 0.5011443 i.e. significant uncertainty
max(bc.full$Soil.elsa.mod)

points(bc[bc$lisa.100 < quantile(bc$lisa.100, 0.75),], bg='orange', pch=21) 

# # comparing the elsa calculation with factor as numerical versus categorical:
# hist(b.elsas[[7]]) # the calculation assuming soil was continuous factor
# hist(bc.full$Soil.elsa) # the calc with soil as categorical factor

# make histograms, calculate max elsa, and check for NAs: 
for (i in 1:length(b.raster.list.names))                                                                                          {
  par(mar=c(1.5,1.5,1.5,1.5), mfrow= c(1,1))
  png(filename = paste0("B.elsa.", b.raster.list.names[[i]],".histogram.png"))
  hist(bc.full[[paste0(b.raster.list.names[[i]],".elsa")]], 
       main = paste("Schreiberi Collections: ELSA histogram for",b.raster.list.names[[i]]))
  dev.off()
  
  print(paste("maximum elsa value for", b.raster.list.names[[i]],":", max(bc.full[[paste0(b.raster.list.names[[i]],".elsa")]])))
  
  print(paste(length(sc.full[is.na(sc.full[[paste0(s.raster.list.names[[i]],".elsa")]]),] ),"NAs for", s.raster.list.names[[i]]))  }

# plot map showing ELSA value of values:
png(filename = "obs 2b - Beersheb coll data - soil elsa.png", width = 18, height = 15, units = 'cm', res = 600)
par(mar=c(1.5,1.5,1.5,1.5))
plot  (bc.full, pch=16, cex=.5,
       main="Beershebensis collections observations, by soil ELSA", cex.main = 1, font.main=2, adj =0.5) 
# need to plot whole set before plotting subset, to ensure that all are included in map borders.
points(bc.full[bc.full$Soil.elsa >= 0.4,], col='red',  pch=16, cex=0.8)
points(bc.full[bc.full$Soil.elsa <  0.4,], col='green',pch=16, cex=0.8)
lines(borders, lty=5, lwd=2, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=21, col='black', bg='yellow')
with  (major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))
legend("bottomright", c("low spatial association", "moderate-high spatial association"), 
       col=c("red","green"), pch=16, cex=0.9, text.font=1)
dev.off()
#######################################################################################################################
# Beershebensis - consolidating reliability for all observations ----
# ELSA values: low value means high spatial autocorrelation and therefore better reliability. High values (eg>0.4) are problematic.

bc.full # 314 features
bc.no.duplicates = remove.duplicates(bc.full)
plot(bc.full, pch=21, bg='red', main="Beershebensis collections observations, by reliability (pos. acc. & ELSA)") # red will be unreliable
plot(bc.no.duplicates, add=T)
length(bc.no.duplicates)
names(bc.no.duplicates)
bc = bc.no.duplicates

table(bc$positional_accuracy)
bc$reliability = "reliable" # reliability = low accuracy + ELSA above threshold for any predictor

# Rain, Jant And Jult all have below-threshold ELSA values for all observations.
# DEM:
for(i in 1:length(bc))    {
  if(bc$DEM.elsa[i]  > 0.02 & bc$positional_accuracy[i] == "low accuracy") {bc$reliability[i] = "unreliable"}}

table(bc$reliability) # 20 unreliable vs 64 reliable
length(subset(bc$DEM.elsa, bc$DEM.elsa > 0.02 )) # 21 out, one musn't be low accuracy.

# TWet:
for(i in 1:length(bc))    {
  if(bc$TWet.elsa[i] >  0.1 & bc$positional_accuracy[i] == "low accuracy") {bc$reliability[i] = "unreliable"}}

table(bc$reliability) # 29 out (an additional 9)
length(subset(bc$TWet.elsa, bc$TWet.elsa > 0.1 )) # 11. So 2 were overlapping.

# Slope:
for(i in 1:length(bc))    {
  if(bc$Slop.elsa[i] > 0.05 & bc$positional_accuracy[i] == "low accuracy") {bc$reliability[i] = "unreliable"}}

table(bc$reliability) # 30 out (an additional 1)
length(subset(bc$Slop.elsa, bc$Slop.elsa > 0.02 )) # 21. So 20 were overlapping.

# Soil:
for(i in 1:length(bc))    {
  if(bc$Soil.elsa[i] >  0.4 & bc$positional_accuracy[i] == "low accuracy") {bc$reliability[i] = "unreliable"}}

table(bc$reliability) # 42 out (an additional 12)
length(subset(bc$Soil.elsa, bc$Soil.elsa > 0.4 )) # 25. So 13 were overlapping.

bc.reliables = subset(bc, reliability == "reliable"); length(bc.reliables) 

png(filename="./output_images/obs 2c - Beersheb coll data reliability map.png", width=22, height=16, units='cm', res=600)
par(mfrow=c(1,1), mar= c(2,2,2,2))
plot(bc, pch=21, bg='red', main="Beershebensis collections observations, by reliability (pos. acc. & ELSA)") # red will be unreliable
lines(borders, lty=2, lwd=3, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=22, col='black', bg='yellow', cex=1.4)
with  (major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))
points(bc.reliables, bg='deepskyblue2', pch=21) 
legend("bottomright", c("Reliable observations", "Unreliable observations"), pch=21, pt.bg=c("deepskyblue2","red"), cex=1, text.font=1)
dev.off()

saveRDS (bc.r, "b.collections.reliables.rds")
# bc.r = readRDS("b.collections.reliables.rds")
saveRDS(bc.full, "bc.full.rds")

#######################################################################################################################
# Elsa for schreiberi ----

# Elsa loop (takes ~3.5 hours to run)

# first remove duplicates, and check which rasters are continuous, which are categorical:
sc.nodups = remove.duplicates(sc.full) # 242 down to 94 points.
s.raster.list # first 5 are continuous, last 2 are categorical

s.elsas = list() ; print("start time"); Sys.time()

# Loop for continuous rasters:
for (i in 1:5) {
  start.time = Sys.time()
  print(s.raster.list.names[[i]])
  sc.cells = cellFromXY(s.raster.list[[i]], sc.nodups)
  s.elsas[[i]] = elsa(s.raster.list[[i]], d= 0.09019927, categorical = FALSE, cells = sc.cells)
  print(paste(s.raster.list.names[i],"loop took", difftime(Sys.time(), start.time, units="mins"), "minutes"))    }

# Loop for categorical rasters (numbers 6-7):
for (i in 6:7) {
  start.time = Sys.time()
  print(s.raster.list.names[[i]])
  sc.cells = cellFromXY(s.raster.list[[i]], sc.nodups)
  s.elsas[[i]] = elsa(s.raster.list[[i]], d= 0.09019927, categorical = TRUE, cells = sc.cells)
  print(paste(s.raster.list.names[i],"loop took", difftime(Sys.time(), start.time, units="mins"), "minutes"))}

s.elsas
saveRDS(s.elsas, paste0(heavies.rds.path,"s.elsas.rds"))

# extra ELSA calc for soil with 5k instead of 10km uncertainty (as too many were excluded under 10km uncertainty)
for (i in 6:6) {
  start.time = Sys.time()
  print(s.raster.list.names[[i]])
  sc.cells = cellFromXY(s.raster.list[[i]], sc.nodups)
  soil.elsa.5km = elsa(s.raster.list[[i]], d= 0.04509963, categorical = TRUE, cells = sc.cells)
  print(paste(s.raster.list.names[i],"loop took", difftime(Sys.time(), start.time, units="mins"), "minutes"))}
hist(soil.elsa.5km)
sc.nodups@data$Soil.elsa.5km = soil.elsa.5km

summary(sc.nodups)
# getting s.elsa values into appropriately named columns in points layer:
for (i in 1:length(s.raster.list.names))                          {
 sc.nodups[[paste0(s.raster.list.names[[i]],".elsa")]] = s.elsas[[i]]    }

Sys.time() # each iteration took 0.5 hours. Whole loop processed in 3.5 hours.
beep()

# make histograms, calculate max elsa, and check for NAs: 
for (i in 1:length(s.raster.list.names))                          {
  
  png(filename = paste0("./output_images/S.elsa.",s.raster.list.names[[i]],".histogram.png"))
  #par(mar=c(2,2,2,2), mfrow= c(1,1))
  hist(sc.nodups[[paste0(s.raster.list.names[[i]],".elsa")]], 
       main = paste("Schreiberi Collections: ELSA histogram for",s.raster.list.names[[i]]),
       xlab = "ELSA value")
  dev.off()
  
  print(paste("maximum elsa value for", s.raster.list.names[[i]],":",
              max(sc.nodups[[paste0(s.raster.list.names[[i]],".elsa")]], na.rm = T)))
  
  print(paste(length(sc.full[is.na(sc.nodups[[paste0(s.raster.list.names[[i]],".elsa")]]),] ),"NAs for", s.raster.list.names[[i]]))
                                                                   } # high elsas with TWet, Soil, & Vegt

# # plotting the NAs (now fixed, so obsolete):
{
# par(mar=c(1.5,1.5,1.5,1.5), mfrow= c(1,1))
# plot  (sc.full, pch=16, cex=.3,
#        main="Schreiberi collections observations, by soil ELSA", cex.main = 1, font.main=2, adj =0.5) 
# points(sc.full[is.na(sc.full$Rain.elsa),], col='blue',  pch=16, cex=0.8); sc.full[is.na(sc.full$Rain.elsa),]
# points(sc.full[is.na(sc.full$Jant.elsa),], col='light blue',  pch=16, cex=0.8)
# points(sc.full[is.na(sc.full$Jult.elsa),], col='orange',  pch=16, cex=0.8)
# points(sc.full[is.na(sc.full$Topo.elsa),], col='red',  pch=16, cex=0.8)
# points(sc.full[is.na(sc.full$Slop.elsa),], col='yellow',  pch=16, cex=0.8)
# points(sc.full[is.na(sc.full$Soil.elsa),], col='brown',  pch=16, cex=0.8)
# points(sc.full[is.na(sc.full$Vegt.elsa),], col='green',  pch=16, cex=0.8)
# all na's were due to points not overlapping with available predictor variable raster extents.
# I've now fixed these by moving the points slightly to within raster extents (in ArcGIS) 
}

# plot map showing ELSA values for TWet:
png(filename="./output_images/obs 4b - Schreiberi collections by TWet elsa.png", width=12, height=15, units='cm', res=600)
par(mar=c(1.5,1.5,1.5,1.5))
plot  (sc.nodups, pch=16, cex=.1,
       main="Schreiberi collections observations, by TWet ELSA", cex.main = 1, font.main=2, adj = 0.5) 
# need to plot whole set before plotting subset, to ensure that all are included in map borders.
points(sc.nodups[sc.nodups$TWet.elsa >= 0.4,], col='red',  pch=16, cex=0.8)
points(sc.nodups[sc.nodups$TWet.elsa <  0.4,], col='green',pch=16, cex=0.8)
lines(borders, lty=5, lwd=2, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=21, col='black', bg='yellow')
with  (major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.8, offset=0.3))
legend("bottomright", c("High ELSA: problematic for low accuracy (5)","Low ELSA: not problematic for low accuracy (89)"), 
       col=c("red","green"), pch=16, cex=0.6, text.font=1)
dev.off()

# plot map showing ELSA values for soil:
png(filename="./output_images/obs 4b - Schreiberi collections by soil elsa.png", width=12, height=15, units='cm',res=600)
par(mar=c(1.5,1.5,1.5,1.5))
plot  (sc.nodups, pch=16, cex=.1,
       main="Schreiberi collections observations, by soil ELSA", cex.main = 1, font.main=2, adj = 0.5) 
# need to plot whole set before plotting subset, to ensure that all are included in map borders.
points(sc.nodups[sc.nodups$Soil.elsa >= 0.4,], col='red',  pch=16, cex=0.8)
points(sc.nodups[sc.nodups$Soil.elsa <  0.4,], col='green',pch=16, cex=0.8)
lines(borders, lty=5, lwd=2, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=21, col='black', bg='yellow')
with  (major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.8, offset=0.3))
legend("bottomright", c("High ELSA: problematic for low accuracy (59)","Low ELSA: not problematic for low accuracy (35)"), 
       col=c("red","green"), pch=16, cex=0.6, text.font=1)
dev.off()

# plot map showing ELSA values for soil with a revised 5 km uncertainty buffer:
png(filename="./output_images/obs 4b - Schreiberi collections by soil elsa (5km uncertainty).png", 
    width=12, height=15, units='cm', res=600)
par(mar=c(1.5,1.5,1.5,1.5))
plot  (sc.nodups, pch=16, cex=.1,
       main="Schreiberi collections observations, by soil ELSA", cex.main = 1, font.main=2, adj = 0.5) 
# need to plot whole set before plotting subset, to ensure that all are included in map borders.
points(sc.nodups[sc.nodups$Soil.elsa.5km >= 0.4,], col='red',  pch=16, cex=0.8)
points(sc.nodups[sc.nodups$Soil.elsa.5km <  0.4,], col='green',pch=16, cex=0.8)
lines(borders, lty=5, lwd=2, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=21, col='black', bg='yellow')
with  (major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.8, offset=0.3))
legend("bottomright", c("High ELSA: problematic for low accuracy (18)","Low ELSA: not problematic for low accuracy (76)"), 
       col=c("red","green"), pch=16, cex=0.6, text.font=1)
dev.off()

# plot map showing ELSA values for Vegt:
png(filename="./output_images/obs 4b - Schreiberi collections by Vegt elsa.png", width=12, height=15, units='cm',res=600)
par(mar=c(1.5,1.5,1.5,1.5))
plot  (sc.nodups, pch=16, cex=.1,
       main="Schreiberi collections observations, by Vegt ELSA", cex.main = 1, font.main=2, adj = 0.5) 
# need to plot whole set before plotting subset, to ensure that all are included in map borders.
points(sc.nodups[sc.nodups$Vegt.elsa >= 0.4,], col='red',  pch=16, cex=0.8)
points(sc.nodups[sc.nodups$Vegt.elsa <  0.4,], col='green',pch=16, cex=0.8)
lines(borders, lty=5, lwd=2, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=21, col='black', bg='yellow')
with  (major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.8, offset=0.3))
legend("bottomright", c("High ELSA: problematic for low accuracy (4)","Low ELSA: not problematic for low accuracy (90)"), 
       col=c("red","green"), pch=16, cex=0.6, text.font=1)
dev.off()

# looping attempt at plotting map. Abandoned for now.
# letter = list("a","b","c","d","e","f","g")
# install.packages("spatialEco")
# library(spatialEco)
# names(sc.nodups)
# for (i in 1:length(s.raster.list.names))    {
#   par(mar=c(1.5,1.5,1.5,1.5), mfrow= c(2,4))
#   points = sp.na.omit(sc.nodups, col.name = paste0(s.raster.list.names[[i]],".elsa"), margin = 1) # good!
#   png(filename = paste0("observations 4(",letter[[i]],") Schreiberi coll by elsa - ",s.raster.list.names[[i]],".png"), 
#                       width = 15, height = 20, units = 'cm', res = 600)
#   plot(points, pch=16, cex=.3,
#        main= paste0("Schreiberi collections by ELSA:", s.raster.list.names[[i]]), cex.main = 1, font.main=2, adj =0.5) 
#   points(sc.nodups[sc.nodups$Soil.elsa >= 0.4,], col='red',  pch=16, cex=0.8)
#   points(points[points[[paste0(s.raster.list.names[[i]],".elsa")]] >= 0.4,], col='red',  pch=16, cex=0.8)
#   points(points[points[[paste0(s.raster.list.names[[i]],".elsa")]] <  0.4,], col='green',pch=16, cex=0.8)
#   lines(borders, lty=5, lwd=2, col="grey15")
#   lines(groads, col="grey73")
#   points(major.cities, pch=21, col='black', bg='yellow')
#   with  (major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
#   dev.off()                                 }

saveRDS(sc.nodups, "sc.nodups.rds")

#######################################################################################################################
# Subsetting Schreiberi by elsa and positional accuracy ----
# ELSA values: low value means high autocorrelation and therefore better reliability. High values (>0.4) are problematic.

length(sc.nodups)
names(sc.nodups)
s.raster.list.names

table(sc.nodups$positional_accuracy)
sc.nodups$reliability = "reliable" # reliability = low accuracy + ELSA above threshold for any predictor

# Rain:
for(i in 1:length(sc.nodups))    {
  if(sc.nodups$Rain.elsa[i]  > 0.1 & sc.nodups$positional_accuracy[i] == "low accuracy") {sc.nodups$reliability[i] = "unreliable"}}

table(sc.nodups$reliability) #  unreliable vs  reliable
length(subset(sc.nodups$Rain.elsa, sc.nodups$Rain.elsa > 0.1 )) #  0 out by rain.

# Jant:
for(i in 1:length(sc.nodups))    {
  if(sc.nodups$Jant.elsa[i] >  0.1 & sc.nodups$positional_accuracy[i] == "low accuracy") {sc.nodups$reliability[i] = "unreliable"}}

table(sc.nodups$reliability) #  unreliable vs  reliable
length(subset(sc.nodups$Jant.elsa, sc.nodups$Jant.elsa > 0.1 )) #  2 out by Jant.

# Jult:
for(i in 1:length(sc.nodups))    {
  if(sc.nodups$Jult.elsa[i] >  0.1 & sc.nodups$positional_accuracy[i] == "low accuracy") {sc.nodups$reliability[i] = "unreliable"}}

table(sc.nodups$reliability) #  unreliable vs  reliable
length(subset(sc.nodups$Jult.elsa, sc.nodups$Jult.elsa > 0.1 )) #  0 out by Jult.

# TWet:
for(i in 1:length(sc.nodups))    {
  if(sc.nodups$TWet.elsa[i] >  0.4 & sc.nodups$positional_accuracy[i] == "low accuracy") {sc.nodups$reliability[i] = "unreliable"}}

table(sc.nodups$reliability) #  unreliable vs  reliable
length(subset(sc.nodups$TWet.elsa, sc.nodups$TWet.elsa > 0.4 )) #  5 out by TWet.

# Slope:
for(i in 1:length(sc.nodups))    {
  if(sc.nodups$Slop.elsa[i] > 0.1 & sc.nodups$positional_accuracy[i] == "low accuracy") {sc.nodups$reliability[i] = "unreliable"}}

table(sc.nodups$reliability) #  unreliable vs  reliable
length(subset(sc.nodups$Slop.elsa, sc.nodups$Slop.elsa > 0.1 )) #  9 out by slope, 4 additional.

# Soil:
for(i in 1:length(sc.nodups))    {
  if(sc.nodups$Soil.elsa[i] >  0.4 & sc.nodups$positional_accuracy[i] == "low accuracy") {sc.nodups$reliability[i] = "unreliable"}}

table(sc.nodups$reliability) #  64 reliable vs 30 unreliable   
length(subset(sc.nodups$Soil.elsa, sc.nodups$Soil.elsa > 0.4 )) #  59 out by soil, additional 19 (not all could be low accuracy).

# Vegt:
for(i in 1:length(sc.nodups))    {
  if(sc.nodups$Vegt.elsa[i] >  0.4 & sc.nodups$positional_accuracy[i] == "low accuracy") {sc.nodups$reliability[i] = "unreliable"}}

table(sc.nodups$reliability) #  unreliable vs  reliable
length(subset(sc.nodups$Vegt.elsa, sc.nodups$Vegt.elsa > 0.4 )) #  4 out by vegt, none additional.

sc.r = subset(sc.nodups, reliability == "reliable"); length(sc.r) 

png(filename = "./output_images/obs 4c - Schreiberi coll. data reliability map.png", width=6, height=12, units='cm', res=600)
par(mar=c(0.5,1,1,1), mfrow=c(1,1))
plot(sc.nodups, pch=21, bg='red', cex=0.1,
     main="Schreiberi collections records, by reliability (pos. acc. & ELSA)",  cex.main=0.4)
lines(borders, lty=2, lwd=1.5, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=22, col='black', bg='yellow', cex=1)
with  (major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.5, offset=0.3))
points(sc.nodups, col = 'red',          pch=16, cex=0.6)
points(sc.r,      col = 'deepskyblue2', pch=16, cex=0.6) 
legend("topleft", c("Reliable observations (63)", "Unreliable observations (30)"), pch=16, col=c("deepskyblue2","red"), cex=0.5)
dev.off()

# note: if want points to have outlines, use pch=21, put the colour as the bg, and in legend as pt.bg=c("deepskyblue2","red")

# additional lines to remove doubtful point identified late in the journey (in beershebensis territory):
sc.nodups <- sc.nodups [ which ( sc.nodups$locality != "Nahal Habsor" | is.na(sc.nodups$locality )), ]
sc.r <- sc.r [ which ( sc.r$locality != "Nahal Habsor" | is.na(sc.r$locality )), ]

saveRDS(sc.nodups, paste0(heavies.rds.path,"sc.nodups.rds"))
saveRDS(sc.r,      paste0(heavies.rds.path,"s.collections.reliables.rds"))

