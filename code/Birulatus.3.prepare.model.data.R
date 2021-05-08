# sdm.3.prepare data for modelling ----

# There are three main functions provide the main steps of developing/using species distibution models. 
# The three steps include data preparation, model ﬁtting and evaluation, and prediction. 
# The functions used for these steps:
# • sdmData: to read data from different formats, and prepare a data object. Both species (single or multiple) and 
# explanatory variables can be introduced to the function, as well as other information such as spatial coordinates, 
# time, grouping variables, etc.
# • sdm: to ﬁt and evaluate species distribution models (multiple algorithms can be used)
# • predict: when models are ﬁtted, they can be used to predict/project given a new dataset.

#########################################################################################################
# Set up and install relevant packages and locations ----

# ipak function: install (if not already installed) and load multiple R packages.
ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)}
ipak(c("rgdal","stringr","usdm", "biomod2","raster","scales", "grid", "foreign","dplyr","magrittr","tidyr","rgeos",
       "magrittr","ggplot2","gridExtra","raster","rasterVis","dismo","sdm","installr","knitr","ggmap",
       "OpenStreetMap","parallel","beepr","rmapshaper", "spatialEco", "rJava")) # removed sf, may be causing problems
# installed.packages()
installAll() # installing everything the sdm relies on.

# setting up functions to check heaviest items on memory:
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# step 2
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos()

# Birulatus heavies will be on E drive (at least for now), with E drive being backed up to HUJI server regularly
B.heavies.spatial.path = 'E:/R/Birulatus_heavies/spatial/'
B.heavies.rds.path     = 'E:/R/Birulatus_heavies/rds/'
B.heavies.image.path   = 'E:/R/Birulatus_heavies/images/'

#B.heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
#B.heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
#B.heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

ITM = CRS("+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.33077,-1.85269,1.66969,5.4248 +units=m +no_defs")

#######################################################################################################################
# Datasets prepared earlier ----

# from this script:

package_names          = readRDS("rds/package_names.rds")
combo.descriptions     = readRDS("rds/combo.descriptions.rds")
obs_packages           = readRDS("rds/obs_packages.rds")
data_packages          = readRDS("rds/data_packages.rds")

# from previous scripts:
borders          = readRDS("rds/borders.rds");            israel.WB = readRDS("rds/israel.WB.rds")
israel.WB.merged = readRDS("./rds/israel.WB.merged.rds"); israel.WB = readRDS("./rds/israel.WB.no.water.rds")
israel.noWB      = readRDS("rds/israel.noWB.rds")

birulatus.study  = readRDS("rds/birulatus.study.rds") # study area
xlims = c(35.14081, 35.86214)
ylims = c(31.59407, 33.00496)

major.cities = readRDS("./rds/major.cities.rds")
small.cities = readRDS("./rds/small.cities.rds")
large.towns  = readRDS("./rds/large.towns.rds")
towns        = readRDS("./rds/towns.rds")
villages     = readRDS("./rds/villages.rds")

groads       = readRDS("./rds/groads.rds")

raster.list         = readRDS(paste0(B.heavies.rds.path,"raster.list.rds"))
raster.list.names   = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Lith")
preds               = readRDS(paste0(B.heavies.rds.path, "preds.rds")) # raster stack
preds.l.nocoll      = readRDS(paste0(B.heavies.rds.path, "preds.l.nocoll.rds")) # raster stack, collinear vars excl.

bi.raw = readRDS("./rds/bi.raw.rds") 
bi     = readRDS("./rds/bi.rds")   # full study area (for modelling with lithology layer)
bip    = readRDS("./rds/bip.rds")  # presences
bia    = readRDS("./rds/bia.rds")  # absences

bi.s     = readRDS("./rds/bi.s.rds")  # soil-delimited study area, for modelling with soils layer (misses part Golan)
bip.s    = readRDS("./rds/bip.s.rds") # presences
bia.s    = readRDS("./rds/bia.s.rds") # absences

# legacy :
# b_package_names         = readRDS("./rds_objects/b_package_names.rds")
# b.combo.descriptions    = readRDS("./rds_objects/b.combo.descriptions.rds")
# obs_packages            = readRDS("./rds_objects/obs_packages.rds")
# b_data_packages         = readRDS("./rds_objects/b_data_packages.rds")

#########################################################################################################
# Prepare observational data for input into data packages ----

points.list        = list(bip, bia) 
points.short.names = list("bip","bia")
points.full.names  = list("bip  = Birulatus presence data", "bia  = Birulatus absence data")

for (p in 1:length(points.list)) {
  print(points.full.names[[p]])                           # print name of dataset to start us off...
  print(class(points.list[[p]]))                          # see class. should be "SpatialPointsDataFrame"
  print(length(points.list[[p]]))                         # how many data points in this dataset?
  points.list[[p]]@coords = points.list[[p]]@coords[,1:2] # Remove third (Z) coordinate if present
  print(summary(points.list[[p]]@coords))                 # check that now there are only two coords for each dataset
  print(str(points.list[[p]]$occurrence))                 # ensure occurrence is recorded consistently: 1's or 0's.
  print(summary(points.list[[p]]$occurrence))             # again, ensure occurrence is recorded consistently
  points.list[[p]]@data <- points.list[[p]]@data[,'occurrence', drop=F] # dropping all attributes other than occurrence
  print(names(points.list[[p]]))                                        # check that only "occurrence" remains
} # check in output that there are just two coords (x1 and x2), and the expected number of features. 

# convert back to original names:
bip   = points.list[[1]]
bia   = points.list[[2]]

saveRDS(bip,         "./rds/bip.rds")
saveRDS(bia,         "./rds/bia.rds")

#########################################################################################################
# Create data packages for each combo -----

# Data combination descriptions
combo.descriptions = list("A: Study area based on lithology")

obs_A = rbind(bip, bia)
# obs_B = rbind(bsp, bsa.r)
# obs_C = rbind(bs) 
# obs_D = rbind(bsp, bsa.r, bc.r)
# obs_E = rbind(bsp, bsa.r, bc)
# obs_F = rbind(bsp, bsa, bc.r)
# obs_G = bsp
# obs_H = rbind(bsp, bc.r)
# obs_I = rbind(bsp, bc)

obs_packages  = list(obs_A)        # obs_B, obs_C, obs_D, obs_E, obs_F, obs_G, obs_H, obs_I)
scenario.names = list('Scenario A: lithology & study area')
data_packages = list()
remove.duplicates(obs_A) # no duplicates

for (d in 1:length(scenario.names)) {
  data_packages[[d]] = sdmData(formula = occurrence ~ ., train = obs_packages[[d]], predictors = preds.l.nocoll)
  print("------------"); print(scenario.names[[d]]); print(data_packages[[d]])   
  print(length(obs_packages[[d]]))   } # 181 observations (none dropped from obs package number, )

# A note on the difference in record numbers: the data_packages function automatically removes spatial duplictes. 
# to see how many duplicate-free points there are in original data set, use remove.duplicates() as above)

# main output objects from this section:
saveRDS(scenario.names,        "./rds/scenario.names.rds")
saveRDS(obs_packages,          "./rds/obs_packages.rds")
saveRDS(data_packages,         "./rds/data_packages.rds")

#########################################################################################################