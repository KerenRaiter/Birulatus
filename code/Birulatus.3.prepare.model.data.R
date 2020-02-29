# sdm.3.prepare data for modelling ----

# There are three main functions provide the main steps of developing/using species distibution models. 
# The three steps include data preparation, model ﬁtting and evaluation, and prediction. 
# The functions used for these steps:
# • sdmData: to read data from diﬀerent formats, and prepare a data object. Both species (single or multiple) and 
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
B.heavies.spatial.path = 'E:/R/Birulatus_heavies/image/'
B.heavies.rds.path     = 'E:/R/Birulatus_heavies/rds/'
B.heavies.image.path   = 'E:/R/Birulatus_heavies/images/'

#B.heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
#B.heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
#B.heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

ITM = CRS("+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.33077,-1.85269,1.66969,5.4248 +units=m +no_defs")

#######################################################################################################################
# Datasets I prepared earlier in this script ----

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

raster.list       = readRDS(paste0(B.heavies.rds.path,"raster.list.rds"))
raster.list.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Lith")
preds             = readRDS(paste0(B.heavies.rds.path,"preds.rds")) # raster stack

bi.raw = readRDS("./rds/bi.raw.rds") 
bi     = readRDS("./rds/bi.rds") 
bip    = readRDS("./rds/bip.rds")
bia    = readRDS("./rds/bia.rds")



# legacy :
b_package_names         = readRDS("./rds_objects/b_package_names.rds")
b.combo.descriptions    = readRDS("./rds_objects/b.combo.descriptions.rds")
obs_packages            = readRDS("./rds_objects/obs_packages.rds")
b_data_packages         = readRDS("./rds_objects/b_data_packages.rds")

ssp  = readRDS ("./rds_objects/ssp.rds")
ssa  = readRDS ("./rds_objects/ssa.rds")
sc   = readRDS ("./rds_objects/sc.rds")
sc.r = readRDS ("./rds_objects/sc.r.rds")

s.6scen.scen.names             = readRDS("./rds_objects/s.6scen.scen.names.rds")
s.6scenario.descriptions       = readRDS("./rds_objects/s.6scenario.descriptions.rds")
s.6scen.obs.packages           = readRDS("./rds_objects/s.6scen.obs.packages.rds")
s.6scen.data.packages          = readRDS("./rds_objects/s.6scen.data.packages.rds")

#########################################################################################################
# Prepare observational data for input into data packages ----

points.list = list(bip,bia) 
points.short.names = list("bip","bia")
points.full.names = list("bip  = Birulatus presence data", "bia  = Birulatus absence data")

for (p in 1:length(points.list)) {
  print(points.full.names[[p]])                         # print name of dataset to start us off...
  print(class(points.list[[p]]))                          # see class. should be "SpatialPointsDataFrame"
  print(length(points.list[[p]]))                         # how many data points in this dataset?
  points.list[[p]]@coords = points.list[[p]]@coords[,1:2] # Remove third (Z) coordinate if present
  print(summary(points.list[[p]]@coords))                 # check that now there are only two coords for each dataset
  print(str(points.list[[p]]$occurrence))                 # ensure occurrence is recorded consistently: 1's or 0's.
  print(summary(points.list[[p]]$occurrence))             # again, ensure occurrence is recorded consistently
  points.list[[p]]@data <- points.list[[p]]@data[,'occurrence', drop=F] # dropping all attributes other than occurrence
  print(names(points.list[[p]]))                                        # check that only "occurrence" remains
}

# output should look like this:
{ 
# [1] "bc = beershebensis collections presences"
# [1] "SpatialPointsDataFrame"
# attr(,"package")
# [1] "sp"
# [1] 314
# coords.x1       coords.x2    
# Min.   :34.39   Min.   :30.78  
# 1st Qu.:34.79   1st Qu.:31.06  
# Median :34.84   Median :31.21  
# Mean   :34.90   Mean   :31.15  
# 3rd Qu.:35.02   3rd Qu.:31.26  
# Max.   :35.28   Max.   :31.45  
# num [1:314] 1 1 1 1 1 1 1 1 1 1 ...
# NULL
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1       1       1       1       1       1 
# [1] "occurrence"
}

bip   = points.list[[1]]
bia   = points.list[[2]]

saveRDS(bip,         "./rds_objects/bip.rds")
saveRDS(bsa,         "./rds_objects/bsa.rds")

#########################################################################################################
# Create data packages for each combo: Beershebensis -----

# Data combination descriptions
combo.descriptions = list("A: All data equal")

obs_A = rbind(bip, bia)
# obs_B = rbind(bsp, bsa.r)
# obs_C = rbind(bs) 
# obs_D = rbind(bsp, bsa.r, bc.r)
# obs_E = rbind(bsp, bsa.r, bc)
# obs_F = rbind(bsp, bsa, bc.r)
# obs_G = bsp
# obs_H = rbind(bsp, bc.r)
# obs_I = rbind(bsp, bc)

obs_packages  = list(obs_A)
                     #obs_B, obs_C, obs_D, obs_E, obs_F, obs_G, obs_H, obs_I)
package_names = list('Combination A')
data_packages = list()
unique(obs_A)

for (d in 1:length(package_names)) {
  data_packages[[d]] = sdmData(formula = occurrence ~ ., train = obs_packages[[d]], predictors = preds)
  print("------------"); print(package_names[[d]]); print(data_packages[[d]])   
  print(length(obs_packages[[d]]))   } # 181 observations (none dropped from obs package number, )



# # A note on the difference in data numbers between input datasets, and data packages:
{
# length(obs_packages[[1]])     # 286 observations
# b_data_packages[[1]]          # 278 observations
# # Convert dataset to tibble and use tidyverse to remove duplictes:
# obs_A_tibble <- as_tibble(obs_packages[[1]])
# obs_A_tibble.unique = obs_A_tibble %>% distinct(coords.x1, coords.x2, .keep_all = TRUE)
# obs_A_tibble.unique # yes this comes up with 279 unique observations! Nice to know that sdm package removes location duplicates automatically.
}

# main output objects from this section:
saveRDS(b_package_names,      "./rds_objects/b_package_names.rds")
saveRDS(b.combo.descriptions, "./rds_objects/b.combo.descriptions.rds")
saveRDS(obs_packages,         "./rds_objects/obs_packages.rds")
saveRDS(b_data_packages,      "./rds_objects/b_data_packages.rds")

b_data_packages = readRDS("./rds_objects/b_data_packages.rds")


#########################################################################################################
# Create data packages for each combo: Schreiberi -----

# Data combination descriptions
s.2scenario.descriptions = list("A: All data",
                                 "B: Exclude unreliable collections data")

s.4scenario.descriptions = list("A: All data (PA)",
                                 "B: Exclude unreliable collections data (PA)",
                                 "E: All the presences (PO)",
                                 "F: All reliable presences only (PO)")

s.6scenario.descriptions = list("A: All data (PA)",
                                 "B: Exclude unreliable collections data (PA)",
                                 "C: Surv. data; all included (PA)",
                                 "D: Surv presences only (PO)",
                                 "E: All the presences (PO)",
                                 "F: All reliable presences only (PO)")

obs_A = rbind(ssp, ssa, sc)
obs_B = rbind(ssp, ssa, sc.r)
obs_C = rbind(ssp, ssa) 
obs_D = rbind(ssp)
obs_E = rbind(ssp, sc)
obs_F = rbind(ssp, sc.r)

s.6scen.obs.packages    = list(obs_A, obs_B, obs_C, obs_D, obs_E, obs_F)
s.6scen.scen.names = list('Schreiberi Scenario A','Schreiberi Scenario B','Schreiberi Scenario C',
                          'Schreiberi Scenario D','Schreiberi Scenario E','Schreiberi Scenario F')

s.6scen.data.packages = list()

for (d in 1:length(s.6scen.scen.names)) {
  s.6scen.data.packages[[d]] = sdmData(formula = occurrence ~ ., train = s.6scen.obs.packages[[d]], predictors=s.preds)
  print("------------"); print(s.6scen.scen.names[[d]]); print(s.6scen.data.packages[[d]])   
  print(length(s.6scen.obs.packages[[d]]))   } 
  # 20 duplicate points in all: must be in ssp. Maybe not identical, but fall in the same cells. 

# Now add background points to last three data packages that are PO. Manually to ensure obs not double-counted.
bg.points = length(s.6scen.data.packages[[4]]@features$rID) 
s.6scen.data.packages[[4]] = sdmData(formula = occurrence~., train = s.6scen.obs.packages[[4]], 
                                     predictors = s.preds, bg = list(n = bg.points, method='gRandom', remove=TRUE))
s.6scen.data.packages[[4]] # 1322 obs; presence-background

bg.points = length(s.6scen.data.packages[[5]]@features$rID) 
s.6scen.data.packages[[5]] = sdmData(formula = occurrence~., train = s.6scen.obs.packages[[5]], 
                                     predictors = s.preds, bg = list(n = bg.points, method='gRandom', remove=TRUE))
s.6scen.data.packages[[5]] # 1508 obs; presence-background

bg.points = length(s.6scen.data.packages[[6]]@features$rID) 
s.6scen.data.packages[[6]] = sdmData(formula = occurrence~., train = s.6scen.obs.packages[[6]], 
                                     predictors = s.preds, bg = list(n = bg.points, method='gRandom', remove=TRUE))
s.6scen.data.packages[[6]] # 1448 obs; presence-background

# main output objects from this section:
saveRDS(s.6scen.scen.names,       "./rds_objects/s.6scen.scen.names.rds")
saveRDS(s.6scenario.descriptions, "./rds_objects/s.6scenario.descriptions.rds")
saveRDS(s.6scen.obs.packages,     "./rds_objects/s.6scen.obs.packages.rds")
saveRDS(s.6scen.data.packages,    "./rds_objects/s.6scen.data.packages.rds")
