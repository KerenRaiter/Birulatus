# sdm.3.prepare data for modelling ----

# There are three main functions provide the main steps of developing/using species distibution models. 
# The three steps include data preparation, model ﬁtting and evaluation, and prediction. 
# The functions used for these steps:
# • sdmData: to read data from different formats, and prepare a data object. Both species (single or multiple) and 
# explanatory variables can be introduced to the function, as well as other information such as spatial coordinates, 
# time, grouping variables, etc.
# • sdm: to ﬁt and evaluate species distribution models (multiple algorithms can be used)
# • predict: when models are ﬁtted, they can be used to predict/project given a new dataset.

# As at May 2021, we have moved to using observational data summarised PER SITE (with multiple nests at a site), and in three extents: 
# 1) 's': the smallest extent "soil-delimited": 20km buffer around presences, excludes golan. n=23
# 2) 'l': slightly larger, "lithology-delimited": 20 km buffer around presences. Incl Golan. n=24
# 3) 'i': israel-wide, includes WB, excludes Gaza and waterbodies. n=27

####################################################################################################
# Set up and install relevant packages and locations ----

# ipak function: install (if not already installed) and load multiple R packages.
ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)}
ipak(c("rgdal","stringr","usdm", "biomod2","raster","scales", "grid", "foreign","dplyr",
       "magrittr","tidyr","rgeos","ggplot2","gridExtra","rasterVis","dismo","sdm",
       "installr","knitr","ggmap","OpenStreetMap","parallel","beepr","rmapshaper", "spatialEco",
       "rJava")) # removed sf, may be causing problems
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

####################################################################################################
# Datasets prepared earlier ----

# from this script:

data.packages          = readRDS("rds/data.packages.rds")

# from previous scripts:

b.s   = readRDS("./rds/b.bysite.s.rds"); table(b.s$occurrence)
b.l   = readRDS("./rds/b.bysite.l.rds"); table(b.l$occurrence)
b.i   = readRDS("./rds/b.bysite.i.rds"); table(b.i$occurrence)

preds.s.nocoll = readRDS(paste0(B.heavies.rds.path,"preds.s.nocoll.rds")) # stack, collinear excl.
preds.l.nocoll = readRDS(paste0(B.heavies.rds.path,"preds.l.nocoll.rds")) # stack, collinear excl.
preds.i.nocoll = readRDS(paste0(B.heavies.rds.path,"preds.i.nocoll.rds")) # stack, collinear excl. 

####################################################################################################
# Prepare observational data for input into data packages ----

set.names = list("Soils-delimited study area", "Lithology-delimited study area", 
                 "Israel-wide study area")

points.list        = list(b.s, b.l, b.i)

for (p in 1:length(points.list)) {
  print(set.names[[p]])                                   # print name of dataset to start us off
  print(class(points.list[[p]]))                          # should be "SpatialPointsDataFrame"
  print(length(points.list[[p]]))                         # how many data points?
  points.list[[p]]@coords = points.list[[p]]@coords[,1:2] # Remove third (Z) coordinate if present
  print(summary(points.list[[p]]@coords))                 # check that only two coords remain
  print(str(points.list[[p]]$occurrence))                 # ensure recorded consistently: 1's or 0's
  print(summary(points.list[[p]]$occurrence))             # again, ensure is recorded consistently
  points.list[[p]]@data <- points.list[[p]]@data[,'occurrence', drop=F] # dropping other attributes
  print(names(points.list[[p]]))                                        # check only occ remains
} # check in output that there are just two coords (x1 and x2), and the expected number of features. 

# convert back to original names:
b.s = points.list[[1]];  b.l = points.list[[2]]; b.i = points.list[[3]] 

####################################################################################################
# Create data packages for each combo -----

data.pack.s = sdmData(formula = occurrence ~ ., train = b.s, predictors = preds.s.nocoll)
data.pack.l = sdmData(formula = occurrence ~ ., train = b.l, predictors = preds.l.nocoll)
data.pack.i = sdmData(formula = occurrence ~ ., train = b.i, predictors = preds.i.nocoll)
data.packages = list(data.pack.s, data.pack.l, data.pack.i)

# main output objects from this section:

saveRDS(data.packages,         "./rds/data.packages.bysite.rds")

####################################################################################################