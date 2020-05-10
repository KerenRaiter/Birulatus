# Birulatus 2 data explore
#######################################################################################################################
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

#######################################################################################################################
# Load data created previously ----
# from this script:
preds.nocoll      = readRDS(paste0(B.heavies.rds.path,"preds.nocoll.rds")) # raster stack

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

raster.list       = readRDS(paste0(B.heavies.rds.path,"raster.list.rds"))
raster.list.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Lith")
preds             = readRDS(paste0(B.heavies.rds.path,"preds.rds")) # raster stack

bi.raw = readRDS("./rds/bi.raw.rds") 
bi = readRDS("./rds/bi.rds") 

#######################################################################################################################
# Plot layers ----

plot(preds.s, main = raster.list.s.names)
plot(preds.l, main = raster.list.l.names)

# Test for multicollinearity ----
# Get variance inflation Factor and test for multicollinearity:

preds = preds.l # instead of changing the reference many times in the code below, set it here once

names(preds) 
lstats = layerStats(preds, 'pearson', na.rm=T)
corr_matrix = lstats$'pearson correlation coefficient'; corr_matrix

png(filename=paste0(B.heavies.image.path,"Correlation matrix for Birulatus_lithset.png"),
    width=20, height=20, units='cm', res=600)
pairs(preds, hist=TRUE, cor=TRUE, use="pairwise.complete.obs", maxpixels=100000)
dev.off()

# Zuur: Some statisticians suggest that VIF values higher then 5 or 10 are too high. In ecology vif larger than 3 is considered too much
vif(preds) 
vifcor(preds)
names(preds) # i.e. need to get rid of #3 and #4.

# getting rid of elevation
preds.l.nocoll = stack(preds[[1]],preds[[2]],preds[[5]],preds[[6]],preds[[7]])
vif(preds.l.nocoll)

vifcor(preds.l.nocoll, th=0.9) # no collinearity problem (I already excluded collinear variables previously)
# there's another function called corvif(), takes in only numerical variables. for reference.

vifstep(preds, th=10) # identify collinear variables that should be excluded. doesn't work

saveRDS(preds.l.nocoll,        paste0(B.heavies.rds.path,"preds.l.nocoll.rds")) # raster stack

#####
# Legacy from here down...Create Beershebensis subsets of reliable and questionable survey absence data ----
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

