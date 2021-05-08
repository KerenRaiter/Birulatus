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

bir.area.s = readRDS("./rds/bir.area.s.rds")
bir.area.l = readRDS("./rds/bir.area.l.rds")
bir.area.i = readRDS("./rds/bir.area.i.rds")
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

b.raw = readRDS("./rds/b.raw.bysite.rds")
b     = readRDS("./rds/b.bysite.rds")
b.s   = readRDS("./rds/b.bysite.s.rds")
b.l   = readRDS("./rds/b.bysite.l.rds")
b.i   = readRDS("./rds/b.bysite.i.rds")

#######################################################################################################################
# Plot layers ----

plot(preds.s, main = raster.list.s.names)
plot(preds.l, main = raster.list.l.names)
plot(preds.i, main = raster.list.i.names)

# Test for multicollinearity ----
# Get variance inflation Factor and test for multicollinearity:

# soil-delimited set:
lstats = layerStats(preds.s, 'pearson', na.rm=T)
corr_matrix = lstats$'pearson correlation coefficient'; corr_matrix
# png(filename=paste0(B.heavies.image.path,"Correlation matrix for Birulatus_soilset.png"),
#     width=20, height=20, units='cm', res=600)
pairs(preds.s, hist=TRUE, cor=TRUE, use="pairwise.complete.obs", maxpixels=100000)
# dev.off()

vif(preds.s)
# Zuur: Some suggest that VIF values >5 or 10 are too high. In ecology vif >3 considered too much
vifcor(preds.s) # excludes elevation & july temp from soil set. Sometimes jan & jul temp instead.
names(preds.s) # ie get rid of #3 (jult) & #4 (elevation) 
preds.s.nocoll = stack(preds.s[[1]],preds.s[[2]],preds.s[[5]],preds.s[[6]],preds.s[[7]])
vif(preds.s.nocoll)
vifcor(preds.s.nocoll, th=0.9) # no collinearity problem remaining
saveRDS(preds.s.nocoll, paste0(B.heavies.rds.path,"preds.s.nocoll.rds")) # raster stack

# lithology-delimited study area:
lstats = layerStats(preds.l, 'pearson', na.rm=T)
corr_matrix = lstats$'pearson correlation coefficient'; corr_matrix
# png(filename=paste0(B.heavies.image.path,"Correlation matrix for Birulatus_lithset.png"),
#     width=20, height=20, units='cm', res=600)
pairs(preds.l, hist=TRUE, cor=TRUE, use="pairwise.complete.obs", maxpixels=100000)
# dev.off()

vif(preds.l)
# Zuur: Some suggest that VIF values >5 or 10 are too high. In ecology vif >3 considered too much
vifcor(preds.l) # excludes elevation & july temp from lith set. Sometimes jan & jul temp instead.
names(preds.l) # ie get rid of #3 (jult) & #4 (elevation) 
preds.l.nocoll = stack(preds.l[[1]],preds.l[[2]],preds.l[[5]],preds.l[[6]],preds.l[[7]])
vif(preds.l.nocoll)
vifcor(preds.l.nocoll, th=0.9) # no collinearity problem remaining
saveRDS(preds.l.nocoll, paste0(B.heavies.rds.path,"preds.l.nocoll.rds")) # raster stack

# israel-wide study area:
lstats = layerStats(preds.i, 'pearson', na.rm=T)
corr_matrix = lstats$'pearson correlation coefficient'; corr_matrix
# png(filename=paste0(B.heavies.image.path,"Correlation matrix for Birulatus_lithset.png"),
#     width=20, height=20, units='cm', res=600)
pairs(preds.i, hist=TRUE, cor=TRUE, use="pairwise.complete.obs", maxpixels=100000)
# dev.off()

vif(preds.i)
# Zuur: Some suggest that VIF values >5 or 10 are too high. In ecology vif >3 considered too much
vifcor(preds.i) # excludes elevation from israel set
names(preds.i) # ie exclude #4 (elevation) 
preds.i.nocoll = stack(preds.i[[1]],preds.i[[2]],preds.i[[3]],preds.i[[5]],preds.i[[6]],preds.i[[7]])
vif(preds.i.nocoll)
vifcor(preds.i.nocoll, th=0.9) # no collinearity problem remaining
saveRDS(preds.i.nocoll, paste0(B.heavies.rds.path,"preds.i.nocoll.rds")) # raster stack

# there's another function called corvif(), takes in only numerical variables. for reference.
