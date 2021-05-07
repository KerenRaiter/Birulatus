# Birulatus 1: data and functions (& things to do) ----

####################################################################################################
# Set up and install relevant packages and locations ----

# ipak function: install (if not already installed) and load multiple R packages.
ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)}
ipak(c("rgdal","stringr","usdm", "biomod2","raster","scales", "grid", "foreign","dplyr","magrittr","tidyr","rgeos",
       "magrittr","ggplot2","gridExtra","raster","rasterVis","dismo","sdm","installr","knitr","ggmap",
       "OpenStreetMap","parallel","beepr","rmapshaper", "spatialEco", "rJava","readxl")) # removed sf, may be causing problems
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
# Datasets I prepared earlier in this script ----

borders          = readRDS("rds/borders.rds");            israel.WB = readRDS("rds/israel.WB.rds")
israel.WB.merged = readRDS("./rds/israel.WB.merged.rds"); israel.WB = readRDS("./rds/israel.WB.no.water.rds")
israel.noWB      = readRDS("rds/israel.noWB.rds")
plot(borders); lines(israel.WB.merged, col = "green", lwd = 4); lines(israel.noWB, col="blue")

bir.area.l    = readRDS("rds/bir.area.l.rds")   # study area
bir.area.s  = readRDS("rds/bir.area.s.rds") # study area limited to where there is soils info
plot(bir.area.l, lwd = 5, col = "brown"); lines(bir.area.s, col = "yellow")
xlims = c(35.14081, 35.86214)
ylims = c(31.59407, 33.00496)

major.cities = readRDS("./rds/major.cities.rds")
small.cities = readRDS("./rds/small.cities.rds")
large.towns  = readRDS("./rds/large.towns.rds")
towns        = readRDS("./rds/towns.rds")
# villages     = readRDS("./rds/villages.rds")

groads       = readRDS("./rds/groads.rds")

# raster.list.l       = readRDS(paste0(B.heavies.rds.path,"raster.list.l.rds")); plot(raster.list.l[[1]])
# raster.list.l.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Lith")
raster.list.s       = readRDS(paste0(B.heavies.rds.path,"raster.list.s.rds")); plot(raster.list.s[[7]])
raster.list.s.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Soil")
preds.s             = readRDS(paste0(B.heavies.rds.path,"preds.s.rds")) # raster stack
plot(preds.s)

bi.raw = readRDS("./rds/bi.raw.rds") 
bi     = readRDS("./rds/bi.rds") ;    bi.s  = readRDS("./rds/bi.s.rds") 
bip    = readRDS("./rds/bip.rds");    bip.s    = readRDS("./rds/bip.s.rds")
bia    = readRDS("./rds/bia.rds");    bia.s    = readRDS("./rds/bia.s.rds")

# old... to use as templates
...
{
s.preds             = readRDS(paste0(heavies.rds.path,"s.preds.rds")) # raster stack

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
}

####################################################################################################
# Importing reference datasets and study area boundaries ----

# Country borders ----
borders   = readOGR(dsn="E:/GIS working/layers/borders",layer="Israel_simple_WGS") # no extension for readOGR function
plot(borders) 

israel.WB = readOGR(dsn = "E:/GIS working/layers/borders", layer="Israel_&_WB_landandwatermerged_WGS")
plot(israel.WB)

israel.WB.merged = readOGR(dsn = "E:/GIS working/layers/borders", layer="Israel_&_WB_merged_landandwatermerged_WGS")
plot(israel.WB.merged)

israel.WB.no.water = readOGR(dsn = "E:/GIS working/layers/borders", layer="Israel_&_West_Bank_minuswaterbodies_WGS")
plot(israel.WB.no.water, col='blue')
israel.WB.no.water.merge <- aggregate(israel.WB.no.water, dissolve=T)
length(israel.WB.no.water.merge@polygons)
plot(israel.WB.no.water.merge)

israel.noWB = borders[borders$Label == "Israel",]; plot(israel.noWB)

# saveRDS(borders, "rds/borders.rds")         
# saveRDS(israel.WB, "rds/israel.WB.rds")
# saveRDS(israel.WB.merged, "./rds/israel.WB.merged.rds")
# saveRDS(israel.WB, "./rds/israel.WB.no.water.rds")
# saveRDS(israel.noWB, "rds/israel.noWB.rds")

# Transformations:
# borders.ITM = spTransform(borders, ITM)

# Study area ----
bir.area.s = readOGR(dsn="E:/GIS working/layers/birulatus",layer="Birulatus_study_area_soils") # reduced
# plot(soil, ylim=c(32.7,33), xlim=c(35.5,36)); lines(bir.area.s) # zooming in on area of interest.
# saveRDS(bir.area.s, "./rds/bir.area.s.rds")
bir.area.s = readRDS("./rds/bir.area.s.rds")
plot(bir.area.s)

# bir.area.l = readOGR(dsn="E:/GIS working/layers/birulatus",layer =     "Birulatus_study_area_original")
# saveRDS(bir.area.l, "./rds/bir.area.l.rds")
bir.area.l = readRDS("./rds/bir.area.l.rds")
plot(bir.area.l)

# bir.area.i = israel.WB.no.water.merge # israel-wide extent incl WB, excl waterbodies
# saveRDS(bir.area.i, "./rds/bir.area.i.rds")
bir.area.i = readRDS("./rds/bir.area.i.rds")
plot(bir.area.i, col='orange')

# Cities and other population centres etc ----
# population.centres = readOGR(dsn="E:/GIS working/layers/society",layer="Population_centres")
# population.centres$lat = population.centres$Latitude__; population.centres$lon = population.centres$Longitude
# par(mar=c(1,1,1,1)); plot(borders)
# points(population.centres, pch=21, col='black', bg='yellow')
# plot(population.centres$lat~population.centres$lon)
# with(population.centres, text(population.centres$lat~population.centres$lon, labels = population.centres$name, pos=4))
# 
# major.cities  = population.centres[population.centres$category == "Major city", ]
# small.cities  = population.centres[population.centres$category == "City", ]
# large.towns   = subset(population.centres, subset = category == "Large town")
# towns         = subset(population.centres, subset = category == "Town"|category=="Large town")
# # villages      = population.centres[population.centres$category == "Village", ]

# saveRDS(major.cities, "./rds/major.cities.rds")
# saveRDS(small.cities, "./rds/small.cities.rds")
# saveRDS(large.towns, "./rds/large.towns.rds")
# saveRDS(towns, "./rds/towns.rds")
# saveRDS(villages, "./rds/villages.rds")

major.cities = readRDS("./rds/major.cities.rds")
small.cities = readRDS("./rds/small.cities.rds")
large.towns  = readRDS("./rds/large.towns.rds")
towns        = readRDS("./rds/towns.rds")

# Roads - detailed (too slow, too detailed):
# roads   = readOGR(dsn="E:/GIS working/layers/infrastructure",layer="OpenstreetMapRoads") # hashed out as too slow and detailed
# plot(roads); saveRDS(roads,"./rds/roads.rds") # detailed dataset - takes ages. Bad layer too.

# Roads - undetailed. from EarthData global dataset: roads between settlements
# groads = readOGR(dsn="E:/GIS working/layers/infrastructure/gROADS-v1-europe.gdb",layer="groads_israel") 
# saveRDS(groads, "./rds/groads.rds")
groads = readRDS("./rds/groads.rds"); plot(groads, col="grey") 

# Land-use data ----
# nat.res   = readOGR(dsn="E:/GIS working/layers/planning and landuse/planning_landuse.gdb",layer="Nature_reserves") 
# nat.park  = readOGR(dsn="E:/GIS working/layers/planning and landuse/planning_landuse.gdb",layer="national_parks_WGS")
# saveRDS(nat.res, paste0(heavies.rds.path,"nat.res.rds"))
# saveRDS(nat.park, paste0(heavies.rds.path,"nat.park.rds"))

# kkl.forestry   = readOGR(dsn="E:/GIS working/layers/planning and landuse/KKL.gdb",layer="KKL_current_forestry")
# kkl.plans.a    = readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="KKL_plans_Alexandra_WGS")
# kkl.plans.b    = readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="KKL_plans_tama35_Yehudah_WGS")
# kkl.plans = readOGR(dsn="E:/GIS working/layers/planning and landuse/KKL.gdb",layer="KKLplans_AnY_excl_currentf_reserves")

# saveRDS(kkl.forestry, paste0(heavies.rds.path,"kkl.forestry.rds"))
# saveRDS(kkl.plans.a,  paste0(heavies.rds.path,"kkl.plans.a.rds"))
# saveRDS(kkl.plans.b,  paste0(heavies.rds.path,"kkl.plans.b.rds"))
# saveRDS(kkl.plans,    paste0(heavies.rds.path,"kkl.plans.rds"))

# par(mar=c(0,0,0,0)); plot(kkl.plans.b, col="pink",border="pink"); plot(kkl.plans.a, add=T, col="red",border="red")
# plot(kkl.forestry, add = T, col="brown",border="brown")

# landuse.unsimplified = readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="Landuse_master")
# landuse              = readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="Landuse_simplified")
# landuse_b            = readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="Landuse_simplified_b")
# landuse_s            = readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="Landuse_simplified_s")
# natreserve_no_firing   = readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="Natreserve_no_firing")
# natreserve_firing_intersect =readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="Natreserve_firing_intersect")
# natpark_no_firing = readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="Natpark_no_firing")
# natpark_firing_intersect = readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="Natpark_firing_intersect")
# firing_no_natcons = readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="Firing_no_natcons") 
# #Firing zones with disturbing land-uses erased (i.e. overlap with nat cons included in this layer):
# firing_excl_dist_l =readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="Firing_zones_excl_disturbing landuse")
# Firing zones with all other landuse types (including conservation) erased:
# firing_excl_alllanduse = readOGR(dsn="E:/GIS working/layers/planning and landuse",layer="Firing_zones_excl_other_landuse_incl_cons") 

# saveRDS(landuse.unsimplified,        paste0(heavies.rds.path,"landuse.unsimplified.rds"))
# saveRDS(landuse,                     paste0(heavies.rds.path,"landuse.simplified.rds"))
# saveRDS(landuse_b,                   paste0(heavies.rds.path,"landuse_b.rds"))
# saveRDS(landuse_s,                   paste0(heavies.rds.path,"landuse_s.rds"))
# saveRDS(natreserve_no_firing,        paste0(heavies.rds.path,"natreserve_no_firing.rds"))
# saveRDS(natreserve_firing_intersect, paste0(heavies.rds.path,"natreserve_firing_intersect.rds"))
# saveRDS(natpark_no_firing,           paste0(heavies.rds.path,"natpark_no_firing.rds"))
# saveRDS(natpark_firing_intersect,    paste0(heavies.rds.path,"natpark_firing_intersect.rds"))
# saveRDS(firing_no_natcons,           paste0(heavies.rds.path,"firing_no_natcons.rds"))
# saveRDS(firing_excl_dist_l,          paste0(heavies.rds.path,"firing_excl_dist_l.rds"))
# saveRDS(firing_excl_alllanduse,      paste0(heavies.rds.path,"firing_excl_alllanduse.rds"))


# # landuse.simple       = rmapshaper::ms_simplify(landuse.unsimplified, keep = 0.2, keep_shapes = TRUE)
# kkl.forestry_simple  = ms_simplify(kkl.forestry, keep = 0.3, keep_shapes = TRUE)
# 
# plot(kkl.forestry)
# plot(kkl.forestry_simple, col="yellow", add=T)
# plot(kkl.forestry); plot(kkl.plans, col="red", border="red", add=T)
# 
# summary(landuse.unsimplified@data$landuse); par(mar=c(0,0,0,0))
# landuse = landuse.unsimplified
# png(filename = "./output_images/landuse_b.png", width = 12, height = 15, units = 'cm', res = 900)
# par(mar=c(0,0,0,0))
# plot(landuse[landuse@data$landuse == "built-up area",], col="darkgrey",border="darkgrey",lwd=0.25, xlim=xlims_b,ylim=ylims_b)
# plot(landuse_b[landuse_b@data$landuse == "conservation",], col="lightgreen",     border=NA, add=T)
# plot(landuse_b[landuse_b@data$landuse == "military",],     col="lightslateblue", border=NA, add=T)
# plot(landuse_b[landuse_b@data$landuse == "forestry",],     col="darkgreen",      border=NA, add=T)
# plot(landuse_b[landuse_b@data$landuse == "agriculture",],  col="chocolate",      border=NA, add=T)
# plot(major.cities, pch=21, bg='yellow', cex=1, add=TRUE)
# with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=0.5, font=2, offset=0.3))
# legend("bottomright", c("Agriculture","Built-up area","Military","Forestry","Conservation"), pch=22, pt.cex=1.5, 
#         col=c("chocolate","darkgrey","lightslateblue","darkgreen","lightgreen"),
#         pt.bg=c("chocolate","darkgrey","lightslateblue","darkgreen","lightgreen"))
# dev.off()

# simplifying dataset with rmshaper doesn't work, did in ArcGIS instead:
# landuse_simple = rmapshaper::ms_simplify(landuse_b, keep = 0.2, keep_shapes = TRUE)

# Plot reference datasets ----
plot(bir.area.i); lines(bir.area.l, col="blue", lwd=2); lines(bir.area.s, col="orange")
lines(groads, col="grey73")
points(towns, pch= 21, bg= 'green', cex = 0.9)
plot(small.cities, pch=21, bg='orange', cex=1.2, add=TRUE)
plot(major.cities, pch=21, bg='yellow', cex=1.6, add=TRUE)

####################################################################################################
# Load and set up raster data  ----
# note: very slow, best to just reload a previously-run version, see end of section line 

par(mfrow = c(2,4), mar=c(2,2,2,2), bty="n")
# rain = raster("E:/GIS working/layers/climate/Annual_Rain_1981-2010.tif"); plot(rain); names(rain)="Precipitation"
# saveRDS(rain, paste0(B.heavies.rds.path,"rain.original.rds"))
rain = readRDS(paste0(B.heavies.rds.path,"rain.original.rds"))
plot(rain, main="Rainfall"); lines(bir.area.i)

# jant = raster("E:/GIS working/layers/climate/av_temp_jan_WGS.tif"); plot(jant); names(jant) = "Jan mean temperature"
# saveRDS(jant, paste0(B.heavies.rds.path,"jant.rds"))
jant = readRDS(paste0(B.heavies.rds.path,"jant.rds"))
plot(jant, main="Jan temperature"); lines(bir.area.i)

# jult = raster("E:/GIS working/layers/climate/av_temp_july_WGS.tif"); plot(jult); names(jult) ="July mean temperature"
# saveRDS(jult, paste0(B.heavies.rds.path,"jult.rds"))
jult = readRDS(paste0(B.heavies.rds.path,"jult.rds"))
plot(jult, main="July temperature"); lines(bir.area.i)

# dem = raster("E:/GIS working/layers/topography/DEM_WGS84.tif"); plot(dem); names(dem) ="Elevation"
# saveRDS(dem, paste0(B.heavies.rds.path,"dem.rds"))
dem = readRDS(paste0(B.heavies.rds.path,"dem.rds"))
plot(dem, main="Elevation"); lines(bir.area.i)

# twet = raster("E:/GIS working/layers/topography/Topo_index_IsraelWB.tif")
# plot(twet); names(twet)="Topographic Wetness"
# saveRDS(twet, paste0(B.heavies.rds.path,"twet.rds"))
twet = readRDS(paste0(B.heavies.rds.path,"twet.rds"))
plot(twet, main="Topographic wetness"); lines(bir.area.i)

# slop = raster("E:/GIS working/layers/topography/Yamazaki_topo/slope_yk.tif"); plot(slop); names(slop)="Slope"
plot(bir.area.i); plot(slop, main="Slope", add=T); lines(bir.area.i)
# issue here that slope data doesn't cover the southern extent. Might be able to download more data.
#let's try making the slope layer from the DEM layer:
# slop = terrain(dem, opt="slope", unit="degrees", neighbors=8)
# issue solved!
# saveRDS(slop, paste0(B.heavies.rds.path,"slop.rds"))
slop = readRDS(paste0(B.heavies.rds.path,"slop.rds"))
plot(slop, main="Slope (degrees)"); lines(bir.area.i)

# lith = raster("E:/GIS working/layers/geology and soils/lithology_to_raster.tif"); plot(lith); names(lith)="Lithology"
# saveRDS(lith, paste0(B.heavies.rds.path,"lith.rds"))
lith = readRDS(paste0(B.heavies.rds.path,"lith.rds"))
plot(lith, main="Lithology"); lines(bir.area.i)
# Special job to deal with NAs in lithology layer
{
# Remove NAs from Lithology:
par(mfrow = c(1,1))
plot(lith, main= "Lithology")
table(lith[]); summary(lith[])
# values go up to 34, then 128 is the 'NA'; there's 2574700 na-s. But are there NAs in study area?
length(lith[lith == 128])  # 2,574,700
lith.crop = crop(lith, extent(bir.area.i))
lith.mask = mask(lith.crop, bir.area.i)
length(lith.mask[lith.mask == 128])  # 2796. So need to fill.
plot(lith.mask, main = "lith.cropped.masked")
# i.e. yes we do need to do some filling.

length(lith[lith == 128])
lith.nas = lith; lith.nas[lith.nas == 128] = NA # convert 128s to NAs
length(lith.nas[lith.nas == 128]) # 0. confirm it's done

# Multiple rounds of filling (to remove persistent voids)
lith.filledX = focal(lith.nas,     w=matrix(1,7,7), fun = modal, 
                     na.rm=TRUE, NAonly=TRUE, pad=TRUE)
lith.filledX = focal(lith.filledX, w=matrix(1,7,7), fun = modal, 
                     na.rm=TRUE, NAonly=TRUE, pad=TRUE)

# Check to see if the relevant NAs have been dealt with:
# convert NAs back to a number (as they don't plot as NAs)
lith.filled.999 = lith.filledX
lith.filled.999[is.na(lith.filled.999)] = 999
# now crop and mask and see if there are still any in either species' buffer:
lith.filled.crop = crop(lith.filled.999, extent(bir.area.i))
lith.filled.mask = mask(lith.filled.crop, bir.area.i)
length(lith.filled.mask[lith.filled.mask == 999])  # need to be 0
plot(lith.filled.mask, main = "lith.filled.cropped.masked")

  # replace the lith layer with my new improved one:
lith = lith.filledX
names(lith) = "lith"
saveRDS(lith,  paste0(B.heavies.rds.path,"lith.filled.rds"))

# delete the objects I no longer need:
lsos()
rm(lith.filled, lith.filledX, lith.crop, lith.mask)
rm(lith.filled.999, lith.filled.crop, lith.filled.mask, lith.999, lith.nas)
gc()
  # end of special job to deal with 'NA' values in lithology
}
UP TO HERE, JUST WORK THROUGH, EVERYTHING SHOULD BE GOOD TO GO...
# soils long story ----
# soil.code2 = raster("E:/GIS working/layers/geology and soils/soils_code2_WGSextended.tif")
# names(soil.code2) = "Soil.type (code 2)"
# soil original processed to fill NA cells (see below). Read in output from there: 'filled' version 
# soil.code2 = readRDS("E:/R/sdm_edrive/rds_objects/soil.filled.rds")
# saveRDS(soil.code2, paste0(B.heavies.rds.path,"soil.code2.filled.rds"))
# soil.code2 = readRDS(paste0(B.heavies.rds.path,"soil.code2.filled.rds"))
# plot(soil.code2, main="Soil code 2")

# Special job to create a raster of soils code 1 polygons
{# (I previously did soils code 2 in ArcGIS, but currently don't have access to ArcGIS and it's been decided to use code 1 instead of code 2)
# 
# library('raster')
# library('rgdal')
# dat = readOGR(dsn="E:/GIS working/layers/geology and soils",layer="soils_WGS")
# par(mfrow = c(1,1), mar=c(2,2,2,2), bty="n")
# dat[["CODE1"]]
# plot(dat)
# 
# # get names
# (nam <- unique(dat$CODE1))
# 
# # create a data.frame
# (nam_df <- data.frame(ID = 1:length(nam), nam = nam))
# 
# # Place IDs
# (dat$ID <- nam_df$ID[match(dat$CODE1,nam_df$nam)])
# 
# # Define RasterLayer object
# r.raster <- raster()
# 
# # Define raster extent
# extent(r.raster) <- extent(dat)
# 
# # Define pixel size
# res(r.raster) <- res(slop)
# 
# # rasterize
# ras <- rasterize(x = dat, y = r.raster, field = "ID")
# 
# # ratify raster
# r <- ratify(ras)
# 
# # Create levels
# rat <- levels(r)[[1]]
# rat$names <- nam_df$nam
# rat$IDs <- nam_df$ID
# levels(r) <- rat
# 
# rasterVis::levelplot(r)
# soil.code1 = r
# table(r[])
# 
# saveRDS(soil.code1, paste0(B.heavies.rds.path,"soil.code1.rds"))
# soil.code1 = readRDS(paste0(B.heavies.rds.path,"soil.code1.rds")); plot(soil.code1, main="Soil type")
# 
# soil.code1 = readRDS(paste0(B.heavies.rds.path,"soil.code1.rds"))
# plot(soil.code1, main="Soil type"); lines(bir.area.i)
}
# Special job to check and deal with NA values in soil) layers
{
# # Remove NAs from Soil:
# soil = soil.code1
# plot(soil, main= "soil")
# length(soil) # 9.3 million pixels total
# table(soil[]); summary(soil[]) # values range from 1-22. 5.5 million are NAs (mostly white mask)
# 
# # Let's see if we need to do any filling of NA values from study area edges:
# # convert NAs back to a number (as they don't plot as NAs)
# soil.999 = soil
# soil.999[is.na(soil.999)] = 999
# length(soil.999[soil.999 == 999])
# 
# # now crop and mask and see if there are still any in either species' buffer:
# soil.crop = crop(soil.999, extent(bir.area.i))
# soil.mask = mask(soil.crop, bir.area.i)
# summary(soil.mask[])
# length(soil.mask[soil.mask == 999])  # 15753630 # all '999's gone! finally, after 6 rounds of filling.
# plot(soil.mask, main = "soil.cropped.masked")
# # i.e. yes we do need to do some filling.
# 
# # length(soil[soil == 128])  # this was relevant for code 2, not for code 1
# # soil.nas = soil; soil.nas[soil.nas == 128] = NA # convert all 128s to NAs.
# # length(soil.nas[soil.nas == 128]) # 0. confirm it's done
# 
# # Multiple rounds of filling (running again to remove persistent voids)
# soil.filledX = focal(soil,         w=matrix(1,7,7), fun = modal, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
# soil.filledX = focal(soil.filledX, w=matrix(1,7,7), fun = modal, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
# 
# table(soil.filledX[]); summary(soil.filledX[]) # need NAs to reduce but not necessarily dissapear
# 
# # Check to see if the relevant NAs have been dealt with:
# # convert NAs back to a number (as they don't plot as NAs)
# soil.filled.999 = soil.filledX
# soil.filled.999[is.na(soil.filled.999)] = 999
# length(soil.filled.999[soil.filled.999 == 999])
# # now crop and mask and see if there are still any in either species' buffer:
# soil.filled.crop = crop(soil.filled.999, extent(bir.area.i))
# soil.filled.mask = mask(soil.filled.crop, bir.area.i)
# length(soil.filled.mask[soil.filled.mask == 999])  # need to get this to zero. Good enough here
# plot(soil.filled.mask, main = "soil.filled.cropped.masked")
# 
# # replace the soil layer with my new improved one:
# soil = soil.filledX
# names(soil) = "Soil"
# saveRDS(soil,  paste0(B.heavies.rds.path,"soil.code1.filled.rds"))
# 
# # delete the objects I no longer need:
# lsos()
# rm(soil.filled, soil.filledX, soil.crop, soil.mask)
# rm(soil.filled.999, soil.filled.crop, soil.filled.mask, soil.999, ras)
# gc()
# # end of special job to deal with 'NA' values in categorical layers.
 }
soil = readRDS(paste0(B.heavies.rds.path,"soil.code1.filled.rds"))
plot(soil, main = "Soil"); lines(bir.area.i)

# Manipulate raster files ----

# make lists for loopy manipulation two lists for the three extents and their specific datasets.
# first make a master raster list and then crop and select relevant rasters for specific lists

master.raster.list = list(rain, jant, jult, dem, twet, slop, lith, soil)
par(mfrow = c(4,2), mar=c(2,2,2,2), bty="n")

# diagnostic loop:
for (i in 1:length(master.raster.list))       {
  plot(master.raster.list[[i]], main = names(master.raster.list[[i]])); lines(bir.area.i)
  print(names(master.raster.list[[i]]))
  print(res(master.raster.list[[i]])); print(crs(master.raster.list[[i]]))}

# get all raster files into the same coordinate system 
# we want WGS84: "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(master.raster.list[[1]])
master.raster.list[[1]] = projectRaster(master.raster.list[[1]], 
                                        crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84   +towgs84=0,0,0")
crs(master.raster.list[[1]])

crs(master.raster.list[[8]])
master.raster.list[[8]] = projectRaster(master.raster.list[[8]], 
                                        crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84   +towgs84=0,0,0")
crs(master.raster.list[[8]]) # I don't know what the no defs means, doesn't seem to create a problem

# looking at resolutions
# i've put them in order of how important resolution might be:
# from: http://www.csgnetwork.com/degreelenllavcalc.html
deg.length.north = 110904.5249690638  # based on north of study area being 33.004964 degrees north
deg.length.south = 110879.71166494413 # based on south of study area being 31.594068 degrees north
deg = mean(c(deg.length.north,deg.length.south)) # 110892.1 for Birulatus study area

for (i in 1:length(master.raster.list))       {
  print(names(master.raster.list[[i]]))
  print(res(master.raster.list[[i]]) * deg)}

# "Precipitation" 291.6463 249.5073
# "Jan.mean.temperature" 54.60929 54.60929
# "July.mean.temperature" 54.60929 54.60929
# "Elevation" 36.52909 36.52909  # the smallest resolution. use this for all.
# "Topographic.Wetness" 462.0505 462.0505
# "Slope" 92.4101 92.4101
# "Lithology" 92.4101 92.4101
# "Soil" 92.37313 92.37313 # the soil resolution is arbitrary as based on rasterised polygons

# now separate out the raster lists for the different extents:
raster.list.s = master.raster.list[c(1:6,8)]
raster.list.l = master.raster.list[c(1:7)]
raster.list.i = master.raster.list[c(1:7)]

raster.list.s.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Soil")
raster.list.l.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Lith")
raster.list.i.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Lith")

# loops to crop all rasters to the same extent for each list:

par(mfrow = c(4,2), mar=c(2,2,2,4))
for (i in 1:length(raster.list.s))                                  {
  raster.list.s[[i]] = crop(raster.list.s[[i]], extent(bir.area.s))
  raster.list.s[[i]] = mask(raster.list.s[[i]], bir.area.s)
  plot(raster.list.s[[i]], main = names(raster.list.s[[i]]))        }

par(mfrow = c(4,2), mar=c(2,2,2,4))
for (i in 1:length(raster.list.l))                                  {
  raster.list.l[[i]] = crop(raster.list.l[[i]], extent(bir.area.l))
  raster.list.l[[i]] = mask(raster.list.l[[i]], bir.area.l)
  plot(raster.list.l[[i]], main = names(raster.list.l[[i]]))        }

par(mfrow = c(4,2), mar=c(2,2,2,4))
for (i in 1:length(raster.list.i))                                  {
  raster.list.i[[i]] = crop(raster.list.i[[i]], extent(bir.area.i))
  raster.list.i[[i]] = mask(raster.list.i[[i]], bir.area.i)
  plot(raster.list.i[[i]], main = names(raster.list.i[[i]]))        }

# use resample to make all rasters have the same resolution as the elevation layer
# NOTE: takes several minutes to run

# resampling loop for israel-wide raster list
par(mfrow = c(2,4), mar = c(1,2,2,4))
for (i in c(1,2,3,5,6)) {  # resampling the rest to align with elevation, #4
  raster.list.i[[i]] = resample(raster.list.i[[i]], raster.list.i[[4]], method="bilinear")
  plot(raster.list.i[[i]], main = raster.list.i.names[[i]])
  print(res(raster.list.i[[i]]))  }    # good.
# resampling lithology/soil with a different method to keep its values as integers
raster.list.i[[7]] = resample(raster.list.i[[7]], raster.list.i[[4]], method="ngb")
table(raster.list.i[[7]][]) # check that all values remain integers
plot(raster.list.i[[7]], main = raster.list.i.names[7])
print(res(raster.list.i[[7]]))

#resampling loop for lithology-delimited raster list
par(mfrow = c(2,4), mar = c(1,2,2,4))
for (i in c(1,2,3,5,6)) {  # resampling the rest to align with elevation, #4
  raster.list.l[[i]] = resample(raster.list.l[[i]], raster.list.l[[4]], method="bilinear")
  plot(raster.list.l[[i]], main = names(raster.list.l[[i]]))
  print(res(raster.list.l[[i]]))  }    # good.
# resampling lithology/soil with a different method to keep its values as integers
raster.list.l[[7]] = resample(raster.list.l[[7]], raster.list.l[[4]], method="ngb")
table(raster.list.l[[7]][])
plot(raster.list.l[[7]], main = raster.list.l.names[7])
print(res(raster.list.l[[7]]))

#resampling loop for soils-delimited raster list
par(mfrow = c(2,4), mar = c(1,2,2,4))
for (i in c(1,2,3,5,6)) {  # resampling the rest to align with elevation, #4
  raster.list.s[[i]] = resample(raster.list.s[[i]], raster.list.s[[4]], method="bilinear")
  plot(raster.list.s[[i]], main = names(raster.list.s[[i]]))
  print(res(raster.list.s[[i]]))  }    # good.
# resampling lithology/soil with a different method to keep its values as integers
raster.list.s[[7]] = resample(raster.list.s[[7]], raster.list.s[[4]], method="ngb")
table(raster.list.s[[7]][])
plot(raster.list.s[[7]], main = raster.list.s.names[7])
print(res(raster.list.s[[7]]))


# Stack, make images, and save raster objects ----
# preds.s = stack(raster.list.s); plot(preds.s)
# preds.l = stack(raster.list.l); plot(preds.l)
# preds.i = stack(raster.list.i); plot(preds.i)

# saveRDS(raster.list.s,  paste0(B.heavies.rds.path,"raster.list.s.rds"))
# saveRDS(raster.list.l,  paste0(B.heavies.rds.path,"raster.list.l.rds"))
# saveRDS(raster.list.i,  paste0(B.heavies.rds.path,"raster.list.i.rds"))

raster.list.s  = readRDS(paste0(B.heavies.rds.path,"raster.list.s.rds"))
raster.list.l  = readRDS(paste0(B.heavies.rds.path,"raster.list.l.rds"))
raster.list.i  = readRDS(paste0(B.heavies.rds.path,"raster.list.i.rds"))

# saveRDS(preds.s, paste0(B.heavies.rds.path,"preds.s.rds"))
# saveRDS(preds.l, paste0(B.heavies.rds.path,"preds.l.rds"))
# saveRDS(preds.i, paste0(B.heavies.rds.path,"preds.i.rds"))

preds.s = readRDS(paste0(B.heavies.rds.path,"preds.s.rds")) # raster stack. Slow.
preds.l = readRDS(paste0(B.heavies.rds.path,"preds.l.rds")) # raster stack. Slow.
preds.i = readRDS(paste0(B.heavies.rds.path,"preds.i.rds")) # raster stack. Slow.

# make bricks but don't bother saving them; brick RDSs don't save the content
brick.s = brick(raster.list.s)


# # experimenting with colour palettes:
par(mfrow=c(2,3), mar=c(1,1,1,1))
# plot(b.preds[[2]], col= rev(rainbow(12, s = 1, v = 1, start = 0, end = 1)))      
# plot(b.preds[[2]], col= (rainbow(12, s = 1, v = 1, start = 0, end = 1))) 
# plot(b.preds[[2]], col= rev(heat.colors(12))) 
# plot(b.preds[[2]], col= (heat.colors(12)))
install.packages("dichromat"); require(dichromat)
# plot(b.preds[[2]], col= colorRampPalette(c("blue","red"))(10)) # beauty!
# plot(b.preds[[2]], col= colorRampPalette(c("blue","red"))(20))

# Plot rasters in two rows (for one row: 20 * 5 cm)
png(filename = paste0(B.heavies.image.path,"Variable rasters(two rows)_soil set.png"), 
    width=9, height=10, units='cm',res=600) 
par(mfrow=c(2,4), mar=c(0,0,2,0), bty="n") # sets the bottom, left, top and right margins respectively
buff = bir.area.s; name = raster.list.s.names; preds = preds.s # UPDATE THIS ROW
plot(preds[[1]], col=cm.colors(30),        main=name[[1]], legend=F, axes=F); lines(buff) # Rain
plot(preds[[2]], col=rev(heat.colors(12)), main=name[[2]], legend=F, axes=F); lines(buff) # Jant
plot(preds[[3]], col=rev(heat.colors(12)), main=name[[3]], legend=F, axes=F); lines(buff) # Jult
plot(preds[[4]], col=topo.colors(30),      main=name[[4]], legend=F, axes=F); lines(buff) # DEM
plot(preds[[5]], col=topo.colors(30),      main=name[[5]], legend=F, axes=F); lines(buff) # TWet
plot(preds[[6]], col=topo.colors(50),      main=name[[6]], legend=F, axes=F); lines(buff) # Slop
plot(preds[[7]], col=rainbow(50),          main=name[[7]], legend=F, axes=F); lines(buff) # Soil/Lith
dev.off()

# Plot rasters in one row: (*could shorted this code with plot-running code discovered recently)
png(filename = paste0(B.heavies.image.path,"Variable rasters(one row)_soil set.png"), 
    width=14, height=5, units='cm',res=600) 
par(mfrow=c(1,7), mar=c(0,0,2,0), bty="n") # sets the bottom, left, top and right margins respectively
buff = bir.area.s; name = raster.list.s.names; preds = preds.s
plot(preds[[1]], col=cm.colors(30),        main=name[[1]], legend=F, axes=F); lines(buff) # Rain
plot(preds[[2]], col=rev(heat.colors(12)), main=name[[2]], legend=F, axes=F); lines(buff) # Jant
plot(preds[[3]], col=rev(heat.colors(12)), main=name[[3]], legend=F, axes=F); lines(buff) # Jult
plot(preds[[4]], col=topo.colors(30),      main=name[[4]], legend=F, axes=F); lines(buff) # DEM
plot(preds[[5]], col=topo.colors(30),      main=name[[5]], legend=F, axes=F); lines(buff) # TWet
plot(preds[[6]], col=topo.colors(50),      main=name[[6]], legend=F, axes=F); lines(buff) # Slop
plot(preds[[7]], col=rainbow(50),          main=name[[7]], legend=F, axes=F); lines(buff) # Soil/lith
dev.off()

# Saving the raster objects:
saveRDS(raster.list.l,  paste0(B.heavies.rds.path,"raster.list.l.rds"))
saveRDS(preds.l,        paste0(B.heavies.rds.path,"preds.l.rds")) # raster stack
saveRDS(raster.list.s,  paste0(B.heavies.rds.path,"raster.list.s.rds"))
saveRDS(preds.s,        paste0(B.heavies.rds.path,"preds.s.rds")) # raster stack

#######################################################################################################################
# Importing observation datasets ----
# NOTE, across the scripts, there are three stages of the observations datasets:
# .raw is the full original datasets after being cleaned and fixed, created in script #1
# .nodups after removal of duplicates (done in script #3)
# clean version: eg bi = birulatus survey data ready for modelling

# Also there are three modelling runs based on different extents:
# bs : (smallest) study area based on 20 km buffer on presences, delimited by soils data - this cuts off the golan heights
# bl : (second smallest) study area based on 20 km buffer on presences, lithology used as a proxy for soils
# bi : Israel-wide (excluding waterbodies etc)

# Originally birulatus observations per nest were used for the modelling. As of late 2020/early 2021, we transitioned to having the observations done by site (n = 23-27)

# Birulatus observations data *by nest* ----
bi.raw <- readOGR(dsn="E:/GIS working/layers/birulatus", layer="birulatus_corrected") # reading species obs
head(bi.raw) 
str(bi.raw) 
summary(bi.raw)
par(mfrow=c(1,7)); plot(bi.raw); lines(israel.WB, col="blue")
hist(bi.raw$lat)
bi.raw$occurrence = ifelse(bi.raw$Birulatus == "Yes",1,0) 
table(bi.raw$occurrence, bi.raw$Birulatus) # good
bi.raw$Birulatus = NULL

# Remove observations where absences are too close to presences (they risk dominating them)
bi.raw = bi.raw[ grep ("remove due to proximity", bi.raw$absen_rem, invert = TRUE) , ]
table(bi.raw$occurrence) # 14 of the 122 absence records removed for being too close to presence records.

# full study area/lithology version:
(bi   = bi.raw[bir.area.l, ])   # super-simple spatial subsetting by polygon clip
(bip = bi[bi$occurrence == 1,]) # bip = birulatus presences, within the full study area (as modelling with lithology)
(bia = bi[bi$occurrence == 0,]) # bia = birulatus presences, within the full study area (as modelling with lithology)

# soil-delimited study area version:
(bi.s = bi.raw[bir.area.s, ]) # a few removed that don't fit in the area that can be modelled with the soils layer.
(bip.s = bi.s[bi.s$occurrence == 1,]) # bip.s = birulatus presences, within soils-delimited study area (misses part Golan)
(bia.s = bi.s[bi.s$occurrence == 0,]) # bia.s = birulatus presences, within soild-delimited study area (misses part Golan)

# Plot observations
png(filename = "images/obs 1 - Birulatus survey data by presence-absence_full study area.png", 
    width=10, height=20, units='cm', res=600)
par(mar=c(1,1,2,1), mfrow=c(1,1))
plot(bir.area.l, col = NA, border="darkgreen",
     main="Birulatus survey data (167 features)", cex.main = 1,  font.main= 2) # 167 features for lith set
lines(israel.WB, col="grey", lty=5, lwd=2)
points(bi[bi$occurrence == 1,], col='blue', pch=16, cex=0.7)
points(bi[bi$occurrence == 0,], col='red',  pch=16, cex=0.7)
lines(groads, col="grey73")
points(major.cities, pch=21, col='black', bg='yellow', cex=1.3)
selected.towns = large.towns[large.towns$name == "Tiberias" | large.towns$name == "Afula",]
points(selected.towns,        pch=21, col='black', bg='yellow', cex=0.8)
with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=0.8, offset=0.4)) 
with(selected.towns, text(selected.towns$lat~selected.towns$lon, labels=selected.towns$name,pos=4,cex=0.7, offset=0.4))
legend("topleft", c("Presence (83 observations)", "Absence (84 observations)"), col=c("blue","red"), pch=16, cex=.7)
dev.off()

# saveRDS(bi.raw,  "./rds/bi.raw.bynest.rds") 
# saveRDS(bi,      "./rds/bi.bynest.rds") 
# saveRDS(bip,      "./rds/bip.bynest.rds")
# saveRDS(bia,      "./rds/bia.bynest.rds")
# saveRDS(bi.s,      "./rds/bi.s.bynest.rds")
# saveRDS(bip.s,      "./rds/bip.s.bynest.rds")
# saveRDS(bia.s,      "./rds/bia.s.bynest.rds")

# Import birulatus observations *by site* introduced May 2021 ----
library("readxl")
getwd()
bir.by.site = read_excel("./data/2020 11 16 Birulatus data summarised by site.xlsx")

# prepare coordinates, data, and proj4string
coords <- bir.by.site[ , c("long", "lat")]                   # coordinates
data   <- bir.by.site[ , c("Location","Birulatus")]          # data
crs    <- CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # coordinate reference system

# make the SpatialPointsDataFrame object
b.raw <- SpatialPointsDataFrame(coords      = coords,
                            data        = data, 
                            proj4string = crs)
class(b.raw)
b.raw # 27 features
str(b.raw@data)
b.raw$occurrence = ifelse(b.raw$Birulatus == "Yes",1,0) 
table(b.raw$occurrence, b.raw$Birulatus) # good
b.raw$Birulatus = NULL
table(b.raw$occurrence)

plot(b.raw, pch=21, bg='blue')
with(b, text(b$lat ~ b$long, labels=b$Location, pos=4, cex=0.6, font=1))
plot(bir.area.s, add=T)
plot(bir.area.l, add=T, col=grey)
plot()
plot(b[b@data$Birulatus == "Yes",],  pch=21, bg='green', cex=2, add=T)
plot(b[b@data$Birulatus == "No",],  pch=21, bg='red', cex=2, add=TRUE)

# subset for lithology-delimited study area:
(b.l   = b.raw[bir.area.l, ])

# subset for soils-delimited study area:
(b.s   = b.raw[bir.area.s, ])

par(mar=c(1,1,2,1), mfrow=c(1,1))
plot(b.raw, pch=21, bg='blue')

plot(bir.area.s)
points(b, pch=21, bg='blue')
(b.s   = b[bir.area.s, ])

points(b[bir.area.s, ], pch=21, bg='green')
with(b, text(b$lat ~ b$long, labels=b$Location, pos=4, cex=1, font=2))
#drop Hispin as it's outside the soil-delimited study area boundary:


# legend("bottomright", c("Agriculture","Built-up area","Military","Forestry","Conservation"), pch=22, pt.cex=1.5, 
#         col=c("chocolate","darkgrey","lightslateblue","darkgreen","lightgreen"),
#         pt.bg=c("chocolate","darkgrey","lightslateblue","darkgreen","lightgreen"))
#

# plot the copper column 
plot(spdf, "Birulatus")

# saveRDS(bi.raw,  "./rds/bi.raw.rds") 
# saveRDS(bi,      "./rds/bi.rds") 
# saveRDS(bip,      "./rds/bip.rds")
# saveRDS(bia,      "./rds/bia.rds")
# saveRDS(bi.s,      "./rds/bi.s.rds")
# saveRDS(bip.s,      "./rds/bip.s.rds")
# saveRDS(bia.s,      "./rds/bia.s.rds")