# Birulatus 1: data and functions (& things to do) ----

# 2. organise layers for import
# 3. organise point data for import.

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

birulatus.study  = readRDS("rds/birulatus.study.rds")
xlims = c(35.14081, 35.86214)
ylims = c(31.59407, 33.00496)

major.cities = readRDS("./rds/major.cities.rds")
small.cities = readRDS("./rds/small.cities.rds")
towns        = readRDS("./rds/towns.rds")
villages     = readRDS("./rds/villages.rds")

groads       = readRDS("./rds/groads.rds")

raster.list       = readRDS(paste0(B.heavies.rds.path,"raster.list.rds"))
raster.list.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Lith")
preds             = readRDS(paste0(B.heavies.rds.path,"preds.rds")) # raster stack


# old... to use as templates...
{
s.preds             = readRDS(paste0(heavies.rds.path,"s.preds.rds")) # raster stack

xlims_b = c(34.267142, 35.397332);   saveRDS(xlims_b, "./rds/xlims_b.rds")
ylims_b = c(30.507978, 31.720564);   saveRDS(ylims_b, "./rds/ylims_b.rds")
xlims_s = c(34.271733, 35.324798);   saveRDS(xlims_s, "./rds/xlims_s.rds")  
ylims_s = c(31.126732, 33.107418);   saveRDS(ylims_s, "./rds/ylims_s.rds")

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

bs.full  = readRDS("./rds/bs.full.rds")    # b = beershebensis, s = surveys
bsp.full = readRDS("./rds/bsp.full.rds")  # presences
bsa.full = readRDS("./rds/bsa.full.rds")  # absences

bc.full  = readRDS("./rds/bc.full.rds")   # b = beershebensis, c = collections

#######################################################################################################################
# Importing reference datasets and study area boundaries ----

# Country borders ----
borders   = readOGR(dsn="E:/GIS working/layers/borders",layer="borders_simple_WGS") # no extension for readOGR function
plot(borders) 

israel.WB = readOGR(dsn = "E:/GIS working/layers/borders", layer="Israel_WB_landandwatermerged_WGS")
plot(israel.WB)

israel.WB.merged = readOGR(dsn = "E:/GIS working/layers/borders", layer="Israel_WB_merged_landandwatermerged_WGS")
plot(israel.WB.merged)

israel.WB.no.water = readOGR(dsn = "E:/GIS working/layers/borders", layer="Israel_&_West_Bank_minuswaterbodies_WGS")
plot(israel.WB.no.water)

israel.noWB = borders[borders$Label == "Israel",]; plot(israel.noWB)

saveRDS(borders, "rds/borders.rds");                     saveRDS(israel.WB, "rds/israel.WB.rds")
saveRDS(israel.WB.merged, "./rds/israel.WB.merged.rds"); saveRDS(israel.WB, "./rds/israel.WB.no.water.rds")
saveRDS(israel.noWB, "rds/israel.noWB.rds")

# Transformations:
# borders.ITM = spTransform(borders, ITM)

# Study area ----
birulatus.study = readOGR(dsn="E:/GIS working/layers/birulatus",layer="Birulatus_study_area") # 20 km, amended
plot(birulatus.study)
saveRDS(birulatus.study, "./rds/birulatus.study.rds")

# Cities and other population centres etc ----
population.centres   = readOGR(dsn="E:/GIS working/layers/society",layer="Population_centres")
population.centres$lat = population.centres$Latitude__; population.centres$lon = population.centres$Longitude
par(mar=c(1,1,1,1)); plot(borders)
points(population.centres, pch=21, col='black', bg='yellow')
plot(population.centres$lat~population.centres$lon)
with(population.centres, text(population.centres$lat~population.centres$lon, labels = population.centres$name, pos=4))

major.cities  = population.centres[population.centres$category == "Major city", ]
small.cities  = population.centres[population.centres$category == "City", ]
large.towns   = subset(population.centres, subset = category == "Large town")
towns         = subset(population.centres, subset = category == "Town" | category == "Large town")
villages      = population.centres[population.centres$category == "Village", ]

saveRDS(major.cities, "./rds/major.cities.rds")
saveRDS(small.cities, "./rds/small.cities.rds")
saveRDS(towns, "./rds/towns.rds")
saveRDS(villages, "./rds/villages.rds")

# Roads - detailed (too slow, too detailed):
# roads   = readOGR(dsn="E:/GIS working/layers/infrastructure",layer="OpenstreetMapRoads") # hashed out as too slow and detailed
# plot(roads); saveRDS(roads,"./rds/roads.rds") # detailed dataset - takes ages. Bad layer too.

# Roads - undetailed. from EarthData global dataset: roads between settlements
groads = readOGR(dsn="E:/GIS working/layers/infrastructure/gROADS-v1-europe.gdb",layer="groads_israel"); plot(groads)  
saveRDS(groads, "./rds/groads.rds")

# Landuse data ----
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

par(mar=c(0,0,0,0)); plot(kkl.plans.b, col="pink",border="pink"); plot(kkl.plans.a, add=T, col="red",border="red")
plot(kkl.forestry, add = T, col="brown",border="brown")

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


# landuse.simple       = rmapshaper::ms_simplify(landuse.unsimplified, keep = 0.2, keep_shapes = TRUE)
kkl.forestry_simple  = ms_simplify(kkl.forestry, keep = 0.3, keep_shapes = TRUE)

plot(kkl.forestry)
plot(kkl.forestry_simple, col="yellow", add=T)
plot(kkl.forestry); plot(kkl.plans, col="red", border="red", add=T)

summary(landuse.unsimplified@data$landuse); par(mar=c(0,0,0,0))
landuse = landuse.unsimplified
png(filename = "./output_images/landuse_b.png", width = 12, height = 15, units = 'cm', res = 900)
par(mar=c(0,0,0,0))
plot(landuse[landuse@data$landuse == "built-up area",], col="darkgrey",border="darkgrey",lwd=0.25, xlim=xlims_b,ylim=ylims_b)
plot(landuse_b[landuse_b@data$landuse == "conservation",],  col="lightgreen",     border=NA, add=T)
plot(landuse_b[landuse_b@data$landuse == "military",],      col="lightslateblue", border=NA, add=T)
plot(landuse_b[landuse_b@data$landuse == "forestry",],      col="darkgreen",      border=NA, add=T)
plot(landuse_b[landuse_b@data$landuse == "agriculture",],   col="chocolate",      border=NA, add=T)
plot(major.cities, pch=21, bg='yellow', cex=1, add=TRUE)
with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=0.5, font=2, offset=0.3))
legend("bottomright", c("Agriculture","Built-up area","Military","Forestry","Conservation"), pch=22, pt.cex=1.5, 
        col=c("chocolate","darkgrey","lightslateblue","darkgreen","lightgreen"),
        pt.bg=c("chocolate","darkgrey","lightslateblue","darkgreen","lightgreen"))
dev.off()

# simplifying dataset with rmshaper doesn't work, did in ArcGIS instead:
# landuse_simple = rmapshaper::ms_simplify(landuse_b, keep = 0.2, keep_shapes = TRUE)

{# b.disturbed <- readOGR(dsn="E:/GIS working/layers/disturbance/disturbance.gdb",layer="disturbance_b_diss") 
saveRDS(b.disturbed, paste0(heavies.rds.path,"b.disturbed.rds"))
b.disturbed = readRDS(paste0(heavies.rds.path,"b.disturbed.rds"))
# This version has an issue in that there are 'uncategorized' polygons that overlay 'categorised' ones. 
# But I can manage it - just plot the uncategorized layer at the bottom.
# b.disturbed.sp <- readOGR(dsn="E:/GIS working/layers/disturbance/disturbance.gdb",layer="merged_disturbance_beershebensis_sp") 
# this is the improved version but I need to dissolve it before it is usable - at the moment it is extremely slow to plot.
} # obsolete merged disturbance layer


{
# builtup <- readOGR(dsn="E:/GIS working/layers/disturbance/disturbance.gdb",layer="built_up_areas_updated2") 
# plot(builtup)
# saveRDS(builtup, paste0(heavies.rds.path,"builtup.rds"))
# 
# agriculture <- readOGR(dsn="E:/GIS working/layers/disturbance/disturbance.gdb",layer="agriparcels_dissolve") 
# plot(agriculture)
# saveRDS(agriculture, paste0(heavies.rds.path,"agriculture.rds"))
# 
# dmt_uncat <- readOGR(dsn="E:/GIS working/layers/disturbance/disturbance.gdb",layer="DMT16_admin_divisions_dist_uncat") 
# plot(dmt_uncat)
# saveRDS(dmt_uncat, paste0(heavies.rds.path,"dmt_uncat.rds"))
# 
# dmt_bu_ag_plntn <- readOGR(dsn="E:/GIS working/layers/disturbance/disturbance.gdb",layer="DMT16_landcover_bu_ag_plntn") 
# plot(dmt_bu_ag_plntn)
# saveRDS(dmt_bu_ag_plntn, paste0(heavies.rds.path,"dmt_bu_ag_plntn.rds"))
# 
# INPA_dist <- readOGR(dsn="E:/GIS working/layers/disturbance/disturbance.gdb",layer="INPA_landcover_categories_disturbed") 
# plot(INPA_dist)
# saveRDS(INPA_dist, paste0(heavies.rds.path,"INPA_dist.rds"))
# 
# KKL_ops <- readOGR(dsn="E:/GIS working/layers/disturbance/disturbance.gdb",layer="KKL_operational_areas") 
# plot(KKL_ops)
# saveRDS(KKL_ops, paste0(heavies.rds.path,"KKL_ops.rds"))
# 
# military <- readOGR(dsn="E:/GIS working/layers/planning and landuse/planning_landuse.gdb",layer="military_bases") 
# plot(military)
# saveRDS(military, paste0(heavies.rds.path,"military.rds"))
# 
# rail <- readOGR(dsn="E:/GIS working/layers/disturbance/disturbance.gdb",layer="railways_30m_buffer") 
# plot(rail)
# saveRDS(rail, paste0(heavies.rds.path,"rail.rds"))
# 
# osm_dist <- readOGR(dsn="E:/GIS working/layers/disturbance/disturbance.gdb",layer="OpenStreetMap_landuse_disturbed") 
# plot(osm_dist)
# saveRDS(osm_dist, paste0(heavies.rds.path,"osm_dist.rds"))

# builtup         = readRDS(paste0(heavies.rds.path,"builtup.rds"))
# agriculture     = readRDS(paste0(heavies.rds.path,"agriculture.rds"))
# dmt_uncat       = readRDS(paste0(heavies.rds.path,"dmt_uncat.rds"))
# dmt_bu_ag_plntn = readRDS(paste0(heavies.rds.path,"dmt_bu_ag_plntn.rds"))
# INPA_dist       = readRDS(paste0(heavies.rds.path,"INPA_dist.rds"))
# KKL_ops         = readRDS(paste0(heavies.rds.path,"KKL_ops.rds"))
# military        = readRDS(paste0(heavies.rds.path,"military.rds"))
# rail            = readRDS(paste0(heavies.rds.path,"rail.rds"))
# osm_dist        = readRDS(paste0(heavies.rds.path,"osm_dist.rds"))

# disturbed.raw.layers = list(builtup,agriculture, dmt_uncat, dmt_bu_ag_plntn,
#                             INPA_dist, KKL_ops, military, rail, osm_dist)
# saveRDS(disturbed.raw.layers, paste0(heavies.rds.path,"disturbed.raw.layers.rds"))
# disturbed.raw.layers        = readRDS(paste0(heavies.rds.path,"disturbed.raw.layers.rds"))
} # individual disturbance datasets (obselete)
  
# plot(borders, xlim=xlims_b, ylim=ylims_b, add=T)
# lines(groads, col="grey73")
# plot(villages, pch=21, bg='lightgreen', cex=0.9, add=TRUE)
# plot(towns, pch= 21, bg= 'green', cex = 0.9, add=TRUE)
# plot(small.cities, pch=21, bg='orange', cex=1.4, add=TRUE)
# 

#######################################################################################################################
# Raster data  ----
# note: very slow, best to just reload a previously-run version, see end of section line 

par(mar=c(0,0,2,0), bty="n")
# rain = raster("E:/GIS working/layers/climate/Annual_Rain_1981-2010.tif"); plot(rain); names(rain)="Precipitation"
# saveRDS(rain, paste0(B.heavies.rds.path,"rain.original.rds"))
rain = readRDS(paste0(B.heavies.rds.path,"rain.original.rds"));          plot(rain, main="Rainfall")

# jant = raster("E:/GIS working/layers/climate/av_temp_jan_WGS.tif"); plot(jant); names(jant) = "Jan mean temperature"
# saveRDS(jant, paste0(B.heavies.rds.path,"jant.rds"))
jant = readRDS(paste0(B.heavies.rds.path,"jant.rds"));          plot(jant, main="Jan temperature")

# jult = raster("E:/GIS working/layers/climate/av_temp_july_WGS.tif"); plot(jult); names(jult) ="July mean temperature"
# saveRDS(jult, paste0(B.heavies.rds.path,"jult.rds"))
jult = readRDS(paste0(B.heavies.rds.path,"jult.rds"));          plot(jult, main="July temperature")

# dem = raster("E:/GIS working/layers/topography/DEM_WGS84.tif"); plot(dem); names(dem) ="Elevation"
# saveRDS(dem, paste0(B.heavies.rds.path,"dem.rds"))
dem = readRDS(paste0(B.heavies.rds.path,"dem.rds"));          plot(dem, main="Elevation")

# twet = raster("E:/GIS working/layers/topography/Topo_index_IsraelWB.tif")
# plot(twet); names(twet)="Topographic Wetness"
# saveRDS(twet, paste0(B.heavies.rds.path,"twet.rds"))
twet = readRDS(paste0(B.heavies.rds.path,"twet.rds"));          plot(twet, main="Topographic wetness")

# slop = raster("E:/GIS working/layers/topography/Yamazaki_topo/slope_yk.tif"); plot(slop); names(slop)="Slope"
# saveRDS(slop, paste0(B.heavies.rds.path,"slop.rds"))
slop = readRDS(paste0(B.heavies.rds.path,"slop.rds"));          plot(slop, main="Slope")

# lith = raster("E:/GIS working/layers/geology and soils/lithology_to_raster.tif"); plot(lith); names(lith)="Lithology"
# saveRDS(lith, paste0(B.heavies.rds.path,"lith.rds"))
lith = readRDS(paste0(B.heavies.rds.path,"lith.rds"));          plot(lith, main="Lithology")

# soil, not doing right now:
# soil = raster("E:/GIS working/layers/geology and soils/soils_code2_WGSextended.tif"); plot(soil)
# names(soil) = "Soil.type"
# soil original processes to fill NA cells with process below. Read in output from there: 'filled' version 
# soil = readRDS("./rds/soil.filled.rds");            plot(soil, main="soil")

# Special job to check and deal with 'NA' '128' values in categorical lithology (and soil) layers ----
# A) Lithology:
plot(lith, main= "Lithology")
table(lith[]); summary(lith[]) 
# values go up to 34, then 128 is the 'NA'; there's 2574700 na-s. But are there NAs in study area?
length(lith[lith == 128])  # 2,574,700 
lith.crop = crop(lith, extent(birulatus.study))

length(lith.mask[lith.mask == 128])  # 0. So no filling needed for lithology layer.
lsos() # just get rid of what I don't need right now:
rm(lith.crop); rm(lith.mask)

# B) Soil: maybe later:
{
# soil = all.rasters[[7]] # only to check. The all.rasters layer should already be cleaned up.
table(soil[]); summary(soil[]) # values go up to 124, then 128 is the 'NA' value. or from filled version, just 43,812,027 NAs.
plot(soil, main= "soil")   #
length(soil[soil == 128])  # 44,590,216 dont have these any more
# soil.nas = soil; soil.nas[soil.nas == 128] = NA # convert all 128s to NAs.
# length(soil.nas[soil.nas == 128]) # 0. confirm it's done

# Multiple rounds of filling (running again to remove persistent voids)
soil.filled  = focal(soil,     w=matrix(1,7,7), fun = modal, na.rm=TRUE, NAonly=TRUE, pad=TRUE) # round 1
soil.filled2 = focal(soil.filled,  w=matrix(1,7,7), fun = modal, na.rm=TRUE, NAonly=TRUE, pad=TRUE) # round 2
soil.filled3 = focal(soil.filled2, w=matrix(1,7,7), fun = modal, na.rm=TRUE, NAonly=TRUE, pad=TRUE) # round 3
soil.filled4 = focal(soil.filled3, w=matrix(1,7,7), fun = modal, na.rm=TRUE, NAonly=TRUE, pad=TRUE) # round 4
soil.filled5 = focal(soil.filled4, w=matrix(1,7,7), fun = modal, na.rm=TRUE, NAonly=TRUE, pad=TRUE) # round 5
soil.filled6 = focal(soil.filled5, w=matrix(1,7,7), fun = modal, na.rm=TRUE, NAonly=TRUE, pad=TRUE) # round 6

table(soil[]);         summary(soil[])          # 27652943 cells of '128'
table(soil.nas[]);     summary(soil.nas[])      # 27652943 NAs
table(soil.filled[]);  summary(soil.filled[])   # 27538832 NAs, ie got rid of 114,111 NAs.
table(soil.filled3[]); summary(soil.filled4[])  # should be well-filled by now

# convert NAs back to a number (as they don't plot as NAs)
soil.filled.999 = soil.filled
soil.filled.999[is.na(soil.filled.999)] = 999
length(soil.filled.999[soil.filled.999 == 999])

# now crop and mask and see if there are still any in either species' buffer:
soil.filled.crop.b = crop(soil.filled.999, extent(beershebensis.buffer))
soil.filled.mask.b = mask(soil.filled.crop.b, beershebensis.buffer)
summary(soil.filled.mask.b[])
length(soil.filled.mask.b[soil.filled.mask.b == 999])  # 15753630 # all '999's gone! finally, after 6 rounds of filling.
plot(soil.filled.mask.b, main = "soil.filled.cropped.masked")

# soil.filled.crop.s = crop(soil.filled.999, extent(schreiberi.buffer))
# soil.filled.mask.s = mask(soil.filled.crop.s, schreiberi.buffer)
# summary(soil.filled.mask.s[]) # all '999's gone! that means that the only ones that remained were outside of study area.
# plot(soil.filled.mask.s, main = "soil.filled.cropped.masked")

# delete the objects I no longer need:
lsos()
rm(soil.filled, soil.filled2, soil.filled3, soil.filled4, soil.filled5)
rm(soil.filled.999,soil.filled3.999,soil.filled4.999)
rm(soil.filled5.999,soil.filled6,soil.nas,soil.filled.crop.s,soil.filled.mask.s)
rm(soil.filled.crop.b,soil.filled.mask.b)
gc()

# replace the soil layer with my new improved one:
soil = soil.filled
names(soil) = "soil"
saveRDS(soil, "./rds/soil.filled.rds")
soil = readRDS("./rds/soil.filled.rds")
# b.raster.list[[6]] = soil
# b.raster.list[[7]] = soil
}
# # end of special job to deal with 'NA' '128' values in vegetation layer.

# Manipulate raster files ----

# get all raster files into the same coordinate system:
crs(rain) # "ellps=GRS80", need to reproject to WGS 84
crs(jant) # good "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" the projection we want= WGS1984.
crs(jult) # good
crs(dem)  # good
crs(twet) # good
crs(slop) # good
crs(lith) # good 

# reproject rain raster:
rain = projectRaster(rain, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"); crs(rain) # good

# make lists for loopy manipulation:
# raster.list     = list(rain, jant, jult, dem, twet, slop, lith)
# saveRDS(raster.list,  paste0(B.heavies.rds.path,"raster.list.rds"))
raster.list = readRDS(paste0(B.heavies.rds.path,"raster.list.rds"))
raster.list.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Lith")
plot(raster.list[[2]], main = names(raster.list[[2]]))
lsos()

# Align the resolutions. Here, i've put them in order of how important resolution might be:
deg = 110865.64762119074
res(dem)  * deg # 36.5 metres. smallest resolution. I'll go with this for now. This is raster.list #4.
res(lith) * deg # 92.4 metres. I'll go with this as the standard resolution
res(slop) * deg # 92.4 metres. more useful without being over the top.
res(rain) * deg # 291 metres
res(twet) * deg # 461 metres
res(jult) * deg # 54 metres
res(jant) * deg # 54 metres
names(raster.list[[4]])

# use resample to make all rasters have the same resolution as the DEM layer NOTE: takes several minutes to run
par(mfrow = c(2,4), mar = c(1,2,2,4))
for (i in c(1,2,3,5,6,7))       {  # resampling the rest to align with dem, which is number 4
  raster.list[[i]] = resample(raster.list[[i]], raster.list[[4]], method="bilinear")
  plot(raster.list[[i]], main = raster.list.names[[i]])
  print(res(raster.list[[i]]))  }    # good.
  
# crop all rasters to the same extent:
par(mfrow = c(2,4), mar=c(2,2,2,4))
for (i in 1:length(raster.list))                                            {
  raster.list[[i]] = crop(raster.list[[i]], extent(birulatus.study))
  raster.list[[i]] = mask(raster.list[[i]], birulatus.study)
  plot(raster.list[[i]], main = raster.list.names[[i]])                     }

# Stack, make images, and save raster objects ----
preds = stack(raster.list); plot(preds)
# saveRDS(preds, paste0(B.heavies.rds.path,"preds.rds"))
b.preds = readRDS(paste0(B.heavies.rds.path,"preds.rds")) # retrieve raster stack. Slow.

# make bricks but don't bother saving or retreiving them; brick RDSs don't save the content
brick = brick(raster.list)

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
png(filename = paste0(B.heavies.image.path,"Variable rasters(two rows).png"), width=9, height=10, units='cm',res=600) 
par(mfrow=c(2,4), mar=c(0,0,2,0), bty="n") # sets the bottom, left, top and right margins respectively
buff = birulatus.study; name = raster.list.names
plot(preds[[1]], col=cm.colors(30),        main=name[[1]], legend=F, axes=F); lines(buff) # Rain
plot(preds[[2]], col=rev(heat.colors(12)), main=name[[2]], legend=F, axes=F); lines(buff) # Jant
plot(preds[[3]], col=rev(heat.colors(12)), main=name[[3]], legend=F, axes=F); lines(buff) # Jult
plot(preds[[4]], col=topo.colors(30),      main=name[[4]], legend=F, axes=F); lines(buff) # TWet
plot(preds[[5]], col=topo.colors(30),      main=name[[5]], legend=F, axes=F); lines(buff) # Slop
plot(preds[[6]], col=topo.colors(50),      main=name[[6]], legend=F, axes=F); lines(buff) # Soil
plot(preds[[7]], col=rainbow(50),          main=name[[7]], legend=F, axes=F); lines(buff) # Soil
dev.off()

# Plot rasters in one row: (*could shorted this code with plot-running code discovered recently)
png(filename = paste0(B.heavies.image.path,"Variable rasters(one row).png"), width=14, height=5, units='cm',res=600) 
par(mfrow=c(1,7), mar=c(0,0,2,0), bty="n") # sets the bottom, left, top and right margins respectively
buff = birulatus.study; name = raster.list.names
plot(preds[[1]], col=cm.colors(30),        main=name[[1]], legend=F, axes=F); lines(buff) # Rain
plot(preds[[2]], col=rev(heat.colors(12)), main=name[[2]], legend=F, axes=F); lines(buff) # Jant
plot(preds[[3]], col=rev(heat.colors(12)), main=name[[3]], legend=F, axes=F); lines(buff) # Jult
plot(preds[[4]], col=topo.colors(30),      main=name[[4]], legend=F, axes=F); lines(buff) # TWet
plot(preds[[5]], col=topo.colors(30),      main=name[[5]], legend=F, axes=F); lines(buff) # Slop
plot(preds[[6]], col=topo.colors(50),      main=name[[6]], legend=F, axes=F); lines(buff) # Soil
plot(preds[[7]], col=rainbow(50),          main=name[[7]], legend=F, axes=F); lines(buff) # Soil
dev.off()

# Saving the raster objects:
saveRDS(raster.list,  paste0(B.heavies.rds.path,"raster.list.rds"))
saveRDS(preds,        paste0(B.heavies.rds.path,"preds.rds")) # raster stack

#######################################################################################################################
# Importing observation datasets ----
# NOTE, across the scripts, there are three versions of the observations datasets:
# .raw means the full original datasets after they have been cleaned and fixed, created in script #1
# .nodups after removal of duplicates (done in script #3)
# clean version: eg bi = birulatus survey data ready for modelling

# Birulatus observations data ----
bi.raw <- readOGR(dsn="E:/GIS working/layers/birulatus", layer="birulatus_corrected") # reading species obs
head(bi.raw) 
str(bi.raw) 
summary(bi.raw)
plot(bi.raw); lines(israel.WB, col="blue")

bi = bi.raw[birulatus.study, ] # super-simple spatial subsetting by polygon clip
bi$occurrence = ifelse(bi$Birulatus == "Yes",1,0) 
table(bi$occurrence, bi$Birulatus) # good
bi$Birulatus = NULL

# Plot observations
png(filename = "images/obs 1 - Birulatus survey data by presence-absence.png", width=10, height=20, units='cm', res=600)
par(mar=c(1,1,2,1), mfrow=c(1,1))
plot(birulatus.study, col = NA,border="darkgreen",
     main="Birulatus survey data (181 features)", cex.main = 1,  font.main= 2)
lines(israel.WB, col="grey", lty=5, lwd=2)
points(bi[bi$occurrence == 1,],col='blue',pch=16, cex=0.7)
points(bi[bi$occurrence == 0,],col='red',pch=16, cex=0.7)
lines(groads, col="grey73")
points(major.cities, pch=21, col='black', bg='yellow', cex=1.3)
selected.towns = large.towns[large.towns$name == "Tiberias" | large.towns$name == "Afula",]
points(selected.towns,        pch=21, col='black', bg='yellow', cex=0.8)
with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=0.6, offset=0.3)) 
with(selected.towns, text(selected.towns$lat~selected.towns$lon, labels=selected.towns$name,pos=4,cex=0.6, offset=0.3)) 
legend("topleft", c("Presence (83 observations)", "Absence (98 observations)"), col=c("blue","red"), pch=16, cex=.7)
dev.off()

# saveRDS(bi.raw,  "./rds/bi.raw.rds") 
# saveRDS(bi,      "./rds/bi.rds")

par(mar=c(1.5,1.5,1.5,1.5), mfrow = c(1,1))
plot  (bc.full, pch=16, cex=1)
lines(borders, lty=5, lwd=2, col="grey15") # hmm, some outliers here.
hist(bc.full$lat) # there are two outliers with latitude below 30 degrees (ie way outside of israel). Remove.
 

# plot by source:
png(filename = "./output_images/obs 2a - Beersheb coll. data by source.png", width=10, height=9, units='cm', res=600)
summary(bc.full@data$source)
par(mar=c(1,1,1,1), mfrow = c(1,1))
plot  (bc.full, pch=16, cex=.2,
      main="Beershebensis collections data (316 features: two outliers removed)",cex.main = .6,  font.main= 2) 
lines(borders, lty=1, lwd=1.5, col="seashell4")
lines(groads,  col="grey73")
points(major.cities, pch=21, col='black', bg='yellow')
with  (major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=0.5, offset=0.3))
points(bc.full[bc.full$source == "HUJR",], col='green',  pch=16, cex=0.5)
points(bc.full[bc.full$source == "TAU",],  col='purple', pch=16, cex=0.5)
legend("bottomright", c("HUJR (133 observations)", "TAU (181 observations)"), col=c("green","purple"), pch=16, cex=.5)
dev.off()

# plot by accuracy:
png(filename="./output_images/obs 2b - Beersheb coll. data by accuracy.png", width=10, height=9, units='cm', res=600)
summary(bc.full@data$positional_accuracy)
par(mar=c(1,1,1,1), mfrow = c(1,1))
plot  (bc.full, pch=16, cex=.1,
      main="Beershebensis collections data by accuracy", cex.main=0.6,  font.main=2)
lines(borders, lty=5, lwd=1.5, col="seashell4")
lines(groads,  col="grey73")
points(major.cities, pch=21, col='black', bg='yellow')
with  (major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=.5, offset=0.3))#,font=2
points(bc.full[bc.full$positional_accuracy == "low accuracy",],col='red',        pch=16, cex=.5)
points(bc.full[bc.full$positional_accuracy == "moderate accuracy",],col='green', pch=16, cex=.5)
points(bc.full[bc.full$positional_accuracy == "high accuracy",],col='blue',      pch=16, cex=.5)
legend("bottomright", c("high accuracy (13)", "moderate accuracy (31)","low accuracy (270)"),
      col=c("blue","green","red"), pch=16, cex=.5, text.font=1)
dev.off()

# saveRDS(bc.full,"./rds/bc.full.rds")

# Summary of observational datasets ----
 
# Beershebensis survey data:       202 features: 75 presences, 127 absences 
# Beershebensis collections data:  316 features: two outliers removed. 
#                                  Positional accuracy:13 high,31 moderate,272 low.
