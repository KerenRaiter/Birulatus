# Birulatus 1: data and functions ----

#######################################################################################################################
# Set up and install relevant packages ----

# ipak function: install (if not already installed) and load multiple R packages.
ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)}
ipak(c("stringr","usdm", "biomod2", "raster", "rgdal", "scales", "grid", "foreign","dplyr","magrittr","tidyr","rgeos",
       "magrittr","ggplot2","gridExtra","raster","rasterVis","dismo","sdm","installr","knitr","ggmap","OpenStreetMap",
       "parallel","beepr","rmapshaper", "spatialEco")) # removed sf, may be causing problems
# install.packages("rJava"); library(rJava)
# install.packages("rgdal"); library(rgdal)
# installed.packages()
installAll() # installing everything the sdm relies on.

# memory.limit()
# memory.limit(500000)

heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

ITM = CRS("+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.33077,-1.85269,1.66969,5.4248 +units=m +no_defs")

########################################################################################################################
# Datasets I prepared earlier in this script ----

b.raster.list.names = list("Rain", "Jant", "Jult","TWet", "Slop", "Soil")
b.raster.list       = readRDS(paste0(heavies.rds.path,"b.raster.list.rds"))
b.preds             = readRDS(paste0(heavies.rds.path,"b.preds.rds")) # raster stack
s.raster.list.names = list("Rain", "Jant", "Jult", "TWet", "Slop", "Soil", "Vegt")
s.raster.list       = readRDS(paste0(heavies.rds.path,"s.raster.list.rds"))
s.preds             = readRDS(paste0(heavies.rds.path,"s.preds.rds")) # raster stack

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

########################################################################################################################
# Importing reference datasets and study area boundaries ----

# Country borders:
# borders   = readOGR(dsn="E:/GIS working/layers/borders",layer="borders_simple_WGS"); plot(borders) # no extension for readOGR function
# saveRDS(borders, "./rds_objects/borders.rds")

# israel.noWB = borders[borders$Label == "Israel",]
# saveRDS(israel.noWB, "./rds_objects/israel.noWB.rds")
# borders.ITM = spTransform(borders, ITM)

# beershebensis.buffer = readOGR(dsn="E:/GIS working/layers/shnuniot",layer="B_allpresences_buffer_30km_incWB")
# plot(beershebensis.buffer)
# saveRDS(beershebensis.buffer, "./rds_objects/beershebensis.buffer_incWB.rds")

# schreiberi.buffer   = readOGR(dsn="E:/GIS working/layers/shnuniot",layer="S_allpresences_buffer_20km_revised")
# plot(schreiberi.buffer)
# saveRDS(schreiberi.buffer,    "./rds_objects/schreiberi.buffer.rds")

# cities and other population centres etc:
# population.centres   = readOGR(dsn="E:/GIS working/layers/society",layer="Population_centres")
# population.centres$lat = population.centres$Latitude__; population.centres$lon = population.centres$Longitude
# plot(borders)
# points(population.centres, pch=21, col='black', bg='yellow')
# plot(population.centres$lat~population.centres$lon)
# with(population.centres, text(population.centres$lat~population.centres$lon, labels = population.centres$name, pos = 4))
# 
# major.cities  = population.centres[population.centres$category == "Major city", ]
# small.cities  = population.centres[population.centres$category == "City", ]
# towns         = subset(population.centres, subset = category == "Town" | category == "Large town")
# villages      = population.centres[population.centres$category == "Village", ]
# 
# saveRDS(major.cities, "./rds_objects/major.cities.rds")
# saveRDS(small.cities, "./rds_objects/small.cities.rds")
# saveRDS(towns, "./rds_objects/towns.rds")
# saveRDS(villages, "./rds_objects/villages.rds")

# Roads - detailed (too slow, too detailed):
# roads   = readOGR(dsn="E:/GIS working/layers/infrastructure",layer="OpenstreetMapRoads") # hashed out as too slow and detailed
# plot(roads); saveRDS(roads,"./rds_objects/roads.rds") # detailed dataset - takes ages. Bad layer too.

# Roads - undetailed. from EarthData global dataset: roads between settlements
# groads <- readOGR(dsn="E:/GIS working/layers/infrastructure/gROADS-v1-europe.gdb",layer="groads_israel"); plot(groads)  
# saveRDS(groads, "./rds_objects/groads.rds")

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

# Load rasters (or skip two steps and use pre-loaded raster list) ----
# rain = raster("E:/GIS working/layers/climate/Annual_Rain_1981-2010.tif")
# saveRDS(rain, "./rds_objects/rain.original.rds")
rain = readRDS("./rds_objects/rain.original.rds");          plot(rain, main="Rainfall")
names(rain) = "Precipitation"

# jant = raster("E:/GIS working/layers/climate/av_temp_jan_WGS.tif")
# saveRDS(jant, "./rds_objects/jant.original.rds")
jant = readRDS("./rds_objects/jant.original.rds");          plot(jant, main="January average temperature")
names(jant) = "Jan.temps"

# jult = raster("E:/GIS working/layers/climate/av_temp_july_WGS.tif")
# saveRDS(jult, "./rds_objects/jult.original.rds")
jult = readRDS("./rds_objects/jult.original.rds");          plot(jult, main="July average temperature")
names(jult) = "July.temps"

# dem  = raster("E:/GIS working/layers/topography/DEM_WGS84.tif")
# saveRDS(dem, "./rds_objects/dem.original.rds")
b.preds.wdem = readRDS("//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/b.preds (w.DEM).rds")
dem = b.preds.wdem[[4]]; plot(dem)
names(dem) = "DEM"
# saveRDS(dem, "//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/dem.beershebensis.rds")

# twet = raster("E:/GIS working/layers/topography/Topo_index_IsraelWB.tif")
# saveRDS(twet, "./rds_objects/twet.original.rds")
twet = readRDS("./rds_objects/twet.original.rds");          plot(twet, main="topographic wetness index")
names(twet) = "T.Wetness"

# slop = raster("E:/GIS working/layers/topography/Yamazaki_topo/slope_yk.tif")
# saveRDS(slop, "./rds_objects/slop.original.rds")
slop = readRDS("./rds_objects/slop.original.rds");          plot(slop, main="topographic slope based on improved dem")
names(slop) = "Slope"

# soil = raster("E:/GIS working/layers/geology and soils/soils_code2_WGSextended.tif"); plot(soil)
# soil original processes to fill NA cells with process below. Read in output from there: 'filled' version 
soil = readRDS("./rds_objects/soil.filled.rds");            plot(soil, main="soil")
names(soil) = "Soil.type"

# plot(soil, main="soil", xlim=c(34.4,35.3), ylim=c(30.9,31.6)) # extra plot, just checking something...
# points(bsp,   bg  = 'yellow', pch=22,  cex=0.8)
# points(bsa,   bg = 'cyan',    pch=21,  cex=0.6)
# points(bsa.r, bg = 'blue',    pch=21,  cex=0.6)
# points(bc,    bg = 'green',   pch=24,  cex=0.6)
# points(bc.r,  bg = 'green4',  pch=24,  cex=0.6)

# vegt = raster("E:/GIS working/layers/biota/vegt_zomech_expanded.tif"); plot(vegt, main="vegetation type")# works. lots of 128s.
# vegt = raster("Z:/keren/R/sdm_archive/vegt_zomech_expanded.tif")
# vegt original processes to fill NA cells with process below. Read in output from there: 'filled' version 
vegt = readRDS("E:/R/sdm_edrive/rds_objects/vegt.filled.rds");                plot(vegt)
names(vegt) = "Veg.type"

all.rasters = list(rain, jant, jult, dem, twet, slop, soil, vegt)
saveRDS(all.rasters,  "./rds_objects/all.rasters.rds")
all.rasters = readRDS("./rds_objects/all.rasters.rds"); 
plot(all.rasters[[2]], main = names(all.rasters[[2]]))
names(all.rasters[[8]])
lsos()

# Special job to deal with 'NA' '128' values in soil and vegetation layer ----
# A) Soil:
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

# replace the soilt layer with my new improved one:
soil = soil.filled
names(soil) = "soil"
saveRDS(soil, "./rds_objects/soil.filled.rds")
soil = readRDS("./rds_objects/soil.filled.rds")
# b.raster.list[[6]] = soil
# b.raster.list[[7]] = soil

# # B) Veg:
# # the issue is that there are a whole lot of NAs surrounding the valued cells of the raster (e.g. in Mediterranean ocean), plus a few 'NAs' (currently labelled '128') in the part of the raster that we care about.
# # I've dealt with most of the problem by expanding the veg polygons, and constraining the study area (shreiberi buffer) as much as possible in ArcGIS. But there remain 487 '128s' that I need to deal with here.
# vegnew = vegt
# # table(vegnew[]); summary(vegnew[]) # 200437929 '128's, no 'NA's.
# # plot(vegnew, main = "vegnew")   
# # length(vegnew[vegnew == 128])  # 487
# # table(vegnew[]); summary(vegnew[])
# # plot(vegnew, main= "vegnew")   # 200437929 '128's and no 'NA's.
# # length(vegnew[vegnew == 128])  # 200437929
# veg.nas = vegnew; veg.nas[veg.nas == 128] = NA # convert all 128s to NAs.
# # length(veg.nas[veg.nas == 128]) # confirm it's done
# 
# # Multiple rounds of filling (multiple required to remove persistent voids)
# veg.filled =  focal(veg.nas,     w=matrix(1,7,7), fun = modal, na.rm=TRUE, NAonly=TRUE, pad=TRUE) # first round seemed to work
# # veg.filled2 = focal(veg.filled,  w=matrix(1,7,7), fun = modal, na.rm=TRUE, NAonly=TRUE, pad=TRUE) # second round (if needed)
# # summary(veg.filled[]) 
# # table(vegnew[]);      summary(vegnew[])
# # table(veg.nas[]);     summary(veg.nas[])
# # table(veg.filled[]);  summary(veg.filled[]) # 16913076-16987063 got rid of 73,000 NAs.
# # table(veg.filled2[]); summary(veg.filled2[])
# 
# # convert NAs back to a number (as they don't plot as NAs)
# veg.filled.999 = veg.filled
# veg.filled.999[is.na(veg.filled.999)] = 999
# 
# # crop and mask and see if there are still any 
# veg.filled.crop = crop(veg.filled.999, extent(schreiberi.buffer))
# veg.filled.mask = mask(veg.filled.crop, schreiberi.buffer)
# summary(veg.filled.mask[]) # all '999's gone! that means that the only ones that remained were outside of study area.
# length(veg.filled.mask[veg.filled.mask == 999])
# plot(veg.filled.mask, main = "veg.filled.cropped.masked") 
# 
# # replace the vegt layer with my new improved one:
# vegt = veg.filled
# writeRaster(vegt, filename='./rds_objects/vegt.filled.tif', format="GTiff", overwrite=TRUE)  
# saveRDS(vegt, "./rds_objects/vegt.filled.rds") # rds version doesn't seem to produce a viable object.
# # end of special job to deal with 'NA' '128' values in vegetation layer.

# Manipulate raster files ----
# If don't already have the individual rasters loaded, then get pre-made list and deconstruct:


# get all raster files into the same coordinate system:
crs(rain) # "ellps=GRS80", therefore there is a projection issue
crs(jant) # good "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" this is the projection we want= WGS1984.
crs(jult) # good
crs(dem)  # good
crs(twet) # good
crs(slop) # good
crs(soil) # good 
crs(vegt) # good

rain = projectRaster(rain, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"); crs(rain) # good

# make lists for loopy manipulation:

b.raster.list     = list(rain, jant, jult, twet, slop, soil)

# s.raster.list = list(rain, jant, jult, twet, slop, soil, vegt)
plot(s.raster.list[[7]])
names(s.raster.list[[7]]) = "Vegt"

# Align the resolutions. Here, i've put them in order of how important resolution might be:
deg = 110865.64762119074
# res(soil) * deg # 32 metres. I'll go with this as the standard resolution
# res(slop) * deg # 92 metres. more useful without being over the top.
# res(vegt) * deg # 11 metres. smallest resolution. too small to be useful, I think.
# res(rain) * deg # 291 metres
# res(topo) * deg # 461 metres.
# res(jult) * deg # 54 metres.
# res(jant) * deg # 54 metres 

# use resample to make the same resolution for beershebensis. NOTE: takes several minutes to run
par(mfrow = c(2,4))
for (i in c(1,2,3,4,5,6))                             {  # we're resampling the rest to align with soils, which is number 6/7
  b.raster.list[[i]] = resample(b.raster.list[[i]], b.raster.list[[7]], method="bilinear")
  plot(b.raster.list[[i]], main = b.raster.list.names[[i]])
  print(res(b.raster.list[[i]]))                      }    # good.
  
# again for schreiberi. 
par(mfrow = c(2,4))
for (i in c(1,2,3,4,5))                               {  # resampling first 5 to align w soils, which is # 6. 7 separate 
  s.raster.list[[i]] = resample(s.raster.list[[i]], s.raster.list[[6]], method="bilinear")
  plot(s.raster.list[[i]], main = s.raster.list.names[[i]])
  print(res(s.raster.list[[i]]))                      } 
# do vegt separately as being a categorical variable, it requires a different resampling method:
s.raster.list[[7]] <- resample(s.raster.list[[7]], s.raster.list[[6]], method="ngb")
plot(s.raster.list[[7]], main=s.raster.list.names[[7]])

# cropping for beershebensis:
par(mfrow = c(2,4), mar=c(2,1,2,2))
for (i in 1:length(b.raster.list))                                               {
  b.raster.list[[i]] = crop(b.raster.list[[i]], extent(beershebensis.buffer))
  b.raster.list[[i]] = mask(b.raster.list[[i]], beershebensis.buffer)
  plot(b.raster.list[[i]], main = b.raster.list.names[[i]])                     }

# cropping for schreiberi:
par(mfrow = c(2,4))
for (i in 1:length(s.raster.list))                                             {
  s.raster.list[[i]] = crop(s.raster.list[[i]], extent(schreiberi.buffer))
  s.raster.list[[i]] = mask(s.raster.list[[i]], schreiberi.buffer)
  plot(s.raster.list[[i]], main = s.raster.list.names[[i]])                     }

# Stack, make images, and save raster objects ----

b.preds = stack(b.raster.list); plot(b.preds) # or:
# b.preds = readRDS(paste0(heavies.rds.path,"b.preds.rds")) # retrieve raster stack. Slow.
s.preds = stack(s.raster.list); plot(s.preds) # make raster stack, or:
# s.preds = readRDS(paste0(heavies.rds.path,"s.preds.rds")) # retreive raster stack. Slow.

# make bricks but don't bother saving or retreiving them; brick RDSs don't save the inherent data
b.brick = brick(b.raster.list)
s.brick = brick(s.raster.list)

# plot beershebensis rasters:
# png(filename = "b variable rasters.png", width = 16, height = 13, units = 'cm', res = 300)
plot(b.preds, main=b.raster.list.names)
# dev.off() # stupid plot arrangement

# # experimenting with colour palettes:
par(mfrow=c(2,3), mar=c(1,1,1,1))
# plot(b.preds[[2]], col= rev(rainbow(12, s = 1, v = 1, start = 0, end = 1)))      
# plot(b.preds[[2]], col= (rainbow(12, s = 1, v = 1, start = 0, end = 1))) 
# plot(b.preds[[2]], col= rev(heat.colors(12))) 
# plot(b.preds[[2]], col= (heat.colors(12)))
install.packages("dichromat"); require(dichromat)
# plot(b.preds[[2]], col= colorRampPalette(c("blue","red"))(10)) # beauty!
# plot(b.preds[[2]], col= colorRampPalette(c("blue","red"))(20))

b.preds.D.names = list("Rain", "Jult", "DEM", "TWet", "Slop", "Soil")

# Better plot (for one row: 20 * 5 cm)
png(filename = paste0(heavies.image.path,"b variable rasters(two rows).png"), width=12, height=10, units='cm', res=600) 
par(mfrow=c(2,3), mar=c(0,0,2,0), bty="n") # sets the bottom, left, top and right margins respectively
buff = beershebensis.buffer
plot(b.preds.D[[1]], col=cm.colors(30),        main=b.preds.D.names[[1]], legend=F, axes=F); lines(buff) # Rain
plot(b.preds.D[[2]], col=rev(heat.colors(12)), main=b.preds.D.names[[2]], legend=F, axes=F); lines(buff) # Jant
plot(b.preds.D[[3]], col=topo.colors(30),      main=b.preds.D.names[[3]], legend=F, axes=F); lines(buff) # Jult
plot(b.preds.D[[4]], col=topo.colors(30),      main=b.preds.D.names[[4]], legend=F, axes=F); lines(buff) # TWet
plot(b.preds.D[[5]], col=topo.colors(30),      main=b.preds.D.names[[5]], legend=F, axes=F); lines(buff) # Slop
plot(b.preds.D[[6]], col=topo.colors(50),      main=b.preds.D.names[[6]], legend=F, axes=F); lines(buff) # Soil
dev.off()

# Plot Shreiberi predictor variables:
png(filename = "./output_images/s variable rasters.png", width = 20, height = 16, units = 'cm', res = 600)
par(mfrow=c(2,4),  mar=c(2,2.5,2.5,4.5)) 
buff = schreiberi.buffer
plot(s.preds[[1]], main="Precipitation",     col= topo.colors(30))   ; lines(buff, lwd=0.5)  
plot(s.preds[[2]], main="Jan temps",         col= rev(rainbow(100))) ; lines(buff, lwd=0.5)
plot(s.preds[[3]], main="July temps",        col= rev(rainbow(100))) ; lines(buff, lwd=0.5)
plot(s.preds[[4]], main="Topographic index", col= topo.colors(30))   ; lines(buff, lwd=0.5)
plot(s.preds[[5]], main="Slope",             col= topo.colors(30))   ; lines(buff, lwd=0.5)
plot(s.preds[[6]], main="Soil types",        col= terrain.colors(6)) ; lines(buff, lwd=0.5)
plot(s.preds[[7]], main="Vegetation types",  col= rainbow(19))       ; lines(buff, lwd=0.5)
dev.off()

# Saving the raster objects:

 saveRDS(b.raster.list,  paste0(heavies.rds.path,"b.raster.list.rds"))
 saveRDS(b.preds,        paste0(heavies.rds.path,"b.preds.rds")) # raster stack

 saveRDS(s.raster.list,  paste0(heavies.rds.path, "s.raster.list.rds"))
 saveRDS(s.preds,        paste0(heavies.rds.path, "s.preds.rds")) # raster stack
 
#########################################################################################################################
# Importing observation datasets ----
 # NOTE, accross the scripts, there are three versions of the observations datasets:
 # .full means the full original datasets after they have been cleaned and fixed, created in script #1
 # .nodups after removal of duplicates (done in script #3)
 # clean version: eg bs = beershebensis survey data; bc = beershebensis collections data, ss = schreiberi survey data, 
 #                   sc = schreiberi collections data.
 
# Beershebensis survey data (bs) ----
bs.full <- readOGR(dsn="E:/GIS working/layers/shnuniot/shnuniot.gdb",layer="B_surveys") # reading species survey data from geodatabase feature class
head(bs.full) 
str(bs.full) 
summary(bs.full)
bs.full$occurrence = ifelse(bs.full$presence == "present",1,0)

bsp.full = bs.full[bs.full$presence == "present",]; length(bsp.full) 
bsa.full = bs.full[bs.full$presence == "absent",];  length(bsa.full) 

par(mar=c(1,1,1,1), mfrow=c(1,1))
png(filename = "./output_images/obs 1 - Beershebensis survey data by presence-absence.png", width = 20, height = 20, units = 'cm', res = 600)
par(mfrow=c(1,1), mar= c(2,2,2,2))
plot  (bs.full, pch=16, cex=.6,
       main="Beershebensis survey data (202 features: 75 presences, 127 absences)",cex.main = 1,  font.main= 2)
points(bs.full[bs.full$presence == "present",],col='blue',pch=16, cex=0.7)
points(bs.full[bs.full$presence == "absent",],col='red',pch=16, cex=0.7)
lines(borders, lty=5, lwd=2, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=21, col='black', bg='yellow')
with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=0.4, offset=0.3)) #,font=2, 
dev.off()

# saveRDS(bs.full,  "./rds_objects/bs.full.rds") 
# saveRDS(bsp.full, "./rds_objects/bsp.full.rds")
# saveRDS(bsa.full, "./rds_objects/bsa.full.rds")

# Beershebensis collections data (bc) ----
bc.full <- readOGR(dsn="E:/GIS working/layers/shnuniot/shnuniot.gdb",layer="B_collections") 
head(bc.full)
summary(bc.full)
bc.full$occurrence = 1

par(mar=c(1.5,1.5,1.5,1.5), mfrow = c(1,1))
plot  (bc.full, pch=16, cex=1)
lines(borders, lty=5, lwd=2, col="grey15") # hmm, some outliers here.
hist(bc.full$lat) # there are two outliers with latitude below 30 degrees (ie way outside of israel). Remove.
bc.full = subset(bc.full, bc.full$lat>=30)

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

# saveRDS(bc.full,"./rds_objects/bc.full.rds")

# Schreiberi survey data (ss) ----

ss.full <- readOGR(dsn="E:/GIS working/layers/shnuniot",layer="S_surveys_export2") # reading species survey data
# in above line, had to import '_export' shapefile rather than gdb feature class, as gdb import wasn't working.
head(ss.full)  # 973 observations... 1042 after multiple revisions.
names(ss.full)
str(ss.full) 
summary(ss.full) # 97 have no location data (from Boaz surveys)
ss.full <- sp.na.omit(ss.full, col.name = "lat_WGS84") 
plot(ss.full)
# plot(ss.full$y_ITM ~ ss.full$x_ITM) # doesn't work, takes ages...?
hist(ss.full$lat_WGS84) # one outliers below 31 degrees latitude, plus a whole lot of 'no data' points.
subset(ss.full, ss.full$lat_WGS84<=31)
ss.full = subset(ss.full, ss.full$lat_WGS84>=31)
plot(ss.full) # that's better.
summary(ss.full) # now 875 data points. New version: 901. New new version (revised buffer): 892. New new new: 944.
summary(ss.full@data$YEAR)
# 90 additional points from 2018, provided by Boaz in Jan 2019
# added another 33 absence points on 19/07/2019 from Beershebensis surveys. More on 29/07/19 from KKL surveys.
# Removed some on 26/07/2019 due to reducing the buffer area. Absences went down from 102 to 93.

summary(ss.full$source)
# simplify the sources:
# shapefile@data$column_update <- as.character(shapefile@data$column_update)
ss.full@data$source <- as.character(ss.full@data$source) # first convert to character as will be factor by default.
ss.full@data$source[ss.full@data$source=="Achiad 2017 KKL landuse impacts survey"]         <- "KKL land-use imapcts survey 2017-18"
ss.full@data$source[ss.full@data$source=="Achiad 2018 KKL landuse impacts survey"]         <- "KKL land-use imapcts survey 2017-18"
ss.full@data$source[ss.full@data$source=="Achiad survey 2016"]                             <- "Achiad distribution survey 2016"
ss.full@data$source[ss.full@data$source=="Keren_Akiva_Tom 2018 KKL landuse impacts survey"]<- "KKL land-use imapcts survey 2017-18"
ss.full@data$source[ss.full@data$source=="Shacham multiple surveys (updated Jan 2019)"]    <- "Shacham multiple surveys 2001-2018"
ss.full@data$source[ss.full@data$source=="Shacham pers. comm. 24/4/18"]                    <- "Shacham multiple surveys 2001-2018"
ss.full@data$source[ss.full@data$source=="Shacham multiple surveys"]                       <- "Shacham multiple surveys 2001-2018"
ss.full@data$source

ss.full$occurrence = ifelse(ss.full$presence == "present",1,0)

ssp.full = ss.full[ss.full$presence == "present",]; length(ssp.full) 
ssa.full = ss.full[ss.full$presence == "absent",];  length(ssa.full)

png(filename = "./output_images/obs 3 - Schreiberi survey data.png", width=6, height=12, units='cm', res = 600)
par(mar=c(0.5,1,1,1), mfrow=c(1,1))
plot  (sc.full, pch=16, cex=0,  # plotted sc invisibly first to make the two maps align.
     main="Schreiberi survey data (973 obs - 98 'no coords' removed)", cex.main=0.5,  font.main=2, adj=0.5) #875 remain
lines(borders, lty=1, lwd=2, col="seashell4")
lines(groads,  col="grey73")
lines(schreiberi.buffer, lwd=2, col="bisque3", lty=2)
points(ss.full[ss.full$presence == "exclude",],col='darkgreen',pch=16, cex=.4)
points(ss.full[ss.full$presence == "present",],col='blue',pch=16, cex=.4)
points(ss.full[ss.full$presence == "absent",], col='red', pch=16, cex=.4)
points(major.cities, pch=21, col='black', bg='yellow')
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.4, offset=0.3))
# points(sc.full, pch=16, col="orange", cex=.5) # adding collections data
legend("topleft", c("Present (829 observations)","Absent (99 observations)","Excluded (16 observations)"), 
       col=c("blue","red","darkgreen"), pch=16, cex=0.5)
dev.off()


png(filename = "./output_images/obs 3 - Schreiberi survey data by source.png", width=6, height=12, units='cm', res=600)
par(mar=c(0.5,1,1,1), mfrow=c(1,1))
plot  (ss.full, pch=16, cex=0.2,  # plotted ss invisibly first to make the two maps align.
     main="Schreiberi survey data (944 obs - 98 incomplete records removed)", cex.main=0.4,  font.main=2, adj=0.5) #875 remain
lines(borders, lty=1, lwd=2, col="seashell4")
lines(groads,  col="grey73")
lines(schreiberi.buffer, lwd=2, col="bisque3", lty=2)
points(subset(ss.full, presence=="present" & source=="Shacham multiple surveys 2001-2018"), col='lightblue', pch=16,cex=.4)
points(subset(ss.full, presence=="present" & source=="Hawlena beershebensis survey 2016"),  col='darkorange',pch=16,cex=.4)
points(subset(ss.full, presence=="present" & source=="Achiad distribution survey 2016"),    col='darkgreen', pch=16,cex=.4)
points(subset(ss.full, presence=="present" & source=="KKL land-use imapcts survey 2017-18"),col='darkblue',  pch=16,cex=.4)

points(subset(ss.full, presence=="absent" & source=="Shacham multiple surveys 2001-2018"),  bg='lightblue', pch=21,col="red",cex=.4)
points(subset(ss.full, presence=="absent" & source=="Hawlena beershebensis survey 2016"),   bg='darkorange',pch=21,col="red",cex=.4)
points(subset(ss.full, presence=="absent" & source=="Achiad distribution survey 2016"),     bg='darkgreen', pch=21,col="red",cex=.4)
points(subset(ss.full, presence=="absent" & source=="KKL land-use imapcts survey 2017-18"), bg='darkblue',  pch=21,col="red",cex=.4)

points(major.cities, pch=21, col='black', bg='yellow')
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.4, offset=0.3))
# points(sc.full, pch=16, col="orange", cex=.5) # adding collections data
legend("topleft", c("Achiad distribution survey 2016: presence (94 obs)", 
                    "Shacham multiple surveys 2001-2018: presence (685 observations)",
                    "KKL land-use imapcts survey 2017-18: presence (42 obs)",
                    "Hawlena beershebensis survey 2016: presence (8 observations)",
                    "Achiad distribution survey 2016: absent (76 obs)", 
                    "Shacham multiple surveys 2001-2018: absent (0 observations)",
                    "KKL land-use imapcts survey 2017-18: absent (6 obs)",
                    "Hawlena beershebensis survey 2016: absent (17 observations)"),
       pt.bg = c("darkgreen","lightblue","darkblue","darkorange","darkgreen","lightblue","darkblue","darkorange"), 
       pch=c(16,16,16,16,21,21,21,21), col = c("darkgreen","lightblue","darkblue","darkorange","red","red","red","red"), cex=0.4)
dev.off()


saveRDS(ss.full, "./rds_objects/ss.full.rds")
saveRDS(ssp.full,"./rds_objects/ssp.full.rds")
saveRDS(ssa.full,"./rds_objects/ssa.full.rds")

# Schreiberi collections data (sc) ----
sc.full <- readOGR(dsn="E:/GIS working/layers/shnuniot",layer="S_collections_export") 
head(sc.full)
summary(sc.full)
sc.full$occurrence = 1
# remove three records which are too doubtful (due to location in boskianus/beershebensis heartland and notes):
sc.full <- sc.full [ which ( sc.full$notes != "?boskianus ?  Seen 2012. Dissected by Stephen Goldberg 03/2012. Scheriberi was was changed to boskianus according to Tamar et al. 2014.;" | is.na(sc.full$notes )), ]
sc.full$positional_accuracy = sc.full$positional

png(filename = "./output_images/obs 4a - Schreiberi coll. data by source.png", width=6, height=12, units='cm', res=600)
summary(sc.full@data$source)
par(mar=c(0.5,1,1,1), mfrow=c(1,1))
plot  (sc.full, pch=16, cex=.2,
       main="Schreiberi collections data (242 observations)", cex.main = 0.6,  font.main= 2, adj = 0.5)
lines(borders, lty=1, lwd=2, col="seashell4")
lines(groads, col="grey73")
points(sc.full[sc.full$source == "HUJR",], col='green', pch=16, cex=.5)
points(sc.full[sc.full$source == "TAU",],  col='purple',pch=16, cex=.5)
points(major.cities, pch=21, col='black',  bg='yellow', cex=0.6)
# points(towns, pch=21, col='black', bg='yellow', cex=0.2)
with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=2, cex=0.4, offset=0.3)) #,font=2
legend("topleft", c("HUJ (68 observations)", "TAU (174 observations)"), col=c("green","purple"), pch=16, cex=0.5)
dev.off()

png(filename="./output_images/obs 4b - Schreiberi coll. data by accuracy.png", width=6, height=12, units='cm', res=600)
summary(sc.full@data$positional)
par(mar=c(0.5,1,1,1), mfrow=c(1,1))
plot(sc.full, pch=16, cex=.1,
     main="Schreiberi collections data by accuracy", cex.main = 0.6,  font.main= 2, adj = 0.5)
lines(borders, lty=1, lwd=2, col="seashell4")
lines(groads, col="grey73")
points(major.cities, pch=21, col='black', bg='yellow', cex=1)
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.4, offset=0.3)) # , font=2
# points(towns, pch=21, col='black', bg='yellow', cex=0.5)
points(sc.full[sc.full$positional_accuracy == "low accuracy",], col='red',  pch=16, cex=.5)
points(sc.full[sc.full$positional_accuracy == "high accuracy",],col='blue', pch=16, cex=.5)
legend("topleft", c("High accuracy (63 observations)", "Low accuracy (179 observations)"), 
       col=c("blue","red"), pch=16, cex=0.5)
dev.off()

# saveRDS(sc.full,"./rds_objects/sc.full.rds")

# Summary of observational datasets ----

# Beershebensis survey data:       202 features: 75 presences, 127 absences 
# Beershebensis collections data:  316 features: two outliers removed. 
#                                  Positional accuracy:13 high,31 moderate,272 low.
# Schreiberi survey data:          973 features: 98 records lacking positional data removed (875 remain).  
#                                  76 absences, 696 presences, 12 exclude.
# Schreiberi collections data:     242 features:  68 from HUJ, 174 from TAU.
#                                  Positional accuracy: 63 high, 0 moderate and 179 low.
