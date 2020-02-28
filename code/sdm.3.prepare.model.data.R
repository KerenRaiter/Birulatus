# sdm.3.prepare data for modelling ----

# note: this code was used for the original process of modelling. Code 7 'alt.analysis.reruns' creates the data 
# packages for the multiple 'alternative analyses' reruns. The final models used emerge from those (final model is with
# DEM instead of Jant, and variable ELSAs: 0.1 for continuous variables, and 0.4 for categorical).

# There are three main functions provide the main steps of developing/using species distibution models. 
# The three steps include data preparation, model ﬁtting and evaluation, and prediction. 
# The functions used for these steps:
# • sdmData: to read data from diﬀerent formats, and prepare a data object. Both species (single or multiple) and 
# explanatory variables can be introduced to the function, as well as other information such as spatial coordinates, 
# time, grouping variables, etc.
# • sdm: to ﬁt and evaluate species distribution models (multiple algorithms can be used)
# • predict: when models are ﬁtted, they can be used to predict/project given a new dataset.

#########################################################################################################
# Housekeeping and load required data ----

# load packages:
x<-c("sdm","usdm","raster","rgdal","tidyverse","png","maptools")
lapply(x, require, character.only = TRUE)
installAll() # installs all the packages that 'sdm' relied on, if they aren't already installed.

heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

# load data

# reference info:
borders     = readRDS("./rds_objects/borders.rds")  
major.cities  = readRDS("./rds_objects/major.cities.rds")
small.cities  = readRDS("./rds_objects/small.cities.rds")
towns         = readRDS("./rds_objects/towns.rds")
villages      = readRDS("./rds_objects/villages.rds")
groads        = readRDS("./rds_objects/groads.rds")

# variable data:
b.raster.list.names = list("Rain", "Jant", "Jult","TWet", "Slop", "Soil")
b.raster.list       = readRDS(paste0(heavies.rds.path,"b.raster.list.rds"))
b.preds             = readRDS(paste0(heavies.rds.path,"b.preds.rds")) # raster stack
s.raster.list.names = list("Rain", "Jant", "Jult", "TWet", "Slop", "Soil", "Vegt")
s.raster.list       = readRDS(paste0(heavies.rds.path,"s.raster.list.rds"))
s.preds             = readRDS(paste0(heavies.rds.path,"s.preds.rds")) # raster stack

# observational datasets:
beershebensis.buffer = readRDS("./rds_objects/beershebensis.buffer_incWB.rds"); plot(beershebensis.buffer)
bs       = readRDS("./rds_objects/b.surveys.rds")
bc       = readRDS("./rds_objects/b.collections.rds");     length(bc);  length(unique(bc$lat)); length(unique(bc$lon))
bsp      = readRDS("./rds_objects/b.surveys.present.rds"); length(bsp); length(unique(c(bsp$WGS84_lat)))
bsa      = readRDS("./rds_objects/b.surveys.absent.rds")
bc.r     = readRDS("./rds_objects/b.collections.reliables.rds"); plot(bc.r); length(bc.r)
bsa.r    = readRDS("./rds_objects/b.surveys.absence.reliables.rds")
# and plot:
{
png(filename = "./output_images/obs 1&2 - All Beersheb observations by reliability.png", 
    width = 16, height = 16, units = 'cm', res = 600)
par(mfrow=c(1,1), mar= c(2,2,2,2))
plot(bs,pch=16,cex=.6,main="Beershebensis observations",cex.main=1,font.main=2)
points(bs[bs$presence == "present",],col='green', pch=16, cex=0.1)
points(bs[bs$presence == "absent",], col='black', pch=16, cex=0.1)
lines(borders, lty=5, lwd=1.5, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=21, col='black', bg='yellow')
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))
points(bsp,   bg = 'yellow',   pch=22,  cex=0.9)
points(bsa,   bg = 'cyan',    pch=21,  cex=1)
points(bsa.r, bg = 'blue',     pch=21,  cex=1)
points(bc,    bg = 'green',   pch=24,  cex=0.9)
points(bc.r,  bg = 'green4',  pch=24,  cex=0.9)
legend("bottomright", 
       c("surveyed presence (75)", "surveyed absence-questionable (24)","surveyed absence-reliable (103)",         "collections record-unreliable (71)","collections record-reliable (243)"),
       pt.bg=c("yellow","cyan","blue","green","green4"),
       pch=c(22,21,21,24,24),    cex=0.7, text.font=1)
dev.off()}

schreiberi.buffer    = readRDS("./rds_objects/schreiberi.buffer.rds"); plot(schreiberi.buffer)
ss.full              = readRDS("./rds_objects/ss.full.rds")   # all schreiberi survey data (errors removed)
ssp.full             = readRDS("./rds_objects/ssp.full.rds")  # presences
ssa.full             = readRDS("./rds_objects/ssa.full.rds")  # absences

sc.full              = readRDS("./rds_objects/sc.full.rds")   # s = schreiberi, c = collections
sc.nodups            = readRDS(paste0(heavies.rds.path,"sc.nodups.rds"))                # from sript 2a; elsa calc
sc.r                 = readRDS(paste0(heavies.rds.path,"s.collections.reliables.rds"))  # from script 2b: elsa calc
# and plot:
{
png(filename = "./output_images/obs 3&4 - All Schreiberi data by reliability new.png", 
    width=6, height=12, units='cm', res=600)
par(mar=c(0.5,1,1,1), mfrow=c(1,1))
plot(sc.nodups, pch=21, bg='red', cex=0.1, xlim=c(34.31854, 35.16423), ylim=c(31.13379, 33.09046), # based on ss & sc
     main="Schreiberi collections records, by reliability (pos. acc. & ELSA)",  cex.main=0.4)
lines(borders, lty=1, lwd=1.5, col="seashell4")
lines(groads, col="grey73")
points(ssp.full,  col='purple',       pch=16, cex=0.4) 
points(ssa.full,  col='pink',         pch=16, cex=0.4) 
points(sc.nodups, col='deepskyblue2', pch=16, cex=0.4)
points(sc.r,      col='darkblue',     pch=16, cex=0.4) 
points(major.cities, pch=22, col='black', bg='yellow', cex=1)
with  (major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 2, cex=0.5, offset=0.3))
legend("topleft", c("Survey presence (681)", "Survey absence (76)",
                    "Reliable collections data (64)", "Unreliable collections data (30)"), 
       pch=16, col=c("purple","pink","darkblue","deepskyblue2"), cex=0.5)
dev.off()
}

#########################################################################################################
# Datasets I prepared earlier in this script ----

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
# Prepare observational data for input into data packages (beershebensis) ----

points.list = list(bs,bsp,bsa,bsa.r,bc,bc.r) 
b.points.short.names = list("bs","bsp","bsa","bsa.r","bc","bc.r")
b.points.full.names = list("bs  = beershebensis all survey data",
                         "bsp = beershebensis survey presences",
                         "bsa = all beershebensis survey absences",
                         "bsa.r = reliable beershebensis survey absences only",
                         "bc = beershebensis collections presences",
                         "bc.r = reliable beershebensis survey absences only")

for (p in 1:length(points.list)) {
  print(b.points.full.names[[p]])                           # print name of dataset to start us off...
  print(class(points.list[[p]]))                          # see class. should be "SpatialPointsDataFrame"
  print(length(points.list[[p]]))                         # how many data points in this dataset?
  points.list[[p]]@coords = points.list[[p]]@coords[,1:2] # Remove third (Z) coordinate if present
  print(summary(points.list[[p]]@coords))                 # check that now there are only two coords for each dataset
  print(str(points.list[[p]]$occurrence))                 # ensure occurrence is recorded consistently. Should have 1's or 0's.
  print(summary(points.list[[p]]$occurrence))             # again, ensure occurrence is recorded consistently
  points.list[[p]]@data <- points.list[[p]]@data[,'occurrence', drop=F]        # dropping all point attributes other than occurrence
  print(names(points.list[[p]]))                                               # check that only "occurrence" remains
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
# 
# bsp   = points.list[[1]]
# bsa   = points.list[[2]]
# bsa.r = points.list[[3]]
# bc    = points.list[[4]]
# bc.r  = points.list[[5]]
# 
# saveRDS(bsp,         "./rds_objects/bsp.rds")
# saveRDS(bsa,         "./rds_objects/bsa.rds")
# saveRDS(bsa.r,       "./rds_objects/bsa.r.rds")
# saveRDS(bc,          "./rds_objects/bc.rds")
# saveRDS(bc.r,        "./rds_objects/bc.r.rds")
# saveRDS(points.list, "./rds_objects/points.list.rds")
  }

saveRDS(points.list, "./rds_objects/points.list.rds")
points.list = readRDS("./rds_objects/points.list.rds")

bs    = points.list[[1]]
bsp   = points.list[[2]]
bsa   = points.list[[3]]
bsa.r = points.list[[4]]
bc    = points.list[[5]]
bc.r  = points.list[[6]]
# 
#########################################################################################################
# Prepare data for input into data packages (Schreiberi) ----
ss.full@coords = ss.full@coords[,1:2]
ssp.full@coords = ssp.full@coords[,1:2]
ssa.full@coords = ssa.full@coords[,1:2]

ss.nodups = remove.duplicates(ss.full)
summary(ss.nodups$presence)
remove.duplicates(ssp.full)
remove.duplicates(ssa.full)

s.points.list = list(ssp.full, ssa.full, sc.nodups, sc.r)
s.points.full.names = list("ssp = schreiberi survey presences",
                           "ssa = all schreiberi survey absences",
                           "ssa.r = reliable schreiberi survey absences only",
                           "sc = schreiberi collections presences",
                           "sc.r = reliable schreiberi survey absences only")
for (p in 1:length(s.points.list)) {
 print(s.points.full.names[[p]])                             # print name of dataset to start us off...
 print(class(s.points.list[[p]]))                            # see class. should be "SpatialPointsDataFrame"
 s.points.list[[p]]@coords = s.points.list[[p]]@coords[,1:2] # Remove third (Z) coordinate if present
 print(length(s.points.list[[p]]))                           # how many features before duplicate removal?
 s.points.list[[p]] = remove.duplicates(s.points.list[[p]])     # remove duplicates
 print(length(s.points.list[[p]]))                           # how many features after duplicate removal?
 print(summary(s.points.list[[p]]@coords))                   # check that remain only two coords for each dataset
 print(str(s.points.list[[p]]$occurrence))          # ensure occurrence is recorded consistently. Should have 1's or 0's.
 print(summary(s.points.list[[p]]$occurrence))      # again, ensure occurrence is recorded consistently
 s.points.list[[p]]@data <- s.points.list[[p]]@data[,'occurrence', drop=F]  # dropping all attributes other than occurnce
 print(names(s.points.list[[p]]))                                           # check that only "occurrence" remains
}

ssp   = s.points.list[[1]] # 723 features
ssa   = s.points.list[[2]] # 99 features
sc    = s.points.list[[3]] # 93 features (63 reliable, 30 unreliable)
sc.r  = s.points.list[[4]] # 63 features

saveRDS(ssp,           "./rds_objects/ssp.rds")
saveRDS(ssa,           "./rds_objects/ssa.rds")
saveRDS(sc,            "./rds_objects/sc.rds")
saveRDS(sc.r,          "./rds_objects/sc.r.rds")
# saveRDS(s.points.list, "./rds_objects/s.points.list.rds")

#########################################################################################################
# Create data packages for each combo: Beershebensis -----

# Data combination descriptions
b.combo.descriptions = list("A: All data",
                          "B: Surv. data; Q absences excluded",
                          "C: Surv. data; all included",
                          "D: Surv. & coll. data; both unrels excl.",
                          "E: Surv. & coll. data; Q absences excl.",
                          "F: Surv. & coll. data; unrel. obs excl.",
                          "G: Surv. data: presence only (PO)",
                          "H: Surv. & coll. data; PO, unrel. excl.",
                          "I: Surv. & coll. data; PO, all incl.")

obs_A = rbind(bsp, bsa, bc)
obs_B = rbind(bsp, bsa.r)
obs_C = rbind(bs) 
obs_D = rbind(bsp, bsa.r, bc.r)
obs_E = rbind(bsp, bsa.r, bc)
obs_F = rbind(bsp, bsa, bc.r)
obs_G = bsp
obs_H = rbind(bsp, bc.r)
obs_I = rbind(bsp, bc)

obs_packages  = list(obs_A, obs_B, obs_C, obs_D, obs_E, obs_F, obs_G, obs_H, obs_I)
b_package_names = list('Combination A','Combination B','Combination C','Combination D','Combination E',
                       'Combination F','Combination G','Combination H','Combination I')
b_data_packages = list()

for (d in 1:length(b_package_names)) {
  b_data_packages[[d]] = sdmData(formula = occurrence ~ ., train = obs_packages[[d]], predictors = b.preds)
  print("------------"); print(b_package_names[[d]]); print(b_data_packages[[d]])   
  print(length(obs_packages[[d]]))   } # 516 observations in obs_A  # end up with 279 observations in data_package[[1]]. explained below: duplicate locations removed.

# and now I'm going to add background points to those last three data packages that are presence-only. Manually to ensure obs not double-counted.
b_data_packages[[7]] # 68 obs; presence-only
b_data_packages[[7]]=sdmData(formula=occurrence~., train=obs_packages[[7]], predictors=b.preds, bg=list(n=68, method='gRandom', remove=TRUE))
b_data_packages[[7]] # 136 obs; presence-background

b_data_packages[[8]] # 111 obs; presence-only (down from original 259 obs that included duplicate locations)
b_data_packages[[8]]=sdmData(formula=occurrence~., train=obs_packages[[8]], predictors=b.preds, bg=list(n=111, method='gRandom', remove=TRUE))
b_data_packages[[8]] # 254 obs; presence-background

b_data_packages[[9]] # 152 obs; presence-only
b_data_packages[[9]]=sdmData(formula=occurrence~., train=obs_packages[[9]], predictors=b.preds, bg=list(n=152, method='gRandom', remove=TRUE))
b_data_packages[[9]] # 304 obs; presence-background

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
