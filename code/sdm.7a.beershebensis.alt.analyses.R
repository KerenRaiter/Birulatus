# Alternative versions of the sdm analysis: changing variables and ELSA threshold (consolidated script) ----
# This script includes code from scripts 2, 2a, 3 & 4

########################################################################################################################
# Housekeeping ----

# load packages:
x = c("sdm","usdm","raster","rgdal","tidyverse","png","beepr","xlsx","mailR","parallel")
lapply(x, require, character.only = TRUE)
installAll() # installs all the packages that 'sdm' relied on, if they aren't already installed.

# make functions:
emailme = function() {
send.mail(from="kerengila@gmail.com",       to="keren.raiter@mail.huji.ac.il",
          subject="the loop is complete",   body="yeah yeah yeah",        html=T,
          smtp=list(host.name = "smtp.gmail.com",          port = 465,
                    user.name = "kerengila@gmail.com",     passwd = "Ivrit333",
                    ssl = T), authenticate=T) #, attach.files="C:\\Users\\Deepanshu\\Nature.xls"  
}

# load data:
heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

b_package_names        = readRDS("./rds_objects/b_package_names.rds")
b.preds                = readRDS(paste0(heavies.rds.path,"b.preds.rds")); plot(b.preds) # raster stack, 6 layers  

combo.descriptions     = readRDS("./rds_objects/combo.descriptions.rds")

# spatial data:
borders                = readRDS("./rds_objects/borders.rds")
major.cities           = readRDS("./rds_objects/major.cities.rds")
small.cities           = readRDS("./rds_objects/small.cities.rds")
towns                  = readRDS("./rds_objects/towns.rds")
villages               = readRDS("./rds_objects/villages.rds")
groads                 = readRDS("./rds_objects/groads.rds")

nat.res                = readRDS(paste0(heavies.spatial.path,"nat.res.rds"));  plot(nat.res, col="darkgreen",  border="darkgreen")
nat.park               = readRDS(paste0(heavies.spatial.path,"nat.park.rds")); plot(nat.park, col="lightgreen",border="lightgreen", add=T)

kkl.mgmt               = readRDS(paste0(heavies.rds.path,"kkl.mgmt.rds"))
kkl.plan               = readRDS(paste0(heavies.rds.path,"kkl.plan.rds"))
kkl.ops                = readRDS(paste0(heavies.rds.path,"kkl.ops.rds"))

b.disturbed            = readRDS(paste0(heavies.rds.path,"b.disturbed.rds"))
builtup                = readRDS(paste0(heavies.rds.path,"builtup.rds"))
agriculture            = readRDS(paste0(heavies.rds.path,"agriculture.rds"))
dmt_uncat              = readRDS(paste0(heavies.rds.path,"dmt_uncat.rds"))
dmt_bu_ag_plntn        = readRDS(paste0(heavies.rds.path,"dmt_bu_ag_plntn.rds"))
INPA_dist              = readRDS(paste0(heavies.rds.path,"INPA_dist.rds"))
KKL_ops                = readRDS(paste0(heavies.rds.path,"KKL_ops.rds"))
military               = readRDS(paste0(heavies.rds.path,"military.rds"))
rail                   = readRDS(paste0(heavies.rds.path,"rail.rds"))
osm_dist               = readRDS(paste0(heavies.rds.path,"osm_dist.rds"))
disturbed.raw.layers = list(builtup,agriculture, dmt_uncat, dmt_bu_ag_plntn,
                            INPA_dist, KKL_ops, military, rail, osm_dist)

plot(kkl.plan, col="pink", xlim=c(34.3,35.4), ylim=c(30.8,31.6))
plot(kkl.ops,  col="lightpink", add=T)
plot(kkl.mgmt, col="blue", add=T)
plot(nat.park, col='darkgreen',  bg='darkgreen', add=T)
plot(nat.res,  col='lightgreen', bg='darkgreen', add=T)
plot(borders, xlim=c(34.3,35.4), ylim=c(30.8,31.6), add=T)
lines(groads, col="grey73")
plot(villages, pch=21, bg='blue', cex=0.9, add=T)
plot(towns, pch= 21, bg= 'green', cex = 0.9, add=T)
plot(small.cities, pch=21, bg='orange', cex=1.4, add=T)
plot(major.cities, pch=21, bg='yellow', cex=1.8, add=T)
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))

bc.full  = readRDS("./rds_objects/bc.full.rds")
bs       = readRDS("./rds_objects/b.surveys.rds")
bc       = readRDS("./rds_objects/b.collections.rds")
bsp      = readRDS("./rds_objects/b.surveys.present.rds")
bsa      = readRDS("./rds_objects/b.surveys.absent.rds")
bc.r     = readRDS("./rds_objects/b.collections.reliables.rds")
bsa.r    = readRDS("./rds_objects/b.surveys.absence.reliables.rds")
plot(bs); points(bc); points(bsp); points(bsa); points(bc.r); points(bsa.r)


########################################################################################################################
# New saved objects ----
b.preds.D = readRDS(paste0(heavies.rds.path,"b.preds.D.rds"))
bc.r.D    = readRDS ("./rds_objects/b.collections.reliables.analysisD.rds")
bc.r.E    = readRDS("./rds_objects/b.collections.reliables.0.4.elsa.rds")

b_alt_packages = readRDS(paste0(heavies.rds.path,"b_alt_packages.rds"))
b_data_packages_DEM_0.4E = readRDS(paste0(heavies.rds.path, "b_data_packages_DEM_0.4E.rds"))

eval.list = readRDS("./rds_objects/eval.list.alternatives.rds")   # 'raw' list of full eval data from each combo
eval.summary = readRDS("./rds_objects/eval.summary.alternatives.rds")  # consolidated list (multi reps averaged)
eval.summary.df = readRDS("./rds_objects/eval.summary.df.alternatives.rds") # consolidated table (all combos together)

########################################################################################################################
# Defining analyses ----

alt.analysis.names = list("Analysis B", "Analysis D", "Analysis E", "Analysis F")

alt.analysis.descriptions = list("Analysis B: Jant (no DEM), and elsa thresholds vary",
                                 "Analysis D: DEM instead of Jant; elsa thresholds vary",
                                 "Analysis E: Jant remains; all elsa thresholds reverted to 0.4",
                                 "Analysis F: DEM replaces Jant; all elsa thresholds reverted to 0.4")
# DEM and Jant are alternated due to collinearity with Jant.

########################################################################################################################
# Setting up alternative predictor variables ----

# b.preds # the conventional 
# plot(b.preds)
# b.preds.wdem           = readRDS(paste0(heavies.rds.path,"b.preds (w.DEM).rds")); plot(b.preds.wdem) # from previous
# b.preds.D = stack(b.preds[[1]], b.preds[[3]], b.preds.wdem[[4]], b.preds[[4]], b.preds[[5]], b.preds[[6]])
b.preds.D.names = list("Rain", "Jult", "DEM", "TWet", "Slop", "Soil")
# vif(b.preds)
# vif(b.preds.wdem) 
source("./code/HighstatLibV8.R")
corvif(b.preds.wdem)
a = c(4,5,6,7,1)
b= c(4,2,8,4,1)
cor(a,b)

cor(b.preds.wdem[[2]][],b.preds.wdem[[4]][], na.rm)
# vif(b.preds.D) # good good.
# saveRDS(b.preds.D, paste0(heavies.rds.path,"b.preds.D.rds"))

########################################################################################################################
# Setting up the alternative observations datasets, with revised ELSA thresholds ----

# ELSA values: low value means high autocorrelation and therefore better reliability. High values (e.g. >0.4) are problematic.

bc.full # 314 features
bc = remove.duplicates(bc.full)

b.preds.D.names
options("scipen"=100, "digits"=4) # forcing R not to use scientific notation
bc$Rain.elsa;     max(bc$Rain.elsa)
bc$Jant.elsa;     max(bc$Jant.elsa) # max 0.107
bc$Jult.elsa;     max(bc$Jult.elsa)
bc$dem.elsa ;     max(bc$dem.elsa)
bc$TWet.elsa;     max(bc$TWet.elsa) # max 0.208
bc$Slop.elsa;     max(bc$Slop.elsa)
bc$Soil.elsa;     max(bc$Soil.elsa) # the only variable with elsa values over 0.4
bc$Soil.mod.elsa; max(bc$Soil.mod.elsa)

table(bc$positional_accuracy)

# Analysis B: Jant (no DEM), and elsa thresholds vary ----

# (note that original 'base case' had error - defined with DEM instead of elsa (due to legacy)).

bc$reliability.B = "reliable" # reliability = low accuracy + ELSA above threshold for any predictor
thresh4conts  = 0.1

# Rain:
for(i in 1:length(bc))    {
  if(bc$Rain.elsa[i]  > thresh4conts & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.B[i] = "unreliable"}}
table(bc$reliability.B) # 20 unreliable vs 64 reliable
length(subset(bc$Rain.elsa, bc$Rain.elsa > thresh4conts )) # 27 out, one musn't be low accuracy.

# Jant:
for(i in 1:length(bc))    {
  if(bc$Jant.elsa[i]  > thresh4conts & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.B[i] = "unreliable"}}
table(bc$reliability.B) # 20 unreliable vs 64 reliable
length(subset(bc$Jant.elsa, bc$Jant.elsa > thresh4conts )) # 27 out, one musn't be low accuracy.

# Jult:
for(i in 1:length(bc))    {
  if(bc$Jult.elsa[i]  > thresh4conts & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.B[i] = "unreliable"}}
table(bc$reliability.B) # 20 unreliable vs 64 reliable
length(subset(bc$Jult.elsa, bc$Jult.elsa > thresh4conts )) # 27 out, one musn't be low accuracy.

# TWet:
for(i in 1:length(bc))    {
  if(bc$TWet.elsa[i] >  thresh4conts & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.B[i] = "unreliable"}}
table(bc$reliability.B) # 29 out (an additional 9)
length(subset(bc$TWet.elsa, bc$TWet.elsa > thresh4conts )) # 11. So 2 were overlapping.

# Slope:
for(i in 1:length(bc))    {
  if(bc$Slop.elsa[i] > thresh4conts & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.B[i] = "unreliable"}}
table(bc$reliability.B) # 30 out (an additional 1)
length(subset(bc$Slop.elsa, bc$Slop.elsa > thresh4conts )) # 21. So 20 were overlapping.

# Soil:
for(i in 1:length(bc))    {
  if(bc$Soil.elsa[i] >  0.4 & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.B[i] = "unreliable"}}
table(bc$reliability.B) # 42 out (an additional 12)
length(subset(bc$Soil.elsa, bc$Soil.elsa > 0.4 )) # 25. So 13 were overlapping.

bc.r.B = subset(bc, reliability.B == "reliable"); length(bc.r.B) # 51 unreliable, 33 reliable

# Analysis D: DEM instead of Jant; elsa thresholds vary ----

bc$reliability.D = "reliable" # reliability = low accuracy + ELSA above threshold for any predictor
thresh4conts = 0.1

# Rain:
for(i in 1:length(bc))    {
  if(bc$Rain.elsa[i]  > thresh4conts & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.D[i] = "unreliable"}}
table(bc$reliability.D) # 20 unreliable vs 64 reliable
length(subset(bc$Rain.elsa, bc$Rain.elsa > thresh4conts )) # 21 out, one musn't be low accuracy.

# Jult:
for(i in 1:length(bc))    {
  if(bc$Jult.elsa[i]  > thresh4conts & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.D[i] = "unreliable"}}
table(bc$reliability.D) # 20 unreliable vs 64 reliable
length(subset(bc$Jult.elsa, bc$Jult.elsa > thresh4conts )) # 21 out, one musn't be low accuracy.

# DEM:
for(i in 1:length(bc))    {
  if(bc$dem.elsa[i]  > thresh4conts & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.D[i] = "unreliable"}}
table(bc$reliability.D) # 20 unreliable vs 64 reliable
length(subset(bc$DEM.elsa, bc$DEM.elsa > thresh4conts )) # 21 out, one musn't be low accuracy.

# TWet:
for(i in 1:length(bc))    {
  if(bc$TWet.elsa[i] >  thresh4conts & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.D[i] = "unreliable"}}
table(bc$reliability.D) # 29 out (an additional 9)
length(subset(bc$TWet.elsa, bc$TWet.elsa > thresh4conts )) # 11. So 2 were overlapping.

# Slope:
for(i in 1:length(bc))    {
  if(bc$Slop.elsa[i] > thresh4conts & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.D[i] = "unreliable"}}
table(bc$reliability.D) # 30 out (an additional 1)
length(subset(bc$Slop.elsa, bc$Slop.elsa > thresh4conts )) # 21. So 20 were overlapping.

# Soil:
for(i in 1:length(bc))    {
  if(bc$Soil.elsa[i] >  0.4 & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.D[i] = "unreliable"}}
table(bc$reliability.D) # 42 out (an additional 12)
length(subset(bc$Soil.elsa, bc$Soil.elsa > 0.4 )) # 25. So 13 were overlapping.

bc.r.D = subset(bc, reliability.D == "reliable"); length(bc.r.D) # 55. So 29 unreliable.

png(filename = "./output_images/obs 2c B collections reliability map, alternative run D.png", 
    width = 22, height = 16, units = 'cm', res = 600)
par(mfrow=c(1,1), mar= c(2,2,2,2))
plot(bc, pch=21, bg='red', main="Beershebensis collections observations, by reliability (pos. acc. & ELSA)") # red will be unreliable
lines(borders, lty=2, lwd=3, col="grey15")
lines(groads, col="grey73")
points(major.cities, pch=22, col='black', bg='yellow', cex = 1.4)
with  (major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))
points(bc.r.D, bg='deepskyblue2', pch=21) 
legend("bottomright", c("Reliable observations", "Unreliable observations"), pch=21, pt.bg=c("deepskyblue2","red"), cex=1, text.font=1)
dev.off()

# saveRDS (bc.r.D, "./rds_objects/b.collections.reliables.DEMvE.rds")

# Analysis E: Jant remains; all elsa thresholds reverted to 0.4 ----

bc$reliability.E = "reliable" # reliability = low accuracy + ELSA above threshold for any predictor
thresh = 0.4

# Rain, Jult, Jant, Twet and slope all have below-0.4-threshold ELSA values for all observations.

# Twet:
for(i in 1:length(bc))    {
  if(bc$TWet.elsa[i] > thresh & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.E[i] = "unreliable"}}
table(bc$reliability.E) # 
length(subset(bc$TWet.elsa, bc$TWet.elsa > thresh )) # 25 had high elsas, but 4 must not have been low accuracy.

# Soil:
for(i in 1:length(bc))    {
  if(bc$Soil.elsa[i] > thresh & bc$positional_accuracy[i] == "low accuracy") {bc$reliability.E[i] = "unreliable"}}
table(bc$reliability.E) # 
length(subset(bc$Soil.elsa, bc$Soil.elsa > thresh )) # 25 had high elsas, but 4 must not have been low accuracy.

bc.r.E = subset(bc, reliability.E == "reliable"); length(bc.r.E) # 63 reliable, ie 21 unreliable

plot(bc, pch=21, bg='red', main="Beershebensis collections reliability (elsa threshold=0.4)") # red = unreliable
points(bc.r.E, pch=21, bg='blue')



# Analysis F: DEM replaces Jant & elsa threshold reverts to 0.4 ----

# note that that this '0.4 threshold' delineation of reliable vs unreliable observations is consistent across 
# analyses E & F, as all elsa values for both jant and dem are below 0.4. So:

bc.r.F = bc.r.E # 63 reliable, ie 21 unreliable


########################################################################################################################
# Polish data points ---- 

points.list = list(bs, bsp, bsa, bsa.r, bc, bc.r.B, bc.r.D, bc.r.E, bc.r.F) 
points.full.names = list("bs  = beershebensis all survey data",
                         "bsp = beershebensis survey presences",
                         "bsa = all beershebensis survey absences",
                         "bsa.r = reliable beershebensis survey absences only",
                         "bc = beershebensis collections presences",
                         "bc.r.B", "bc.r.D", "bc.r.E", "bc.r.F")

for (p in 1:length(points.list)) {
  print(points.full.names[[p]])                           # print name of dataset to start us off...
  print(class(points.list[[p]]))                          # see class. should be "SpatialPointsDataFrame"
  print(length(points.list[[p]]))                         # how many data points in this dataset?
  points.list[[p]]@coords = points.list[[p]]@coords[,1:2] # Remove third (Z) coordinate if present
  print(summary(points.list[[p]]@coords))                 # check that now there are only two coords for each dataset
  print(str(points.list[[p]]$occurrence))                 # ensure occurrence is recorded consistently. Should have 1's or 0's.
  print(summary(points.list[[p]]$occurrence))             # again, ensure occurrence is recorded consistently
  points.list[[p]]@data <- points.list[[p]]@data[,'occurrence', drop=F]        # dropping all point attributes other than occurrence
  print(names(points.list[[p]])) }                                             # check that only "occurrence" remains

bs  = points.list[[1]]; bsp = points.list[[2]]; bsa = points.list[[3]]; bsa.r = points.list[[4]]; bc = points.list[[5]]
bc.r.B = points.list[[6]]; bc.r.D = points.list[[7]]; bc.r.E = points.list[[8]]; bc.r.F = points.list[[9]] 

########################################################################################################################
# Create data packages ----
# Data combination descriptions
b_package_names = list('Combination A','Combination B','Combination C','Combination D','Combination E',
                       'Combination F','Combination G','Combination H','Combination I')
combo.descriptions = list("A: All data", "B: Surv. data; Q absences excluded", "C: Surv. data; all included",
                          "D: Surv. & coll. data; both unrels excl.", "E: Surv. & coll. data; Q absences excl.",
                          "F: Surv. & coll. data; unrel. obs excl.", "G: Surv. data: presence only (PO)",
                          "H: Surv. & coll. data; PO, unrel. excl.", "I: Surv. & coll. data; PO, all incl.")

alt.an.preds = list(b.preds, b.preds.D, b.preds, b.preds.D)
reliable.colls.sets        = list(bc.r.B, bc.r.D, bc.r.E, bc.r.F)
obs_A_alts = list(); obs_B_alts = list(); obs_C_alts = list(); obs_D_alts = list(); obs_E_alts = list()
obs_F_alts = list(); obs_G_alts = list(); obs_H_alts = list(); obs_I_alts = list()

b_alt_packages = list()

for (a in 1:length(alt.an.preds)){
  print(alt.analysis.descriptions[[a]])
  obs_A_alts[[a]] = rbind(bsp, bsa, bc)
  obs_B_alts[[a]] = rbind(bsp, bsa.r)
  obs_C_alts[[a]] = rbind(bs) 
  obs_D_alts[[a]] = rbind(bsp, bsa.r, reliable.colls.sets[[a]])
  obs_E_alts[[a]] = rbind(bsp, bsa.r, bc)
  obs_F_alts[[a]] = rbind(bsp, bsa, reliable.colls.sets[[a]])
  obs_G_alts[[a]] = bsp
  obs_H_alts[[a]] = rbind(bsp, reliable.colls.sets[[a]])
  obs_I_alts[[a]] = rbind(bsp, bc)               
  
  obs_packages  = list(obs_A_alts[[a]], obs_B_alts[[a]], obs_C_alts[[a]], obs_D_alts[[a]], obs_E_alts[[a]],
                       obs_F_alts[[a]], obs_G_alts[[a]], obs_H_alts[[a]], obs_I_alts[[a]])
  
  b_alt_packages[[a]] = list()
  
  for (d in 1:length(b_package_names))                                                                          {
    preds = alt.an.preds[[a]]
    b_alt_packages[[a]][[d]] = sdmData(formula = occurrence ~ ., train = obs_packages[[d]], predictors = preds) } 
  
  # now add background points to the last three data packages that are presence-only. 
  bg.points = length(b_alt_packages[[a]][[7]]@features$rID)  # 68 obs; presence-only
  b_alt_packages[[a]][[7]] = sdmData(formula = occurrence ~ ., train = obs_packages[[7]], predictors = preds, 
                                     bg=list(n=bg.points, method='gRandom', remove=TRUE))
  b_alt_packages[[a]][[7]] # 136 obs; presence-background
  
  bg.points = length(b_alt_packages[[a]][[8]]@features$rID)  # 101 obs; presence-only
  b_alt_packages[[a]][[8]] = sdmData(formula = occurrence ~ ., train = obs_packages[[8]], predictors = preds, 
                                     bg=list(n=bg.points, method='gRandom', remove=TRUE))
  b_alt_packages[[a]][[8]] # 136 obs; presence-background
  
  bg.points = length(b_alt_packages[[a]][[9]]@features$rID)  # 152 obs; presence-only
  b_alt_packages[[a]][[9]] = sdmData(formula = occurrence ~ ., train = obs_packages[[9]], predictors = preds, 
                                     bg=list(n=bg.points, method='gRandom', remove=TRUE))
  b_alt_packages[[a]][[9]] # 136 obs; presence-background   
}

saveRDS(b_alt_packages, paste0(heavies.rds.path,"b_alt_packages.rds"))
b_data_package_DEM_0.4E = b_alt_packages[[2]]
saveRDS(b_data_package_DEM_0.4E, paste0(heavies.rds.path, "b_data_package_DEM_0.4E.rds"))

########################################################################################################################
# Create evaluation models ----

sdm.cv = function(data) {
  sdm(occurrence ~ ., data = data, methods =c('gam','rf','svm'),  # took out non-top algorithms
      n=20, replication = 'cv', cv.folds=5) }

# Remember, b_alt_packages is a list of lists. It has four items, each for an analysis alterantive, and within each one 
# are the 9 data combinations

# create models for each combo 
alt.models = list()                                          

for (a in 1:2) {     # replaced 'length(alt.analysis.names)' with '2'.                                                              
  start.time = Sys.time()
  print(alt.analysis.descriptions[[a]])
  alt.models[[a]] = list()                                  

  for (p in 1:length(b_package_names))  {
  package.start = Sys.time()
  data = b_alt_packages[[a]][[p]]
  alt.models[[a]][[p]] = sdm.cv(data)                       
  print(paste(b_package_names[p],"loop took", difftime(Sys.time(), package.start, units="mins"), "minutes")) }
  print(paste(alt.analysis.names[[a]],"loop took", difftime(Sys.time(), start.time, units="mins"), "minutes")) }

saveRDS(alt.models, paste0(heavies.rds.path,"alt.models.topalgs.rds")) 
emailme()

########################################################################################################################
# Evaluate models ----

eval.list = list()    # evaluation data, by model (i.e. 4500 models: 100repsx5 foldsx9 algorithims,for each datacombo)
eval.summary = list() # list of evaluation data, averaged by algorithim (9 dataframes of 9 rows: 9 datacombos X 9 algs)

for (a in 1:length(alt.analysis.names)) {             # takes ~ 1 minute                                                      
  eval.list[[a]] = list()
  eval.summary[[a]] = list()
  
  for (c in 1:length(alt.models[[a]]))  { # make sure this number is 9
  model.inf.ev = NULL
  # extract model info and eval statistics and merge them (change name of model list as appropriate):
  model_info = getModelInfo (alt.models[[a]][[c]])
  model_eval = getEvaluation(alt.models[[a]][[c]], wtest= 'test.dep', stat=c('TSS','Kappa','AUC','COR','Deviance',
                                                                           'obs.prevalence','threshold'), opt=2)
  model.inf.ev = merge(model_info, model_eval, by = "modelID") # 'model.inf.ev' - a temporary vector for use in next bit
  # then put them into list and aggregate them
  eval.list[[a]][[c]]   = model.inf.ev 
  eval.summary[[a]][[c]]= data.frame(method=aggregate(model.inf.ev$method, by=list(model.inf.ev$method),FUN=mode)[1],
                                data_combo = b_package_names[[c]],
                                TSS     = aggregate(model.inf.ev$TSS,       by=list(model.inf.ev$method),  FUN=mean)[2],
                                TSS_sd  = aggregate(model.inf.ev$TSS,       by=list(model.inf.ev$method),  FUN=sd)  [2],
                                AUC     = aggregate(model.inf.ev$AUC,       by=list(model.inf.ev$method),  FUN=mean)[2],
                                AUC_sd  = aggregate(model.inf.ev$AUC,       by=list(model.inf.ev$method),  FUN=sd)  [2],
                                kappa   = aggregate(model.inf.ev$Kappa,     by=list(model.inf.ev$method),  FUN=mean)[2],
                                COR     = aggregate(model.inf.ev$COR,       by=list(model.inf.ev$method),  FUN=mean)[2],
                                deviance= aggregate(model.inf.ev$Deviance,  by=list(model.inf.ev$method),  FUN=mean)[2],
                                obs.prev= aggregate(model.inf.ev$Prevalence,by=list(model.inf.ev$method),  FUN=mean)[2],
                                threshold= aggregate(model.inf.ev$threshold,by=list(model.inf.ev$method),  FUN=mean)[2],
                                total.mods=aggregate(model.inf.ev$method,   by=list(model.inf.ev$method),FUN=length)[2])
  names(eval.summary[[a]][[c]]) = c('method', 'data_combo', 'TSS', 'TSS_sd', 'AUC', 'AUC_sd', 'kappa', 'COR', 
                                    'deviance','obs.prevalence', 'threshold.mss', 'total_models')           }}

eval.summary

eval.summary.df = list()
for (a in 1:length(alt.analysis.names)){
eval.summary.df[[a]] = do.call("rbind", eval.summary[[a]])} # converting list to a single df for plotting

# saving the outputs from the alternative run evaluation
saveRDS(eval.list,          "./rds_objects/eval.list.alternatives.rds")   # 'raw' list of full eval data from each combo
saveRDS(eval.summary,       "./rds_objects/eval.summary.alternatives.rds")  # consolidated list (multi reps averaged)
saveRDS(eval.summary.df,    "./rds_objects/eval.summary.df.alternatives.rds") # consolidated table (all combos together)


########################################################################################################################
# Compare performance across alternative analyses ----

alt.analysis.descriptions[[1]]; mean(eval.summary.df[[1]]$TSS) 
alt.analysis.descriptions[[2]]; mean(eval.summary.df[[2]]$TSS) 
alt.analysis.descriptions[[3]]; mean(eval.summary.df[[3]]$TSS)
alt.analysis.descriptions[[4]]; mean(eval.summary.df[[4]]$TSS)

methods.summary = list()

for (a in 1:length(alt.analysis.names)){          
methods.summary[[a]] = aggregate(eval.summary.df[[a]][, c("TSS", "AUC")],
                  eval.summary.df[[a]]["method"],
                  function(x) c(mean=mean(x), sd=sd(x)) )
methods.summary[[a]]$alternative = alt.analysis.names[[a]]
methods.summary[[a]]$TSS.sd   = methods.summary[[a]]$TSS[,2]
methods.summary[[a]]$AUC.sd   = methods.summary[[a]]$AUC[,2]
methods.summary[[a]]$TSS.mean = methods.summary[[a]]$TSS[,1]
methods.summary[[a]]$AUC.mean = methods.summary[[a]]$AUC[,1]
methods.summary[[a]]$TSS = NULL
methods.summary[[a]]$AUC = NULL
methods.summary[[a]] = methods.summary[[a]][,c(2,1,5,3,6,4)]
} # getting them back in a nice order

methods.summary.df = do.call("rbind", methods.summary)
alternatives.summary = aggregate(methods.summary.df[, c("TSS.mean", "AUC.mean")],
                  methods.summary.df["alternative"],
                  function(x) c(mean=mean(x), sd=sd(x)) )
alternatives.summary
plot(alternatives.summary$TSS.mean[,1])
plot(alternatives.summary$AUC.mean[,1])
# So DEM is stronger than Jant. Good.
# Reverting ELSA values to 0.4 has a variable effect. The strongest combination appears to be DEM with variable elsa.

# same thing, just using top algorithms
eval.summary.df.topalgs = list()
for (a in 1:length(alt.analysis.names)) {
eval.summary.df.topalgs[[a]] = subset(eval.summary.df[[a]], 
                                 eval.summary.df[[a]]$method == 'svm' | eval.summary.df[[a]]$method == 'gam' 
                                 | eval.summary.df[[a]]$method == 'rf') }

alt.analysis.descriptions[[1]]; mean(eval.summary.df.topalgs[[1]]$TSS) 
alt.analysis.descriptions[[2]]; mean(eval.summary.df.topalgs[[2]]$TSS) 
alt.analysis.descriptions[[3]]; mean(eval.summary.df.topalgs[[3]]$TSS)
alt.analysis.descriptions[[4]]; mean(eval.summary.df.topalgs[[4]]$TSS)

png(filename = "./output_images/alternative analyses.png", 
    width = 10, height = 7, units = 'cm', res = 300)
par(mar=c(1,2,1,1))
plot(c(1,2,3,4), 
     c(mean(eval.summary.df.topalgs[[1]]$TSS), mean(eval.summary.df.topalgs[[2]]$TSS), 
     mean(eval.summary.df.topalgs[[3]]$TSS), mean(eval.summary.df.topalgs[[4]]$TSS)),
     pch = 21, bg=c(1,2,3,4), xaxt='n',
     ylab = "TSS", xlab="Alternative analysis", xaxis = NULL)
legend("bottomleft", c("varEt;Jant","varEt;DEM","0.4Et;Jant","0.4Et;DEM"), pch = 21, pt.bg=c(1,2,3,4), cex=1, text.font=1)
dev.off()

########################################################################################################################
# Create evaluation models - just for first two alternatives (more reps) ----

sdm.cv = function(data) {
  sdm(occurrence ~ ., data = data, methods =c('gam','rf','svm'),  # took out non-top algorithms
      n=100, replication = 'cv', cv.folds=5) }

# Remember, b_alt_packages is a list of lists. It has four items, each for an analysis alterantive, and within each one 
# are the 9 data combinations

# create models for each combo 
alt.models.first2 = list()                                          # changed alt.models to alt.models.first2

for (a in 1:2) {     # replaced 'length(alt.analysis.names)' with '2'.                                                              
  start.time = Sys.time()
  print(alt.analysis.descriptions[[a]])
  alt.models.first2[[a]] = list()                                   # changed alt.models to alt.models.first2
  
  for (p in 1:length(b_package_names))  {
  package.start = Sys.time()
  data = b_alt_packages[[a]][[p]]
  alt.models.first2[[a]][[p]] = sdm.cv(data)                        # changed alt.models to alt.models.first2
  print(paste(b_package_names[p],"loop took", difftime(Sys.time(), package.start, units="mins"), "minutes")) }
  
  print(paste(alt.analysis.names[[a]],"loop took", difftime(Sys.time(), start.time, units="mins"), "minutes")) }

saveRDS(alt.models.first2, paste0(heavies.rds.path,"alt.models.first2.rds")) # changed alt.models to alt.models.first2
send.mail(from="kerengila@gmail.com",       to="keren.raiter@mail.huji.ac.il",
          subject="the loop is complete",   body="yeah yeah yeah",        html=T,
          smtp=list(host.name = "smtp.gmail.com",          port = 465,
                    user.name = "kerengila@gmail.com",     passwd = "Ivrit333",
                    ssl = T), authenticate=T) #, attach.files="C:\\Users\\Deepanshu\\Downloads\\Nature.xls"

########################################################################################################################
# Evaluate models                                 - just for first two alternatives (more reps) ----

eval.list.first2 = list()    # evaluation data, by model (i.e. 4500 models: 100repsx5 foldsx9 algorithims,for each datacombo)
eval.summary.first2 = list() # list of evaluation data, averaged by algorithim (9 dataframes of 9 rows: 9 datacombos X 9 algs)

for (a in 1:2) {             # length(alt.analysis.names) takes ~ 1 minute                                                      
  eval.list.first2[[a]] = list()
  eval.summary.first2[[a]] = list()
  
  for (c in 1:length(alt.models.first2[[a]]))  { # make sure this number is 9
  model.inf.ev = NULL
  # extract model info and eval statistics and merge them (change name of model list as appropriate):
  model_info = getModelInfo (alt.models.first2[[a]][[c]])
  model_eval = getEvaluation(alt.models.first2[[a]][[c]], wtest= 'test.dep', stat=c('TSS','Kappa','AUC','COR','Deviance',
                                                                           'obs.prevalence','threshold'), opt=2)
  model.inf.ev = merge(model_info, model_eval, by = "modelID") # 'model.inf.ev' - a temporary vector for use in next bit
  # then put them into list and aggregate them
  eval.list.first2[[a]][[c]]   = model.inf.ev 
  eval.summary.first2[[a]][[c]]= data.frame(method=aggregate(model.inf.ev$method, by=list(model.inf.ev$method),FUN=mode)[1],
                                data_combo = b_package_names[[c]],
                                TSS     = aggregate(model.inf.ev$TSS,       by=list(model.inf.ev$method),  FUN=mean)[2],
                                TSS_sd  = aggregate(model.inf.ev$TSS,       by=list(model.inf.ev$method),  FUN=sd)  [2],
                                AUC     = aggregate(model.inf.ev$AUC,       by=list(model.inf.ev$method),  FUN=mean)[2],
                                AUC_sd  = aggregate(model.inf.ev$AUC,       by=list(model.inf.ev$method),  FUN=sd)  [2],
                                kappa   = aggregate(model.inf.ev$Kappa,     by=list(model.inf.ev$method),  FUN=mean)[2],
                                COR     = aggregate(model.inf.ev$COR,       by=list(model.inf.ev$method),  FUN=mean)[2],
                                deviance= aggregate(model.inf.ev$Deviance,  by=list(model.inf.ev$method),  FUN=mean)[2],
                                obs.prev= aggregate(model.inf.ev$Prevalence,by=list(model.inf.ev$method),  FUN=mean)[2],
                                threshold= aggregate(model.inf.ev$threshold,by=list(model.inf.ev$method),  FUN=mean)[2],
                                total.mods=aggregate(model.inf.ev$method,   by=list(model.inf.ev$method),FUN=length)[2])
  names(eval.summary.first2[[a]][[c]]) = c('method', 'data_combo', 'TSS', 'TSS_sd', 'AUC', 'AUC_sd', 'kappa', 'COR', 
                                    'deviance','obs.prevalence', 'threshold.mss', 'total_models')           }}

eval.summary.first2

eval.summary.df.first2 = list()
for (a in 1:2){    #length(alt.analysis.names)
eval.summary.df.first2[[a]] = do.call("rbind", eval.summary.first2[[a]])} # converting list to a single df for plotting

# saving the outputs from the alternative run evaluation
saveRDS(eval.list.first2,          "./rds_objects/eval.list.first2alternatives.rds")   # 'raw' list of full eval data from each combo
saveRDS(eval.summary.first2,       "./rds_objects/eval.summary.first2alternatives.rds")  # consolidated list (multi reps averaged)
saveRDS(eval.summary.df.first2,    "./rds_objects/eval.summary.df.first2alternatives.rds") # consolidated table (all combos together)

########################################################################################################################
# Compare performance across alternative analyses - just for first two alternatives (more reps) ----

alt.analysis.descriptions[[1]]; mean(eval.summary.df.first2[[1]]$TSS) 
alt.analysis.descriptions[[2]]; mean(eval.summary.df.first2[[2]]$TSS) 
alt.analysis.descriptions[[3]]; mean(eval.summary.df[[3]]$TSS)
alt.analysis.descriptions[[4]]; mean(eval.summary.df[[4]]$TSS)

methods.summary.first2 = list()

for (a in 1:2){          #length(alt.analysis.names)
methods.summary.first2[[a]] = aggregate(eval.summary.df.first2[[a]][, c("TSS", "AUC")],
                  eval.summary.df.first2[[a]]["method"],
                  function(x) c(mean=mean(x), sd=sd(x)) )
methods.summary.first2[[a]]$alternative = alt.analysis.names[[a]]
methods.summary.first2[[a]]$TSS.sd   = methods.summary.first2[[a]]$TSS[,2]
methods.summary.first2[[a]]$AUC.sd   = methods.summary.first2[[a]]$AUC[,2]
methods.summary.first2[[a]]$TSS.mean = methods.summary.first2[[a]]$TSS[,1]
methods.summary.first2[[a]]$AUC.mean = methods.summary.first2[[a]]$AUC[,1]
methods.summary.first2[[a]]$TSS = NULL
methods.summary.first2[[a]]$AUC = NULL
methods.summary.first2[[a]] = methods.summary.first2[[a]][,c(2,1,5,3,6,4)]
} # getting them back in a nice order

methods.summary.df.first2 = do.call("rbind", methods.summary.first2)
alternatives.summary.first2 = aggregate(methods.summary.df.first2[, c("TSS.mean", "AUC.mean")],
                  methods.summary.df.first2["alternative"],
                  function(x) c(mean=mean(x), sd=sd(x)) )
alternatives.summary.first2
plot(alternatives.summary.first2$TSS.mean[,1])
plot(alternatives.summary.first2$AUC.mean[,1])
# So DEM is stronger than Jant. Good.
# Reverting ELSA values to 0.4 has a variable effect. The strongest combination appears to be DEM with variable elsa.
