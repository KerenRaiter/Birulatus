# Final Beershebensis model: DEM instead of Jant, and variable (0.1 & 0.4) ELSA threshold ----
# This script includes code from scripts 4,5 & 6

#######################################################################################################################
# Housekeeping ----

# load packages:
x = c("sdm","usdm","raster","rgdal","tidyverse","png","beepr","xlsx","mailR","parallel","rgeos","sf")
lapply(x, require, character.only = TRUE)
installAll() # installs all the packages that 'sdm' relied on, if they aren't already installed.

# make packages:
emailme = function() {
send.mail(from="kerengila@gmail.com",       to="keren.raiter@mail.huji.ac.il",
          subject="the loop is complete",   body="yeah yeah yeah",        html=T,
          smtp=list(host.name = "smtp.gmail.com",          port = 465,
                    user.name = "kerengila@gmail.com",     passwd = "Ivrit333",
                    ssl = T), authenticate=T) #, attach.files="C:\\Users\\Deepanshu\\Nature.xls"  
}

sdm.cv = function(data) {
  sdm(occurrence ~ ., data = data, methods =c("cart",'fda','gam','brt','rf','glm', 'mda','rpart','svm'),  
      n=10, replication = 'cv', cv.folds=5) }

# load data:
heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

b.model.list.complete  = readRDS("./rds_objects/b.model.list.complete.rds")  # to retreive
b_data_packages        = readRDS("./rds_objects/b_data_packages.rds")
eval.list              = readRDS("./rds_objects/eval.list.5fold.100reps.rds")  # 'raw' list of full eval data from each combo
eval.summary           = readRDS("./rds_objects/eval.summary.5fold.100reps.rds")    # consolidated list form
eval.summary.df        = do.call("rbind", eval.summary) # converting the list to a single dataframe for easy plotting

b_package_names        = readRDS("./rds_objects/b_package_names.rds")
combo.names            = b_package_names
combo.descriptions     = readRDS("./rds_objects/combo.descriptions.rds")
b.preds.D              = readRDS(paste0(heavies.rds.path,"b.preds.D.rds"))

# spatial data:
beershebensis.buffer = readRDS("./rds_objects/beershebensis.buffer_incWB.rds"); 
schreiberi.buffer    = readRDS("./rds_objects/schreiberi.buffer.rds")
par(mar=c(0,0,0,0))
plot(beershebensis.buffer, xlim=c(34.267,35.39774), ylim=c(30.50798,33.10742)); plot(schreiberi.buffer, add=TRUE)
xlims_b = c(34.267142, 35.397332)
ylims_b = c(30.507978, 31.720564)
xlims_s = c(34.271733, 35.324798)  
ylims_s = c(31.126732, 33.107418)

borders                = readRDS("./rds_objects/borders.rds")
major.cities           = readRDS("./rds_objects/major.cities.rds")
small.cities           = readRDS("./rds_objects/small.cities.rds")
towns                  = readRDS("./rds_objects/towns.rds")
villages               = readRDS("./rds_objects/villages.rds")
groads                 = readRDS("./rds_objects/groads.rds")

nat.res  = readRDS(paste0(heavies.rds.path,"nat.res.rds"));  plot(nat.res, col="darkgreen",  border="darkgreen")
nat.park = readRDS(paste0(heavies.rds.path,"nat.park.rds")); plot(nat.park, col="lightgreen",border="lightgreen", add=T)

kkl.forestry   = readRDS(paste0(heavies.rds.path,"kkl.forestry.rds"))
kkl.forestry.simple = ms_simplify(kkl.forestry, keep = 0.4, keep_shapes = TRUE)
kkl.plans.a    = readRDS(paste0(heavies.rds.path,"kkl.plans.a.rds"))
kkl.plans.b    = readRDS(paste0(heavies.rds.path,"kkl.plans.b.rds"))

landuse.unsimplified          = readRDS(paste0(heavies.rds.path,"landuse.unsimplified.rds"))
landuse                       = readRDS(paste0(heavies.rds.path,"landuse.rds"))
landuse_b                     = readRDS(paste0(heavies.rds.path,"landuse_b.rds"))
landuse_s                     = readRDS(paste0(heavies.rds.path,"landuse_s.rds"))
natreserve_no_firing          = readRDS(paste0(heavies.rds.path,"natreserve_no_firing.rds"))
natreserve_firing_intersect   = readRDS(paste0(heavies.rds.path,"natreserve_firing_intersect.rds"))
natpark_no_firing             = readRDS(paste0(heavies.rds.path,"natpark_no_firing.rds"))
natpark_firing_intersect      = readRDS(paste0(heavies.rds.path,"natpark_firing_intersect.rds"))
firing_no_natcons             = readRDS(paste0(heavies.rds.path,"firing_no_natcons.rds"))
firing_excl_dist_l            = readRDS(paste0(heavies.rds.path,"firing_excl_dist_l.rds"))
firing_excl_alllanduse        = readRDS(paste0(heavies.rds.path,"firing_excl_alllanduse.rds"))

# b.disturbed            = readRDS(paste0(heavies.rds.path,"b.disturbed.rds"))
# builtup                = readRDS(paste0(heavies.rds.path,"builtup.rds"))
# agriculture            = readRDS(paste0(heavies.rds.path,"agriculture.rds"))
# dmt_uncat              = readRDS(paste0(heavies.rds.path,"dmt_uncat.rds"))
# dmt_bu_ag_plntn        = readRDS(paste0(heavies.rds.path,"dmt_bu_ag_plntn.rds"))
# INPA_dist              = readRDS(paste0(heavies.rds.path,"INPA_dist.rds"))
# KKL_ops                = readRDS(paste0(heavies.rds.path,"KKL_ops.rds"))
# military               = readRDS(paste0(heavies.rds.path,"military.rds"))
# rail                   = readRDS(paste0(heavies.rds.path,"rail.rds"))
# osm_dist               = readRDS(paste0(heavies.rds.path,"osm_dist.rds"))
# disturbed.raw.layers = list(builtup,agriculture, dmt_uncat, dmt_bu_ag_plntn,
#                             INPA_dist, KKL_ops, military, rail, osm_dist)

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

points.list = readRDS("./rds_objects/points.list.rds")
bc.full  = readRDS("./rds_objects/bc.full.rds")
bs       = readRDS("./rds_objects/b.surveys.rds")
bc       = readRDS("./rds_objects/b.collections.rds")
bsp      = readRDS("./rds_objects/b.surveys.present.rds")
bsa      = readRDS("./rds_objects/b.surveys.absent.rds")
bc.r.D   = readRDS("./rds_objects/b.collections.reliables.DEMvE.rds")
shapefile(bc.r.D, paste0(heavies.spatial.path,"bc.r.D.shp"), overwrite=TRUE)
bsa.r    = readRDS("./rds_objects/b.surveys.absence.reliables.rds")
plot(bs); points(bc); points(bsp); points(bsa); points(bc.r.D); points(bsa.r)

b_data_packages_DEMvE = readRDS(paste0(heavies.rds.path, "b_data_packages_DEMvE.rds"))

#######################################################################################################################
# Summary of outputs ----

xlims_b_d = c(34.386,35.339) # distribution limits (smaller than study area)
ylims_b_d = c(30.807,31.603)

#######################################################################################################################
# Create evaluation models ----

sdm.cv = function(data) {
  sdm(occurrence ~ ., data = data, methods =c("cart",'fda','gam','brt','rf','glm', 'mda','rpart','svm'),  
      n=10, replication = 'cv', cv.folds=5) }

# create models for each combo 
eval_models_DEMvE = list()                                          

  for (p in 1:length(combo.names))  {
  package.start = Sys.time()
  data = b_data_packages_DEMvE[[p]]
  eval_models_DEMvE[[p]] = sdm.cv(data)                       
  print(paste(combo.names[p],"loop took", difftime(Sys.time(), package.start, units="mins"), "minutes")) }

saveRDS(eval_models_DEMvE, paste0(heavies.rds.path,"eval_models_DEMvE.rds"))
emailme()
eval_models_DEMvE = readRDS(paste0(heavies.rds.path,"eval_models_DEMvE.rds"))

#######################################################################################################################
# Evaluate models ----

# an evaluation i prepared earlier:
# eval.list       = readRDS("./rds_objects/eval.list_DEMvE.rds") # 'raw' list of full eval data from each combo
# eval.summary    = readRDS("./rds_objects/eval.summary_DEMvE.rds") # consolidated list (multi reps averaged)
# eval.summary.df = readRDS("./rds_objects/eval.summary.df_DEMvE.rds") # consolidated table (all combos together)

# # or rerun:
eval.list    = list() # evaluation data, by model (i.e. 4500 models: 100repsx5 foldsx9 algorithims,for each datacombo)
eval.summary = list() # list of evaluation data, averaged by algorithim (9 dataframes of 9 rows: 9 datacombos X 9 algs)

  for (c in 1:length(combo.names))  { # make sure this number is 9
  model.inf.ev = NULL
  # extract model info and eval statistics and merge them (change name of model list as appropriate):
  model_info = getModelInfo (eval_models_DEMvE[[c]])
  model_eval = getEvaluation(eval_models_DEMvE[[c]], wtest= 'test.dep', stat=c('TSS','Kappa','AUC','COR','Deviance',
                                                                           'obs.prevalence','threshold'), opt=2)
  model.inf.ev = merge(model_info, model_eval, by = "modelID") # 'model.inf.ev' - a temporary vector for use in next bit
  # then put them into list and aggregate them
  eval.list[[c]]   = model.inf.ev
  eval.summary[[c]]= data.frame(method=aggregate(model.inf.ev$method, by=list(model.inf.ev$method),FUN=mode)[1],
                                data_combo = combo.names[[c]],
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
  names(eval.summary[[c]]) = c('method', 'data_combo', 'TSS', 'TSS_sd', 'AUC', 'AUC_sd', 'kappa', 'COR',
                                    'deviance','obs.prevalence', 'threshold.mss', 'total_models')           }

eval.summary
eval.summary.df = do.call("rbind", eval.summary) # converting list to a single df for plotting

# saving the outputs from the alternative run evaluation
saveRDS(eval.list,          "./rds_objects/eval.lis_DEMvE.rds")   # 'raw' list of full eval data from each combo
saveRDS(eval.summary,       "./rds_objects/eval.summary_DEMvE.rds")  # consolidated list (multi reps averaged)
saveRDS(eval.summary.df,    "./rds_objects/eval.summary.df_DEMvE.rds") # consolidated table (all combos together)

#######################################################################################################################
# Compare performance across methods ----

methods.summary = aggregate(eval.summary.df[, c("TSS", "AUC")],
                  eval.summary.df["method"],
                  function(x) c(mean=mean(x), sd=sd(x)) )
methods.summary$TSS.sd   = methods.summary$TSS[,2]
methods.summary$AUC.sd   = methods.summary$AUC[,2]
methods.summary$TSS.mean = methods.summary$TSS[,1]
methods.summary$AUC.mean = methods.summary$AUC[,1]
methods.summary$TSS = NULL
methods.summary$AUC = NULL
methods.summary = methods.summary[,c(1,4,2,5,3)] # getting them back in a nice order

saveRDS(methods.summary,    "./rds_objects/methods.summary.rds")  
plot(methods.summary$method, methods.summary$TSS.mean) # rf, gam, and svm are still the best (followed by brt)
plot(methods.summary$method, methods.summary$AUC.mean) # similar to above.

# # same thing, just using top algorithms
# eval.summary.df.topalgs = subset(eval.summary.df,
#                      eval.summary.df$method == 'svm' | eval.summary.df$method == 'gam'| eval.summary.df$method == 'rf')

#######################################################################################################################
# ROC curves for top 3 models ----
# step 1: make the curves and save to .png individually

# make topmodels list: 
sdm.tops = function(data) {sdm(occurrence ~ ., data = data, methods = c('gam','rf','svm'), 
      n=100, replication = 'cv', cv.folds=5) }

# create models for each combo 
model.list.cv.tops = list()

for (i in 1:length(combo.names))                                                                        {
  start.time = Sys.time();  print(combo.names[i])
  data = b_data_packages_DEMvE[[i]]
  model.list.cv.tops[[i]] = sdm.tops(data)  
  print(paste(combo.names[[i]],"loop took", difftime(Sys.time(), start.time, units="mins"), "minutes"))   }
saveRDS(model.list.cv.tops, "//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/model.list.cv.tops.rds")
  
send.mail(from="kerengila@gmail.com",       to="keren.raiter@mail.huji.ac.il",
          subject="the loop is complete",   body="yeah yeah yeah",        html=T,
          smtp=list(host.name = "smtp.gmail.com",          port = 465,
                    user.name = "kerengila@gmail.com",     passwd = "Ivrit333",
                    ssl = T), authenticate=T) #, attach.files="C:\\Users\\Deepanshu\\Nature.xls"

# step 1a: run the first combo (top row) separately, to give it titles: 
i = 1
start_time = Sys.time()
combo.letter = list ("A","B","C","D","E","F","G","H","I")

  filename = paste0(heavies.image.path,"ROC ", combo.names[[i]],' - RF.png')
  png(filename = filename, width = 9.975, height = 5.7, units = "cm", res = 300)
  par(mar = c(0,2,1.5,0), mgp=c(3, 0.5, 0), las=1) # bottom, left, top, and right
  roc(model.list.cv.tops[[i]], method = "rf",  smooth = T, cex.axis = 0.9, tck=-0.03, cex.main = 1.4, main= "Random forests")
  text(x = 0.03, y = 0.94, labels = combo.letter[[i]],cex = 1.3, xpd = NA)
  dev.off()
  
  filename = paste0(heavies.image.path,"ROC ", combo.names[[i]],' - SVM.png')
  png(filename = filename, width = 9.5, height = 5.7, units = "cm", res = 300)
  par(mar = c(0,0,1.5,0)) # bottom, left, top, and right
  roc(model.list.cv.tops[[i]],method = "svm",  smooth = T, cex.main = 1.4, main= "Support vector machines")
  dev.off()
  
  filename = paste0(heavies.image.path,"ROC ", combo.names[[i]],' - GAM.png')
  png(filename = filename, width = 9.5, height = 5.7, units = "cm", res = 300)
  par(mar = c(0,0,1.5,0)) # bottom, left, top, and right
  roc(model.list.cv.tops[[i]],method = "gam",  smooth = T, cex.main = 1.4, main= "Generalised additive models")
  dev.off()
  
  print(paste(combo.names[[i]],"loop took", difftime(Sys.time(),start_time, units="mins"), "minutes")) 
  
# step 1b: run the middle rows (no titles, no x-axis labels):
for (i in 2:(length(combo.names)-1))                                                                  {
  start_time = Sys.time()
  
  filename = paste0(heavies.image.path,"ROC ", combo.names[[i]],' - RF.png')
  png(filename = filename, width = 9.975, height = 5.225, units = "cm", res = 300)
  par(mar = c(0,2,0,0), mgp=c(3, 0.5, 0), las=1) # bottom, left, top, and right
  roc(model.list.cv.tops[[i]],method = "rf",  smooth = T, cex.axis = 0.9, tck=-0.02, main = NULL)
  text(x = 0.03, y = 0.94, labels = combo.letter[[i]],cex = 1.3, xpd = NA)
  dev.off() 
  
  filename = paste0(heavies.image.path,"ROC ", combo.names[[i]],' - SVM.png')
  png(filename = filename, width = 9.5, height = 5.225, units = "cm", res = 300)
  par(mar = c(0,0,0,0)) # bottom, left, top, and right
  roc(model.list.cv.tops[[i]],method = "svm",  smooth = T, main = NULL)
  dev.off()
  
  filename = paste0(heavies.image.path,"ROC ", combo.names[[i]],' - GAM.png')
  png(filename = filename, width = 9.5, height = 5.225, units = "cm", res = 300)
  par(mar = c(0,0,0,0)) # bottom, left, top, and right
  roc(model.list.cv.tops[[i]],method = "gam",  smooth = T, main = NULL)
  dev.off()
  
  print(paste(combo.names[[i]],"loop took", difftime(Sys.time(),start_time, units="mins"), "minutes"))}

# step 1c: run the last combo separately, to give it X-axis labels: 
i = 9
start_time = Sys.time()

  filename = paste0(heavies.image.path,"ROC ", combo.names[[i]],' - RF.png')
  png(filename = filename, width = 9.975, height = 6.65, units = "cm", res = 300)
  par(mar = c(3,2,0,0), mgp=c(3, 0.5, 0), las=1) # bottom, left, top, and right
  roc(model.list.cv.tops[[i]],method = "rf",  smooth = T, cex.axis = 0.9, tck=-0.02, main= NULL)
  text(x = 0.02, y = 0.95, labels = combo.letter[[i]],cex = 1.3, xpd = NA)
  dev.off()
  
  filename = paste0(heavies.image.path,"ROC ", combo.names[[i]],' - SVM.png')
  png(filename = filename, width = 9.5, height = 6.65, units = "cm", res = 300)
  par(mar = c(3,0,0,0), mgp=c(3, 0.5, 0), las=1) # bottom, left, top, and right
  roc(model.list.cv.tops[[i]],method = "svm",  smooth = T, cex.lab = 0.7, cex.main = 0.8, main= NULL)
  dev.off()
  
  filename = paste0(heavies.image.path,"ROC ", combo.names[[i]],' - GAM.png')
  png(filename = filename, width = 9.5, height = 6.65, units = "cm", res = 300)
  par(mar = c(3,0,0,0), mgp=c(3, 0.5, 0), las=1) # bottom, left, top, and right
  roc(model.list.cv.tops[[i]],method = "gam",  smooth = T, cex.lab = 0.7, cex.main = 0.8, main= NULL)
  dev.off()
  
  print(paste(combo.names[[i]],"loop took", difftime(Sys.time(),start_time, units="mins"), "minutes")) 

# step 2: combine plots (for want of a better way to do this!):

png(filename = paste0(heavies.image.path,"ROC - All combinations - top models.png"),
    width=26, height=40, units="cm", res=600)
par(mar = c(1,1,0,0), mgp=c(0,0,0)) # mgp sets distance between (1) the axis titles and the axes and (2) the axis labels and the axes. Default is mgp = c(3, 0.1, 0).
plot(0:9, 0:9, type = "n", xaxt = "n", yaxt = "n", xlab = "1 - Specificity (false positive rate)", ylab = "Sensitivity (true positive rate)", bty="n")
# numbers at end: xstart, ystart, xend, yend
# combination A
rasterImage(readPNG(source="./output_images/ROC Combination A - RF.png"),  -0.3, 7.8, 3, 9) 
rasterImage(readPNG(source="./output_images/ROC Combination A - SVM.png"), 3, 7.8, 6, 9)
rasterImage(readPNG(source="./output_images/ROC Combination A - GAM.png"), 6, 7.8, 9, 9)
# combination B
rasterImage(readPNG(source="./output_images/ROC Combination B - RF.png"),  -0.3, 6.8, 3, 7.8)
rasterImage(readPNG(source="./output_images/ROC Combination B - SVM.png"), 3, 6.8, 6, 7.8)
rasterImage(readPNG(source="./output_images/ROC Combination B - GAM.png"), 6, 6.8, 9, 7.8)
# combination C
rasterImage(readPNG(source="./output_images/ROC Combination C - RF.png"),  -0.3, 5.8, 3, 6.8)
rasterImage(readPNG(source="./output_images/ROC Combination C - SVM.png"), 3, 5.8, 6, 6.8)
rasterImage(readPNG(source="./output_images/ROC Combination C - GAM.png"), 6, 5.8, 9, 6.8)
# combination D
rasterImage(readPNG(source="./output_images/ROC Combination D - RF.png"),  -0.3, 4.8, 3, 5.8)
rasterImage(readPNG(source="./output_images/ROC Combination D - SVM.png"), 3, 4.8, 6, 5.8)
rasterImage(readPNG(source="./output_images/ROC Combination D - GAM.png"), 6, 4.8, 9, 5.8)
# combination E
rasterImage(readPNG(source="./output_images/ROC Combination E - RF.png"),  -0.3, 3.8, 3, 4.8)
rasterImage(readPNG(source="./output_images/ROC Combination E - SVM.png"), 3, 3.8, 6, 4.8)
rasterImage(readPNG(source="./output_images/ROC Combination E - GAM.png"), 6, 3.8, 9, 4.8)
# combination F
rasterImage(readPNG(source="./output_images/ROC Combination F - RF.png"),  -0.3, 2.8, 3, 3.8)
rasterImage(readPNG(source="./output_images/ROC Combination F - SVM.png"), 3, 2.8, 6, 3.8)
rasterImage(readPNG(source="./output_images/ROC Combination F - GAM.png"), 6, 2.8, 9, 3.8)
# combination G
rasterImage(readPNG(source="./output_images/ROC Combination G - RF.png"),  -0.3, 1.8, 3, 2.8)
rasterImage(readPNG(source="./output_images/ROC Combination G - SVM.png"), 3, 1.8, 6, 2.8)
rasterImage(readPNG(source="./output_images/ROC Combination G - GAM.png"), 6, 1.8, 9, 2.8)
#combination H
rasterImage(readPNG(source="./output_images/ROC Combination H - RF.png"),  -0.3, 0.8, 3, 1.8)
rasterImage(readPNG(source="./output_images/ROC Combination H - SVM.png"), 3, 0.8, 6, 1.8)
rasterImage(readPNG(source="./output_images/ROC Combination H - GAM.png"), 6, 0.8, 9, 1.8)
# combination I
rasterImage(readPNG(source="./output_images/ROC Combination I - RF.png"),  -0.3, -0.4, 3, 0.8)
rasterImage(readPNG(source="./output_images/ROC Combination I - SVM.png"), 3, -0.4, 6, 0.8)
rasterImage(readPNG(source="./output_images/ROC Combination I - GAM.png"), 6, -0.4, 9, 0.8)

dev.off()

send.mail(from="kerengila@gmail.com",       to="keren.raiter@mail.huji.ac.il",
          subject="the loop is complete",   body="yeah yeah yeah",        html=T,
          smtp=list(host.name = "smtp.gmail.com",          port = 465,
                    user.name = "kerengila@gmail.com",     passwd = "Ivrit333",
                    ssl = T), authenticate=T) #, attach.files="C:\\Users\\Deepanshu\\Nature.xls"

#######################################################################################################################
# Complete models for each data combo ----

b.complete.models = list() 

for (i in 1:length(b_data_packages))                                                                        {
  start.time = Sys.time()
  print(combo.names[i])
  data = b_data_packages_DEMvE[[i]]
  b.complete.models[[i]] = sdm(occurrence ~ ., data = data, methods =c('gam','rf','svm'))
  print(paste(combo.names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes"))}

# see how they went:
combo.names[[1]]; b.complete.models[[1]]
combo.names[[2]]; b.complete.models[[2]]
combo.names[[3]]; b.complete.models[[3]]
combo.names[[4]]; b.complete.models[[4]]
combo.names[[5]]; b.complete.models[[5]]
combo.names[[6]]; b.complete.models[[6]]
combo.names[[7]]; b.complete.models[[7]]
combo.names[[8]]; b.complete.models[[8]]
combo.names[[9]]; b.complete.models[[9]]
b.complete.models[[i]][[5]]

# # save and/or retreive
# saveRDS(b.model.list.complete, "./rds_objects/b.model.list.complete.rds")    # to save
# b.model.list.complete  = readRDS("./rds_objects/b.model.list.complete.rds")  # to retreive

#######################################################################################################################
# Predict the model outputs ----

predmaps.rf    = list()
predmaps.svm   = list()
predmaps.gam   = list()

for (i in 1:length(combo.names))                                                           {
  start.time       = Sys.time()
  par(mar=c(2,2,2,1))

  filename.rf  = paste0(heavies.spatial.path, 'Prediction ', combo.names[[i]], ' - RF.tif')
  predmaps.rf[[i]] = predict(b.complete.models[[i]], newdata = b.preds.D, filename = filename.rf, 
                             format="GTiff", overwrite=TRUE, w=2, nc=20)
  plot(predmaps.rf[[i]], main=paste(combo.names[[i]],"RF, TSS =", eval.summary[[i]][2,"TSS"]))
  
  filename.svm = paste0(heavies.spatial.path, 'Prediction ', combo.names[[i]], ' - SVM.tif')
  predmaps.svm[[i]]= predict(b.complete.models[[i]], newdata = b.preds.D, filename = filename.svm, 
                             format="GTiff", overwrite=TRUE, w=3, nc=20)
  plot(predmaps.svm[[i]], main=paste(combo.names[[i]],"SVM, TSS =", eval.summary[[i]][3,"TSS"]))
  
  filename.gam = paste0(heavies.spatial.path, 'Prediction ', combo.names[[i]], ' - GAM.tif')
  predmaps.gam[[i]]= predict(b.complete.models[[i]], newdata = b.preds.D, filename = filename.gam, 
                             format="GTiff", overwrite=TRUE, w=1, nc=20)
  plot(predmaps.gam[[i]], main=paste(combo.names[[i]],"GAM, TSS =", eval.summary[[i]][1,"TSS"]))

  print(paste(combo.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

emailme()

# how to view model info:
# summary(predmaps.gam[[i]])

# Retreive predicted outcomes and plot ----

# predmaps.rf    = list()
# predmaps.svm   = list()
# predmaps.gam   = list()

# for(i in 1:length(combo.names))                                                                       {
#  
#   filename.rf      = paste('./output_images/Prediction', combo.names[[i]], '- RF.tif')
#   predmaps.rf[[i]] = raster(filename.rf)
#   plot(predmaps.rf[[i]], main=paste(combo.names[[i]],"RF, TSS =", eval.summary[[i]][5,"TSS"]))
#   
#   filename.svm      = paste('./output_images/Prediction', combo.names[[i]], '- SVM.tif')
#   predmaps.svm[[i]] = raster(filename.svm)
#   plot(predmaps.svm[[i]], main=paste(combo.names[[i]],"SVM, TSS =", eval.summary[[i]][9,"TSS"]))
#   
#   filename.gam     = paste('./output_images/Prediction', combo.names[[i]], '- GAM.tif')
#   predmaps.gam[[i]] = raster(filename.gam)
#   plot(predmaps.gam[[i]], main=paste(combo.names[[i]],"GAM, TSS =", eval.summary[[i]][3,"TSS"]))   }

# Save plots as pictures ----
# for(i in 1:length(combo.names))                                                                    {
#   
#   filename.rf      = paste('./output_images/Predicted probabilities', combo.names[[i]], '- RF.png')
#   png(filename     = filename.rf, width = 18, height = 15, units = 'cm', res = 600)
#   plot(predmaps.rf[[i]], main=paste(combo.names[[i]],"RF, TSS =", eval.summary[[i]][5,"TSS"]))
#   dev.off()
#   
#   filename.svm     = paste('./output_images/Predicted probabilities', combo.names[[i]], '- SVM.png')
#   png(filename     = filename.svm, width = 18, height = 15, units = 'cm', res = 600)
#   plot(predmaps.svm[[i]], main=paste(combo.names[[i]],"SVM, TSS =", eval.summary[[i]][9,"TSS"]))
#   dev.off()
#   
#   filename.gam      = paste('./output_images/Predicted probabilities', combo.names[[i]], '- GAM.png')
#   png(filename      = filename.gam, width = 18, height = 15, units = 'cm', res = 600)
#   plot(predmaps.gam[[i]], main=paste(combo.names[[i]],"GAM, TSS =", eval.summary[[i]][3,"TSS"])) 
#   dev.off()                                                                                            }

#######################################################################################################################
# Map the model occurrence predictions using threshold that maximises sensitivity plus specificity ----

occmaps.rf  =list()
occmaps.svm =list()
occmaps.gam =list()

for (i in 1:length(combo.names))                                                                 { # takes 10 mins
  
  start.time = Sys.time()
  
  threshold.rf     = eval.summary[[i]][2, "threshold.mss"]
  threshold.svm    = eval.summary[[i]][3, "threshold.mss"]
  threshold.gam    = eval.summary[[i]][1, "threshold.mss"]
  
  occmaps.rf[[i]]  = predmaps.rf[[i]]  ; occmaps.rf[[i]][occmaps.rf[[i]]   < threshold.rf]   <- 0
  occmaps.svm[[i]] = predmaps.svm[[i]] ; occmaps.svm[[i]][occmaps.svm[[i]] < threshold.svm]  <- 0
  occmaps.gam[[i]] = predmaps.gam[[i]] ; occmaps.gam[[i]][occmaps.gam[[i]] < threshold.gam]  <- 0
  # occmaps.rf[[i]][occmaps.rf[[i]]   >= threshold.rf]  <- 1 # optional: make one-shade prediction.
  # occmaps.svm[[i]][occmaps.svm[[i]] >= threshold.svm] <- 1 # optional: make one-shade prediction.
  # occmaps.gam[[i]][occmaps.gam[[i]] >= threshold.gam] <- 1 # optional: make one-shade prediction.
 
  hist(predmaps.rf[[i]]);  points(x=threshold.rf,  y=0, pch=24, bg='red')
  hist(predmaps.svm[[i]]); points(x=threshold.svm, y=0, pch=24, bg='red')
  hist(predmaps.gam[[i]]); points(x=threshold.gam, y=0, pch=24, bg='red')
  
  filename.rf  = paste0(heavies.images.path,'Predicted distribution ', combo.names[[i]], ' - RF.png')
  png(filename = filename.rf, width = 18, height = 15, units = 'cm', res = 600)
  plot(occmaps.rf[[i]]) # can also like this: plot(predmaps.rf[[i]], col = c('white','green'),breaks=c(0,threshold.rf,1))
  dev.off()
  filename.rf  = paste0(heavies.spatial.path,'Predicted distribution ', combo.names[[i]], ' - RF.tif')
  writeRaster(occmaps.rf[[i]], filename = filename.rf, options=c('TFW=YES'), overwrite= TRUE)
  
  filename.svm  = paste0(heavies.images.path,'Predicted distribution ', combo.names[[i]], ' - SVM.png')
  png(filename  = filename.svm, width = 18, height = 15, units = 'cm', res = 600)
  plot(occmaps.svm[[i]]) # can also like this: plot(predmaps.svm[[i]], col=c('white','green'),breaks=c(0,threshold.svm,1))
  dev.off()
  filename.svm  = paste0(heavies.spatial.path,'Predicted distribution', combo.names[[i]], '- SVM.tif')
  writeRaster(occmaps.svm[[i]], filename = filename.svm, options=c('TFW=YES'), overwrite=TRUE)
  
  filename.gam  = paste0(heavies.images.path,'Predicted distribution ', combo.names[[i]], ' - GAM.png')
  png(filename  = filename.gam, width = 18, height = 15, units = 'cm', res = 600)
  plot(occmaps.gam[[i]]) # can also like this: plot(predmaps.gam[[i]], col=c('white','green'),breaks=c(0,threshold.gam,1))
  dev.off()
  filename.gam  = paste0(heavies.spatial.path,'Predicted distribution ', combo.names[[i]], ' - GAM.tif')
  writeRaster(predmaps.gam[[i]], filename = filename.gam, options=c('TFW=YES'), overwrite=TRUE)       
  
  print(paste(combo.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

# Make one composite image ----
plot(occmaps.rf[[i]], frame.plot=FALSE) # checking coordinate ranges for text placement

png(filename=paste0(heavies.images.path,"Predicted distributions - all combinations.png"), width=30,height=60,units="cm", res=600)
par(mfrow=c(9,3), mar = c(0,0,2,2), mgp=c(0,0,0), bty="n")
for (i in 1:length(combo.names))                           {
  plot(occmaps.rf[[i]], axes=FALSE, ann=FALSE)
  text(x = 34.1, y = 31.6, labels = "RF",cex = 1.3, xpd = NA)
  plot(occmaps.svm[[i]], axes=FALSE, main = combo.descriptions[[i]])
  text(x = 34.1, y = 31.6, labels = "SVM",cex = 1.3, xpd = NA)
  plot(occmaps.gam[[i]], axes=FALSE, ann=FALSE, main = "GAM")
  text(x = 34.1, y = 31.6, labels = "GAM",cex = 1.3, xpd = NA)  }
dev.off()

emailme()

#######################################################################################################################
# Run combo-ensemble loop ----
ensembles = list()

for (i in 1:length(combo.names))                                                                       { 
  start.time = Sys.time();  print(combo.names[i])
  weights = c(eval.summary[[i]]$TSS[eval.summary[[i]]$method == "rf"],
              eval.summary[[i]]$TSS[eval.summary[[i]]$method == "svm"],
              eval.summary[[i]]$TSS[eval.summary[[i]]$method == "gam"])
  filename = paste0(heavies.spatial.path,'Ensemble - ',combo.names[[i]], '.tif')
  ensembles[[i]] <- ensemble(b.complete.models[[i]], newdata = b.preds.D, filename = filename,
                             setting = list(method = 'weighted', weights = weights), nc=20, format="GTiff")  
  print(paste(combo.names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes")) }

saveRDS(ensembles, paste0(heavies.rds.path,"ensembles_DEMvE.rds"))
ensembles = readRDS(paste0(heavies.rds.path,"ensembles_DEMvE.rds"))
# plot(ensembles[[2]]) # just checking

emailme()
#######################################################################################################################
# Map the combo-ensemble distributions using threshold that maximises sensitivity plus specificity ----

# here 'prediction' = prediction with full range of probabilities; distribution = above-threshold.
ensemble.dist = list()
ensemble.thresholds = list()

for (i in 1:length(combo.names)) {
  
  start.time = Sys.time()
  
  # get threshold:
  ensemble.thresholds[[i]] = mean(eval.summary[[i]][2, "threshold.mss"], 
                                  eval.summary[[i]][3, "threshold.mss"],
                                  eval.summary[[i]][1, "threshold.mss"])
  
  # get distribution:
  ensemble.dist[[i]]  = ensembles[[i]]
  ensemble.dist[[i]][ensemble.dist[[i]] <  ensemble.thresholds[[i]]] <- NA
  ensemble.dist[[i]][ensemble.dist[[i]] >= ensemble.thresholds[[i]]] = 1 # could leave out this line to show var
  # leaving above line out, would show the variability of probabilities above the threshold.
  
  # make histogram:
  filename.hist  = paste0(heavies.image.path,'Ensemble histogram - ', combo.names[[i]], '.png')
  png(filename = filename.hist, width = 18, height = 15, units = 'cm', res = 300)
  hist(ensembles[[i]]);  points(x = ensemble.thresholds[[i]], y=0, pch=24, bg='red')
  dev.off()
  
  # make image:                        # strangely, this works in the loop, but plots a blob when run individually.
  filename  = paste0(heavies.image.path,'Ensemble distribution - ', combo.names[[i]], ' oneshade.png')
  png(filename = filename, width = 12, height = 10, units = 'cm', res = 900)
  par(mar = c(0,0,0,0))
  plot(ensemble.dist[[i]], col = "139", xlim=c(34.2,35.4), ylim=c(30.7,31.7), legend = F) #30.7-31.6 34.2-35.4
  # plot(ensemble.dist[[i]], col = c('white','green'), breaks=c(0,threshold.rf,1)) #alternative way to plot two-shade
  dev.off()
  
  # make raster:
  filename.raster = paste0(heavies.spatial.path,'Ensemble distribution - ', combo.names[[i]], ' oneshade.tif')
  writeRaster(ensemble.dist[[i]], filename = filename.raster, options=c('TFW=YES'), overwrite= TRUE)
  
  print(paste(combo.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

saveRDS(ensemble.thresholds, "./rds_objects/ensemble.thresholds.rds")
saveRDS(ensemble.dist, paste0(heavies.rds.path,"ensemble.dist.oneshade.rds"))

emailme()
#######################################################################################################################
# Map pairs of predictions to elucidate data effects ----
combo.descriptions
myLetters <- letters[1:26] # setting up letter-number correlation. e.g. match("a", myLetters)

# 1) The effect of the questionable absences ----
# A vs E, C vs B, F vs D # list the more comprehensive model first
combos = c("a","e","c","b","f","d")
numbers   = list(match(combos[[1]], myLetters), match(combos[[2]], myLetters), match(combos[[3]], myLetters),
                 match(combos[[4]], myLetters), match(combos[[5]], myLetters), match(combos[[6]], myLetters))
rasters = list()
meanTSS = list()

png(filename = paste0(heavies.image.path,"Combo comparisons - questionable absences.png"), width=20, height=25, units='cm', res=900)
par(mfrow=c(3,2), mar =c(0,0,1,0), bty="n")
for (i in 1:length(combos)) {
  rasters[[i]] = raster(paste0(heavies.spatial.path,"Ensemble distribution - Combination ", toupper(combos[[i]]),".tif"))
  number = numbers[[i]]
  meanTSS[[i]] = round(mean(eval.summary[[number]]$TSS[c(3,5,9)]), digits=2)
  plot(rasters[[i]], xlim=c(34.45,35.2), ylim=c(30.85,31.4), bty="n", axes=FALSE, legend=F,
       main=paste(combo.descriptions[[number]],"TSS=",meanTSS[[i]]), cex.main=1.3)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))}
dev.off()
# conclusion: the questionable absences have limited the predicted northern distribution considerably.


# 2) The effect of the unreliable collections records ----
# A vs F, E vs D, I vs H # listing the more comprehensive model first
combos = c("a","f","e","d","i","h")
numbers   = list(match(combos[[1]], myLetters), match(combos[[2]], myLetters),match(combos[[3]], myLetters),
                 match(combos[[4]], myLetters), match(combos[[5]], myLetters),match(combos[[6]], myLetters))
rasters = list()
meanTSS = list()

png(filename = paste0(heavies.image.path,"Combo comparisons - unreliable coll data.png"), width=20, height=25, units='cm', res=900)
par(mfrow=c(3,2), mar =c(0,0,1,0), bty="n")
for (i in 1:length(combos)) {
  rasters[[i]] = raster(paste0(heavies.spatial.path,"Ensemble distribution - Combination ",toupper(combos[[i]]),".tif"))
  number = numbers[[i]]
  meanTSS[[i]] = round(mean(eval.summary[[number]]$TSS[c(3,5,9)]), digits=2)
  plot(rasters[[i]], xlim=c(34.45,35.2), ylim=c(30.85,31.4), bty="n", axes=FALSE, legend=F,
       main=paste(combo.descriptions[[number]],"TSS=",meanTSS[[i]]), cex.main=1.3)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))}
dev.off()

# 3) Presence-Absence versus presence-only ----
# B vs G, D vs H, E vs I # listing the more comprehensive model first
combos = c("b","g","d","h","e","i")
numbers   = list(match(combos[[1]], myLetters), match(combos[[2]], myLetters),match(combos[[3]], myLetters),
                 match(combos[[4]], myLetters), match(combos[[5]], myLetters),match(combos[[6]], myLetters))
rasters = list()
meanTSS = list()

png(filename = paste0(heavies.image.path,"Combo comparisons - PA versus PO.png"), width = 20, height = 25, units = 'cm', res = 900))
par(mfrow=c(3,2), mar =c(0,0,1,0), bty="n")
for (i in 1:length(combos)) {
  rasters[[i]] = raster(paste0(heavies.spatial.path,"Ensemble distribution - Combination ",toupper(combos[[i]]),".tif"))
  number = numbers[[i]]
  meanTSS[[i]] = round(mean(eval.summary[[number]]$TSS[c(3,5,9)]), digits=2)
  plot(rasters[[i]], xlim=c(34.45,35.2), ylim=c(30.85,31.4), bty="n", axes=FALSE, legend=F, 
       main=paste(combo.descriptions[[number]],"TSS=",meanTSS[[i]]), cex.main=1.3)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))}
dev.off()

#######################################################################################################################
# Plot response curves ----

rcurves = list()

i=1 # for some reason this for-loop doesn't produce viable images! but running each iteration manually does.
# for (i in 1:length(combo.names))                                                                 {
filename = paste0(heavies.image.path,'Mean response curves - ', combo.names[[i]], '.png')
rcurves[[i]] = rcurve(b.complete.models[[i]], id=1:3, mean=TRUE, confidence=TRUE, smooth=T, main=title, size=1000)

png(filename = filename, width = 18, height = 5, units = 'cm', res = 600)
par(mar=c(0,0,1,0))
curve = rcurve(b.complete.models[[i]], id=1:3, mean = TRUE, confidence = TRUE, smooth = T, 
               main = combo.names[[i]], size= 1000, main.cex=0.5, gg=T)
curve + theme(axis.text.x = element_blank(),    axis.text.y=element_blank(),  
              axis.title.x=element_blank(),     legend.position="none", panel.background=element_blank(),   
              panel.grid.major=element_blank(), plot.background=element_blank())
dev.off()  
# }

png(filename = paste0(heavies.image.path,'Mean response curves - all combinations.png'), width=30, height = 54, units = "cm", res=600)
par(mar = c(0,0,0,0), mgp=c(0,0,0))
plot(0:9, 0:9, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
rasterImage(readPNG(source="./output_images/Mean response curves - Combination A.png"),  0, 8, 9, 9)
rasterImage(readPNG(source="./output_images/Mean response curves - Combination B.png"),  0, 7, 9, 8)
rasterImage(readPNG(source="./output_images/Mean response curves - Combination C.png"),  0, 6, 9, 7)
rasterImage(readPNG(source="./output_images/Mean response curves - Combination D.png"),  0, 5, 9, 6)
rasterImage(readPNG(source="./output_images/Mean response curves - Combination E.png"),  0, 4, 9, 5)
rasterImage(readPNG(source="./output_images/Mean response curves - Combination F.png"),  0, 3, 9, 4)
rasterImage(readPNG(source="./output_images/Mean response curves - Combination G.png"),  0, 2, 9, 3)
rasterImage(readPNG(source="./output_images/Mean response curves - Combination H.png"),  0, 1, 9, 2)
rasterImage(readPNG(source="./output_images/Mean response curves - Combination I.png"),  0, 0, 9, 1)
dev.off()

# note: use getResponseCurve() at https://rdrr.io/cran/sdm/man/response.html to extract the actual data and 
# plot more nicely (time consuming).
# use rcurve(b.complete.models[[1]], id=1, smooth = T, main = "whatever") to plot individual models.

saveRDS(rcurves, "./rds_objects/rcurves.rds")

#######################################################################################################################
# Plot variable importance ----

# 'how to' notes:
{#getVarImp(m,id=1,wtest='training') # variable importance based on training dataset

#vi <- getVarImp(m,id=1,wtest='test.dep') 

#plot(vi,'auc')

#plot(vi,'cor')
  }

varimportance.rf  = list()
varimportance.svm = list()
varimportance.gam = list()
varimportance     = list()
par(mar=c(3,4.5,3,1.5))

for (i in 1:length(combo.names))                                                                          {
  varimportance.rf[[i]]  = getVarImp(b.complete.models[[i]], id=1, wtest='training', varImportance='tss')
  varimportance.svm[[i]] = getVarImp(b.complete.models[[i]], id=2, wtest='training', varImportance='tss')
  varimportance.gam[[i]] = getVarImp(b.complete.models[[i]], id=3, wtest='training', varImportance='tss')
  
  varimportance[[i]] = data.frame(variable = c(varimportance.rf[[i]]@variables[1], varimportance.rf[[i]]@variables[2], 
                                               varimportance.rf[[i]]@variables[3], varimportance.rf[[i]]@variables[4], 
                                               varimportance.rf[[i]]@variables[5], varimportance.rf[[i]]@variables[6]),
                             importance = c(mean(c(varimportance.rf[[i]]@varImportance$AUCtest[1], 
                                                   varimportance.svm[[i]]@varImportance$AUCtest[1],
                                                   varimportance.gam[[i]]@varImportance$AUCtest[1])),
                                            mean(c(varimportance.rf[[i]]@varImportance$AUCtest[2], 
                                                   varimportance.svm[[i]]@varImportance$AUCtest[2],
                                                   varimportance.gam[[i]]@varImportance$AUCtest[2])),
                                            mean(c(varimportance.rf[[i]]@varImportance$AUCtest[3], 
                                                   varimportance.svm[[i]]@varImportance$AUCtest[3],
                                                   varimportance.gam[[i]]@varImportance$AUCtest[3])),
                                            mean(c(varimportance.rf[[i]]@varImportance$AUCtest[4], 
                                                   varimportance.svm[[i]]@varImportance$AUCtest[4],
                                                   varimportance.gam[[i]]@varImportance$AUCtest[4])),
                                            mean(c(varimportance.rf[[i]]@varImportance$AUCtest[5], 
                                                   varimportance.svm[[i]]@varImportance$AUCtest[5],
                                                   varimportance.gam[[i]]@varImportance$AUCtest[5])),
                                            mean(c(varimportance.rf[[i]]@varImportance$AUCtest[6], 
                                                   varimportance.svm[[i]]@varImportance$AUCtest[6],
                                                   varimportance.gam[[i]]@varImportance$AUCtest[6]))))

  order = rev(c(1,3,6,2,5,4))  # Get order of rows, for plotting in order that is consistent with overall importance (below)
  varimportance[[i]] <- varimportance[[i]][order,]      # sort

  title    = paste0("Variable importance for ", combo.names[[i]])
  filename = paste0('./output_images/Variable importance - ', combo.names[[i]], '.png')
  png(filename = filename, width = 18, height = 10, units = 'cm', res = 600)
  # par(mar = c(5.1, 4.1, 4.1, 2.1)) # the default.
  par(mar = c(3, 4.5, 3, 1.5)) # sets the bottom, left, top and right margins respectively, in number of lines of text.
  barplot(varimportance[[i]]$importance, 
        names.arg = varimportance[[i]]$variable, xlab = "Variable importance averaged across the three top models",
        main=title, horiz=TRUE, cex.names=0.8, las=1, mgp=c(3,0.2,0), tck=-0.008, cex.main=1.6, col='lightgreen', xlim=c(0,0.4)) 
  dev.off()                                                                                                     }

saveRDS(varimportance.rf,  paste0(heavies.rds.path,"varimportance.rf.rds"))
saveRDS(varimportance.svm, paste0(heavies.rds.path,"varimportance.svm.rds"))
saveRDS(varimportance.gam, paste0(heavies.rds.path,"varimportance.gam.rds"))
saveRDS(varimportance,     paste0(heavies.rds.path,"varimportance.rds"))

# Total variable importance, averaged over all models ----
# varimportance[[1]]@varImportance$AUCtest[1] 
# v=1
# varimportance[[1]]@variables[v]

var.importance.allcombos = list()

for (v in 1:length(varimportance[[1]]$variable))          {
  
  var = varimportance[[1]]$variable[[v]] # name of variable
  
  var.importance.allcombos[[v]] = data.frame(Variable = var,
             combo1 = varimportance[[1]]$importance[v], # the one refers to the data combo, the v refers to variable in question
             combo2 = varimportance[[2]]$importance[v],
             combo3 = varimportance[[3]]$importance[v],
             combo4 = varimportance[[4]]$importance[v],
             combo5 = varimportance[[5]]$importance[v],
             combo6 = varimportance[[6]]$importance[v],
             combo7 = varimportance[[7]]$importance[v],
             combo8 = varimportance[[8]]$importance[v],
             combo9 = varimportance[[9]]$importance[v])    }

var.importance.allcombos.df      = do.call("rbind", var.importance.allcombos)
var.importance.allcombos.df$mean = rowMeans(var.importance.allcombos.df[,c("combo1","combo2","combo3","combo4","combo5",
                                                                           "combo6","combo7","combo8","combo9")] )

order <- order(var.importance.allcombos.df$mean)                        # Get order of rows, for plotting in order
var.importance.allcombos.df <- var.importance.allcombos.df[order,]      # sort

png("./output_images/Variable importance - averaged across all (xlim 0.4).png", width=18, height =10, units='cm', res=600)
# par() #default is 5.1 4.1 4.1 2.1
par(mar=c(3,4.5,3,1.5))
barplot(var.importance.allcombos.df$mean, 
        names.arg = var.importance.allcombos.df$Variable, xlab = "Variable importance averaged across all models",
        main="Averaged variable importance", horiz=TRUE, cex.names=0.8, las=1, mgp=c(3,0.2,0), tck=-0.008, cex.main=1.6, 
        col='lightblue', xlim=c(0,0.4))
dev.off()

saveRDS(var.importance.allcombos,    "./rds_objects/var.importance.rds")
saveRDS(var.importance.allcombos.df, "./rds_objects/var.importance.df.rds")

#######################################################################################################################
# The grand ensemble ----

# ensemble.stack = stack(ensembles)
# grand_ensemble = overlay(ensemble.stack, fun=mean)  # takes ~15 mins
# summary(grand_ensemble[])
# min(grand_ensemble[], na.rm=T)
# max(grand_ensemble[], na.rm=T)
# grand_ensemble[grand_ensemble < 0] = 0
# grand_ensemble[grand_ensemble > 1] = 1
# writeRaster(grand_ensemble, filename = paste0(heavies.spatial.path,"grandensemble_DEMvE.tif"), options=c('TFW=YES'), overwrite= TRUE)
grand_ensemble = raster(paste0(heavies.spatial.path,"grandensemble_DEMvE.tif"))

# png(filename = paste0(heavies.image.path,'Grand_ensemble DEMvE.png'), width = 30, height = 22, units = 'cm', res = 600)
# par(mar=c(1,1,2,1), bty="n")
# plot(grand_ensemble, xlim=c(34.3,35.4), ylim=c(30.8,31.6), main="Grand Ensemble", cex.main=1.5, bty="n", axes=FALSE)
# plot(bs,pch=16,cex=.6,main="Beershebensis observations",cex.main=1,font.main=2, add=T)
# points(bs[bs$presence == "present",],col='green', pch=16, cex=0.1)
# points(bs[bs$presence == "absent",], col='black', pch=16, cex=0.1)
# lines(borders, lty=5, lwd=1.5, col="grey15")
# lines(groads, col="grey73")
# points(major.cities, pch=21, col='black', bg='yellow')
# with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))
# points(bsp,     bg  = 'yellow',   pch=22,  cex=0.9)
# points(bsa,     bg = 'cyan',    pch=21,  cex=1)
# points(bsa.r,   bg = 'red',     pch=21,  cex=1)
# points(bc,      bg = 'green',   pch=24,  cex=0.9)
# points(bc.r.E,  bg = 'green4',  pch=24,  cex=0.9)
# plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
# with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8, offset=0.3))
# legend("bottomright",
#        c("Surveyed presence (75)", "Surveyed absence-questionable (24)","Surveyed absence-reliable (103)",
#          "Collections record-unreliable (71)","Collections record-reliable (243)"),
#        pt.bg=c("yellow","cyan","red","green","green4"),
#        pch=c(22,21,21,24,24),    cex=0.8, text.font=1)
# dev.off()

# The grand distribution ----

ensemble.thresholds = readRDS("./rds_objects/b.ensemble.thresholds.rds")
grand.threshold     = mean(do.call("rbind", ensemble.thresholds)) # 0.4847891
# hist(grand_ensemble[]); points(x = grand.threshold, y=0, pch=24, bg='red')
# grand.treshold.lowersd = grand.threshold - sd(do.call("rbind", ensemble.thresholds))
# grand.threshold.se     = sd(do.call("rbind", ensemble.thresholds))/sqrt(length(do.call("rbind", ensemble.thresholds)))
# # grand.threshold.lower = grand.threshold-grand.threshold.se

# grand.distribution = grand_ensemble
# names(grand.distribution) = "grand.distribution"
# grand.distribution[grand.distribution < grand.threshold] <- NA
# writeRaster(grand.distribution, 
#             filename = paste0(heavies.spatial.path,"grand.distribution.tif"), options=c('TFW=YES'), overwrite=T)
grand.distribution = raster(paste0(heavies.spatial.path,"grand.distribution.tif"))
xlims_b_d = c(34.386,35.339) # distribution limits (smaller than study area)
ylims_b_d = c(30.807,31.603)

greens.pal <- colorRampPalette(c("chartreuse3", "darkgreen"))
# plot(c(100, 200), c(300, 450), type= "n", xlab = "", ylab = "") # These two lines are for testing the colours
# rect(100, 400, 125, 450, col = "chartreuse3", border = "blue")

# Unitone distibution ----
distribution.unitone = grand_ensemble
distribution.unitone[distribution.unitone < grand.threshold] <- NA
distribution.unitone[distribution.unitone >= grand.threshold] <- 1
# writeRaster(distribution.unitone, filename=paste0(heavies.spatial.path,"grand.distribution.unitone"),overwrite=T)
par(mar=c(0,0,0,0), bty="n")
plot(distribution.unitone, col="purple", xlim=xlims_b_d, ylim=ylims_b_d, legend=F)

# Plotting clean grand distribution ----
png(filename = paste0(heavies.image.path,"Grand_distribution.clean2.png"), width=25, height=22, units='cm', res=900)
par(mar=c(1,1,1,1), bty="n")
plot(grand.distribution, xlim= xlims_b_d, ylim=ylims_b_d, bty="n", axes=FALSE, col = greens.pal(20))
dev.off() #need to fix the color palette! Now that I've NA'd the values below the threshold.

# Plotting distribution with observations data ----

png(filename = paste0(heavies.image.path,"Grand_distribution w ref data.png"), width=25, height=22, units='cm', res=900)
par(mar=c(1,1,1,1), bty="n")
plot(grand.distribution, col=greens.pal(20), xlim = xlims_b_d, ylim = ylims_b_d, bty="n", axes=F)
lines(borders, lty=5, lwd=1.5, col="grey15")
lines(groads, col="grey73")
points(bsp,    bg  = 'yellow', pch=22,  cex=0.7)
points(bsa,    bg = 'cyan',    pch=21,  cex=0.7)
points(bsa.r,  bg = 'blue',    pch=21,  cex=0.7)
points(bc,     bg = 'red',   pch=24,  cex=0.7)
points(bc.r.D, bg = 'darkorange',  pch=24,  cex=0.7)
points(major.cities, pch=21, col='black', bg='yellow', cex=1.5)
with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=1, font=2, offset=.3))
legend("bottomright", 
       c("Surveyed presence (75)", "Surveyed absence-questionable (24)","Surveyed absence-reliable (103)",
         "Collections record-unreliable (71)","Collections record-reliable (243)"),
       pch=c(22,21,21,24,24), cex=1, text.font=1,
       pt.bg=c("yellow","cyan","blue","green","green4"))
dev.off()


#######################################################################################################################
# Obsolete attempts to map and calculate landuse impacts ----

landuse = landuse.unsimplified

# not such a useful plot, as landuse isn't limited to distribution
png(filename=paste0(heavies.image.path,"Grand_distribution.w.landuse.png"), width=30, height=22, units='cm', res=600)
par(mar=c(0,0,0,0), bty="n")
plot(grand.distribution, col="blue", xlim = xlims_b_d, ylim = ylims_b_d, axes=FALSE, legend=F)
plot(landuse[landuse@data$landuse == "built-up area",], col="darkgrey",   border="darkgrey", lwd=0.25, add=T)
plot(landuse[landuse@data$landuse == "military",],      col="lightpink",  border=NA, add=T) # here, col is fill.
plot(landuse[landuse@data$landuse == "agriculture",],   col="burlywood",  border=NA, add=T)
plot(landuse[landuse@data$landuse == "forestry",],      col="darkgreen",  border=NA, add=T)
# plot(landuse[landuse@data$landuse == "conservation",],  col=NA,     border="lightgreen", add=T)
# plot(nat.res,      col=NA, border='lightgreen', add=T)
# plot(nat.park,     col=NA, border='green',     add=T)
plot(major.cities, pch=21, bg='yellow', cex=1, add=TRUE)
with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos = 4, cex=0.5, font=2, offset=0.3))
lines(borders, lty=5, lwd=1.5, col="grey15")
lines(groads, col="grey73")
# plot(villages, pch=21, bg='blue', cex=0.9, add=T)
plot(towns,          pch=21, bg='yellow', cex=0.9, add=T)
plot(small.cities,   pch=21, bg='yellow', cex=1.3, add=T)
points(major.cities, pch=21, bg='yellow', cex=1.5)
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=1, text.font=2, offset=0.3))
with(towns, text(towns$lat~towns$lon, labels = towns$name, pos = 4, cex=1, text.font=2))
legend("bottomright", c("Agriculture","Built-up area","Military","Forestry"), pch=22, pt.cex=1.5, 
        col=c(NA,NA,NA,NA),                   # in legend, col is for border
        pt.bg=c("burlywood", "darkgrey", "lightpink", "darkgreen",NA)) # pt.bg is for shape fill
dev.off()

# Subsetting distribution by landuse --

distribution.poly = rasterToPolygons(distribution.unitone, digits=12, na.rm=TRUE, dissolve=T) # takes forever
saveRDS(distribution.poly, paste0(heavies.rds.path,"b_distribution_poly.rds"))
distribution.poly = readRDS(paste0(heavies.rds.path,"b_distribution_poly.rds")) # here's one I made earlier

plot(distribution.poly, col="green")
names(distribution.poly) = "distr_DEMvE"

shapefile(distribution.poly, paste0(heavies.spatial.path,"b.distr.poly.shp"), overwrite=TRUE) # worked. Writes shapefile
# writeOGR(distribution.poly, heavies.spatial.path, driver = "ESRI Shapefile", layer = "b_distr.poly3.shp") # error

distribution.sf = st_as_sf(distribution.poly) # converting to sf object format
builtup.area    = st_as_sf(landuse[landuse@data$landuse == "built-up area",])
plot(builtup.area, col = sf.colors(categorical = TRUE, alpha = .5), add=T)

plot(distribution.poly, col="purple")
disturbing_landuse = landuse[landuse@data$landuse != "conservation",]

# getting in to the right format:
distribution.sf  = st_as_sf(distribution.poly)
disturbing.LU.sf = st_as_sf(disturbing_landuse)

ITM = CRS("+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 
          +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.33077,-1.85269,1.66969,5.4248 +units=m +no_defs")

distribution.itm = spTransform(distribution.sf, ITM)
disturbing.LU.itm = spTransform(disturbing.LU.sf, ITM)

summary(landuse@data$landuse); summary(disturbing_landuse@data$landuse)

# reproject:

distribution.sf.itm  = spTransform(distribution.sf, ITM)
disturbing.LU.sf.itm = spTransform(disturbing.LU.sf, ITM)

# extract undisturbed parts of distribution:
dist.undisturbed = st_difference(distribution.sf.itm, disturbing.LU.sf.itm)
plot(undisturbed.dist, col="green", border=NA, add=T)

# Plotting remaining distribution ---
builtup.area.sf = st_as_sf(landuse[landuse@data$landuse == "built-up area",])
plot(builtup.area.sf, col = sf.colors(categorical = TRUE, alpha = .5), add=T)

st_intersection(distribution.sf, builtup.area)

# convert all to ITM
# don't think this worked...


distribution.unitone 
distribution.poly = rasterToPolygons(distribution.unitone, digits=12, na.rm=TRUE, dissolve=T)
plot(distribution.poly, col="green", border=NA)
summary(distribution.poly)


ITM = CRS("+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.33077,-1.85269,1.66969,5.4248 +units=m +no_defs")

spTransform(distribution.poly, ITM)

writeOGR(distribution.poly, heavies.spatial.path, driver = "ESRI Shapefile", layer = "distribution.poly.shp")

# st_read(distribution.poly) #error
# st_difference(distribution.poly, disturbed.raw.layers[[1]]) # error

# calculating disturbance by landuse types 
disturbed.raw.layers[[1]]@data$landuse #
disturbed.raw.layers[[2]]@data$landuse #
disturbed.raw.layers[[3]]@data$landuse #
disturbed.raw.layers[[4]]@data$landuse #
disturbed.raw.layers[[5]]@data$landuse #
disturbed.raw.layers[[6]]@data$landuse #
disturbed.raw.layers[[7]]@data$landuse #
disturbed.raw.layers[[8]]@data$landuse #
disturbed.raw.layers[[9]]@data$landuse #

{
all.ag = aggregate(
  for (i in 1:length(disturbed.raw.layers)){
  disturbed.raw.layers[[i]][disturbed.raw.layers[[i]]@data$landuse == "agriculture",] } , fun=NULL)

all.built = aggregate(
   for (i in 1:length(disturbed.raw.layers)){
  disturbed.raw.layers[[i]][disturbed.raw.layers[[i]]@data$landuse == "built-up areas and industry",] } )

all.plant = aggregate(
   for (i in 1:length(disturbed.raw.layers)){
  disturbed.raw.layers[[i]][disturbed.raw.layers[[i]]@data$landuse == "plantation",] } )

all.military = aggregate(
   for (i in 1:length(disturbed.raw.layers)){
  disturbed.raw.layers[[i]][disturbed.raw.layers[[i]]@data$landuse == "military",] } )
} # didn't work

# military_overlap = st_difference(distribution.poly, all.military) # didn't work



# Mapping and calculating landuse impacts ----

# I couldn't, at the time, find a straightforward way to do this in R. So exported distribution polygon and cropped there.
distribution.poly = rasterToPolygons(distribution.unitone, digits=12, na.rm=TRUE, dissolve=T) # takes forever
names(distribution.poly) = "b.distr.DEMvE"
saveRDS(distribution.poly, paste0(heavies.rds.path,"b_distribution_poly.rds"))
distribution.poly = readRDS(paste0(heavies.rds.path,"b_distribution_poly.rds")) # here's one I made earlier

plot(distribution.poly, col="green")

shapefile(distribution.poly, paste0(heavies.spatial.path,"b.distr.poly.shp"), overwrite=TRUE) # worked. Writes shapefile
# writeOGR(distribution.poly, heavies.spatial.path, driver = "ESRI Shapefile", layer = "b_distr.poly3.shp") # error

# Now go to ArcGIS to crop.

# Import rasters of distribution clipped by landuse types from ArcGIS:
# then get area:
distr_ag       = raster(paste0(heavies.spatial.path, "g_distr_ag.tif"))
distr_military = raster(paste0(heavies.spatial.path, "g_distr_milit.tif"))
distr_built    = raster(paste0(heavies.spatial.path, "g_distr_built.tif"))
distr_plnt     = raster(paste0(heavies.spatial.path, "g_distr_plnt.tif"))
distr_kklplan  = raster(paste0(heavies.spatial.path, "g_distr_kklplan.tif"))
distr_natr     = raster(paste0(heavies.spatial.path, "g_distr_natr.tif"))
distr_natp     = raster(paste0(heavies.spatial.path, "g_distr_natp.tif"))

png(filename = paste0(heavies.image.path,"Grand_distribution.generous.disturbed.png"), width=30, height=22, units='cm', res=900)
par(mfrow=c(1,1), mar=c(0,0,0,0), bty="n")
plot(grand.distribution.generous, col="green", legend=F, xlim=c(34.3,35.4), ylim=c(30.8,31.6))
plot(distr_ag,       col="brown", add=T, legend=FALSE, axes=F)
plot(distr_military, col="orange", add=T, legend=FALSE, axes=F)
plot(distr_built,    col="darkgrey", add=T, legend=FALSE, axes=F)
plot(distr_plnt,     col="darkgreen", add=T, legend=FALSE, axes=F)
plot(distr_kklplan,  col="pink", add=T, legend=FALSE, axes=F)
plot(distr_natr,     col="lightblue", add=T, legend=FALSE, axes=F)
plot(distr_natp,     col="darkblue", add=T, legend=FALSE, axes=F)

xcoordsmean = mean(34.27161, 35.3942) # 34.27161
ycoordsmean = mean(30.6072, 31.72065) # 30.6072

# length of degrees from here: http://www.csgnetwork.com/degreelenllavcalc.html
# area = ncell * cell.prop.of.degree * latdegreelength * longdegreelength:
longdegree.km  = 95893.45308334891/1000
latdegree.km   = 110862.72016164708/1000
cell.dim       = 0.0002950308

dist.size     = (length(grand.distribution[][!is.na(grand.distribution[])]) * cell.dim^2 * latdegree.km*longdegree.km)
ag.size       = (length(distr_ag[][!is.na(distr_ag[])]) * cell.dim^2 * latdegree.km*longdegree.km)
military.size = (length(distr_military[][!is.na(distr_military[])]) * cell.dim^2 * latdegree.km*longdegree.km)
built.size    = (length(distr_built[][!is.na(distr_built[])]) * cell.dim^2 * latdegree.km*longdegree.km)
plnt.size     = (length(distr_plnt[][!is.na(distr_plnt[])]) * cell.dim^2 * latdegree.km*longdegree.km)

ag.size      /dist.size # 28%
military.size/dist.size # 10%
plnt.size    /dist.size # 3% 
built.size   /dist.size # 17%
(ag.size + military.size + plnt.size + built.size) / dist.size # i.e. 58% of the historical habitat is disturbed
3/32 # plantations are 10(-15%)% of the remaining habitat.

#######################################################################################################################