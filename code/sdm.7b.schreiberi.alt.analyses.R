# Alternative version of the schreiberi sdm analysis: removing veg type from predictor variables (consolidated script) ----
# This script includes code from scripts 2, 2a, 3, 4, 5 & 6

########################################################################################################################
# Housekeeping ----

# load packages:
{
x<-c("sdm","usdm","raster","rgdal","tidyverse","png","beepr","xlsx","mailR","parallel")
lapply(x, require, character.only = TRUE)
installAll() # installs all the packages that 'sdm' relied on, if they aren't already installed.
}

# make functions:
emailme = function() {
send.mail(from="kerengila@gmail.com",       to="keren.raiter@mail.huji.ac.il",
          subject="the loop is complete",   body="yeah yeah yeah",        html=T,
          smtp=list(host.name = "smtp.gmail.com",          port = 465,
                    user.name = "kerengila@gmail.com",     passwd = "Ivrit333",
                    ssl = T), authenticate=T) #, attach.files="C:\\Users\\Deepanshu\\Nature.xls"  
}

plot.withreference.data = function(distribution) {
  par(mar=c(1,1,1,1), bty="n")
  plot(distribution, col="green", xlim=c(34.27173,35.3248), ylim=c(31.12667,33.10742), bty="n", axes=F,legend=F)
  lines(borders, lwd=1, col="seashell4")
  plot(schreiberi.buffer, lwd=0.5, add=T)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
  points(ssp,  col='purple',       pch=16, cex=0.3) 
  points(ssa,  col='pink',         pch=16, cex=0.3) 
  points(sc,   col='deepskyblue2', pch=16, cex=0.3)
  points(sc.r, col='darkblue',     pch=16, cex=0.3) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)"), 
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"),pt.cex = 0.4, cex=1)
  png(filename = paste0(heavies.image.path,"S ", deparse(substitute(distribution)), ".png"), 
      width=16, height=22, units='cm',res=900)
  par(mar=c(1,1,1,1), bty="n")
  plot(distribution, col="green", xlim=c(34.27173,35.3248), ylim=c(31.12667,33.10742), bty="n", axes=F,legend=F)
  lines(borders, lwd=1, col="seashell4")
  plot(schreiberi.buffer, lwd=0.5, add=T)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
  points(ssp,  col='purple',       pch=16, cex=0.3) 
  points(ssa,  col='pink',         pch=16, cex=0.3) 
  points(sc,   col='deepskyblue2', pch=16, cex=0.3)
  points(sc.r, col='darkblue',     pch=16, cex=0.3) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)"), 
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"),pt.cex = 0.4, cex=1)
  dev.off()
}

# List objects by size:
{
.ls.objects <- function (pos = 1, pattern, order.by, decreasing=FALSE, head=FALSE, n=5) {
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
  out                                                                                  }
lsos <- function(..., n=10) {.ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)}}
lsos()

# load data:
heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

s.raster.list.names = list("Rain", "Jant", "Jult", "TWet", "Slop", "Soil", "Vegt")
s.raster.list       = readRDS(paste0(heavies.rds.path,"s.raster.list.rds"))
s.preds             = readRDS(paste0(heavies.rds.path,"s.preds.rds")) # raster stack

s.6scen.scen.names             = readRDS("./rds_objects/s.6scen.scen.names.rds")
s.6scenario.descriptions       = readRDS("./rds_objects/s.6scenario.descriptions.rds")
# s.6scen.data.packages          = readRDS("./rds_objects/s.6scen.data.packages.rds")

schreiberi.buffer              = readRDS("./rds_objects/schreiberi.buffer.rds")
plot(schreiberi.buffer, xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742))

# observations versions with full data :
ss.full    = readRDS("./rds_objects/ss.full.rds")   # all schreiberi survey data (errors removed)
ssp.full   = readRDS("./rds_objects/ssp.full.rds")  # presences
ssa.full   = readRDS("./rds_objects/ssa.full.rds")  # absences
sc.full    = readRDS("./rds_objects/sc.full.rds")   # s = schreiberi, c = collections
sc.nodups  = readRDS(paste0(heavies.rds.path, "sc.nodups.rds")) # from script 2b; elsa calc. Duplicates removed.
sc.r.full  = readRDS(paste0(heavies.rds.path, "s.collections.reliables.rds"))  # from script 2b: elsa calc

# consolidated observations, with all extraneous attributes removed:
ssp  = readRDS ("./rds_objects/ssp.rds")
ssa  = readRDS ("./rds_objects/ssa.rds")
sc   = readRDS ("./rds_objects/sc.rds")
sc.r = readRDS ("./rds_objects/sc.r.rds")

s.eval.summary    = readRDS("./rds_objects/s.eval.summary.topmethods.rds")    # consolidated list form

# spatial data:
borders                = readRDS("./rds_objects/borders.rds")
major.cities           = readRDS("./rds_objects/major.cities.rds")
small.cities           = readRDS("./rds_objects/small.cities.rds")
towns                  = readRDS("./rds_objects/towns.rds")
villages               = readRDS("./rds_objects/villages.rds")
groads                 = readRDS("./rds_objects/groads.rds")

nat.res                = readRDS(paste0(heavies.rds.path,"nat.res.rds"))
nat.park               = readRDS(paste0(heavies.rds.path,"nat.park.rds")) 
plot(nat.res, col="darkgreen",  border="darkgreen"); plot(nat.park, col="lightgreen",border="lightgreen", add=T)
kkl.mgmt               = readRDS(paste0(heavies.rds.path,"kkl.mgmt.rds"))
kkl.plan               = readRDS(paste0(heavies.rds.path,"kkl.plan.rds"))
kkl.ops                = readRDS(paste0(heavies.rds.path,"kkl.ops.rds"))

#landuse   = readRDS(paste0(heavies.rds.path,"landuse.rds"))
landuse_s = readRDS(paste0(heavies.rds.path,"landuse_s.rds"))

########################################################################################################################
# New saved objects ----
s.preds.noveg      = readRDS(paste0(heavies.rds.path,"s.preds.noveg.rds"))
s.data.packs.noveg = readRDS(paste0(heavies.rds.path,"s.data.packs.noveg.rds"))

s.eval.list.noveg       = readRDS("./rds_objects/s.eval.list.noveg.rds")   # 'raw' list of full eval data from each combo
s.eval.summary.noveg    = readRDS("./rds_objects/s.eval.summary.noveg.rds")  # consolidated list (multi reps averaged)
s.eval.summary.df.noveg = readRDS("./rds_objects/s.eval.summary.df.novegs.rds") # consolidated table (all combos together)

########################################################################################################################
# Setting up alternative predictor variables ----

s.preds ; plot(s.preds)
s.preds.noveg = stack(s.preds[[1]], s.preds[[2]], s.preds[[3]], s.preds[[4]], s.preds[[5]], s.preds[[6]]); plot(s.preds.noveg)

vif(s.preds)
vif(s.preds.noveg) 
source("./code/HighstatLibV8.R")
corvif(s.preds.noveg)

saveRDS(s.preds.noveg, paste0(heavies.rds.path, "s.preds.noveg.rds"))

########################################################################################################################
# Create data packages ----

s.2scen.scen.names       = list('Schreiberi Scenario A','Schreiberi Scenario B')
s.2scen.descriptions     = list("A: All data", "B: Exclude unreliable collections data")

obs_A = rbind(ssp, ssa, sc)
obs_B = rbind(ssp, ssa, sc.r)

s.2scen.obs.packages  = list(obs_A, obs_B)

s.noveg.data.packages = list()

for (d in 1:length(s.2scen.scen.names)) {
  s.noveg.data.packages[[d]] = sdmData(formula = occurrence ~ ., train = s.2scen.obs.packages[[d]], 
                                       predictors=s.preds.noveg)
  print("------------"); print(s.2scen.scen.names[[d]]); print(s.noveg.data.packages[[d]])   
  print(length(s.2scen.obs.packages[[d]]))   } 
  # 20 points dropped in each: must be in ssp. Maybe not identical, but fall in the same cells. 

saveRDS(s.noveg.data.packages, paste0(heavies.rds.path,"s.noveg.data.packages.rds"))

########################################################################################################################
# Create evaluation models ----

sdm.cv = function(data) {
  sdm(occurrence ~ ., data = data, methods =c('rf','gam','svm'),  # took out non-top algorithms
      n=100, replication = 'cv', cv.folds=5) }

# create models for each combo 
s.eval.models.noveg = list()                                          

for (p in 1:length(s.noveg.data.packages))  {         # should take ~400 minutes, i.e. 6.6 hours
  start.time = Sys.time()
  data = s.noveg.data.packages[[p]]
  s.eval.models.noveg[[p]] = sdm.cv(data)                       
  print(paste(s.2scen.scen.names[p],"loop took", difftime(Sys.time(), start.time, units="mins"), "minutes")) }
  
saveRDS(s.eval.models.noveg, paste0(heavies.rds.path,"s.eval.models.noveg.rds")) 
emailme()

########################################################################################################################
# Evaluate models ----

eval.list    = list()    # evaluation data, by model (i.e. 4500 models; 100reps x 5 folds x 9 algorithims, for each of 9 datacombos)
eval.summary = list() # list of evaluation data, averaged by algorithim (i.e. 9 dataframes of 9 rows: 9 data combos X 9 algorithms)

modelset = s.eval.models.noveg # to prevent multiple repetitions of exact model set name below:
for (i in 1:length(modelset))                                                  {
  model.inf.ev = NULL
  # extract model info and eval statistics and merge them (change name of model list as appropriate):
  model_info = getModelInfo (modelset[[i]])
  model_eval = getEvaluation(modelset[[i]], wtest= 'test.dep', 
                             stat=c('TSS','Kappa','AUC','COR','Deviance','obs.prevalence','threshold'), opt=2)
  model.inf.ev = merge(model_info, model_eval, by = "modelID") # 'model.inf.ev' - a temporary vector for use in next bit.
  # then put them into list and aggregate them
  eval.list[[i]]   = model.inf.ev 
  eval.summary[[i]]= data.frame(method=aggregate(model.inf.ev$method, by=list(model.inf.ev$method),FUN=mode)[1],
                                data_combo = s.6scen.scen.names[[i]],          # keep this line relevant!
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
  names(eval.summary[[i]])=c('method','data_combo','TSS','TSS_sd','AUC','AUC_sd','kappa','COR','deviance',
                             'obs.prevalence','threshold.mss','total_models')   }

eval.summary
eval.summary.df = do.call("rbind", eval.summary) # converting the list to a single dataframe for easy plotting

# Schreiberi: comparing before and after reducing study area size and absence data (worse TSS, to be expected):
s.eval.summary = readRDS("./rds_objects/s.eval.summary.topmethods.rds")       # consolidated: multiple reps averaged
options(digits=3)
eval.summary[[1]]$TSS             # 0.733 0.650 0.547 new ones worse
s.eval.summary[[1]]$TSS           # 0.776 0.678 0.626 old
eval.summary[[2]]$TSS             # 0.746 0.672 0.566 new ones worse
s.eval.summary[[2]]$TSS           # 0.784 0.701 0.648 old. 
# OK model performance has definitely dropped with the removal of vegt. But perhaps for the best?
eval.summary[[1]]$AUC             # 0.733 0.650 0.547 new ones worse
s.eval.summary[[1]]$AUC           # 0.776 0.678 0.626 old
eval.summary[[2]]$AUC             # 0.746 0.672 0.566 new ones worse
s.eval.summary[[2]]$AUC           # 0.784 0.701 0.648 old. 
# AUC values are still high.

# saving the outputs
saveRDS(eval.list,       "./rds_objects/s.eval.list.noveg.rds")       # 'raw' list of eval data from each model
saveRDS(eval.summary,    "./rds_objects/s.eval.summary.noveg.rds")    # consolidated: multiple reps averaged
saveRDS(eval.summary.df, "./rds_objects/s.eval.summary.df.noveg.rds") # superconsolidate:all scenarios, 1 table)
write.xlsx(eval.summary,    "./output_data/s.model evaluation summary (noveg).xlsx") # export to excel sprdsheet
write.xlsx(eval.summary.df, "./output_data/s.model evaluation summary onetable (noveg).xlsx")

# Plot relationship between TSS and AUC
AUC_ordered = ordered(eval.summary.df$AUC)
TSS_lower = eval.summary.df$TSS - eval.summary.df$TSS_sd
TSS_upper = eval.summary.df$TSS + eval.summary.df$TSS_sd

r = ggplot(eval.summary.df, aes(x=AUC, y=TSS, color = data_combo)) + geom_point() 
r
cor(eval.summary.df$TSS, eval.summary.df$AUC) # extremely high correlation: 0.995

########################################################################################################################
# Create complete models ----
s.models.complete.noveg = list()
scenario.names = s.2scen.scen.names

for (i in 1:length(scenario.names))                                                                        {
  start.time = Sys.time()
  print(scenario.names[i])
  data = s.noveg.data.packages[[i]]
  s.models.complete.noveg[[i]] = sdm(occurrence ~ ., data = data, 
                                         methods =c('rf','gam','svm')) #"cart",'fda','gam','brt','rf','glm', 'mda','rpart','svm'
  print(paste(scenario.names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes"))}

# Predict the model outputs: Schreiberi ----

modelset       = s.models.complete.noveg

s.predmaps.rf    = list()
s.predmaps.svm   = list()
s.predmaps.gam   = list()

for (i in 1:length(scenario.names))                                                           { # started ~ 7:50
  start.time       = Sys.time()
  par(mar=c(2,2,2,1))

  tifname.rf  = paste0(heavies.spatial.path, 'Prediction ', scenario.names[[i]], ' - RF noveg.tif')
  s.predmaps.rf[[i]] = predict(modelset[[i]], newdata = s.preds.noveg, filename = tifname.rf, 
                             format="GTiff", overwrite = TRUE, w = 1, nc = 20)
  pngname.rf  = paste0(heavies.spatial.path, 'Prediction ', scenario.names[[i]], ' - RF noveg.png')
  png(filename = pngname.rf, width=16, height=22, units='cm', res=600); par(mar=c(1,1,1,1), bty="n")
  plot(s.predmaps.rf[[i]], main=paste(scenario.names[[i]],"RF, TSS =", eval.summary[[i]][1,"TSS"]))
  dev.off
  
  tifname.gam = paste0(heavies.spatial.path, 'Prediction ', scenario.names[[i]], ' - GAM noveg.tif')
  s.predmaps.gam[[i]]= predict(modelset[[i]], newdata = s.preds.noveg, filename = tifname.gam, 
                             format="GTiff", overwrite = TRUE, w = 2, nc = 20)
  pngname.gam  = paste0(heavies.spatial.path, 'Prediction ', scenario.names[[i]], ' - GAM noveg.png')
  png(filename = pngname.gam, width=16, height=22, units='cm', res=600); par(mar=c(1,1,1,1), bty="n")
  plot(s.predmaps.gam[[i]], main=paste(scenario.names[[i]],"GAM, TSS =", eval.summary[[i]][2,"TSS"]))
  dev.off
  
  tifname.svm = paste0(heavies.spatial.path, 'Prediction ', scenario.names[[i]], ' - SVM noveg.tif')
  s.predmaps.svm[[i]]= predict(modelset[[i]], newdata = s.preds.noveg, filename = tifname.svm, 
                             format="GTiff", overwrite = TRUE, w = 3, nc = 20)
  pngname.svm  = paste0(heavies.spatial.path, 'Prediction ', scenario.names[[i]], ' - SVM noveg.png')
  png(filename = pngname.svm, width=16, height=22, units='cm', res=600); par(mar=c(1,1,1,1), bty="n")
  plot(s.predmaps.svm[[i]], main=paste(scenario.names[[i]],"SVM, TSS =", eval.summary[[i]][3,"TSS"]))
  dev.off
  
  print(paste(scenario.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

# how to view model info:
# summary(predmaps.gam[[i]])

emailme() # send an email when the loop is complete:

########################################################################################################################
# Map predicted occurrence with threshold that maximises sensitivity plus specificity (per scenario & method) ----

s.occmaps.rf    =list()
s.occmaps.svm   =list()
s.occmaps.gam   =list()

methods = list("RF","GAM", "SVM")

for (i in 1:length(scenario.names))      {
  start.time = Sys.time()
  
  s.threshold.rf     = eval.summary[[i]][1, "threshold.mss"] # 5 instead of 2
  s.threshold.svm    = eval.summary[[i]][2, "threshold.mss"] # 9 instead of 3
  s.threshold.gam    = eval.summary[[i]][3, "threshold.mss"] # 3 instead of 1
  
  s.occmaps.rf[[i]]  = s.predmaps.rf[[i]]  ; s.occmaps.rf[[i]] [s.occmaps.rf[[i]]  < s.threshold.rf]   <- 0
  s.occmaps.svm[[i]] = s.predmaps.svm[[i]] ; s.occmaps.svm[[i]][s.occmaps.svm[[i]] < s.threshold.svm]  <- 0
  s.occmaps.gam[[i]] = s.predmaps.gam[[i]] ; s.occmaps.gam[[i]][s.occmaps.gam[[i]] < s.threshold.gam]  <- 0
  # s.occmaps.rf[[i]] [s.occmaps.rf[[i]]  >= s.threshold.rf]  <- 1 # leaving this step out.
  # s.occmaps.svm[[i]][s.occmaps.svm[[i]] >= s.threshold.svm] <- 1 # leaving this step out.
  # s.occmaps.gam[[i]][s.occmaps.gam[[i]] >= s.threshold.gam] <- 1 # leaving this step out.
  
  for (m in 1:length(methods))            {
  
  # histogram section:
  histname = paste0(heavies.image.path,'Prediction histogram ', scenario.names[[i]],' - ', methods[[m]],' noveg.png')
  png(filename = histname, width = 14, height = 10, units = 'cm', res = 300)
  title     = paste0("Prediction histogram ", scenario.names[[i]], ' - ',methods[[m]])
  predmap   = eval(parse(text = paste0('s.predmaps.', tolower(methods[[m]]))))
  threshold = eval(parse(text = paste0('s.threshold.',tolower(methods[[m]]))))
  hist(predmap[[i]], main = title, xlab="Probability of occurrence for each map cell")  
  points(x = threshold,  y=0, pch=24, bg='red')
  dev.off()                               
  
  # distribution map image section
  pngname  = paste0(heavies.image.path,'Predicted distribution ', scenario.names[[i]],' - ',methods[[m]],' noveg.png')
  png(filename = pngname, width = 14, height = 25, units = 'cm', res = 400) 
  par(mar = c(2,2,0,0), mgp=c(2,0.5,0)) # , bty="n"
  occmap   = eval(parse(text = paste0('s.occmaps.', tolower(methods[[m]]))))
  plot(occmap[[i]], xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742),legend=F) 
  plot(schreiberi.buffer, lwd=0.5, add=T)
  # can also plot like this plot(s.predmaps.rf[[i]], col = c('white','green'),breaks=c(0,s.threshold.rf,1))
  points(ssp.full,  col='purple',       pch=16, cex=0.4) 
  points(ssa.full,  col='pink',         pch=16, cex=0.4) 
  points(sc.nodups, col='deepskyblue2', pch=16, cex=0.4)
  points(sc.r,      col='darkblue',     pch=16, cex=0.4) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)"), 
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"), cex=1)
  dev.off()
  
  # distribution raster-making section
  tifname  = paste0(heavies.spatial.path,'Predicted distribution ', scenario.names[[i]],' - ',methods[[m]],' noveg.tif')
  writeRaster(occmap[[i]], filename = tifname, options=c('TFW=YES'), overwrite= TRUE)
  }
  
  print(paste(scenario.names[[i]],"loop took", difftime(Sys.time(),start.time, units = "mins"), "minutes"))}

# Combine plots to make one composite image  ----
plot(s.occmaps.rf[[i]], frame.plot=FALSE) # checking coordinate ranges for text placement

png(filename=paste0(heavies.image.path,"S predicted distributions - by scenario and algorithm noveg new.png"),
                    width = 20, height = 28, units = "cm", res = 600)
par(mfrow=c(2,3), mar = c(0,0,1.5,.5), mgp=c(0,0,0), bty="n")
for (i in 1:length(scenario.names))                           {
  plot(s.occmaps.rf[[i]], axes=FALSE, ann=FALSE, legend=F)
  text(x = 34.4, y = 33, labels = "RF",cex = 1.3, xpd = NA)
  plot(s.occmaps.gam[[i]], axes=FALSE, main = s.6scenario.descriptions[[i]], legend=F, cex.main=1)
  text(x = 34.4, y = 33, labels = "GAM",cex = 1.3, xpd = NA)
  plot(s.occmaps.svm[[i]], axes=FALSE, ann=FALSE, legend=F)
  text(x = 34.4, y = 33, labels = "SVM",cex = 1.3, xpd = NA)
    }
dev.off()

# Now over to 'run ensemble loop' in sdm.6b
