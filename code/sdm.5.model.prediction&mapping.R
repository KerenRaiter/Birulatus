# sdm.5.model.prediction and mapping ----

########################################################################################################
# Housekeeping ----

code.start.time = Sys.time()

# clear user interface and free memory:
if(!is.null(dev.list())) dev.off() # Clear plots
cat("\014")                        # Clear console
rm(list=ls())                      # Clean workspace
gc()

# load packages:
x<-c("sdm","usdm","raster","rgdal","tidyverse","png","beepr","xlsx","mailR","parallel")
lapply(x, require, character.only = TRUE)
installAll() # installs all the packages that 'sdm' relied on, if they aren't already installed.

emailme = function() {
send.mail(from="kerengila@gmail.com",       to="keren.raiter@mail.huji.ac.il",
          subject="the loop is complete",   body="yeah yeah yeah",        html=T,
          smtp=list(host.name = "smtp.gmail.com",          port = 465,
                    user.name = "kerengila@gmail.com",     passwd = "Ivrit333",
                    ssl = T), authenticate=T) #, attach.files="C:\\Users\\Deepanshu\\Nature.xls"  
}

# load data

heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

# reference information:
borders       = readRDS("./rds_objects/borders.rds")  
major.cities  = readRDS("./rds_objects/major.cities.rds")
small.cities  = readRDS("./rds_objects/small.cities.rds")
towns         = readRDS("./rds_objects/towns.rds")
villages      = readRDS("./rds_objects/villages.rds")
groads        = readRDS("./rds_objects/groads.rds")

# observational and prediction datasets
b_package_names    = readRDS("./rds_objects/b_package_names.rds")
b_data_packages    = readRDS("./rds_objects/b_data_packages.rds")
b.preds            = readRDS("//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/b.preds.rds"); plot(b.preds)
b.combo.descriptions = readRDS("./rds_objects/b.combo.descriptions.rds")

s.raster.list.names = list("Rain", "Jant", "Jult", "TWet", "Slop", "Soil", "Vegt")
# s.raster.list       = readRDS(paste0(heavies.rds.path,"s.raster.list.rds"))
s.preds             = readRDS(paste0(heavies.rds.path,"s.preds.rds")) # raster stack

s.6scen.scen.names             = readRDS("./rds_objects/s.6scen.scen.names.rds")
s.6scenario.descriptions       = readRDS("./rds_objects/s.6scenario.descriptions.rds")
# s.6scen.obs.packages           = readRDS("./rds_objects/s.6scen.obs.packages.rds")
s.6scen.data.packages          = readRDS("./rds_objects/s.6scen.data.packages.rds")

beershebensis.buffer = readRDS("./rds_objects/beershebensis.buffer_incWB.rds"); 
plot(beershebensis.buffer, xlim=c(34.267,35.39774), ylim=c(30.50798,31.72056))
schreiberi.buffer    = readRDS("./rds_objects/schreiberi.buffer.rds"); plot(schreiberi.buffer)
plot(schreiberi.buffer, xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742))

bs.full  = readRDS("./rds_objects/bs.full.rds")    # b = beershebensis, s = surveys
bsp.full = readRDS("./rds_objects/bsp.full.rds")  # presences
bsa.full = readRDS("./rds_objects/bsa.full.rds")  # absences
bc.full  = readRDS("./rds_objects/bc.full.rds")   # b = beershebensis, c = collections

ss.full    = readRDS("./rds_objects/ss.full.rds")   # all schreiberi survey data (errors removed)
ssp.full   = readRDS("./rds_objects/ssp.full.rds")  # presences
ssa.full   = readRDS("./rds_objects/ssa.full.rds")  # absences
sc.full    = readRDS("./rds_objects/sc.full.rds")   # s = schreiberi, c = collections
sc.nodups  = readRDS(paste0(heavies.rds.path,"sc.nodups.rds"))                # from sript 2a; elsa calc
sc.r       = readRDS(paste0(heavies.rds.path,"s.collections.reliables.rds"))  # from script 2b: elsa calc

# eval.summary = readRDS("./rds_objects/b.eval.summary.5fold.100reps.rds")  

# List objects by size
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

########################################################################################################
# Script outputs ----

b.model.list.complete  = readRDS(paste0(heavies.rds.path,"b.model.list.complete.rds")) 
s.model.list.complete.2scen  = readRDS(paste0(heavies.rds.path,"s.model.list.complete.2scen.rds")) 

# To retreive prediction and predicted occurrence rasters:

# {
# s.predmaps.rf    = list()
# s.predmaps.svm   = list()
# s.predmaps.gam   = list()

# for(i in 1:2) {     # or do to length of s scenario names if applicable
#  
#   filename.rf  = paste0(heavies.spatial.path, 'Prediction ', s.6scen.scen.names[[i]], ' - RF.tif')
#   s.predmaps.rf[[i]] = raster(filename.rf)
#   plot(predmaps.rf[[i]], main=paste(s.6scen.scen.names[[i]],"RF, TSS =", s.eval.summary[[i]][5,"TSS"]))
#   
#   filename.svm = paste0(heavies.spatial.path, 'Prediction ', s.6scen.scen.names[[i]], ' - SVM.tif')
#   predmaps.svm[[i]] = raster(filename.svm)
#   plot(predmaps.svm[[i]], main=paste(s.6scen.scen.names[[i]],"SVM, TSS =", s.eval.summary[[i]][9,"TSS"]))
#   
#   filename.gam = paste0(heavies.spatial.path, 'Prediction ', s.6scen.scen.names[[i]], ' - GAM.tif')
#   predmaps.gam[[i]] = raster(filename.gam)
#   plot(predmaps.gam[[i]], main=paste(s.6scen.scen.names[[i]],"GAM, TSS =", s.eval.summary[[i]][3,"TSS"])) }

########################################################################################################
# Complete models for each data combo-algorith combination: Beershebensis  ----

# create models for each combo
b.model.list.complete = list() # number two is with DEM included.

for (i in 1:length(b_data_packages))                                                                        {
  start.time = Sys.time()
  print(b_package_names[i])
  data = b_data_packages[[i]]
  b.model.list.complete[[i]] = sdm(occurrence ~ ., data = data, methods =c("cart",'fda','gam','brt','rf','glm', 'mda','rpart','svm'))
  print(paste(b_package_names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes"))}

# # see how they went:
# b_package_names[[1]]; b.model.list.complete[[1]]
# b_package_names[[2]]; b.model.list.complete[[2]]
# b_package_names[[3]]; b.model.list.complete[[3]]
# b_package_names[[4]]; b.model.list.complete[[4]]
# b_package_names[[5]]; b.model.list.complete[[5]]
# b_package_names[[6]]; b.model.list.complete[[6]]
# b_package_names[[7]]; b.model.list.complete[[7]]
# b_package_names[[8]]; b.model.list.complete[[8]]
# b_package_names[[9]]; b.model.list.complete[[9]]
# b.model.list.complete[[i]][[5]]

# saveRDS(b.model.list.complete, paste0(heavies.rds.path,"b.model.list.complete.rds"))  

########################################################################################################
# Retreive evaluation summary data for reference: Beershebensis ----
eval.list       = readRDS("./rds_objects/eval.list.5fold.100reps.rds")  # 'raw' list of full eval data from each combo
eval.summary    = readRDS("./rds_objects/eval.summary.5fold.100reps.rds")    # consolidated list form
eval.summary.df = do.call("rbind", eval.summary) # converting the list to a single dataframe for easy plotting

########################################################################################################
# Predict the model outputs: Beershebensis ----

predmaps.rf    = list()
predmaps.svm   = list()
predmaps.gam   = list()

for (i in 1:length(b_package_names))                                                           {
  start.time       = Sys.time()
  par(mar=c(2,2,2,1))

  filename.rf  = paste0(heavies.spatial.path, 'Prediction ', b_package_names[[i]], ' - RF.tif')
  predmaps.rf[[i]] = predict(b.model.list.complete[[i]], newdata=b.preds, filename = filename.rf, 
                             format="GTiff", overwrite=TRUE, w=5, nc=20)
  plot(predmaps.rf[[i]], main=paste(b_package_names[[i]],"RF, TSS =", eval.summary[[i]][5,"TSS"]))
  
  filename.svm = paste0(heavies.spatial.path, 'Prediction ', b_package_names[[i]], ' - SVM.tif')
  predmaps.svm[[i]]= predict(b.model.list.complete[[i]], newdata=b.preds, filename = filename.svm, 
                             format="GTiff", overwrite=TRUE, w=9, nc=20)
  plot(predmaps.svm[[i]], main=paste(b_package_names[[i]],"SVM, TSS =", eval.summary[[i]][9,"TSS"]))
  
  filename.gam = paste0(heavies.spatial.path, 'Prediction ', b_package_names[[i]], ' - GAM.tif')
  predmaps.gam[[i]]= predict(b.model.list.complete[[i]], newdata=b.preds, filename = filename.gam, 
                             format="GTiff", overwrite=TRUE, w=3, nc=20)
  plot(predmaps.gam[[i]], main=paste(b_package_names[[i]],"GAM, TSS =", eval.summary[[i]][3,"TSS"]))

  print(paste(b_package_names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

# how to view model info:
# summary(predmaps.gam[[i]])

emailme() # send an email when the loop is complete:

########################################################################################################
# Retreive predicted outcomes: Beershebensis (only if previous section not run) ----
# 
# eval.summary = readRDS("./rds_objects/eval.summary.5fold.100reps.rds")  
# 
# predmaps.rf    = list()
# predmaps.svm   = list()
# predmaps.gam   = list()

# # load from rasters, plot, and save full images 
{
# for(i in 1:length(b_package_names))                                                                       {
#  
#   filename.rf      = paste('./output_images/Prediction', b_package_names[[i]], '- RF.tif')
#   predmaps.rf[[i]] = raster(filename.rf)
#   plot(predmaps.rf[[i]], main=paste(b_package_names[[i]],"RF, TSS =", eval.summary[[i]][5,"TSS"]))
#   
#   filename.svm      = paste('./output_images/Prediction', b_package_names[[i]], '- SVM.tif')
#   predmaps.svm[[i]] = raster(filename.svm)
#   plot(predmaps.svm[[i]], main=paste(b_package_names[[i]],"SVM, TSS =", eval.summary[[i]][9,"TSS"]))
#   
#   filename.gam     = paste('./output_images/Prediction', b_package_names[[i]], '- GAM.tif')
#   predmaps.gam[[i]] = raster(filename.gam)
#   plot(predmaps.gam[[i]], main=paste(b_package_names[[i]],"GAM, TSS =", eval.summary[[i]][3,"TSS"]))   }
# 
# lsos()
# 
# # Save plots as full-colur pictures ----
# for(i in 1:length(b_package_names))                                                                    {
#   
#   filename.rf      = paste('./output_images/Predicted probabilities', b_package_names[[i]], '- RF.png')
#   png(filename     = filename.rf, width = 18, height = 15, units = 'cm', res = 600)
#   plot(predmaps.rf[[i]], main=paste(b_package_names[[i]],"RF, TSS =", eval.summary[[i]][5,"TSS"]))
#   dev.off()
#   
#   filename.svm     = paste('./output_images/Predicted probabilities', b_package_names[[i]], '- SVM.png')
#   png(filename     = filename.svm, width = 18, height = 15, units = 'cm', res = 600)
#   plot(predmaps.svm[[i]], main=paste(b_package_names[[i]],"SVM, TSS =", eval.summary[[i]][9,"TSS"]))
#   dev.off()
#   
#   filename.gam      = paste('./output_images/Predicted probabilities', b_package_names[[i]], '- GAM.png')
#   png(filename      = filename.gam, width = 18, height = 15, units = 'cm', res = 600)
#   plot(predmaps.gam[[i]], main=paste(b_package_names[[i]],"GAM, TSS =", eval.summary[[i]][3,"TSS"])) 
#   dev.off()                                                                                            }
}

########################################################################################################
# Map predicted occurrence with threshold that maximises sensitivity plus specificity: Beershebensis ----

eval.summary  = readRDS("./rds_objects/eval.summary.5fold.100reps.rds") # consolidated list form
b.occmaps.rf  =list()
b.occmaps.svm =list()
b.occmaps.gam =list()

for (i in 1:length(b_package_names))                                                                 {
  start.time = Sys.time()
  
  b.threshold.rf     = eval.summary[[i]][5, "threshold.mss"]
  b.threshold.svm    = eval.summary[[i]][9, "threshold.mss"]
  b.threshold.gam    = eval.summary[[i]][3, "threshold.mss"]
  
  b.occmaps.rf[[i]]  = b.predmaps.rf[[i]]  ; b.occmaps.rf[[i]][b.occmaps.rf[[i]]   < b.threshold.rf]   <- 0
  b.occmaps.svm[[i]] = b.predmaps.svm[[i]] ; b.occmaps.svm[[i]][b.occmaps.svm[[i]] < b.threshold.svm]  <- 0
  b.occmaps.gam[[i]] = b.predmaps.gam[[i]] ; b.occmaps.gam[[i]][b.occmaps.gam[[i]] < b.threshold.gam]  <- 0
  # b.occmaps.rf[[i]][b.occmaps.rf[[i]]   >= b.threshold.rf]  <- 1 # leaving this step out.
  # b.occmaps.svm[[i]][b.occmaps.svm[[i]] >= b.threshold.svm] <- 1 # leaving this step out.
  # b.occmaps.gam[[i]][b.occmaps.gam[[i]] >= b.threshold.gam] <- 1 # leaving this step out.
 
  hist(b.predmaps.rf[[i]]);  points(x=b.threshold.rf, y=0, pch=24, bg='red')
  hist(b.predmaps.svm[[i]]); points(x=b.threshold.svm, y=0, pch=24, bg='red')
  hist(b.predmaps.gam[[i]]); points(x=b.threshold.gam, y=0, pch=24, bg='red')
  
  pngname.rf   = paste0(heavies.image.path,'Predicted distribution ', b_package_names[[i]], ' - RF.png')
  png(filename = pngname.rf, width = 18, height = 15, units = 'cm', res = 600)
  plot(b.occmaps.rf[[i]]) # can also plot like this plot(b.predmaps.rf[[i]], col = c('white','green'),breaks=c(0,b.threshold.rf,1))
  dev.off()
  tifname.rf  = paste0(heavies.spatial.path,'Predicted distribution ', b_package_names[[i]], ' - RF.tif')
  writeRaster(b.occmaps.rf[[i]], filename = tifname.rf, options=c('TFW=YES'), overwrite= TRUE)
  
  pngname.svm   = paste0(heavies.image.path,'Predicted distribution ', b_package_names[[i]], ' - SVM.png')
  png(filename  = pngname.svm, width = 18, height = 15, units = 'cm', res = 600)
  plot(b.occmaps.svm[[i]]) # can also plot like this plot(b.predmaps.svm[[i]], col=c('white','green'),breaks=c(0,b.threshold.svm,1))
  dev.off()
  tifname.svm  = paste0(heavies.spatial.path,'Predicted distribution ', b_package_names[[i]], ' - SVM.tif')
  writeRaster(b.occmaps.svm[[i]], filename = tifname.svm, options=c('TFW=YES'), overwrite=TRUE)
  
  pngname.gam  = paste0(heavies.image.path,'Predicted distribution ', b_package_names[[i]], ' - GAM.png')
  png(filename  = pngname.gam, width = 18, height = 15, units = 'cm', res = 600)
  plot(b.occmaps.gam[[i]]) # can also plot like this plot(b.predmaps.gam[[i]], col=c('white','green'),breaks=c(0,b.threshold.gam,1))
  dev.off()
  tifname.gam  = paste0(heavies.spatial.path,'Predicted distribution ', b_package_names[[i]], ' - GAM.tif')
  writeRaster(b.predmaps.gam[[i]], filename = tifname.gam, options=c('TFW=YES'), overwrite=TRUE)       
  
  print(paste(b_package_names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

# Combine plots to make one composite image ----
plot(b.occmaps.rf[[i]], frame.plot=FALSE) # checking coordinate ranges for text placement

png(filename = paste0(heavies.image.path,"B predicted distributions - all combinations.png", 
                    width = 30, height = 60, units = "cm", res = 600))
par(mfrow=c(9,3), mar = c(0,0,2,2), mgp=c(0,0,0), bty="n")
for (i in 1:length(b_package_names))                           {
  plot(b.occmaps.rf[[i]], axes=FALSE, ann=FALSE)
  text(x = 34.1, y = 31.6, labels = "RF",cex = 1.3, xpd = NA)
  plot(b.occmaps.svm[[i]], axes=FALSE, main = combo.descriptions[[i]])
  text(x = 34.1, y = 31.6, labels = "SVM",cex = 1.3, xpd = NA)
  plot(b.occmaps.gam[[i]], axes=FALSE, ann=FALSE, main = "GAM")
  text(x = 34.1, y = 31.6, labels = "GAM",cex = 1.3, xpd = NA)  }
dev.off()

########################################################################################################
# Complete models for each data combo-algorith combination: Schreiberi  ----

# create models for each combo
s.models.complete = list()
scenario.names = s.6scen.scen.names

for (i in 1:length(scenario.names))                                                                        {
  start.time = Sys.time()
  print(scenario.names[i])
  data = s.6scen.data.packages[[i]]
  s.models.complete[[i]] = sdm(occurrence ~ ., data = data, 
                                         methods =c('gam','rf','svm')) #"cart",'fda','gam','brt','rf','glm', 'mda','rpart','svm'
  print(paste(scenario.names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes"))}

# # see how they went:
scenario.names[[1]]; s.models.complete[[1]]
scenario.names[[2]]; s.models.complete[[2]]
scenario.names[[3]]; s.models.complete[[3]]
scenario.names[[4]]; s.models.complete[[4]]
scenario.names[[5]]; s.models.complete[[5]]
scenario.names[[6]]; s.models.complete[[6]]
s.models.complete[[i]][[3]]

saveRDS(s.models.complete, paste0(heavies.rds.path,"s.models.complete.rds"))

########################################################################################################
# Retreive evaluation summary data for reference: Schreiberi ----
s.eval.list       = readRDS("./rds_objects/s.eval.list.topmethods.rds")  # 'raw' list of full eval data from each combo
s.eval.summary    = readRDS("./rds_objects/s.eval.summary.topmethods.rds")    # consolidated list form
s.eval.summary.df = do.call("rbind", s.eval.summary) # converting the list to a single dataframe for easy plotting

########################################################################################################
# Predict the model outputs: Schreiberi ----

modelset = s.models.complete
scenario.names = s.6scen.scen.names

s.predmaps.rf    = list()
s.predmaps.svm   = list()
s.predmaps.gam   = list()

for (i in 1:length(scenario.names))                                                           {
  start.time       = Sys.time()
  par(mar=c(2,2,2,1))

  filename.rf  = paste0(heavies.spatial.path, 'Prediction ', scenario.names[[i]], ' - RF new.tif')
  s.predmaps.rf[[i]] = predict(modelset[[i]], newdata = s.preds, filename = filename.rf, 
                             format="GTiff", overwrite = TRUE, w = 2, nc = 20)
  plot(s.predmaps.rf[[i]], main=paste(scenario.names[[i]],"RF, TSS =", s.eval.summary[[i]][2,"TSS"]))
  
  filename.svm = paste0(heavies.spatial.path, 'Prediction ', scenario.names[[i]], ' - SVM new.tif')
  s.predmaps.svm[[i]]= predict(modelset[[i]], newdata = s.preds, filename = filename.svm, 
                             format="GTiff", overwrite = TRUE, w = 3, nc = 20)
  plot(s.predmaps.svm[[i]], main=paste(scenario.names[[i]],"SVM, TSS =", s.eval.summary[[i]][3,"TSS"]))
  
  filename.gam = paste0(heavies.spatial.path, 'Prediction ', scenario.names[[i]], ' - GAM new.tif')
  s.predmaps.gam[[i]]= predict(modelset[[i]], newdata = s.preds, filename = filename.gam, 
                             format="GTiff", overwrite = TRUE, w = 1, nc = 20)
  plot(s.predmaps.gam[[i]], main=paste(scenario.names[[i]],"GAM, TSS =", s.eval.summary[[i]][1,"TSS"]))

  print(paste(scenario.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

# how to view model info:
# summary(predmaps.gam[[i]])

emailme() # send an email when the loop is complete:

########################################################################################################
# Map predicted occurrence with threshold that maximises sensitivity plus specificity: Schreiberi ----

s.eval.summary  = readRDS("./rds_objects/s.eval.summary.topmethods.rds") # consolidated list form
s.occmaps.rf    =list()
s.occmaps.svm   =list()
s.occmaps.gam   =list()

for (i in 1:length(scenario.names))                                                                 {
  start.time = Sys.time()
  
  s.threshold.rf     = s.eval.summary[[i]][2, "threshold.mss"] # 5 instead of 2
  s.threshold.svm    = s.eval.summary[[i]][3, "threshold.mss"] # 9 instead of 3
  s.threshold.gam    = s.eval.summary[[i]][1, "threshold.mss"] # 3 instead of 1
  
  s.occmaps.rf[[i]]  = s.predmaps.rf[[i]]  ; s.occmaps.rf[[i]] [s.occmaps.rf[[i]]  < s.threshold.rf]   <- 0
  s.occmaps.svm[[i]] = s.predmaps.svm[[i]] ; s.occmaps.svm[[i]][s.occmaps.svm[[i]] < s.threshold.svm]  <- 0
  s.occmaps.gam[[i]] = s.predmaps.gam[[i]] ; s.occmaps.gam[[i]][s.occmaps.gam[[i]] < s.threshold.gam]  <- 0
  # s.occmaps.rf[[i]] [s.occmaps.rf[[i]]  >= s.threshold.rf]  <- 1 # leaving this step out.
  # s.occmaps.svm[[i]][s.occmaps.svm[[i]] >= s.threshold.svm] <- 1 # leaving this step out.
  # s.occmaps.gam[[i]][s.occmaps.gam[[i]] >= s.threshold.gam] <- 1 # leaving this step out.
  
  # histogram sub-loop
  {
  histname.rf   = paste0(heavies.image.path,'Prediction histrogram with threshold ', scenario.names[[i]], ' - RF.png')
  png(filename = histname.rf, width = 14, height = 10, units = 'cm', res = 200)
  title = paste0("Prediction histogram ", scenario.names[[i]], ' - RF')
  hist(s.predmaps.rf[[i]], main = title, xlab="Probability of occurrence for each map cell")  
  points(x = s.threshold.rf,  y=0, pch=24, bg='red')
  dev.off()
  
  histname.svm   = paste0(heavies.image.path,'Prediction histrogram with threshold ', scenario.names[[i]], ' - SVM.png')
  png(filename = histname.svm, width = 14, height = 10, units = 'cm', res = 200)
  title = paste0("Prediction histogram ", scenario.names[[i]], ' - SVM')
  hist(s.predmaps.svm[[i]], main = title, xlab="Probability of occurrence for each map cell")  
  points(x = s.threshold.svm,  y=0, pch=24, bg='red')
  dev.off() 
  
  histname.gam   = paste0(heavies.image.path,'Prediction histrogram with threshold ', scenario.names[[i]], ' - GAM.png')
  png(filename = histname.gam, width = 14, height = 10, units = 'cm', res = 200)
  title = paste0("Prediction histogram ", scenario.names[[i]], ' - GAM')
  hist(s.predmaps.gam[[i]], main = title, xlab="Probability of occurrence for each map cell")  
  points(x = s.threshold.gam,  y=0, pch=24, bg='red')
  dev.off() 
  }
  
  # distribution map image and raster sub-loop
  {
  pngname.rf   = paste0(heavies.image.path,'Predicted distribution ', scenario.names[[i]], ' - RF.png')
  png(filename = pngname.rf, width = 14, height = 25, units = 'cm', res = 600) 
  par(mar = c(2,2,0,0), mgp=c(2,0.5,0)) # , bty="n"
  plot(s.occmaps.rf[[i]], xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742),legend=F) 
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
  
  tifname.rf  = paste0(heavies.spatial.path,'Predicted distribution ', scenario.names[[i]], ' - RF.tif')
  writeRaster(s.occmaps.rf[[i]], filename = tifname.rf, options=c('TFW=YES'), overwrite= TRUE)
  
  pngname.svm   = paste0(heavies.image.path,'Predicted distribution ', scenario.names[[i]], ' - SVM.png')
  png(filename  = pngname.svm, width = 14, height = 25, units = 'cm', res = 600)
  par(mar = c(2,2,0,0), mgp=c(2,0.5,0)) # , bty="n"
  plot(s.occmaps.svm[[i]],legend=F) 
  plot(schreiberi.buffer, lwd=0.5, add=T)
  points(ssp.full,  col='purple',       pch=16, cex=0.4) 
  points(ssa.full,  col='pink',         pch=16, cex=0.4) 
  points(sc.nodups, col='deepskyblue2', pch=16, cex=0.4)
  points(sc.r,      col='darkblue',     pch=16, cex=0.4) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)"), 
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"), cex=1)
  dev.off()
  
  tifname.svm  = paste0(heavies.spatial.path,'Predicted distribution ', scenario.names[[i]], ' - SVM.tif')
  writeRaster(s.occmaps.svm[[i]], filename = tifname.svm, options=c('TFW=YES'), overwrite=TRUE)
  
  pngname.gam   = paste0(heavies.image.path,'Predicted distribution ', scenario.names[[i]], ' - GAM.png')
  png(filename  = pngname.gam, width = 14, height = 25, units = 'cm', res = 600)
  par(mar = c(2,2,0,0), mgp=c(2,0.5,0)) # , bty="n"
  plot(s.occmaps.gam[[i]],legend=F)
  plot(schreiberi.buffer, lwd=0.5, add=T)
  points(ssp.full,  col='purple',       pch=16, cex=0.4) 
  points(ssa.full,  col='pink',         pch=16, cex=0.4) 
  points(sc.nodups, col='deepskyblue2', pch=16, cex=0.4)
  points(sc.r,      col='darkblue',     pch=16, cex=0.4) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)"), 
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"), cex=1)
  dev.off()
  
  tifname.gam  = paste0(heavies.spatial.path,'Predicted distribution ', scenario.names[[i]], ' - GAM.tif')
  writeRaster(s.occmaps.gam[[i]], filename = tifname.gam, options=c('TFW=YES'), overwrite=TRUE)       
  }
  
  print(paste(scenario.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

# Combine plots to make one composite image  ----
plot(s.occmaps.rf[[i]], frame.plot=FALSE) # checking coordinate ranges for text placement

png(filename=paste0(heavies.image.path,"S predicted distributions - by scenario and algorithm.png"),
                    width = 40, height = 30, units = "cm", res = 600)
par(mfrow=c(3,6), mar = c(0,0,2,2), mgp=c(0,0,0), bty="n")
for (i in 1:length(scenario.names))                           {
  plot(s.occmaps.rf[[i]], axes=FALSE, ann=FALSE)
  text(x = 34.4, y = 33, labels = "RF",cex = 1.3, xpd = NA)
  plot(s.occmaps.svm[[i]], axes=FALSE, main = s.6scenario.descriptions[[i]])
  text(x = 34.4, y = 33, labels = "SVM",cex = 1.3, xpd = NA)
  plot(s.occmaps.gam[[i]], axes=FALSE, ann=FALSE, main = "GAM")
  text(x = 34.4, y = 33, labels = "GAM",cex = 1.3, xpd = NA)  }
dev.off()

######################################################################################################
# other how to ----

# How to make an ensemble of just the models with high TSS, weighted by TSS:
id = eval.list[[i]]$modelID[models$TSS > 0.5]
package.ensembles[[i]] = ensemble(model.list[[i]], b.preds, filename = filename, overwrite=TRUE, 
                                  setting=list(method ='weighted', stat='TSS', opt=2, id=id))
png(plot, filename = filename, width = 17, height = 25.7, units = "cm", res = 100)
plot(package.ensembles[[i]])
dev.off()

class(b.model.list.complete[[i]])

# changing model settings:
# you can change the setting by using the modelSettings argument in the sdm function. Eg to change brt trees:
m <- sdm(...., modelSettings=list(brt=list(n.trees=3000)))
# For each method, you need to check the relevant package (e.g., dismo for maxent) to see what parameters are used. In the case of maxent, yes, currently the default settings are used.

########################################################################################################
