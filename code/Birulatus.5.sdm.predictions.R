# sdm.5.model.prediction and mapping ----

########################################################################################################
# Set up and install relevant packages and locations ----

# clear user interface and free memory:
if(!is.null(dev.list())) dev.off() # Clear plots
cat("\014")                        # Clear console
rm(list=ls())                      # Clean workspace
gc()

# ipak function: install (if not already installed) and load multiple R packages.
ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)}
ipak(c("rgdal","stringr","usdm", "biomod2","raster","scales", "grid", "foreign","dplyr","magrittr","tidyr","rgeos",
       "magrittr","ggplot2","gridExtra","raster","rasterVis","dismo","sdm","installr","knitr","ggmap",
       "OpenStreetMap","parallel","beepr","rmapshaper", "spatialEco", "rJava","xlsx"))
installAll() # installing everything the sdm relies on.

emailme = function() {
  send.mail(from="kerengila@gmail.com",       to="keren.raiter@mail.huji.ac.il",
            subject="the loop is complete",   body="yeah yeah yeah",        html=T,
            smtp=list(host.name = "smtp.gmail.com",          port = 465,
                      user.name = "kerengila@gmail.com",     passwd = "Ivrit333",
                      ssl = T), authenticate=T) #, attach.files="C:\\Users\\Deepanshu\\Nature.xls"  
}

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

# Birulatus heavies will be on E drive (at least for now), with E drive being backed up to HUJI server regularly
B.heavies.spatial.path = 'E:/R/Birulatus_heavies/spatial/'
B.heavies.rds.path     = 'E:/R/Birulatus_heavies/rds/'
B.heavies.image.path   = 'E:/R/Birulatus_heavies/images/'

# Datasets prepared earlier ----
# from this script:

# from previous scripts:
eval.list       = readRDS("./rds/s.eval.list.topmethods.rds")       # 'raw' list of eval data from each model
eval.summary    = readRDS("./rds/s.eval.summary.topmethods.rds")    # consolidated: multiple reps averaged
eval.summary.df = readRDS("./rds/s.eval.summary.df.topmethods.rds") # superconsolidate:all scenarios, 1 table)
methods.summary = readRDS("./rds/s.eval.summary.df.topmethods.rds") # summary by method, in order.
top.algorithms  = c('rf','brt','svm','gam')

package_names          = readRDS("rds/package_names.rds")
scenario.descriptions  = readRDS("rds/scenario.descriptions.rds")
obs_packages           = readRDS("rds/obs_packages.rds")
data_packages          = readRDS("rds/data_packages.rds")

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
preds.nocoll      = readRDS(paste0(B.heavies.rds.path,"preds.nocoll.rds")) # raster stack


bi.raw = readRDS("./rds/bi.raw.rds") 
bi     = readRDS("./rds/bi.rds") 
bip    = readRDS("./rds/bip.rds")
bia    = readRDS("./rds/bia.rds")




########################################################################################################
# Complete models  ----

# create models for each combo
model.list.complete = list()

for (i in 1:length(data_packages))                                                                        {
  start.time = Sys.time()
  print(package_names[i])
  data = data_packages[[i]]
  model.list.complete[[i]] = sdm(occurrence ~ ., data = data, methods = top.algorithms)
  print(paste(package_names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes"))}

# # see how they went:
# b_package_names[[1]]; b.model.list.complete[[1]]
# b_package_names[[2]]; b.model.list.complete[[2]]
# b_package_names[[3]]; b.model.list.complete[[3]]
# b_package_names[[4]]; b.model.list.complete[[4]]
# b.model.list.complete[[i]][[5]]

########################################################################################################
# Predict the model outputs ----

predmaps.rf    = list()
predmaps.brt   = list()
predmaps.svm   = list()
predmaps.gam   = list()

for (i in 1:length(package_names))                                                           {
  start.time       = Sys.time()
  par(mar=c(2,2,2,1))

  filename.rf  = paste0(B.heavies.spatial.path, 'Prediction ', package_names[[i]], ' - RF.tif')
  predmaps.rf[[i]] = predict(model.list.complete[[i]], newdata=preds.nocoll, filename = filename.rf, 
                             format="GTiff", overwrite=TRUE, w=1, nc=20)
  plot(predmaps.rf[[i]], main=paste(package_names[[i]],"RF, TSS =", eval.summary[[i]][5,"TSS"]))
  
  filename.brt  = paste0(B.heavies.spatial.path, 'Prediction ', package_names[[i]], ' - BRT.tif')
  predmaps.brt[[i]] = predict(model.list.complete[[i]], newdata=preds.nocoll, filename = filename.brt, 
                             format="GTiff", overwrite=TRUE, w=2, nc=20)
  plot(predmaps.brt[[i]], main=paste(package_names[[i]],"BRT, TSS =", eval.summary[[i]][4,"TSS"]))
  
  filename.svm = paste0(B.heavies.spatial.path, 'Prediction ', package_names[[i]], ' - SVM.tif')
  predmaps.svm[[i]]= predict(model.list.complete[[i]], newdata=preds.nocoll, filename = filename.svm, 
                             format="GTiff", overwrite=TRUE, w=3, nc=20)
  plot(predmaps.svm[[i]], main=paste(package_names[[i]],"SVM, TSS =", eval.summary[[i]][9,"TSS"]))
  
  filename.gam = paste0(B.heavies.spatial.path, 'Prediction ', package_names[[i]], ' - GAM.tif')
  predmaps.gam[[i]]= predict(model.list.complete[[i]], newdata=preds.nocoll, filename = filename.gam, 
                             format="GTiff", overwrite=TRUE, w=4, nc=20)
  plot(predmaps.gam[[i]], main=paste(package_names[[i]],"GAM, TSS =", eval.summary[[i]][3,"TSS"]))

  print(paste(package_names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

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
