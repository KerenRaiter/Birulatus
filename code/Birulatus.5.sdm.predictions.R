# sdm.5.model.prediction and mapping ----

###################################################################################################
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
ipak(c("rgdal","stringr","usdm", "biomod2","raster","scales", "grid", "foreign","dplyr","magrittr",
       "tidyr","rgeos","ggplot2","gridExtra","rasterVis","dismo","sdm","installr","knitr","ggmap",
       "OpenStreetMap","parallel","beepr","rmapshaper", "spatialEco", "rJava","xlsx","readxl"))
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

predmaps.fda = readRDS(paste0(B.heavies.rds.path, "predmaps.fda.rds"))
predmaps.svm = readRDS(paste0(B.heavies.rds.path, "predmaps.svm.rds"))
predmaps.rf  = readRDS(paste0(B.heavies.rds.path, "predmaps.rf.rds"))


set.ensembles = readRDS(paste0(B.heavies.rds.path, "set.ensembles.rds"))


# from previous scripts:

# 'raw' list of eval data from each model:
eval.list       = readRDS(paste0(B.heavies.rds.path, "eval.list.topmethods.rds"))       
# consolidated: multiple reps averaged:
eval.summary    = readRDS(paste0(B.heavies.rds.path, "eval.summary.topmethods.rds"))    
# superconsolidate:all scenarios, 1 table)
eval.summary.df = readRDS(paste0(B.heavies.rds.path, "eval.summary.df.topmethods.rds")) 
# summary by method, in order:
# methods.summary = readRDS("./rds/s.eval.summary.df.topmethods.rds") 
top.algs        = c('fda','svm','rf')
top.algs.l      = list('fda','svm','rf')

set.names = list("Soils-delimited study area", "Lithology-delimited study area", 
                 "Israel-wide study area")
set.descriptions = list("Focused study area; soil-type included",
                        "Focused study area; lithology included",
                        "Broad model, no soil/lithology data")
data.packs          = readRDS("./rds/data.packs.bysite.rds")

borders          = readRDS("rds/borders.rds");            israel.WB = readRDS("rds/israel.WB.rds")
israel.WB.merged = readRDS("./rds/israel.WB.merged.rds"); israel.WB = readRDS("./rds/israel.WB.no.water.rds")
israel.noWB      = readRDS("rds/israel.noWB.rds")

bir.area.s = readRDS("./rds/bir.area.s.rds")
bir.area.l = readRDS("./rds/bir.area.l.rds")
bir.area.i = readRDS("./rds/bir.area.i.rds")

major.cities = readRDS("./rds/major.cities.rds")
major.cities = major.cities[major.cities$name != "Tyre (Sur)", ]   # remove Tyre
small.cities = readRDS("./rds/small.cities.rds")
large.towns  = readRDS("./rds/large.towns.rds")
towns        = readRDS("./rds/towns.rds")

groads       = readRDS("./rds/groads.rds")

raster.list.s  = readRDS(paste0(B.heavies.rds.path,"raster.list.s.rds"))
raster.list.l  = readRDS(paste0(B.heavies.rds.path,"raster.list.l.rds"))
raster.list.i  = readRDS(paste0(B.heavies.rds.path,"raster.list.i.rds"))

raster.list.s.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Soil")
raster.list.l.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop", "Lith")
raster.list.i.names = list("Rain", "Jant", "Jult", "DEM", "TWet", "Slop")

# preds.s = readRDS(paste0(B.heavies.rds.path,"preds.s.rds")) # raster stack. Slow.
# preds.l = readRDS(paste0(B.heavies.rds.path,"preds.l.rds")) # raster stack. Slow.
# preds.i = readRDS(paste0(B.heavies.rds.path,"preds.i.rds")) # raster stack. Slow.

preds.s.nocoll      = readRDS(paste0(B.heavies.rds.path,"preds.s.nocoll.rds")) # raster stack
preds.l.nocoll      = readRDS(paste0(B.heavies.rds.path,"preds.l.nocoll.rds")) # raster stack
preds.i.nocoll      = readRDS(paste0(B.heavies.rds.path,"preds.i.nocoll.rds")) # raster stack
preds = list(preds.s.nocoll, preds.l.nocoll, preds.i.nocoll)

b.raw = readRDS("./rds/b.raw.bysite.rds")
b     = readRDS("./rds/b.bysite.rds")
b.s   = readRDS("./rds/b.bysite.s.rds")
b.l   = readRDS("./rds/b.bysite.l.rds")
b.i   = readRDS("./rds/b.bysite.i.rds")

###################################################################################################
# Complete models  ----

# create models for each combo
model.list.complete = list()

for (i in 1:length(data.packs))  {
  start.time = Sys.time()
  print(set.names[i])
  data = data.packs[[i]]
  model.list.complete[[i]] = sdm(occurrence ~ ., data = data, methods = top.algs)
  print(paste(set.names[[i]]," loop took ",difftime(Sys.time(),start.time, units="mins")," minutes"))}

# # see how they went:
# set.names[[1]]; model.list.complete[[1]]
# set.names[[2]]; model.list.complete[[2]]
# set.names[[3]]; model.list.complete[[3]]
# model.list.complete[[i]][[5]]

###################################################################################################
# Predict species occurrence probability over whole study area ----

for (a in 1:length(top.algs)) { assign ( paste0("predmaps.",top.algs[[a]]), list() ) } # make lists

for (i in 1:length(set.names))                                                     {  # takes ~5mins
  start.time       = Sys.time()
  par(mar=c(2,2,2,1))

  # FDA:
  filename.fda  = paste0(B.heavies.spatial.path, 'Prediction ', set.names[[i]], ' - FDA.tif')
  predmaps.fda[[i]] = predict(model.list.complete[[i]], newdata=preds[[i]], filename = filename.fda,
                            format="GTiff", overwrite=TRUE, w = 1, nc=4) # w is model number
  plot(predmaps.fda[[i]], main=paste(set.names[[i]],"FDA, TSS =", 
                                     eval.summary[[i]][eval.summary[[i]]$method == "fda","TSS"]))

  # SVM:
  filename.svm = paste0(B.heavies.spatial.path, 'Prediction ', set.names[[i]], ' - SVM.tif')
  predmaps.svm[[i]]= predict(model.list.complete[[i]], newdata=preds[[i]], filename = filename.svm, 
                             format="GTiff", overwrite=TRUE, w = 2, nc=4) # nc is number of comp cores
  plot(predmaps.svm[[i]], main=paste(set.names[[i]],"SVM, TSS =", 
                                     eval.summary[[i]][eval.summary[[i]]$method == "svm","TSS"]))
  
  # RF:
  filename.rf  = paste0(B.heavies.spatial.path, 'Prediction ', set.names[[i]], ' - RF.tif')
  predmaps.rf[[i]] = predict(model.list.complete[[i]], newdata = preds[[i]], filename = filename.rf,
                            format="GTiff", overwrite=TRUE, w = 3, nc=4)
  plot(predmaps.rf[[i]], main=paste(set.names[[i]],"RF, TSS =", 
                                    eval.summary[[i]][eval.summary[[i]]$method == "rf","TSS"]))

  print(paste(set.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

# how to view model info:
# summary(predmaps.gam[[i]])

saveRDS(predmaps.fda, paste0(B.heavies.rds.path, "predmaps.fda.rds"))
saveRDS(predmaps.svm, paste0(B.heavies.rds.path, "predmaps.svm.rds"))
saveRDS(predmaps.rf,  paste0(B.heavies.rds.path, "predmaps.rf.rds"))

beep()
emailme() # send an email when the loop is complete:

# save predictions as images
for(i in 1:length(set.names))  {

  filename.fda     = paste0(B.heavies.image.path, 'Prediction ', set.names[[i]], ' - FDA.png')
  png(filename     = filename.fda, width = 18, height = 30, units = 'cm', res = 600)
  plot(predmaps.fda[[i]], 
       main = paste(set.names[[i]],"FDA, TSS =", 
                  eval.summary[[i]][eval.summary[[i]]$method == "fda","TSS"]))
  dev.off()
  
  filename.svm     = paste0(B.heavies.image.path, 'Prediction ', set.names[[i]], ' - SVM.png')
  png(filename     = filename.svm, width = 18, height = 30, units = 'cm', res = 600)
  plot(predmaps.svm[[i]], 
       main = paste(set.names[[i]],"SVM, TSS =", 
                  eval.summary[[i]][eval.summary[[i]]$method == "svm","TSS"]))
  dev.off()
  
  
  filename.rf      = paste0(B.heavies.image.path, 'Prediction ', set.names[[i]], ' - RF.png')
  png(filename     = filename.rf, width = 18, height = 30, units = 'cm', res = 600)
  plot(predmaps.rf[[i]], 
       main = paste(set.names[[i]],"RF, TSS =", 
                  eval.summary[[i]][eval.summary[[i]]$method == "rf","TSS"]))
  dev.off()
}

###################################################################################################
# Retrieve predicted outcomes: (only if previous section not run) legacy ----
# 
# eval.summary = readRDS("./rds_objects/eval.summary.5fold.100reps.rds")  
# 
# predmaps.rf    = list()
# predmaps.svm   = list()
# predmaps.gam   = list()

# # load from rasters, plot, and save full images 
{
# for(i in 1:length(set.names))                                                                       {
#  
#   filename.rf      = paste('./output_images/Prediction', set.names[[i]], '- RF.tif')
#   predmaps.rf[[i]] = raster(filename.rf)
#   plot(predmaps.rf[[i]], main=paste(set.names[[i]],"RF, TSS =", eval.summary[[i]][5,"TSS"]))
#   
#   filename.svm      = paste('./output_images/Prediction', set.names[[i]], '- SVM.tif')
#   predmaps.svm[[i]] = raster(filename.svm)
#   plot(predmaps.svm[[i]], main=paste(set.names[[i]],"SVM, TSS =", eval.summary[[i]][9,"TSS"]))
#   
#   filename.gam     = paste('./output_images/Prediction', set.names[[i]], '- GAM.tif')
#   predmaps.gam[[i]] = raster(filename.gam)
#   plot(predmaps.gam[[i]], main=paste(set.names[[i]],"GAM, TSS =", eval.summary[[i]][3,"TSS"]))   }
# 
# lsos()
# 
# # Save plots as full-colur pictures ----
# for(i in 1:length(set.names))                                                                    {
#   
#   filename.rf      = paste('./output_images/Predicted probabilities', set.names[[i]], '- RF.png')
#   png(filename     = filename.rf, width = 18, height = 15, units = 'cm', res = 600)
#   plot(predmaps.rf[[i]], main=paste(set.names[[i]],"RF, TSS =", eval.summary[[i]][5,"TSS"]))
#   dev.off()
#   
#   filename.svm     = paste('./output_images/Predicted probabilities', set.names[[i]], '- SVM.png')
#   png(filename     = filename.svm, width = 18, height = 15, units = 'cm', res = 600)
#   plot(predmaps.svm[[i]], main=paste(set.names[[i]],"SVM, TSS =", eval.summary[[i]][9,"TSS"]))
#   dev.off()
#   
#   filename.gam      = paste('./output_images/Predicted probabilities', set.names[[i]], '- GAM.png')
#   png(filename      = filename.gam, width = 18, height = 15, units = 'cm', res = 600)
#   plot(predmaps.gam[[i]], main=paste(set.names[[i]],"GAM, TSS =", eval.summary[[i]][3,"TSS"])) 
#   dev.off()                                                                                      }
}

###################################################################################################
# Ensembles across the different methods ----

set.ensembles = list()
models        = model.list.complete
preds = list(preds.s.nocoll, preds.l.nocoll, preds.i.nocoll)

for(i in 1:length(set.names)) {

  file = paste0(B.heavies.spatial.path, 'Ensemble prediction - ', set.names[[i]], '.tif')
  set.ensembles[[i]] = ensemble(models[[i]], preds[[i]], filename = file, overwrite = TRUE, 
                                setting=list(method ='weighted', stat='TSS', opt=2)) 
                                 # opt=2 selects for max(sensitivity + specificity)
  
  file = paste0(B.heavies.image.path, 'Ensemble prediction - ', set.names[[i]], '.png')
  png(plot, filename = file, width = 18, height = 30, units = "cm", res = 100)
  plot(set.ensembles[[i]])
  dev.off()
  }

saveRDS(set.ensembles, paste0(B.heavies.rds.path, "set.ensembles.rds"))

beep()  

###################################################################################################
# Map ensemble distributions (within-scenario ensembles) ----

plot(set.ensembles[[1]]); plot(set.ensembles[[2]]); plot(set.ensembles[[3]])

# here 'prediction' = prediction with full range of probabilities; distribution = above-threshold.

thresholds       = list()
for (t in 1:length(set.names)){
  thresholds[[t]] = 
    weighted.mean( c(eval.summary[[t]][eval.summary[[t]]$method == "fda", "threshold.mss"],
                     eval.summary[[t]][eval.summary[[t]]$method == "svm", "threshold.mss"],
                     eval.summary[[t]][eval.summary[[t]]$method == "rf",  "threshold.mss"]),
                   c(eval.summary[[t]][eval.summary[[t]]$method == "fda", "TSS"],
                     eval.summary[[t]][eval.summary[[t]]$method == "svm", "TSS"],
                     eval.summary[[t]][eval.summary[[t]]$method == "rf",  "TSS"])) }

ensemble.dist       = list()
study.areas         = list(bir.area.s, bir.area.l, bir.area.i)
xmins               = list( xmin(bir.area.s), xmin(bir.area.l), xmin(bir.area.i) )
xmaxs               = list( xmax(bir.area.s), xmax(bir.area.l), xmax(bir.area.i) )
ymins               = list( ymin(bir.area.s), ymin(bir.area.l), ymin(bir.area.i) )
ymaxs               = list( ymax(bir.area.s), ymax(bir.area.l), ymax(bir.area.i) )
select.towns        = large.towns[large.towns$name == "Tiberias" | large.towns$name == "Afula",]

for (i in 1:length(set.names)) {   # takes ~ 1 minute
  start.time = Sys.time()
  # get distribution:
  ensemble.dist[[i]] = set.ensembles[[i]]
  ensemble.dist[[i]][ ensemble.dist[[i]] <  thresholds[[i]]]  <- 0
  # s.ensemble.dist[[i]][s.ensemble.dist[[i]] >= s.ensemble.thresholds[[i]]]  <- 1 
  # can leave above step out, to keep the variability of probabilities above the threshold.
  
  # make histogram:
  hist.filename  = paste0(B.heavies.image.path,'Ensemble histogram - ', set.names[[i]], '.png')
  png(filename = hist.filename, width = 18, height = 15, units = 'cm', res = 300)
  hist(set.ensembles[[i]]);  points(x = thresholds[[i]], y=0, pch=24, bg='red')
  dev.off()
  
  # make multitone distribution image (showing different levels of probability of occurrence):
  plot.filename  = paste0(B.heavies.image.path,'Ensemble distribution - ', set.names[[i]], '.png')
  par(mar = c(2,2,2,0), mgp=c(2,0.5,0)) # , bty="n"
  png(filename = plot.filename, width = 16, height = 25, units = 'cm', res = 900)
  plot(ensemble.dist[[i]], xlim=c(xmins[[i]], xmaxs[[i]]), ylim=c(ymins[[i]], ymaxs[[i]]), 
       # or xlim = c(34.5,36)???
       main = "Predicted Birulatus distribution, using an ensemble of 4 modelling approaches", cex.main=0.7) 
  lines(israel.WB, col="darkgrey", lty=5, lwd=2)
  lines(study.areas[[i]])
  points(b[b$occurrence == 1,],col='blue',pch=16, cex=0.7)
  points(b[b$occurrence == 0,],col='red',pch=16, cex=0.7)
  lines(groads, col="lightgrey")
  points(major.cities, pch=21, col='black', bg='yellow', cex=1.3)
  points(select.towns,        pch=21, col='black', bg='yellow', cex=0.8)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=0.6, offset=0.3)) 
  with(select.towns, text(select.towns$lat~select.towns$lon, labels=select.towns$name, pos=4, cex=0.6, offset=0.3))
  legend("bottomright",c("Presence", "Absence"), col=c("blue","red"),pch=16,cex=.7)
  
  # alternatively: plot(s.predmaps.rf[[i]], col = c('white','green'), breaks=c(0, s.threshold.rf,1))
  dev.off()
  
  # make raster:
  raster.filename = paste0(B.heavies.spatial.path,'Ensemble distribution - ',set.names[[i]], '.tif')
  writeRaster(ensemble.dist[[i]], filename = raster.filename, options=c('TFW=YES'), overwrite= TRUE)
  
  # make unitone version for converting to polygon:
  # ensemble.dist.unitone = ensemble.dist[[i]]
  # ensemble.dist.unitone[ensemble.dist.unitone >= thresholds[[i]]]  <- 1
  # writeRaster(ensemble.dist.unitone, 
  #            filename = paste0(B.heavies.spatial.path,'Ensemble distribution unitone - ', 
  #                              set.names[[i]], '.tif'),
  #            options=c('TFW=YES'), overwrite= TRUE)
  # unsure if the following doesn't work or is just super slow:
  # distribution.poly = rasterToPolygons(ensemble.dist.unitone, digits=12, na.rm=TRUE,dissolve=T); plot(distribution.poly)
  
  # make unitone distribution image:
  plot.filename  = paste0(B.heavies.image.path,'Ensemble distribution - ', set.names[[i]], '_unitone.png')
  par(mar = c(2,2,2,0), mgp=c(2,0.5,0)) # , bty="n"
  png(filename = plot.filename, width = 16, height = 25, units = 'cm', res = 300)
  plot(ensemble.dist[[i]], col = c('white','blue'), breaks=c(0, thresholds[[i]], 1), 
       legend=FALSE, xlim=c(xmins[[i]], xmaxs[[i]]), ylim=c(ymins[[i]], ymaxs[[i]]),
       main = "Predicted Birulatus distribution, using an ensemble of 3 modelling approaches", cex.main=0.7) 
  lines(israel.WB, col="darkgrey", lty=5, lwd=2)
  lines(study.areas[[i]])
  points(b[b$occurrence == 1,], col='blue', pch=16, cex=0.7)
  points(b[b$occurrence == 0,], col='red',  pch=16, cex=0.7)
  lines(groads, col="lightgrey")
  points(major.cities, pch=21, col='black', bg='yellow', cex=1.3)
  select.towns = large.towns[large.towns$name == "Tiberias" | large.towns$name == "Afula",]
  points(select.towns,        pch=21, col='black', bg='yellow', cex=0.8)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=0.6, offset=0.3)) 
  with(select.towns, text(select.towns$lat~select.towns$lon, labels=select.towns$name, pos=4, cex=0.6, offset=0.3))
  legend("bottomright",c("Presence","Absence","Predicted distribution"), 
         col=c("blue","red","green"), pt.bg="darkgreen", pch=c(16,16,22), cex=.7)
  
  # alternatively: plot(s.predmaps.rf[[i]], col = c('white','green'), breaks=c(0, s.threshold.rf,1))
  dev.off()
  
  print(paste(set.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

# need to figure out how to convert to polygon, and also to convert to KML (either from polygon or raster)

saveRDS(thresholds, "./rds/ensemble.thresholds.rds")
saveRDS(ensemble.dist, paste0(B.heavies.rds.path,"ensemble.dist.rds"))

# or load previously made versions:
thresholds     = readRDS("./rds/ensemble.thresholds.rds")
ensemble.dist  = readRDS(paste0(B.heavies.rds.path,"ensemble.dist.rds"))

# Combine plots to make one composite image ----
bir.area.i # 34.26693, 35.89533, 29.49042, 33.33407  (xmin, xmax, ymin, ymax)
           # checking coordinate ranges for text placement

png(filename = paste0(B.heavies.image.path,"Predicted distributions by study area.png"), 
                    width = 20, height = 16, units = "cm", res = 400)
{
par(mfrow=c(1,3), mar = c(0,0,2,4), mgp=c(0,0,0), oma = c(0, 0, 1.8, 0), bty = "n")

for (i in 1:length(set.names)) { 
  plot(bir.area.i, axes=FALSE, ann=FALSE)
  plot(ensemble.dist[[i]], add=TRUE,
       col = c('white','chartreuse4'), breaks=c(0, thresholds[[i]], 1), 
     legend=FALSE, xlim=c(34.26693, 35.89533), ylim=c(29.49042, 33.33407)) 
lines(groads, col="lightgrey")
lines(israel.WB, col="darkgrey", lty=5, lwd=2)
points(b[b$occurrence == 1,], col='blue', pch=16, cex=0.7)
points(b[b$occurrence == 0,], col='red',  pch=16, cex=0.7)
points(major.cities, pch=21, col='black', bg='yellow', cex=1.3)
points(select.towns,        pch=21, col='black', bg='yellow', cex=0.8)
lines(study.areas[[i]], col="orange")
with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=0.6, offset=0.3)) 
with(select.towns, text(select.towns$lat~select.towns$lon, labels=select.towns$name, pos=4, cex=0.6, offset=0.3))
text(x = 35.2, y = 33.4, labels = toupper(set.descriptions[[i]]), cex = 0.9, xpd = NA) 
legend("bottomright", c("Presence","Absence","Predicted distribution", "Study area"), 
       pch=c(16,16,22,22),  cex=.7, 
       col=c("blue","red","chartreuse4","orange"), 
       pt.bg= c(NA, NA, "chartreuse4", NA))
}
mtext("Predicted Birulatus distribution, using an ensemble of 3 modelling approaches", 
      outer = TRUE, cex = 1.2)
} 
dev.off()

###################################################################################################
# Legacy ----

  # distribution map image and raster sub-loop
  {
  pngname.rf   = paste0(heavies.image.path,'Predicted distribution ', set.names[[i]], ' - RF.png')
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
  
  tifname.rf  = paste0(heavies.spatial.path,'Predicted distribution ', set.names[[i]], ' - RF.tif')
  writeRaster(s.occmaps.rf[[i]], filename = tifname.rf, options=c('TFW=YES'), overwrite= TRUE)
  
  pngname.svm   = paste0(heavies.image.path,'Predicted distribution ', set.names[[i]], ' - SVM.png')
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
  
  tifname.svm  = paste0(heavies.spatial.path,'Predicted distribution ', set.names[[i]], ' - SVM.tif')
  writeRaster(s.occmaps.svm[[i]], filename = tifname.svm, options=c('TFW=YES'), overwrite=TRUE)
  
  pngname.gam   = paste0(heavies.image.path,'Predicted distribution ', set.names[[i]], ' - GAM.png')
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
  
  tifname.gam  = paste0(heavies.spatial.path,'Predicted distribution ', set.names[[i]], ' - GAM.tif')
  writeRaster(s.occmaps.gam[[i]], filename = tifname.gam, options=c('TFW=YES'), overwrite=TRUE)       
  
print(paste(set.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}



###################################################################################################
# other how to ----

# How to make an ensemble of just the models with high TSS, weighted by TSS:
id = eval.list[[i]]$modelID[models$TSS > 0.5]
package.ensembles[[i]] = ensemble(model.list[[i]], b.preds, filename = filename, overwrite=TRUE, 
                                  setting=list(method ='weighted', stat='TSS', opt=2, id=id))
png(plot, filename = filename, width = 17, height = 25.7, units = "cm", res = 100)
plot(package.ensembles[[i]])
dev.off()

class(model.list.complete[[i]])

# changing model settings:
# you can change the setting by using the modelSettings argument in the sdm function. Eg to change brt trees:
m <- sdm(...., modelSettings=list(brt=list(n.trees=3000)))
# For each method, you need to check the relevant package (e.g., dismo for maxent) to see what parameters are used. In the case of maxent, yes, currently the default settings are used.

########################################################################################################
