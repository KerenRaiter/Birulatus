# Ensemble modelling for Birulatus ----
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
ipak(c("stringr","usdm","sdm","raster","rgdal","tidyverse","png","beepr","biomod2","scales", "grid", "foreign",
       "dplyr","magrittr","tidyr","rgeos","magrittr","ggplot2","gridExtra","raster","rasterVis","dismo","installr",
       "knitr","ggmap","OpenStreetMap","parallel","rmapshaper", "spatialEco", "rJava","xlsx","mailR"))
installAll() # installing everything the sdm relies on.

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
  png(filename = paste0(heavies.image.path,"schreiberi/","S ", deparse(substitute(distribution)), ".png"), 
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

# define map limits for plotting later:
xlims.s      = c(35.14081, 35.86214) # for plotting whole study area
ylims.s      = c(31.59407, 33.00496) # for plotting whole study area
#xlims.s.dist = c(34.27179, 35.32475) # for plotting distributions (i.e. may be smaller)
#ylims.s.dist = c(31.12669, 33.10753) # for plotting distributions (i.e. may be smaller)

# Load datasets prepared earlier from previous scripts ----

predmaps.rf       = readRDS(paste0(B.heavies.rds.path,"predmaps.rf.rds"))
predmaps.brt      = readRDS(paste0(B.heavies.rds.path,"predmaps.brt.rds"))
predmaps.svm      = readRDS(paste0(B.heavies.rds.path,"predmaps.svm.rds"))
predmaps.gam      = readRDS(paste0(B.heavies.rds.path,"predmaps.gam.rds"))

occmaps.rf       = readRDS(paste0(B.heavies.rds.path,"occmaps.rf.rds"))
occmaps.brt      = readRDS(paste0(B.heavies.rds.path,"occmaps.brt.rds"))
occmaps.svm      = readRDS(paste0(B.heavies.rds.path,"occmaps.svm.rds"))
occmaps.gam      = readRDS(paste0(B.heavies.rds.path,"occmaps.gam.rds"))

eval.list       = readRDS("./rds/s.eval.list.topmethods.rds")       # 'raw' list of eval data from each model
eval.summary    = readRDS("./rds/s.eval.summary.topmethods.rds")    # consolidated: multiple reps averaged
eval.summary.df = readRDS("./rds/s.eval.summary.df.topmethods.rds") # superconsolidate:all scenarios, 1 table)
methods.summary = readRDS("./rds/s.eval.summary.df.topmethods.rds") # summary by method, in order.
top.algs        = c('rf','brt','svm','gam')
top.algs.l      = list('rf','brt','svm','gam')

scenario.names         = readRDS("rds/scenario.names.rds")
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

###################################################################################################
# Summary of outputs ----

# s.models.tops         = readRDS(paste0(heavies.rds.path,"s.models.tops.rds")) # no longer exists
s.ensembles           = readRDS(paste0(heavies.rds.path,"s.ensembles.rds"))
# s.ensemble.thresholds = readRDS(paste0(heavies.rds.path,"s.ensemble.thresholds.rds"))
s.ensemble.dist       = readRDS(paste0(heavies.rds.path,"s.ensemble.dist.rds"))

#variable importance by model and combination
varimportance.rf    = readRDS(paste0(heavies.rds.path,"s.varimportance.rf.rds"))
varimportance.svm   = readRDS(paste0(heavies.rds.path,"s.varimportance.svm.rds"))
varimportance.gam   = readRDS(paste0(heavies.rds.path,"s.varimportance.gam.rds"))
varimportance       = readRDS(paste0(heavies.rds.path,"s.varimportance.rds"))

# variable importanct averaged across combos
var.importance      = readRDS("./rds_objects/s.var.importance.rds")
var.importance.df   = readRDS("./rds_objects/s.var.importance.df.rds") 
rcurves             = readRDS(paste0(heavies.rds.path,"s.rcurves.rds"))

# note: since 3/08/2019, have decided on a better model configuration than the original: veg excluded 
# (unreliable/incomplete), presence-absence models only, and complete datasets only (no survey-only). 
# This means that there are only two scenarios: with and without the unreliable collections data. See sdm.7b.

s.preds.noveg      = readRDS(paste0(heavies.rds.path,"s.preds.noveg.rds"))
s.data.packs.noveg = readRDS(paste0(heavies.rds.path,"s.data.packs.noveg.rds"))

s.eval.list.noveg       = readRDS("./rds_objects/s.eval.list.noveg.rds")   # 'raw' list of full eval data from each combo
s.eval.summary.noveg    = readRDS("./rds_objects/s.eval.summary.noveg.rds")  # consolidated list (multi reps averaged)
s.eval.summary.df.noveg = readRDS("./rds_objects/s.eval.summary.df.noveg.rds") # consolidated table (all combos together)

grand.ensemble    = raster(paste0(heavies.spatial.path, "s.grandensemble.noveg.tif")); plot(grand.ensemble)
grand.distribution = raster(paste0(heavies.spatial.path, "s.grand.distribution.noveg.tif"))

###################################################################################################
# Create models for each combo, just with the top algorithms ----

# create models for each combo
model.list.complete = list()

for (i in 1:length(data_packages))                                               {
  data = data_packages[[i]]
  model.list.complete[[i]] = sdm(occurrence ~ ., data = data, methods = top.algs)}

###################################################################################################
# Run ensemble loop (within-scenario ensembles) ----

ensembles      = list()
modelset       = model.list.complete

for (i in 1:length(scenario.names))                                                                         { #~3 hours?
  start.time = Sys.time();  print(scenario.names[i])
  
  weights        = list()
  for (a in 1:length(top.algs.l)){
    weights[[a]] = eval.summary[[i]]$TSS[eval.summary[[i]]$method == top.algs[[a]]]}
  
  filename = paste0(B.heavies.spatial.path,'Birulaltus ensemble - ', scenario.names[[i]], '.tif')
  ensembles[[i]] = ensemble(modelset[[i]], newdata = preds.nocoll, filename = filename, 
                            setting = list(method = 'weighted', weights = unlist(weights)), 
                            nc=20, format="GTiff", overwrite=T)  
  print(paste(scenario.names[[i]]," loop took ", difftime(Sys.time(), start.time, units="mins")," minutes")) }

saveRDS(ensembles, paste0(B.heavies.rds.path, "ensembles.rds"))
emailme()

###################################################################################################
# Map ensemble distributions (within-scenario ensembles) ----

# here 'prediction' = prediction with full range of probabilities; distribution = above-threshold.
ensemble.dist       = list()
ensemble.thresholds = list()

for (i in 1:length(scenario.names)) {   # takes ~ 3 minutes
  
  start.time = Sys.time()
  
  # get threshold:
  threshold.mss = list()
  for (a in 1:length(top.algs)){ threshold.mss[[a]] = eval.summary[[i]][top.algs.evalsum.num[[a]], "threshold.mss"]}
  ensemble.thresholds[[i]] = mean(unlist(threshold.mss))
  
  # get distribution:
  ensemble.dist[[i]] = ensembles[[i]]
  ensemble.dist[[i]][ensemble.dist[[i]] <  ensemble.thresholds[[i]]]  <- 0
  # s.ensemble.dist[[i]][s.ensemble.dist[[i]] >= s.ensemble.thresholds[[i]]]  <- 1 
  # can leave above step out, to keep the variability of probabilities above the threshold.
  
  # make histogram:
  hist.filename  = paste0(B.heavies.image.path,'Ensemble histogram - ', scenario.names[[i]], '.png')
  png(filename = hist.filename, width = 18, height = 15, units = 'cm', res = 300)
  hist(ensembles[[i]]);  points(x = ensemble.thresholds[[i]], y=0, pch=24, bg='red')
  dev.off()
  
  # make distribution image:
  plot.filename  = paste0(B.heavies.image.path,'Ensemble distribution - ', scenario.names[[i]], '.png')
  par(mar = c(2,2,2,0), mgp=c(2,0.5,0)) # , bty="n"
  png(filename = plot.filename, width = 14, height = 25, units = 'cm', res = 900)
  plot(ensemble.dist[[i]], xlim=c(35.14091, 35.86199), ylim=c(31.59407, 33.00494),
       main = "Predicted Birulatus distribution, using an ensemble of 4 modelling approaches", cex.main=0.7) 
  lines(birulatus.study)
  lines(israel.WB, col="grey", lty=5, lwd=2)
  points(bi[bi$occurrence == 1,],col='blue',pch=16, cex=0.7)
  points(bi[bi$occurrence == 0,],col='red',pch=16, cex=0.7)
  lines(groads, col="grey73")
  points(major.cities, pch=21, col='black', bg='yellow', cex=1.3)
  select.towns = large.towns[large.towns$name == "Tiberias" | large.towns$name == "Afula",]
  points(select.towns,        pch=21, col='black', bg='yellow', cex=0.8)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels=major.cities$name, pos=4, cex=0.6, offset=0.3)) 
  with(select.towns, text(select.towns$lat~select.towns$lon, labels=select.towns$name, pos=4, cex=0.6, offset=0.3))
  legend("bottomright",c("Presence (83 observations)", "Absence (98 observations)"), col=c("blue","red"),pch=16,cex=.7)
  
  # alternatively: plot(s.predmaps.rf[[i]], col = c('white','green'), breaks=c(0, s.threshold.rf,1))
  dev.off()
  
  # make raster:
  raster.filename = paste0(B.heavies.spatial.path,'Ensemble distribution - ', scenario.names[[i]], '.tif')
  writeRaster(ensemble.dist[[i]], filename = raster.filename, options=c('TFW=YES'), overwrite= TRUE)
  
  # make unitone version for converting to polygon:
  ensemble.dist.unitone = ensemble.dist[[i]]
  ensemble.dist.unitone[ensemble.dist.unitone >= ensemble.thresholds[[i]]]  <- 1
  writeRaster(ensemble.dist.unitone, 
              filename = paste0(B.heavies.spatial.path,'Ensemble distribution unitone - ',scenario.names[[i]], '.tif'),
              options=c('TFW=YES'), overwrite= TRUE)
  # unsure if the following doesn't work or is just super slow:
  distribution.poly = rasterToPolygons(ensemble.dist.unitone, digits=12, na.rm=TRUE,dissolve=T); plot(distribution.poly)
  
  print(paste(scenario.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

saveRDS(ensemble.thresholds, "./rds/ensemble.thresholds.rds")
saveRDS(ensemble.dist, paste0(B.heavies.rds.path,"ensemble.dist.rds"))
###################################################################################################
# Map pairs of predictions to elucidate data effects ----
myLetters <- letters[1:26] # setting up letter-number correlation. e.g. match("a", myLetters)

# 1) The effect of the unreliable collections records ----
# A vs B, E vs F # list the more comprehensive model first
combos = c("a","b","e","f")
numbers   = list(match(combos[[1]], myLetters), match(combos[[2]], myLetters), 
                 match(combos[[3]], myLetters), match(combos[[4]], myLetters)) # temporary list only
rasters = list() # temporary list only
meanTSS = list() # temporary list only

png(filename = paste0(heavies.image.path,"Schreiberi scenario comparisons - positional uncertainty new.png"), 
    width=30, height=16, units='cm', res=900)
par(mfrow=c(1,4), mar =c(0.25,0,1,0), bty="n")
for (i in 1:length(combos)) {
  rasters[[i]] = raster(paste0(heavies.spatial.path,"Ensemble distribution - Schreiberi Scenario ",
                               toupper(combos[[i]]),".tif"))
  number = numbers[[i]]
  meanTSS[[i]] = round(mean(s.eval.summary[[number]]$TSS[c(1,2,3)]), digits=2)
  plot(rasters[[i]], xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742), bty="n", axes=FALSE, legend=F,
       main=paste(s.6scenario.descriptions[[number]],"TSS=", meanTSS[[i]]), cex.main=0.98)
  lines(borders, lwd=1, col="seashell4")
  plot(schreiberi.buffer, lwd=0.5, add=T)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=T)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
  points(ssp,  col='purple',       pch=16, cex=0.4) 
  points(ssa,  col='pink',         pch=16, cex=0.4) 
  points(sc,   col='deepskyblue2', pch=16, cex=0.4)
  points(sc.r, col='darkblue',     pch=16, cex=0.4) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)"), 
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"), cex=1)}
dev.off()
# conclusion: not a huge difference - just slightly more up the coast in the presence-only models when the unreliables are included, and slightly less down south in the presence-absence models when the unreliables are included.


# 2) Comparing unbalanced models with true absences, with balanced models with pseudoabsences ----
# A vs F, C vs D # listing the more comprehensive model first
combos = c("a","f","c","d")
numbers   = list(match(combos[[1]], myLetters), match(combos[[2]], myLetters),
                 match(combos[[3]], myLetters), match(combos[[4]], myLetters)) # temporary list only
rasters = list() # temporary list only
meanTSS = list() # temporary list only

png(filename = paste0(heavies.image.path,"Schreiberi scenario comparisons - unbalanced PA vs balanced PO.png"), 
    width=30, height=16, units='cm', res=900)
par(mfrow=c(1,4), mar =c(0.25,0,1,0), bty="n")  # mfrow=c(nrows, ncols)
for (i in 1:length(combos)) {
  rasters[[i]] = raster(paste0(heavies.spatial.path,"Ensemble distribution - Schreiberi Scenario "
                               ,toupper(combos[[i]]),".tif"))
  number = numbers[[i]]
  meanTSS[[i]] = round(mean(s.eval.summary[[number]]$TSS[c(1,2,3)]), digits=2)
  plot(rasters[[i]], xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742), bty="n", axes=FALSE, legend=F,
       main=paste(s.6scenario.descriptions[[number]],"TSS=", meanTSS[[i]]), cex.main=0.98)
  lines(borders, lwd=1, col="seashell4")
  plot(schreiberi.buffer, lwd=0.5, add=T)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
  points(ssp,  col='purple',       pch=16, cex=0.4) 
  points(ssa,  col='pink',         pch=16, cex=0.4) 
  points(sc,   col='deepskyblue2', pch=16, cex=0.4)
  points(sc.r, col='darkblue',     pch=16, cex=0.4) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)"), 
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"), cex=1)}
dev.off()
# conclusion: presence-only models miss the south area; possibly because the greater number of (pseudo)absences
# cancels out the effect of the few presences there. But in other areas (more north-east of gaza), PO models perform better.

# 3*) Comparing all 6 scenarios ----
# A vs B, E vs F # list the more comprehensive model first
combos = c("a","b","c","d","e","f")
numbers   = list(match(combos[[1]], myLetters), match(combos[[2]], myLetters), 
                 match(combos[[3]], myLetters), match(combos[[4]], myLetters), 
                 match(combos[[5]], myLetters), match(combos[[6]], myLetters)) # temporary list only
rasters = list() # temporary list only
meanTSS = list() # temporary list only

png(filename = paste0(heavies.image.path,"Schreiberi scenario comparisons - all scenarios.png"), 
    width=45, height=16, units='cm', res=1200)
par(mfrow=c(1,6), mar =c(0.25,0,1,0), bty="n")
for (i in 1:length(combos)) {
  rasters[[i]] = raster(paste0(heavies.spatial.path,"Ensemble distribution - Schreiberi Scenario ",
                               toupper(combos[[i]]),".tif"))
  number = numbers[[i]]
  meanTSS[[i]] = round(mean(s.eval.summary[[number]]$TSS[c(1,2,3)]), digits=2) # TSS rows were c(3,5,9)
  plot(rasters[[i]], xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742), bty="n", axes=FALSE, legend=F,
       main=paste(s.6scenario.descriptions[[number]],"TSS=", meanTSS[[i]]), cex.main=0.98)
  lines(borders, lwd=1, col="seashell4")
  plot(schreiberi.buffer, lwd=0.5, add=T)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=T)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
  points(ssa,  col='pink',         pch=16, cex=0.1) 
  points(ssp,  col='purple',       pch=16, cex=0.1) 
  points(sc,   col='deepskyblue2', pch=16, cex=0.1)
  points(sc.r, col='darkblue',     pch=16, cex=0.1) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)"), 
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"), cex=1)}
dev.off()
# conclusion: .



# 4*) Comparing the 2 noveg scenarios ----
# A vs B # list the more comprehensive model first
combos  = c("a","b")
numbers = list(match(combos[[1]], myLetters), match(combos[[2]], myLetters)) # temporary list only
rasters = list() # temporary list only
meanTSS = list() # temporary list only

png(filename = paste0(heavies.image.path,"schreiberi/Schreiberi scenario comparisons - both noveg scenarios.png"), 
    width=15, height=16, units='cm', res=1200)
par(mfrow=c(1,2), mar =c(0.25,0,1,0), bty="n")
for (i in 1:length(combos)) {
  rasters[[i]] = raster(paste0(heavies.spatial.path,"Ensemble distribution - Schreiberi Scenario ",
                               toupper(combos[[i]]),".tif"))
  number = numbers[[i]]
  meanTSS[[i]] = round(mean(s.eval.summary[[number]]$TSS[c(1,2,3)]), digits=2) # TSS rows were c(3,5,9)
  plot(rasters[[i]], xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742), bty="n", axes=FALSE, legend=F,
       main=paste(s.2scen.descriptions[[number]],"TSS=", meanTSS[[i]]), cex.main=0.65)
  lines(borders, lwd=1, col="seashell4")
  plot(schreiberi.buffer, lwd=0.5, add=T)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=T)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.4))
  points(ssa,  col='pink',         pch=16, cex=0.1) 
  points(ssp,  col='purple',       pch=16, cex=0.1) 
  points(sc,   col='deepskyblue2', pch=16, cex=0.1)
  points(sc.r, col='darkblue',     pch=16, cex=0.1) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)"), 
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"), cex=0.8)}
dev.off()
# conclusion: .

###################################################################################################
# Plot response curves ----

modelset        = model.list.complete
rcurves = list()

i=1 # for some reason this for-loop doesn't produce viable images! but running each iteration manually does.
#for (i in 1:length(scenario.names))       {
filename = paste0(B.heavies.image.path,'Mean response curves - ', scenario.names[[i]], '.png')
title = paste0("Response curves for ", scenario.names[[i]])
rcurves[[i]] = rcurve(modelset[[i]], id=1:4, mean=TRUE, confidence=TRUE, smooth=T, main=title, size=1000)

png(filename = filename, width = 18, height = 8, units = 'cm', res = 600)
par(mar=c(0,0,1,0))
curve = rcurve(modelset[[i]], id=1:4, mean=T, confidence=TRUE, smooth=T, 
               main=scenario.names[[i]], size=1000, main.cex=0.5, gg=T)
curve + theme(axis.text.x = element_blank(), axis.text.y=element_blank(),  
              axis.title.x=element_blank(), legend.position="none", panel.background=element_blank(), 
              panel.grid.major=element_blank(), plot.background=element_blank())
dev.off()                                  
#}

png(filename = paste0(heavies.image.path,"S mean response curves - all scenarios.png"), 
    width=30, height=36, units="cm", res=600)
par(mar = c(0,0,0,0), mgp=c(0,0,0))
plot(0:6, 0:6, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
rasterImage(readPNG(source= paste0(heavies.image.path,'Mean response curves - Schreiberi scenario A.png')), 0,5,6,6)
rasterImage(readPNG(source= paste0(heavies.image.path,'Mean response curves - Schreiberi scenario B.png')), 0,4,6,5)
rasterImage(readPNG(source= paste0(heavies.image.path,'Mean response curves - Schreiberi scenario C.png')), 0,3,6,4)
rasterImage(readPNG(source= paste0(heavies.image.path,'Mean response curves - Schreiberi scenario D.png')), 0,2,6,3)
rasterImage(readPNG(source= paste0(heavies.image.path,'Mean response curves - Schreiberi scenario E.png')), 0,1,6,2)
rasterImage(readPNG(source= paste0(heavies.image.path,'Mean response curves - Schreiberi scenario F.png')), 0,0,6,1)
dev.off()

# note: use getResponseCurve() at https://rdrr.io/cran/sdm/man/response.html to extract the actual data and plot more nicely (time consuming).
# use rcurve(b.model.list.tops[[1]], id=1, smooth = T, main = "whatever") to plot individual models.

saveRDS(rcurves, paste0(heavies.rds.path,"rcurves.rds"))
###################################################################################################
# Plot variable importance ----

# 'how to' notes:
{#getVarImp(m,id=1,wtest='training') # variable importance based on training dataset

#vi <- getVarImp(m,id=1,wtest='test.dep') 

#plot(vi,'auc')

#plot(vi,'cor')
  }

var.importance = getVarImp(modelset[[i]], id=c(1:4), wtest='training', varImportance='tss')
plot(var.importance)

varimportance.rf  = list()
varimportance.brt = list()
varimportance.svm = list()
varimportance.gam = list()
varimportance     = list()
par(mar=c(3,4.5,3,1.5))

for (i in 1:length(scenario.names))                                                               {
  varimportance.rf[[i]]  = getVarImp(modelset[[i]], id=1, wtest='training', varImportance='tss')
  varimportance.brt[[i]] = getVarImp(modelset[[i]], id=2, wtest='training', varImportance='tss')
  varimportance.svm[[i]] = getVarImp(modelset[[i]], id=3, wtest='training', varImportance='tss')
  varimportance.gam[[i]] = getVarImp(modelset[[i]], id=4, wtest='training', varImportance='tss')
  
  varimportance[[i]] = data.frame(variable = c(varimportance.rf[[i]]@variables[1], 
                                               varimportance.rf[[i]]@variables[2], 
                                               varimportance.rf[[i]]@variables[3],
                                               varimportance.rf[[i]]@variables[4], 
                                               varimportance.rf[[i]]@variables[5]),
                                   importance = c(mean(c(varimportance.rf[[i]]@varImportance$AUCtest[1], 
                                                         varimportance.brt[[i]]@varImportance$AUCtest[1],
                                                         varimportance.svm[[i]]@varImportance$AUCtest[1],
                                                         varimportance.gam[[i]]@varImportance$AUCtest[1])),
                                                   mean(c(varimportance.rf[[i]]@varImportance$AUCtest[2], 
                                                         varimportance.brt[[i]]@varImportance$AUCtest[2],
                                                         varimportance.svm[[i]]@varImportance$AUCtest[2],
                                                         varimportance.gam[[i]]@varImportance$AUCtest[2])),
                                                    mean(c(varimportance.rf[[i]]@varImportance$AUCtest[3], 
                                                          varimportance.brt[[i]]@varImportance$AUCtest[3],
                                                          varimportance.svm[[i]]@varImportance$AUCtest[3],
                                                          varimportance.gam[[i]]@varImportance$AUCtest[3])),
                                                    mean(c(varimportance.rf[[i]]@varImportance$AUCtest[4], 
                                                           varimportance.brt[[i]]@varImportance$AUCtest[4],
                                                           varimportance.svm[[i]]@varImportance$AUCtest[4],
                                                           varimportance.gam[[i]]@varImportance$AUCtest[4])),
                                                    mean(c(varimportance.rf[[i]]@varImportance$AUCtest[5], 
                                                           varimportance.brt[[i]]@varImportance$AUCtest[4],
                                                           varimportance.svm[[i]]@varImportance$AUCtest[5],
                                                           varimportance.gam[[i]]@varImportance$AUCtest[5]))))

  order = (rev(c(2,4,1,5,3)))  # Get order of rows as determined by overall average importance (below) # removed rev()
  varimportance[[i]] <- varimportance[[i]][order,]      # sort

  title    = paste0("Variable importance for the ", scenario.names[[i]])
  filename = paste0(B.heavies.image.path,'Variable importance - ', scenario.names[[i]], '.png')
  png(filename = filename, width = 18, height = 10, units = 'cm', res = 600)
  # par(mar = c(5.1, 4.1, 4.1, 2.1)) # the default.
  par(mar = c(3, 8.5, 3, 0)) # sets the bottom, left, top and right margins respectively, in number of lines of text.
  barplot(varimportance[[i]]$importance, 
        names.arg = varimportance[[i]]$variable, xlab = "Variable importance averaged across the four top models",
        main=title, horiz=TRUE, cex.names=0.8, las=1, mgp=c(3, 0.2, 0), tck=-0.008, cex.main=1.2, 
        col='lightgreen', xlim=c(0,0.28)) 
  dev.off()                                                                                                     }

saveRDS(varimportance.rf,  paste0(heavies.rds.path, "s.varimportance.rf.rds"))
saveRDS(varimportance.svm, paste0(heavies.rds.path, "s.varimportance.svm.rds"))
saveRDS(varimportance.gam, paste0(heavies.rds.path, "s.varimportance.gam.rds"))
saveRDS(varimportance,     paste0(heavies.rds.path, "s.varimportance.rds"))

# Total variable importance, averaged over all models ----
#varimportance[[1]]@varImportance$AUCtest[1] 
#v=1
#varimportance[[1]]@variables[v]

var.importance.allcombos = list()

for (v in 1:6)          {     # manually replaced the 'length(varimportance[[1]]$variable)' with 6 as it still gave 7.
  
  var = varimportance[[1]]$variable[[v]] # name of variable
  
  var.importance.allcombos[[v]] = data.frame(Variable = var,
             scenario1 = varimportance[[1]]$importance[v], # '1' is the scenario, 'v' refers to variable in question
             scenario2 = varimportance[[2]]$importance[v]
#             ,                                                 # adjusting the original 6-scenario code for 2 scens.
#             scenario3 = varimportance[[3]]$importance[v],
#             scenario4 = varimportance[[4]]$importance[v],
#             scenario5 = varimportance[[5]]$importance[v],
#             scenario6 = varimportance[[6]]$importance[v]
             )    }

var.importance.allcombos.df = do.call("rbind", var.importance.allcombos)
var.importance.allcombos.df$mean = rowMeans(var.importance.allcombos.df[,c("scenario1","scenario2")] )

order <- order(var.importance.allcombos.df$mean)              # Get order of rows, for plotting in order
var.importance.allcombos.df <- var.importance.allcombos.df[order,]      # sort

png(paste0(heavies.image.path,"schreiberi/S variable importance - averaged across both nonveg.png"), 
           width = 18, height = 10, units = 'cm', res = 600)
# par() #default is 5.1 4.1 4.1 2.1
par(mar=c(3,4.5,3,1.5))
barplot(var.importance.allcombos.df$mean, 
        names.arg = var.importance.allcombos.df$Variable, xlab = "Variable importance averaged across all models",
        main="Averaged variable importance", horiz=TRUE, cex.names=0.8, las=1, 
        mgp=c(3, 0.2, 0), tck=-0.008, cex.main=1.6, col='lightblue')
dev.off()

saveRDS(var.importance.allcombos,    "./rds_objects/s.var.importance.rds")
saveRDS(var.importance.allcombos.df, "./rds_objects/s.var.importance.df.rds")

###################################################################################################
# The grand ensemble, etc. ----

# To reduce code duplication, this section has been deleted; 
# please refer to the corresponding section in sdm.8a.finalmodel.beershebensis

###################################################################################################
# Extra stuff that might come in handy one day -----

# Studies have shown that predictions or projections by alternative models can be so variable that challenge the common practice of relying on one single method. A solution is to utilize several models (‘ensembles’) and use appropriate techniques to explore the resulting range of projections. Signiﬁcant improvements on the robustness of a forecast can be achieved if an ensemble approach is used and the results analysed appropriately.
# In the sdm package, the ensemble function can be used to generate an ensemble prediction or forecast based on the multiple models that are used in the sdm function. Several methods are implemented and can be used by a user in a ﬂexible way. Here is an example: 

# in the following, we predict the habitat suitability using the ensemble function # since the newdata is a raster object, the output is also a raster object
# ensemble based on a Weighted averaging that is weighted using AUC statistic 
e1 <- ensemble(m1,newdata=preds,filename='e1.img',setting=list(method='weighted',stat='AUC'))
plot(e1)

# ensemble based on a Weighted averaging that is weighted using TSS statistic with threshold criterion number 
e2 <- ensemble(m2,newdata=preds,filename='e2.img',setting=list(method='weighted',stat='TSS',opt=2))
e2
plot(e2)

# ensemble based on an Unweighted averaging 
e3 <- ensemble(m2,newdata=preds,filename='e3.img',setting=list(method='unweighted'))
plot(e3)

# to create an ensemble based on evaluation figure:
# You can find out which models have the TSS > 0.7 and get their modelID (you can use getEvaluation function to get the statistics for all modelIDs and extract the id of those that meet your condition). Then, in the setting of the ensemble function, you can add the argument id to which you give the modelD of the models you want to participate in the ensemble procedure.

me <- getEvaluation(m)

id <- me$modelID[me$TSS > 0.7]

en <- ensemble(m, preds, filename='file.tif', setting=list(method='weighted', stat='TSS', opt=2, id=id))

