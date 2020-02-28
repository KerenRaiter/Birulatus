# Ensemble forecasting for schreiberi ----

###################################################################################################
# Housekeeping ----

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

# define map limits for plotting later:
xlims.s      = c(34.27173, 35.3248)  # for plotting whole study area
ylims.s      = c(31.12667, 33.10742) # for plotting whole study area
xlims.s.dist = c(34.27179, 35.32475) # for plotting distributions
ylims.s.dist = c(31.12669, 33.10753) # for plotting distributions

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

# builtup                = readRDS(paste0(heavies.rds.path,"builtup.rds")); plot(builtup, col=builtup@data$landuse)
# agriculture            = readRDS(paste0(heavies.rds.path,"agriculture.rds"))
# dmt_uncat              = readRDS(paste0(heavies.rds.path,"dmt_uncat.rds"))
# dmt_bu_ag_plntn        = readRDS(paste0(heavies.rds.path,"dmt_bu_ag_plntn.rds"))
# INPA_dist              = readRDS(paste0(heavies.rds.path,"INPA_dist.rds"))
# KKL_ops                = readRDS(paste0(heavies.rds.path,"KKL_ops.rds"))
# military               = readRDS(paste0(heavies.rds.path,"military.rds"))
# rail                   = readRDS(paste0(heavies.rds.path,"rail.rds"))
# osm_dist               = readRDS(paste0(heavies.rds.path,"osm_dist.rds"))
# 
# disturbed.raw.layers = list(builtup,agriculture, dmt_uncat, dmt_bu_ag_plntn,
#                             INPA_dist, KKL_ops, military, rail, osm_dist)

plot(kkl.plan, col="pink", border="pink", xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742))
plot(kkl.ops,  col="lightblue", border="lightblue", add=T)
plot(kkl.mgmt, col="blue", border="blue", add=T)
plot(nat.park, col='darkgreen',  bg='darkgreen', add=T)
plot(nat.res,  col='lightgreen', bg='darkgreen', add=T)
plot(borders, xlim=c(34.3,35.4), ylim=c(30.8,31.6), add=T)
lines(groads, col="grey73")
plot(villages, pch=21, bg='blue', cex=0.9, add=T)
plot(towns, pch= 21, bg= 'green', cex = 0.9, add=T)
plot(small.cities, pch=21, bg='orange', cex=1.4, add=T)
plot(major.cities, pch=21, bg='yellow', cex=1.8, add=T)
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8))

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
# Create models for each combo, just with the top three algorithms ----
# or load previously made one:
# s.models.tops = s.models.complete  # or:
# s.models.tops = readRDS(paste0(heavies.rds.path,"s.models.complete.rds"))

s.models.complete.noveg = list() # for schreiberi no veg, svm performed too poorly so was kicked out.
scenario.names = s.2scen.scen.names

for (i in 1:length(scenario.names))                                                                          {
  start_time = Sys.time();  print(s.6scen.scen.names[i])
  data = s.noveg.data.packages[[i]]
  s.models.complete.noveg[[i]] = sdm(occurrence ~ ., data = data, methods =c('rf','gam','svm'))
  print(paste(scenario.names[[i]]," loop took ", difftime(Sys.time(),start_time, units="mins")," minutes"))  }

saveRDS(s.models.complete.noveg,  paste0(heavies.rds.path,"s.models.complete.noveg.rds"))

###################################################################################################
# Run ensemble loop (within-scenario ensembles) ----
# s.ensembles = readRDS(paste0(heavies.rds.path,"s.ensembles.rds")) # or load previously-made version

s.ensembles    = list()
modelset       = s.models.complete.noveg
scenario.names = s.2scen.scen.names
s.eval.summary = readRDS("./rds_objects/s.eval.summary.noveg.rds")    # consolidated: multiple reps averaged

for (i in 1:length(scenario.names))                                                                         { #~3 hours?
  start.time = Sys.time();  print(scenario.names[i])
  weights = c(s.eval.summary[[i]]$TSS[s.eval.summary[[i]]$method == "gam"],
              s.eval.summary[[i]]$TSS[s.eval.summary[[i]]$method == "rf" ],
              s.eval.summary[[i]]$TSS[s.eval.summary[[i]]$method == "svm"])
  filename = paste0(heavies.spatial.path,'S ensemble - ', scenario.names[[i]], 'noveg.tif')
  s.ensembles[[i]] <- ensemble(modelset[[i]], newdata = s.preds, filename = filename, 
                               setting = list(method = 'weighted', weights = weights), 
                               nc=20, format="GTiff", overwrite= TRUE)  
  print(paste(scenario.names[[i]]," loop took ", difftime(Sys.time(), start.time, units="mins")," minutes")) }

saveRDS(s.ensembles, paste0(heavies.rds.path, "s.ensembles.noveg.rds"))
emailme()

###################################################################################################
# Map ensemble distributions (within-scenario ensembles) ----

# here 'prediction' = prediction with full range of probabilities; distribution = above-threshold.
s.ensemble.dist       = list()
s.ensemble.thresholds = list()

for (i in 1:length(scenario.names)) {   # takes ~ 3 minutes
  
  start.time = Sys.time()
  
  # get threshold:
  s.ensemble.thresholds[[i]] = mean(s.eval.summary[[i]][1, "threshold.mss"], 
                                    s.eval.summary[[i]][2, "threshold.mss"],
                                    s.eval.summary[[i]][3, "threshold.mss"])
  
  # get distribution:
  s.ensemble.dist[[i]] = s.ensembles[[i]]
  s.ensemble.dist[[i]][s.ensemble.dist[[i]] <  s.ensemble.thresholds[[i]]]  <- 0
  s.ensemble.dist[[i]][s.ensemble.dist[[i]] >= s.ensemble.thresholds[[i]]]  <- 1 
  # can leave above step out, to keep the variability of probabilities above the threshold.
  
  # make histogram:
  hist.filename  = paste0(heavies.image.path,'Ensemble histogram - ', scenario.names[[i]], ' noveg.png')
  png(filename = hist.filename, width = 18, height = 15, units = 'cm', res = 300)
  hist(s.ensembles[[i]]);  points(x = s.ensemble.thresholds[[i]], y=0, pch=24, bg='red')
  dev.off()
  
  # make distribution image:
  plot.filename  = paste0(heavies.image.path,'Ensemble distribution - ', scenario.names[[i]], ' noveg.png')
  par(mar = c(2,2,0,0), mgp=c(2,0.5,0)) # , bty="n"
  png(filename = plot.filename, width = 14, height = 25, units = 'cm', res = 900)
  plot(s.ensemble.dist[[i]], xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742),legend=F) 
  # alternatively: plot(s.predmaps.rf[[i]], col = c('white','green'), breaks=c(0, s.threshold.rf,1))
  dev.off()
  
  # make raster:
  raster.filename = paste0(heavies.spatial.path,'Ensemble distribution - ', scenario.names[[i]], ' noveg.tif')
  writeRaster(s.ensemble.dist[[i]], filename = raster.filename, options=c('TFW=YES'), overwrite= TRUE)
  
  print(paste(scenario.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

saveRDS(s.ensemble.thresholds, "./rds_objects/s.ensemble.thresholds.noveg.rds")
saveRDS(s.ensemble.dist, paste0(heavies.rds.path,"s.ensemble.dist.noveg.rds"))
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

scenario.names  = s.2scen.scen.names
modelset        = s.models.complete.noveg
s.rcurves = list()

i=2 # for some reason this for-loop doesn't produce viable images! but running each iteration manually does.
#for (i in 1:length(scenario.names))       {
filename = paste0(heavies.image.path,'schreiberi/Mean response curves - ', scenario.names[[i]], '.png')
title = paste0("Response curves for ", scenario.names[[i]])
s.rcurves[[i]] = rcurve(modelset[[i]], id=1:3, mean=TRUE, confidence=TRUE, smooth=T, main=title, size=1000)

png(filename = filename, width = 18, height = 5, units = 'cm', res = 600)
par(mar=c(0,0,1,0))
curve = rcurve(modelset[[i]], id=1:3, mean=T, confidence=TRUE, smooth=T, 
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

saveRDS(s.rcurves, paste0(heavies.rds.path,"s.rcurves.rds"))
###################################################################################################
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

for (i in 1:length(scenario.names))                                                               {
  varimportance.rf[[i]]  = getVarImp(modelset[[i]], id=1, wtest='training', varImportance='tss')
  varimportance.svm[[i]] = getVarImp(modelset[[i]], id=2, wtest='training', varImportance='tss')
  varimportance.gam[[i]] = getVarImp(modelset[[i]], id=3, wtest='training', varImportance='tss')
  
  varimportance[[i]] = data.frame(variable = c(varimportance.rf[[i]]@variables[1], 
                                               varimportance.rf[[i]]@variables[2], 
                                               varimportance.rf[[i]]@variables[3],
                                               varimportance.rf[[i]]@variables[4], 
                                               varimportance.rf[[i]]@variables[5], 
                                               varimportance.rf[[i]]@variables[6], 
                                               varimportance.rf[[i]]@variables[7]),
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
                                                          varimportance.gam[[i]]@varImportance$AUCtest[6])),
                                                    mean(c(varimportance.rf[[i]]@varImportance$AUCtest[7], 
                                                          varimportance.svm[[i]]@varImportance$AUCtest[7],
                                                          varimportance.gam[[i]]@varImportance$AUCtest[7]))))

  order = (c(5,4,3,6,1,2,7))  # Get order of rows as determined by overall average importance (below) # removed rev()
  varimportance[[i]] <- varimportance[[i]][order,]      # sort

  title    = paste0("Variable importance for ", scenario.names[[i]])
  filename = paste0(heavies.image.path,'schreiberi/Variable importance - ', scenario.names[[i]], '.png')
  png(filename = filename, width = 18, height = 10, units = 'cm', res = 600)
  # par(mar = c(5.1, 4.1, 4.1, 2.1)) # the default.
  par(mar = c(3, 4.5, 3, 1.5)) # sets the bottom, left, top and right margins respectively, in number of lines of text.
  barplot(varimportance[[i]]$importance, 
        names.arg = varimportance[[i]]$variable, xlab = "Variable importance averaged across the three top models",
        main=title, horiz=TRUE, cex.names=0.8, las=1, mgp=c(3, 0.2, 0), tck=-0.008, cex.main=1.6, col='lightgreen', 
        xlim=c(0,0.35)) 
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

