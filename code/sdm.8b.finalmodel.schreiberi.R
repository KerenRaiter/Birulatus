# Final Schreiberi model: No veg, only 2 scenarios (with and without unreliable data ----

# This script follows on from 6b and 7b and synthesises their direction. It includes code from scripts 4,5 & 6.

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

# load data:
heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

s.preds.noveg      = readRDS(paste0(heavies.rds.path,"s.preds.noveg.rds"))
s.data.packs.noveg = readRDS(paste0(heavies.rds.path,"s.data.packs.noveg.rds"))

s.eval.list.noveg       = readRDS("./rds_objects/s.eval.list.noveg.rds")   # 'raw' list of full eval data from each combo
s.eval.summary.noveg    = readRDS("./rds_objects/s.eval.summary.noveg.rds")  # consolidated list (multi reps averaged)
s.eval.summary.df.noveg = readRDS("./rds_objects/s.eval.summary.df.novegs.rds") # consolidated table (all combos together)

# define map limits for plotting later:
xlims.s      = c(34.27173, 35.3248)  # for plotting whole study area
ylims.s      = c(31.12667, 33.10742) # for plotting whole study area
xlims.s.dist = c(34.27179, 35.32475) # for plotting distributions
ylims.s.dist = c(31.12669, 33.10753) # for plotting distributions

# spatial data:
schreiberi.buffer    = readRDS("./rds_objects/schreiberi.buffer.rds")
par(mar=c(0,0,0,0))
plot(schreiberi.buffer, add=TRUE)

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
landuse_s                     = readRDS(paste0(heavies.rds.path,"landuse_s.rds"))
natreserve_no_firing          = readRDS(paste0(heavies.rds.path,"natreserve_no_firing.rds"))
natreserve_firing_intersect   = readRDS(paste0(heavies.rds.path,"natreserve_firing_intersect.rds"))
natpark_no_firing             = readRDS(paste0(heavies.rds.path,"natpark_no_firing.rds"))
natpark_firing_intersect      = readRDS(paste0(heavies.rds.path,"natpark_firing_intersect.rds"))
firing_no_natcons             = readRDS(paste0(heavies.rds.path,"firing_no_natcons.rds"))
firing_excl_dist_l            = readRDS(paste0(heavies.rds.path,"firing_excl_dist_l.rds"))
firing_excl_alllanduse        = readRDS(paste0(heavies.rds.path,"firing_excl_alllanduse.rds"))

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

# observations versions with full data :
ss.full    = readRDS("./rds_objects/ss.full.rds")   # all schreiberi survey data (errors removed)
ssp.full   = readRDS("./rds_objects/ssp.full.rds")  # presences
ssa.full   = readRDS("./rds_objects/ssa.full.rds")  # absences
sc.full    = readRDS("./rds_objects/sc.full.rds")   # s = schreiberi, c = collections
sc.nodups  = readRDS(paste0(heavies.rds.path, "sc.nodups.rds")) # from script 2b; elsa calc. Duplicates removed.
sc.r.full  = readRDS(paste0(heavies.rds.path, "s.collections.reliables.rds"))  # from script 2b: elsa calc
writeOGR(obj = sc.r.full, dsn="E:/GIS working/layers/shnuniot", layer="sc.r.full", driver = "ESRI Shapefile")

# consolidated observations, with all extraneous attributes removed:
ssp  = readRDS ("./rds_objects/ssp.rds")
ssa  = readRDS ("./rds_objects/ssa.rds")
sc   = readRDS ("./rds_objects/sc.rds")
sc.r = readRDS ("./rds_objects/sc.r.rds")

#######################################################################################################################
# Summary of outputs ----

grand.ensemble = raster(paste0(heavies.spatial.path,"s.grand.ensemble.noveg.tif"))

#######################################################################################################################
# The grand ensemble ----

# make grand ensemble by averaging predictions using raster calc mean function:
ensemble.stack = stack(s.ensembles)
grand.ensemble = overlay(ensemble.stack, fun=mean)  # takes many minutes
# or load previously-made version:
# grand.ensemble = raster(paste0(heavies.spatial.path,"s.grand.ensemble.noveg.tif"))
# raster() function creates a RasterLayer object from scratch, a file, an Extent object, a matrix, an 'image' object etc.
plot(grand.ensemble)
writeRaster(grand.ensemble, filename=paste0(heavies.spatial.path,"s.grand.ensemble.noveg.tif"), 
            options=c('TFW = YES'), overwrite= T)

modelset[[1]]@models # this gives lots of model info...
modelset[[1]]@models$occurrence$gam$`1`@evaluation$training@threshold_based

hist(grand.ensemble[])
summary(grand.ensemble[])
min(grand.ensemble[], na.rm=T)
max(grand.ensemble[], na.rm=T)
grand.ensemble[grand.ensemble < 0] = 0
grand.ensemble[grand.ensemble > 1] = 1

png(filename = paste0(heavies.image.path,"S Grand ensemble noveg.png"), width=16, height=22, units='cm', res=900)
par(mar=c(1,1,2,1), bty="n")
plot(grand.ensemble, xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742), 
     main="Schreiberi Grand Ensemble (noveg)", cex.main=1.5, bty="n", axes=FALSE)
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
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"), cex=1)
 # plot(kkl.plan, col=NA, border='darkblue', add=T) #; plot(kkl.mgmt, col=NA, border="lightblue", add=T)
 # plot(nat.res, col = NA, border='darkgreen', add=T)
dev.off()

# The grand threshold ----

weights = c(s.eval.summary.noveg[[1]]$TSS[s.eval.summary.noveg[[1]]$method == "gam"],
            s.eval.summary.noveg[[1]]$TSS[s.eval.summary.noveg[[1]]$method == "rf" ],
            s.eval.summary.noveg[[1]]$TSS[s.eval.summary.noveg[[1]]$method == "svm"],
            s.eval.summary.noveg[[2]]$TSS[s.eval.summary.noveg[[2]]$method == "gam"],
            s.eval.summary.noveg[[2]]$TSS[s.eval.summary.noveg[[2]]$method == "rf" ],
            s.eval.summary.noveg[[2]]$TSS[s.eval.summary.noveg[[2]]$method == "svm"])

thresholds = c(s.eval.summary.noveg[[1]]$threshold.mss[s.eval.summary.noveg[[1]]$method == "gam"],
               s.eval.summary.noveg[[1]]$threshold.mss[s.eval.summary.noveg[[1]]$method == "rf" ],
               s.eval.summary.noveg[[1]]$threshold.mss[s.eval.summary.noveg[[1]]$method == "svm"],
               s.eval.summary.noveg[[2]]$threshold.mss[s.eval.summary.noveg[[2]]$method == "gam"],
               s.eval.summary.noveg[[2]]$threshold.mss[s.eval.summary.noveg[[2]]$method == "rf" ],
               s.eval.summary.noveg[[2]]$threshold.mss[s.eval.summary.noveg[[2]]$method == "svm"])

weighted.mean(c(1,5,8), c(1,2,11))  
grand.threshold = weighted.mean(thresholds, weights)

# The grand distribution ----
grand.distribution = grand.ensemble
names(grand.distribution) = "s.grand.distribution noveg"
grand.distribution[grand.distribution < grand.threshold] <- NA
writeRaster(grand.distribution, 
            filename = paste0(heavies.spatial.path,"s.grand.distribution.noveg.tif"), options=c('TFW=YES'), overwrite=T)

distribution.unitone = grand.ensemble
distribution.unitone[distribution.unitone < grand.threshold] <- NA
distribution.unitone[distribution.unitone >= grand.threshold] <- 1
# writeRaster(distribution.unitone, filename=paste0(heavies.spatial.path,"s.grand.distribution.unitone"),overwrite=T)
plot(distribution.unitone)

# Plotting clean grand distribution ----
png(filename = paste0(heavies.image.path,"S Grand_distribution.clean noveg.png"), width=16, height=22, units='cm', res=900)
par(mfrow = c(1,1), mar=c(1,1,1,1), bty="n")
plot(grand.distribution, xlim=xlim, ylim=ylim, bty="n", axes=FALSE)
dev.off()

# Plotting grand distribution with reference data ----
png(filename = paste0(heavies.image.path,"S Grand_distribution.refdata.png"), width=16, height=22, units='cm',res=900)
par(mar=c(1,1,1,1), bty="n")
plot(grand.distribution, col="green", xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742), bty="n", axes=F, legend=F)
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
# legend("bottomleft", 
#        c("Agriculture", "Built-up areas and industry", "Military", 
#          "Plantation","KKL management", "Nature reserve","National Park"),
#        pch=22, pt.cex=1.5, text.font=1, col=c(NA,NA,NA,NA,"blue","darkgreen","green"),
#        pt.bg=c("burlywood","darkgrey","thistle3","darksalmon",NA,NA,NA))
dev.off()

# # PA-only grand distribution ----
{
# PA.ensemble.stack = stack(s.ensembles[[1]], s.ensembles[[2]], s.ensembles[[3]])
# grand.ensemble.PA = overlay(PA.ensemble.stack, fun=mean)  # started 3:37 takes ~20 minutes
# PA.grand.threshold = mean(do.call("rbind", s.ensemble.thresholds)[1:3,]) # check this is correct
# PA.grand.distribution = grand.ensemble.PA
# names(PA.grand.distribution) = "Schreiberi PA grand.distribution"
# PA.grand.distribution[PA.grand.distribution < PA.grand.threshold] <- NA
# plot(PA.grand.distribution)
# 
# png(filename = paste0(heavies.image.path,"S Grand_distribution.PA.refdata.png"), width=16, height=22, units='cm',res=900)
# par(mar=c(1,1,1,1), bty="n")
# plot(PA.grand.distribution, col="green", xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742), bty="n", axes=F,legend=F)
#  lines(borders, lwd=1, col="seashell4")
#  plot(schreiberi.buffer, lwd=0.5, add=T)
#  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
#  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
#  points(ssp,  col='purple',       pch=16, cex=0.3) 
#  points(ssa,  col='pink',         pch=16, cex=0.3) 
#  points(sc,   col='deepskyblue2', pch=16, cex=0.3)
#  points(sc.r, col='darkblue',     pch=16, cex=0.3) 
#  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
#                     "Reliable collections data (63)", "Unreliable collections data (30)"), 
#                     pch=16, col=c("purple","pink","darkblue","deepskyblue2"),pt.cex = 0.4, cex=1)
# dev.off()
}

# Grand distribution with standard deviation expansion ----
grand.ensemble # from above
grand.treshold.minus.sd = grand.threshold - sd(thresholds)

grand.distribution.sd.expand = grand.ensemble
grand.distribution.sd.expand[grand.distribution.sd.expand < grand.treshold.minus.sd] <- NA 
names(grand.distribution.sd.expand) = "grand.distribution.sd.expand"

plot(grand.distribution.sd.expand, col="red",   legend=F)
plot(grand.distribution,           col="chartreuse4", legend=F, add=T)
# lowersd version has a few additional cells... but not a massive difference.

plot.withreference.data(grand.distribution.sd.expand)
{
  par(mar=c(1,1,1,0), bty="n")
  plot(grand.distribution.sd.expand, col="lightgreen",  xlim=xlim, ylim=ylim, bty="n", axes=F,legend=F)
  plot(grand.distribution,           col="chartreuse4", add=T, legend=F)
  lines(borders, lwd=1, col="seashell4")
  plot(schreiberi.buffer, lwd=0.5, add=T)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
  points(ssa,  col='pink',         pch=16, cex=0.3) 
  points(ssp,  col='purple',       pch=16, cex=0.3) 
  points(sc,   col='deepskyblue2', pch=16, cex=0.3)
  points(sc.r, col='darkblue',     pch=16, cex=0.3) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)"), 
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"),pt.cex = 0.4, cex=1)
  
  png(filename = paste0(heavies.image.path,"S ", deparse(substitute(grand.distribution.sd.expand)), ".png"), 
      width=16, height=22, units='cm',res=900)
  par(mar=c(1,1,1,1), bty="n")
  plot(grand.distribution.sd.expand, col="lightgreen", xlim=xlim, ylim=ylim, bty="n", axes=F,legend=F)
  plot(grand.distribution,           col="chartreuse4", add=T, legend=F)
  lines(borders, lwd=1, col="seashell4")
  plot(schreiberi.buffer, lwd=0.5, add=T)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
  points(ssa,  col='pink',         pch=16, cex=0.3) 
  points(ssp,  col='purple',       pch=16, cex=0.3) 
  points(sc,   col='deepskyblue2', pch=16, cex=0.3)
  points(sc.r, col='darkblue',     pch=16, cex=0.3) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)","Predicted distribution",
                    "Area of potential distribution (sd)"), 
                    pch=c(16,16,16,16,21,21), col=c("purple","pink","darkblue","deepskyblue2","chartreuse4","lightgreen"),pt.cex = 0.4, cex=1)
  dev.off()
}

# Grand distribution expanded by standard error on threshold ----

grand.threshold.minus.se = grand.threshold - sd(thresholds)/sqrt(length(thresholds))

grand.distribution.se.expand = grand.ensemble
grand.distribution.se.expand[grand.distribution.se.expand < grand.threshold.minus.se] <- NA 
names(grand.distribution.se.expand) = "grand.distribution.se.expand"

plot(grand.distribution.sd.expand, col="blue",   legend=F)
plot(grand.distribution.se.expand, col="red",   legend=F, add=T)
plot(grand.distribution,           col="chartreuse4", legend=F, add=T)
# lowerse version has a few additional cells, and a few less than the sd.expand... but not a massive difference.
# plot:
{
  par(mar=c(1,1,1,0), bty="n")
  plot(grand.distribution.se.expand, col="lightgreen",  xlim=xlim, ylim=ylim, bty="n", axes=F,legend=F)
  plot(grand.distribution,           col="chartreuse4", add=T, legend=F)
  lines(borders, lwd=1, col="seashell4")
  plot(schreiberi.buffer, lwd=0.5, add=T)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
  points(ssa,  col='pink',         pch=16, cex=0.3) 
  points(ssp,  col='purple',       pch=16, cex=0.3) 
  points(sc,   col='deepskyblue2', pch=16, cex=0.3)
  points(sc.r, col='darkblue',     pch=16, cex=0.3) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)"), 
                    pch=16, col=c("purple","pink","darkblue","deepskyblue2"),pt.cex = 0.4, cex=1)
  
  png(filename = paste0(heavies.image.path,"S ", deparse(substitute(grand.distribution.se.expand)), ".png"), 
      width=16, height=22, units='cm',res=900)
  par(mar=c(1,1,1,1), bty="n")
  plot(grand.distribution.se.expand, col="lightgreen", xlim=xlim, ylim=ylim, bty="n", axes=F,legend=F)
  plot(grand.distribution,           col="chartreuse4", add=T, legend=F)
  lines(borders, lwd=1, col="seashell4")
  plot(schreiberi.buffer, lwd=0.5, add=T)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos=2, cex=0.8, offset=0.3))
  points(ssa,  col='pink',         pch=16, cex=0.3) 
  points(ssp,  col='purple',       pch=16, cex=0.3) 
  points(sc,   col='deepskyblue2', pch=16, cex=0.3)
  points(sc.r, col='darkblue',     pch=16, cex=0.3) 
  legend("topleft", title = "Observational data", c("Survey presence (723)", "Survey absence (99)",
                    "Reliable collections data (63)", "Unreliable collections data (30)","Predicted distribution",
                    "Area of potential distribution (sd)"), 
                    pch=c(16,16,16,16,21,21), col=c("purple","pink","darkblue","deepskyblue2","chartreuse4","lightgreen"),pt.cex = 0.4, cex=1)
  dev.off()
}

###################################################################################################
# Mapping and calculating landuse impacts ----

# I couldn't, at the time, find a straightforward way to do this in R. So exported to ArcGIS and cropped there.
distribution.poly = rasterToPolygons(distribution.unitone, digits=12, na.rm=TRUE, dissolve=T) # takes forever
names(distribution.poly)    = "s.distr"
saveRDS(distribution.poly,  paste0(heavies.rds.path,"s.distribution.poly.rds"))
distribution.poly = readRDS(paste0(heavies.rds.path,"s.distribution.poly.rds")) # here's one I made earlier
plot(distribution.poly, col="green")
shapefile(distribution.poly, paste0(heavies.spatial.path,"s.distr.poly.shp"), overwrite=TRUE) # worked. Writes shapefile
# writeOGR(distribution.poly, heavies.spatial.path, driver = "ESRI Shapefile", layer = "s.distr.poly.shp") # error

distribution.sd.poly = rasterToPolygons(grand.distribution.sd.expand, digits=12, na.rm=TRUE, dissolve=T) # takes forever
names(distribution.sd.poly) = "s.distr.sd"
saveRDS(distribution.sd.poly, paste0(heavies.rds.path,  "s.distribution.sd.poly.rds"))
distribution.sd.poly = readRDS(paste0(heavies.rds.path, "s.distribution.sd.poly.rds")) # here's one I made earlier
plot(distribution.sd.poly, col="lightgreen")
shapefile(distribution.sd.poly, paste0(heavies.spatial.path,"b.distr.sd.poly.shp"), overwrite=TRUE) # Writes shapefile

# Now go to ArcGIS to crop, map, and calculate
