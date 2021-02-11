# Ensemble forecasting for beershebensis ----

###################################################################################################
# Housekeeping ----

# load packages:
x<-c("sdm","usdm","raster","rgdal","tidyverse","png","beepr","xlsx","mailR","parallel")
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

#  Beershebensis data:
# b.model.list.complete  = readRDS("./rds_objects/b.model.list.complete.rds") 
b_data_packages        = readRDS("./rds_objects/b_data_packages.rds")
eval.list              = readRDS("./rds_objects/eval.list.5fold.100reps.rds")  # 'raw' list of eval data from each model
eval.summary           = readRDS("./rds_objects/eval.summary.5fold.100reps.rds")  # consolidated list form
eval.summary.df        = do.call("rbind", eval.summary) # converting the list to a single dataframe for easy plotting

b_package_names        = readRDS("./rds_objects/b_package_names.rds")
b.preds                = readRDS(paste0(heavies.rds.path,"b.preds.rds")); plot(b.preds) # raster stack, 6 layers  
b.combo.descriptions    = readRDS("./rds_objects/b.combo.descriptions.rds")

beershebensis.buffer = readRDS("./rds_objects/beershebensis.buffer_incWB.rds"); 
plot(beershebensis.buffer, xlim=c(34.267,35.39774), ylim=c(30.50798,31.72056))

bs.full  = readRDS("./rds_objects/bs.full.rds")    # b = beershebensis, s = surveys
bsp.full = readRDS("./rds_objects/bsp.full.rds")  # presences
bsa.full = readRDS("./rds_objects/bsa.full.rds")  # absences
bc.full  = readRDS("./rds_objects/bc.full.rds")   # b = beershebensis, c = collections
bs       = readRDS("./rds_objects/b.surveys.rds")
bc       = readRDS("./rds_objects/b.collections.rds")
bsp      = readRDS("./rds_objects/b.surveys.present.rds")
bsa      = readRDS("./rds_objects/b.surveys.absent.rds")
bc.r     = readRDS("./rds_objects/b.collections.reliables.rds")
bsa.r    = readRDS("./rds_objects/b.surveys.absence.reliables.rds")
plot(bs); points(bc); points(bsp); points(bsa); points(bc.r); points(bsa.r)

# Schreiberi data:
s.raster.list.names = list("Rain", "Jant", "Jult", "TWet", "Slop", "Soil", "Vegt")
s.raster.list       = readRDS(paste0(heavies.rds.path,"s.raster.list.rds"))
s.preds             = readRDS(paste0(heavies.rds.path,"s.preds.rds")) # raster stack

s.6scen.scen.names             = readRDS("./rds_objects/s.6scen.scen.names.rds")
s.6scenario.descriptions       = readRDS("./rds_objects/s.6scenario.descriptions.rds")
# s.6scen.obs.packages           = readRDS("./rds_objects/s.6scen.obs.packages.rds")
s.6scen.data.packages          = readRDS("./rds_objects/s.6scen.data.packages.rds")

schreiberi.buffer              = readRDS("./rds_objects/schreiberi.buffer.rds"); plot(schreiberi.buffer)
plot(schreiberi.buffer, xlim=c(34.27173, 35.3248), ylim=c(31.12667, 33.10742))

ss.full    = readRDS("./rds_objects/ss.full.rds")   # all schreiberi survey data (errors removed)
ssp.full   = readRDS("./rds_objects/ssp.full.rds")  # presences
ssa.full   = readRDS("./rds_objects/ssa.full.rds")  # absences
sc.full    = readRDS("./rds_objects/sc.full.rds")   # s = schreiberi, c = collections
sc.nodups  = readRDS(paste0(heavies.rds.path,"sc.nodups.rds"))                # from script 2b; elsa calc
sc.r       = readRDS(paste0(heavies.rds.path,"s.collections.reliables.rds"))  # from script 2b: elsa calc

s.eval.summary    = readRDS("./rds_objects/s.eval.summary.5fold.100reps.rds")    # consolidated list form

# spatial data:
borders                = readRDS("./rds_objects/borders.rds")
major.cities           = readRDS("./rds_objects/major.cities.rds")
small.cities           = readRDS("./rds_objects/small.cities.rds")
towns                  = readRDS("./rds_objects/towns.rds")
villages               = readRDS("./rds_objects/villages.rds")
groads                 = readRDS("./rds_objects/groads.rds")

nat.res                = readRDS(paste0(heavies.spatial.path,"nat.res.rds"))
nat.park               = readRDS(paste0(heavies.rds.path,"nat.park.rds")) 
plot(nat.res, col="darkgreen",  border="darkgreen"); plot(nat.park, col="lightgreen",border="lightgreen", add=T)

kkl.mgmt               = readRDS(paste0(heavies.rds.path,"kkl.mgmt.rds"))
kkl.plan               = readRDS(paste0(heavies.rds.path,"kkl.plan.rds"))
kkl.ops                = readRDS(paste0(heavies.rds.path,"kkl.ops.rds"))

# b.disturbed            = readRDS(paste0(heavies.rds.path,"b.disturbed.rds"))
#landuse   = readRDS(paste0(heavies.rds.path,"landuse.rds"))
#landuse_b = readRDS(paste0(heavies.rds.path,"landuse_b.rds"))
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
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8))

###################################################################################################
# Summary of outputs ----

b.model.list.tops     = readRDS("./rds_objects/b.model.list.tops.rds")
b.ensembles           = readRDS("./rds_objects/ensembles.rds")
b.ensemble.thresholds = readRDS("./rds_objects/ensemble.thresholds.rds")
b.ensemble.dist       = readRDS(paste0(heavies.rds.path,"b.ensemble.dist.rds"))

#variable importance by model and combination
varimportance.rf    = readRDS(paste0(heavies.rds.path,"varimportance.rf.rds"))
varimportance.svm   = readRDS(paste0(heavies.rds.path,"varimportance.svm.rds"))
varimportance.gam   = readRDS(paste0(heavies.rds.path,"varimportance.gam.rds"))
varimportance       = readRDS(paste0(heavies.rds.path,"varimportance.rds"))

# variable importanct averaged across combos
var.importance      = readRDS("./rds_objects/var.importance.rds")
var.importance.df   = readRDS("./rds_objects/var.importance.df.rds") 
rcurves             = readRDS("./rds_objects/rcurves.rds")

###################################################################################################
# Create models for each combo, just with the top three algorithms ----
b.model.list.tops = list()

for (i in 1:length(b_data_packages))                                                                          {
  start_time = Sys.time();  print(b_package_names[i])
  data = b_data_packages[[i]]
  b.model.list.tops[[i]] = sdm(occurrence ~ ., data = data, methods =c('rf','svm','gam'))
  print(paste(b_package_names[[i]]," loop took ", difftime(Sys.time(),start_time, units="mins")," minutes"))  }

saveRDS(b.model.list.tops,  "./rds_objects/b.model.list.tops.rds")

###################################################################################################
# Run ensemble loop ----
ensembles = list()

for (i in 1:length(b_data_packages))                                                                               {
  start.time = Sys.time();  print(b_package_names[i])
  weights = c(eval.summary[[i]]$TSS[eval.summary[[i]]$method == "rf"],
              eval.summary[[i]]$TSS[eval.summary[[i]]$method == "svm"],
              eval.summary[[i]]$TSS[eval.summary[[i]]$method == "gam"])
  filename = paste0(heavies.spatial.path,'Ensemble - ',b_package_names[[i]], '.tif')
  ensembles[[i]] <- ensemble(b.model.list.tops[[i]], newdata = b.preds, filename = filename,
                             setting = list(method = 'weighted', weights = weights), nc=20, format="GTiff")  
  print(paste(b_package_names[[i]]," loop took ", difftime(Sys.time(),start.time, units="mins")," minutes")) }

saveRDS(ensembles, paste0(heavies.rds.path,"ensembles.rds"))

###################################################################################################
# Map the ensemble distributions using threshold to maximise sensitivity plus specificity ----

# here 'prediction' prediction with full range of probabilities; distribution = above-threshold.
b.ensemble.dist = list()
b.ensemble.thresholds = list()

for (i in 1:length(b_package_names)) {
  
  start.time = Sys.time()
  
  # get threshold:
  b.ensemble.thresholds[[i]] = mean(b.eval.summary[[i]][5, "threshold.mss"], 
                                  b.eval.summary[[i]][9, "threshold.mss"],
                                  b.eval.summary[[i]][3, "threshold.mss"])
  
  # get distribution:
  b.ensemble.dist[[i]]  = ensembles[[i]]
  b.ensemble.dist[[i]][b.ensemble.dist[[i]]   < b.ensemble.thresholds[[i]]] <- 0
  # b.ensemble.dist[[i]][b.ensemble.dist[[i]]   >= b.ensemble.thresholds[[i]]]]  <- 1 
  # leaving above step out, to keep the variability of probabilities above the threshold.
  
  # make histogram:
  filename.hist  = paste0('./output_images/Ensemble histogram - ', b_package_names[[i]], '.png')
  png(filename = filename.hist, width = 18, height = 15, units = 'cm', res = 300)
  hist(ensembles[[i]]);  points(x = b.ensemble.thresholds[[i]], y=0, pch=24, bg='red')
  dev.off()
  
  # make image:
  filename  = paste0('./output_images/Ensemble distribution - ', b_package_names[[i]], '.png')
  png(filename = filename, width = 18, height = 15, units = 'cm', res = 900)
  plot(b.ensemble.dist[[i]]) 
  # alternatively: plot(predmaps.rf[[i]], col = c('white','green'), breaks=c(0,threshold.rf,1))
  dev.off()
  
  # make raster:
  filename.raster = paste0(heavies.spatial.path,'Ensemble distribution - ', b_package_names[[i]], '.tif')
  writeRaster(b.ensemble.dist[[i]], filename = filename.raster, options=c('TFW=YES'), overwrite= TRUE)
  
  print(paste(b_package_names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes"))}

saveRDS(b.ensemble.thresholds, "./rds_objects/b.ensemble.thresholds.rds")
saveRDS(b.ensemble.dist, paste0(heavies.rds.path,"b.ensemble.dist.rds"))

###################################################################################################
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

png(filename = "./output_images/Combo comparisons - questionable absences.png", width=20, height=25, units='cm', res=900)
par(mfrow=c(3,2), mar =c(0.25,0,0.25,0), bty="n")
for (i in 1:length(combos)) {
  rasters[[i]] = raster(paste0(heavies.spatial.path,"Ensemble distribution - Combination ", toupper(combos[[i]]),".tif"))
  number = numbers[[i]]
  meanTSS[[i]] = round(mean(eval.summary[[number]]$TSS[c(3,5,9)]), digits=2)
  plot(rasters[[i]], xlim=c(34.45,35.2), ylim=c(30.85,31.4), bty="n", axes=FALSE, legend=F,
       main=paste(combo.descriptions[[number]],"TSS=",meanTSS[[i]]), cex.main=1.3)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8))}
dev.off()
# conclusion: the questionable absences have limited the predicted northern distribution considerably.


# 2) The effect of the unreliable collections records ----
# A vs F, E vs D, I vs H # listing the more comprehensive model first
combos = c("a","f","e","d","i","h")
numbers   = list(match(combos[[1]], myLetters), match(combos[[2]], myLetters),match(combos[[3]], myLetters),
                 match(combos[[4]], myLetters), match(combos[[5]], myLetters),match(combos[[6]], myLetters))
rasters = list()
meanTSS = list()

png(filename = "./output_images/Combo comparisons - unreliable coll data.png", width=20, height=25, units='cm', res=900)
par(mfrow=c(3,2), mar =c(0.25,0,0.25,0), bty="n")
for (i in 1:length(combos)) {
  rasters[[i]] = raster(paste0(heavies.spatial.path,"Ensemble distribution - Combination ",toupper(combos[[i]]),".tif"))
  number = numbers[[i]]
  meanTSS[[i]] = round(mean(eval.summary[[number]]$TSS[c(3,5,9)]), digits=2)
  plot(rasters[[i]], xlim=c(34.45,35.2), ylim=c(30.85,31.4), bty="n", axes=FALSE, legend=F,
       main=paste(combo.descriptions[[number]],"TSS=",meanTSS[[i]]), cex.main=1.3)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8))}
dev.off()

# 3) Presence-Absence versus presence-only ----
# B vs G, D vs H, E vs I # listing the more comprehensive model first
combos = c("b","g","d","h","e","i")
numbers   = list(match(combos[[1]], myLetters), match(combos[[2]], myLetters),match(combos[[3]], myLetters),
                 match(combos[[4]], myLetters), match(combos[[5]], myLetters),match(combos[[6]], myLetters))
rasters = list()
meanTSS = list()

png(filename = "./output_images/Combo comparisons - PA versus PO.png", width = 20, height = 25, units = 'cm', res = 900)
par(mfrow=c(3,2), mar =c(0.25,0,0.25,0), bty="n")
for (i in 1:length(combos)) {
  rasters[[i]] = raster(paste0(heavies.spatial.path,"Ensemble distribution - Combination ",toupper(combos[[i]]),".tif"))
  number = numbers[[i]]
  meanTSS[[i]] = round(mean(eval.summary[[number]]$TSS[c(3,5,9)]), digits=2)
  plot(rasters[[i]], xlim=c(34.45,35.2), ylim=c(30.85,31.4), bty="n", axes=FALSE, legend=F, 
       main=paste(combo.descriptions[[number]],"TSS=",meanTSS[[i]]), cex.main=1.3)
  plot(major.cities, pch=21, bg='yellow', cex=1.3, add=TRUE)
  with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=0.8))}
dev.off()

###################################################################################################
# Plot response curves ----

rcurves = list()

i=9 # for some reason this for-loop doesn't produce viable images! but running each iteration manually does.
# for (i in 1:length(b_package_names))                                                                 {
filename = paste0('./output_images/Mean response curves - ', b_package_names[[i]], '.png')
rcurves[[i]] = rcurve(b.model.list.tops[[i]], id=1:3, mean=TRUE, confidence=TRUE, smooth=T, main=title, size=1000)

png(filename = filename, width = 18, height = 5, units = 'cm', res = 600)
par(mar=c(0,0,1,0))
curve = rcurve(b.model.list.tops[[i]], id=1:3, mean=T, confidence=TRUE, smooth=T, 
               main=b_package_names[[i]], size=1000, main.cex=0.5, gg=T)
curve + theme(axis.text.x = element_blank(), axis.text.y=element_blank(),  
              axis.title.x= element_blank(), legend.position="none", panel.background=element_blank(), 
              panel.grid.major=element_blank(), plot.background=element_blank())
dev.off()  
# }

png(filename = "./output_images/Mean response curves - all combinations.png", 
    width=30, height=54, units="cm", res=600)
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

# note: use getResponseCurve() at https://rdrr.io/cran/sdm/man/response.html to extract the actual data and plot more nicely (time consuming).
# use rcurve(b.model.list.tops[[1]], id=1, smooth = T, main = "whatever") to plot individual models.

saveRDS(rcurves, paste0(heavies.rds.path,"b.rcurves.rds"))

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

for (i in 1:length(b_data_packages))          {
  varimportance.rf[[i]]  = getVarImp(b.model.list.tops[[i]], id=1, wtest='training', varImportance='tss')
  varimportance.svm[[i]] = getVarImp(b.model.list.tops[[i]], id=2, wtest='training', varImportance='tss')
  varimportance.gam[[i]] = getVarImp(b.model.list.tops[[i]], id=3, wtest='training', varImportance='tss')
  
  varimportance[[i]] = data.frame(variable = c(varimportance.rf[[i]]@variables[1], varimportance.rf[[i]]@variables[2], varimportance.rf[[i]]@variables[3],
                                          varimportance.rf[[i]]@variables[4], varimportance.rf[[i]]@variables[5], varimportance.rf[[i]]@variables[6]),
                             importance = c(mean(c(varimportance.rf[[i]]@varImportance$AUCtest[1], varimportance.svm[[i]]@varImportance$AUCtest[1],
                                               varimportance.gam[[i]]@varImportance$AUCtest[1])),
                                            mean(c(varimportance.rf[[i]]@varImportance$AUCtest[2], varimportance.svm[[i]]@varImportance$AUCtest[2],
                                               varimportance.gam[[i]]@varImportance$AUCtest[2])),
                                            mean(c(varimportance.rf[[i]]@varImportance$AUCtest[3], varimportance.svm[[i]]@varImportance$AUCtest[3],
                                               varimportance.gam[[i]]@varImportance$AUCtest[3])),
                                            mean(c(varimportance.rf[[i]]@varImportance$AUCtest[4], varimportance.svm[[i]]@varImportance$AUCtest[4],
                                               varimportance.gam[[i]]@varImportance$AUCtest[4])),
                                            mean(c(varimportance.rf[[i]]@varImportance$AUCtest[5], varimportance.svm[[i]]@varImportance$AUCtest[5],
                                               varimportance.gam[[i]]@varImportance$AUCtest[5])),
                                            mean(c(varimportance.rf[[i]]@varImportance$AUCtest[6], varimportance.svm[[i]]@varImportance$AUCtest[6],
                                               varimportance.gam[[i]]@varImportance$AUCtest[6]))))

  order = rev(c(1,3,6,2,5,4))  # Get order of rows, for plotting in order that is consistent with overall importance (below)
  varimportance[[i]] <- varimportance[[i]][order,]      # sort

  title    = paste0("Variable importance for ", b_package_names[[i]])
  filename = paste0('./output_images/Variable importance - ', b_package_names[[i]], '.png')
  png(filename = filename, width = 18, height = 10, units = 'cm', res = 600)
  # par(mar = c(5.1, 4.1, 4.1, 2.1)) # the default.
  par(mar = c(3, 4.5, 3, 1.5)) # sets the bottom, left, top and right margins respectively, in number of lines of text.
  barplot(varimportance[[i]]$importance, 
        names.arg = varimportance[[i]]$variable, xlab = "Variable importance averaged across the three top models",
        main=title, horiz=TRUE, cex.names=0.8, las=1, mgp=c(3, 0.2, 0), tck=-0.008, cex.main=1.6, col='lightgreen', xlim=c(0,0.35)) 
  dev.off()                                                                                                     }

saveRDS(varimportance.rf, paste0(heavies.rds.path,"varimportance.rf.rds"))
saveRDS(varimportance.svm, paste0(heavies.rds.path,"varimportance.svm.rds"))
saveRDS(varimportance.gam, paste0(heavies.rds.path,"varimportance.gam.rds"))
saveRDS(varimportance, paste0(heavies.rds.path,"varimportance.rds"))

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

var.importance.allcombos.df = do.call("rbind", var.importance)
var.importance.allcombos.df$mean = rowMeans(var.importance.df[,c("combo1","combo2","combo3","combo4","combo5",
                                                                 "combo6","combo7","combo8","combo9")] )

order <- order(var.importance.allcombos.df$mean)              # Get order of rows, for plotting in order
var.importance.allcombos.df <- var.importance.df[order,]      # sort

png("./output_images/Variable importance - averaged across all.png", width=18, height=10, units='cm', res=600)
# par() #default is 5.1 4.1 4.1 2.1
par(mar=c(3,4.5,3,1.5))
barplot(var.importance.allcombos.df$mean, 
        names.arg = var.importance.allcombos.df$Variable, xlab = "Variable importance averaged across all models",
        main="Averaged variable importance", horiz=TRUE, cex.names=0.8, las=1, mgp=c(3, 0.2, 0), tck=-0.008, cex.main=1.6, 
        col='lightblue')
dev.off()

saveRDS(var.importance.allcombos,    "./rds_objects/var.importance.rds")
saveRDS(var.importance.allcombos.df, "./rds_objects/var.importance.df.rds")

###################################################################################################
# The grand ensemble, etc. ----

# To reduce code duplication, this section has been deleted; 
# please refer to the corresponding section in sdm.8.beershebensis_final model

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