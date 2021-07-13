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

# save all objects up to here:
# save.image(file = paste0(B.heavies.rds.path, "workspace script 5, 9.7.2021.RData"))
load(paste0(B.heavies.rds.path, "workspace script 5, 9.7.2021.RData"))

predmaps.fda = readRDS(paste0(B.heavies.rds.path, "predmaps.fda.rds"))
predmaps.svm = readRDS(paste0(B.heavies.rds.path, "predmaps.svm.rds"))
predmaps.rf  = readRDS(paste0(B.heavies.rds.path, "predmaps.rf.rds"))

set.ensembles = readRDS(paste0(B.heavies.rds.path, "set.ensembles.rds")) # no content :(

thresholds     = readRDS("./rds/ensemble.thresholds.rds")
ensemble.dist  = readRDS(paste0(B.heavies.rds.path,"ensemble.dist.rds"))


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
                        "Broad model, continuous data only")
data.packs          = readRDS("./rds/data.packs.bysite.rds")

borders          = readRDS("rds/borders.rds")
israel.WB        = readRDS("rds/israel.WB.rds")
israel.WB.merged = readRDS("./rds/israel.WB.merged.rds")
israel.WB        = readRDS("./rds/israel.WB.no.water.rds")
israel.noWB      = readRDS("rds/israel.noWB.rds")
waterbodies      = readRDS("rds/waterbodies.rds")

bir.area.s = readRDS("./rds/bir.area.s.rds")
bir.area.l = readRDS("./rds/bir.area.l.rds")
bir.area.i = readRDS("./rds/bir.area.i.rds")

major.cities = readRDS("./rds/major.cities.rds")
four.cities  = subset(major.cities, subset = name == "Tel Aviv" | name == "Be'er Sheva" | 
                                             name == "Haifa"    | name == "Jerusalem")
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
# Predict species occurrence probability over whole study area (individual algorithms) ----

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

# for(i in 1:length(set.names)) {
i=1 # loop doesn't work...
i=2
i=3

  file = paste0(B.heavies.spatial.path, 'Ensemble prediction - ', set.names[[i]], '.tif')
  set.ensembles[[i]] = ensemble(models[[i]], newdata =preds[[i]], filename = file, overwrite = TRUE, 
                                setting = list(method ='weighted', stat='TSS', opt=2)) 
                                 # opt=2 selects for max(sensitivity + specificity)
  {
  file = paste0(B.heavies.image.path, 'Ensemble prediction - ', set.names[[i]], '.png')
  png(plot, filename = file, width = 18, height = 30, units = "cm", res = 300)
  plot(set.ensembles[[i]])
  plot(waterbodies, col = "lightblue", border = NA, add = T)
  lines(groads, col="lightgrey")
  plot(israel.WB.merged, border = "grey", add = T)
  lines(study.areas[[i]], col="orange")
  points(b[b$occurrence == 1,], col='blue', pch=16, cex=0.5)
  points(b[b$occurrence == 0,], col='red',  pch=16, cex=0.5)
  points(four.cities, pch=21, col='black', bg='black', cex=1)
  with  (four.cities, text(four.cities$lat ~ four.cities$lon, 
                           labels = four.cities$name, pos=4, cex=0.6, offset=0.3)) 
  legend("bottomright", c("Observed presence", "Observed absence", "Study area"), cex = 0.5,  
         pch = c(16,16,22), col=c("blue","red","orange"), pt.bg= c(NA, NA, NA))
  dev.off()}
#   }

saveRDS(set.ensembles, paste0(B.heavies.rds.path, "set.ensembles.rds")) # doesn't save the content
# and no way of deconstructing seems to allow me to save the heavy contents either!
save(set.ensembles, file = paste0(B.heavies.rds.path, "set.ensembles.RData"))

beep()

set.ensembles

# Alternative method (not needed here):
{
# # The above ensembling method uses rasters as the new data, so the output is also rasters. 
# # Below is a way to create an ensemble based on the evaluation data. This allows you to make an ensemble using a selection of models, selected by their performance. You just find out which models have e.g. TSS > 0.7 and get their modelID (you can use getEvaluation function to get the statistics for all modelIDs and extract the id of those that meet your condition). Then, in the setting of the ensemble function, you can add the argument id to which you give the modelD of the models you want to participate in the ensemble procedure.
# 
# me.1 = getEvaluation(models[[1]])
# me.2 = getEvaluation(models[[2]])
# me.3 = getEvaluation(models[[3]])
# 
# # use the following to specify just models with a certain performance level (here unneeded):
# id <- me.1$modelID[me.1$TSS > 0.7]
# 
# en.1 = ensemble(models[[1]], preds[[1]], filename='file.tif', 
#                 setting=list(method='weighted', stat='TSS', opt=2, id=id)) 
}

###################################################################################################
# Variable importance ----

# 'how to' notes:
{#getVarImp(m,id=1,wtest='training') # variable importance based on training dataset
  
  #vi <- getVarImp(m,id=1,wtest='test.dep') 
  
  #plot(vi,'auc')
  
  #plot(vi,'cor')
}

# the automatic way ----
varimp = list()

for(v in 1:length(set.names)) {
  
  varimp[[v]] = getVarImp( model.list.complete[[v]] ) 
  # (once I was able to include ",  varImportance='tss'" in the above, now doesn't work)

  file = paste0(B.heavies.image.path, 'Variable importance - ', set.names[[v]], '.png')
  png(plot, filename = file, width = 14, height = 14, units = "cm", res = 300)
  plot( varimp[[v]], main = set.descriptions[[v]] , cex = 0.4) 
  dev.off()  
  
  plot( varimp[[v]], main = set.descriptions[[v]] , cex = 0.4)   
  
  varimp[[v]]
  #str(varimp[[v]])  
  }

# The manual way ----
# The automatic way is nice! and has error bars! but I don't know how to extract the tss variable importance data, nor change the order, so going back to calculating the average manually:

# first, calculate variable importance for individual algorithms and sets, and average within sets:
varimportance.fda      = list()
varimportance.svm      = list()
varimportance.rf       = list()
varimportance          = list()
par(mar=c(3,4.5,3,1.5))

for (i in 1:length(set.names))                                                               {
  varimportance.fda[[i]]  = getVarImp(modelset[[i]], id=1, wtest='training', varImportance='tss')
  varimportance.svm[[i]]  = getVarImp(modelset[[i]], id=2, wtest='training', varImportance='tss')
  varimportance.rf[[i]]   = getVarImp(modelset[[i]], id=3, wtest='training', varImportance='tss')
  
  varimportance[[i]] = data.frame(variable = c(varimportance.fda[[i]]@variables[1], 
                                               varimportance.fda[[i]]@variables[2], 
                                               varimportance.fda[[i]]@variables[3],
                                               varimportance.fda[[i]]@variables[4], 
                                               varimportance.fda[[i]]@variables[5]),
                       importance = c(mean(c(varimportance.fda[[i]]@varImportance$AUCtest[1], 
                                             varimportance.svm[[i]]@varImportance$AUCtest[1],
                                             varimportance.rf [[i]]@varImportance$AUCtest[1])),
                                      mean(c(varimportance.fda[[i]]@varImportance$AUCtest[2], 
                                             varimportance.svm[[i]]@varImportance$AUCtest[2],
                                             varimportance.rf [[i]]@varImportance$AUCtest[2])),
                                      mean(c(varimportance.fda[[i]]@varImportance$AUCtest[3], 
                                             varimportance.svm[[i]]@varImportance$AUCtest[3],
                                             varimportance.rf [[i]]@varImportance$AUCtest[3])),
                                      mean(c(varimportance.fda[[i]]@varImportance$AUCtest[4], 
                                             varimportance.svm[[i]]@varImportance$AUCtest[4],
                                             varimportance.rf [[i]]@varImportance$AUCtest[4])),
                                      mean(c(varimportance.fda[[i]]@varImportance$AUCtest[5], 
                                             varimportance.svm[[i]]@varImportance$AUCtest[5],
                                             varimportance.rf [[i]]@varImportance$AUCtest[5]))))
  
  order = (rev(c(5,4,1,2,3)))  # Get order of rows as determined by overall average importance (below) # removed rev()
  varimportance[[i]] <- varimportance[[i]][order,]      # sort
  
  title    = paste0("Variable importance for ", set.descriptions[[i]]) 
  # title = "Variable importance averaged across the three top models"
  # filename = paste0(B.heavies.image.path,'Variable importance - ', 
  #                   set.descriptions[[i]],'.png') # didn't work when file name specified separately
  png(filename = paste0(B.heavies.image.path,
                        'Mean variable importance-', set.descriptions[[i]],'.png'), 
      width = 18, height = 10, units = 'cm', res = 600)  
  par(mar = c(3, 8.5, 3, 0.5)) # sets the bottom, left, top and right margins respectively.
                               # the default: par(mar = c(5.1, 4.1, 4.1, 2.1)) # units = lines of text
  barplot(varimportance[[i]]$importance, 
          names.arg = varimportance[[i]]$variable, 
          xlab = "Variable importance averaged across the four top models",
          main=title, cex.main=1.2, horiz=TRUE, cex.names=0.8, las=1, 
          mgp=c(3, 0.2, 0), tck=-0.008,  col='lightgreen', xlim=c(0,0.4)) 
  dev.off()                                                                                                     }

saveRDS(varimportance.rf,  paste0(B.heavies.rds.path, "varimportance.rf.rds"))
saveRDS(varimportance.svm, paste0(B.heavies.rds.path, "varimportance.svm.rds"))
saveRDS(varimportance.gam, paste0(B.heavies.rds.path, "varimportance.gam.rds"))
saveRDS(varimportance,     paste0(B.heavies.rds.path, "varimportance.rds"))

# Then average variable importance over all models

var.importance.allsets = data.frame(
  Variable = c("Topo-wetness", "Jan temp", "Precipitation", "Slope", "Soil", 
               "Lithology", "July temp"),
  Importance = c(mean(varimportance[[1]][varimportance[[1]]$variable =="Topographic.Wetness", 2 ],
                      varimportance[[2]][varimportance[[2]]$variable =="Topographic.Wetness", 2 ],
                      varimportance[[3]][varimportance[[3]]$variable =="Topographic.Wetness", 2 ]),
                 mean(varimportance[[1]][varimportance[[1]]$variable =="Jan.mean.temperature", 2 ],
                      varimportance[[2]][varimportance[[2]]$variable =="Jan.mean.temperature", 2 ],
                      varimportance[[3]][varimportance[[3]]$variable =="Jan.mean.temperature", 2 ]),
                 mean(varimportance[[1]][varimportance[[1]]$variable =="Precipitation", 2 ],
                      varimportance[[2]][varimportance[[2]]$variable =="Precipitation", 2 ],
                      varimportance[[3]][varimportance[[3]]$variable =="Precipitation", 2 ]),
                 mean(varimportance[[1]][varimportance[[1]]$variable =="slope", 2 ],
                      varimportance[[2]][varimportance[[2]]$variable =="slope", 2 ],
                      varimportance[[3]][varimportance[[3]]$variable =="slope", 2 ]),
                 varimportance[[1]][varimportance[[1]]$variable =="Soil", 2 ],
                 varimportance[[2]][varimportance[[2]]$variable =="lith", 2 ],
                 varimportance[[3]][varimportance[[3]]$variable =="July.mean.temperature", 2 ] ))

order = order(var.importance.allsets$Importance)
var.importance.allsets.ordered = var.importance.allsets[order,] 

png(paste0(B.heavies.image.path,"Variable importance averages for all sets.png"), 
    width = 18, height = 10, units = 'cm', res = 600)   # par() #default is 5.1 4.1 4.1 2.1
par(mar=c(3,4.4,3,1.5))
barplot(var.importance.allsets.ordered$Importance, 
        names.arg = var.importance.allsets.ordered$Variable, 
        xlab = "Variable importance averaged across all sets",
        main="Averaged variable importance", horiz=TRUE, cex.names=0.8, las=1, 
        mgp=c(3, 0.2, 0), tck=-0.008, cex.main=1.4, col='lightblue')
dev.off()

saveRDS(var.importance.allsets.ordered, "./rds/var.importance.allsets.ordered.rds")

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
  par(mfrow = c(1,1), mar = c(2,2,2,0), mgp=c(2,0.5,0)) # , bty="n"
  png(filename = plot.filename, width = 16, height = 25, units = 'cm', res = 900)
  plot(ensemble.dist[[i]], xlim=c(xmins[[i]], xmaxs[[i]]), ylim=c(ymins[[i]], ymaxs[[i]]), 
       # or xlim = c(34.5,36)???
       main = "Predicted Birulatus distribution, using an ensemble of 3 modelling approaches", 
       cex.main=0.7) 
  plot(waterbodies, col="lightblue", border=NA, add = T)
  lines(groads, col="lightgrey")
  plot(israel.WB.merged, col=NA, lty=5, lwd=1, add=T)
  lines(study.areas[[i]], col="orange")
  points(b[b$occurrence == 1,],col='blue', pch=16, cex=0.5)
  points(b[b$occurrence == 0,],col='red',  pch=16, cex=0.5)
  points(four.cities, pch=21, col='black', bg='black', cex=1)
  with  (four.cities, text(four.cities$lat~four.cities$lon, 
                           labels=four.cities$name, pos=4, cex=0.6, offset=0.3)) 
  legend("bottomright", c("Observed presence", "Observed absence", "Study area"), cex = 0.5, 
         pch=c(16,16,22), col=c("blue","red","orange"), pt.bg= c(NA, NA, NA))
  
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
  plot.filename  = paste0(B.heavies.image.path,'Ensemble distribution - ', 
                          set.names[[i]], '_unitone.png')
  par(mar = c(2,2,2,0), mgp=c(2,0.5,0)) # , bty="n"
  png(filename = plot.filename, width = 16, height = 25, units = 'cm', res = 300)
  plot(ensemble.dist[[i]], col = c('white','blue'), breaks=c(0, thresholds[[i]], 1), 
       legend=FALSE, xlim=c(xmins[[i]], xmaxs[[i]]), ylim=c(ymins[[i]], ymaxs[[i]]),
       main = "Predicted Birulatus distribution, using an ensemble of 3 modelling approaches", 
       cex.main=0.7) 
  plot(waterbodies, col="lightblue", border=NA, add = T)
  lines(groads, col="lightgrey")
  lines(israel.WB, col="darkgrey", lty=5, lwd=1)
  lines(study.areas[[i]], col = "orange")
  points(b[b$occurrence == 1,], col='blue', pch=16, cex=0.5)
  points(b[b$occurrence == 0,], col='red',  pch=16, cex=0.5)
  points(four.cities, pch=21, col='black', bg='black', cex=1)
  with  (four.cities, text(four.cities$lat~four.cities$lon,
                           labels=four.cities$name, pos=4, cex=0.6, offset=0.3))
  # points(select.towns, pch=21, col='black', bg='yellow', cex=0.8)
  # with(select.towns, text(select.towns$lat~select.towns$lon, 
  #                         labels=select.towns$name, pos=4, cex=0.6, offset=0.3))
  legend("bottomright", c("Observed presence","Observed absence","Predicted distribution","Study area"), cex=.5,
         col = c("blue","red","blue","orange"), pt.bg = c(NA,NA,"blue",NA), pch=c(16,16,22,22))
  
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
par(mfrow=c(1,3), mar = c(0,0,2,4), mgp=c(0,0,0), oma = c(0, 0, 1.8, 0)) #, bty = "n"

for (i in 1:length(set.names)) { 
  plot(bir.area.i, axes=FALSE, ann=FALSE, border="darkgrey", )
  plot(ensemble.dist[[i]], add=TRUE,
       col = c('white','chartreuse4'), breaks=c(0, thresholds[[i]], 1), 
     legend=FALSE, xlim=c(34.26693, 35.89533), ylim=c(29.49042, 33.33407)) 
  plot(waterbodies, col = "lightblue", border = NA, add = T)
  lines(groads, col="lightgrey")
  lines(israel.WB, col="darkgrey", lty=5, lwd=1)
  lines(study.areas[[i]], col="orange")
  points(b[b$occurrence == 1,], col='blue', pch=16, cex=0.5)
  points(b[b$occurrence == 0,], col='red',  pch=16, cex=0.5)
  points(four.cities, pch=21, col='black', bg='yellow', cex=1)
  with  (four.cities, text(four.cities$lat~four.cities$lon, 
                           labels=four.cities$name, pos=4, cex=0.6, offset=0.3)) 
  text(x = 35.2, y = 33.55, labels = toupper(set.descriptions[[i]]), cex = 0.9, xpd = NA) 
  legend("bottomright", c("Observed presence","Observed absence","Predicted distribution", "Study area"), cex = 0.5, 
         pch = c(16,16,22,22), col = c("blue","red","chartreuse4","orange"),
         pt.bg = c(NA,NA,"chartreuse4",NA))
  box(lty = 1, col = 'black')
}
mtext("Predicted Birulatus distribution, using an ensemble of 3 modelling approaches", 
      outer = TRUE, cex = 1.2)
} 
dev.off()

###################################################################################################
# Plot response curves ----

modelset        = model.list.complete
rcurves         = list()                  # list to store response curves

i=3 # for some reason this for-loop doesn't produce viable images! but running each iteration manually does.
# 11/05/2020: even the code didn't work, I had to manually save from the plot window.
# for (i in 1:length(set.names))       {

filename = paste0(B.heavies.image.path,'Mean response curves - ', set.descriptions[[i]], '.png')
title = paste0("Response curves for ", set.descriptions[[i]])
rcurves[[i]] = rcurve(modelset[[i]], id=1:3, mean=TRUE, confidence=TRUE, 
                      smooth=T, main=title, size=1000)

# png(filename = filename, width = 18, height = 8, units = 'cm', res = 600)
# preset file name above not working so do it manually:
png(filename = paste0(B.heavies.image.path, 
                      'Response curves - Broad model, no soil nor lithology data.png'), 
    width = 18, height = 8, units = 'cm', res = 600)
par(mar=c(0,0,1,0))
curve = rcurve(modelset[[i]], id=1:3, mean=T, confidence=TRUE, smooth=T, 
               main=set.descriptions[[i]], size=1000, main.cex=0.5, gg=T)
curve + theme(axis.text.x = element_blank(), axis.text.y = element_blank(),  
              axis.title.x=element_blank(), legend.position="none", 
              panel.background=element_blank(), 
              panel.grid.major=element_blank(), 
              plot.background=element_blank())
dev.off()                                  
# }

str(rcurves[[i]]$data$variable)

# Response curves again, with x-axes 
i=1 # loop doesn't work so do each of these then code chunk below...
i=2 # loop doesn't work so do each of these then code chunk below...
i=3 # loop doesn't work so do each of these then code chunk below...

{
var_names = c("Jan.mean.temperature"="Jan temp", "Precipitation"="Precipitation", "slope"="Slope",
              "Soil"="Soil", "Topographic.Wetness"="Topo-W", "lith"="Lithology", 
              "July.mean.temperature"="July temp")
filename = paste0(B.heavies.image.path,
                  'Mean response curves - ', set.descriptions[[i]], ' with axes.png')
title    = paste0("Response curves for ", set.descriptions[[i]])

png(filename = filename, width = 18, height = 8, units = 'cm', res = 300)
par(mar=c(1,1,1,0))
curve = rcurve(modelset[[i]], id=1:3, mean=T, confidence=TRUE, smooth=T, 
               main = set.descriptions[[i]], size=1000, main.cex=0.5, gg=T) 

curve + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=10),
              axis.title.x = element_text(size=12, color='black'), 
              legend.position="none", 
              panel.background=element_blank(), 
              panel.grid.major=element_blank(), 
              plot.background=element_blank()) + 
  facet_grid(.~variable, scales='free_x', labeller = as_labeller(var_names))
dev.off() }                                 



filename = paste0(B.heavies.image.path,
                  'Mean response curves - ', set.descriptions[[i]], ' with axes.png')
title    = paste0("Response curves for ", set.descriptions[[i]])

png(filename = filename, width = 18, height = 8, units = 'cm', res = 300)
par(mar=c(1,1,1,0))
curve = rcurve(modelset[[i]], id=1:3, mean=T, confidence=TRUE, smooth=T, 
               main = set.descriptions[[i]], size=1000, main.cex=0.5, gg=T) 

curve + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=10),
              axis.title.x = element_text(size=12, color='black'), 
              legend.position="none", 
              panel.background=element_blank(), 
              panel.grid.major=element_blank(), 
              plot.background=element_blank()) + 
  facet_grid(.~variable, scales='free_x', labeller = as_labeller(var_names))
dev.off() 


saveRDS(rcurves, paste0(B.heavies.rds.path,"rcurves.rds"))

###################################################################################################
# Extra stuff that might come in handy one day -----

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

# Alternative way of calculating variable importance across all sets 
{
var.importance.allsets = list()

for (v in 1:6)          {     # manually replaced the 'length(varimportance[[1]]$variable)' with 6 as it still gave 7.
  
  var = varimportance[[1]]$variable[[v]] # name of variable
  
  var.importance.allcombos[[v]] = data.frame(Variable = var,
                                             set1 = varimportance[[1]]$importance[v], 
                                             # '1' is the scenario, 'v' refers to variable in question
                                             set2 = varimportance[[2]]$importance[v],                                                                                       set3 = varimportance[[3]]$importance[v]  )    }

var.importance.allcombos.df = do.call("rbind", var.importance.allcombos)
var.importance.allcombos.df$mean = rowMeans(var.importance.allcombos.df[,c("scenario1","scenario2")] )

order <- order(var.importance.allcombos.df$mean)              # Get order of rows, for plotting in order
var.importance.allcombos.df <- var.importance.allcombos.df[order,]      # sort

# png(paste0(B.heavies.image.path,"Variable importance - averaged across xxx.png"), 
#     width = 18, height = 10, units = 'cm', res = 600)
# par() #default is 5.1 4.1 4.1 2.1
par(mar=c(3,4.5,3,1.5))
barplot(var.importance.allcombos.df$mean, 
        names.arg = var.importance.allcombos.df$Variable, xlab = "Variable importance averaged across all models",
        main="Averaged variable importance", horiz=TRUE, cex.names=0.8, las=1, 
        mgp=c(3, 0.2, 0), tck=-0.008, cex.main=1.6, col='lightblue')
# dev.off()
}

###################################################################################################
