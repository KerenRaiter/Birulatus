# sdm.4.model: Running the models and evaluating them ----

# There are three main functions provide the main steps of developing/using species distibution models. The three steps include data preparation, model ﬁtting and evaluation, and prediction. The functions used for these steps:
# • sdmData: to read data from diﬀerent formats, and prepare a data object. Both species (single or multiple) and explanatory variables can be introduced to the function, as well as other information such as spatial coordinates, time, grouping variables, etc.
# • sdm: to ﬁt and evaluate species distribution models (multiple algorithms can be used)
# • predict: when models are ﬁtted, they can be used to predict/project given a new dataset.

####################################################################################################
# Set up and install relevant packages and locations ----

# ipak function: install (if not already installed) and load multiple R packages.
ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)}
ipak(c("rgdal","stringr","usdm", "biomod2","raster","scales", "grid", "foreign","dplyr",
       "tidyr","rgeos","magrittr","ggplot2","gridExtra","rasterVis","dismo","sdm","installr",
       "knitr","ggmap","OpenStreetMap","parallel","beepr","rmapshaper","spatialEco","rJava","xlsx"))
# xlsx fails
installAll() # installing everything the sdm relies on.

emailme = function() {
send.mail(from="kerengila@gmail.com",       to="keren.raiter@mail.huji.ac.il",
          subject="the loop is complete",   body="yeah yeah yeah",        html=T,
          smtp=list(host.name = "smtp.gmail.com",          port = 465,
                    user.name = "kerengila@gmail.com",     passwd = "Ivrit333",
                    ssl = T), authenticate=T) #, attach.files="C:\\Users\\Deepanshu\\Nature.xls"  
}

# Function to check heaviest items on memory:
{
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
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
  out
}

# step 2
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos()}

# Birulatus heavies will be on E drive (at least for now), with E drive being backed up to HUJI server regularly
B.heavies.spatial.path = 'E:/R/Birulatus_heavies/spatial/'
B.heavies.rds.path     = 'E:/R/Birulatus_heavies/rds/'
B.heavies.image.path   = 'E:/R/Birulatus_heavies/images/'

#B.heavies.spatial.path = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/spatial/'
#B.heavies.rds.path     = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/'
#B.heavies.image.path   = '//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/images/'

ITM = CRS("+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.33077,-1.85269,1.66969,5.4248 +units=m +no_defs")

####################################################################################################
# Datasets prepared earlier ----

# from this script:

eval.list       = readRDS(paste0(B.heavies.rds.path, "eval.list.topmethods.rds"))       # 'raw' list of eval data from each model
eval.summary    = readRDS(paste0(B.heavies.rds.path, "eval.summary.topmethods.rds"))    # consolidated: multiple reps averaged
eval.summary.df = readRDS(paste0(B.heavies.rds.path, "eval.summary.df.topmethods.rds")) # superconsolidate:all scenarios, 1 table)
methods.summary = readRDS("./rds/s.eval.summary.df.topmethods.rds") # summary by method, in order.
top.algorithms  = c('rf','brt','svm','gam')

# from previous scripts:

israel.WB = readRDS("rds/israel.WB.rds") # borders
israel.WB.merged = readRDS("./rds/israel.WB.merged.rds") # borders

b.s   = readRDS("./rds/b.bysite.s.rds")
b.l   = readRDS("./rds/b.bysite.l.rds")
b.i   = readRDS("./rds/b.bysite.i.rds")

bir.area.s = readRDS("./rds/bir.area.s.rds")
bir.area.l = readRDS("./rds/bir.area.l.rds")
bir.area.i = readRDS("./rds/bir.area.i.rds")

preds.s.nocoll = readRDS(paste0(B.heavies.rds.path,"preds.s.nocoll.rds")) # stack, collinear excl.
preds.l.nocoll = readRDS(paste0(B.heavies.rds.path,"preds.l.nocoll.rds")) # stack, collinear excl.
preds.i.nocoll = readRDS(paste0(B.heavies.rds.path,"preds.i.nocoll.rds")) # stack, collinear excl. 

data.packs = readRDS("rds/data.packs.bysite.rds")

major.cities = readRDS("./rds/major.cities.rds")
small.cities = readRDS("./rds/small.cities.rds")
large.towns  = readRDS("./rds/large.towns.rds")
towns        = readRDS("./rds/towns.rds")
villages     = readRDS("./rds/villages.rds")

groads       = readRDS("./rds/groads.rds")

####################################################################################################
# Validation information for reference ----

# Cross-validation, subsampling and bootstrapping are all resampling methods, but they perform resampling differently.

# Cross-validation (K-folds cross-validation):
# Sometimes called rotation estimation,[1][2][3] or out-of-sample testing is a model validation technique for assessing how the results of a statistical analysis will generalize to an independent data set. It is mainly used in settings where the goal is prediction, and one wants to estimate how accurately a predictive model will perform in practice. In a prediction problem, a model is usually given a dataset of known data on which training is run (training dataset), and a dataset of unknown data (or first seen data) against which the model is tested (called the validation dataset or testing set).[4],[5] The goal of cross-validation is to test the model’s ability to predict new data that was not used in estimating it, in order to flag problems like overfitting or selection bias[6] and to give an insight on how the model will generalize to an independent dataset (i.e., an unknown dataset, for instance from a real problem).
# One round of cross-validation involves partitioning a sample of data into complementary subsets, performing the analysis on one subset (called the training set), and validating the analysis on the other subset (called the validation set or testing set). To reduce variability, in most methods multiple rounds of cross-validation are performed using different partitions, and the validation results are combined (e.g. averaged) over the rounds to give an estimate of the model’s predictive performance.
# K-folds cross validation: Split the shuffled dataset into k chunks of data. Train with (k - 1) chunks of data objects, test it on (k - 1)th chunk. Next time, include the one that you excluded in the last iteration to train your model and test it on this excluded chunk. Repeat. This should take you K iterations. This is K times slower than random subsampling. Also good for huge data. 

# Bootstrapping:
# In statistics, bootstrapping is any test or metric that relies on random sampling with replacement. Bootstrapping allows assigning measures of accuracy (defined in terms of bias, variance, confidence intervals, prediction error or some other such measure) to sample estimates.This technique allows estimation of the sampling distribution of almost any statistic using random sampling methods.[3][4] Generally, it falls in the broader class of resampling methods.
# We don't know the true distribution of the real population, but we do know the estimated distribution, based on on our sample; an empirical distribution which puts equal probability on each observation in the sample. If the estimated distribution is a reasonable approximation to the true distribution, we can use the sampling distribution of the parameter estimate under the estimated distribution to approximately quantify the uncertainty in the estimated parameter. This can be carried out by drawing a large number of independent (i.e. with replacement) re-samples from the data, the same size as the original data, creating many variants of the original data. The variation in the estimated parameter, applied to the resamples, approximates how the estimated parameter would vary if we drew more samples from the full, real distribution.

# Subsampling:
# I assume this refers to random subsampling, which randomly splits the dataset into a training set and a test set. It can be 80–20, 70–30, 60–40. This is also called as Hold-out. The difference between Random subsampling and k-folds cross-validation is that in the K-folds, the dataset is split into chunks, whereas this isn't. But seeing as it's shuffled first, I don't see the difference, other than that in K-folds cv the test is done K times, each time with a different chunk as the test set. Good for huge data. If you are getting similar scores and optimal model’s parameters with some of your iterations.

# Leave-one-out (not used here but included for completeness: 
# Special case of K-fold. K = 100 (in our example). Train your model with 99 objects, test it on 100th object. Repeat this with leaving out one object each time. This should take you 100 iterations. In our example this is 100 times slower than random subsampling. Good for less data and unbalanced dataset and target values.

# Algorithm/Method information:
getmethodNames('sdm')
# RPart is 'Recursive Partitioning and Regression Trees'
# SVM is 'Support Vector Machines'
# I'm going to leave out bioclim as it is a profile model which is overly simplistic. This is confirmed by its consistently low performance in models compared to the other methods. 
# Similarly I'm going to leave out glmnet (:GLM with lasso or elasticnet regularization; https://rdrr.io/rforge/sdm/src/inst/methods/sdm/glmnet.R as it's a poor performer and has been causing some issues with some models not working.)

####################################################################################################
# Create evaluation models for each scenario -----
# Define input list, output list, and function 

set.names = list("Soils-delimited study area", "Lithology-delimited study area", 
                 "Israel-wide study area")

# define function with 'n' runs, 'cv.folds' folds cross-validation, taking 30 percent as test: 
sdm.cv = function(data) {
  sdm(occurrence ~ ., data = data, methods =c("cart",'fda','gam','brt','rf','glm', 'mda',
                                              'rpart','svm'), 
      n=100, replication = 'cv', cv.folds=5) }

# create models for each scenario 
model.list.cv.100n = list()

for (i in 1:length(data.packs))                                                                        {
  start.time = Sys.time()
  print(set.names[i])
  data = data.packs[[i]]
  model.list.cv.100n[[i]] = sdm.cv(data)  
  print(paste(set.names[[i]],"loop took", difftime(Sys.time(), start.time, units="mins"),"minutes"))   }

saveRDS(model.list.cv.100n, paste0(B.heavies.rds.path,"model.list.cv.100n.rds"))
beep()
emailme()
  
# # see how they went:
set.names[[1]]; model.list.cv.100n[[1]] # gam and brt didn't work, rpart gives NaN
set.names[[2]]; model.list.cv.100n[[2]] # gam and brt didn't work, rpart gives NaN
set.names[[3]]; model.list.cv.100n[[3]] # gam and brt didn't work, rpart is OK.

getVarImp(model.list.cv.100n[[1]])
getVarImp(model.list.cv.100n[[2]])
getVarImp(model.list.cv.100n[[3]])

# save and/or retrieve
# saveRDS(model.list.cv.100n, paste0(B.heavies.rds.path,"B.heavies.rds.path/model.list.cv.100n.rds"))

# or retreive from saved to prevent running the above code:
# model.list.cv.100n  = readRDS(paste0(B.heavies.rds.path,"B.heavies.rds.path/model.list.cv.100n.rds"))

####################################################################################################
# Summarise model evaluations by methods for each scenario -----
eval.list = list() # evaluation data by model (i.e. 4500 models:100reps x5folds x9algs x9scenarios)
eval.summary = list() # list of evaluation data, averaged by alg (i.e. 9 dataframes of 9 rows:9 scenarios X 9 algs)

modelset = model.list.cv.100n # to prevent having to repeat this multiple times in the code below:

for (i in 1:length(modelset))                                 {
  model.inf.ev = NULL
  # extract model info and eval statistics and merge them:
  model_info = getModelInfo (modelset[[i]])
  model_eval = getEvaluation(modelset[[i]], wtest= 'test.dep', 
                             stat=c('TSS','Kappa','AUC','COR','Deviance','obs.prevalence',
                                    'threshold'), opt=2)
  model.inf.ev = merge(model_info, model_eval, by = "modelID") #temporary vector for use in next bit
  # then put them into list and aggregate them
  eval.list[[i]]   = model.inf.ev 
  eval.summary[[i]]= data.frame(method=aggregate(model.inf.ev$method, by=list(model.inf.ev$method),
                                                 FUN=mode)[1], set = set.names[[i]], #keep relevant!
                                TSS     = aggregate(model.inf.ev$TSS,       
                                                    by=list(model.inf.ev$method),  FUN=mean)[2],
                                TSS_sd  = aggregate(model.inf.ev$TSS,       
                                                    by=list(model.inf.ev$method),  FUN=sd)  [2],
                                AUC     = aggregate(model.inf.ev$AUC,       
                                                    by=list(model.inf.ev$method),  FUN=mean)[2],
                                AUC_sd  = aggregate(model.inf.ev$AUC,       
                                                    by=list(model.inf.ev$method),  FUN=sd)  [2],
                                kappa   = aggregate(model.inf.ev$Kappa,     
                                                    by=list(model.inf.ev$method),  FUN=mean)[2],
                                COR     = aggregate(model.inf.ev$COR,       
                                                    by=list(model.inf.ev$method),  FUN=mean)[2],
                                deviance= aggregate(model.inf.ev$Deviance,  
                                                    by=list(model.inf.ev$method),  FUN=mean)[2],
                                obs.prev= aggregate(model.inf.ev$Prevalence,
                                                    by=list(model.inf.ev$method),  FUN=mean)[2],
                                threshold= aggregate(model.inf.ev$threshold,
                                                     by=list(model.inf.ev$method),  FUN=mean)[2],
                                total.mods=aggregate(model.inf.ev$method,   
                                                     by=list(model.inf.ev$method),FUN=length)[2])
  names(eval.summary[[i]])=c('method','scenario','TSS','TSS_sd','AUC','AUC_sd','kappa','COR',
                             'deviance','obs.prevalence','threshold.mss','total_models')   }

eval.summary
(eval.summary.df = do.call("rbind", eval.summary)) # converting the list to a single dataframe for easy plotting


# saving the outputs from the 5-fold, 100-replicate run evaluation
saveRDS(eval.list,       paste0(B.heavies.rds.path, "eval.list.topmethods.rds"))      # 'raw' list of eval data from each model
saveRDS(eval.summary,    paste0(B.heavies.rds.path, "eval.summary.topmethods.rds"))    # consolidated: multiple reps averaged
saveRDS(eval.summary.df, paste0(B.heavies.rds.path, "eval.summary.df.topmethods.rds")) # superconsolidate:all scenarios, 1 table)
# write.xlsx(eval.summary,    paste0(B.heavies.rds.path, "model evaluation summary.xlsx")) # export to excel sprdsheet - fails
# write.xlsx(eval.summary.df, paste0(B.heavies.rds.path, "model evaluation summary onetable.xlsx"))

# Plot relationship between TSS and AUC
AUC_ordered = ordered(eval.summary.df$AUC)
TSS_lower = eval.summary.df$TSS - eval.summary.df$TSS_sd
TSS_upper = eval.summary.df$TSS + eval.summary.df$TSS_sd

r = ggplot(eval.summary.df, aes(x=AUC, y=TSS, color = scenario)) + geom_point() 
r
cor(eval.summary.df$TSS, eval.summary.df$AUC) # high correlation: 0.87

####################################################################################################
# Defining 'top' algorithims for future focus ----
# I'm going to use TSS to define the topmodels - we have already seen that this is highly correlated with AUC.
# To my knowledge; can't extract top models but can specify which ones to use when drawing ROC curves and running ensembles.

# Selecting by average TSS and AUC values ----
methods.summary = aggregate(model.inf.ev[, c("TSS", "AUC")], # improved from shnunit version, aggregates original list
                            model.inf.ev["method"], function(x) c(mean=mean(x), sd=sd(x)) )
methods.summary$TSS.sd = methods.summary$TSS[,2]
methods.summary$AUC.sd = methods.summary$AUC[,2]
methods.summary$TSS.mean = methods.summary$TSS[,1]
methods.summary$AUC.mean = methods.summary$AUC[,1]
methods.summary$TSS = NULL
methods.summary$AUC = NULL
methods.summary = methods.summary[,c(1,4,2,5,3)] # getting them back in a nice order

(methods.summary = methods.summary[order(-methods.summary$TSS.mean),])
rownames(methods.summary) <- 1:nrow(methods.summary)
methods.summary
methods.summary$method = factor(methods.summary$method, 
                                levels=methods.summary$method[order(-methods.summary$TSS.mean)])
# write.xlsx(methods.summary,"./data/methods.summary.xlsx") # export to excel sprdsheet

# top algorithms are fda, svm and rf 
#  - Flexible Discriminant Analysis (‘fda’) – a statistical regression algorithm
#  - Support vector machines ('svm') – a machine-learning algorithm
#  - Random Forests ('rf') – a machine-learning algorithm
saveRDS(methods.summary, "./rds/methods.summary.rds")

# plot TSS by method, with error bars:
palette(c("red","magenta","blue","cyan","green3","brown","gray","purple","yellow")) 
# adding&changing colour palette (default has 8 only)
algorithm = 1:9

ymin = min(methods.summary$TSS.mean - methods.summary$TSS.sd)
ymax = max(methods.summary$TSS.mean + methods.summary$TSS.sd)

lower.end = methods.summary$TSS.mean - methods.summary$TSS.sd
upper.end = methods.summary$TSS.mean + methods.summary$TSS.sd

png(filename = paste0(B.heavies.image.path,"Algorithm performance by TSS.withbars.by site.png"), 
    width=14, height=8, units='cm', res=300)
par(mfrow = c(1,1), mgp=c(2,0.5,0), mar=c(3,3,0,0)+0.1)
plot(methods.summary$TSS[,1], bg=methods.summary$method, pch=21, cex=1.5, 
     ylim = range(c(ymin, ymax)), xlim=c(1,7.5), 
     xlab="Algorithm",xaxt='n',ylab="Average TSS")
with(methods.summary, text(methods.summary$TSS.mean~rownames(methods.summary),
                           labels = methods.summary$method, pos=4, cex=0.9, offset=0.35))
#legend("topright", c('rf','svm','gam','brt','mda','cart','rpart','glm','fda'), 
#  pt.bg=c(1:9),pch=21,cex=1.5,text.font=1)
arrows(algorithm, lower.end, algorithm,   upper.end,  length=0.05, angle=90, code=3)
points(methods.summary$TSS.mean, bg=methods.summary$method, pch=21, cex=1.4) # put points on top
dev.off()

# plot without error bars:

ymin = min(methods.summary$TSS.mean)
ymax = max(methods.summary$TSS.mean)

png(filename = paste0(B.heavies.image.path,"Algorithm performance by TSS.nobars. by site.png"), 
    width=14, height=8, units='cm', res=300)
par(mgp=c(2,0.5,0),mar=c(3,3,0,0))
plot(methods.summary$TSS.mean, bg=methods.summary$method, pch=21, cex=1.5, xlab="Algorithm",
     xaxt='n',ylab="Average TSS",
     ylim=c(ymin, 1.02), xlim=c(1,7.5))
with(methods.summary, text(methods.summary$TSS.mean ~ rownames(methods.summary), 
                           labels = methods.summary$method, pos = 4, cex=1))
# legend("topright",c("rf","svm","gam","brt","mda","cart","rpart","glm","fda"),pt.bg=c(1:9), pch=21,cex=1.5,text.font=1)
# arrows(algorithm, lower.end, algorithm,   upper.end,  length=0.05, angle=90, code=3)
# points(methods.summary$TSS[,1], bg=methods.summary$method, pch=21, cex=1.5) # just putting points on top of lines
dev.off()

# Plot TSS by SD for all models (summarised by scenario-alg combination)
png(filename = paste0(B.heavies.image.path,"Variability of algorithm performance.by site.png"), 
    width=20, height=12, units='cm', res=600)
plot(methods.summary$TSS.mean, methods.summary$TSS.sd, pch=21, bg=methods.summary$method, 
     xlab="Algorithm average performance (mean TSS)", 
     ylab="Performance variability (TSS standard deviation)", 
     # xlim=c(0.48,0.785), ylim = c(0.09, 0.14)
     )
points(methods.summary$TSS.mean[methods.summary$method == "rf"], 
       methods.summary$TSS.sd[methods.summary$method == "rf"], pch=21, bg= "green3", cex=1.5)
points(methods.summary$TSS.mean[methods.summary$method == "svm"], 
       methods.summary$TSS.sd[methods.summary$method == "svm"],  pch=21, bg= "yellow", cex=1.5)
points(methods.summary$TSS.mean[methods.summary$method == "gam"], 
       methods.summary$TSS.sd[methods.summary$method == "gam"],  pch=21, bg= "blue", cex=1.5)
points(methods.summary$TSS.mean[methods.summary$method == "brt"], 
       methods.summary$TSS.sd[methods.summary$method == "brt"],  pch=21, bg= 4, cex=1.5)
with(methods.summary, text(methods.summary$TSS.sd ~ methods.summary$TSS.mean, 
                           labels = methods.summary$method, pos = 4, cex=1))
# legend("bottomleft", c("cart","fda","gam","brt","rf","glm","mda","rpart","svm"), pt.bg=c(1:9), pch=21, cex=1.1)
dev.off()
# Performance of the top 3 methods is as consistent as any other methods. The SD of TSS for the top RF models in particularly low.

# Outputs from top model selection ----
# write.xlsx(methods.summary, "./data/model evaluation summary (aggregated by method).xlsx")
# top models are RF, BRT, SVM, and GAM (CART is up there too, but so similar to BRT, not considered to add much)

####################################################################################################
# the end of this script