# sdm.4.model: fitting the data and running the models ----

# There are three main functions provide the main steps of developing/using species distibution models. The three steps include data preparation, model ﬁtting and evaluation, and prediction. The functions used for these steps:
# • sdmData: to read data from diﬀerent formats, and prepare a data object. Both species (single or multiple) and explanatory variables can be introduced to the function, as well as other information such as spatial coordinates, time, grouping variables, etc.
# • sdm: to ﬁt and evaluate species distribution models (multiple algorithms can be used)
# • predict: when models are ﬁtted, they can be used to predict/project given a new dataset.

########################################################################################################
# Housekeeping and load required data ----

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

# load data

# reference info:
borders     = readRDS("./rds_objects/borders.rds")  
major.cities  = readRDS("./rds_objects/major.cities.rds")
small.cities  = readRDS("./rds_objects/small.cities.rds")
towns         = readRDS("./rds_objects/towns.rds")
villages      = readRDS("./rds_objects/villages.rds")
groads        = readRDS("./rds_objects/groads.rds")

# variable data:
b.raster.list.names = list("Rain", "Jant", "Jult","TWet", "Slop", "Soil")
b.raster.list       = readRDS(paste0(heavies.rds.path,"b.raster.list.rds"))
b.preds             = readRDS(paste0(heavies.rds.path,"b.preds.rds")) # raster stack
s.raster.list.names = list("Rain", "Jant", "Jult", "TWet", "Slop", "Soil", "Vegt")
s.raster.list       = readRDS(paste0(heavies.rds.path,"s.raster.list.rds"))
s.preds             = readRDS(paste0(heavies.rds.path,"s.preds.rds")) # raster stack

# observational datasets:
beershebensis.buffer = readRDS("./rds_objects/beershebensis.buffer_incWB.rds"); plot(beershebensis.buffer)
bs       = readRDS("./rds_objects/b.surveys.rds")
bc       = readRDS("./rds_objects/b.collections.rds");     length(bc);  length(unique(bc$lat)); length(unique(bc$lon))
bsp      = readRDS("./rds_objects/b.surveys.present.rds"); length(bsp); length(unique(c(bsp$WGS84_lat)))
bsa      = readRDS("./rds_objects/b.surveys.absent.rds")
bc.r     = readRDS("./rds_objects/b.collections.reliables.rds"); plot(bc.r); length(bc.r)
bsa.r    = readRDS("./rds_objects/b.surveys.absence.reliables.rds")

schreiberi.buffer    = readRDS("./rds_objects/schreiberi.buffer.rds"); plot(schreiberi.buffer)
ss.full              = readRDS("./rds_objects/ss.full.rds")   # all schreiberi survey data (errors removed)
ssp.full             = readRDS("./rds_objects/ssp.full.rds")  # presences
ssa.full             = readRDS("./rds_objects/ssa.full.rds")  # absences

sc.full              = readRDS("./rds_objects/sc.full.rds")   # s = schreiberi, c = collections
sc.nodups            = readRDS(paste0(heavies.rds.path,"sc.nodups.rds"))                # from sript 2a; elsa calc
sc.r                 = readRDS(paste0(heavies.rds.path,"s.collections.reliables.rds"))  # from script 2b: elsa calc

# data packages:
b_package_names = readRDS("./rds_objects/b_package_names.rds")
b_data_packages = readRDS("./rds_objects/b_data_packages.rds")

# for Schreiberi the '6-scenario' option: inclused with and without absences, and collections data, altogether
s.6scen.scen.names             = readRDS("./rds_objects/s.6scen.scen.names.rds")
s.6scenario.descriptions       = readRDS("./rds_objects/s.6scenario.descriptions.rds")
s.6scen.data.packages          = readRDS("./rds_objects/s.6scen.data.packages.rds")

# List objects by size ----
# from https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session?rq=1 
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

lsos()

#########################################################################################################
# Datasets I prepared earlier in this script (not comprehensive) ----

# retreive evaluation summary data
# eval.list       = readRDS("./rds_objects/b.eval.list.5fold.100reps.rds")  # 'raw' list of full eval data from each combo
# eval.summary    = readRDS("./rds_objects/b.eval.summary.5fold.100reps.rds")    # consolidated list form
# eval.summary.df = readRDS("./rds_objects/b.eval.summary.df.5fold.100reps.rds") # consolidated table form

# s.6scen.model.list.cv.100n = readRDS(paste0(heavies.rds.path, "s.6scen.model.list.cv.100n.rds")) # huge object

s.eval.list    = readRDS("./rds_objects/s.eval.list.5fold.100reps.rds")          # 'raw' list of eval data from each model
s.eval.summary = readRDS("./rds_objects/s.eval.summary.5fold.100reps.rds")       # consolidated: multiple reps averaged
s.eval.summary = readRDS("./rds_objects/s.eval.summary.topmethods.rds")       # consolidated: multiple reps averaged
s.eval.summary.df = readRDS("./rds_objects/s.eval.summary.df.5fold.100reps.rds") # superconsolidate:all scenarios, 1table)

# "./output_data/s.model evaluation summary(5fold,100reps).xlsx") # export to excel sprdsheet
# "./output_data/s.model evaluation summary onetable(5fold,100reps2).xlsx")

########################################################################################################
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

########################################################################################################
# Exploration of the effect of the different evaluation approaches ----
# Subsampling evaluation; 50 runs taking 30 percent as test:
modelA.data = b_data_packages[[1]]
a.sb = sdm(occurrence ~ ., data = modelA.data, methods =c("cart",'fda','gam','brt','rf','glm', 'glmnet',
                                                          'mda','rpart','svm'), # maxent method not available
                                                           n=50, replication = 'sub', test.percent = 30); roc(a.sb, smooth = T)

# Cross-validation evaluation; 10 runs, 5 folds, taking 30 percent as test: 
a.cv= sdm(occurrence ~ ., data = modelA.data, methods =c("cart",'fda','gam','brt','rf','glm', 'glmnet',
                                                         'mda','rpart','svm'), 
                                                          n=10, replication = 'cv', cv.folds=5, test.percent = 30); roc(a.cv, smooth = T)

# Bootstrap evaluation: 50 runs, taking 30 percent as test:
a.bt= sdm(occurrence ~ ., data = modelA.data, methods =c("cart",'fda','gam','brt','rf','glm', 'glmnet',
                                                         'mda','rpart','svm'), 
                                                          n=50, replication = 'boot', test.percent = 30); roc(a.bt, smooth = T)

# Combining Cross-validation and bootstrapping evaluation methods:
# a.cb = sdm(occurrence ~ ., data = modelA.data, methods =c("cart",'fda','gam','brt','rf','glm', 'glmnet',                                                          'mda','rpart','svm'), n=10, replication = c('cv','boot'),cv.folds=5, test.percent = 30)

a.sb; a.sb.e = getEvaluation(a.sb)
a.cv; a.cv.e = getEvaluation(a.cv)
a.bt; a.bt.e = getEvaluation(a.bt)
# a.cb; a.cb.e = getEvaluation(a.cb)
# I've copied the outputs into my SDM word document to make for easy comparison, and concluded that the different eval methods vary substantially, though are fairly consistent in their relative ranks. Subsampling is the harshest, bootstrap is the kindest, cv is almost as harsh as subsampling. Using a combo of CV and Bootstrap is probably good. Actually, no, as that will produce different evaluation info for each evaluation method, different ROC curves and all. too much. Let's just do cross-validation. 

# par(mar=c(2,2,2,2))  # sets the bottom, left, top and right margins respectively
# png("roc.a.sb.png", width = 30, height = 20, units = "cm", res = 600) 
# roc(a.sb,smooth=TRUE); dev.off()
# 
# png("roc.a.cv.png", width = 30, height = 20, units = "cm", res = 600) 
# roc(a.cv,smooth=TRUE); dev.off()
# 
# png("roc.a.bt.png", width = 30, height = 20, units = "cm", res = 600) 
# roc(a.bt,smooth=TRUE); dev.off()
# 
# png("roc.a.cb.png", width = 30, height = 20, units = "cm", res = 600) 
# roc(a.cb,smooth=TRUE); dev.off()
# end of exploration of different evaluation approaches. Eval method to go forward: cv.

# 
########################################################################################################
# Create evaluation models for each dataset combination: Beershebensis -----
# Define input list, output list, and function 

# define function with 'n' runs, 'cv.folds' folds cross-validation, taking 30 percent as test: 
sdm.cv = function(data) {
  sdm(occurrence ~ ., data = data, methods =c("cart",'fda','gam','brt','rf','glm', 'mda','rpart','svm'), 
      n=100, replication = 'cv', cv.folds=5) }
# See "Exploration of the effect of the different evaluation approaches" and below for other ways to evaluate.

# create models for each combo 
model.list.cv.100n = list()

for (i in 1:length(b_data_packages))                                                                        {
  start.time = Sys.time()
  print(b_package_names[i])
  data = b_data_packages[[i]]
  model.list.cv.100n[[i]] = sdm.cv(data)  
  print(paste(b_package_names[[i]],"loop took", difftime(Sys.time(), start.time, units="mins"), "minutes"))   }

# saveRDS(model.list.cv.100n, "//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/model.list.cv.100n.rds")
emailme()

# # modelling with subsampling to save time (not doing this any more)
{
# model.list.sub = list()
# sdm.sub = function(data) {
#   sdm(occurrence ~ ., data = data, methods =c("cart",'fda','gam','brt','rf','glm', 'mda','rpart','svm'), 
#       n=5, replication = 'sub', test.percent = 30) }
# 
# for (i in 1:length(b_data_packages))                                                             {
#   start_time = Sys.time()
#   print(b_package_names[i])
#   data = b_data_packages[[i]]
#   model.list.sub[[i]] = sdm.sub(data)  
#   print(paste(b_package_names[[i]]," loop took ", (Sys.time()-start_time)," minutes/.seconds"))  }
}
  
# # see how they went:
# b_package_names[[1]]; model.list.cv.100n[[1]]
# b_package_names[[2]]; model.list.cv.100n[[2]]
# b_package_names[[3]]; model.list.cv.100n[[3]]
# b_package_names[[4]]; model.list.cv.100n[[4]]
# b_package_names[[5]]; model.list.cv.100n[[5]]
# b_package_names[[6]]; model.list.cv.100n[[6]]
# b_package_names[[7]]; model.list.cv.100n[[7]]
# b_package_names[[8]]; model.list.cv.100n[[8]]
# b_package_names[[9]]; model.list.cv.100n[[9]]

# save and/or retreive
# saveRDS(model.list.cv.100n,  "//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/model.list.cv.100n.rds")

# or retreive from saved to prevent running the above code:
# model.list.cv.100n  = readRDS( "//vscifs.cc.huji.ac.il/eeb/HawlenaLab/keren/R/sdm_heavies/rds/model.list.cv.100n.rds")
########################################################################################################
# Create evaluation models for each dataset combination: Schreiberi -----
# Define input list, output list, and function 

# define function with 'n' runs, 'cv.folds' folds cross-validation, taking 30 percent as test: 
sdm.cv = function(data) {
  sdm(occurrence ~ ., data = data, methods = c('rf','gam','svm'), # temporary removal: "cart",'fda','gam','brt','rf','glm','mda','rpart','svm'
      n=20, replication = 'cv', cv.folds = 5) }

# create models for each combo 
scenario.names = s.6scen.scen.names
s.eval.models.topmethods = list()

for (i in 1:length(scenario.names))                   { 
  start.time = Sys.time()
  print(scenario.names[i])
  data = s.6scen.data.packages[[i]]
  s.eval.models.topmethods[[i]] = sdm.cv(data)  
  print(paste(scenario.names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes")) }

saveRDS(s.eval.models.topmethods, paste0(heavies.rds.path,  "s.eval.model.list.topmethods_wkkl.rds"))

emailme()
Sys.time()

# # see how they went:
s.6scen.scen.names[[1]]; s.eval.model.list.topmethods[[1]]
s.6scen.scen.names[[2]]; s.eval.model.list.topmethods[[2]]
s.6scen.scen.names[[3]]; s.eval.model.list.topmethods[[3]]
s.6scen.scen.names[[4]]; s.eval.model.list.topmethods[[4]]
s.6scen.scen.names[[5]]; s.eval.model.list.topmethods[[5]]
s.6scen.scen.names[[6]]; s.eval.model.list.topmethods[[6]]

########################################################################################################
# Summarise model evaluations by methods for each dataset combination (Same code for Beersheb & Sch-----
eval.list = list()    # evaluation data, by model (i.e. 4500 models; 100reps x 5 folds x 9 algorithims, for each of 9 datacombos)
eval.summary = list() # list of evaluation data, averaged by algorithim (i.e. 9 dataframes of 9 rows: 9 data combos X 9 algorithms)

modelset = s.eval.models.topmethods # to prevent multiple repetitions of exact model set name below:
for (i in 1:length(modelset))                                 {
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

# Schreiberi: comparing before and after adding the beershebensis-survey absences in:
options(digits=2)
eval.summary[[1]]$TSS   # 0.64 0.50 0.69 0.65 0.78 0.56 0.57 0.65 0.62 new ones
s.eval.summary[[1]]$TSS[] # 0.58 0.47 0.68 0.61 0.76 0.52 0.56 0.61 0.55 old
eval.summary[[2]]$TSS   # 0.65 0.51 0.71 0.66 0.79 0.57 0.57 0.67 0.64 new ones
s.eval.summary[[2]]$TSS # 0.59 0.49 0.70 0.62 0.77 0.53 0.56 0.63 0.58 old. 
eval.summary[[3]]$TSS   # 0.69 0.53 0.71 0.68 0.81 0.58 0.62 0.70 0.73 new ones
s.eval.summary[[3]]$TSS # 0.64 0.49 0.68 0.64 0.79 0.55 0.60 0.65 0.69 old. 
# New ones, i.e. with additional absences included, are consistently better. So replace first 3 of old eval.summary:
summary(s.eval.list)
s.eval.list[[3]]    = eval.list[[3]]    # 1, 2, then 3: replaced digits.
s.eval.summary[[3]] = eval.summary[[3]] # 1, 2, then 3: replaced digits.
s.eval.summary[[6]]$data_combo = "Schreiberi Scenario F"
s.eval.summary.df = do.call("rbind", s.eval.summary) # converting the list to a single dataframe for easy plotting

# Schreiberi: comparing before and after reducing study area size and absence data (worse TSS, to be expected):
options(digits=3)
eval.summary[[1]]$TSS             # 0.77 0.69 0.60 new ones worse
s.eval.summary[[1]]$TSS[c(5,3,9)] # 0.78 0.69 0.62 old
eval.summary[[2]]$TSS             # 0.78 0.70 0.62 new ones worse
s.eval.summary[[2]]$TSS[c(5,3,9)] # 0.79 0.71 0.64 old. 
eval.summary[[3]]$TSS             # 0.81 0.70 0.72 new ones worse
s.eval.summary[[3]]$TSS[c(5,3,9)] # 0.81 0.71 0.73 old.
eval.summary[[4]]$TSS             # 0.840 0.786 0.811 new ones worse
s.eval.summary[[4]]$TSS[c(5,3,9)] # 0.854 0.791 0.832 old
eval.summary[[5]]$TSS             # 0.799 0.733 0.759 new ones worse
s.eval.summary[[5]]$TSS[c(5,3,9)] # 0.799 0.742 0.761 old. 
eval.summary[[6]]$TSS             # 0.828 0.791 0.797 new ones better
s.eval.summary[[6]]$TSS[c(5,3,9)] # 0.818 0.770 0.794 old.

# Schreiberi: comparing before and after including KKL LU impacts survey data:
options(digits=3)
eval.summary[[1]]$TSS             # 0.776 0.678 0.626 new ones partly better
s.eval.summary[[1]]$TSS           # 0.772 0.686 0.601 old
s.eval.summary[[1]]$TSS[c(5,3,9)] # 0.78  0.69  0.62 prior to reducing study area size and absence data (best)

eval.summary[[2]]$TSS             # 0.784 0.701 0.648 new ones better
s.eval.summary[[2]]$TSS           # 0.782 0.700 0.624 old. 
s.eval.summary[[2]]$TSS[c(5,3,9)] # 0.79  0.71  0.64 prior to reducing study area size and absence data (best)

eval.summary[[3]]$TSS             # 0.807 0.688 0.728 new ones better
s.eval.summary[[3]]$TSS           # 0.806 0.700 0.721 old.
s.eval.summary[[3]]$TSS[c(5,3,9)] # 0.81  0.71  0.73 prior to reducing study area size and absence data (best)

eval.summary[[4]]$TSS             # 0.832 0.744 0.789 new ones worse
s.eval.summary[[4]]$TSS           # 0.840 0.786 0.811 old
s.eval.summary[[4]]$TSS[c(5,3,9)] # 0.854 0.791 0.832 prior to reducing study area size and absence data (best by far)

eval.summary[[5]]$TSS             # 0.820 0.773 0.797 new ones better
s.eval.summary[[5]]$TSS           # 0.799 0.733 0.759 old. 
s.eval.summary[[5]]$TSS[c(5,3,9)] # 0.799 0.742 0.761 prior to reducing study area size and absence data (worse)

eval.summary[[6]]$TSS             # 0.821 0.758 0.780 new ones worse
s.eval.summary[[6]]$TSS           # 0.828 0.791 0.797 old (best)
s.eval.summary[[6]]$TSS[c(5,3,9)] # 0.818 0.770 0.794 prior to reducing study area size and absence data (mixed better/worse)


# saving the outputs from the 5-fold, 100-replicate run evaluation
saveRDS(eval.list,       "./rds_objects/s.eval.list.topmethods.rds")       # 'raw' list of eval data from each model
saveRDS(eval.summary,    "./rds_objects/s.eval.summary.topmethods.rds")    # consolidated: multiple reps averaged
saveRDS(eval.summary.df, "./rds_objects/s.eval.summary.df.topmethods.rds") # superconsolidate:all scenarios, 1 table)
write.xlsx(eval.summary,    "./output_data/s.model evaluation summary(topmethods).xlsx") # export to excel sprdsheet
write.xlsx(eval.summary.df, "./output_data/s.model evaluation summary onetable(topmethods).xlsx")

# Plot relationship between TSS and AUC
AUC_ordered = ordered(eval.summary.df$AUC)
TSS_lower = eval.summary.df$TSS - eval.summary.df$TSS_sd
TSS_upper = eval.summary.df$TSS + eval.summary.df$TSS_sd

r = ggplot(eval.summary.df, aes(x=AUC, y=TSS, color = data_combo)) + geom_point() 
r
cor(eval.summary.df$TSS, eval.summary.df$AUC) # extremely high correlation: 0.98377 for B. 0.9615/0.958 for Shfela.

# can't get the smooth line/ribbon aspects to work; leaving it for now.
# + geom_ribbon(aes(x= eval.summary.df$AUC, ymin = TSS_lower, ymax = TSS_upper))
#  aes(x=time, ymin=p.shreiberi.lower, ymax=p.shreiberi.upper), alpha=0.2)
# geom_ribbon(aes(x=time, ymin=p.shreiberi.lower, ymax=p.shreiberi.upper), alpha=0.2) +
#  scale_fill_manual(values = c("Min. temp"="blue", "Mean temp"="green", "Max. temp"="red"),guide=FALSE) +
#  theme_bw() +
#  theme(legend.position=c(0.465,0.775), legend.text = element_text(size=7)) + 
#  labs(x="Time of day (hours)", y=expression(italic("A. s")~"observations 10,000 m"^-2~hr^-1))

########################################################################################################
# Definining 'top' algorithims for future focus ----
# I'm going to use TSS to define the topmodels - we have already seen that this is highly correlated with AUC.
# To my knowledge; can't extract top models but can specify which ones to use when drawing ROC curves and running ensembles.

# Approach 1: select top algorithms by frequency in top 5 list (by TSS) for each dataset combination ----

# Extract and order top 5 models from each dataset combination:
top5 = list()
for (i in 1:length(eval.summary))                   {
top5[[i]] = eval.summary[[i]][rank(eval.summary[[i]]$TSS)>=5,]
top5[[i]] = top5[[i]][order(-top5[[i]]$TSS),]       }

top5_df = do.call("rbind", top5)

# extract how many times each method appears in the top 5:
methods = list("cart","fda","gam","brt","rf","glm","mda","rpart","svm")
methods.points = list()
for (i in 1:length(methods))                                                                 {
  methods.points[[i]] = data.frame(method = methods[[i]],   # number of times appeared in top five is named 'points'.
                                    points = length(which(top5_df$method == methods[[i]])))  }
methods.points.df = do.call("rbind", methods.points)
(methods.points.df = methods.points.df[order(-methods.points.df$points),] )
# gam, brt, rf, and svm all performed equally well, all appearing in all of the top 5s for each data combination.

# Approach 2: selecting by average TSS and AUC values (the one I'm using) ----
methods.summary = aggregate(eval.summary.df[, c("TSS", "AUC")],
                  eval.summary.df["method"],
                  function(x) c(mean=mean(x), sd=sd(x)) )
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
write.xlsx(methods.summary,"./output_data/s.methods.summary.xlsx") # export to excel sprdsheet

# plot TSS by method:
methods.summary$method = factor(methods.summary$method, levels = methods.summary$method[order(-methods.summary$TSS.mean)])

# plot with error bars:
palette(c("red","magenta","blue","cyan","green3","brown","gray","purple","yellow")) # adding&changing colour palette (default has 8 only)
algorithm = 1:9
lower.end = methods.summary$TSS.mean - methods.summary$TSS.sd
upper.end = methods.summary$TSS.mean + methods.summary$TSS.sd

png(filename = paste0(heavies.image.path,"S.evalmodels.TSS.withbars.png"), width=20, height=15, units='cm', res=600)
par(mgp=c(2,0.5,0),mar=c(3.5,3.5,4,2)+0.1)
plot(methods.summary$TSS[,1], bg=methods.summary$method, pch=21, cex=1.5, 
     ylim=range(c(lower.end,upper.end)),xlim=c(0,10), xlab="Algorithm",xaxt='n',ylab="Average TSS")
with(methods.summary, text(methods.summary$TSS.mean~rownames(methods.summary),labels=methods.summary$method,pos=2,cex=1))
#legend("topright", c('rf','svm','gam','brt','mda','cart','rpart','glm','fda'), pt.bg=c(1:9), pch=21, cex=1.5,text.font=1)
arrows(algorithm, lower.end, algorithm,   upper.end,  length=0.05, angle=90, code=3)
points(methods.summary$TSS.mean, bg=methods.summary$method, pch=21, cex=1.5) # just putting points on top of lines
dev.off()

# plot without error bars:
png(filename = paste0(heavies.image.path,"S.evalmodels.TSS.nobars.png"), width=20, height=15, units='cm', res=600)
par(mgp=c(2,0.5,0),mar=c(3.5,3.5,4,2)+0.1)
plot(methods.summary$TSS.mean, bg=methods.summary$method, pch=21, cex=1.5, xlab="Algorithm",xaxt='n',ylab="Average TSS")
with(methods.summary, text(methods.summary$TSS.mean~rownames(methods.summary), labels = methods.summary$method, pos = 4, cex=1))
# legend("topright", c("rf","svm","gam","brt","mda","cart","rpart","glm","fda"), pt.bg=c(1:9), pch=21,cex=1.5,text.font=1)
# arrows(algorithm, lower.end, algorithm,   upper.end,  length=0.05, angle=90, code=3)
# points(methods.summary$TSS[,1], bg=methods.summary$method, pch=21, cex=1.5) # just putting points on top of lines
dev.off()

# Plot TSS by SD for all models (summarised by combo-alg combination)
png(filename = paste0(heavies.image.path,"S.evalmodels.TSS-SDs.png"), width=20, height=15, units='cm', res=600)
plot(eval.summary.df$TSS, eval.summary.df$TSS_sd, pch=21, bg=eval.summary.df$method, 
     xlab="TSS", ylab="TSS standard deviation")
points(eval.summary.df$TSS[eval.summary.df$method == "rf"], eval.summary.df$TSS_sd[eval.summary.df$method == "rf"],
       pch=21, bg= "green3", cex=1.2)
points(eval.summary.df$TSS[eval.summary.df$method == "svm"], eval.summary.df$TSS_sd[eval.summary.df$method == "svm"],
       pch=21, bg= "yellow", cex=1.2)
points(eval.summary.df$TSS[eval.summary.df$method == "gam"], eval.summary.df$TSS_sd[eval.summary.df$method == "gam"],
       pch=21, bg= "blue", cex=1.2)
legend("bottomleft", c("cart","fda","gam","brt","rf","glm","mda","rpart","svm"), pt.bg=c(1:9), pch=21, cex=1.1)
dev.off()
# Performance of the top 3 methods is as consistent as any other methods. The SD of TSS for RF models in particularly low.

# Approach 3: get all methods above certain TSS and AUC levels (obsolete) ----

TSS_topmods_methods = list()
AUC_topmods_methods = list()
for (i in 1:9)                                                                     {
  TSS_topmods_methods[[i]] = paste(subset(eval.summary[[i]], TSS > 0.5)$method)
  AUC_topmods_methods[[i]] = paste(subset(eval.summary[[i]], AUC > 0.755)$method)   } # or use AUC = 0.7811

# comparing the top models according to TSS versus AUC:
i=1
print(paste('Model combination ',i)); TSS_topmods_methods[[i]]; AUC_topmods_methods[[i]]; i=i+1
print(paste('Model combination ',i)); TSS_topmods_methods[[i]]; AUC_topmods_methods[[i]]; i=i+1
print(paste('Model combination ',i)); TSS_topmods_methods[[i]]; AUC_topmods_methods[[i]]; i=i+1
print(paste('Model combination ',i)); TSS_topmods_methods[[i]]; AUC_topmods_methods[[i]]; i=i+1
print(paste('Model combination ',i)); TSS_topmods_methods[[i]]; AUC_topmods_methods[[i]]; i=i+1
print(paste('Model combination ',i)); TSS_topmods_methods[[i]]; AUC_topmods_methods[[i]]; i=i+1
print(paste('Model combination ',i)); TSS_topmods_methods[[i]]; AUC_topmods_methods[[i]]; i=i+1
print(paste('Model combination ',i)); TSS_topmods_methods[[i]]; AUC_topmods_methods[[i]]; i=i+1
print(paste('Model combination ',i)); TSS_topmods_methods[[i]]; AUC_topmods_methods[[i]]; i = 1
# When TSS threshold is 0.5 and AUC threshold is 0.7, in some cases AUC is more generous (considers more models to be 'top'), in some cases the two indicators indicate the same set of 'top' methods.
# if AUC threshold is increased to 0.7811, then the methods considered 'top' by both methods are almost identical (once I remove glmnet).

# what's going on with model combination 3?
# I went through and double-checked the input data to ensure that there are no mistakes. 
b_data_packages[[3]]
#sdm(occurrence ~ ., data = b_data_packages[[3]], methods =c("cart",'fda','gam','brt','rf','glm', 'mda','rpart','svm'), n=11, replication = 'cv', cv.folds=5)
eval.summary[[3]] # it's just a very low performer. Interesting; because it's essentially just all the survey data.
TSS_topmods_methods[[3]]  = paste(subset(eval.summary[[3]], TSS > 0.25)$method)
(AUC_topmods_methods[[3]] = paste(subset(eval.summary[[3]], AUC > 0.5555)$method))

# to plot ROC curves of topmodels:
roc(model.list.cv.100n[[i]][1], method = TSS_topmods_methods[[i]], smooth = T, cex.lab = 0.7, cex.main = 0.8, pin= c(5.5, 5.3))

# Outputs from top model selection ----
write.xlsx(methods.summary, "./output_data/model evaluation summary (aggregated by method)2.xlsx")
# top models are rf, SVM, and gam

########################################################################################################
# ROC curves for all models ----
b_package_names # exisitng list of packages
par("oma") # outer margin margins - can leave these at zero.
par(mar=c(1,1,1,1))  # sets the bottom, left, top and right margins respectively. Units are 'lines of text'.
methods = c("rf","svm","gam","brt","mda","cart","rpart","glm","fda")

for (i in 1:length(b_package_names)) {
 start.time = Sys.time()
 filename = paste0(heavies.image.path,b_package_names[[i]],' - ROC all methods.png')
 png(filename = filename, width = 15, height = 16, units = "cm", res = 600)
 roc(model.list.cv.100n[[i]], method = methods, smooth = T, cex.lab = 0.8, cex.main = 0.8)
 dev.off() 
 print(paste(b_package_names[[i]],"loop took", difftime(Sys.time(),start.time, units="mins"), "minutes")) }

# ROC curves for top 3 models ----
# step 1: run just the top models (optional)  ----

# make topmodels list (to avoid remaking full model list): 
{
  sdm.tops = function(data) {sdm(occurrence ~ ., data = data, methods = c('gam','rf','svm'), 
      n=100, replication = 'cv', cv.folds=5) }

# create models for each combo 
model.list.cv.tops = list()

for (i in 1:length(s.6scen.scen.names))                                                                        {
  start.time = Sys.time();  print(s.6scen.scen.names[i])
  data = s.6scen.data.packages[[i]]
  model.list.cv.tops[[i]] = sdm.tops(data)  
  print(paste(s.6scen.scen.names[[i]],"loop took", difftime(Sys.time(), start.time, units="mins"), "minutes"))   }
saveRDS(model.list.cv.tops, paste0(heavies.rds.path,"S.model.list.cv.tops.rds"))
  
emailme()
}
# or just designate the model set from above that you're using:

modelset = model.list.cv.tops # to prevent multiple repetitions of exact model set name below:

# step 2: make the curves and save to .png individually -----
# step 2a: run the first combo (top row) separately, to give it titles:
i = 1
start_time = Sys.time()
combo.letter = list ("A","B","C","D","E","F","G","H","I")

  filename = paste0(heavies.image.path,"ROC ", s.6scen.scen.names[[i]],' - RF.png')
  png(filename = filename, width = 9.975, height = 5.7, units = "cm", res = 300)
  par(mar = c(0,2,1.5,0), mgp=c(3, 0.5, 0), las=1) # bottom, left, top, and right
  roc(modelset[[i]], method = "rf",  smooth = T, cex.axis = 0.9, tck=-0.03, cex.main = 1.4, main= "Random forests")
  text(x = 0.03, y = 0.94, labels = combo.letter[[i]],cex = 1.3, xpd = NA)
  dev.off()
  
  filename = paste0(heavies.image.path,"ROC ", s.6scen.scen.names[[i]],' - SVM.png')
  png(filename = filename, width = 9.5, height = 5.7, units = "cm", res = 300)
  par(mar = c(0,0,1.5,0)) # bottom, left, top, and right
  roc(modelset[[i]],method = "svm",  smooth = T, cex.main = 1.4, main= "Support vector machines")
  dev.off()
  
  filename = paste0(heavies.image.path,"ROC ", s.6scen.scen.names[[i]],' - GAM.png')
  png(filename = filename, width = 9.5, height = 5.7, units = "cm", res = 300)
  par(mar = c(0,0,1.5,0)) # bottom, left, top, and right
  roc(modelset[[i]],method = "gam",  smooth = T, cex.main = 1.4, main= "Generalised additive models")
  dev.off()
  
  print(paste(s.6scen.scen.names[[i]],"loop took", difftime(Sys.time(),start_time, units="mins"), "minutes")) 
  
# step 2b: run the middle rows (no titles, no x-axis labels):
for (i in 2:(length(s.6scen.scen.names)-1))                                                                  {
  start_time = Sys.time()
  
  filename = paste0(heavies.image.path,"ROC ", s.6scen.scen.names[[i]],' - RF.png')
  png(filename = filename, width = 9.975, height = 5.225, units = "cm", res = 300)
  par(mar = c(0,2,0,0), mgp=c(3, 0.5, 0), las=1) # bottom, left, top, and right
  roc(modelset[[i]],method = "rf",  smooth = T, cex.axis = 0.9, tck=-0.02, main = NULL)
  text(x = 0.03, y = 0.94, labels = combo.letter[[i]],cex = 1.3, xpd = NA)
  dev.off() 
  
  filename = paste0(heavies.image.path,"ROC ", s.6scen.scen.names[[i]],' - SVM.png')
  png(filename = filename, width = 9.5, height = 5.225, units = "cm", res = 300)
  par(mar = c(0,0,0,0)) # bottom, left, top, and right
  roc(modelset[[i]],method = "svm",  smooth = T, main = NULL)
  dev.off()
  
  filename = paste0(heavies.image.path,"ROC ", s.6scen.scen.names[[i]],' - GAM.png')
  png(filename = filename, width = 9.5, height = 5.225, units = "cm", res = 300)
  par(mar = c(0,0,0,0)) # bottom, left, top, and right
  roc(modelset[[i]],method = "gam",  smooth = T, main = NULL)
  dev.off()
  
  print(paste(s.6scen.scen.names[[i]],"loop took", difftime(Sys.time(),start_time, units="mins"), "minutes"))}

# step 2c: run the last combo separately, to give it X-axis labels: 
i = 6 {
start_time = Sys.time()

  filename = paste0(heavies.image.path,"ROC ", s.6scen.scen.names[[i]],' - RF.png')
  png(filename = filename, width = 9.975, height = 6.65, units = "cm", res = 300)
  par(mar = c(3,2,0,0), mgp=c(3, 0.5, 0), las=1) # bottom, left, top, and right
  roc(modelset[[i]],method = "rf",  smooth = T, cex.axis = 0.9, tck=-0.02, main= NULL)
  text(x = 0.02, y = 0.95, labels = combo.letter[[i]],cex = 1.3, xpd = NA)
  dev.off()
  
  filename = paste0(heavies.image.path,"ROC ", s.6scen.scen.names[[i]],' - SVM.png')
  png(filename = filename, width = 9.5, height = 6.65, units = "cm", res = 300)
  par(mar = c(3,0,0,0), mgp=c(3, 0.5, 0), las=1) # bottom, left, top, and right
  roc(modelset[[i]],method = "svm",  smooth = T, cex.lab = 0.7, cex.main = 0.8, main= NULL)
  dev.off()
  
  filename = paste0(heavies.image.path,"ROC ", s.6scen.scen.names[[i]],' - GAM.png')
  png(filename = filename, width = 9.5, height = 6.65, units = "cm", res = 300)
  par(mar = c(3,0,0,0), mgp=c(3, 0.5, 0), las=1) # bottom, left, top, and right
  roc(modelset[[i]],method = "gam",  smooth = T, cex.lab = 0.7, cex.main = 0.8, main= NULL)
  dev.off()
}
  print(paste(s.6scen.scen.names[[i]],"loop took", difftime(Sys.time(),start_time, units="mins"), "minutes")) 

# step 3: combine plots (for want of a better way to do this!) ----

# ROC - All combinations - top models plot for Beershebensis:
{
png(filename = paste0(heavies.image.path,"B.ROC - All combinations - top models.png"), 
                      width=26, height=40, units="cm", res=600)
par(mar = c(1,1,0,0), mgp=c(0,0,0)) 
# mgp sets distance between (1) axis titles and axes and (2) axis labels and the axes. Default is mgp = c(3, 0.1, 0).
plot(0:9, 0:9, type = "n", xaxt = "n", yaxt = "n", 
     xlab = "1 - Specificity (false positive rate)", ylab = "Sensitivity (true positive rate)", bty="n")

i=1 # combination A # numbers at end: xstart, ystart, xend, yend
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - RF.png")),  -.3, 7.8, 3, 9) 
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - SVM.png")),   3, 7.8, 6, 9)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - GAM.png")),   6, 7.8, 9, 9)

i=2 # combination B
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - RF.png")),  -.3, 6.8, 3, 7.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - SVM.png")),   3, 6.8, 6, 7.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - GAM.png")),   6, 6.8, 9, 7.8)

1=3 # combination C
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - RF.png")),  -.3, 5.8, 3, 6.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - SVM.png")),   3, 5.8, 6, 6.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - GAM.png")),   6, 5.8, 9, 6.8)

1=4 # combination D
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - RF.png")),  -.3, 4.8, 3, 5.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - SVM.png")),   3, 4.8, 6, 5.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - GAM.png")),   6, 4.8, 9, 5.8)

i=5 # combination E
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - RF.png")),  -.3, 3.8, 3, 4.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - SVM.png")),   3, 3.8, 6, 4.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - GAM.png")),   6, 3.8, 9, 4.8)

i=6 # combination F
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - RF.png")),  -.3, 2.8, 3, 3.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - SVM.png")),   3, 2.8, 6, 3.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - GAM.png")),   6, 2.8, 9, 3.8)

i=7 # combination G
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - RF.png")),  -.3, 1.8, 3, 2.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - SVM.png")),   3, 1.8, 6, 2.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - GAM.png")),   6, 1.8, 9, 2.8)

i=8 #combination H
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - RF.png")),  -.3, 0.8, 3, 1.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - SVM.png")),   3, 0.8, 6, 1.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - GAM.png")),   6, 0.8, 9, 1.8)

i=9 # combination I
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - RF.png")),  -.3, -.4, 3, 0.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - SVM.png")),   3, -.4, 6, 0.8)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",b_package_names[[i]], " - GAM.png")),   6, -.4, 9, 0.8)

dev.off()
}

# ROC - All combinations - top models plot for Schreiberi:
{
png(filename = paste0(heavies.image.path,"S.ROC - All combinations - top models.png"), 
                      width=26, height=27, units="cm", res=600)
par(mar = c(1,1,0,0), mgp=c(0,0,0)) 
# mgp sets distance between (1) axis titles and axes and (2) axis labels and the axes. Default is mgp = c(3, 0.1, 0).
plot(0:6, 0:6, type = "n", xaxt = "n", yaxt = "n", xlab = "1 - Specificity (false positive rate)", 
     ylab = "Sensitivity (true positive rate)", bty="n")

i=1 # combination A              # numbers at end: xstart, ybottom, xend, ytop
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - RF.png")),  -.2,   5, 2, 6.2) 
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - SVM.png")),   2,   5, 4, 6.2)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - GAM.png")),   4,   5, 6, 6.2)

i=2 # combination B
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - RF.png")),  -.2,   4, 2, 5)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - SVM.png")),   2,   4, 4, 5)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - GAM.png")),   4,   4, 6, 5)

i=3 # combination C
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - RF.png")),  -.2,   3, 2, 4)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - SVM.png")),   2,   3, 4, 4)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - GAM.png")),   4,   3, 6, 4)

i=4 # combination D
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - RF.png")),  -.2,   2, 2, 3)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - SVM.png")),   2,   2, 4, 3)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - GAM.png")),   4,   2, 6, 3)

i=5 # combination E
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - RF.png")),  -.2,   1, 2, 2)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - SVM.png")),   2,   1, 4, 2)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - GAM.png")),   4,   1, 6, 2)

i=6 # combination F
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - RF.png")),  -.2, -.3, 2, 1)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - SVM.png")),   2, -.3, 4, 1)
rasterImage(readPNG(source = paste0(heavies.image.path,"ROC ",s.6scen.scen.names[[i]]," - GAM.png")),   4, -.3, 6, 1)

dev.off()
}
  
# can extract ROC values to plot mannually, but a bit time consuming!
# ROC = list() 
# for ... loop...
# ROC = getRoc(model.list.cv.100n[[i]],method = "gam", smooth = T)
# lines(ROC[[i]][,1] ~ ROC[[i]][,2])
