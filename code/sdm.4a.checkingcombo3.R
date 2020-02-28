sdm.4a.checkingcombo3.R

b_data_packages = readRDS("./rds_objects/b_data_packages.rds")

b_data_packages[[3]]

sdm(occurrence ~ ., data = b_data_packages[[3]], methods =c("cart",'fda','gam','brt','rf','glm', 'mda','rpart','svm'), 
    n=11, replication = 'cv', cv.folds=5)
