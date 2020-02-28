# SDM etc

# 1. housekeeping ----
setwd("C:/Users/owner/Dropbox/Postdoc/data/R")
# install.packages("installr"); install.packages("raster"); install.packages("biomod2")
library(installr) #install+load installr
updateR() # use installr to update R.
library(biomod2)
# install.packages("tiff") 
# install.packages("ggplot2"); install.packages("plyr");install.packages("car"); install.packages("lmerTest"); 
library(tiff); 
# library(ggplot2); library(plyr); library(lattice); library(car); library(beepr);require(lmerTest); library(nlme); library(MuMIn)
# source("HighstatLibV6.R")

shp <- read.csv(file = "20180712 Shfela_24.1 Occurrence points with envirionmental data.csv", header = TRUE) # shp = shfela presence
str(shp); names(shp)
summary(shp)
as.factor(shp$vegt); table(shp$vegt)
summary(shp$vegt)

sha <- read.csv(file = "20180712 Shfela_24.1 absence points with envirionmental data.csv", header = TRUE) # shp = shfela presence
str(sha); names(sha)
summary(sha)
as.factor(sha$vegt); table(sha$vegt)
summary(sha$vegt)

# looking at the individual factors within presence data

as.factor(shp$vegt); table(shp$vegt)
aggregate(data.frame(count = vegt), list(value = vegt), length)

hist(rain, breaks=20)
hist(jant, breaks=20)
hist(jult, breaks=20)
hist(jant, breaks=20, col="red")
hist(jult, breaks=20, add=T, col="green")

# now look at shfela absence
attach(sha)
as.factor(sha$vegt); table(sha$vegt)
as.factor(sha$soilc2); as.data.frame(table(sha$soil2))
aggregate(data.frame(count = vegt), list(value = vegt), length)
table(sha$vegt)
table(shp$vegt)
hist(sha$vegt, breaks=129)
hist(shp$vegt, breaks=129)
shp$presence = "1"
sha$presence = "0"
plot(sh_pa$lon, sh_pa$lat)

sh_pa <- rbind(shp, sha) 
summary(sh_pa)
attach(sh_pa)

ggplot(sh_pa,aes(x=vegt,group=presence,fill=presence))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()

hist(rain, breaks=20)
hist(jant, breaks=20)
hist(jult, breaks=20)
hist(jant, breaks=20, col="red")
hist(jult, breaks=20, add=T, col="green")

aov(shp$vegt, sha$vegt)
