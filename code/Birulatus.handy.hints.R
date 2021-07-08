# Handy mapping hints

#####
# Some standards for me to use ----

# set code margin to 100 (tools-global options>code>display). International standard is 80, some use 120. This is my compromise between saving line-space and making it easy to display on various veiwers.

#####
# Mapping pretty backgrounds ----
map <- get_stamenmap( bbox = c(left = 110, bottom = -40, right = 160, top = -10), zoom = 4, maptype = "watercolor")
ggmap(map) + 
  theme_void() + 
  ggtitle("Title") + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "grey", fill=NA, size=2)
  )

# Let's check all the possibilities offered by stamen
maptype = c("terrain-labels", "terrain-lines", "toner", "toner-2011",
            "toner-background", "toner-hybrid", "toner-lines",
            "toner-lite", "watercolor")
mylist <- vector("list", length(maptype))
num=0
for(i in maptype ){
  num=num+1
  print(i)
  map <- get_stamenmap( bbox = c(left = 150, bottom = -30, right = 160, top = -25), zoom = 8, maptype = i)
  p = ggmap(map) + theme_void() + ggtitle(i) + theme(plot.title = element_text(colour = "orange"), panel.border = element_rect(colour = "grey", fill=NA, size=2))
  mylist[[num]] <- p
}

# Arrange all this map in the same image with gridExtra:
n <- length(mylist)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(mylist, ncol=nCol))

#####
# Mapping backgrounds with leaflet ----
# install.packages("leaflet")

# Map birthplace of R:
require(leaflet)
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map

# my application:
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=34.8516, lat=31.0461, popup="Israel")
m  # Print the map

# Map Southern Israel:
m <- leaflet() %>% setView(lng=34.8516, lat=31.0461, zoom = 9)
m %>% addTiles()
m %>% addMarkers(bs.survey.pa)

# Putting dots on a map with leaflet
install.packages("rstudio/leaflet")
library(leaflet)
# REGULAR NON-PIPING WAY
ct_only <- filter(dunk, state=="CT") # This is taking the dataframe 'dunk' that has all the locations and information on all Dunkin' Donuts across the country and creating a new one by pulling only the rows with 'CT' in the column 'state'
ct_count <- count(ct_only, city) # This is counting the number of city observations and assigning it to the variable 'ct_count'

# USING THE PIPE OPERATOR
ct_count <- dunk %>% filter(state=="CT") %>% count(city) # This is saying take the 'dunk' data frame, filter it, and then count how often 'city' appears

# Note: the filter() and count() functions are from the 'dplyr' package that you loaded earlier

# Making a map with a marker
m <- leaflet() %>%
  addTiles() %>%  
  setView(-72.690940, 41.651426, zoom = 8) %>%
  addMarkers(lng=-72.690940, lat=41.651426, popup="<b>Hello</b><br><a href='http://www.trendct.org'>-TrendCT.org</a>")
m 


# Making a map from a list of latitude and longitude

ct <- read.csv("ctlist.csv", stringsAsFactors=FALSE) # Brings in the file 'ctlist.csv'

# Be sure to first set the working directory in R to where the file is listed



m <- leaflet(bs.survey.pa) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
                                        attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 

m %>% setView(31, 34, zoom = 8)
m %>% addCircles(~lng, ~lat, popup=bs.survey.pa$presence, weight = 3, radius=40, 
                 
                 color="#ffa500", stroke = TRUE, fillOpacity = 0.8) 

# Add a legend to the map

m <- leaflet(ct) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  
  setView(-72.690940, 41.651426, zoom = 8) %>% 
  
  addCircles(~lng, ~lat, popup=ct$type, weight = 3, radius=40, 
             
             color="#ffa500", stroke = TRUE, fillOpacity = 0.8) %>% 
  
  addLegend("bottomright", colors= "#ffa500", labels="Dunkin'", title="In Connecticut")
m
#####
# Other potentially useful global mapping packages: rworldmap & ggmap ----

require(rworldmap)
mapCountryData() # and so on...

#####
# Using relative file paths ----

# need to have the working directory set, i.e. 
setwd("E:/R SDMs")
# then read in or save out files/objects relative to it, e.g.:
read.table("./R SDMs/filename.extension")

#####
# Read and create shapefiles and geodatabase feature classes ----
require(rgdal)

# from geodatabse (include geodatabase name in path)
groads <- readOGR(dsn="E:/GIS working/layers/infrastructure/gROADS-v1-europe.gdb",layer="groads_israel")

# from shapefile. don't use extension in readOGR function:
bs.surv <- readOGR(dsn="E:/GIS working/layers/shnuniot",layer="Bs_surveyPA_WGS")

#####
# Writing a spatial object to a shapefile ----
# ogrDrivers() use this to get name of driver, if wanting to write to a different format. for now we use "ESRI Shapefile".
writeOGR(bsp, "bsp", driver = "ESRI Shapefile", layer = "bsp.shp")

#####
# Get colours for plots etc ----

colors() 
# or see: http://research.stowers.org/mcm/efg/R/Color/Chart/ColorChart.pdf

#####
# Subsetting data ----

# good source: https://stats.idre.ucla.edu/r/faq/frequently-asked-questions-about-rhow-can-i-subset-a-data-setthe-r-program-as-a-text-file-for-all-the-code-on-this-page-subsetting-is-a-very-important-component/
# subsetting according to a variable condition
x.sub <- subset(x.df, y > 2)
x.sub

# subsetting using mutiple conditional statments:
x.sub1 <- subset(x.df, y > 2 & V1 > 0.6)
x.sub1

# Subsetting both rows and columns
x.sub2 <- subset(x.df, y > 2 & V2 > 0.4, select = c(V1, V4)) # or
x.sub3 <- subset(x.df, y > 3, select = V2:V5)

# Subsetting rows using indices
# Another method for subsetting data sets is by using the bracket notation which designates the indices of the data set. The first index is for the rows and the second for the columns. The x.sub4 data frame contains only the observations for which the values of variable y are equal to 1. Note that leaving the index for the columns blank indicates that we want x.sub4 to contain all the variables (columns) of the original data frame.
x.sub4 <- x.df[x.df$y == 1, ]
# (b.s.p = b.s[b.s$occurrence == 1,]) # birulatus . soils area . presences
# (b.s.a = b.s[b.s$occurrence == 0,]) # birulatus . soils area . absences

# Subsetting rows selecting on more than one value
# We use the %in% notation when we want to subset on multiple values of y. The x.sub5 data frame contains only the observations for which the values of variable y are equal to either 1 or 4.
x.sub5 <- x.df[x.df$y %in% c(1, 4), ]

# Subsetting columns using indices
# We can also use the indices to subset the variables (columns) of the data set. The x.sub6 data frame contains only the first two variables of the x.df data frame. Note that leaving the index for the rows blank indicates that we want x.sub6 to contain all the rows of the original data frame.
x.sub6 <- x.df[, 1:2]

# The x.sub7 data frame contains all the rows but only the 1st, 3rd and 5th variables (columns) of the x.df data set.
x.sub7 <- x.df[, c(1, 3, 5)]

# Subsetting both rows and columns using indices
# The x.sub8 data frame contains the 3rd-6th variables of x.df and only observations number 1 and 3.
x.sub8 <- x.df[c(1, 3), 3:6]

# Subsetting by specific values
towns     = subset(population.centres, subset = category == "Town" | category == "Large town")

#####
# Setting plot title fonts ----

# from: https://www.statmethods.net/advgraphs/parameters.html
# You can easily set font size and style, but font family is a bit more complicated.
# 
# option: description

# font: Integer specifying font to use for text. 
# 1=plain, 2=bold, 3=italic, 4=bold italic, 5=symbol 

# font.axis: font for axis annotation 

# font.lab: font for x and y labels 

# font.main: font for titles 

# font.sub: font for subtitles

# ps: font point size (roughly 1/72 inch)
# text size = ps*cex

# family: font family for drawing text. Standard values are "serif", "sans", "mono", "symbol". Mapping is device dependent. 

# In windows, mono is mapped to "TT Courier New", serif is mapped to"TT Times New Roman", sans is mapped to "TT Arial", mono is mapped to "TT Courier New", and symbol is mapped to "TT Symbol" (TT=True Type). You can add your own mappings. 

#####
# Setting plot parameters ----

# mar – A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).

# mgp – A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).

# las – A numeric value indicating the orientation of the tick mark labels and any other text added to a plot after its initialization. The options are as follows: always parallel to the axis (the default, 0), always horizontal (1), always perpendicular to the axis (2), and always vertical (3).

#####
# Get plots back on plots pane, after finishing to use a new plots window -----
dev.off() # run until output reads null device.
#####
# Making a plot with box plots on the top and side ----
# http://rfunction.com/archives/1538 
#####
# Converting data.frame to SpatialPointsDataFrame ----

# for sdmData in sdm package, in many cases the training data needs to be a SpatialPointsDataFrame (with only one column in its data.frame with the name of species, or value of 1 [presence] if it is single species).
# When you have raster object as predictors, you need Spatial object as training data to make it possible to intersect them together. 
# If you have coordinate columns in your data.frame, you can use coordinates function to turn in into a SpatialPointsDataFrame. I assume that the coordinate columns named long and lat, then:
  
coordinates(dataframe) = ~long+lat
head(dataframe) 
###

# check if in the data.frame you have any additional columns than the column containing the species data. If you have additional columns, and the name of the column with the species records is 'species', then you can use the following to remove the extra columns:

dataframe@data <- dataframe@data[,'species',drop=F]
head(dataframe) 


#####
# Setting or changing a coordiante system for a spatial object ----

coordinates(bs) = ~WGS84_long + WGS84_lat # If needed, replace these names with names of lat and long corresponding columns.
#####
# Merging two dataframes ----

small.towns = population.centres[population.centres$category == "Town", ]
large.towns = population.centres[population.centres$category == "Large town",]
towns       = rbind(small.towns,large.towns)

#####
# Convert vector to a concatenated list ----
paste(1:12) # same as as.character(1:12)
paste("A", 1:6, sep = "")
paste("Today is", date())
c('gam','rf','svm')
#####
# Getting system and RAM info ----
Sys.info() # To check what platform you’re on
gc() # garbage can i.e. empty trash; recommended to use first, to get the true available memory

# only works in windows. specifies in megabytes (1,048,576 bytes): 
memory.size()            # 5,520.  empty brackets defaults to max=F;  if FALSE: the amount currently in use, if NA the memory limit.
memory.size(max = NA)    # 32,663. this is the memory limit.
memory.size(max = TRUE)  # 11,462. the maximum amount of memory obtained from the OS
memory.size(TRUE)        # 11,462. Same as above, max obtained from OS
?memory.size
memory.limit(size = NA)  # 32,663. reports current limit
# I know that my HUJI computer RAM is 32 GB; 1.8 GHz, 4 core. So that makes sense; but it would be good if I could increase the memory obtained from the OS.

# works on Linux:
as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) # on Linux. (i think this reports in kB)
# 10858376 on rstudio cloud. ie 10,858 MB or 10 GB. So slightly less than mine in practice. So not much help.
#####
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

# OR, this version. apparently same as above but written in simple and highly commented style:
#Find the objects       
MemoryObjects = ls()    
#Create an array
MemoryAssessmentTable=array(NA,dim=c(length(MemoryObjects),2))
#Name the columns
colnames(MemoryAssessmentTable)=c("object","bytes")
#Define the first column as the objects
MemoryAssessmentTable[,1]=MemoryObjects
#Define a function to determine size        
MemoryAssessmentFunction=function(x){object.size(get(x))}
#Apply the function to the objects
MemoryAssessmentTable[,2]=t(t(sapply(MemoryAssessmentTable[,1],MemoryAssessmentFunction)))
#Produce a table with the largest objects first
noquote(MemoryAssessmentTable[rev(order(as.numeric(MemoryAssessmentTable[,2]))),])

# OR, a much newer solution:
library(pryr)

object_size(1:10)
## 88 B

object_size(mean)
## 832 B

object_size(mtcars)
## 6.74 kB

# and remember to throw out the rubbish:
gc(reset = T)

#####
# Parallel processing ----
library(parallel)
detectCores()

# The socket approach launches a new version of R on each core. Technically this connection is done via networking (e.g. the same as if you connected to a remote server), but the connection is happening all on your own computer3 I mention this because you may get a warning from your computer asking whether to allow R to accept incoming connections, you should allow it.
# The forking approach copies the entire current version of R and moves it to a new core.

#####
# Understanding memory usage in R ----
# from http://adv-r.had.co.nz/memory.html

devtools::install_github("hadley/lineprof")
library(pryr)
 
object_size(1:10)
object_size(mean)
object_size(mtcars)
sizes <- sapply(0:50, function(n) object_size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Size (bytes)", 
     type = "s")
x <- 1:1e6
object_size(x)
## 4 MB
y <- list(x, x, x)
object_size(y)
## 4 MB
# y isn’t three times as big as x because R is smart enough to not copy x three times; instead it just points to the existing x.
# It’s misleading to look at the sizes of x and y individually. If you want to know how much space they take up together, you have to supply them to the same object_size() call:
  
object_size(x, y)
## 4 MB

# Memory usage and garbage collection
# While object_size() tells you the size of a single object, pryr::mem_used() tells you the total size of all objects in memory:

mem_used()
# 45 MB
# This number won’t agree with the amount of memory reported by your operating system for a number of reasons:
# It only includes objects created by R, not the R interpreter itself.
# Both R and the operating system are lazy: they won’t reclaim memory until it’s actually needed. R might be holding on to memory because the OS hasn’t yet asked for it back.
# R counts the memory occupied by objects but there may be gaps due to deleted objects. This problem is known as memory fragmentation.
# mem_change() builds on top of mem_used() to tell you how memory changes during code execution. Positive numbers represent an increase in the memory used by R, and negative numbers represent a decrease.
# Need about 4 mb to store 1 million integers

x=NULL
mem_change(x <- 1:1e6)
## 4.01 MB
# We get that memory back when we delete it
mem_change(rm(x))
## -4 MB
mem_change(x <- 1:1e6)
## 4 MB
# Also point to 1:1e6 from y
mem_change(y <- x)
## 1.36 kB
# Remove x, no memory freed because y is still pointing to it
mem_change(rm(x))
## 1.25 kB
# Now nothing points to it and the memory can be freed
mem_change(rm(y))
## -4 MB

# mem_change() captures the net change in memory when running a block of code. Sometimes, however, we may want to measure incremental change. One way to do this is to use memory profiling to capture usage every few milliseconds. This functionality is provided by utils::Rprof() but it doesn’t provide a very useful display of the results. Instead we’ll use the lineprof package. It is powered by Rprof(), but displays the results in a more informative manner.

# To demonstrate lineprof, we’re going to explore a bare-bones implementation of read.delim() with only three arguments:

read_delim <- function(file, header = TRUE, sep = ",") {
  # Determine number of fields by reading first line
  first <- scan(file, what = character(1), nlines = 1,
                sep = sep, quiet = TRUE)
  p <- length(first)
  
  # Load all fields as character vectors
  all <- scan(file, what = as.list(rep("character", p)),
              sep = sep, skip = if (header) 1 else 0, quiet = TRUE)
  
  # Convert from strings to appropriate types (never to factors)
  all[] <- lapply(all, type.convert, as.is = TRUE)
  
  # Set column names
  if (header) {
    names(all) <- first
  } else {
    names(all) <- paste0("V", seq_along(all))
  }
  
  # Convert list into data frame
  as.data.frame(all)
}
#####
# Save the script with code ----
# useful if you need to save but clicking save is unavailable as script is running for a long time.
rstudioapi::documentSave() # I think this saves all open scripts?
rstudioapi::documentSave(rstudioapi::getActiveDocumentContext()$id) # I think this saves current script.

#####
# Using paste() and paste0() etc ----
## When passing a single vector, paste0 and paste work like as.character.
paste0(1:12)
paste(1:12)        # same
as.character(1:12) # same

## If you pass several vectors to paste0, they are concatenated in a
## vectorized way.
(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))

## paste works the same, but separates each input with a space.
## Notice that the recycling rules make every input as long as the longest input.
paste(month.abb, "is the", nth, "month of the year.")
paste(month.abb, letters)

## You can change the separator by passing a sep argument
## which can be multiple characters.
paste(month.abb, "is the", nth, "month of the year.", sep = "_*_")

## To collapse the output into a single string, pass a collapse argument.
paste0(nth, collapse = ", ")

## For inputs of length 1, use the sep argument rather than collapse
paste("1st", "2nd", "3rd", collapse = ", ") # probably not what you wanted
paste("1st", "2nd", "3rd", sep = ", ")

## You can combine the sep and collapse arguments together.
paste(month.abb, nth, sep = ": ", collapse = "; ")

## Using paste() in combination with strwrap() can be useful
## for dealing with long strings.
(title <- paste(strwrap(
  "Stopping distance of cars (ft) vs. speed (mph) from Ezekiel (1930)",
  width = 30), collapse = "\n"))
plot(dist ~ speed, cars, main = title)
# }

#####
# Add colours to the colour palette ----
palette("default")
palette(c(palette("default"),"purple","brown"))
palette()
colors()[1:20]
palette(c("red","magenta","blue","cyan","green3","brown","gray","purple","yellow")) # adding&changing colour palette (default has 8 only)

# from community example https://www.rdocumentation.org/packages/grDevices/versions/3.6.0/topics/colorRamp
colors() # returns a vector of color names.

colors()[1:3]

# colorRamp is more useful
# returns a function that returns ONE color based on input
mycolors <- colorRamp(c(colors()[1:10]))

mycolors(.3) # argument between 0 and 1

hist(ChickWeight$weight, col = mycolors(.5))

# curious - why three colors? Because these are the RGB values.
# equivalent to...
hist(ChickWeight$weight, col = c(221.5, 207.5, 190))

# one way to get multiple values from colorramp
mycolors(c(.1,.3,.9))
hist(ChickWeight$weight, col = mycolors(c(.1,.3,.9)))


# or... define with colorRampPalette...
mycolorspal <- colorRampPalette(c(colors()[1:10]))

mycolorspal(10)

greens.pal <- colorRampPalette(c("lightgreen", "darkgreen"))

greens.pal(10)

hist(ChickWeight$weight, col = greens.pal(10))
#####
# Mini raster algebra tutorial ----
r <- raster(ncol=10, nrow=10)
values(r) <- 1:ncell(r)
plot(r)
# Now some algebra.
s <- r + 10
plot(s)
s <- sqrt(s)
s <- s * r + 5
values(r)
r[] # this gives raster layer cell values, just like the line above.
r[] <- runif(ncell(r)) # generates random numbers with a uniform distribution between 0 and 1
r <- round(r)
r <- r == 1 # produces trues and falses for the cells where the original raster did or didn't equal 1.
# You can also use replacement functions.
summary(r[])
s[r] <- -0.5 # with s and r being rasters of the same ncells, s cells are replaced by -0.5 in the 49 locations where r is true 
s[!r] <- 5   # s cells become 5 where r is not true
plot(s)
s[s == 5] <- 15

# to plot a raster but break up continuous cell values in to categorical:
brk = c(0,0.25,0.5,0.75,1.0)
plot(x, col=terrain.colors(3), breaks=brk)

# and a note on other file formats that accompany geotiffs:
# These files are generated in ArcGIS. Are auxiliary files for georeferencing and visualization, aren't part of GTiff as .shx or .dbf are in a shapefile.
# .tfw is ESRI World file
# .ovr are piramid layers
# .xml is schema look and histogram
# .cpg is for TIFF interpretation.
# .dbf is for raster attribute table (thanks to @radouxju)
# You can add .tfw using GDAL through raster package:
  
library(raster)

r <- raster()
values(r) <- sample(x = 1:10,size = ncell(r),replace = T)

writeRaster(r,'test.tif',options=c('TFW=YES'))

#####
# convert strings from upper case to lower case and vice versa ----
toupper("rd")
tolower("RF")
#####
# Send an email (e.g. when a process has finished) ----
library(mailR)
send.mail(from="kerengila@gmail.com",       to="keren.raiter@mail.huji.ac.il",
          subject="the loop is complete",   body="yeah yeah yeah",        html=T,
          smtp=list(host.name = "smtp.gmail.com",          port = 465,
                    user.name = "kerengila@gmail.com",     passwd = "Ivrit333",
                    ssl = T), authenticate=T) #, attach.files="C:\\Users\\Deepanshu\\Downloads\\Nature.xls")

#####
# Install 'elsa' -----

# 1.	You need to install RTools from https://cran.r-project.org/bin/windows/Rtools/ but this doesn’t solve the issue alone.
# 2.	However, if you try to install it in R using install.packages("Rtools"), you may get an error saying “Warning in install.packages : package ‘Rtools’ is not available (for R version 3.5.2)”
# 3.	I used a workaround from https://github.com/r-lib/devtools/issues/1772: 
  
install.packages("devtools")                         # make sure you have the latest version from CRAN
library(devtools)                                             # load package
devtools::install_github("r-lib/pkgbuild")  # install updated version of pkgbuild from GitHub
library(pkgbuild)                                             # load package
find_rtools()                                                    # should be TRUE, assuming you have Rtools 3.5

# now installing elsa should work:
install_github("babaknaimi/elsa")
#####
# Convert letters to numbers, and match letters to any defined values ----
myLetters <- letters[1:26]

match("a", myLetters)
# [1] 1

# It is almost as easy to associate other values to the letters. The following is an example using a random selection of integers.
# assign values for each letter, here a sample from 1 to 2000
set.seed(1234)
myValues <- sample(1:2000, size=26)
names(myValues) <- myLetters

myValues[match("a", names(myValues))]
# a 
# 228

#####
# Using sf package to remove overlapping features or feature-parts----

set.seed(131)
library(sf)
m = rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))
p = st_polygon(list(m))
n = 100
l = vector("list", n)
for (i in 1:n)
  l[[i]] = p + 10 * runif(2)
s = st_sfc(l) # sfc: Create simple feature geometry list column
plot(s, col = sf.colors(categorical = TRUE, alpha = .5))
title("overlapping squares")
d = st_difference(s) # sequential differences: s1, s2-s1, s3-s2-s1, ...
plot(d, col = sf.colors(categorical = TRUE, alpha = .5))
title("non-overlapping differences")
i = st_intersection(s) # all intersections
plot(i, col = sf.colors(categorical = TRUE, alpha = .5))
title("non-overlapping intersections")
summary(lengths(st_overlaps(s, s))) # includes self-counts!
summary(lengths(st_overlaps(d, d)))
summary(lengths(st_overlaps(i, i)))
sf = st_sf(s)
i = st_intersection(sf) # all intersections
plot(i["n.overlaps"])
summary(i$n.overlaps - lengths(i$origins))
# A helper function that erases all of y from x:
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

# Using sf to combine or union feature geometries
nc = st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source `/home/travis/build/r-spatial/sf/inst/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XY
#> bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> epsg (SRID):    4267
#> proj4string:    +proj=longlat +datum=NAD27 +no_defs
plot(nc)
st_combine(nc)
plot(st_combine(nc))
#> Geometry set for 1 feature 
#> geometry type:  MULTIPOLYGON
#> dimension:      XY
#> bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> epsg (SRID):    4267
#> proj4string:    +proj=longlat +datum=NAD27 +no_defs
#> MULTIPOLYGON (((-81.47276 36.23436, -81.54084 3...
plot(st_union(nc))

# using raster package:
# NOT RUN {
e1 <- extent(-10, 10, -20, 20)
e2 <- extent(0, 20, -40, 5)
union(e1, e2)

#SpatialPolygons
(require(rgdal) & require(rgeos)) 
  p <- shapefile(system.file("external/lux.shp", package="raster"))
  p@data$ID_3 = c(2:13)
  plot(p, col= rainbow(12))
  p0 <- aggregate(p)
  plot(p0)
  b <- as(extent(6, 6.4, 49.75, 50), 'SpatialPolygons')
  plot(b)
  projection(b) <- projection(p)
  u <- union(p0, b)
  plot(u, col=2:5)
  
  u2 = union(p,b)
  u2@data$ID_3 = c(2:21)
  plot(u2, col = terrain.colors(20))

  

#####
# Cool plot with two lines plotted seperately on different axes ----
x <- 1:100
y1 <- rnorm(100)
y2 <- rnorm(100)+100

par(mar=c(5,5,5,5))

plot(x,y1,pch=0,type="b",col="red",yaxt="n",ylim=c(-8,2),ylab="") # optionally add ', bty="n"' to remove border
axis(side=2, at=c(-2,0,2))
mtext("red line", side = 2, line=2.5, at=0)

par(new=T)
plot(x,y2,pch=1,type="b",col="blue",yaxt="n",ylim=c(98,108), ylab="", bty="n") # add ', bty="n"' to remove border
axis(side=4, at=c(98,100,102), labels=c("98%","100%","102%"))
mtext("blue line", side=4, line=2.5, at=100)

#####
# Display and remove spatial features with NAs ----
# from https://www.rdocumentation.org/packages/spatialEco/versions/1.1-1/topics/sp.na.omit

library(sp)
data(meuse)
plot(meuse)
coordinates(meuse) <- ~x+y

# Display rows with NA  
meuse@data[!complete.cases(meuse@data),] 

# Remove all NA's in rows (and associated points)
meuse2 <- sp.na.omit(meuse) 
dim(meuse)
dim(meuse2)

# Plot deleted points in red
plot(meuse, col='red', pch=20)
plot(meuse2, col='black', pch=20, add=TRUE)

# Remove NA's associated with specific column 
meuse2 <- sp.na.omit(meuse, col.name = "om") 
head(meuse@data)
head(meuse2@data)

#####
# Map with two legends ----

png(filename=paste0(heavies.image.path,"Grand_distribution.noveg.disturbed.png"), width=16,height=22,units='cm',res=900)
plot(grand.distribution.generous, xlim=c(34.3,35.4), ylim=c(30.8,31.6), bty="n", axes=FALSE)
plot(b.disturbed[b.disturbed@data$landuse == "disturbed-uncategorized",], col="darkgrey", border="darkgrey", add=T)
plot(b.disturbed[b.disturbed@data$landuse == "built-up areas and industry",], col="darkgrey", border="darkgrey", add=T)
plot(b.disturbed[b.disturbed@data$landuse == "military",], col="thistle3", border="thistle3", add=T)
plot(b.disturbed[b.disturbed@data$landuse == "agriculture",], col="burlywood", border="burlywood", add=T)
plot(b.disturbed[b.disturbed@data$landuse == "plantation",], col="darksalmon", border="darksalmon", add=T)
plot(bs,pch=16,cex=.6,main="Beershebensis observations",cex.main=1,font.main=2, add=T)
points(bs[bs$presence == "present",],col='green', pch=16, cex=0.1)
points(bs[bs$presence == "absent",], col='black', pch=16, cex=0.1)
lines(borders, lty=5, lwd=1.5, col="grey15")
lines(groads, col="grey73")
points(bsp,   bg  = 'yellow', pch=22,  cex=0.8)
points(bsa,   bg = 'cyan',    pch=21,  cex=0.6)
points(bsa.r, bg = 'blue',    pch=21,  cex=0.6)
points(bc,    bg = 'green',   pch=24,  cex=0.6)
points(bc.r,  bg = 'green4',  pch=24,  cex=0.6)
plot(kkl.plan, col=NA, border='darkblue', add=T)
plot(kkl.ops,  col=NA, border='blue', add=T)
plot(nat.res,  col=NA, border='darkgreen', add=T)
plot(nat.park, col=NA, border='green', add=T)
plot(villages, pch=21, bg='blue', cex=0.9, add=T)
plot(towns, pch= 21, bg= 'green', cex = 0.9, add=T)
plot(small.cities, pch=21, bg='orange', cex=1.4, add=T)
points(major.cities, pch=21, col='black', bg='yellow', cex=1.5)
with(major.cities, text(major.cities$lat~major.cities$lon, labels = major.cities$name, pos = 4, cex=1, font=2))
legend("bottomright", 
       c("Surveyed presence (75)", "Surveyed absence-questionable (24)","Surveyed absence-reliable (103)",
         "Collections record-unreliable (71)","Collections record-reliable (243)"),
       pch=c(22,21,21,24,24), cex=1, text.font=1,
       pt.bg=c("yellow","cyan","blue","green","green4"))
legend("bottomleft", 
       c("Agriculture", "Built-up areas and industry", "Military", 
         "Plantation","KKL management", "Nature reserve","National Park"),
       pch=22, pt.cex=1.5, text.font=1, col=c(NA,NA,NA,NA,"blue","darkgreen","green"),
       pt.bg=c("burlywood","darkgrey","thistle3","darksalmon",NA,NA,NA))
dev.off()

##### 
# Create a KML for veiwing spatial data in Google maps or similar ----

# from https://stackoverflow.com/questions/7813141/how-to-create-a-kml-file-using-r

# Check the writeOGR function in the rgdal package. Here is a simple example:
library("sp")
library("rgdal")
data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS("+init=epsg:28992")
meuse_ll <- spTransform(meuse, CRS("+proj=longlat +datum=WGS84"))
writeOGR(meuse_ll["zinc"], "meuse.kml", layer="zinc", driver="KML") 

# I think is worth mentioning the plotKML package as well.

# However, for easy sharing among colleagues I found interesting the mapview package based on leaflet package. One can save a map as HTML document with various options for a background map; no need of Google Earth and the HTML map will run on your browser.

# Some examples:
  
library(sp)
library(rgdal)
library(raster)
library(plotKML)
library(mapview)

# A SpatialPointsDataFrame example
data(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992") # CRS Amersfoort (Netherlands)
# make a KML file from SpatialPointsDataFrame object
# will get a warning like "Reprojecting to +proj=longlat +datum=WGS84 ..."
# as it is expected to work with geographical coordinates with datum=WGS84, 
# but seems that it takes care of the reprojecting. 
plotKML::kml(meuse,
             file.name    = "meuse_cadium.kml",
             points_names = meuse$cadmium,
             colour    = "#FF0000",
             alpha     = 0.6,
             size      = 1,
             shape     = "http://maps.google.com/mapfiles/kml/pal2/icon18.png")
# Or, an easy to make interactive map with mapView()
mapView(meuse)

# A RasterLayer example   
data(meuse.grid)
gridded(meuse.grid) <- ~x+y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
dist_rst <- raster(meuse.grid["dist"])
# make a KML file from RasterLayer object
plotKML::kml(dist_rst,
             file.name    = "dist_rst.kml",
             colour_scale = SAGA_pal[[1]])
# Or, easy to make interactive map with mapView() - display raster and add the points
mapView(dist_rst, legend=TRUE) + meuse
# However, note that for bigger raster datasets mapView() might reduce from resolution

# links to tutorials and examples provided on page

# also try http://zevross.com/blog/2015/08/21/process-a-raster-for-use-in-a-google-map-using-r/
####
#####
# Selecting specific models for an ensemble in sdm package ----

# You can do that by specifying models IDs:
ensemble(m, id=...)

# Model IDs can be found with:
ids <- getModelInfo(m)
ids.rf <- ids[ids$method=="rf",]
ids.rf$modelID

# Alternatively, you can separate models from the original sdmModels object with:
ids <- c(1,2,3,4) 
m2 <- subset(m, ids)
# or:
m2 <- m[[1:4]]
# and use it in the ensemble function
ensemble(m2, ...)

#####
# Calculating distance to nearest point ----

# from https://stackoverflow.com/questions/21977720/r-finding-closest-neighboring-point-and-number-of-neighbors-within-a-given-rad

# Best option is to use libraries sp and rgeos, which enable you to construct spatial classes and perform geoprocessing.

library(sp)
library(rgeos)

# Read the data and transform them to spatial objects:
"C:\Users\owner\Desktop\test.txt"
mydata <- read.delim('c:/Users/owner/Desktop/test.txt', header=T) # not good format
mydata = read_()

sp.mydata <- mydata
coordinates(sp.mydata) <- ~long+lat

class(sp.mydata)
# "SpatialPointsDataFrame"
attr(,"package")
# "sp"
# Now calculate pairwise distances between points

d <- gDistance(sp.mydata, byid=T)

# Find second shortest distance (closest distance is of point to itself, therefore use second shortest)

min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])

# Construct new data frame with desired variables

newdata <- cbind(mydata, mydata[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))

colnames(newdata) <- c(colnames(mydata), 'neighbor', 'n.lat', 'n.long', 'n.area', 'n.canopy', 'n.avg.depth', 'distance')

# newdata
# pond      lat      long area canopy avg.depth     neighbor    n.lat    n.long n.area n.canopy n.avg.depth
# 6            A10 41.95928 -72.14605 1500     66  60.61538 Borrow.Pit.3 41.95546 -72.15375      0        0    29.22222
# 3          AA006 41.96431 -72.12100  250      0  57.77778   Blacksmith 41.95508 -72.12380    361       77    71.31250
# 2     Blacksmith 41.95508 -72.12380  361     77  71.31250        AA006 41.96431 -72.12100    250        0    57.77778
# 5   Borrow.Pit.1 41.95601 -72.15419    0      0  41.44444 Borrow.Pit.2 41.95571 -72.15413      0        0    37.70000
# 4   Borrow.Pit.2 41.95571 -72.15413    0      0  37.70000 Borrow.Pit.1 41.95601 -72.15419      0        0    41.44444
# 5.1 Borrow.Pit.3 41.95546 -72.15375    0      0  29.22222 Borrow.Pit.2 41.95571 -72.15413      0        0    37.70000
# 6.1      Boulder 41.91822 -72.14978 1392     98  43.53333 Borrow.Pit.3 41.95546 -72.15375      0        0    29.22222
# distance
# 6   0.0085954872
# 3   0.0096462277
# 2   0.0096462277
# 5   0.0003059412
# 4   0.0003059412
# 5.1 0.0004548626
# 6.1 0.0374480316

# Edit: if coordinates are in degrees and you would like to calculate distance in kilometers, use package geosphere

library(geosphere)

d <- distm(sp.mydata)

# rest is the same
# This should provide better results, if the points are scattered across the globe and coordinates are in degrees

#####
# Round up to the nearest power of ten ----

roundUp <- function(x) 10^ceiling(log10(x))
roundUp(c(0.0023, 3.99, 10, 1003)) # also works on a vector

#####
# Rasterize polygons ----

p1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))
p1 <- list(p1, hole)
p2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55), c(-10,0))
p3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45), c(-125,0))

pols <- spPolygons(p1, p2, p3)

r <- raster(ncol=90, nrow=45)
r <- rasterize(pols, r, fun=sum)

plot(r)
plot(pols, add=T)

# add a polygon
p5 <- rbind(c(-180,10), c(0,90), c(40,90), c(145,-10),
            c(-25, -15), c(-180,0), c(-180,10))
addpoly <- SpatialPolygons(list(Polygons(list(Polygon(p5)), 1)))
addpoly <- as(addpoly, "SpatialPolygonsDataFrame")
addpoly@data[1,1] <- 10
r2 <- rasterize(addpoly, r, field=1, update=TRUE, updateValue="NA")
plot(r2)
plot(pols, border="blue", lwd=2, add=TRUE)
plot(addpoly, add=TRUE, border="red", lwd=2)

# get the percentage cover of polygons in a cell
r3 <- raster(ncol=36, nrow=18)
r3 <- rasterize(pols, r3, getCover=TRUE)

#####
# rasterize SpatialPolygonsDataFrame and keep factor field ----

# https://gis.stackexchange.com/questions/325586/r-rasterize-spatialpolygonsdataframe-and-keep-factor-field

library('raster')
library('rgdal')
# Load a SpatialPolygonsDataFrame example (Brazil administrative level 2) shapefile
dat <- raster::getData(country = "BRA", level = 2)
plot(dat)

# get names
nam <- unique(dat$NAME_1)

# create a data.frame
nam_df <- data.frame(ID = 1:length(nam), nam = nam)

# Place IDs
dat$ID <- nam_df$ID[match(dat$NAME_1,nam_df$nam)]

# Define RasterLayer object
r.raster <- raster()

# Define raster extent
extent(r.raster) <- extent(dat)

# Define pixel size
res(r.raster) <- 0.1

# rasterize
ras <- rasterize(x = dat, y = r.raster, field = "ID")

# ratify raster
r <- ratify(ras)

# Create levels
rat <- levels(r)[[1]]
rat$names <- nam_df$nam
rat$IDs <- nam_df$ID
levels(r) <- rat

rasterVis::levelplot(r)

#####
# Create SpatialPpointsDataFrame from coordinates ----

# To make a SpatialPointsDataFrame you need 3 components:
#  coordinates
#  data
#  proj4string of the coordinates (AKA, coordinate reference system (CRS))

# load some example data
library(sp)                       # spatial library
data(meuse)                       # load built in dataset

# prepare coordinates, data, and proj4string
coords <- meuse[ , c("x", "y")]   # coordinates
data   <- meuse[ , 3:14]          # data
crs    <- CRS("+init=epsg:28992") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf <- SpatialPointsDataFrame(coords      = coords,
                               data        = data, 
                               proj4string = crs)
# check the object class
class(spdf)

# plot the copper column 
spplot(spdf, "copper")

#####
# Choose a file interactively -----
# It’s also possible to choose a file interactively using the function file.choose(), which I recommend if you’re a beginner in R programming:
  my_data <- read_excel(file.choose())

#####
# Control number of decimal digits in print output ----

options(digits=3)

#####
# Collapse all code sections (code folding) ----

# Edit > folding > collapse all
# or:
# Alt + O

#####
# GitHub personal access token ----

# from https://gist.github.com/Z3tt/3dab3535007acf108391649766409421

## set your user name and email:
usethis::use_git_config(user.name = "KerenRaiter", user.email = "keren.raiter@me.com")

## create a personal access token for authentication:
usethis::create_github_token() 
## in case usethis version < 2.0.0: usethis::browse_github_token() (or even better: update usethis!)


## set personal access token:
credentials::set_github_pat("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")

## or store it manually in '.Renviron':
usethis::edit_r_environ()`
## store your personal access token with: GITHUB_PAT=xxxyyyzzz
## and make sure '.Renviron' ends with a newline

# 4. Verify settings

usethis::git_sitrep()

## Your username and email should be stated correctly in the output. 
## Also, the report shoud cotain something like:
## 'Personal access token: '<found in env var>''

?git_vaccinate
usethis::git_vaccinate()

#####
# Weighted mean ----

weighted.mean(x, weights)
weighted.mean(c(2,3,4,1),c(0.2,0.1,9,0.05))
#####
# Parse - construct names by loop, and evaluate that object, and deparse ----

# non-sensical example:

list.abc = list(78, 923, 12)
list.pqr = list(0.4, 0.71, 0.055)
list.xyz = list(751, 952, 9072)

letters = list("abc", "pqr", "xyz")
results = list()

for (i in 1:length(letters)) { 
  for (a in 1:length(list.abc)) {
results = eval(parse(text = paste0("list.", letters[[a]])))[[i]]
  }
}

# Example in use:
for (i in 1:length(set.names))                           {
  for (a in 1:length(top.algs.l))      {
    occmap = eval(parse(text = paste0("occmaps.",top.algs[[a]])))[[i]]
    plot(occmap, axes=FALSE, ann=FALSE )
  }  }

#####
# How to add title for graph with multiple plots ----

par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot(1:10,  main="Plot 1")
plot(1:100,  main="Plot 2")
mtext("Title for Two Plots", outer = TRUE, cex = 1.5)

# an example of deparse in use:
png(filename = paste0(B.heavies.image.path,"schreiberi/","S ", deparse(substitute(distribution)), ".png"))
 
