library(maptools)
library("ks", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library(plyr)
library(ggplot2)
library(car)
library(MASS)
library(sp)

source("/home/nithya/Desktop/Crime-Prediction-master/CrimePredictionUtil.R")

#import crime data
theft.2014.jan.to.feb = sample.crime("/home/nithya/Desktop/Crime-Prediction-master/2014_THEFT.csv", -1, 1, 2)
str(theft.2014.jan.to.feb)
print(str(theft.2014.jan.to.feb))
theft.2013.dec = sample.crime("/home/nithya/Desktop/Crime-Prediction-master/2013_THEFT.csv", -1, 12, 12)
str(theft.2013.dec)
theft.jan = sample.crime("/home/nithya/Desktop/Crime-Prediction-master/2013_THEFT.csv", -1, 1, 1)
str(theft.jan)
theft.combined <- rbind(theft.2013.dec,theft.2014.jan.to.feb)
summary(theft.combined)

theft.2014.jan = theft.2014.jan.to.feb[theft.2014.jan.to.feb$month == 1,]
theft.2014.jan$sixhr <- cut(theft.2014.jan$timestamp, "6 hours")
str(theft.2014.jan$sixhr)
# read chicago boundary
city.boundary = read.shapefile("/home/nithya/Desktop/Crime-Prediction-master/City_Boundary/City_Boundary/City_Boundary.shp", "poly", "+init=epsg:3435", "+init=epsg:26971")
# set prediction resolution
prediction.resolution.meters = 200

# get negative observations within chicago
non.crime.points = cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
head(non.crime.points)
names(non.crime.points)[1] = "response"

# get positive observations from february within chicago
training.crime.points = cbind(1, theft.feb[,c("x","y")])
names(training.crime.points)[1] = "response"

# combine positive and negative points
training.data = rbind(non.crime.points, training.crime.points)

# calculate crime density for each training observation based on january records
theft.density = run.spatial.kde(theft.jan[,c("x","y")], training.data[,c("x","y")], 1000)
theft.density

plot(theft.density, display="filled.contour2", cont=seq(10,90,by=10))
plot(theft.density, display="persp", thin=3, border=1, col="red")
plot(theft.density)

kde.resolution.meters = 200
kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)
kde.est.points
plot.spatial.kde(theft.density, kde.est.points)
theft.density[,1]

