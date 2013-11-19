## tewelde 
## Date 18-11-2013
## SRandom forest Classification
## Exe_ 6
#################
setwd("E:/lesson 6") ## working directory
#load the necessary packages
library(rasta)
library(igraph)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(maptools)
# load in the training classes and Look-Up Table (LUT) of the Gewata area
data(lulcGewata)
data(LUTGewata)
## plot of the original lulcGewata  
cols <- c("orange", "light green", "brown", "light pink", "dark green", "light blue")
plot(lulcGewata, col=cols, legend=FALSE)
legend("topright", legend=LUTGewata$Class, fill=cols)
## plot of lulcGewata with out legend 
plot(lulcGewata, col=cols, legend=FALSE)
## plot cropland and############################################## 
cropland <- drawPoly(sp=TRUE)
projection(cropland) <- projection(lulcGewata)
projection(cropland)
plot(lulcGewata)
plot(cropland, add=TRUE)
## convert it to SpatialPolygonsDataFrame 
cropland <- SpatialPolygonsDataFrame(cropland, data=data.frame(
  class="cropland"), match.ID=FALSE)
cropland
## plot forest and #########################################################
## set project coordinate system
forest <- drawPoly(sp=TRUE)
projection(forest) <- projection(lulcGewata)
projection(forest)
plot(lulcGewata)
plot(forest, add=TRUE)
## convert it to SpatialPolygonsDataFrame 
forest <- SpatialPolygonsDataFrame(forest, data=data.frame(
  class="forest"), match.ID=FALSE)
forest
## plot wetland and ###############################################
## set project coordinate system
wetland <- drawPoly(sp=TRUE)
projection(wetland) <- projection(lulcGewata)
projection(wetland)
plot(lulcGewata)
plot(wetland, add=TRUE)
## convert it to SpatialPolygonsDataFrame 
wetland <- SpatialPolygonsDataFrame(wetland, data=data.frame(
  class="wetland"), match.ID=FALSE)
wetland
#### plot bamboo and ###############################################
## set project coordinate system
bamboo <- drawPoly(sp=TRUE)
projection(bamboo) <- projection(lulcGewata)
projection(bamboo)
plot(lulcGewata)
plot(bamboo, add=TRUE)
## convert it to SpatialPolygonsDataFrame 
bamboo <- SpatialPolygonsDataFrame(bamboo, data=data.frame(
  class="bamboo"), match.ID=FALSE)
bamboo)
## plot coffeeplantation and #########################################################
## set project coordinate system
coffeeplantation <- drawPoly(sp=TRUE)
projection(coffeeplantation) <- projection(lulcGewata)
projection(coffeeplantation)
plot(lulcGewata)
plot(coffeeplantation, add=TRUE)
## convert it to SpatialPolygonsDataFrame 
coffeeplantation <- SpatialPolygonsDataFrame(forest, data=data.frame(
  class="coffeeplantation"), match.ID=FALSE)
### coffeplantaion zoominf in  #################### 
plot(lulcGewata, col=cols, legend=FALSE)
e <- drawExtent() # zoom into a coffee area
plot(lulcGewata, col=cols, legend=FALSE, ext=e)
# now define a training polygon
coffeeplantaion1 <- drawPoly(sp=TRUE)
projection(coffeeplantaion1) <- projection(lulcGewata)
coffeeplantaion1 <- SpatialPolygonsDataFrame(coffeeplantaion1, data=data.frame(
  class="coffeeplantaion1"), match.ID=FALSE)
coffeeplantaion1
## plot barsoil and #########################################################
## set project coordinate system
barsoil <- drawPoly(sp=TRUE)
projection(barsoil) <- projection(lulcGewata)
projection(barsoil)
plot(lulcGewata)
plot(barsoil, add=TRUE)
## convert it to SpatialPolygonsDataFrame 
barsoil <- SpatialPolygonsDataFrame(barsoil, data=data.frame(
  class="barsoil"), match.ID=FALSE)
barsoil

##baresoil1 zooming 
plot(lulcGewata, col=cols, legend=FALSE)
e <- drawExtent() # zoom into a baresoil1 area
plot(lulcGewata, col=cols, legend=FALSE, ext=e)
# now define a training polygon
baresoil1 <- drawPoly(sp=TRUE)
projection(baresoil1) <- projection(lulcGewata)
baresoil1 <- SpatialPolygonsDataFrame(baresoil1, data=data.frame(
  class="baresoil1"), match.ID=FALSE)
baresoil1
## fusing ploygones
cropland <- spChFIDs(cropland, "cropland")
forest <- spChFIDs(forest, "forest")
wetland <- spChFIDs(wetland, "wetland")
bamboo <- spChFIDs(bamboo, "bamboo")
coffeeplantaion1 <- spChFIDs(coffeeplantaion1, "coffeeplantaion1")
baresoil1 <- spChFIDs(baresoil1, "baresoil1")
## bounding the training polygones
trainingPoly1 <- spRbind(cropland, forest)
trainingPoly1 <- spRbind(trainingPoly1, coffeeplantaion1)
trainingPoly1 <- spRbind(trainingPoly1, wetland)
trainingPoly1 <- spRbind(trainingPoly1, bamboo)
trainingPoly1 <- spRbind(trainingPoly1, baresoil1)
## check out
trainingPoly1@data
plot(lulcGewata)
plot(trainingPoly1, add=TRUE)
trainingPoly1@data$class
##preparing data for the classification: 
data(GewataB2)#band 2
data(GewataB3)# band 3
data(GewataB4)# band 4
## stack or Brick all the bands in to one
gewata <- brick(GewataB2, GewataB3, GewataB4)
## Compute the NDVI of the raster brick images of Gewata
ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)})
## filtering the data of NDVI using Focal Nighbourhood function
w <- matrix(1/9, nc=3, nr=3)
ndvi<- focal(ndvi, w=w)
## Add data of vegetation tree cover percentage
data(vcfGewata)
## Remove values greater than of 100 %
vcfGewata[vcfGewata > 100] <- NA
## rescale the NDVI to 10000
ndvi <- calc(ndvi, fun = function(x) floor(x*10000))
dataType(ndvi) <- "INT2U"
## Covariates value of the raster brickr
covs <- addLayer(gewata, ndvi, vcfGewata)
plot(covs)
## plot of the training polygons
trainingPoly1@data
plot(ndvi)
plot(trainingPoly1, add = TRUE)
trainingPoly1@data
trainingPoly1@data$class
## Reclass and assign CODES
reclass <- function(x){
  which(x==levels(trainingPoly1@data$class))
}
trainingPoly1@data$Code <- sapply(trainingPoly1@data$class, FUN=reclass)
classes <- rasterize(trainingPoly1, ndvi, field='Code')
dataType(classes) <- "INT1U"
## Plot with legends
cols <- c("orange","dark green","light green","light blue","yellow", "brown")
plot(classes, col=cols, legend=FALSE)
legend("bottomright", legend=c("cropland", "forest", "bamboo","wetland","coffeeplant1","baresoil1"), fill=cols, bg="white")
## mask the rasterBrick which only representing the training pixels.
covmasked <- mask(covs, classes)
plot(covmasked)
names(classes) <- "class"
trainingbrick <- addLayer(covmasked, classes)
#ploting 
##Add All Values of the inputs for the random forest from the raster brick
valuetable <- getValues(trainingbrick)
valuetable <- as.data.frame(valuetable)
##Remove NA and keep only the valid data
valuetable <- valuetable[!is.na(valuetable$class),]
## Convert to factors
valuetable$class <- factor(valuetable$class, levels = c(1:6))
## Remove NA values from the covariates/predictor columns
valuetable <- na.omit(valuetable)
## RANDOM FOREST Supervised Classification of Gewata
library(randomForest)
modelRF <- randomForest(x=valuetable[,c(1:5)], y=valuetable$class,
                        importance = TRUE)
names(modelRF)
summary(modelRF)
## Confusion Matrices
modelRF$confusion 
colnames(modelRF$confusion) <- c("cropland", "forest","bamboo", "wetland","coffeeplantaion1","baresoil1","class.error")
rownames(modelRF$confusion) <- c("cropland", "forest","bamboo", "wetland","coffeeplantaion1","baresoil1")
modelRF$confusion #  0.820512821 oveall accuracy
##  classes accuracy, higest and lowest accuracy are forest and barsoil respectively 
varImpPlot(modelRF)
# model prediction for classification 
predLC <- predict(covs, model=modelRF, na.rm=TRUE)
cols <- c("orange","dark green","light green","light blue","yellow", "brown")
plot(predLC, col=cols, legend=FALSE)
legend("topright", legend=c("cropland", "forest", "bamboo","wetland","coffeeplantaion1","baresoil1"), fill=cols, bg="white")

## ggPlot each classes to compare with the land cover
valuetable$label <- with(valuetable, ifelse(class==1, "coffeeplantaion1",
                                      ifelse(class==2, "forest",
                                      ifelse(class==3,"bamboo",
                                       ifelse(class==4,"wetland",    
                                       ifelse(class==5,"coffeeplant","baresoil1" ))))))
# 1. NDVI
pndvi <- ggplot(data=valuetable, aes(x=NDVI)) +
  geom_histogram(binwidth=300) +
  facet_wrap(~ label) +
  theme_bw()
pndvi
# 2. VCF
pvcf <- ggplot(data=valuetable, aes(x=vcf2000Gewata)) +
  geom_histogram(binwidth=5) +
  labs(x="% Tree Cover") +
  facet_wrap(~ label) +
  theme_bw()
pvcf
# 4. Bands 3 and 4
pB3B4 <- ggplot(data=valuetable, aes(x=gewataB3, y=gewataB4)) +
  stat_bin2d() +
  facet_wrap(~ label) +
  theme_bw()
pB3B4
# 4. Bands 2 and 3
pB2B3 <- ggplot(data = valuetable, aes(x=gewataB2, y=gewataB3)) +
  stat_bin2d() +
  facet_wrap(~ label) +
  theme_bw()
pB2B3
##End
