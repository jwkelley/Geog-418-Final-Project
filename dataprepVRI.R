#Libraries
library(spgwr)
library(spatstat)
library(tmap)
library(gstat)
library(sf)
library(raster)
library(rgdal)
library(e1071)
library(spdep)

#Set working directory
dir <- "H:/GEOG_418_2020/FinalProject/VRI/"
setwd(dir)

#Reading in elevation dataset
elev <- readOGR(".", "ElevSample") #Read in data
elev <- spTransform(elev, CRS("+init=epsg:26910"))

#Reading in VRI data
VRI <- readOGR(".", "WatershedVRI") #Read in shapefile
VRI <- spTransform(VRI, CRS("+init=epsg:26910"))
head(VRI@data)

#Create choropleth map of height
map_HT <- tm_shape(VRI) +
  tm_polygons(col = "PROJ_HEI_1",
              title = "Stand Height",
              style = "jenks",
              palette = "viridis", n = 6) +
  tm_legend(legend.position = c("LEFT", "BOTTOM"))

map_HT

#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(elev, "regular", n=5000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(elev)







# ##Spatial interpolation with Kriging
# zerodist(elev)
# f.0 <- as.formula(grid_code ~ 1)
# var.smpl <- variogram(f.0, elev, cloud = FALSE) #, cutoff=1000000, width=89900)
# dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
#                           vgm(model="Exp"))
# plot(var.smpl, dat.fit)
# 
# s <- Sys.time()
# dat.krg <- krige(f.0, elev, grd, dat.fit)
# e <- Sys.time()
# print(e-s)


# #####################
# ###Descriptive Stats
# meanHT <- mean(VRI$PROJ_HEI_1 ,na.rm = TRUE)
# sdHT <- sd(VRI$PROJ_HEI_1 ,na.rm = TRUE)
# modeHT <- as.numeric(names(sort(table(VRI$PROJ_HEI_1), decreasing = TRUE))[1])
# medHT <- median(VRI$PROJ_HEI_1 , na.rm = TRUE)
# skewHT <- skewness(VRI$PROJ_HEI_1 , na.rm = TRUE)[1]
# kurtHT <- kurtosis(VRI$PROJ_HEI_1 , na.rm = TRUE)[1]
# CoVHT <- (sdHT / meanHT) * 100
# qqnorm(VRI$PROJ_HEI_1, pch = 1, frame = FALSE)
# qqline(VRI$PROJ_HEI_1, col = "steelblue", lwd = 2)
# 
# meanElev <- mean(as.numeric(elev$grid_code) ,na.rm = TRUE)
# sdElev <- sd(as.numeric(elev$grid_code) ,na.rm = TRUE)
# modeElev <- as.numeric(names(sort(table(as.numeric(elev$grid_code)), decreasing = TRUE))[1])
# medElev <- median(as.numeric(elev$grid_code) , na.rm = TRUE)
# skewElev <- skewness(as.numeric(elev$grid_code) , na.rm = TRUE)[1]
# kurtElev <- kurtosis(as.numeric(elev$grid_code) , na.rm = TRUE)[1]
# CoVElev <- (sdElev / meanElev) * 100
# qqnorm(elev$grid_code, pch = 1, frame = FALSE)
# qqline(elev$grid_code, col = "steelblue", lwd = 2)
# 
# #####
# #Create a table of descriptive stats
# data = c("HT", "PM 2.5") #Create an object for the labels
# means = c(meanHT, meanElev) #Create an object for the means
# sd = c(sdHT, sdElev) #Create an object for the standard deviations
# median = c(medHT, medElev) #Create an object for the medians
# mode <- c(modeHT, modeElev) #Create an object for the modes
# skewness <- c(skewHT, skewElev) #Create an object for the skewness
# kurtosis <- c(kurtHT, kurtElev) #Create an object for the kurtosis
# CoV <- c(CoVHT, CoVElev) #Create an object for the CoV
# 
# means <- round(means, 3)
# sd <- round(sd, 3)
# median <- round(median, 3)
# mode <- round(mode,3)
# skewness <- round(skewness,3)
# kurtosis <- round(kurtosis,3)
# CoV <- round(CoV, 3)
# descripStats <- data.frame(Data = data, Means = means, SD = sd, 
#                            Median = median, Mode = mode, Skewness = skewness, 
#                            Kurtosis = kurtosis, CoV = CoV)
# 
# #################################################
# #################################################
# #Global Moran's I Test
# 
# ###Create Spatial Neighbourhood Weights Matrix ###
# #queen's neighbour
# VRI.nb <- poly2nb(VRI)
# 
# plot(VRI)
# plot(VRI.nb, coordinates(VRI), add = TRUE, col = "red")
# 
# #Create the spatial weights neighbour list using the queen's case
# VRI.lw <- nb2listw(VRI.nb, zero.policy = TRUE, style = "W")
# print.listw(VRI.lw, zero.policy = TRUE)
# 
# #Calculate Global Morans
# mi <- moran.test(VRI$PROJ_HEI_1, VRI.lw, zero.policy = TRUE)
# mi
# 
# #To contextualize your Moran's I value, retrieve range of potential Moran's I values.
# moran.range <- function(lw) {
#   wmat <- listw2mat(lw)
#   return(range(eigen((wmat + t(wmat))/2)$values))
# }
# moran.range(VRI.lw)
# 
# mI <- mi$estimate[[1]]
# eI <- mi$estimate[[2]]
# var <- mi$estimate[[3]]
# 
# z <- (mI - eI) / sqrt(var)
# 
# #################################################
# #################################################
# #Local Moran's I Test
# 
# lisa.test <- localmoran(VRI$PROJ_HEI_1, VRI.lw, zero.policy = TRUE)
# 
# VRI$Ii <- lisa.test[,1]
# VRI$E.Ii<- lisa.test[,2]
# VRI$Var.Ii<- lisa.test[,3]
# VRI$Z.Ii<- lisa.test[,4]
# VRI$P<- lisa.test[,5]
# ########################
# map_LISA <- tmap::tm_shape(VRI) +
#   tm_polygons(col = "P",
#               title = "Local Moran's I",
#               style = "jenks",
#               palette = "viridis", n = 6)
# map_LISA
# 
# moran.plot(VRI$PROJ_HEI_1, VRI.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Stand Height",
#            ylab="Spatially Lagged Stand Height", quiet=NULL)


# ###Point Pattern Analysis
# winExt <- as.matrix(extent(elev))
# elevPPP <- ppp(x = coordinates(elev)[,1],
#              y = coordinates(elev)[,2],
#              window = as.owin(list(xrange = winExt[1,], yrange = winExt[2,])))
# 
# nearestNeighbour <- nndist(elevPPP)
# 
# ##Convert the nearestNeighbor object into a dataframe.
# nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
# ##Change the column name to "Distance"
# colnames(nearestNeighbour) = "Distance"
# 
# ##Calculate the nearest neighbor statistic to test for a random spatial distribution.
# #mean nearest neighbour
# nnd = sum(nearestNeighbour$Distance)/nrow(nearestNeighbour)
# 
# #mean nearest neighbour for random spatial distribution
# studyArea <- rgeos::gArea(VRI)
# pointDensity <- nrow(nearestNeighbour) / studyArea
# r.nnd = 1 / (2 * sqrt(pointDensity))
# d.nnd = 1.07453 / sqrt(pointDensity)
# R = nnd / r.nnd
# SE.NND <- .26136 / sqrt(nrow(nearestNeighbour) * pointDensity)
# z = (nnd - r.nnd) / SE.NND
# 
# #####
# ##K-FUNCTION
# #basic k-function
# k.fun <- Kest(elevPPP, correction = "Ripley")
# plot(k.fun)
# 
# #use simulation to test the point pattern against CSR
# k.fun.e <- envelope(elevPPP, Kest, nsim = 99, correction = "Ripley")
# plot(k.fun.e)