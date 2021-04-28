#Found & adapted from https://stackoverflow.com/questions/9595117/identify-a-linear-feature-on-a-raster-map-and-return-a-linear-shape-object-using/9643004#9643004

#Load libraries
library(deldir)
library(rgeos)
library(sp)
library(sf)
library(smoothr)

#test polygon - load your shapefile here
rPoly <- read_sf("scarborough_shapefile.shp")
plot(rPoly$geometry)
rPoly <- as(rPoly,"Spatial")
crs(rPoly)

## Find points on boundary of rPoly (see question)
rPolyPts <-  coordinates(as(as(rPoly, "SpatialLinesDataFrame"),
                            "SpatialPointsDataFrame"))

## Perform Voronoi tessellation of those points and extract coordinates of tiles
rVoronoi <- tile.list(deldir(rPolyPts[, 1], rPolyPts[,2]))
rVoronoiPts <- SpatialPoints(do.call(rbind, 
                                     lapply(rVoronoi, function(x) cbind(x$x, x$y))))
crs(rVoronoiPts) <- crs(rPoly)
## Find the points on the Voronoi tiles that fall inside 
## the linear feature polygon


## N.B. That the width parameter may need to be adjusted if coordinate
## system is fractional (i.e. if longlat), but must be negative or zero


#CM. tweak "width" here depending on your projection. How close 
#to the edge of your polygon do you want to keep points?

rLinePts <- gIntersection(gBuffer(rPoly, width=-0.01), rVoronoiPts)

## Create SpatialLines object
rLine <- SpatialLines(list(Lines(Line(rLinePts), ID="1")))


plot(rLine,add=T)

#CM. May need to adjust the smoothness here for wider/less easy polygons
rLine_smooth <- smooth(rLine,method = "ksmooth",smoothness = 10)

plot(rPoly)
plot(rLine_smooth,add=T)


#CM. output code
centreLine <- st_as_sf(rLine_smooth)
write_sf(centreLine,"centreLine.shp")