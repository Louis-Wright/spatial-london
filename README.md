# Spatial Analysis in R - London examples

# Source
# https://github.com/Robinlovelace/Creating-maps-in-R

# Setup
setwd('~/R/gis-london/maps-in-R')
x = c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
# install.packages(x)
lapply(x, library, character.only = TRUE)                   # load the required packages

# Load data
lnd = readOGR(dsn = "data/london_sport.shp")                # polys are london boroughs 2001
head(lnd@data, n = 2)                                       # summary of first two data points 
mean(lnd$Partic_Per)                                        # mean for sport part. (%) per borough
sapply(lnd@data, class)                                     # class of data/column
lnd$Pop_2001 = as.numeric(as.character(lnd$Pop_2001))       # coerce data into numeric

# Plots
lnd@proj4string
plot(lnd)
plot(lnd@data)

lnd@data[lnd$Partic_Per < 13, 1:3]                           # select forst 3 rows where sports part. < 13
sel = lnd$Partic_Per > 20 & lnd$Partic_Per < 25              # Select zones where part. 20%-25%
plot(lnd[sel, ])                                             # output not shown here
head(sel)                                                    # test output of previous selection

plot(lnd, col = "lightgrey")                                 # plot the london_sport object
sel = lnd$Partic_Per > 25
plot(lnd[sel, ], col = "turquoise", add = TRUE)              # add selected zones to map

# Exercise (not shown on tut)
file.edit("README.Rmd")
plot(lnd, col = "grey")

cent_lnd <- gCentroid(lnd[lnd$name == "City of London",])       # find L's geo centroid (add ", byid = T" for all)
points(cent_lnd, cex = 3)                                       # adds cirlce in centre of L
lnd_buffer <- gBuffer(spgeom = cent_lnd, width = 10000)         # set 10 km buffer

# method 1 of subsetting selects any intersecting zones
lnd_central <- lnd[lnd_buffer,]                                 # the selection is too big!

plot(lnd_central, col = "lightblue", add = T)                   # test method
plot(lnd_buffer, add = T)                                       # some areas just touch the buffer

# method2 of subsetting selects only points within the buffer
lnd_cents <- SpatialPoints(coordinates(lnd),
                           proj4string = CRS(proj4string(lnd))) # create spatialpoints
sel <- lnd_cents[lnd_buffer,]                                   # select points inside buffer
points(sel)                                                     # show where the points are located
lnd_central <- lnd[sel,]                                        # select zones intersecting w. sel
plot(lnd_central, add = T, col = "lightslateblue", 
     border = "grey")
plot(lnd_buffer, add = T, border = "red", lwd = 2)
text(coordinates(cent_lnd), "Central\nLondon")                  # Add text to the plot
# End of excercise

# Selecting Quadrants
# Find the centre of the london area
easting_lnd <- coordinates(gCentroid(lnd))[[1]]
northing_lnd <- coordinates(gCentroid(lnd))[[2]]

# arguments to test whether or not a coordinate is east or north of the centre
east <- sapply(coordinates(lnd)[,1], function(x) x > easting_lnd)
north <- sapply(coordinates(lnd)[,2], function(x) x > northing_lnd)

# test if the coordinate is east and north of the centre
lnd$quadrant <- "unknown"                                       # prevent NAs in result
lnd$quadrant[east & north] <- "northeast"

lnd$quadrant[!east & north] <- "northwest"
lnd$quadrant[east & !north] <- "southeast"
lnd$quadrant[!east & !north] <- "southwest"
plot(lnd)
plot(lnd[east & north,], add = TRUE, col = "red" )
llgridlines(lnd, lty= 3, side ="EN", offset = -0.5)

lnd_disolved = rgeos::gUnaryUnion(spgeom = lnd, id = lnd$quadrant)
library(tmap)
qtm(lnd, fill = "quadrant") +
  tm_shape(lnd_disolved) +
  tm_borders(lwd = 9)

# Spatial indluence
# source: http://www.rspatial.org/analysis/rst/2-scale_distance.html#two-nearest-neighbours

library(raster)
library(spdep)

plot(lnd, col = "grey")
xy = coordinates(lnd)
wr <- poly2nb(lnd, row.names=lnd$ons_label, queen=FALSE) # adj neigbours
plot(wr, xy, col='red', lwd=2, add=TRUE)

# number of neighbors for each area
wm <- nb2mat(wr, style='B', zero.policy = TRUE)
i <- rowSums(wm)
i

# Expresses as percentage
round(100 * table(i) / length(i), 1)

# Distance based:
wd5 <- dnearneigh(xy, 0, 5000)
wd10 <- dnearneigh(xy, 0, 10000)
wd25 <- dnearneigh(xy, 0, 25000)

plot(wd25, xy, col='red', lwd=2, add=TRUE)

plotit <- function(nb, lab='') {
  plot(lnd, col='gray', border='white')
  plot(nb, xy, add=TRUE, pch=20)
  text(6.3, 50.1, paste0('(', lab, ')'), cex=1.25)
}
plotit(wd25, '25 km')
plotit(wd10, '10 km')
plotit(wd5, '5 km')
