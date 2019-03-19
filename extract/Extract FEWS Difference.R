setwd('G://My Drive/DHS Spatial Covars/FEWS Validation/Shapefiles/')

library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(dplyr)

f1 <- 'EA_201410_CS'
f2 <- 'EA_201710_CS'

makeRasterFromPoly <- function(poly, res){
  
  xmx <- ceiling(extent(poly)[2]/res)*res
  xmn <- floor(extent(poly)[1]/res)*res
  ymx <- ceiling(extent(poly)[4]/res)*res
  ymn <- floor(extent(poly)[3]/res)*res
  
  x <- length(seq(xmn, xmx, 0.005))
  y <- length(seq(ymn, ymx, 0.005))
  
  m <- matrix(rep(0, x*y), ncol=x, nrow=y)
  
  r <- raster(m, xmx=xmx, xmn=xmn, ymx=ymx, ymn=ymn, 
              crs=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 '))
  r
}

ipc1 <- readOGR('.', f1)
ipc2 <- readOGR('.', f2)

ref <- raster::union(extent(ipc1), extent(ipc2)) %>%
  makeRasterFromPoly(0.005)

ipc1rast <- rasterize(ipc1, ref, field='CS')
ipc1rast[ipc1rast > 5] <- NA

ipc2rast <- rasterize(ipc2, ref, field='CS')
ipc2rast[ipc2rast > 5] <- NA

ipc_diff <- ipc2rast - ipc1rast

writeRaster(ipc_diff, filename = paste0('../Difference Rasters/', f1, '-', f2, '.tif'), format='GTiff')
            