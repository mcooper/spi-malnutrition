setwd('G://My Drive/DHS Spatial Covars/Build Up Areas/')

library(gdalUtils)
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)

bu90 <- raster('BuiltUp1990_proj.tif')
bu00 <- raster('BuiltUp2000_proj.tif')
bu14 <- raster('BuiltUp2014_proj.tif')

bu90f <- focal(bu90, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
bu00f <- focal(bu00, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
bu14f <- focal(bu14, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

dat <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  unique

sp <- SpatialPointsDataFrame(coords = dat[ , c('longitude', 'latitude')], data=dat)

dat$bu90 <- extract(bu90f, sp)
dat$bu00 <- extract(bu00f, sp)
dat$bu14 <- extract(bu14f, sp)

estBuiltUp <- function(year, bu90, bu00, bu14){
  if (year <= 1990){
    return(bu90)
  }
  else if (year == 2000){
    return(bu00)
  }
  else if (year >= 2014){
    return(bu14)
  }
  else if (year > 1990 & year < 2000){
    val <- ((2000 - year)*bu90 + (year - 1990)*bu00)/10
    return(val)
  }
  else if (year > 2000 & year < 2014){
    val <- ((2014 - year)*bu00 + (year - 2000)*bu14)/14
    return(val)
  }
}

dat$builtup <- mapply(FUN=estBuiltUp, year=dat$interview_year, bu90=dat$bu90, bu00=dat$bu00, bu14=dat$bu14)

dat %>% 
  select(interview_year, builtup, code) %>% 
  write.csv('../../DHS Processed/builtup.csv', row.names=F)

writeRaster(bu14, '../Final Rasters/builtup.tif', format='GTiff')
