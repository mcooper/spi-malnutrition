setwd('G://My Drive/DHS Spatial Covars/Irrigation/')

library(gdalUtils)
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)

#' There are two rasters: One for percentage of total 
#' area equipped for irrigation and one for the 
#' percentage of the area equipped that was actually 
#' irrigated.  I will multiply the two off them to 
#' get the percentage of the total area that was
#' actually equipped for irrigation.


#First read in and resample irrigation data
aei_pct <- raster('gmia_v5_aei_pct.asc', crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))/100
aai_pct_aei <- raster('gmia_v5_aai_pct_aei.asc', crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))/100
aai_pct_aei[is.na(aai_pct_aei)] <- 0

aai_pct <- aei_pct*aai_pct_aei

resamp <- raster('../../CHIRPS/Monthly/chirps-v2.0.1981.01.tif')

aei_pct <- resample(aei_pct, resamp, method="bilinear")
aai_pct <- resample(aai_pct, resamp, method="bilinear")

#then get queens case averages with focal
aei_pct <- focal(aei_pct, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
aai_pct <- focal(aai_pct, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

#then save it to file
writeRaster(aei_pct, '../Final Rasters/irrig_aei.tif', format='GTiff', overwrite=T)
writeRaster(aai_pct, '../Final Rasters/irrig_aai.tif', format='GTiff', overwrite=T)

#Then extract values, averaging across Queen's case

dat <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code) %>%
  unique

sp <- SpatialPointsDataFrame(coords = dat[ , c('longitude', 'latitude')], data=dat)

dat$irrig_aei <- extract(aei_pct, sp)
dat$irrig_aai <- extract(aai_pct, sp)

write.csv(dat, '../../DHS Processed/irrigation.csv', row.names=F)
