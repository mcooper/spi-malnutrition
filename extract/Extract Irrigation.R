setwd('G://My Drive/DHS Spatial Covars/Irrigation/')

library(gdalUtils)
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
library(foreach)

#First read in and resample irrigation data
r <- raster('gmia_v5_aei_pct.asc', crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))

resamp <- raster('../AVHRR/AVHRR_NDVI_1982-2016/1982_vi_mn_75_100.tif')

r <- resample(r, resamp, method="bilinear")

#then get queens case averages with focal

r2 <- focal(r, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

#then save it to file
writeRaster(r2, '../Final Rasters/irrigation.tif', format='GTiff')

#Then extract values, averaging across Queen's case

dat <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  unique

sp <- SpatialPointsDataFrame(coords = dat[ , c('longitude', 'latitude')], data=dat)

dat$irrigation <- extract(r2, sp)

write.csv(dat, '../../DHS Processed/irrigation.csv', row.names=F)
