setwd('G://My Drive/DHS Spatial Covars/Elevation/')

library(rgdal)
library(raster)
library(rgeos)
library(dplyr)

#GTOPO30

r <- raster('exportElev.tif')
r[r < 0] <- NA

resamp <- raster('../AVHRR/AVHRR_NDVI_1982-2016/1982_vi_mn_75_100.tif')

#https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/terrain
roughness <- focal(r, matrix(rep(1, 9), ncol=3), fun=function(x){max(x, na.rm=T)-min(x, na.rm=T)}, padValue=NA)
roughness[is.infinite(roughness)] <- NA

rough_res <- resample(roughness, resamp)

#get queens case averages with focal

rough_res_foc <- focal(rough_res, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

#then save it to file
writeRaster(r2, '../Final Rasters/roughness.tif', format='GTiff')

#Then extract values, averaging across Queen's case

dat <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code) %>%
  unique

sp <- SpatialPointsDataFrame(coords = dat[ , c('longitude', 'latitude')], data=dat)

dat$roughness <- extract(rough_res_foc, sp)

write.csv(dat, '../../DHS Processed/roughness.csv', row.names=F)
