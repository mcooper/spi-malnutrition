setwd('G://My Drive/DHS Spatial Covars/Elevation/')

library(rgdal)
library(raster)
library(rgeos)
library(dplyr)

#https://www.eea.europa.eu/data-and-maps/data/world-digital-elevation-model-etopo5

r <- raster('alwdgg.tif')
r[r < 0] <- NA

resamp <- raster('../AVHRR/AVHRR_NDVI_1982-2016/1982_vi_mn_75_100.tif')

r <- resample(r, resamp, method="bilinear")

#get queens case averages with focal

r2 <- focal(r, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

#then save it to file
for (year in seq(1990, 2020)){
  writeRaster(r2, paste0('../Final Rasters/', year, '/elevation.tif'), format='GTiff')
}
  
#Then extract values
dat <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  unique

sp <- SpatialPointsDataFrame(coords = dat[ , c('longitude', 'latitude')], data=dat)

dat$elevation <- extract(r2, sp)

dat$elevation[is.na(dat$elevation)] <- 0

write.csv(dat, '../../DHS Processed/elevation.csv', row.names=F)
