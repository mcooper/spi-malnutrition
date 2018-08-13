setwd('G://My Drive/DHS Spatial Covars/Nutrition Diversity/')

library(rgdal)
library(raster)
library(dplyr)

#First read in and resample irrigation data
r <- raster('MFAD.asc', crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))

resamp <- raster('../AVHRR/AVHRR_NDVI_1982-2016/1982_vi_mn_75_100.tif')

r <- resample(r, resamp, method="bilinear")

#then get queens case averages with focal

r2 <- focal(r, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

r3 <- r2

for (i in 1:20){
  print(i)
  r3 <- focal(r3, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
}

r2[is.na(r2)] <- r3[is.na(r2)]

#then save it to file
writeRaster(r2, '../Final Rasters/nutritiondiversity.tif', format='GTiff', overwrite=T)

#Then extract values, averaging across Queen's case
dat <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  unique

sp <- SpatialPointsDataFrame(coords = dat[ , c('longitude', 'latitude')], data=dat)

sp$nutritiondiversity <- extract(r2, sp)

#Get points that are within a few cells on non-na cells, set the rest to 0
for (i in 1:10){
  r2 <- focal(r2, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
  sp$nutritiondiversity[is.na(sp$nutritiondiversity)] <- extract(r2, sp[is.na(sp$nutritiondiversity), ])
  print(sum(is.na(sp$nutritiondiversity)))
}

sp$nutritiondiversity[is.na(sp$nutritiondiversity)] <- 0

write.csv(sp@data, '../../DHS Processed/nutritiondiversity.csv', row.names=F)
