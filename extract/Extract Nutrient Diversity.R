setwd('G://My Drive/DHS Spatial Covars/Nutrition Diversity/')

library(rgdal)
library(raster)
library(dplyr)

#First read in and resample irrigation data
mfad <- raster('MFAD.asc', crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))
h <- raster('H.asc', crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))
s <- raster('S.asc', crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))

mfad[mfad < 0.15] <- NA

resamp <- raster('../AVHRR/1982_vi_mn_75_100.tif')

mfad <- resample(mfad, resamp, method="bilinear")
h <- resample(h, resamp, method="bilinear")
s <- resample(s, resamp, method="bilinear")

#####################################
#MFAD
#######################################
#get queens case averages with focal
mfad2 <- focal(mfad, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

#Fill NA space with nearest neighbor
mfad3 <- mfad2
for (i in 1:20){
  print(i)
  mfad3 <- focal(mfad3, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
}
mfad2[is.na(mfad2)] <- mfad3[is.na(mfad2)]

#then save it to file
for (year in seq(1990, 2020)){
  writeRaster(mfad2, paste0('../Final Rasters/', year, '/nutritiondiversity_mfad.tif'), format='GTiff', overwrite=T)
}

#####################################
#S
#######################################
#get queens case averages with focal
s2 <- focal(s, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

#Fill NA space with nearest neighbor
s3 <- s2
for (i in 1:20){
  print(i)
  s3 <- focal(s3, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
}
s2[is.na(s2)] <- s3[is.na(s2)]

#then save it to file
for (year in seq(1990, 2020)){
  writeRaster(s2, paste0('../Final Rasters/', year, '/nutritiondiversity_s.tif'), format='GTiff', overwrite=T)
}

#####################################
#H
#######################################
#get queens case averages with focal
h2 <- focal(h, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

#Fill NA space with nearest neighbor
h3 <- h2
for (i in 1:20){
  print(i)
  h3 <- focal(h3, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
}
h2[is.na(h2)] <- h3[is.na(h2)]

#then save it to file
for (year in seq(1990, 2020)){
  writeRaster(h2, paste0('../Final Rasters/', year, '/nutritiondiversity_h.tif'), format='GTiff', overwrite=T)
}

#Then extract values, averaging across Queen's case
dat <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  unique

sp <- SpatialPointsDataFrame(coords = dat[ , c('longitude', 'latitude')], data=dat)

sp$nutritiondiversity_mfad <- extract(mfad2, sp)
sp$nutritiondiversity_h <- extract(h2, sp)
sp$nutritiondiversity_s <- extract(s2, sp)

#Cormoros is NA, assume values are similar to those in coastal TZA or MDG
sp$nutritiondiversity_mfad[is.na(sp$nutritiondiversity_mfad)] <- 0.598502
sp$nutritiondiversity_h[is.na(sp$nutritiondiversity_h)] <- 1.421454
sp$nutritiondiversity_s[is.na(sp$nutritiondiversity_s)] <- 30

write.csv(sp@data, '../../DHS Processed/nutritiondiversity.csv', row.names=F)
