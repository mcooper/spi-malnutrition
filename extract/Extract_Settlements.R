setwd('G://My Drive/DHS Spatial Covars/Settlements/')

library(rgdal)
library(raster)
library(dplyr)

s90 <- raster('settlements90.tif')
s00 <- raster('settlements00.tif')
s15 <- raster('settlements15.tif')

s90high <- s90 > 2
s00high <- s00 > 2
s15high <- s15 > 2

s90low <- s90 > 1
s00low <- s00 > 1
s15low <- s15 > 1

ref <- raster('chirps-v2.0.1981.01.tif')

s90high_res <- resample(s90high, ref, method="bilinear")
s00high_res <- resample(s00high, ref, method="bilinear")
s15high_res <- resample(s15high, ref, method="bilinear")

s90low_res <- resample(s90low, ref, method="bilinear")
s00low_res <- resample(s00low, ref, method="bilinear")
s15low_res <- resample(s15low, ref, method="bilinear")

s90high_res_fc <- focal(s90high_res, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
s00high_res_fc <- focal(s00high_res, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
s15high_res_fc <- focal(s15high_res, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

s90low_res_fc <- focal(s90low_res, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
s00low_res_fc <- focal(s00low_res, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
s15low_res_fc <- focal(s15low_res, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

dat <- read.csv('~/dhsprocessed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  unique

sp <- SpatialPointsDataFrame(coords = dat[ , c('longitude', 'latitude')], data=dat)

dat$high90 <- extract(s90high_res_fc, sp)
dat$high00 <- extract(s00high_res_fc, sp)
dat$high15 <- extract(s15high_res_fc, sp)

dat$low90 <- extract(s90low_res_fc, sp)
dat$low00 <- extract(s00low_res_fc, sp)
dat$low15 <- extract(s15low_res_fc, sp)

estSettle <- function(year, settle90, settle00, settle15){
  if (year <= 1990){
    return(settle90)
  }
  else if (year == 2000){
    return(settle00)
  }
  else if (year >= 2015){
    return(settle15)
  }
  else if (year > 1990 & year < 2000){
    val <- ((2000 - year)*settle90 + (year - 1990)*settle00)/10
    return(val)
  }
  else if (year > 2000 & year < 2015){
    val <- ((2015 - year)*settle00 + (year - 2000)*settle15)/15
    return(val)
  }
}

dat$high_settle <- mapply(FUN=estSettle, year=dat$interview_year, settle90=dat$high90, settle00=dat$high00, settle15=dat$high15)

dat$low_settle <- mapply(FUN=estSettle, year=dat$interview_year, settle90=dat$low90, settle00=dat$low00, settle15=dat$low15)


dat %>% 
  select(interview_year, high_settle, low_settle, code) %>% 
  write.csv('~/dhsprocessed/settled.csv', row.names=F)

writeRaster(s90high_res_fc, '~/dhsprocessed/high_settle1990.tif', format='GTiff', overwrite=T)
writeRaster(s00high_res_fc, '~/dhsprocessed/high_settle2000.tif', format='GTiff', overwrite=T)
writeRaster(s15high_res_fc, '~/dhsprocessed/high_settle2015.tif', format='GTiff', overwrite=T)

writeRaster(s90low_res_fc, '~/dhsprocessed/low_settle1990.tif', format='GTiff', overwrite=T)
writeRaster(s00low_res_fc, '~/dhsprocessed/low_settle2000.tif', format='GTiff', overwrite=T)
writeRaster(s15low_res_fc, '~/dhsprocessed/low_settle2015.tif', format='GTiff', overwrite=T)



