library(raster)
library(rgdal)
library(dplyr)

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  arrange(interview_year) %>%
  unique

newdata <- data.frame()
for (y in unique(data$interview_year)){
  
  sel <- data[data$interview_year == y, ]
  sp <- SpatialPointsDataFrame(coords = sel[ , c('longitude', 'latitude')], data=sel)
  
  if (y == 1994){
    y <- 1995
  }
  
  if (y == 2000){
    y <- 2001
  }
  
  print(y)
  setwd('G://My Drive/DHS Spatial Covars/AVHRR/AVHRR_NDVI_1982-2016')
  ndvi <- raster(list.files(pattern=as.character(y)))
  
  setwd('G://My Drive/DHS Spatial Covars/AVHRR/AVHRR Forest/')
  forest <- raster(list.files(pattern=as.character(y)))
  
  setwd('G://My Drive/DHS Spatial Covars/AVHRR/AVHRR Bareground/')
  bare <- raster(list.files(pattern=as.character(y)))

  print('focal')
  ndvi <- focal(ndvi, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
  forest <- focal(forest, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
  bare <- focal(bare, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
  
  print('extract')
  sel$ndvi <- extract(ndvi, sp)
  sel$forest <- extract(forest, sp)
  sel$bare <- extract(bare, sp)
  
  #One friggin NA in Nigeria?
  while (sum(is.na(sel$ndvi)) > 0){
    print('dealing with NAs in Nigeria')
    ndvi <- focal(ndvi, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
    sel$ndvi[is.na(sel$ndvi)] <- extract(ndvi, sp[is.na(sel$ndvi), ])
  }
  
  newdata <- bind_rows(newdata, sel)
}

write.csv(newdata, '../../../DHS Processed/avhrr.csv', row.names=F)

writeRaster(ndvi, '../../Final Rasters/ndvi.tif', format='GTiff')
writeRaster(bare, '../../Final Rasters/bare.tif', format='GTiff')
writeRaster(forest, '../../Final Rasters/forest.tif', format='GTiff')