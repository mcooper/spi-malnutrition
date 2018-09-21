library(raster)
library(rgdal)
library(dplyr)

setwd('G://My Drive/DHS Spatial Covars/AVHRR')

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  arrange(interview_year) %>%
  unique

extractYear <- function(sp, y){
  ndvi <- raster(paste0(y, '_vi_mn_75_100.tif'))/10000
  
  vcf <- stack(paste0('VCF_', y, '.tif'))
  
  #1st layer - Forest
  #2nd layer - Other Vegetation (1 - (Forest + Bare))
  #3rd layer - Bare
  
  forest <- vcf[[1]]
  bare <- vcf[[3]]
  
  print('focal')
  ndvi <- focal(ndvi, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
  forest <- focal(forest, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
  bare <- focal(bare, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
  
  print('extract')
  sp@data$ndvi <- extract(ndvi, sp)
  sp@data$forest <- extract(forest, sp)
  sp@data$bare <- extract(bare, sp)
  
  #One friggin NA in Nigeria?
  while (sum(is.na(sp@data$ndvi)) > 0){
    print('dealing with NAs in Nigeria')
    ndvi <- focal(ndvi, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
    sp@data$ndvi[is.na(sp@data$ndvi)] <- extract(ndvi, sp[is.na(sp@data$ndvi), ])
  }
  
  return(sp@data)
}

newdata <- data.frame()
for (y in unique(data$interview_year)){
  print(y)
  
  sel <- data[data$interview_year == y, ]
  sp <- SpatialPointsDataFrame(coords = sel[ , c('longitude', 'latitude')], data=sel)
  
  if (y == 1994 | y == 2000){
    res1 <- extractYear(sp, y - 1)
    res2 <- extractYear(sp, y + 1)
    
    res <- bind_rows(res1, res2) %>%
      group_by(latitude, longitude, code) %>%
      summarize(ndvi=mean(ndvi),
                forest=mean(forest),
                bare=mean(bare)) %>%
      mutate(interview_year = y)
  } else{
    res <- extractYear(sp, y)
  }
  
  newdata <- bind_rows(newdata, res)
}

write.csv(newdata, '../../DHS Processed/avhrr.csv', row.names=F)

#Read in 2016 rasters and write them
ndvi <- raster('2016_vi_mn_75_100.tif')/10000
vcf <- stack('VCF_2016.tif')
forest <- vcf[[1]]
bare <- vcf[[3]]

ndvi <- focal(ndvi, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
forest <- focal(forest, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
bare <- focal(bare, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

writeRaster(ndvi, '../Final Rasters/ndvi.tif', format='GTiff', overwrite=TRUE)
writeRaster(bare, '../Final Rasters/bare.tif', format='GTiff', overwrite=TRUE)
writeRaster(forest, '../Final Rasters/forest.tif', format='GTiff', overwrite=TRUE)