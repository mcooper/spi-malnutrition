setwd('G://My Drive/DHS Spatial Covars/Dominant Field Sizes/')

library(rgdal)
library(raster)
library(dplyr)

#First read in and resample irrigation data
r <- raster('dominant_field_sizes.tif')
# 
# Legend:
#   0 - no fields
# 3502 - very large fields
# 3503- large fields
# 3504 - medium fields
# 3505 - small fields
# 3506 - very small fields
# 
# .             Very large fields have an area of greater than 100 ha;
# .             Large fields have an area between 16 ha and 100 ha;
# .             Medium fields have an area between 2.56 ha and 16 ha;
# .             Small fields have an area between 0.64 ha and 2.56 ha;
# .             Very small fields have an area smaller than 0.64 ha.

r <- reclassify(r, matrix(c(3502, 200, 
                            3503, 58,
                            3504, 9.28,
                            3505, 1.6,
                            3506, 0.32), ncol=2, byrow = T))


resamp <- raster('../AVHRR/AVHRR_NDVI_1982-2016/1982_vi_mn_75_100.tif')

r2 <- resample(r, resamp, method="bilinear")

#then get queens case averages with focal

r2 <- focal(r2, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

#then save it to file
for (year in seq(1990, 2020)){
  writeRaster(r2, paste0('../Final Rasters/', year, '/fieldsize.tif'), format='GTiff', overwrite=TRUE)
}
  
#Then extract values, averaging across Queen's case
dat <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  unique

sp <- SpatialPointsDataFrame(coords = dat[ , c('longitude', 'latitude')], data=dat)

sp$fieldsize <- extract(r2, sp)

write.csv(sp@data, '../../DHS Processed/fieldsize.csv', row.names=F)
