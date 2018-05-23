setwd('G://My Drive/DHS Spatial Covars/Irrigation/')

library(rgdal)
library(raster)
library(rgeos)
library(dplyr)

##########################################
#First test different methods of extraction
#
#It after examining outputs in QGIS, I think biliner works the best
############################################

# 
# data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
#   select(latitude, longitude, code, interview_year) %>%
#   unique
# 
# r <- raster('gmia_v5_aei_pct.asc', crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))
# 
# sp <- SpatialPointsDataFrame(coords = data[ , c('longitude', 'latitude')], data=data, 
#                              proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))
# 
# sp <- spTransform(sp, CRS = CRS('+proj=aeqd +lat_0=0 +lon_0=0'))
# r <- projectRaster(r, crs = CRS('+proj=aeqd +lat_0=0 +lon_0=0'))
# 
# sp$irrig_simple <- raster::extract(r, sp)
# sp$irrig_bilinear <- raster::extract(r, sp, method='bilinear', fun=mean)
# 
# sp$irrig_extracbuffer <- raster::extract(r, sp, buffer=5000, fun=mean)
# 
# spbuf <- gBuffer(sp, width=5000, byid=TRUE)
# 
# spbuf$irrig_manualbuffer <- raster::extract(r, sp, fun=mean)
# 
# writeOGR(spbuf, '.', 'irrig_buffer.shp', driver='ESRI Shapefile')
# 
# writeRaster(r, 'irrig_proj.tif', format='GTiff')

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  select(latitude, longitude, code, interview_year) %>%
  unique

r <- raster('gmia_v5_aei_pct.asc', crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))

sp <- SpatialPointsDataFrame(coords = data[ , c('longitude', 'latitude')], data=data,
                             proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))

sp$irrigation <- raster::extract(r, sp, method='bilinear', fun=mean)

write.csv(sp@data, '../../DHS Processed/irrigation.csv', row.names=F)

writeRaster(r, '../Final Rasters/irrigation.tif', format='GTiff')







