setwd('G://My Drive/DHS Spatial Covars/Farm Systems')

library(dplyr)
library(sp)
library(rgeos)
library(rgdal)
library(raster)

sp <- read.csv('../../DHS Processed/sp_export.csv', stringsAsFactors = F) %>%
  dplyr::select(latitude, longitude, code) %>%
  unique

sp <- SpatialPointsDataFrame(sp[ , c('longitude', 'latitude')], sp, 
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

fao <- readOGR('.', 'all_farming_systems', p4s = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
fao@data$ID <- seq(1, nrow(fao@data))

fao <- spTransform(fao, CRS('+proj=aeqd +lat_0=0 +lon_0=0'))
sp <- spTransform(sp, CRS('+proj=aeqd +lat_0=0 +lon_0=0'))

#First get overlapping points and polygons
sp@data[names(fao)] <- over(sp, fao)

#Now handle those that are outside polygons
goodcoords <- sp[!is.na(sp@data$DESCRIPTIO), ]
badcoords <- sp[is.na(sp@data$DESCRIPTIO), ]

n <- 1:nrow(badcoords)
total <- nrow(badcoords)

for (i in n){
  badcoords@data[i, 'ID'] <- fao$ID[which.min(gDistance(badcoords[i, ], fao, byid=T))]
  print(which(n==i)/total)
}

badcoords@data[ , c("DESCRIPTIO", "THEID", "SYSTEM", "ORIG")] <- NULL

badcoords_df <- merge(badcoords@data, fao@data, all.x=T, all.y=F)

all <- bind_rows(goodcoords@data, badcoords_df) %>%
  dplyr::select(code, continent=ORIG, farm_system=DESCRIPTIO, farm_system_id=ID)

write.csv(all, '../../DHS Processed/FarmingSystems.csv', row.names = F)


fao <- readOGR('.', 'all_farming_systems', p4s = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
fao@data$ID <- seq(1, nrow(fao@data))

ref <- raster('G://My Drive/CHIRPS/Monthly/chirps-v2.0.1981.01.tif')

rid <- rasterize(fao, ref, field='ID')

writeRaster(rid, 'G://My Drive/DHS Spatial Covars/Farm Systems/farm_system_id.tif', format='GTiff')
