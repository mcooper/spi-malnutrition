setwd('D://Documents and Settings/mcooper/Google Drive/Dissertation/')

library(dplyr)
library(sp)
library(rgeos)
library(rgdal)
library(raster)

sp <- read.csv('sp_export.csv', stringsAsFactors = F)

sp <- SpatialPointsDataFrame(sp[ , c('LONGNUM', 'LATNUM')], sp, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

fews <- readOGR('FEWS', 'FEWS_NET_LH_World')

cols <- c("COUNTRY", "LZCODE", "LZNAMEEN", "CLASS")

sp@data[ , cols] <- over(sp, fews[ , cols])

write.csv(sp@data, 'FEWS_Zones.csv', row.names=F)
