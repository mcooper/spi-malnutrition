library(sp)
library(rgeos)
library(rgdal)
library(dplyr)

wdpa <- readOGR('G://My Drive/WDPA', 'WDPA_July2018-shapefile-polygons',
                stringsAsFactors=F)

wdpa <- wdpa[wdpa$IUCN_CAT %in% c('Ia', 'Ib', 'II', 'III', 'IV', 'V', 'VI') & wdpa$Status_YR != 0, ]

wdpa <- spTransform(wdpa, CRS("+proj=aeqd +lat_0=0 +lon_0=0"))

sp <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  select(latitude, longitude, code, interview_year) %>%
  unique

sp <- SpatialPointsDataFrame(sp[ , c('longitude', 'latitude')], sp, proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

n <- length(sp)
nearestPaClass <- character(n)
nearestPaDist <- character(n)

#TODO: Paralellize and run in cloud
wdpa$STATUS_YR <- as.numeric(wdpa$STATUS_YR)

for (i in unique(sp$interview_year)){
  wdpa_sel <- wdpa[as.numeric(wdpa$STATUS_YR) <= i, ]
  sp_sel <- sp[sp$interview_year == i, ]
  
  for (j in seq_along(sp_sel)){
    code <- sp_sel@data[j, 'code']
    year <- sp_sel@data[j, 'interview_year']
    
    dists <- gDistance(sp_sel[j,], wdpa_sel, byid=TRUE)
    nearestPaClass <- wdpa_sel$IUCN_CAT[which.min(dists)]
    nearestPaID <- wdpa_sel$WDPAID[which.min(dists)]
    nearestPaDist <- min(dists)
    
    sp$nearestPaClass[sp$code==code & sp$interview_year==year] <- nearestPaClass
    sp$nearestPaID[sp$code==code & sp$interview_year==year] <- nearestPaID
    sp$nearestPaDist[sp$code==code & sp$interview_year==year] <- nearestPaDist
  }
}


