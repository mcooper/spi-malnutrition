library(sp)
library(rgeos)
library(rgdal)
library(dplyr)
library(foreach)
library(doParallel)

wdpa <- readOGR('.', 'WDPA_Aug2018-shapefile-polygons',
                stringsAsFactors=F)

wdpa <- spTransform(wdpa, CRS("+proj=aeqd +lat_0=0 +lon_0=0"))

wdpa@data$STATUS_YR <- as.numeric(wdpa@data$STATUS_YR)

wdpa <- wdpa[wdpa$IUCN_CAT %in% c('Ia', 'Ib', 'II', 'III', 'IV', 'V', 'VI') & wdpa$STATUS_YR != 0, ]

sp <- read.csv('~/sp_export.csv') %>%
  select(latitude, longitude, code, interview_year) %>%
  unique

sp <- SpatialPointsDataFrame(sp[ , c('longitude', 'latitude')], sp, proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

sp <- spTransform(sp, CRS("+proj=aeqd +lat_0=0 +lon_0=0"))

cl <- makeCluster(detectCores(), outfile = '')
registerDoParallel(cl)

for (i in unique(sp$interview_year)){
  print(i)
  
  wdpa_sel <- wdpa[wdpa$STATUS_YR <= i, ]
  sp_sel <- sp[sp$interview_year == i, ]
  
  foreach(j=seq_along(sp_sel), .packages=c('sp', 'rgeos', 'rgdal', 'dplyr')) %dopar% {
    code <- sp_sel@data[j, 'code']
    year <- sp_sel@data[j, 'interview_year']
    
    dists <- gDistance(sp_sel[j,], wdpa_sel, byid=TRUE)
    nearestPaClass <- wdpa_sel$IUCN_CAT[which.min(dists)]
    nearestPaID <- wdpa_sel$WDPAID[which.min(dists)]
    nearestPaDist <- min(dists)
    
    df <- data.frame(code, year, nearestPaClass, nearestPaID, nearestPaDist)
    
    write.csv(df, '~/WDPAoutputs/', row.names=F)
  }
}


