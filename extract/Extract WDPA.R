library(sp)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)
library(foreach)
library(doParallel)

wdpa <- readOGR('.', 'WDPA_Aug2018-shapefile-polygons',
                stringsAsFactors=F)

wdpa <- spTransform(wdpa, CRS("+proj=aeqd +lat_0=0 +lon_0=0"))

wdpa@data$STATUS_YR <- as.numeric(wdpa@data$STATUS_YR)

wdpa <- wdpa[wdpa$IUCN_CAT %in% c('Ia', 'Ib', 'II', 'III', 'IV', 'V', 'VI') & wdpa$STATUS_YR != 0, ]

sp <- read.csv('~/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  unique

sp <- SpatialPointsDataFrame(sp[ , c('longitude', 'latitude')], sp, proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

sp <- spTransform(sp, CRS("+proj=aeqd +lat_0=0 +lon_0=0"))

cl <- makeCluster(28, outfile = '')
registerDoParallel(cl)

foreach(i=unique(sp$interview_year), .packages=c('sp', 'rgeos', 'rgdal', 'dplyr', 'raster')) %dopar% {
  print(i)
  
  wdpa_sel <- wdpa[wdpa$STATUS_YR <= i, ]
  sp_sel <- sp[sp$interview_year == i, ]
  
  for(j in seq_along(sp_sel)){
    code <- sp_sel@data[j, 'code']
    year <- sp_sel@data[j, 'interview_year']
    
    ext <- extent(sp_sel[j, ])
    e2 <- extent(ext[1] - 100000, ext[2] + 100000, ext[3] - 100000, ext[4] + 100000)
    wdpa_sel2 <- crop(wdpa_sel, e2)
    
    if (length(wdpa_sel2) >= 1){
      dists <- gDistance(sp_sel[j,], wdpa_sel2, byid=TRUE)
      nearestPaClass <- wdpa_sel$IUCN_CAT[which.min(dists)]
      nearestPaID <- wdpa_sel$WDPAID[which.min(dists)]
      nearestPaDist <- min(dists)
    } else{
      nearestPaClass <- NA
      nearestPaID <- NA
      nearestPaDist <- NA
    }
    
    df <- data.frame(code, year, nearestPaClass, nearestPaID, nearestPaDist)
    
    write.csv(df, paste0('~/WDPAoutputs/', code, year), row.names=F)
  }
}


