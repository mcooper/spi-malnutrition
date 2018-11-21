setwd('G://My Drive/DHS Spatial Covars/FEWS Validation/Shapefiles')

library(rgdal)
library(raster)
library(dplyr)

getFEWSdata <- function(prefix, resolution=0.1, verbose=FALSE){
  all <- data.frame()
  for (i in list.files(pattern=paste0(prefix, '.*shp$'))){
    sp <- readOGR('.', gsub('.shp', '', i), verbose=F)
    
    ext <- extent(sp)
    
    lats <- seq(-90.025, 90.025, resolution)
    lons <- seq(-180.025, 180.025, resolution)
    
    lats <- lats[lats < ext[4] & lats > ext[3]]
    lons <- lons[lons < ext[2] & lons > ext[1]]
    
    gd <- expand.grid(lons, lats)
    names(gd) <- c('longitude', 'latitude')
    
    points <- SpatialPoints(gd, proj4string = CRS(proj4string(sp)))
    
    dat <- over(points, sp, field="CS")
    
    gd$CS <- dat$CS
    
    gd <- na.omit(gd)
    
    gd$yearmonth <- gsub("\\D", '', i)
  
    all <- bind_rows(all, gd)
    
    if(verbose){
      print(i)
    }
  }
  return(all)
}

#Latin America + Carribean
LAC <- getFEWSdata("LAC", resolution=0.25, verbose=T)

#Central Asia
CA <- getFEWSdata("CA", resolution=0.25, verbose=T)

#West Africa
WA <- getFEWSdata("WA", resolution=0.25, verbose=T)

#Southern Africa
SA <- getFEWSdata("SA", resolution=0.25, verbose=T)

#Eastern Africa
EA <- getFEWSdata("EA", resolution=0.25, verbose=T)

FEWS <- Reduce(bind_rows, list(LAC, CA, WA, SA, EA))

write.csv(FEWS, '../FEWS_CS_categories.csv', row.names=F)
