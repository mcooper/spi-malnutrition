setwd('D://Documents and Settings/mcooper/Google Drive/Dissertation/Farming Systems/')

library(rgdal)

sps <- list.files(pattern='.shp$')
sps <- gsub('.shp$', '', sps)

files <- sapply(sps, FUN=readOGR, dsn='.')

process <- function(dat, rename){
  if('DESC' %in% names(dat@data)){
    names(dat@data)[names(dat@data)=='DESC'] <- 'DESCRIPTIO'
  }
  if('SYSTEM' %in% names(dat@data) & !'DESCRIPTIO' %in% names(dat@data)){
    dat@data$DESCRIPTIO <- dat@data$SYSTEM
  }
  if(!'SYSTEM' %in% names(dat@data)){
    dat@data$SYSTEM <- NA
  }
  dat@data <- dat@data[ , c('DESCRIPTIO', 'THEID', 'SYSTEM')]
  dat@data$ORIG <- rename
  return(dat)  
}

procfiles <- mapply(FUN=process, dat=files, rename=names(files))

comb <- rbind(procfiles[[1]], procfiles[[2]], procfiles[[3]], procfiles[[4]],
              procfiles[[5]], procfiles[[6]], makeUniqueIDs=TRUE)

writeOGR(comb, dsn = '.', 'all_farming_systems', driver='ESRI Shapefile', overwrite_layer = T)
