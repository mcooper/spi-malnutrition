####################################
#Script to get all landcover within 25km of a point. From Globeland 30
#
#Right now reprojecting and mosiacking is a pain, so will only do points that are
#fully within a scene when buffered
####################################

library(dplyr)
library(foreach)
library(doParallel)
library(rgdal)
library(raster)
library(rgeos)

options(stringsAsFactors = F)

#First determine which raster each point belongs to
sp <- read.csv('~/dhsprocessed/sp_export.csv') %>%
  dplyr::select(interview_year, code, latitude, longitude) %>%
  unique

sp <- SpatialPointsDataFrame(sp[ , c('longitude', 'latitude')], data=sp, proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

globeland <- readOGR('~/globeland30/globemapsheet', 'GlobeMapSheet')

sp@data$image <- over(sp, globeland) %>%
  .$REMARK

touchmat <- gTouches(globeland, byid=TRUE)

colnames(touchmat) <- globeland$REMARK
rownames(touchmat) <- globeland$REMARK

rownames(touchmat)[touchmat[ , img]]

mk_img_file <- function(img, year){
  splt <- strsplit(img, '_')[[1]]
  lat <- splt[1]
  lon <- splt[2]
  
  if (nchar(lat)==2){
    lat <- paste0(substr(lat, 1, 1), "0",
                  substr(lat, 1, 1))
  }
  
  if (nchar(lon)==1){
    lon <- paste0("0", lon)
  }
  
  paste0(lat, "_", lon, "_", year, "LC030")
}

getRast <- function(f, year){
  #Format name
  f <- mk_img_file(f, year)
  
  #Copy file from blob storage
  system(paste0("cp ~/globeland30/", f, '.zip ', f, '.zip'))
  
  #Unzip file
  system(paste0("unzip -o ", f, ".zip"))
  
  #Read Raster
  r <- raster(paste0(f, "/", tolower(f), ".tif"))
  
  return(r)
}

cl <- makeCluster(4, outfile = '')
registerDoParallel(cl)

foreach(i=unique(sp@data$image), .packages=c('raster', 'dplyr', 'rgdal', 'rgeos')) %dopar% {
  system(paste0("mkdir ", i))
  
  setwd(i)
  
  #Copy over center images, unzip, and read them in
  c00 <- getRast(i, 2000)
  c10 <- getRast(i, 2010)
  
  #Todo: figure out mosaicing
  #Not really working right now
  #Even when they have the same projection, the origin of images are different
  
  # 
  # #Get neighboring polygons
  # nb <- rownames(touchmat)[touchmat[ , i]]
  # ilong <- strsplit(i, '_')[[1]][1]
  # nblong <- sapply(nb, function(x) strsplit(x, '_')[[1]][1])
  # nb <- nb[nblong==ilong]
  # 
  # #Get remaining images for 2000 as list
  # l00 <- list(c00)
  # ind <- 2
  # for (img in nb){
  #   r <- getRast(img, 2000)
  #   #reproject if necessary
  #   if (proj4string(r) != proj4string(c00)){
  #     #r <- projectRaster(r, crs=CRS(proj4string(c00)))
  #     next
  #   }
  #   l00[[ind]] <- r
  #   ind <- ind + 1
  # }
  # 
  # #Get remaining images for 2010
  # l10 <- list(c10)
  # ind <- 2
  # for (img in nb){
  #   r <- getRast(img, 2010)
  #   #reproject if necessary
  #   if (proj4string(r) != proj4string(c10)){
  #     #r <- projectRaster(r, crs=CRS(proj4string(c10)))
  #     next
  #   }
  #   l10[[ind]] <- r
  #   ind <- ind + 1
  # }
  # 
  # #mosaic 2000
  # l00[['fun']] <- max
  # r00 <- do.call(mosaic, l00)
  # 
  # #mosaic 2010
  # l10[['fun']] <- max
  # r10 <- do.call(mosaic, l10)
  
  sel <- sp[sp@data$image == i, ]
  
  sel <- spTransform(sel, CRS(proj4string(c00)))
  
  valmap <- data.frame(val=c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 255),
                       lc=c('agriculture', 'forest', 'grassland', 'shrubland', 'wetland', 'landwater',
                            'tundra', 'artificial', 'bare', 'snow', 'ocean'))

  v00 <- getValues(c00)
  v10 <- getValues(c10)
  
  for (v in 1:nrow(valmap)){
    if(valmap$val[v] %in% v00){
      sel[ , paste0(valmap$lc[v], "00")] <- extract(c00==valmap$val[v], sel, buffer=25000, fun=sum)
    }
    if(valmap$val[v] %in% v10){
      sel[ , paste0(valmap$lc[v], "10")] <- extract(c10==valmap$val[v], sel, buffer=25000, fun=sum)
    }
  }
  
  write.csv(sel@data, paste0('~/globeland_summaries/', i, '.csv'), row.names=F)
  
  setwd("~")
  
  system(paste0("rm ~/", i, " -rf"))

  cat(i, which(i == unique(sp@data$image))/length(unique(sp@data$image)), "\n")
}