setwd('~')

library(dplyr)
library(raster)
library(rgdal)
library(MASS)
library(foreach)
library(doParallel)

spbuffer <- 50000
yearlag <- 3

#
#Run in cloud
#
#

################################
#Define function from sp.kde package
#Couldn't install package on linux but only needed this snippet
###############################

fhat <- function (x, y, h, w, n = 25, lims = c(range(x), range(y))) {
  nx <- length(x)
  if (length(y) != nx) 
    stop("data vectors must be the same length")
  if (length(w) != nx & length(w) != 1) 
    stop("weight vectors must be 1 or length of data")
  if (missing(h)) { 
    h <- c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y))
  } else { 
    h <- rep(h, length.out = 2L)
  }	
  if (any(h <= 0)) stop("bandwidths must be strictly positive")
  if (missing(w)) { w <- numeric(nx) + 1 }
  gx <- seq(lims[1], lims[2], length = n[1])
  gy <- seq(lims[3], lims[4], length = n[2])
  h <- h/4
  ax <- outer(gx, x, "-") / h[1]
  ay <- outer(gy, y, "-") / h[2]
  z <- ( matrix(rep(w, n[1]), nrow = n[1], ncol = nx, byrow = TRUE) * 
           matrix(stats::dnorm(ax), n[1], nx) ) %*% t(matrix(stats::dnorm(ay), n[2], nx)) /
    ( sum(w) * h[1] * h[2] )
  return(list(x = gx, y = gy, z = z))
}

sp.kde <- function(x, y, bw, newdata) {
  n = c(raster::nrow(newdata), raster::ncol(newdata))
  k  <- fhat(sp::coordinates(x)[,1], sp::coordinates(x)[,2], w = y, 
             h = bw, n = n, lims = as.vector(raster::extent(newdata)) )
  k$z <- (k$z - min(k$z)) / (max(k$z) - min(k$z))
  kde.est <- raster::raster(sp::SpatialPixelsDataFrame(sp::SpatialPoints(expand.grid(k$x, k$y)), 
                                                       data.frame(kde = as.vector(array(k$z,length(k$z))))))							
  sp::proj4string(kde.est) <- sp::proj4string(x)
  return(kde.est)
}

################################################
#Prepare Data
################################################
data <- read.csv('ged181.csv')

sppoints <- SpatialPointsDataFrame(data[ , c('longitude', 'latitude')], data=data, proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

ref <- raster('mean_annual_precip.tif')

#Do Syria
#Get syria polygon and syria data
world <- readOGR('.', 'ne_50m_admin_0_countries')
syria_pol <- world[world$SOVEREIGNT == 'Syria', ]

pts <- ref %>% crop(syria_pol) %>%
  rasterToPoints %>%
  data.frame
rast_pts <- SpatialPoints(pts[ , c('x', 'y')], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

syria_pts <- rast_pts[syria_pol, ] %>%
  data.frame

#Data from Syrian Observatory for Human Rights
#Retreived from https://en.wikipedia.org/wiki/Casualties_of_the_Syrian_Civil_War
syria <- data.frame(year=c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                    best=c(7841, 49294, 73447, 76021, 55219, 49742, 33425, 18067))

syria_pts_data <- merge(syria_pts, syria)
syria_pts_data$best <- syria_pts_data$best/nrow(syria_pts)
names(syria_pts_data) <- c('longitude', 'latitude', 'year', 'best')

syria_pts_spdf <- SpatialPointsDataFrame(syria_pts_data[ , c('longitude', 'latitude')], data=syria_pts_data,
                                         proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

sppoints <- sppoints[ , c('longitude', 'latitude', 'year', 'best')]
sppoints <- rbind(sppoints, syria_pts_spdf)

#Convert raster and sppoints to AEZ
ref_proj <- projectRaster(ref, crs=CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84"))
sppoints <- spTransform(sppoints, CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84"))

##################################
#Make annual rasters
###################################

cl <- makeCluster(3, outfile = '')
registerDoParallel(cl)

foreach(y=c(2017, 2016, seq(2009, 1989 + yearlag)), .packages=c('raster', 'rgdal', 'MASS')) %dopar% {
  
  print(y)
  
  if(y==(1989 + yearlag)){
    sel <- sppoints[sppoints$year <= y, ]
  }else{
    sel <- sppoints[sppoints$year <= y & sppoints$year > (y-yearlag), ]
  }
  
  res <- sp.kde(x=sel, y=log(sel$best + 1), bw=spbuffer, newdata=ref_proj)
  
  resproj <- projectRaster(res, ref)
  
  writeRaster(resproj, paste0('~/bodycount', y, '.tif'), format='GTiff', overwrite=T)
}

#
#Run Locally
#
#

setwd('G://My Drive/DHS Spatial Covars/')

sp <- read.csv('../DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  unique

all <- data.frame()
for (y in seq(1992, 2016)){
  if (y == 1992){
    sel <- sp %>%
      filter(interview_year <= 1992)
  } else{
    sel <- sp %>%
      filter(interview_year == y)
  }
  r <- raster(paste0('Final Rasters/', y, '/bodycount.tif'))
  sel <- SpatialPointsDataFrame(sel[ , c('longitude', 'latitude')], data=sel)
  sel <- extract(r, sel, sp=T)
  all <- bind_rows(all, sel@data)
  print(y)
}

write.csv(all, 'G://My Drive/DHS Processed/bodycount.csv', row.names=F)

