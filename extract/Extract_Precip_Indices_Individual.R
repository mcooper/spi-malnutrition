library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('/mnt/mnt')

dat <- read.csv('~/dhsprocessed/hhvars.csv') %>%
  select(interview_month, interview_year, code, latitude, longitude, age)

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('CHIRPS/chirps-v2.0.1981.01.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[r==-9999] <- NA

sp@data$tmpcode <- extract(codes, sp)

#Deal with points near a coast, coming up NA
spna <- sp[is.na(sp@data$tmpcode) , ]
spna$tmpcode <- NULL
badcoords <- unique(spna@coords)
tmpcode <- apply(X = badcoords, MARGIN = 1, FUN = function(xy) codes@data@values[which.min(replace(distanceFromPoints(codes, xy), is.na(codes), NA))])
badcoords <- cbind.data.frame(badcoords, tmpcode)
spna <- merge(spna@data, badcoords)
sp <- bind_rows(spna, sp@data[!is.na(sp@data$tmpcode), ])

rll <- rasterToPoints(codes) %>% data.frame
rll <- rll[rll$layer %in% sp$tmpcode, ]

#Read in precip data
precip_in_folder <- 'CHIRPS/'
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files(precip_in_folder, pattern='tif$')
gdalbuildvrt(paste0(precip_in_folder, precip_files), precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in phenology data
pheno_in_folder <- 'Phenology/'
pheno_vrt_file <- extension(rasterTmpFile(), 'ivrt')
pheno_files <- list.files(pheno_in_folder, pattern='tif$')
gdalbuildvrt(paste0(pheno_in_folder, pheno_files), pheno_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmax data
tmax_in_folder <- 'TMAX/'
tmax_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmax_files <- list.files(tmax_in_folder, pattern='^tmax.*tif$')
gdalbuildvrt(paste0(tmax_in_folder, tmax_files), tmax_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmin data
tmin_in_folder <- 'TMIN/'
tmin_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmin_files <- list.files(tmin_in_folder, pattern='^tmin.*tif$')
gdalbuildvrt(paste0(tmin_in_folder, tmin_files), tmin_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

extract_neighbors <- function(vrt, x, y){
  
  m <- gdallocationinfo(vrt, x, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  u <- gdallocationinfo(vrt, x, y + 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  b <- gdallocationinfo(vrt, x, y - 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  l <- gdallocationinfo(vrt, x - 0.05, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  r <- gdallocationinfo(vrt, x + 0.05, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  ul <- gdallocationinfo(vrt, x - 0.05, y + 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  ur <- gdallocationinfo(vrt, x + 0.05, y + 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  bl <- gdallocationinfo(vrt, x - 0.05, y - 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  br <- gdallocationinfo(vrt, x + 0.05, y - 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  dat <- cbind(m, u, b, l, r, ul, ur, bl, br)
  
  dat[dat == -9999] <- NA
  
  return(rowMeans(dat, na.rm=T))
  
}


parseDates <- function(dates){
  
  dates <- as.numeric(dates)
  
  dates[dates < 0] <- 0
  
  if (all(dates == 0)){
    return(rep(1, 432))
  }
  
  #Get number from 0-36 (see under 'Phenology': https://mars.jrc.ec.europa.eu/asap/download.php)
  dates[dates > 36 & dates <= 72] <- dates[dates > 36 & dates <= 72] - 36
  dates[dates > 72] <- dates[dates > 72] - 72
  
  #Convert Dekad to Month
  dates <- dates/3
  dates <- ceiling(dates)
  
  year <- rep(NA, 12)
  
  #Make months of first season 1
  if (dates[1] < dates[2]){
    year[dates[1]:dates[2]] <- 1
  } else{
    year[dates[1]:12] <- 1
    year[1:dates[2]] <- 1
  }
  
  #Make months of second season 2
  if (dates[3] > dates[4]){
    year[dates[4]:12] <- 1
    year[1:dates[3]] <- 1
  } else{
    year[dates[3]:dates[4]] <- 1
  }
  
  return(rep(year, 36))
}

getSPI <- function(index, var, window, month, year){
  ind <- which(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month') == ymd(paste(year, month, 1, sep='-')))
  
  ts <- as.numeric(index(var, window, na.rm=TRUE)$fitted)
  
  return(ts[ind])
  
}

cl <- makeCluster(64, outfile = '')
registerDoParallel(cl)

foreach(n=1:nrow(rll), .combine=bind_rows, .packages=c('raster', 'lubridate', 'gdalUtils', 'SPEI', 'dplyr', 'zoo')) %dopar% {
  
  precip <- extract_neighbors(precip_vrt_file, rll$x[n], rll$y[n])
  
  tmax <- extract_neighbors(tmax_vrt_file, rll$x[n], rll$y[n])
  
  tmin <- extract_neighbors(tmin_vrt_file, rll$x[n], rll$y[n])
  
  pheno <- gdallocationinfo(pheno_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE)[c(3, 1, 4, 2)]
  
  phenoMask <- parseDates(pheno)
  
  precip_season <- precip*phenoMask
  
  PET <- hargreaves(tmin-273.15, tmax-273.15, lat=rll$y[n], Pre=precip) %>%
    as.vector
  
  s <- precip - PET
  
  s_season <- precip*phenoMask
  
  sel <- sp[sp$tmpcode == rll$layer[n], ]
  
  for (i in 1:nrow(sel)){
    sel$spi_age[i] <- getSPI(spi, precip, sel$age[i], sel$interview_month[i], sel$interview_year[i])
    sel$spei_age[i] <- getSPI(spei, s, sel$age[i], sel$interview_month[i], sel$interview_year[i])
    sel$spi_gs_age[i] <- getSPI(spi, precip_season, sel$age[i], sel$interview_month[i], sel$interview_year[i])
    sel$spei_gs_age[i] <- getSPI(spei, s_season, sel$age[i], sel$interview_month[i], sel$interview_year[i])
    sel$spi_ageutero[i] <- getSPI(spi, precip, sel$age[i] + 9, sel$interview_month[i], sel$interview_year[i])
    sel$spei_ageutero[i] <- getSPI(spei, s, sel$age[i] + 9, sel$interview_month[i], sel$interview_year[i])
    sel$spi_gs_ageutero[i] <- getSPI(spi, precip_season, sel$age[i] + 9, sel$interview_month[i], sel$interview_year[i])
    sel$spei_gs_ageutero[i] <- getSPI(spei, s_season, sel$age[i] + 9, sel$interview_month[i], sel$interview_year[i])
  }
  
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  write.csv(sel, paste0('~/PrecipIndicesIndividual/', n), row.names=F)
}

setwd('~/PrecipIndicesIndividual/')

df <- list.files()%>%
	lapply(read.csv) %>%
	bind_rows %>%
	select(-tmpcode)

write.csv(df, '~/dhsprocessed/PrecipIndices_Individual.csv', row.names=F)





