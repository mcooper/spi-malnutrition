library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('~/geo')

dat <- read.csv('~/dhsprocessed/sp_export.csv')

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
  
  return(rowMeans(cbind(m, u, b, l, r, ul, ur, bl, br), na.rm=T))
  
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

cl <- makeCluster(7, outfile = '')
registerDoParallel(cl)

df <- foreach(n=1:nrow(rll), .combine=bind_rows, .packages=c('raster', 'lubridate', 'gdalUtils', 'SPEI', 'dplyr', 'zoo')) %dopar% {
  
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
  
  interview <- data.frame(tmpcode=rll$layer[n],
                          interview_month=month(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          interview_year=year(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          spei12=as.numeric(spei(s, 12, na.rm=TRUE)$fitted),
                          spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
                          spei36=as.numeric(spei(s, 36, na.rm=TRUE)$fitted),
                          spi12=as.numeric(spi(precip, 12, na.rm=TRUE)$fitted),
                          spi24=as.numeric(spi(precip, 24, na.rm=TRUE)$fitted),
                          spi36=as.numeric(spi(precip, 36, na.rm=TRUE)$fitted),
                          precip_10yr_mean=rollapply(precip, width=12*10, FUN=mean, partial=TRUE, align='right'),
                          tmin_10yr_mean=rollapply(tmax, width=12*10, FUN=mean, partial=TRUE, align='right'),
                          tmax_10yr_mean=rollapply(tmin, width=12*10, FUN=mean, partial=TRUE, align='right'),
                          spei12gs=as.numeric(spei(s_season, 12, na.rm=TRUE)$fitted),
                          spei24gs=as.numeric(spei(s_season, 24, na.rm=TRUE)$fitted),
                          spei36gs=as.numeric(spei(s_season, 36, na.rm=TRUE)$fitted),
                          spi12gs=as.numeric(spi(precip_season, 12, na.rm=TRUE)$fitted),
                          spi24gs=as.numeric(spi(precip_season, 24, na.rm=TRUE)$fitted),
                          spi36gs=as.numeric(spi(precip_season, 36, na.rm=TRUE)$fitted))
  
  # birthdate <- data.frame(tmpcode=rll$layer[n],
  #                         calc_birthmonth=month(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
  #                         calc_birthyear=year(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
  #                         birthday_9monthtotal=rollapply(precip, width=9, FUN=sum, partial=TRUE, align='right'),
  #                         birthday_spei9=as.numeric(spei(s, 9, na.rm=TRUE)$fitted),
  #                         birthday_spi9=as.numeric(spi(precip, 9, na.rm=TRUE)$fitted))
  # 
  # thousanddays <- data.frame(tmpcode=rll$layer[n],
  #                            thousandday_month=month(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
  #                            thousandday_year=year(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
  #                            thousandday_spei33=as.numeric(spei(s, 33, na.rm=TRUE)$fitted),
  #                            thousandday_spi33=as.numeric(spi(precip, 33, na.rm=TRUE)$fitted),
  #                            thousandday_spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
  #                            thousandday_spi24=as.numeric(spi(precip, 24, na.rm=TRUE)$fitted))
  
  meanannual <- data.frame(tmpcode=rll$layer[n],
                           mean_annual_precip=mean(precip, na.rm=T)*12)
  
  sel <- sp[sp$tmpcode == rll$layer[n], ]
  sel <- Reduce(function(x, y){merge(x,y,all.x=T,all.y=F)}, list(sel, interview, meanannual))
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  sel
}

df <- df %>%
  select(-tmpcode)

write.csv(df, '~/dhsprocessed/PrecipIndices.csv', row.names=F)





