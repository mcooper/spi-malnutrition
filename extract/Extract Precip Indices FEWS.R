library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('/mnt')

dat <- read.csv('~/dhsprocessed/FEWS_CS_categories.csv') %>%
  unique

spdat <- dat %>%
  dplyr::select(latitude, longitude) %>%
  unique

#Read in precip data
precip_in_folder <- 'ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/tifs/'
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files(precip_in_folder, pattern='tif$')[1:432]
gdalbuildvrt(paste0(precip_in_folder, precip_files), precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmax data
tmax_in_folder <- 'temperature/'
tmax_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmax_files <- list.files(tmax_in_folder, pattern='^tmax.*tif$')
gdalbuildvrt(paste0(tmax_in_folder, tmax_files), tmax_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmin data
tmin_in_folder <- 'temperature/'
tmin_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmin_files <- list.files(tmin_in_folder, pattern='^tmin.*tif$')
gdalbuildvrt(paste0(tmin_in_folder, tmin_files), tmin_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

cl <- makeCluster(16, outfile = '')
registerDoParallel(cl)

foreach(n=1:nrow(spdat), .packages=c('raster', 'lubridate', 'gdalUtils', 'SPEI', 'dplyr', 'zoo')) %dopar% {
  
  precip <- gdallocationinfo(precip_vrt_file, spdat$longitude[n], spdat$latitude[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  tmax <- gdallocationinfo(tmax_vrt_file, spdat$longitude[n], spdat$latitude[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  tmin <- gdallocationinfo(tmin_vrt_file, spdat$longitude[n], spdat$latitude[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  PET <- hargreaves(tmin-273.15, tmax-273.15, lat=spdat$latitude[n], Pre=precip) %>%
    as.vector
  
  s <- precip - PET
  
  interview <- data.frame(latitude=spdat$latitude[n],
                          longitude=spdat$longitude[n],
                          yearmonth=format(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month'), "%Y%m"),
                          spei12=as.numeric(spei(s, 12, na.rm=TRUE)$fitted),
                          spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
                          spei36=as.numeric(spei(s, 36, na.rm=TRUE)$fitted),
                          spi12=as.numeric(spi(precip, 12, na.rm=TRUE)$fitted),
                          spi24=as.numeric(spi(precip, 24, na.rm=TRUE)$fitted),
                          spi36=as.numeric(spi(precip, 36, na.rm=TRUE)$fitted))
  
  sel <- merge(interview, dat, all.x=F, all.y=F)
  cat(n, round(n/nrow(spdat)*100, 4), 'percent done\n') 
  write.csv(sel, paste0('~/PrecipIndices/', n), row.names=F)
}

df <- data.frame()
for (f in list.files('~/PrecipIndices/')){
  df <- bind_rows(df, read.csv(paste0('~/PrecipIndices/', f)))
}

for (spi in c('spei12', 'spei24', 'spei36', 'spi12', 'spi24', 'spi36')){
  df <- df[!is.infinite(df[ , spi]), ]
}

write.csv(df, '~/dhsprocessed/FEWS_CS_SPEI.csv', row.names=F)





