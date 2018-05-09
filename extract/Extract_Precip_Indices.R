library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('/home/mattcoop')

dat <- read.csv('sp_export.csv')

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('CHIRPS/Monthly/chirps-v2.0.1981.01.tif')
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
precip_in_folder <- 'CHIRPS/Monthly/'
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files(precip_in_folder, pattern='tif$')
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


rollfun <- function(nums){
  if (length(nums) != 17){
    return(NA)
  }
  return(sum(nums[1:9]))
}

cl <- makeCluster(7, outfile = '')
registerDoParallel(cl)

df <- foreach(n=1:nrow(rll), .combine=bind_rows, .packages=c('raster', 'lubridate', 'gdalUtils', 'SPEI', 'dplyr', 'zoo')) %dopar% {
  precip <- gdallocationinfo(precip_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  tmax <- gdallocationinfo(tmax_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  tmin <- gdallocationinfo(tmin_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
    
  PET <- hargreaves(tmin-273.15, tmax-273.15, lat=rll$y[n], Pre=precip) %>%
    as.vector
  
  s <- precip - PET
  
  s[s < 0] <- 0
    
  interview <- data.frame(tmpcode=rll$layer[n],
                          interview_month=month(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          interview_year=year(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          spei6=as.numeric(spei(s, 6, na.rm=TRUE)$fitted),
                          spei12=as.numeric(spei(s, 12, na.rm=TRUE)$fitted),
                          spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
                          spei36=as.numeric(spei(s, 36, na.rm=TRUE)$fitted),
                          spi6=as.numeric(spi(precip, 6, na.rm=TRUE)$fitted),
                          spi12=as.numeric(spi(precip, 12, na.rm=TRUE)$fitted),
                          spi24=as.numeric(spi(precip, 24, na.rm=TRUE)$fitted),
                          spi36=as.numeric(spi(precip, 36, na.rm=TRUE)$fitted))
  
  birthdate <- data.frame(tmpcode=rll$layer[n],
                          calc_birthmonth=month(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          calc_birthyear=year(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          birthday_9monthtotal=rollapply(precip, width=17, FUN=rollfun, partial=TRUE),
                          birthday_spei9=as.numeric(spei(s, 9, na.rm=TRUE)$fitted),
                          birthday_spi9=as.numeric(spi(precip, 9, na.rm=TRUE)$fitted))
  
  thousanddays <- data.frame(tmpcode=rll$layer[n],
                             thousandday_month=month(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                             thousandday_year=year(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                             thousandday_spei33=as.numeric(spei(s, 33, na.rm=TRUE)$fitted),
                             thousandday_spi33=as.numeric(spi(precip, 33, na.rm=TRUE)$fitted),
                             thousandday_spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
                             thousandday_spi24=as.numeric(spi(precip, 24, na.rm=TRUE)$fitted))
  
  meanannual <- data.frame(tmpcode=rll$layer[n],
                           mean_annual_precip=mean(precip, na.rm=T)*12)
  
  sel <- sp[sp$tmpcode == rll$layer[n], ]
  sel <- Reduce(function(x, y){merge(x,y,all.x=T,all.y=F)}, list(sel, interview, birthdate, thousanddays, meanannual))
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  sel
}

df <- df %>%
  select(-tmpcode)

write.csv(df, 'Coords&Precip.csv', row.names=F)





