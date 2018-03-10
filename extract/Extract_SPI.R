library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)

setwd('/home/mw_coop_r')

dat <- read.csv('sp_export.csv')

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
rll <- rll[rll$layer %in% sp@data$tmpcode, ]

in_folder <- '/home/mw_coop_r/CHIRPS/'
tifs <- dir(in_folder, pattern='.tif$')
vrt_file <- extension(rasterTmpFile(), 'ivrt')

files <- list.files('CHIRPS', pattern='tif$')

gdalbuildvrt(paste0(in_folder, files), vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

rollfun <- function(nums){
  if (length(nums) != 17){
    return(NA)
  }
  return(sum(nums[1:9]))
}

df <- data.frame()
for (n in 1:nrow(rll)){
  s <- gdallocationinfo(vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE)
  s <- as.numeric(s)
  interview <- data.frame(tmpcode=rll$layer[n],
                          interview_month=month(seq(ymd('1981-01-01'), ymd('2017-09-01'), by='1 month')),
                          interview_year=year(seq(ymd('1981-01-01'), ymd('2017-09-01'), by='1 month')),
                          spi6=as.numeric(spi(s, 6, na.rm=TRUE)$fitted),
                          spi12=as.numeric(spi(s, 12, na.rm=TRUE)$fitted),
                          spi24=as.numeric(spi(s, 24, na.rm=TRUE)$fitted),
                          spi36=as.numeric(spi(s, 36, na.rm=TRUE)$fitted))
  birthdate <- data.frame(tmpcode=rll$layer[n],
                          calc_birthmonth=month(seq(ymd('1981-01-01'), ymd('2017-09-01'), by='1 month')),
                          calc_birthyear=year(seq(ymd('1981-01-01'), ymd('2017-09-01'), by='1 month')),
                          birthday_9monthtotal=rollapply(s, width=17, FUN=rollfun, partial=TRUE),
                          birthday_spi9=as.numeric(spi(s, 9, na.rm=TRUE)$fitted))
  thousanddays <- data.frame(tmpcode=rll$layer[n],
                             thousandday_month=month(seq(ymd('1981-01-01'), ymd('2017-09-01'), by='1 month')),
                             thousandday_year=year(seq(ymd('1981-01-01'), ymd('2017-09-01'), by='1 month')),
                             thousandday_spi33=as.numeric(spi(s, 33, na.rm=TRUE)$fitted))
  meanannual <- data.frame(tmpcode=rll$layer[n],
                           mean_annual_precip=mean(s[1:240], na.rm=T)*12)
  sel <- sp[sp$tmpcode == rll$layer[n], ]
  sel <- Reduce(function(x, y){merge(x,y,all.x=T,all.y=F)}, list(sel, interview, birthdate, thousanddays, meanannual))
  df <- bind_rows(sel, df)
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
}

df <- df %>%
  select(-tmpcode)

write.csv(df, 'Coords&SPI.csv', row.names=F)





