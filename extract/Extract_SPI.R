library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)

setwd('/home/mw_coop_r')

dat <- read.csv('sp_export.csv')

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('CHIRPS/chirps-v2.0.1981.01.tif')
r <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))

sp@data$tmpcode <- extract(r, sp)

rll <- rasterToPoints(r) %>% data.frame
rll <- rll[rll$layer %in% sp@data$tmpcode, ]

in_folder <- '/home/mw_coop_r/CHIRPS/'
tifs <- dir(in_folder, pattern='.tif$')
vrt_file <- extension(rasterTmpFile(), 'ivrt')

files <- list.files('CHIRPS', pattern='tif$')

gdalbuildvrt(paste0(in_folder, files), vrt_file, separate=TRUE, verbose=T,
             overwrite=TRUE)

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
             birthday_spi9=as.numeric(spi(s, 9, na.rm=TRUE)$fitted))
  thousanddays <- data.frame(tmpcode=rll$layer[n],
             thousandday_month=month(seq(ymd('1981-01-01'), ymd('2017-09-01'), by='1 month')),
             thousandday_year=year(seq(ymd('1981-01-01'), ymd('2017-09-01'), by='1 month')),
             thousandday_spi33=as.numeric(spi(s, 33, na.rm=TRUE)$fitted))
  
  sel <- sp@data[sp@data$tmpcode == rll$layer[n], ]
  final <- Reduce(function(x, y){merge(x,y,all.x=T,all.y=F)}, list(sel, interview, birthdate, thousanddays))
  df <- bind_rows(sel, df)
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
}

df <- df %>%
  select(-tmpcode)

write.csv(df, 'Coords&SPI.csv', row.names=F)





