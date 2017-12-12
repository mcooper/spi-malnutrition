library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)

setwd('/home/mw_coop_r')

dat <- read.csv('sp_export.csv')

sp <- SpatialPointsDataFrame(coords=dat[ c('LONGNUM', 'LATNUM')], data = dat)

r <- raster('CHIRPS/chirps-v2.0.1981.01.tif')
r <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))

sp@data$code <- extract(r, sp)

rll <- rasterToPoints(r) %>% data.frame
rll <- rll[rll$layer %in% sp@data$code, ]

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
  temp <- data.frame(code=rll$layer[n],
             month=month(seq(ymd('1981-01-01'), ymd('2017-09-01'), by='1 month')),
             year=year(seq(ymd('1981-01-01'), ymd('2017-09-01'), by='1 month')),
             spi6=as.numeric(spi(s, 6, na.rm=TRUE)$fitted),
             spi12=as.numeric(spi(s, 12, na.rm=TRUE)$fitted),
             spi24=as.numeric(spi(s, 24, na.rm=TRUE)$fitted),
             spi36=as.numeric(spi(s, 36, na.rm=TRUE)$fitted))
  sel <- merge(temp, sp@data, all.x=F, all.y=F)
  df <- bind_rows(sel, df)
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
}

write.csv(df, 'Coords&SPI.csv', row.names=F)

# spi_data$tmp <- spi_data$year
# spi_data$tmp[spi_data$year==2012 & spi_data$country=='Bangladesh'] <- 2011
# 
# spi_data <- spi_data[!is.na(spi_data$year), ]
# 
# spi_data$survey <- paste0(spi_data$country, spi_data$tmp)
# 
# ggplot(spi_data) + geom_histogram(aes(x=spi36, fill=survey))
# 
# ggplot(spi_data %>% filter(survey=='Zambia2015')) + 
#   geom_point(aes(x=longitude, y=latitude, color=spi6))






