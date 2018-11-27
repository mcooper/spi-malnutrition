#Run in the cloud

library(raster)
library(rgdal)
library(dplyr)

tt2000 <- raster('acc_50k.tif')
tt2015 <- raster('accessibility_to_cities_2015_v1.0.tif')
tt2015[tt2015 < 0] <- NA

ref <- raster('Bareground_cover_1982.tif')

tt2000 <- resample(tt2000, ref)
tt2015 <- resample(tt2015, ref)

data <- read.csv('sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  arrange(interview_year) %>%
  unique

dat$latitude[dat$code == 'NG-5-1-773'] <- 4.4161116
dat$longitude[dat$code == 'NG-5-1-773'] <- 7.1868954

tt2000 <- focal(tt2000, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
tt2015 <- focal(tt2015, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

dat2000 <- data %>%
  filter(interview_year < 2008)

dat2015 <- data %>%
  filter(interview_year >= 2008)

sp2000 <- SpatialPoints(dat2000[ , c('longitude', 'latitude')])
sp2015 <- SpatialPoints(dat2015[ , c('longitude', 'latitude')])

dat2000$market_dist <- extract(tt2000, sp2000)
dat2015$market_dist <- extract(tt2015, sp2015)

dat <- bind_rows(dat2000, dat2015)

data <- merge(data, dat %>% dplyr::select(code, interview_year, market_dist))

write.csv(data, 'market_distance.csv', row.names=F)

writeRaster(tt2000, 'market_distance2000.tif', format="GTiff")
writeRaster(tt2015, 'market_distance2015.tif', format="GTiff")