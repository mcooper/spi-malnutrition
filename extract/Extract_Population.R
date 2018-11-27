#Run in the cloud

library(raster)
library(rgdal)
library(dplyr)

pop1990 <- raster('gpw_v1_population_count_rev10_1990_2pt5_min.tif')
pop2000 <- raster('gpw_v4_population_count_rev10_2000_2pt5_min.tif')
pop2005 <- raster('gpw_v4_population_count_rev10_2005_2pt5_min.tif')
pop2010 <- raster('gpw_v4_population_count_rev10_2010_2pt5_min.tif')
pop2015 <- raster('gpw_v4_population_count_rev10_2015_2pt5_min.tif')
pop2020 <- raster('gpw_v4_population_count_rev10_2020_2pt5_min.tif')

pop1990[is.na(pop1990)] <- 0
pop2000[is.na(pop2000)] <- 0
pop2005[is.na(pop2005)] <- 0
pop2010[is.na(pop2010)] <- 0
pop2015[is.na(pop2015)] <- 0
pop2020[is.na(pop2020)] <- 0

ref <- raster('chirps-v2.0.1981.01.tif')

pop1990 <- resample(pop1990, ref)
pop2000 <- resample(pop2000, ref)
pop2005 <- resample(pop2005, ref)
pop2010 <- resample(pop2010, ref)
pop2015 <- resample(pop2015, ref)
pop2020 <- resample(pop2020, ref)

data <- read.csv('sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  arrange(interview_year) %>%
  unique

pop1990 <- focal(pop1990, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
pop2000 <- focal(pop2000, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
pop2005 <- focal(pop2005, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
pop2010 <- focal(pop2010, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
pop2015 <- focal(pop2015, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
pop2020 <- focal(pop2020, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)

dat1990 <- data %>%
  filter(interview_year < 1996)
dat2000 <- data %>%
  filter(interview_year < 2003 & interview_year >= 1996)
dat2005 <- data %>%
  filter(interview_year < 2008 & interview_year >= 2003)
dat2010 <- data %>%
  filter(interview_year < 2013 & interview_year >= 2008)
dat2015 <- data %>%
  filter(interview_year >= 2013)

sp1990 <- SpatialPoints(dat1990[ , c('longitude', 'latitude')])
sp2000 <- SpatialPoints(dat2000[ , c('longitude', 'latitude')])
sp2005 <- SpatialPoints(dat2005[ , c('longitude', 'latitude')])
sp2010 <- SpatialPoints(dat2010[ , c('longitude', 'latitude')])
sp2015 <- SpatialPoints(dat2015[ , c('longitude', 'latitude')])

dat1990$population <- extract(pop1990, sp1990)
dat2000$population <- extract(pop2000, sp2000)
dat2005$population <- extract(pop2005, sp2005)
dat2010$population <- extract(pop2010, sp2010)
dat2015$population <- extract(pop2015, sp2015)

dat <- Reduce(bind_rows, list(dat1990, dat2000, dat2005, dat2010, dat2015))

write.csv(dat, 'population.csv', row.names=F)

writeRaster(pop1990, 'population1990.tif', format="GTiff", overwrite=TRUE)
writeRaster(pop2000, 'population2000.tif', format="GTiff", overwrite=TRUE)
writeRaster(pop2005, 'population2005.tif', format="GTiff", overwrite=TRUE)
writeRaster(pop2010, 'population2010.tif', format="GTiff", overwrite=TRUE)
writeRaster(pop2015, 'population2015.tif', format="GTiff", overwrite=TRUE)
writeRaster(pop2020, 'population2020.tif', format="GTiff", overwrite=TRUE)




