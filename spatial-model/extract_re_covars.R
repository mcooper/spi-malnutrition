setwd('D://Documents and Settings/mcooper/Google Drive/Dissertation/Rasters/')

library(raster)
library(dplyr)
library(maptools)
library(foreach)

data <- read.csv('../../../GitHub/spi-malnutrition/results/random_effect.csv')

#Get GDP
gdp <- read.csv('../../../GitHub/spi-malnutrition/data/country_gdp_all.csv')

data <- merge(data, gdp)

#Make SPD
spd <- SpatialPointsDataFrame(data[ , c('LONGNUM', 'LATNUM')], data, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Distance to Oceans
#https://stackoverflow.com/questions/35555709/global-raster-of-geographic-distances
r <- raster('DistanceToCoast/coastdist.tif')

spd <- extract(r, spd, sp=TRUE)
spd@data$coastdist[is.na(spd@data$coastdist)] <- 0

#Time to travel to Major Cities
tt2000 <- raster('Time to Travel to Major Cities/acc_50k.tif')
names(tt2000) <- 'citydist'
tt2015 <- raster('Time to Travel to Major Cities/accessibility_to_cities_2015_v1.0.tif')
names(tt2015) <- 'citydist'

spd2000 <- extract(tt2000, spd[spd$year < 2008, ], sp=TRUE)
spd2015 <- extract(tt2015, spd[spd$year >= 2008, ], sp=TRUE)
spd2015 <- spd2015[spd2015$citydist > -0, ]

spd <- spRbind(spd2000, spd2015)

#AverageAnnualPrecip
p <- raster('Precipitation/AverageAnnualPrecip.tif')
names(p) <- 'precip'

spd <- extract(p, spd, sp=TRUE)

#population density
pop1990 <- raster('Population/gpw_v1_population_count_rev10_1990_2pt5_min.tif')
names(pop1990) <- 'population'
pop2000 <- raster('Population/gpw_v4_population_count_rev10_2000_2pt5_min.tif')
names(pop2000) <- 'population'
pop2005 <- raster('Population/gpw_v4_population_count_rev10_2005_2pt5_min.tif')
names(pop2005) <- 'population'
pop2010 <- raster('Population/gpw_v4_population_count_rev10_2010_2pt5_min.tif')
names(pop2010) <- 'population'
pop2015 <- raster('Population/gpw_v4_population_count_rev10_2015_2pt5_min.tif')
names(pop2015) <- 'population'

spd1990 <- extract(pop1990, spd[spd$year < 1996, ], sp=TRUE)
spd2000 <- extract(pop2000, spd[spd$year < 2003 & spd$year >= 1996,], sp=TRUE)
spd2005 <- extract(pop2005, spd[spd$year < 2008 & spd$year >= 2003,], sp=TRUE)
spd2010 <- extract(pop2010, spd[spd$year < 2013 & spd$year >= 2008,], sp=TRUE)
spd2015 <- extract(pop2015, spd[spd$year < 2018 & spd$year >= 2013,], sp=TRUE)

spd <- Reduce(spRbind, list(spd1990, spd2000, spd2005, spd2010, spd2015))

#AVHRR Stuff
ct <- 1
spdlist <- list()
for (i in unique(spd$year)){
  print(i)
  
  spdsel <- spd[spd$year == i, ] 
  
  if(i==2000){
    i <- 2001
  }

  tree <- raster(paste0('AVHRR/AVHRR_VCF_1982_2016_v1/Tree_cover_', i, '.tif'))
  names(tree) <- 'tree'
  bare <- raster(paste0('AVHRR/AVHRR_VCF_1982_2016_v1/Bareground_cover_', i, '.tif'))
  names(bare) <- 'bare'
  ndvi <- raster(paste0('AVHRR/AVHRR_NDVI_1982-2016/', i, '_vi_mn_75_100.tif'))
  names(ndvi) <- 'ndvi'
  
  spdsel <- extract(tree, spdsel, sp=TRUE)
  spdsel <- extract(bare, spdsel, sp=TRUE)
  spdsel <- extract(ndvi, spdsel, sp=TRUE)
  
  spdlist[[ct]] <- spdsel
  
  ct <- ct + 1
}

spd <- Reduce(spRbind, spdlist)

write.csv(spd@data, '../../../GitHub/spi-malnutrition/spatial-model/re_covariate.csv', row.names=F)






