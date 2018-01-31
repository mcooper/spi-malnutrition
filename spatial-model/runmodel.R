setwd('D://Documents and Settings/mcooper/Google Drive/Dissertation/Rasters/')

library(raster)

data <- read.csv('../../../GitHub/spi-malnutrition/spatial-model/re_covariate.csv')


gdp <- raster('GDP/gpd2015.tif')
gdp <- aggregate(gdp, 2)
names(gdp) <- 'gdp'
#-180, 180, -60, 85  (xmin, xmax, ymin, ymax)

coast <- raster('DistanceToCoast/coastdist.tif')
coast <- crop(coast, gdp)
coast <- disaggregate(coast, 12)
names(coast) <- 'coastdist'

tt2015 <- raster('Time to Travel to Major Cities/citydist_ag.tif')
names(tt2015) <- 'citydist'

p <- raster('Precipitation/AverageAnnualPrecip.tif')
p <- crop(p, gdp)
names(p) <- 'precip'

pop2015 <- raster('Population/gpw_v4_population_count_rev10_2015_2pt5_min.tif')
names(pop2015) <- 'population'
pop2015 <- aggregate(pop2015, 2, fun=mean)

tree <- raster(paste0('AVHRR/AVHRR_VCF_1982_2016_v1/Tree_cover_2016.tif'))
tree <- crop(tree, gdp)
tree <- resample(tree, pop2015)
names(tree) <- 'tree'

bare <- raster(paste0('AVHRR/AVHRR_VCF_1982_2016_v1/Bareground_cover_2016.tif'))
bare <- crop(bare, gdp)
bare <- resample(bare, pop2015)
names(bare) <- 'bare'

ndvi <- raster(paste0('AVHRR/AVHRR_NDVI_1982-2016/2016_vi_mn_75_100.tif'))
ndvi <- crop(ndvi, gdp)
ndvi <- resample(ndvi, pop2015)
names(ndvi) <- 'ndvi'

st <- stack(gdp, coast, tt2015, p, pop2015, tree, bare, ndvi)


library(randomForest)

rf <- randomForest(spiimpact ~ gdp + coastdist + citydist + precip + population + tree + bare + ndvi, data=na.omit(data))

mod <- lm(spiimpact ~ gdp + coastdist + citydist + precip + population + tree + bare + ndvi, data=na.omit(data))


prediction <- predict(st, model=rf)
