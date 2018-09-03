library(dplyr)
library(raster)
library(randomForest)

load('G://My Drive/DHS Processed/640-tree-rfmod.Rdata')

covars <- stack('G://My Drive/DHS Spatial Covars/Final Rasters/SpatialCovars.grd')

covars <- dropLayer(covars, which(names(covars) == "bare"))

ref <- covars[['market_distance']]
names(ref) <- 'spei24'
ref <- setValues(ref, 0)

covars <- addLayer(covars, ref)

names(covars)[names(covars)=='gdp2020'] <- 'gdp'
names(covars)[names(covars)=='CHIRPS_10yr_avg'] <- 'precip_10yr_mean'
names(covars)[names(covars)=='TMAX_10yr_avg'] <- 'tmax_10yr_mean'
names(covars)[names(covars)=='TMIN_10yr_avg'] <- 'tmin_10yr_mean'
names(covars)[names(covars)=='market_distance'] <- 'market_dist'

spi0 <- predict(covars, rfmod)

start <- Sys.time()
#spi1
covars[['spei24']] <- setValues(covars[['spei24']], 1)
spi1 <- predict(covars, rf)
spi1dif <- spi1-spi0
Sys.time() - start

#spi1.5
covars[['spei24']] <- setValues(covars[['spei24']], 1.5)
spi1.5 <- predict(covars, rf)
spi1.5dif <- spi1.5-spi0

#spi2
covars[['spei24']] <- setValues(covars[['spei24']], 2)
spi2 <- predict(covars, rf)
spi2dif <- spi2-spi0

#spi3
covars[['spei24']] <- setValues(covars[['spei24']], 3)
spi3 <- predict(covars, rf)
spi3dif <- spi3-spi0

#spineg1
covars[['spei24']] <- setValues(covars[['spei24']], -1)
spineg1 <- predict(covars, rf)
spineg1dif <- spineg1 - spi0

#spineg1.5
covars[['spei24']] <- setValues(covars[['spei24']], -1.5)
spineg1.5 <- predict(covars, rf)
spineg1.5dif <- spineg1.5 - spi0

#spineg2
covars[['spei24']] <- setValues(covars[['spei24']], -2)
spineg2 <- predict(covars, rf)
spineg2dif <- spineg2 - spi0

#spineg3
covars[['spei24']] <- setValues(covars[['spei24']], -3)
spineg3 <- predict(covars, rf)
spineg3dif <- spineg3-spi0

testdf <- data.frame(spei=seq(-3, 3, 0.1))

samp <- extract(covars, SpatialPoints(list(0, 25)))

for (i in 51:nrow(testdf)){
  print(i)
  samp[ , 20] <- testdf$spei[i]
  testdf$predict[i] <- predict(rf, samp)
}


