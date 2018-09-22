library(dplyr)
library(raster)
library(randomForest)

setwd('~/dhsprocessed')

cropread <- function(r){
  r <- raster(r)
  r <- crop(r, extent(-180, 180, -50, 50))
  r
}

ag_pct_gdp <- cropread('ag_pct_gdp.tif')
builtup <- cropread('builtup.tif')
crop_prod <- cropread('crop_prod.tif')
elevation <- cropread('elevation.tif')
fieldsize <- cropread('fieldsize.tif')
forest <- cropread('forest.tif')
grid_gdp <- cropread('grid_gdp.tif')
grid_hdi <- cropread('grid_hdi.tif')
government_effectiveness <- cropread('government_effectiveness.tif') 
imports_percap <- cropread('imports_percap.tif')
irrig_aei <- cropread('irrig_aei.tif')
market_dist <- cropread('market_dist.tif')
ndvi <- cropread('ndvi.tif')
nutritiondiversity <- cropread('nutritiondiversity.tif')
population <- cropread('population.tif')
roughness <- cropread('roughness.tif')
stability_violence <- cropread('stability_violence.tif')

covars <- stack(list(ag_pct_gdp, forest, government_effectiveness, irrig_aei, 
                     market_dist, ndvi, population,
                     stability_violence, crop_prod, fieldsize, nutritiondiversity, 
                     builtup, elevation, roughness,
                     imports_percap, grid_gdp, grid_hdi))


load('~/rf-mod-dryloess')
load('~/rf-mod-wetloess')
load('~/rf-mod-normalloess')


#normal HAZ scores
normal <- predict(covars, rfnormal)

wet <- predict(covars, rfwet)

dry <- predict(covars, rfdry)

drought <- dry - normal
flood <- wet - normal
