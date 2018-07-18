library(ggplot2)
library(dplyr)
library(lme4)
library(broom)

setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
lc <- read.csv('landcover_processed.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, lc, spei, cov))

all$lbuiltup <- log(all$builtup)

mod <- lmer(haz_dhs ~ interview_year + (1|country/interview_month) + age + birth_order + hhsize + sex + mother_years_ed + toilet +
            head_age + head_sex + wealth_index + urban_rural + spei36*market_dist + spei36*forest + spei36*bare + spei36*ag_pct_gdp + 
              spei36*precip_10yr_mean + spei36*gdp + spei36*government_effectiveness + spei36*irrigation + 
              spei36*ndvi + spei36*population + spei36*stability_violence + spei36*tmax_10yr_mean + spei36*tmin_10yr_mean + 
              spei36*fieldsize + spei36*nutritiondiversity + (1|surveycode) + (1|country), data=all)

library(raster)

setwd('G://My Drive/DHS Spatial Covars/Final Rasters')

market_dist <- raster('market_distance.tif')
forest <- raster('forest.tif')
bare <- raster('bare.tif')
ag_pct_gdp <- raster('ag_pct_gdp.tif')
precip_10yr_mean <- raster('CHIRPS_10yr_avg.tif')
gdp <- raster('gdp2020.tif')
govt_eff <- raster('government_effectiveness.tif')
irrigation <- raster('irrigation.tif')
ndvi <- raster('ndvi.tif')
pop <- raster('population.tif')
sv <- raster('stability_violence.tif')
tmax <- raster('TMAX_10yr_avg.tif')
tmin <- raster('TMIN_10yr_avg.tif')
field_size <- raster('fieldsize.tif')
nd <- raster('nutritiondiversity.tif')

f <- -1.601e-05*market_dist + -4.119e-05*forest + 4.050e-04*bare -4.354e-04*ag_pct_gdp + 5.979e-05*precip_10yr_mean + 1.130e-05*gdp + 2.208e-04*govt_eff + 
  1.556e-04*irrigation + 2.909e-06*ndvi + 4.837e-07*pop + 1.285e-03*sv + 1.096e-02*tmax -6.328e-03*tmin + 1.339e-03*field_size -9.379e-02*nd









