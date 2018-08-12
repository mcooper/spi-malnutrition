library(dplyr)
library(lme4)
library(broom)
library(car)

setwd('~/dhsprocessed')
setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov)) %>%
  na.omit

#Try rescaling some geospatial covariates
all$precip_10yr_mean <- (all$precip_10yr_mean*12)/1000
all$gdp <- all$gdp/1000
all$market_dist <- all$market_dist/(24*7)
all$ndvi <- all$ndvi/5000
all$population <- all$population/1000
all$tmax_10yr_mean <- all$tmax_10yr_mean - 273.15
all$tmin_10yr_mean <- all$tmin_10yr_mean - 273.15
all$builtup <- all$builtup*100
all$elevation <- all$elevation/1000

#Inverse Hyperbolic Sine Transformation
#http://worthwhile.typepad.com/worthwhile_canadian_initi/2011/07/a-rant-on-inverse-hyperbolic-sine-transformations.html
#
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}


#'Collinearity with:
#' NDVI and Bare -0.9
#' population and builtup 0.77
#' tmax and tmin 0.82
#' Forest and NDVI - 0.697
#' AgGDP and GDP - 0.63
#' NDVI and precip - 0.52
#' NDVI and Forest - 0.58
#' Forest and Bare - 0.51

all$forest_ihs <- ihs(all$forest)
all$gdp_l <- log(all$gdp)
all$gdp_ihs <- ihs(all$gdp)
#all$market_dist <- ihs(all$market_dist)
all$population_ihs <- ihs(all$population)
all$fieldsize_ihs <- ihs(all$fieldsize)
all$builtup_ihs <- ihs(all$builtup)
all$elevation_ihs <- ihs(all$elevation)

#Make categorical
all$spei24 <- ifelse(all$spei24 > 1.5, "Wet",
                     ifelse(all$spei24 < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

all$countrymonth <- paste0(all$country, all$calc_birthmonth)

mod <- lm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
               birth_order + hhsize + sex + mother_years_ed + toilet +
               head_age + head_sex + wealth_index +
               #spei24*ag_pct_gdp +
               spei24*builtup +
               spei24*crop_prod +
               spei24*elevation +
               #spei24*fieldsize +
               #spei24*forest +
               spei24*gdp +
               spei24*government_effectiveness +
               spei24*irrigation +
               #spei24*market_dist +
               spei24*ndvi +
               #spei24*bare + 
               spei24*nutritiondiversity +
               spei24*population +
               spei24*precip_10yr_mean +
               spei24*stability_violence +
               spei24*tmax_10yr_mean
             , data=all)

summary(mod)

###Do Moran's I on the mod
all$residuals <- residuals(mod)


library(raster)

setwd('G://My Drive/DHS Spatial Covars/Final Rasters')

setNAs <- function(raster, column){
  raster[raster > (max(all[ , column], na.rm=T) + sd(all[ , column], na.rm=T))] <- NA
  raster[raster < (min(all[ , column], na.rm=T) - sd(all[ , column], na.rm=T))] <- NA
  return(raster)
}

ag_pct_gdp <- raster('ag_pct_gdp.tif') %>%
  setNAs('ag_pct_gdp')

builtup <- (raster('builtup.tif')*100) %>%
  setNAs('builtup')

crop_prod <- raster('crop_prod.tif') %>%
  setNAs('crop_prod')

elevation <- (raster('elevation.tif')/1000) %>%
  setNAs('elevation')

#fieldsize <- raster('fieldsize.tif')

#forest <- raster('forest.tif')

gdp <- (raster('gdp2020.tif')/1000) %>%
  setNAs('gdp')

government_effectiveness <- raster('government_effectiveness.tif') %>%
  setNAs('government_effectiveness')

irrigation <- raster('irrigation.tif') %>%
  setNAs('irrigation')

#market_dist <- raster('market_distance.tif')/(24*7)

ndvi <- (raster('ndvi.tif')/5000) %>%
  setNAs('ndvi')

nutritiondiversity <- raster('nutritiondiversity.tif') %>%
  setNAs('nutritiondiversity')

population <- (raster('population.tif')/1000) %>%
  setNAs('population')

precip_10yr_mean <- (raster('CHIRPS_10yr_avg.tif')*12/1000) %>%
  setNAs('precip_10yr_mean')

stability_violence <- raster('stability_violence.tif') %>%
  setNAs('stability_violence')

tmax <- (raster('TMAX_10yr_avg.tif') - 273.15) %>%
  setNAs('tmax_10yr_mean')

s <- tidy(mod)
row.names(s) <- s$term

wet <- s['spei24Wet', 'estimate'] + 
  s['spei24Wet:ag_pct_gdp', 'estimate']*ag_pct_gdp + 
  s['spei24Wet:builtup', 'estimate']*builtup + 
  s['spei24Wet:crop_prod', 'estimate']*crop_prod + 
  s['spei24Wet:elevation', 'estimate']*elevation + 
  #s['spei24Wet:fieldsize', 'estimate']*fieldsize +
  #s['spei24Wet:forest', 'estimate']*forest + 
  s['spei24Wet:gdp', 'estimate']*gdp + 
  s['spei24Wet:government_effectiveness', 'estimate']*government_effectiveness + 
  s['spei24Wet:irrigation', 'estimate']*irrigation + 
  #s['spei24Wet:market_dist', 'estimate']*market_dist + 
  s['spei24Wet:ndvi', 'estimate']*ndvi + 
  s['spei24Wet:nutritiondiversity', 'estimate']*nutritiondiversity + 
  s['spei24Wet:population', 'estimate']*population + 
  s['spei24Wet:precip_10yr_mean', 'estimate']*precip_10yr_mean + 
  s['spei24Wet:stability_violence', 'estimate']*stability_violence + 
  s['spei24Wet:tmax', 'estimate']*tmax

dry <- s['spei24Dry', 'estimate'] + 
  s['spei24Dry:ag_pct_gdp', 'estimate']*ag_pct_gdp + 
  s['spei24Dry:builtup', 'estimate']*builtup + 
  s['spei24Dry:crop_prod', 'estimate']*crop_prod + 
  s['spei24Dry:elevation', 'estimate']*elevation + 
  #s['spei24Dry:fieldsize', 'estimate']*fieldsize +
  #s['spei24Dry:forest', 'estimate']*forest + 
  s['spei24Dry:gdp', 'estimate']*gdp + 
  s['spei24Dry:government_effectiveness', 'estimate']*government_effectiveness + 
  s['spei24Dry:irrigation', 'estimate']*irrigation + 
  #s['spei24Dry:market_dist', 'estimate']*market_dist + 
  s['spei24Dry:ndvi', 'estimate']*ndvi + 
  s['spei24Dry:nutritiondiversity', 'estimate']*nutritiondiversity + 
  s['spei24Dry:population', 'estimate']*population + 
  s['spei24Dry:precip_10yr_mean', 'estimate']*precip_10yr_mean + 
  s['spei24Dry:stability_violence', 'estimate']*stability_violence + 
  s['spei24Dry:tmax', 'estimate']*tmax

drynas <- dry
drynas[dry > 0] <- NA

wetnas <- wet
wetnas[wet > 0] <- NA

s2 <- s
s2$estimate[s2$statistic > -2 & s2$statistic < 2] <- 0

wet_signif <- s2['spei24Wet', 'estimate'] + 
  s2['spei24Wet:ag_pct_gdp', 'estimate']*ag_pct_gdp + 
  s2['spei24Wet:builtup', 'estimate']*builtup + 
  s2['spei24Wet:crop_prod', 'estimate']*crop_prod + 
  s2['spei24Wet:elevation', 'estimate']*elevation + 
  s2['spei24Wet:fieldsize', 'estimate']*fieldsize +
  s2['spei24Wet:forest', 'estimate']*forest + 
  s2['spei24Wet:gdp', 'estimate']*gdp + 
  s2['spei24Wet:government_effectiveness', 'estimate']*government_effectiveness + 
  s2['spei24Wet:irrigation', 'estimate']*irrigation + 
  s2['spei24Wet:market_dist', 'estimate']*market_dist + 
  s2['spei24Wet:ndvi', 'estimate']*ndvi + 
  s2['spei24Wet:nutritiondiversity', 'estimate']*nutritiondiversity + 
  s2['spei24Wet:population', 'estimate']*population + 
  s2['spei24Wet:precip_10yr_mean', 'estimate']*precip_10yr_mean + 
  s2['spei24Wet:stability_violence', 'estimate']*stability_violence + 
  s2['spei24Wet:tmax', 'estimate']*tmax

dry_signif <- s2['spei24Dry', 'estimate'] + 
  s2['spei24Dry:ag_pct_gdp', 'estimate']*ag_pct_gdp + 
  s2['spei24Dry:builtup', 'estimate']*builtup + 
  s2['spei24Dry:crop_prod', 'estimate']*crop_prod + 
  s2['spei24Dry:elevation', 'estimate']*elevation + 
  s2['spei24Dry:fieldsize', 'estimate']*fieldsize +
  s2['spei24Dry:forest', 'estimate']*forest + 
  s2['spei24Dry:gdp', 'estimate']*gdp + 
  s2['spei24Dry:government_effectiveness', 'estimate']*government_effectiveness + 
  s2['spei24Dry:irrigation', 'estimate']*irrigation + 
  s2['spei24Dry:market_dist', 'estimate']*market_dist + 
  s2['spei24Dry:ndvi', 'estimate']*ndvi + 
  s2['spei24Dry:nutritiondiversity', 'estimate']*nutritiondiversity + 
  s2['spei24Dry:population', 'estimate']*population + 
  s2['spei24Dry:precip_10yr_mean', 'estimate']*precip_10yr_mean + 
  s2['spei24Dry:stability_violence', 'estimate']*stability_violence + 
  s2['spei24Dry:tmax', 'estimate']*tmax

dry[dry > 0] <- NA
wet[wet > 0] <- NA

plot(dry, main='Expected Change in HAZ Scores From a 24-Month SPEI of < -1.5',
     xlim=c(-100, 150), ylim=c(-40, 50), axes=F)
plot(wet, main='Expected Change in HAZ Scores From a 24-Month SPEI of > 1.5',
     xlim=c(-100, 150), ylim=c(-40, 50), axes=F)


