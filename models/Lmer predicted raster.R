library(ggplot2)
library(dplyr)
library(lme4)
library(broom)
library(MASS)

setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov)) %>%
  na.omit

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


all$bare_ihs <- ihs(all$bare)
all$forest_ihs <- ihs(all$forest)
all$gdp_l <- log(all$gdp)
all$market_dist_ihs <- ihs(all$market_dist)
all$ndvi_resc <- all$ndvi/5000
all$population_ihs <- ihs(all$population)
all$fieldsize_ihs <- ihs(all$fieldsize)
all$builtup_ihs <- ihs(all$builtup)
all$elevation_ihs <- ihs(all$elevation)

#Make categorical
all$spei24 <- ifelse(all$spei24 > 1.5, "Wet",
                     ifelse(all$spei24 < -1.5, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

mod <- lmer(haz_dhs ~ interview_year + (1|country/interview_month) + age + 
              birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + wealth_index + urban_rural + 
              spei24*ag_pct_gdp +
              spei24*builtup_ihs +
              spei24*crop_prod +
              spei24*elevation_ihs +
              spei24*fieldsize_ihs +
              spei24*forest_ihs +
              spei24*gdp_l +
              spei24*government_effectiveness +
              spei24*irrigation +
              spei24*market_dist_ihs +
              spei24*ndvi_resc +
              spei24*nutritiondiversity +
              spei24*population_ihs +
              spei24*precip_10yr_mean +
              spei24*stability_violence +
              spei24*tmax_10yr_mean +
              (1|surveycode) + (1|country), data=all)

###Do Moran's I on the mod
all$residuals <- residuals(mod)


library(raster)

setwd('G://My Drive/DHS Spatial Covars/Final Rasters')

setNAs <- function(raster, column){
  raster[raster > max(all[ , column], na.rm=T)] <- NA
  raster[raster < min(all[ , column], na.rm=T)] <- NA
  return(raster)
}

ag_pct_gdp <- raster('ag_pct_gdp.tif')

builtup <- raster('builtup.tif') %>% 
  ihs

crop_prod <- raster('crop_prod.tif')

elevation <- raster('elevation.tif') %>%
  ihs

fieldsize <- raster('fieldsize.tif') %>% 
  ihs

forest <- raster('forest.tif') %>% 
  ihs

gdp <- raster('gdp2020.tif') %>% 
  log %>%
  setNAs('gdp_l')

government_effectiveness <- raster('government_effectiveness.tif')

irrigation <- raster('irrigation.tif')

market_dist <- raster('market_distance.tif') %>% 
  ihs

ndvi <- (raster('ndvi.tif')/5000)

nutritiondiversity <- raster('nutritiondiversity.tif')

population <- raster('population.tif') %>% 
  ihs

precip_10yr_mean <- raster('CHIRPS_10yr_avg.tif')

stability_violence <- raster('stability_violence.tif') %>%
  ihs

tmax <- raster('TMAX_10yr_avg.tif')

s <- tidy(mod)
row.names(s) <- s$term

wet <- s['spei24Wet', 'estimate'] + 
  s['spei24Wet:ag_pct_gdp', 'estimate']*ag_pct_gdp + 
  s['spei24Wet:builtup', 'estimate']*builtup + 
  s['spei24Wet:crop_prod', 'estimate']*crop_prod + 
  s['spei24Wet:elevation', 'estimate']*elevation + 
  s['spei24Wet:fieldsize', 'estimate']*fieldsize +
  s['spei24Wet:forest', 'estimate']*forest + 
  s['spei24Wet:gdp', 'estimate']*gdp + 
  s['spei24Wet:government_effectiveness', 'estimate']*government_effectiveness + 
  s['spei24Wet:irrigation', 'estimate']*irrigation + 
  s['spei24Wet:market_dist', 'estimate']*market_dist + 
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
  s['spei24Dry:fieldsize', 'estimate']*fieldsize +
  s['spei24Dry:forest', 'estimate']*forest + 
  s['spei24Dry:gdp', 'estimate']*gdp + 
  s['spei24Dry:government_effectiveness', 'estimate']*government_effectiveness + 
  s['spei24Dry:irrigation', 'estimate']*irrigation + 
  s['spei24Dry:market_dist', 'estimate']*market_dist + 
  s['spei24Dry:ndvi', 'estimate']*ndvi + 
  s['spei24Dry:nutritiondiversity', 'estimate']*nutritiondiversity + 
  s['spei24Dry:population', 'estimate']*population + 
  s['spei24Dry:precip_10yr_mean', 'estimate']*precip_10yr_mean + 
  s['spei24Dry:stability_violence', 'estimate']*stability_violence + 
  s['spei24Dry:tmax', 'estimate']*tmax

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


