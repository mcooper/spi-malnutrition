library(ggplot2)
library(dplyr)
library(lme4)
library(broom)

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

#Make categorical
all$spei24 <- ifelse(all$spei24 > 1.5, "Wet",
                     ifelse(all$spei24 < -1.5, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

mod <- lmer(haz_dhs ~ interview_year + (1|country/interview_month) + age + 
              birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + wealth_index + urban_rural + 
              spei24*market_dist_ihs + 
              spei24*forest_ihs + 
              spei24*ag_pct_gdp + spei24*precip_10yr_mean + spei24*gdp_l + 
              spei24*government_effectiveness + spei24*irrigation + 
              spei24*ndvi_resc + spei24*population_ihs + spei24*stability_violence + 
              spei24*tmax_10yr_mean + 
              spei24*fieldsize_ihs + spei24*nutritiondiversity + 
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

market_dist <- raster('market_distance.tif') %>% 
  ihs

forest <- raster('forest.tif') %>% 
  ihs

ag_pct_gdp <- raster('ag_pct_gdp.tif')

precip_10yr_mean <- raster('CHIRPS_10yr_avg.tif')

gdp <- raster('gdp2020.tif') %>% 
  log %>%
  setNAs('gdp_l')

government_effectiveness <- raster('government_effectiveness.tif')

irrigation <- raster('irrigation.tif')

ndvi <- (raster('ndvi.tif')/5000)

pop <- raster('population.tif') %>% 
  ihs

stability_violence <- raster('stability_violence.tif') %>%
  ihs

tmax <- raster('TMAX_10yr_avg.tif')

fieldsize <- raster('fieldsize.tif') %>% 
  ihs

nutritiondiversity <- raster('nutritiondiversity.tif')

builtup <- raster('builtup.tif') %>% 
  ihs

s <- tidy(mod)
row.names(s) <- s$term

wet <- s['spei24Wet', 'estimate'] + 
  s['spei24Wet:market_dist_ihs', 'estimate']*market_dist + 
  s['spei24Wet:forest', 'estimate']*forest + 
  s['spei24Wet:ag_pct_gdp', 'estimate']*ag_pct_gdp + 
  s['spei24Wet:precip_10yr_mean', 'estimate']*precip_10yr_mean + 
  s['spei24Wet:gdp', 'estimate']*gdp + 
  s['spei24Wet:government_effectiveness', 'estimate']*government_effectiveness + 
  s['spei24Wet:irrigation', 'estimate']*irrigation + 
  s['spei24Wet:ndvi', 'estimate']*ndvi + 
  s['spei24Wet:stability_violence', 'estimate']*stability_violence + 
  s['spei24Wet:tmax', 'estimate']*tmax +
  s['spei24Wet:fieldsize', 'estimate']*fieldsize +
  s['spei24Wet:nutritiondiversity', 'estimate']*nutritiondiversity

dry <- s['spei24Dry', 'estimate'] + 
  s['spei24Dry:market_dist_ihs', 'estimate']*market_dist + 
  s['spei24Dry:forest', 'estimate']*forest + 
  s['spei24Dry:ag_pct_gdp', 'estimate']*ag_pct_gdp + 
  s['spei24Dry:precip_10yr_mean', 'estimate']*precip_10yr_mean + 
  s['spei24Dry:gdp', 'estimate']*gdp + 
  s['spei24Dry:government_effectiveness', 'estimate']*government_effectiveness + 
  s['spei24Dry:irrigation', 'estimate']*irrigation + 
  s['spei24Dry:ndvi', 'estimate']*ndvi + 
  s['spei24Dry:stability_violence', 'estimate']*stability_violence + 
  s['spei24Dry:tmax', 'estimate']*tmax +
  s['spei24Dry:fieldsize', 'estimate']*fieldsize +
  s['spei24Dry:nutritiondiversity', 'estimate']*nutritiondiversity

s2 <- s
s2$estimate[s2$statistic > -2 & s2$statistic < 2] <- 0

wet_signif <- s['spei24Wet', 'estimate'] + 
  s['spei24Wet:market_dist_ihs', 'estimate']*market_dist + 
  s['spei24Wet:forest', 'estimate']*forest + 
  s['spei24Wet:ag_pct_gdp', 'estimate']*ag_pct_gdp + 
  s['spei24Wet:precip_10yr_mean', 'estimate']*precip_10yr_mean + 
  s['spei24Wet:gdp', 'estimate']*gdp + 
  s['spei24Wet:government_effectiveness', 'estimate']*government_effectiveness + 
  s['spei24Wet:irrigation', 'estimate']*irrigation + 
  s['spei24Wet:ndvi', 'estimate']*ndvi + 
  s['spei24Wet:stability_violence', 'estimate']*stability_violence + 
  s['spei24Wet:tmax', 'estimate']*tmax +
  s['spei24Wet:fieldsize', 'estimate']*fieldsize +
  s['spei24Wet:nutritiondiversity', 'estimate']*nutritiondiversity

dry_signif <- s['spei24Dry', 'estimate'] + 
  s['spei24Dry:market_dist_ihs', 'estimate']*market_dist + 
  s['spei24Dry:forest', 'estimate']*forest + 
  s['spei24Dry:ag_pct_gdp', 'estimate']*ag_pct_gdp + 
  s['spei24Dry:precip_10yr_mean', 'estimate']*precip_10yr_mean + 
  s['spei24Dry:gdp', 'estimate']*gdp + 
  s['spei24Dry:government_effectiveness', 'estimate']*government_effectiveness + 
  s['spei24Dry:irrigation', 'estimate']*irrigation + 
  s['spei24Dry:ndvi', 'estimate']*ndvi + 
  s['spei24Dry:stability_violence', 'estimate']*stability_violence + 
  s['spei24Dry:tmax', 'estimate']*tmax +
  s['spei24Dry:fieldsize', 'estimate']*fieldsize +
  s['spei24Dry:nutritiondiversity', 'estimate']*nutritiondiversity






