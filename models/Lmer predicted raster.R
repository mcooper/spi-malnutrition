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
all$imports_percap <- all$imports_percap/1000

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

library(raster)

setwd('G://My Drive/DHS Spatial Covars/Final Rasters')

setNAs <- function(raster, column){
  raster[raster > (max(all[ , column], na.rm=T) + sd(all[ , column], na.rm=T)/2)] <- NA
  raster[raster < (min(all[ , column], na.rm=T) - sd(all[ , column], na.rm=T)/2)] <- NA
  return(raster)
}

ag_pct_gdp <- raster('ag_pct_gdp.tif') #%>% setNAs('ag_pct_gdp')

builtup <- (raster('builtup.tif')*100) #%>% setNAs('builtup')

crop_prod <- raster('crop_prod.tif') #%>% setNAs('crop_prod')

elevation <- (raster('elevation.tif')/1000) #%>% setNAs('elevation')

fieldsize <- raster('fieldsize.tif')

forest <- raster('forest.tif')

gdp <- (raster('gdp2020.tif')/1000) %>%
  setNAs('gdp')

gdp_l <- log(gdp)

government_effectiveness <- raster('government_effectiveness.tif') #%>% setNAs('government_effectiveness')

high_settle <- raster('high_settle.tif') #%>% setNAs('high_settle')

imports_percap <- raster('imports_percap.tif')

irrigation <- raster('irrigation.tif') #%>% setNAs('irrigation')

low_settle <- raster('low_settle.tif') #%>% setNAs('low_settle')

market_dist <- raster('market_distance.tif')/(24*7)

ndvi <- (raster('ndvi.tif')/5000) #%>% setNAs('ndvi')

nutritiondiversity <- raster('nutritiondiversity.tif') #%>% setNAs('nutritiondiversity')

population <- (raster('population.tif')/1000) #%>% setNAs('population')

precip_10yr_mean <- (raster('CHIRPS_10yr_avg.tif')*12/1000) #%>% setNAs('precip_10yr_mean')

roughness <- raster('roughness.tif') #%>% setNAs('roughness')

stability_violence <- raster('stability_violence.tif') #%>% setNAs('stability_violence')

tmax_10yr_mean <- (raster('TMAX_10yr_avg.tif') - 273.15) #%>% setNAs('tmax_10yr_mean')

makeRasts <- function(mod){
  s <- tidy(mod)
  
  #Make wet raster
  wet_coefs <- s[grepl('speiWet', s$term), c('term', 'estimate')]
  dry_coefs <- s[grepl('speiDry', s$term), c('term', 'estimate')]
  
  wet_coefs$term <- gsub('speiWet:', '', wet_coefs$term)
  dry_coefs$term <- gsub('speiDry:', '', dry_coefs$term)
  
  wet <- wet_coefs$estimate[1]
  for (i in 2:nrow(wet_coefs)){
    rast <- get(wet_coefs$term[i])
    wet <- wet + rast*wet_coefs$estimate[i]
  }
  
  dry <- dry_coefs$estimate[1]
  for (i in 2:nrow(dry_coefs)){
    rast <- get(dry_coefs$term[i])
    dry <- dry + rast*dry_coefs$estimate[i]
  }
  
  wet_censored <- wet
  wet_censored[wet > 0] <- NA
  
  dry_censored <- dry
  dry_censored[dry > 0] <- NA
  
  return(list(wet, wet_censored, dry, dry_censored))
}

#Make categorical
all$spei <- all$spei36gs

all$spei <- ifelse(all$spei > 1.5, "Wet",
                   ifelse(all$spei < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")


mod <- lm(haz_dhs ~ interview_year + age + urban_rural + 
            birth_order + hhsize + sex + mother_years_ed + toilet +
            head_age + head_sex + wealth_index + spei + 
               #spei*ag_pct_gdp +
               spei*builtup +
               spei*crop_prod +
               spei*elevation +
               spei*forest +
               spei*gdp_l +
               spei*government_effectiveness +
               #spei*high_settle + 
               #spei*imports_percap + 
               spei*irrigation +
               #spei*low_settle + 
               spei*market_dist +
               #spei*ndvi +
               #spei*bare + 
               spei*nutritiondiversity +
               spei*population +
               spei*precip_10yr_mean +
               spei*roughness + 
               spei*stability_violence +
               spei*tmax_10yr_mean
             , data=all)

summary(mod)

rasts <- makeRasts(mod)

#wet_censored
plot(rasts[[2]])

#dry_censored
plot(rasts[[4]])

#dry
plot(rasts[[3]])

#wet
plot(rasts[[1]])


writeRaster(rasts[[1]], 'Wet.tif', format='GTiff', overwrite=T)
writeRaster(rasts[[3]], 'Dry.tif', format='GTiff', overwrite=T)


plot(dry, main='Expected Change in HAZ Scores From a 24-Month SPEI of < -1.5',
     xlim=c(-100, 150), ylim=c(-40, 50), axes=F)
plot(wet, main='Expected Change in HAZ Scores From a 24-Month SPEI of > 1.5',
     xlim=c(-100, 150), ylim=c(-40, 50), axes=F)


