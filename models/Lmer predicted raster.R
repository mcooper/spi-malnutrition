library(dplyr)
library(lme4)
library(MASS)
library(broom)
library(tidyr)

setwd('~/dhsprocessed')
setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov)) %>%
  na.omit

#Try rescaling some geospatial covariates
all$mean_annual_precip <- (all$mean_annual_precip)/1000
all$gdp <- all$gdp/1000
all$grid_gdp <- all$grid_gdp/1000
all$market_dist <- all$market_dist/(24*7)
all$population <- all$population/1000
all$tmax_10yr_mean <- all$tmax_10yr_mean - 273.15
all$tmin_10yr_mean <- all$tmin_10yr_mean - 273.15
all$builtup <- all$builtup*100
all$elevation <- all$elevation/1000
all$imports_percap <- all$imports_percap/1000
all$gdp_l <- log(all$gdp)
all$grid_gdp_l <- log(all$grid_gdp)

source('C://Git/spi-malnutrition/models/mod_utils.R')

#Make categorical
all$spei <- all$spei24

all$spei <- ifelse(all$spei > 1.5, "Wet",
                   ifelse(all$spei < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

mod <- rlm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
            birth_order + hhsize + sex + mother_years_ed + toilet +
            head_age + head_sex + wealth_index + 
            
            #GDP related vars
            #spei*ag_pct_gdp +
            #spei*gdp_l +
            #spei*grid_gdp_l + 
            #spei*grid_gdp + 
            spei*grid_hdi + 
            spei*imports_percap + 
            
            #spei*enrollment + 
             
            #Pop-Urban vars
            spei*builtup +
            #spei*low_settle + #Slightly colinear with irrigation (0.50) so avoid this one
            #spei*high_settle + 
            #spei*population +
            
            #Land Cover Vars
            spei*mean_annual_precip +
            spei*forest +
            #spei*ndvi +
            #spei*bare + 
            
            #Topographic Vars
            #spei*elevation +
            spei*roughness + 
            #spei*tmax_10yr_mean +
          
            #Totally Independat Vars
            #spei*market_dist + #not the same before and after 2000
            #spei*fieldsize + 
            spei*crop_prod +
            #spei*government_effectiveness +
            spei*irrig_aai +
            #spei*nutritiondiversity +
            spei*stability_violence
           ,
           data=all)

summary(mod)

#Baseline
#plot(makeRasts(mod, ''))

#Dry
plot(make_rasts_year(mod, "speiDry", 2017,
                     list(mean_annual_precip=function(x){x/1000},
                          imports_percap=function(x){x/1000},
                          builtup=function(x){x*100})))

#Wet
plot(makeRasts(mod, "speiWet"))

for (i in seq(1990, 2020)){
  print(i)
  rast <- make_rasts_year(mod, "speiDry", i,
                        list(mean_annual_precip=function(x){x/1000},
                             imports_percap=function(x){x/1000},
                             builtup=function(x){x*100}))
  writeRaster(rast, paste0('G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/Dry', i, '.tif'), format='GTiff', overwrite=T)
}

for (i in seq(1990, 2020)){
  print(i)
  r <- raster(paste0('G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/Dry', i, '.tif'))
  print(minValue(r))
}