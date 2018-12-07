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
fs <- read.csv('FarmingSystems.csv')
lc <- read.csv('landcover_processed.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, fs, lc)) %>%
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

#Make categorical
all$spei <- all$spei24

all$spei <- ifelse(all$spei > 1.5, "Wet",
                   ifelse(all$spei < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

mod <- lmer(haz_dhs ~ age + as.factor(calc_birthmonth) + 
            birth_order + hhsize + sex + mother_years_ed + toilet +
            head_age + head_sex + wealth_index + 
            
            #GDP related vars
            #spei*ag_pct_gdp +
            #spei*gdp_l +
            #spei*grid_gdp_l + 
            #spei*grid_gdp + 
            grid_hdi + 
            imports_percap + 
            
            #spei*enrollment + 
             
            #Pop-Urban vars
            builtup +
            #spei*low_settle + #Slightly colinear with irrigation (0.50) so avoid this one
            #spei*high_settle + 
            #spei*population +
            
            #Land Cover Vars
            mean_annual_precip +
            (nat_water|farm_system_id) +
            (nat_grass|farm_system_id) +
            (nat_trees|farm_system_id) +
            market_dist +
            #spei*ndvi +
            #spei*bare + 
            
            #Topographic Vars
            #spei*elevation +
            roughness + 
            #spei*tmax_10yr_mean +
          
            #Totally Independat Vars
            #spei*market_dist + #not the same before and after 2000
            #spei*fieldsize + 
            crop_prod +
            #spei*government_effectiveness +
            irrig_aai +
            #spei*nutritiondiversity +
            stability_violence
           ,
           data=all)

summary(mod)

library(raster)
fao <- raster('G:/My Drive/DHS Spatial Covars/Farm Systems/farm_system_id.tif')

#Forestland
trees <- raster('G:/My Drive/DHS Spatial Covars/ESA Land Cover/forest_resample.tif')
treesdf <- data.frame(coef(mod)$farm_system) %>%
  mutate(farm_system_id = as.numeric(row.names(.))) %>%
  dplyr::select(farm_system_id, nat_trees)
treesdf <- bind_rows(data.frame(farm_system_id=seq(0, 1662)[!seq(0, 1662) %in% treesdf$farm_system_id],
                                 nat_trees=NA),
                      treesdf)
fao_trees <- reclassify(fao, treesdf)
trees_pred <- trees*fao_trees
trees_pred[trees_pred < 0] <- 0

#Grassland
grass <- raster('G:/My Drive/DHS Spatial Covars/ESA Land Cover/grass_resample.tif')
grassdf <- data.frame(coef(mod)$farm_system) %>%
  mutate(farm_system_id = as.numeric(row.names(.))) %>%
  dplyr::select(farm_system_id, nat_grass)
grassdf <- bind_rows(data.frame(farm_system_id=seq(0, 1662)[!seq(0, 1662) %in% grassdf$farm_system_id],
                                 nat_grass=NA),
                      grassdf)
fao_grass <- reclassify(fao, grassdf)
grass_pred <- grass*fao_grass
grass_pred[grass_pred < 0] <- 0

#Waterland
water <- raster('G:/My Drive/DHS Spatial Covars/ESA Land Cover/water_resample.tif')
waterdf <- data.frame(coef(mod)$farm_system) %>%
  mutate(farm_system_id = as.numeric(row.names(.))) %>%
  dplyr::select(farm_system_id, nat_water)
waterdf <- bind_rows(data.frame(farm_system_id=seq(0, 1662)[!seq(0, 1662) %in% waterdf$farm_system_id],
                                nat_water=NA),
                     waterdf)
fao_water <- reclassify(fao, waterdf)
water_pred <- water*fao_water
water_pred[water_pred < 0] <- 0

setwd("G:/My Drive/Dissertation/Ecosystem Services")

writeRaster(water_pred, 'water.tif', format='GTiff')
writeRaster(grass_pred, 'grass.tif', format='GTiff')
writeRaster(forest_pred, 'forest.tif', format='GTiff')


