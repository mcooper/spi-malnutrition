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

all$dry <- all$spei=='Dry'
all$wet <- all$spei=='Wet'
all$normal <- all$spei=='Normal'

sum <- all %>% group_by(farm_system_id, farm_system) %>%
  summarize(dry=sum(dry),
            wet=sum(wet),
            normal=sum(normal),
            trees=sd(nat_trees),
            grass=sd(nat_grass),
            water=sd(nat_water))

sum$keep <- sum$dry > 1000 & sum$normal > 1000

write.csv(sum, 'G://My Drive/DHS Processed/FAO_sel.csv', row.names = F)
