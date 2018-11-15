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
all$precip_10yr_mean <- (all$precip_10yr_mean*12)/1000
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
all$spei <- all$spei12

all$spei <- ifelse(all$spei > 1, "Wet",
                   ifelse(all$spei < -1, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

spcovars <- c("ag_pct_gdp", "bare", "forest", "gdp", 
"government_effectiveness", "irrig_aai", "irrig_aei", "market_dist", 
"ndvi", "population", "stability_violence", "crop_prod", "fieldsize", 
"nutritiondiversity", "builtup", "elevation", "high_settle", 
"low_settle", "roughness", "imports_percap", "grid_gdp", "grid_hdi", 
"enrollment", "precip_10yr_mean", "tmax_10yr_mean", "tmin_10yr_mean")

df <- data.frame()
for (i in spcovars){
  all$var <- all[ , i]
  
  mod <- lm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
               birth_order + hhsize + sex + mother_years_ed + toilet +
               head_age + head_sex + wealth_index + spei*var,
             data=all)
  
  coefs <- tidy(mod) %>%
    filter(term %in% c('var', 'speiDry:var', 'speiWet:var'))
  
  new <- data.frame(Normal=coefs$estimate[coefs$term=='var'],
             Wet=coefs$estimate[coefs$term=='speiWet:var'],
             Dry=coefs$estimate[coefs$term=='speiDry:var'],
             Variable=i)
  
  df <- bind_rows(df, new)
  print(i)
}

write.csv(df, 'C://Users/matt/Desktop/temp.csv', row.names=F)
