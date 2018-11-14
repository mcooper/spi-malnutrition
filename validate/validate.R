library(dplyr)
library(lme4)
library(MASS)
library(broom)

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
all$spei <- all$spei24

all$spei <- ifelse(all$spei > 1.5, "Wet",
                   ifelse(all$spei < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

training <- all %>% filter(country!="ML")
testing <- all %>% filter(country=="ML")

mod_geo <- rlm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
             birth_order + hhsize + sex + mother_years_ed + toilet +
             head_age + head_sex + wealth_index + 
             spei*grid_gdp + 
             spei*population +
             spei*ndvi +
             spei*tmax_10yr_mean +
             spei*market_dist + 
             spei*crop_prod +
             spei*government_effectiveness +
             spei*irrig_aai +
             spei*nutritiondiversity +
             spei*stability_violence,
           data=all %>% filter(country != "ML"))

mod_hh <- rlm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
                 birth_order + hhsize + sex + mother_years_ed + toilet +
                 head_age + head_sex + wealth_index,
               data=all %>% filter(country != "ML"))

testing$predicted_hh <- predict(mod_hh, testing)
testing$residuals_hh <- testing$predicted_hh - testing$haz_dhs

testing$predicted_geo <- predict(mod_geo, testing)
testing$residuals_geo <- testing$predicted_geo - testing$haz_dhs

sqrt(mean(testing$residuals_hh^2))
sqrt(mean(testing$residuals_geo^2))
