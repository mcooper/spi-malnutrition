library(ggplot2)
library(dplyr)
library(lme4)
library(broom)

setwd('G://My Drive/DHS Spatial Covars/Final Rasters/')

hha <- read.csv('HH_data_A.csv')
lc <- read.csv('landcover_processed.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, lc, spei, cov))

all$lbuiltup <- log(all$builtup)

mod <- lmer(haz_dhs ~ interview_year + (1|country/interview_month) + age + birth_order + hhsize + sex + mother_years_ed + toilet +
            head_age + head_sex + wealth_index + urban_rural + (1|surveycode) + (1|country), data=all)

all$residuals <- residuals(mod)

mod2 <- lm(residuals ~ spei24*market_dist + spei24*forest + spei24*bare + spei24*market_dist + spei24*ag_pct_gdp + 
             spei24*precip_10yr_mean + spei24*gdp + spei24*government_effectiveness + spei24*irrigation + 
             spei24*ndvi + spei24*population + spei24*stability_violence + spei24*tmax_10yr_mean + spei24*tmin_10yr_mean + 
             spei24*crop_prod + spei24*fieldsize + spei24*nutritiondiversity + urban_rural, data=all)

library(raster)

setwd()

s <- stack(list.files(pattern='*.tif$'))
