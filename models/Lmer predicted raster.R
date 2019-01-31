library(dplyr)
library(broom)
library(tidyr)
library(MASS)
library(lme4)

source('C://Git/spi-malnutrition/models/mod_utils.R')

setwd('~/dhsprocessed')
setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov)) %>%
  na.omit

#Make categorical
all$spei <- all$spei24

all$spei <- ifelse(all$spei > 1.4, "Normal",
                   ifelse(all$spei < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

sel <- all %>%
  filter(builtup < 20 & bare < 95 & spei24 <= 1.4)

transformations <- list(mean_annual_precip=function(x){log((x + 1)/1000)},
                        #imports_percap=function(x){log(x)},
                        population=function(x){log(x + 1)},
                        elevation=function(x){x/1000},
                        market_dist=function(x){x/24})

for (n in names(all)){
  if (n %in% names(transformations)){
    all[ , n] <- transformations[[n]](all[ , n])
  }
}


mod <- lmer(haz_dhs ~ age + calc_birthmonth + 
             birth_order + hhsize + sex + mother_years_ed + toilet +
             head_age + head_sex + wealth_index 
           #GDP related vars
           #+ spei*ag_pct_gdp
           #+ spei*grid_gdp
           + spei*grid_hdi
           + spei*imports_percap
           # 
           ## + spei*enrollment
           #  
           # #Pop-Urban vars
           + spei*population
           #+ spei*low_settle#Slightly colinear with irrigation (0.50) so avoid this one
           #+ spei*high_settle
           + spei*builtup
           # 
           # #Land Cover Vars
           #+ spei*ndvi
           #+ spei*bare
           + spei*mean_annual_precip
           # 
           # #Topographic Vars
           + spei*elevation
           #+ spei*roughness
           #+ spei*tmax_10yr_mean
           # 
           # #Totally Independat Vars
           #+ spei*market_dist#not the same before and after 2000
           #+ spei*crop_prod
           + spei*government_effectiveness
           + spei*irrig_aei
           + spei*nutritiondiversity_mfad
           + spei*stability_violence
           + spei*assistance
            + (1|country)
            + (1|surveycode)
            + (1|interview_year)
          ,
           data=sel)

summary(mod)

#Baseline
#plot(makeRasts(mod, ''))

#Dry
rast <- make_rasts_year(mod, "speiDry", 2016,
                     transformations,
                     #censor=F,
                     mask=95);plot(rast)



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