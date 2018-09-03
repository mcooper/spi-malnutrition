library(dplyr)
library(broom)
library(foreach)
library(doParallel)

#setwd('~/dhsprocessed')
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

all$gdp_l <- log(all$gdp)

#exclude:
popbuilt <- c(' + spei*population', 
              ' + spei*builtup', 
              ' + spei*population + spei*builtup', 
              ' + spei*low_settle', 
              ' + spei*high_settle')

roughness <- c(' + spei*roughness', 
               ' + spei*elevation', 
               ' + spei*roughness + spei*elevation')

es <- c(' + spei*ndvi', 
        ' + spei*forest', 
        ' + spei*ndvi + spei*forest')

wdi <- c(' + spei*government_effectiveness', 
         ' + spei*stability_violence', 
         ' + spei*government_effectiveness + spei*stability_violence')

ag <- c(' + spei*ag_pct_gdp', '')

#transform
gdp <- c(' + spei*gdp', ' + spei*gdp_l')

#cutoffs:
cutoffs <- c('1', '2', '1.5', 'loess')

spei <- c('spei24', 'spei36gs')

gd <- expand.grid(popbuilt, roughness, es, wdi, gdp, cutoffs, spei, ag,
                  stringsAsFactors = FALSE)

names(gd) <- c('popbuilt', 'roughness', 'es', 'wdi',
               'gdp', 'cutoffs', 'spei', 'ag')

cl <- makeCluster(64, outfile = '')
registerDoParallel(cl)

foreach(i=1:nrow(gd), .packages=c('dplyr', 'broom')) %dopar% {
  
  df <- gd[i, ]
  
  form <- "haz_dhs ~ age + as.factor(calc_birthmonth) + 
               birth_order + hhsize + sex + mother_years_ed + toilet +
               head_age + head_sex + wealth_index + 
               spei*crop_prod + spei*irrigation + spei*nutritiondiversity +
               spei*precip_10yr_mean + spei*tmax_10yr_mean"
  
  form <- paste0(form, gd$popbuilt[i], gd$roughness[i], gd$es[i], gd$wdi[i], gd$gdp[i],
                 gd$ag[i])
  
  all$spei <- all[ , gd$spei[i]]
  
  if(gd$cutoffs[i]=="1"){
    all$spei <- ifelse(all$spei > 1, "Wet",
                         ifelse(all$spei < -1, "Dry", "Normal")) %>%
      as.factor %>%
      relevel(ref = "Normal")
  } else if(gd$cutoffs[i]=="2"){
    all$spei <- ifelse(all$spei > 2, "Wet",
                          ifelse(all$spei < -2, "Dry", "Normal")) %>%
      as.factor %>%
      relevel(ref = "Normal")
  } else if(gd$cutoffs[i]=="1.5"){
    all$spei <- ifelse(all$spei > 1.5, "Wet",
                          ifelse(all$spei < -1.5, "Dry", "Normal")) %>%
      as.factor %>%
      relevel(ref = "Normal")
  } else{
    all$spei <- ifelse(all$spei > 1.5, "Wet",
                          ifelse(all$spei < -0.4, "Dry", "Normal")) %>%
      as.factor %>%
      relevel(ref = "Normal")
  }
  
  mod <- lm(as.formula(form), data=all)
  
  df[ , 'AIC'] <- AIC(mod)
  
  s <- tidy(mod)
  row.names(s) <- s$term

  #Wet Coefs
  df[ , 'Wet_estimate'] <- s['speiWet', 'estimate']
  df[ , 'Wet:ag_pct_gdp_estimate'] <- s['speiWet:ag_pct_gdp', 'estimate'] 
  df[ , 'Wet:builtup_estimate'] <- s['speiWet:builtup', 'estimate']
  df[ , 'Wet:crop_prod_estimate'] <- s['speiWet:crop_prod', 'estimate']
  df[ , 'Wet:elevation_estimate'] <- s['speiWet:elevation', 'estimate']
  df[ , 'Wet:forest_estimate'] <- s['speiWet:forest', 'estimate']
  df[ , 'Wet:gdp_estimate'] <- s['speiWet:gdp', 'estimate']
  df[ , 'Wet:gdp_l_estimate'] <- s['speiWet:gdp_l', 'estimate']
  df[ , 'Wet:government_effectiveness_estimate'] <- s['speiWet:government_effectiveness', 'estimate']
  df[ , 'Wet:high_settle_estimate'] <- s['speiWet:high_settle', 'estimate']
  df[ , 'Wet:irrigation_estimate'] <- s['speiWet:irrigation', 'estimate']
  df[ , 'Wet:low_settle_estimate'] <- s['speiWet:low_settle', 'estimate']
  df[ , 'Wet:ndvi_estimate'] <- s['speiWet:ndvi', 'estimate']
  df[ , 'Wet:nutritiondiversity_estimate'] <- s['speiWet:nutritiondiversity', 'estimate']
  df[ , 'Wet:population_estimate'] <- s['speiWet:population', 'estimate']
  df[ , 'Wet:precip_10yr_mean_estimate'] <- s['speiWet:precip_10yr_mean', 'estimate']
  df[ , 'Wet:roughness_estimate'] <- s['speiWet:roughness', 'estimate']
  df[ , 'Wet:stability_violence_estimate'] <- s['speiWet:stability_violence', 'estimate']
  df[ , 'Wet:tmax_estimate'] <- s['speiWet:tmax', 'estimate']
  
  #Wet Statistic
  df[ , 'Wet_statistic'] <- s['speiWet', 'statistic']
  df[ , 'Wet:ag_pct_gdp_statistic'] <- s['speiWet:ag_pct_gdp', 'statistic'] 
  df[ , 'Wet:builtup_statistic'] <- s['speiWet:builtup', 'statistic']
  df[ , 'Wet:crop_prod_statistic'] <- s['speiWet:crop_prod', 'statistic']
  df[ , 'Wet:elevation_statistic'] <- s['speiWet:elevation', 'statistic']
  df[ , 'Wet:forest_statistic'] <- s['speiWet:forest', 'statistic']
  df[ , 'Wet:gdp_statistic'] <- s['speiWet:gdp', 'statistic']
  df[ , 'Wet:gdp_l_statistic'] <- s['speiWet:gdp_l', 'statistic']
  df[ , 'Wet:government_effectiveness_statistic'] <- s['speiWet:government_effectiveness', 'statistic']
  df[ , 'Wet:high_settle_statistic'] <- s['speiWet:high_settle', 'statistic']
  df[ , 'Wet:irrigation_statistic'] <- s['speiWet:irrigation', 'statistic']
  df[ , 'Wet:low_settle_statistic'] <- s['speiWet:low_settle', 'statistic']
  df[ , 'Wet:ndvi_statistic'] <- s['speiWet:ndvi', 'statistic']
  df[ , 'Wet:nutritiondiversity_statistic'] <- s['speiWet:nutritiondiversity', 'statistic']
  df[ , 'Wet:population_statistic'] <- s['speiWet:population', 'statistic']
  df[ , 'Wet:precip_10yr_mean_statistic'] <- s['speiWet:precip_10yr_mean', 'statistic']
  df[ , 'Wet:roughness_statistic'] <- s['speiWet:roughness', 'statistic']
  df[ , 'Wet:stability_violence_statistic'] <- s['speiWet:stability_violence', 'statistic']
  df[ , 'Wet:tmax_statistic'] <- s['speiWet:tmax', 'statistic']
  
  #Dry Coefs
  df[ , 'Dry_estimate'] <- s['speiDry', 'estimate']
  df[ , 'Dry:ag_pct_gdp_estimate'] <- s['speiDry:ag_pct_gdp', 'estimate'] 
  df[ , 'Dry:builtup_estimate'] <- s['speiDry:builtup', 'estimate']
  df[ , 'Dry:crop_prod_estimate'] <- s['speiDry:crop_prod', 'estimate']
  df[ , 'Dry:elevation_estimate'] <- s['speiDry:elevation', 'estimate']
  df[ , 'Dry:forest_estimate'] <- s['speiDry:forest', 'estimate']
  df[ , 'Dry:gdp_estimate'] <- s['speiDry:gdp', 'estimate']
  df[ , 'Dry:gdp_l_estimate'] <- s['speiDry:gdp_l', 'estimate']
  df[ , 'Dry:government_effectiveness_estimate'] <- s['speiDry:government_effectiveness', 'estimate']
  df[ , 'Dry:high_settle_estimate'] <- s['speiDry:high_settle', 'estimate']
  df[ , 'Dry:irrigation_estimate'] <- s['speiDry:irrigation', 'estimate']
  df[ , 'Dry:low_settle_estimate'] <- s['speiDry:low_settle', 'estimate']
  df[ , 'Dry:ndvi_estimate'] <- s['speiDry:ndvi', 'estimate']
  df[ , 'Dry:nutritiondiversity_estimate'] <- s['speiDry:nutritiondiversity', 'estimate']
  df[ , 'Dry:population_estimate'] <- s['speiDry:population', 'estimate']
  df[ , 'Dry:precip_10yr_mean_estimate'] <- s['speiDry:precip_10yr_mean', 'estimate']
  df[ , 'Dry:roughness_estimate'] <- s['speiDry:roughness', 'estimate']
  df[ , 'Dry:stability_violence_estimate'] <- s['speiDry:stability_violence', 'estimate']
  df[ , 'Dry:tmax_estimate'] <- s['speiDry:tmax', 'estimate']
  
  #Dry Statistic
  df[ , 'Dry_statistic'] <- s['speiDry', 'statistic']
  df[ , 'Dry:ag_pct_gdp_statistic'] <- s['speiDry:ag_pct_gdp', 'statistic'] 
  df[ , 'Dry:builtup_statistic'] <- s['speiDry:builtup', 'statistic']
  df[ , 'Dry:crop_prod_statistic'] <- s['speiDry:crop_prod', 'statistic']
  df[ , 'Dry:elevation_statistic'] <- s['speiDry:elevation', 'statistic']
  df[ , 'Dry:forest_statistic'] <- s['speiDry:forest', 'statistic']
  df[ , 'Dry:gdp_statistic'] <- s['speiDry:gdp', 'statistic']
  df[ , 'Dry:gdp_l_statistic'] <- s['speiDry:gdp_l', 'statistic']
  df[ , 'Dry:government_effectiveness_statistic'] <- s['speiDry:government_effectiveness', 'statistic']
  df[ , 'Dry:high_settle_statistic'] <- s['speiDry:high_settle', 'statistic']
  df[ , 'Dry:irrigation_statistic'] <- s['speiDry:irrigation', 'statistic']
  df[ , 'Dry:low_settle_statistic'] <- s['speiDry:low_settle', 'statistic']
  df[ , 'Dry:ndvi_statistic'] <- s['speiDry:ndvi', 'statistic']
  df[ , 'Dry:nutritiondiversity_statistic'] <- s['speiDry:nutritiondiversity', 'statistic']
  df[ , 'Dry:population_statistic'] <- s['speiDry:population', 'statistic']
  df[ , 'Dry:precip_10yr_mean_statistic'] <- s['speiDry:precip_10yr_mean', 'statistic']
  df[ , 'Dry:roughness_statistic'] <- s['speiDry:roughness', 'statistic']
  df[ , 'Dry:stability_violence_statistic'] <- s['speiDry:stability_violence', 'statistic']
  df[ , 'Dry:tmax_statistic'] <- s['speiDry:tmax', 'statistic']
  
  write.csv(df, paste0('~/mod_outputs/', i), row.names=F)
  
  print(i)
}


