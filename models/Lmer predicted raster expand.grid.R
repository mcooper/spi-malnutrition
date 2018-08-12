library(dplyr)
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
all$market_dist <- all$market_dist/(24*7)
all$ndvi <- all$ndvi/5000
all$population <- all$population/1000
all$tmax_10yr_mean <- all$tmax_10yr_mean - 273.15
all$tmin_10yr_mean <- all$tmin_10yr_mean - 273.15
all$builtup <- all$builtup*100
all$elevation <- all$elevation/1000

all$countrymonth <- paste0(all$country, all$calc_birthmonth)

#Transform some variables

#Inverse Hyperbolic Sine Transformation
#http://worthwhile.typepad.com/worthwhile_canadian_initi/2011/07/a-rant-on-inverse-hyperbolic-sine-transformations.html
#
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}

#all$forest <- ihs(all$forest)
#all$gdp <- log(all$gdp)
all$market_dist <- ihs(all$market_dist)
all$population <- ihs(all$population)
#all$fieldsize <- ihs(all$fieldsize)
all$builtup <- ihs(all$builtup)
#all$elevation <- ihs(all$elevation)

#Parameters to tweak
country <- c(TRUE, FALSE)
surveycode <- c(TRUE, FALSE)
countrymonth <- c('countrymonth', 'month', 'none')
year <- c('continuous', 'factor')

#exclude:
popbuilt <- c('population', 'builtup', 'both')

#transform
gdp <- c(TRUE, FALSE)

#cutoffs:
cutoffs <- c('1', '2', '1.5', 'loess')

gd <- expand.grid(country, surveycode, countrymonth, year,
                  popbuilt, cutoffs, gdp)

names(gd) <- c('country', 'surveycode', 'countrymonth', 'year',
               'popbuilt', 'cutoffs', 'gdp')

for (i in 1:nrow(gd)){
  
  form <- "haz_dhs ~ age +
               birth_order + hhsize + sex + mother_years_ed + toilet +
               head_age + head_sex + wealth_index + urban_rural +
               spei24c*ag_pct_gdp + spei24c*builtup + spei24c*crop_prod +
               spei24c*elevation + spei24c*fieldsize + spei24c*forest +
               spei24c*gdp_t + spei24c*government_effectiveness + spei24c*irrigation +
               spei24c*market_dist + spei24c*ndvi + spei24c*nutritiondiversity +
               spei24c*precip_10yr_mean + 
               spei24c*stability_violence + spei24c*tmax_10yr_mean"

  if(gd$country[i]){
    form <- paste0(form, " + country")
  }
  
  if(gd$surveycode[i]){
    form <- paste0(form, " + surveycode")
  }
  
  if(gd$countrymonth[i]=="countrymonth"){
    form <- paste0(form, " + countrymonth")
  } else if(gd$countrymonth[i]=="month"){
    form <- paste0(form, " + as.factor(calc_birthmonth)")
  }
  
  if(gd$year[i]=="continuous"){
    form <- paste0(form, " + interview_year")
  } else if(gd$year[i]=="factor"){
    form <- paste0(form, " + as.factor(interview_year)")
  }
  
  if(gd$popbuilt[i] == "population"){
    form <- paste0(form, " + spei24c*population")
  } else if(gd$popbuilt[i] == "builtup"){
    form <- paste0(form, " + spei24c*builtup")
  } else{
    form <- paste0(form, " + spei24c*builtup + spei24c*population")
  }
  
  if(gd$gdp[i]){
    all$gdp_t <- log(all$gdp)
  } else{
    all$gdp_t <- all$gdp
  }
  
  if(gd$cutoffs[i]=="1"){
    all$spei24c <- ifelse(all$spei24 > 1, "Wet",
                         ifelse(all$spei24 < -1, "Dry", "Normal")) %>%
      as.factor %>%
      relevel(ref = "Normal")
  } else if(gd$cutoffs[i]=="2"){
    all$spei24c <- ifelse(all$spei24 > 2, "Wet",
                          ifelse(all$spei24 < -2, "Dry", "Normal")) %>%
      as.factor %>%
      relevel(ref = "Normal")
  } else if(gd$cutoffs[i]=="1.5"){
    all$spei24c <- ifelse(all$spei24 > 1.5, "Wet",
                          ifelse(all$spei24 < -1.5, "Dry", "Normal")) %>%
      as.factor %>%
      relevel(ref = "Normal")
  } else{
    all$spei24c <- ifelse(all$spei24 > 1.5, "Wet",
                          ifelse(all$spei24 < -0.4, "Dry", "Normal")) %>%
      as.factor %>%
      relevel(ref = "Normal")
  }
  
  mod <- lm(as.formula(form), data=all)
  
  gd[i, 'AIC'] <- AIC(mod)
  
  s <- tidy(mod)
  row.names(s) <- s$term

  #Wet Coefs
  gd[i, 'spei24cWet_estimate'] <- s['spei24cWet', 'estimate']
  gd[i, 'spei24cWet:ag_pct_gdp_estimate'] <- s['spei24cWet:ag_pct_gdp', 'estimate'] 
  gd[i, 'spei24cWet:builtup_estimate'] <- s['spei24cWet:builtup', 'estimate']
  gd[i, 'spei24cWet:crop_prod_estimate'] <- s['spei24cWet:crop_prod', 'estimate']
  gd[i, 'spei24cWet:elevation_estimate'] <- s['spei24cWet:elevation', 'estimate']
  gd[i, 'spei24cWet:fieldsize_estimate'] <- s['spei24cWet:fieldsize', 'estimate']
  gd[i, 'spei24cWet:forest_estimate'] <- s['spei24cWet:forest', 'estimate']
  gd[i, 'spei24cWet:gdp_t_estimate'] <- s['spei24cWet:gdp_t', 'estimate']
  gd[i, 'spei24cWet:government_effectiveness_estimate'] <- s['spei24cWet:government_effectiveness', 'estimate']
  gd[i, 'spei24cWet:irrigation_estimate'] <- s['spei24cWet:irrigation', 'estimate']
  gd[i, 'spei24cWet:market_dist_estimate'] <- s['spei24cWet:market_dist', 'estimate']
  gd[i, 'spei24cWet:ndvi_estimate'] <- s['spei24cWet:ndvi', 'estimate']
  gd[i, 'spei24cWet:nutritiondiversity_estimate'] <- s['spei24cWet:nutritiondiversity', 'estimate']
  gd[i, 'spei24cWet:population_estimate'] <- s['spei24cWet:population', 'estimate']
  gd[i, 'spei24cWet:precip_10yr_mean_estimate'] <- s['spei24cWet:precip_10yr_mean', 'estimate']
  gd[i, 'spei24cWet:stability_violence_estimate'] <- s['spei24cWet:stability_violence', 'estimate']
  gd[i, 'spei24cWet:tmax_estimate'] <- s['spei24cWet:tmax', 'estimate']

  #Dry Coefs
  gd[i, 'spei24cDry_estimate'] <- s['spei24cDry', 'estimate']
  gd[i, 'spei24cDry:ag_pct_gdp_estimate'] <- s['spei24cDry:ag_pct_gdp', 'estimate'] 
  gd[i, 'spei24cDry:builtup_estimate'] <- s['spei24cDry:builtup', 'estimate']
  gd[i, 'spei24cDry:crop_prod_estimate'] <- s['spei24cDry:crop_prod', 'estimate']
  gd[i, 'spei24cDry:elevation_estimate'] <- s['spei24cDry:elevation', 'estimate']
  gd[i, 'spei24cDry:fieldsize_estimate'] <- s['spei24cDry:fieldsize', 'estimate']
  gd[i, 'spei24cDry:forest_estimate'] <- s['spei24cDry:forest', 'estimate']
  gd[i, 'spei24cDry:gdp_t_estimate'] <- s['spei24cDry:gdp_t', 'estimate']
  gd[i, 'spei24cDry:government_effectiveness_estimate'] <- s['spei24cDry:government_effectiveness', 'estimate']
  gd[i, 'spei24cDry:irrigation_estimate'] <- s['spei24cDry:irrigation', 'estimate']
  gd[i, 'spei24cDry:market_dist_estimate'] <- s['spei24cDry:market_dist', 'estimate']
  gd[i, 'spei24cDry:ndvi_estimate'] <- s['spei24cDry:ndvi', 'estimate']
  gd[i, 'spei24cDry:nutritiondiversity_estimate'] <- s['spei24cDry:nutritiondiversity', 'estimate']
  gd[i, 'spei24cDry:population_estimate'] <- s['spei24cDry:population', 'estimate']
  gd[i, 'spei24cDry:precip_10yr_mean_estimate'] <- s['spei24cDry:precip_10yr_mean', 'estimate']
  gd[i, 'spei24cDry:stability_violence_estimate'] <- s['spei24cDry:stability_violence', 'estimate']
  gd[i, 'spei24cDry:tmax_estimate'] <- s['spei24cDry:tmax', 'estimate']

  #Wet Statistic
  gd[i, 'spei24cWet_statistic'] <- s['spei24cWet', 'statistic']
  gd[i, 'spei24cWet:ag_pct_gdp_statistic'] <- s['spei24cWet:ag_pct_gdp', 'statistic'] 
  gd[i, 'spei24cWet:builtup_statistic'] <- s['spei24cWet:builtup', 'statistic']
  gd[i, 'spei24cWet:crop_prod_statistic'] <- s['spei24cWet:crop_prod', 'statistic']
  gd[i, 'spei24cWet:elevation_statistic'] <- s['spei24cWet:elevation', 'statistic']
  gd[i, 'spei24cWet:fieldsize_statistic'] <- s['spei24cWet:fieldsize', 'statistic']
  gd[i, 'spei24cWet:forest_statistic'] <- s['spei24cWet:forest', 'statistic']
  gd[i, 'spei24cWet:gdp_t_statistic'] <- s['spei24cWet:gdp_t', 'statistic']
  gd[i, 'spei24cWet:government_effectiveness_statistic'] <- s['spei24cWet:government_effectiveness', 'statistic']
  gd[i, 'spei24cWet:irrigation_statistic'] <- s['spei24cWet:irrigation', 'statistic']
  gd[i, 'spei24cWet:market_dist_statistic'] <- s['spei24cWet:market_dist', 'statistic']
  gd[i, 'spei24cWet:ndvi_statistic'] <- s['spei24cWet:ndvi', 'statistic']
  gd[i, 'spei24cWet:nutritiondiversity_statistic'] <- s['spei24cWet:nutritiondiversity', 'statistic']
  gd[i, 'spei24cWet:population_statistic'] <- s['spei24cWet:population', 'statistic']
  gd[i, 'spei24cWet:precip_10yr_mean_statistic'] <- s['spei24cWet:precip_10yr_mean', 'statistic']
  gd[i, 'spei24cWet:stability_violence_statistic'] <- s['spei24cWet:stability_violence', 'statistic']
  gd[i, 'spei24cWet:tmax_statistic'] <- s['spei24cWet:tmax', 'statistic']
  
  #Dry Statistic
  gd[i, 'spei24cDry_statistic'] <- s['spei24cDry', 'statistic']
  gd[i, 'spei24cDry:ag_pct_gdp_statistic'] <- s['spei24cDry:ag_pct_gdp', 'statistic'] 
  gd[i, 'spei24cDry:builtup_statistic'] <- s['spei24cDry:builtup', 'statistic']
  gd[i, 'spei24cDry:crop_prod_statistic'] <- s['spei24cDry:crop_prod', 'statistic']
  gd[i, 'spei24cDry:elevation_statistic'] <- s['spei24cDry:elevation', 'statistic']
  gd[i, 'spei24cDry:fieldsize_statistic'] <- s['spei24cDry:fieldsize', 'statistic']
  gd[i, 'spei24cDry:forest_statistic'] <- s['spei24cDry:forest', 'statistic']
  gd[i, 'spei24cDry:gdp_t_statistic'] <- s['spei24cDry:gdp_t', 'statistic']
  gd[i, 'spei24cDry:government_effectiveness_statistic'] <- s['spei24cDry:government_effectiveness', 'statistic']
  gd[i, 'spei24cDry:irrigation_statistic'] <- s['spei24cDry:irrigation', 'statistic']
  gd[i, 'spei24cDry:market_dist_statistic'] <- s['spei24cDry:market_dist', 'statistic']
  gd[i, 'spei24cDry:ndvi_statistic'] <- s['spei24cDry:ndvi', 'statistic']
  gd[i, 'spei24cDry:nutritiondiversity_statistic'] <- s['spei24cDry:nutritiondiversity', 'statistic']
  gd[i, 'spei24cDry:population_statistic'] <- s['spei24cDry:population', 'statistic']
  gd[i, 'spei24cDry:precip_10yr_mean_statistic'] <- s['spei24cDry:precip_10yr_mean', 'statistic']
  gd[i, 'spei24cDry:stability_violence_statistic'] <- s['spei24cDry:stability_violence', 'statistic']
  gd[i, 'spei24cDry:tmax_statistic'] <- s['spei24cDry:tmax', 'statistic']

  print(i)
}


