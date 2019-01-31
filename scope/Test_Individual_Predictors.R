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
  filter(builtup < 20 & bare < 95 & spei24 <= 1.5)


transformations <- list(grid_gdp=function(x){log((x + 1)/1000)},
                        mean_annual_precip=function(x){log((x + 1)/1000)},
                        imports_percap=function(x){log(x)},
                        population=function(x){log(x + 1)},
                        elevation=function(x){(x/1000) + 1},
                        roughness=function(x){log(x+1)},
                        assistance=function(x){log(x+1)})

for (n in names(all)){
  if (n %in% names(transformations)){
    sel[ , paste0(n, '_t')] <- transformations[[n]](sel[ , n])
  }
}

geovars <- c("mean_annual_precip", "ag_pct_gdp", 
             "ndvi", "forest", "bare", "irrig_aei", "irrig_aai", "market_dist", 
             "gdp", "crop_prod", "government_effectiveness", "stability_violence", 
             "population", "fieldsize", "nutritiondiversity_mfad", "nutritiondiversity_h", 
             "nutritiondiversity_s", "builtup", "elevation", "roughness", 
             "high_settle", "low_settle", "imports_percap", "grid_gdp", "grid_hdi", 
             "enrollment", "assistance", "bodycount", "mean_annual_precip_t", 
             "population_t", "elevation_t", "roughness_t", "imports_percap_t", 
             "grid_gdp_t", "assistance_t")

df <- data.frame()
for (var in geovars){
  sel$geovar <- sel[ , var]
  
  mod <- rlm(haz_dhs ~ age + calc_birthmonth + 
               birth_order + hhsize + sex + mother_years_ed + toilet +
               head_age + head_sex + wealth_index + spei*geovar
             ,
             data=sel)

  re_mod <- lmer(haz_dhs ~ age + calc_birthmonth + 
                   birth_order + hhsize + sex + mother_years_ed + toilet +
                   head_age + head_sex + wealth_index + spei*geovar + 
                   (1|country) + (1|interview_year) + (1|surveycode)
                 ,
                 data=sel)
  
  c <- tidy(mod)
  
  est <- c[c$term=='geovar', 'estimate']
  stat <- c[c$term=='geovar', 'statistic']
  
  Dry_est <- c[c$term=='speiDry:geovar', 'estimate']
  Dry_stat <- c[c$term=='speiDry:geovar', 'statistic']
  
  re <- tidy(re_mod)
  
  re_est <- re[re$term=='geovar', 'estimate']
  re_stat <- re[re$term=='geovar', 'statistic']
  
  re_Dry_est <- re[re$term=='speiDry:geovar', 'estimate']
  re_Dry_stat <- re[re$term=='speiDry:geovar', 'statistic']
  
  df <- bind_rows(df, data.frame(var, est, stat, Dry_est, Dry_stat, re_est, re_stat, re_Dry_est, re_Dry_stat))
  
  print(var)
  
}

write.csv(df, 'G://My Drive/Dissertation/mod_summaries.csv', row.names=F)



