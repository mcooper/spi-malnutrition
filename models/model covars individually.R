library(dplyr)
library(broom)
library(tidyr)
library(MASS)

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

all$population <- all$population/16 #convert to people per sq km

sel <- all %>%
  filter(builtup < 20 & bare < 95 & spei24 <= 1.4)

transformations <- list(ndvi=function(x){(trunc(rank(x))/length(x))*10},
                        government_effectiveness=function(x){(trunc(rank(x))/length(x))*10},
                        grid_gdp=function(x){(x/1000)},
                        grid_hdi=function(x){(trunc(rank(x))/length(x))*10},
                        mean_annual_precip=function(x){x/100},
                        nutritiondiversity_mfad=function(x){(trunc(rank(x))/length(x))*10},
                        population=function(x){(x)/1000},
                        roughness=function(x){(trunc(rank(x))/length(x))*10},
                        stability_violence=function(x){(trunc(rank(x))/length(x))*10},
                        irrig_aai=function(x){x*10},
                        tmax_10yr_mean=function(x){x},
                        assistance=function(x){x/10}
                        )

for (n in names(transformations)){
  sel[ , paste0(n)] <- transformations[[n]](sel[ , n])
}

spcovars <- c("ndvi", "government_effectiveness", "grid_gdp", "grid_hdi",
              "mean_annual_precip", "nutritiondiversity_mfad", "population",
              "roughness", "stability_violence", "irrig_aai", "tmax_10yr_mean",
              "assistance")

df <- data.frame()
for (i in spcovars){
  sel$var <- sel[ , i]
  
  mod <- lm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
               birth_order + hhsize + sex + mother_years_ed + toilet +
               head_age + head_sex + wealth_index + spei*var,
             data=sel)
  
  coefs <- tidy(mod) %>%
    filter(term %in% c('var', 'speiDry:var'))
  
  new <- data.frame(Normal=coefs$estimate[coefs$term=='var'],
                    Normal_SE=coefs$std.error[coefs$term=='var'],
                    Dry=coefs$estimate[coefs$term=='speiDry:var'],
                    Dry_SE=coefs$std.error[coefs$term=='speiDry:var'],
                     Variable=i)
  
  df <- bind_rows(df, new)
  print(i)
}

write.csv(df, 'G://My Drive/Dissertation/Visualizations/individual_regressors.csv', row.names=F)
