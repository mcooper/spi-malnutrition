library(dplyr)
library(broom)
library(foreach)
library(doParallel)
library(MASS)

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
all$ndvi <- all$ndvi/5000
all$population <- all$population/1000
all$tmax_10yr_mean <- all$tmax_10yr_mean - 273.15
all$tmin_10yr_mean <- all$tmin_10yr_mean - 273.15
all$builtup <- all$builtup*100
all$elevation <- all$elevation/1000
all$imports_percap <- all$imports_percap/1000

all$gdp_l <- log(all$gdp)
all$grid_gdp_l <- log(all$grid_gdp)

#exclude:
popbuilt <- paste0(' + spei*',
                   c('population', 'builtup', 'low_settle', 'high_settle'))

roughness <- paste0(' + spei*',
                    c('roughness', 
                      'elevation', 
                      'tmax_10yr_mean'))

es <- paste0(' + spei*',
             c('ndvi', 
               'forest', 
               'bare',
               'precip_10yr_mean'))

gdp <- paste0(' + spei*',
              c("ag_pct_gdp",
                "gdp",
                "gdp_l", 
                "grid_gdp_l",  
                "grid_gdp",  
                "grid_hdi",  
                "imports_percap"))

md <- c(' + spei*market_dist', '')

#cutoffs:
cutoffs <- c('1', '2', '1.5', 'loess')

spei <- c('spei24', 'spei36gs')

gd <- expand.grid(popbuilt, roughness, es, gdp, md, cutoffs, spei,
                  stringsAsFactors = FALSE)

names(gd) <- c('popbuilt', 'roughness', 'es',
               'gdp', 'md', 'cutoffs', 'spei')

cl <- makeCluster(2, outfile = '')
registerDoParallel(cl)

foreach(i=1:nrow(gd), .packages=c('dplyr', 'broom', 'MASS')) %dopar% {
  
  df <- gd[i, ]
  
  form <- "haz_dhs ~ age + as.factor(calc_birthmonth) + 
  birth_order + hhsize + sex + mother_years_ed + toilet +
  head_age + head_sex + wealth_index + 
  spei*crop_prod + spei*irrigation + spei*nutritiondiversity +
  spei*government_effectiveness + spei*stability_violence"
  
  form <- paste0(form, gd$popbuilt[i], gd$roughness[i], gd$es[i], gd$gdp[i], gd$md[i])
  
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
  
  mod <- rlm(as.formula(form), data=all)
  
  df[ , 'AIC'] <- AIC(mod)
  
  s <- tidy(mod)
  row.names(s) <- s$term
  
  s <- s[grepl('spei', s$term), ]
  
  s$term <- gsub('spei', '', s$term)
  
  for (j in 1:nrow(s)){
    df[ , paste0(s$term[j], '_est')] <- s[j, 'estimate']
    df[ , paste0(s$term[j], '_stat')] <- s[j, 'statistic']
  }
  
  write.csv(df, paste0('~/mod_outputs/', i), row.names=F)
  
  print(i)
}


