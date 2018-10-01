library(dplyr)
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
all$ndvi <- all$ndvi
all$population <- all$population/1000
all$tmax_10yr_mean <- all$tmax_10yr_mean - 273.15
all$tmin_10yr_mean <- all$tmin_10yr_mean - 273.15
all$builtup <- all$builtup*100
all$elevation <- all$elevation/1000
all$imports_percap <- all$imports_percap/1000

all$gdp_l <- log(all$gdp)
all$grid_gdp_l <- log(all$grid_gdp)


all %>% dplyr::select(grid_gdp, crop_prod, government_effectiveness, 
                      irrig_aei, ndvi, nutritiondiversity, population, 
                      stability_violence, tmax_10yr_mean, market_dist) %>% 
  group_by() %>% 
  summarize_all(funs(min, max, mean)) %>% 
  gather(key, value) %>%
  mutate(fun=gsub('_', '', substr(key, nchar(key)-3, nchar(key))),
         key=gsub('_min|_max|_mean', '', key),
         value=signif(value, 3)) %>%
  spread(fun, value) %>%
  dplyr::select(key, min, max, mean) %>%
  write.csv('C://Users/matt/summary_stats.csv', row.names=F)

