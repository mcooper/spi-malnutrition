library(dplyr)
library(tidyr)
library(ggplot2)

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

all$spei <- ifelse(all$spei > 1.5, "Wet Period",
                   ifelse(all$spei < -0.4, "Dry Period", "Normal Period"))

plot <- all %>%
  dplyr::select(grid_gdp, population, ndvi, tmax_10yr_mean, market_dist, 
         crop_prod, government_effectiveness, irrig_aai, nutritiondiversity, 
         stability_violence, spei) %>%
  group_by(spei) %>%
  summarize_all(funs(max, min)) %>%
  gather(Variable, Value, -spei) %>%
  mutate(Func=substr(Variable, nchar(Variable)-3, nchar(Variable)),
         Func=gsub('_', '', Func),
         Variable=gsub('_max$|_min$', '', Variable)) %>%
  spread(Func, Value)

ggplot(plot) +
  geom_linerange(aes(ymin=min, ymax=max, x=spei)) +
  facet_grid(Variable~., scales='free_y')
