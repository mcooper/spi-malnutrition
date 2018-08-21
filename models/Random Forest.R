library(dplyr)
library(lme4)
library(broom)
library(car)
library(randomForest)
library(doParallel)

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

all$gdp_l <- log(all$gdp)

data <- all %>%
  select(spei24, precip_10yr_mean, 
           forest, gdp, government_effectiveness, irrigation, market_dist, 
           ndvi, population, stability_violence, tmax_10yr_mean, 
           tmin_10yr_mean, crop_prod, fieldsize, nutritiondiversity, 
           builtup, elevation, high_settle, low_settle, roughness)

residuals <- lm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
                      birth_order + hhsize + sex + mother_years_ed + toilet +
                      head_age + head_sex + wealth_index, data=all) %>%
  residuals



cl <- makeCluster(4, outfile = '')
registerDoParallel(cl)


start <- Sys.time()
rf <- foreach(ntree=rep(15, 4), .combine=combine, .multicombine=TRUE,
              .packages='randomForest') %dopar% {
    randomForest(residuals ~ ., data=data, ntree=ntree)
}
Sys.time() - start

setwd('~')

system('./telegram.sh')

