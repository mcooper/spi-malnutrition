library(dplyr)
library(randomForest)
library(doParallel)

setwd('~/dhsprocessed')

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov)) %>%
  na.omit

data <- all %>%
  dplyr::select(ag_pct_gdp, forest, government_effectiveness, irrig_aei, market_dist, ndvi, population,
                stability_violence, crop_prod, fieldsize, nutritiondiversity, builtup, elevation, roughness,
                imports_percap, grid_gdp, grid_hdi, precip_10yr_mean, tmin_10yr_mean, tmax_10yr_mean)

haz <- all$haz_dhs


cl <- makeCluster(8, outfile = '')
registerDoParallel(cl)


for (i in c("loess", "ones")){
  
  if(i=='loess'){
    category <- ifelse(all$spei24 > 1.5, "Wet",
                       ifelse(all$spei24 < -0.4, "Dry", "Normal"))
  }
  
  if(i=='ones'){
    category <- ifelse(all$spei24 > 1, "Wet",
                       ifelse(all$spei24 < -1, "Dry", "Normal"))
  }
  
  if(i=='onepointfive'){
    category <- ifelse(all$spei24 > 1.5, "Wet",
                       ifelse(all$spei24 < -1.5, "Dry", "Normal"))
  }
  
  if(i=='two'){
    category <- ifelse(all$spei24 > 2, "Wet",
                       ifelse(all$spei24 < -2, "Dry", "Normal"))
  }
  
  #wet
  rfwet <- foreach(n=seq(1,8), .combine=combine, .multicombine=TRUE, .packages='randomForest') %dopar% {
    mod <- randomForest(haz[category=='Wet'] ~ ., data=data[category=='Wet', ], ntree=65)
    mod
  }
  save('rfwet', file=paste0('rf-haz-wet', i))
  
  #dry
  rfdry <- foreach(n=seq(1,8), .combine=combine, .multicombine=TRUE, .packages='randomForest') %dopar% {
    mod <- randomForest(haz[category=='Dry'] ~ ., data=data[category=='Dry', ], ntree=65)
    mod
  }
  save('rfdry', file=paste0('rf-haz-dry', i))
  
  #normal
  rfnormal <- foreach(n=seq(1,8), .combine=combine, .multicombine=TRUE, .packages='randomForest') %dopar% {
    mod <- randomForest(haz[category=='Normal'] ~ ., data=data[category=="Normal", ], ntree=65)
    mod
  }
  save('rfnormal', file=paste0('rf-haz-normal', i))
  
}

system('./../telegram.sh')


