library(dplyr)
library(broom)
library(tidyr)
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
all$population <- all$population/1000
all$tmax_10yr_mean <- all$tmax_10yr_mean - 273.15
all$tmin_10yr_mean <- all$tmin_10yr_mean - 273.15
all$builtup <- all$builtup*100
all$elevation <- all$elevation/1000
all$imports_percap <- all$imports_percap/1000

pca <- all %>% 
  dplyr::select(ag_pct_gdp, forest, government_effectiveness, 
         irrig_aai, market_dist, ndvi, population, 
         stability_violence, crop_prod, nutritiondiversity, 
         builtup, elevation, roughness, 
         imports_percap, grid_gdp, grid_hdi, enrollment,
         precip_10yr_mean, tmax_10yr_mean, tmin_10yr_mean) %>%
  prcomp

all <- cbind(all, pca$x)

library(raster)

setwd('G://My Drive/DHS Spatial Covars/Final Rasters')

readCrop <- function(str){
  r <- raster(str)
  
  ext <- new("Extent" , xmin = -180 , xmax = 180, ymin = -50, ymax = 50)
  
  crop(r, ext)
}

ag_pct_gdp <- readCrop('ag_pct_gdp.tif')
builtup <- (readCrop('builtup.tif')*100)
crop_prod <- readCrop('crop_prod.tif')
elevation <- (readCrop('elevation.tif')/1000)
forest <- readCrop('forest.tif')
enrollment <- readCrop('enrollment.tif')
grid_gdp <- (readCrop('grid_gdp.tif')/1000)
grid_hdi <- readCrop('grid_hdi.tif')
government_effectiveness <- readCrop('government_effectiveness.tif')
imports_percap <- readCrop('imports_percap.tif')/1000
irrig_aai <- readCrop('irrig_aai.tif')
market_dist <- readCrop('market_dist.tif')/(24*7)
ndvi <- readCrop('ndvi.tif')
nutritiondiversity <- readCrop('nutritiondiversity.tif')
population <- (readCrop('population.tif')/1000)
precip_10yr_mean <- (readCrop('precip_10yr_mean.tif')*12/1000)
roughness <- readCrop('roughness.tif')
stability_violence <- readCrop('stability_violence.tif')
tmax_10yr_mean <- (readCrop('tmax_10yr_mean.tif') - 273.15)
tmin_10yr_mean <- (readCrop('tmin_10yr_mean.tif') - 273.15)

stk <- stack(ag_pct_gdp, forest, government_effectiveness, 
             irrig_aai, market_dist, ndvi, population, 
             stability_violence, crop_prod, nutritiondiversity, 
             builtup, elevation, roughness, 
             imports_percap, grid_gdp, grid_hdi, enrollment,
             precip_10yr_mean, tmax_10yr_mean, tmin_10yr_mean)

pred <- predict(stk, pca, index=1:20)
names(pred) <- gsub('layer.', 'PC', names(pred))


#Make categorical
all$spei <- all$spei24

all$spei <- ifelse(all$spei > 1.5, "Wet",
                   ifelse(all$spei < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

makeRastsPCA <- function(mod, term, usebase=FALSE, censor=TRUE){
  #mod is the model that has been fit
  #term can be either 'speiDry', 'speiWet', or ''
  #  'speiDry' and 'speiWet' will map estimated changes in HAZ scores during a wet or dry year
  #  '' will give a map of estimated HAZ scores during a normal year
  #usebase will use a raster of 0s and 1s to exclude certain areas.
  #censor will set values greater than 0 to NA
  
  s <- tidy(mod)
  
  coefs <- s[grepl(term, s$term), c('term', 'estimate')]
  
  coefs$term <- gsub(paste0(term, ':'), '', coefs$term)
  
  if(term == ''){
    coefs <- coefs[!grepl('spei', coefs$term), ]
  }
  
  if (usebase){
    rast <- base*coefs$estimate[1]
  } else{
    rast <- coefs$estimate[1]
  }
  for (i in 2:nrow(coefs)){
    if(coefs$term[i] %in% names(pred)){
      tmp_rast <- pred[[i]]
    } else{
      tmp_rast <- 0
    }
    suppressWarnings(rast <- rast + tmp_rast*coefs$estimate[i])
  }
  
  if (censor){
    rast[rast > 0] <- NA
  }
  
  return(rast)
}

mod <- rlm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
             birth_order + hhsize + sex + mother_years_ed + toilet +
             head_age + head_sex + wealth_index + 
             spei*PC1 + spei*PC2 + spei*PC3 + spei*PC4 + spei*PC5 + spei*PC6 + spei*PC7 + 
             spei*PC8 + spei*PC8 + spei*PC9 + spei*PC10 + spei*PC11 + spei*PC12
           ,
           data=all)

summary(mod)


#Dry
plot(makeRastsPCA(mod, "speiDry"))


