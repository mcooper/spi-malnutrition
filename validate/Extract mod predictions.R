library(raster)
library(dplyr)
library(rgdal)

setwd('G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/')

dat <- read.csv('G://My Drive/DHS Spatial Covars/FEWS Validation/FEWS_CS_SPEI.csv')

dat$yearmonth <- dat$year
dat$year <- substr(dat$year, 1, 4)

df <- data.frame()
for (y in seq(2009, 2016)){
  rast <- raster(paste0('Dry', y, '.tif'))
  
  sel <- dat[grepl(y, dat$year), ]
  
  ll <- sel[ , c('longitude', 'latitude')] %>% 
    unique
  
  llsp <- SpatialPointsDataFrame(ll, ll)
  
  llsp@data$mod <- extract(rast, llsp)
 
  llsp@data$year <- y
  
  df <- bind_rows(df, llsp@data)
  
  print(y) 
}

df$year <- as.character(df$year)

dat <- merge(dat, df)

write.csv(dat, 'G://My Drive/DHS Spatial Covars/FEWS Validation/FEWS_CS_SPEI_mod.csv', row.names=F)
