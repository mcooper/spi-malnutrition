setwd('G://My Drive/CHIRPS/Monthly')

library(raster)

fs <- list.files()[1:432]

dat <- lapply(fs, raster)

agg <- Reduce('+', dat)

map <- agg/36

map[map<0] <- NA

for (year in seq(1990, 2020)){
  writeRaster(map, paste0('G://My Drive/DHS Spatial Covars/Final Rasters/', year, '/mean_annual_precip.tif'), format='GTiff')
}