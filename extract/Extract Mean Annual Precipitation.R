setwd('G://My Drive/CHIRPS/Monthly')

library(raster)

fs <- list.files()[1:432]

dat <- lapply(fs, raster)

agg <- Reduce('+', dat)

map <- agg/36

map[map<0] <- NA

writeRaster(map, 'G://My Drive/DHS Spatial Covars/Final Rasters/mean_annual_precip.tif', format='GTiff')
