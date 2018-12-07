library(raster)

setwd("G://My Drive/DHS Spatial Covars/ESA Land Cover/")

ref <- raster('G://My Drive/Chirps/Monthly/chirps-v2.0.1981.01.tif')

f <- raster("forest.tif")
f_res <- resample(f, ref)
writeRaster(f_res, 'forest_resample.tif', format='GTiff')

g <- raster("grass.tif")
g_res <- resample(g, ref)
writeRaster(g_res, 'grass_resample.tif', format='GTiff')

w <- raster("water.tif")
w_res <- resample(w, ref)
writeRaster(w_res, 'water_resample.tif', format='GTiff')
 