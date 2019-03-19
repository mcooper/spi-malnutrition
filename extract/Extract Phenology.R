setwd('G://My Drive/DHS Spatial Covars/Phenology')

library(dplyr)
library(raster)
library(rgdal)

start1 <- raster('phenos1_v02.tif')
start2 <- raster('phenos2_v02.tif')

end1 <- raster('phenoe1_v02.tif')
end2 <- raster('phenoe2_v02.tif')

ref <- raster('../../CHIRPS/Monthly/chirps-v2.0.1981.01.tif')

start1res <- resample(start1, ref, method="bilinear")#, filename="start1aggregate.tif", format='GTiff')
start2res <- resample(start2, ref, method="bilinear")#, filename="start2aggregate.tif", format='GTiff')

end1res <- resample(end1, ref, method="bilinear")#, filename="end1aggregate.tif", format='GTiff')
end2res <- resample(end2, ref, method="bilinear")#, filename="end2aggregate.tif", format='GTiff')

