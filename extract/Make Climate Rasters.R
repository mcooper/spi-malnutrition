##############################################################
#Script to calculate mean temp, precip for past 10 years
#Only rasters for past 10 years of data in folders
#############################################################

##Precip
setwd('~/CHIRPS')

fs <- list.files()

library(raster)

f <- stack(fs)

m <- mean(f)

m[m < 0] <- NA

writeRaster(m, 'CHIRPS_10yr_avg.tif', format='GTiff', overwrite=T)

##TMIN
setwd('../TMIN')

fs <- list.files()

f <- stack(fs)

m <- mean(f)

ref <- raster('../CHIRPS/chirps-v2.0.2007.09.tif')

r <- resample(m, ref)

writeRaster(r, 'TMIN_10yr_avg.tif', format='GTiff', overwrite=T)

##TMAX
setwd('../TMAX')

fs <- list.files()

f <- stack(fs)

m <- mean(f)

ref <- raster('../CHIRPS/chirps-v2.0.2007.09.tif')

r <- resample(m, ref)

writeRaster(r, 'TAX_10yr_avg.tif', format='GTiff', overwrite=T)
