library(raster)
library(lubridate)

################################
#Process radiation data
################################

# data in w m-2 d-1
# From MERRA-2 satellite
# DOwnloaded from: https://disc.sci.gsfc.nasa.gov/datasets/M2TMNXLFO_V5.12.4/summary?keywords=tavgm_2d_lfo_nx
# Variable used is Incident_shortwave_land, or SWGDN
# This will be Ra in the Hargreaves equation for PET

setwd('G://My Drive/Radiation')

fs <- list.files(pattern='.nc$')

for (f in fs){
  r <- raster(f)
  
  name <- gsub('.SUB.nc', '.tif', f)
  
  writeRaster(r, name, format='GTiff', overwrite=T)
  
  print(name)
  
}

################################
#Process temp data
################################

# data in http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/
# Used by Kat Grace to look at birthweight: https://doi.org/10.1016/j.gloenvcha.2015.06.010 she has a good summary of the methods there
# It originally came from this paper: https://journals.ametsoc.org/doi/abs/10.1175/JCLI3790.1
# It looks like they took reanalysis products, then resampled to fine resolution based on elevation, then adjusted to fit CRU
# In Kelvin

#This was done on a cloud computer

setwd('precip')

library(foreach)
library(doParallel)

cl <- makeCluster(7)
registerDoParallel(cl)
foreach(f=list.files(pattern = '.nc$'), .packages=c('raster', 'lubridate')) %dopar% {

  r <- stack(f)
  
  r <- crop(r, extent(0, 360, -60, 90))
  
  r <- setExtent(r, extent(-180, 180, -60, 90))
  
  val <- substr(f, 1, 4)

  year <- substr(f, 12, 15)
  
  indices <- ymd(paste0(year, '-1-1')) + days(0:(dim(r)[3] - 1))
  
  for (i in 1:12){
    
    sel <- r[[which(month(indices)==i)]]
    out <- calc(sel, fun=mean)
    
    name <- paste0(val, year, substr(100 + i, 2, 3), '.tif')
    print(name)
    
    writeRaster(out, name, format='GTiff')
  }
}

stopCluster(cl)

