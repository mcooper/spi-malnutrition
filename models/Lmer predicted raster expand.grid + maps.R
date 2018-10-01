library(rasterVis)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(dplyr)
library(foreach)
library(doParallel)

#script run on the table resulting from: https://github.com/mcooper/spi-malnutrition/blob/e678037a57c2d22670543692491de995d99a4270/models/Lmer%20predicted%20raster%20expand.grid.R

csv <- read.csv('~/temp_mod_res.csv') %>%
  filter(spei=='spei24' & cutoffs == 'loess' &
           !gdp %in% c(' + spei*ag_pct_gdp', ' + spei*imports_percap'))
sel <- csv[ , grepl('est$', names(csv))]

setwd('dhsprocessed')

ag_pct_gdp <- raster('ag_pct_gdp.tif')
builtup <- (raster('builtup.tif')*100)
bare <- raster('bare.tif')
crop_prod <- raster('crop_prod.tif')
elevation <- (raster('elevation.tif')/1000)
fieldsize <- raster('fieldsize.tif')
forest <- raster('forest.tif')
gdp <- (raster('gdp2020.tif')/1000)
gdp_l <- log(gdp)
grid_gdp <- (raster('grid_gdp.tif')/1000)
grid_gdp_l <- log(grid_gdp)
grid_hdi <- raster('grid_hdi.tif')
government_effectiveness <- raster('government_effectiveness.tif')
high_settle <- raster('high_settle.tif')
imports_percap <- raster('imports_percap.tif')/1000
irrigation <- raster('irrig_aei.tif')
low_settle <- raster('low_settle.tif')
market_dist <- raster('market_dist.tif')/(24*7)
ndvi <- raster('ndvi.tif')*2
nutritiondiversity <- raster('nutritiondiversity.tif')
population <- (raster('population.tif')/1000)
precip_10yr_mean <- (raster('precip_10yr_mean.tif')*12/1000)
roughness <- raster('roughness.tif')
stability_violence <- raster('stability_violence.tif')
tmax_10yr_mean <- (raster('tmax_10yr_mean.tif') - 273.15)
sp <- readOGR('.', 'ne_50m_admin_0_countries')

setwd('~/expand.grid')

cl <- makeCluster(32, outfile = '')
registerDoParallel(cl)

export = c('ag_pct_gdp', 'builtup', 'crop_prod', 'elevation', 'fieldsize', 'forest', 
           'gdp','gdp_l','grid_gdp','grid_gdp_l','grid_hdi','government_effectiveness',
           'high_settle','imports_percap','irrigation','low_settle','market_dist','ndvi',
           'nutritiondiversity','population','precip_10yr_mean','roughness','stability_violence',
           'tmax_10yr_mean','sp')

foreach(i=1:nrow(sel), .packages=c('rasterVis', 'raster', 'rgdal', 'sp', 'RColorBrewer'), .export=export) %dopar% {
  for (s in c("Dry", "Wet")){
    
    dat <- sel[i, grepl(s, names(sel))]
    dat <- dat[ , !is.na(dat)]
    
    rast <- dat[ , paste0(s, '_est')]
    
    for (j in 2:ncol(dat)){
      term <- names(dat[ , j, drop=FALSE])
      
      estimate <- dat[ , j]
      
      term <- gsub('Wet.|Dry.|_est', '', term)
      
      tmp_rast <- get(term)
      
      suppressWarnings(rast <- rast + tmp_rast*estimate)
    }
    
    rast[rast > 0] <- 0
    
    if (s == 'Dry'){
      col.l <- c(colorRampPalette(c("#780000", "#dc0000", "#fd8c00", "#fdc500"))(29), "#DDDDDD") 
    }
    if (s == 'Wet'){
      col.l <- c(colorRampPalette(c("#15719f", "#528ab4", "#62a1c7", "#7bc7dd", "#95d6ea"))(29), "#DDDDDD") 
    }
    
    plot <- levelplot(rast, xlim=c(-100, 150), ylim=c(-40, 50), col.regions=col.l,
                      xlab='', ylab='', 
                      margin=F, 
                      main=list(label="Expected Change in Mean HAZ Scores Under Drought", cex=3),
                      maxpixels=1.5e6,
                      scales=list(draw=FALSE)) + 
      layer(sp.polygons(sp))
    
    name <- paste0(gsub('Dry\\.|Wet\\.|_est', '', names(dat)), collapse="+")
    
    writeRaster(rast, paste0(i, '-', name, "-", s, ".tiff"), format='GTiff')
    
    png(paste0(i, '-', name, "-", s, ".png"), height = 1000, width = 2000)
    print(plot)
    dev.off()
  }
  print(i)
}