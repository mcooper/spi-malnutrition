library(rasterVis)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(dplyr)

setwd('G://My Drive/Dissertation/Final Maps/annual pngs/')

sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile',
              'ne_50m_admin_0_countries')
col.l <- c(colorRampPalette(c("#780000", "#dc0000", "#fd8c00", "#fdc500"))(29), "#DDDDDD") 

for (year in seq(1990, 2020)){
  dry <- raster(paste0('G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/Dry', year, '.tif'))
  
  ##################
  #Drought
  ###################
  
  plt <- levelplot(dry, xlim=c(-180, 180), ylim=c(-40, 50),
            at=seq(-0.4, 0, length.out = 30),
            col.regions=col.l,
            xlab='', ylab='', 
            margin=F, 
            main=list(label=paste0(year, " - Expected Change in Mean HAZ Scores Under Drought"), cex=3),
            maxpixels=1.5e6,
            scales=list(draw=FALSE),
            colorkey=list(labels=list(cex=2), space="bottom", height=0.5)) + 
    layer(sp.polygons(sp, col="#444444"))
  
  png(paste0(year, '.png'), 
      width=1250, height=625)
  print(plt)
  dev.off()
  
}

#must run from terminal manually
#convert -delay 60 -loop 0 *.png droughtTS.gif'
