library(rasterVis)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(dplyr)
library(viridis)

rast <- raster('G://My Drive/Dissertation/Final Maps/AGU_map.tif')

sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile',
              'ne_50m_admin_0_countries')

##################
#Drought
###################
col.l <- c("#AAAAAA", viridis(20, option = 'plasma')) 

rastc <- crop(rast, extent(-18, 100, -35, 45))

levelplot(rastc, col.regions=col.l,
          xlab='', ylab='', 
          margin=F, 
          main=list(label="Increases in Mean HAZ Scores Under Drought Due to Natural Land Cover", cex=3),
          maxpixels=1.5e6,
          scales=list(draw=FALSE),
          colorkey=list(labels=list(cex=2))) + 
  layer(sp.polygons(sp, col="#000000"))
