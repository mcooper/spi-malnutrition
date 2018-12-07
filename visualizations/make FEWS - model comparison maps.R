library(rasterVis)
library(rgdal)
library(dplyr)

########################################
#East Africa Drought 2017
########################################
dry <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/Dry2017.tif')
dif <- raster('G://My Drive/DHS Spatial Covars/FEWS Validation/Difference Rasters/EA_201307_CS-EA_201706_CS.tifGTiff.gri')

sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile',
              'ne_50m_admin_0_countries')

countries <- c('Somalia', 'South Sudan', 'Sudan', 'Kenya', 'Ethiopia', 'Uganda')

col.mod <- c(colorRampPalette(c("#780000", "#dc0000", "#fd8c00", "#fdc500"))(29), "#DDDDDD") 
col.fews <- c("#DDDDDD", "#DDDDDD", "#fd8c00", "#dc0000", "#780000")

drycrop <- crop(dry, sp[sp$SOVEREIGNT %in% countries, ])
difcrop <- crop(dif, sp[sp$SOVEREIGNT %in% countries, ])

modpred <- levelplot(drycrop, xlim=c(22, 52), ylim=c(-5, 24),
                 at=seq(-0.4, 0, length.out = 30),
                 col.regions=col.mod,
                 xlab='', ylab='', 
                 margin=F, 
                 main="Predicted Change in Mean HAZ Scores",
                 maxpixels=1.5e6,
                 scales=list(draw=FALSE),
                 colorkey=list(labels=list(cex=2), space="bottom", height=0.5)) + 
  layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

fewspred <- levelplot(dif, xlim=c(22, 52), ylim=c(-5, 24),
                      at=c(-1.5, -0.5, 0.5, 1.5, 2.5, 3.5),
                      col.regions=col.fews,
                      xlab='', ylab='', 
                      margin=F, 
                      main='Observed change in IPC Phases',
                      maxpixels=1.5e6,
                      scales=list(draw=FALSE),
                      colorkey=list(labels=list(cex=2, labels=c('-1', '0', '1', '2', '3', '4')), space="bottom", height=0.5)) + 
  layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

png('G:/My Drive/Dissertation/Final Maps/EastAfrica.png', 
    width=1000, height=500)
print(grid.arrange(modpred, fewspred, ncol=2))
dev.off()



########################################
#South Africa Drought 2016
########################################
dry <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/Dry2016.tif')
dif <- raster('G://My Drive/DHS Spatial Covars/FEWS Validation/Difference Rasters/SA_201201_CS-SA_201602_CS.tifGTiff.grd')

countries <- c('Zambia', 'Malawi', 'Zimbabwe', 'Mozambique')

col.mod <- c(colorRampPalette(c("#780000", "#dc0000", "#fd8c00", "#fdc500"))(29), "#DDDDDD") 
col.fews <- c("#DDDDDD", "#DDDDDD", "#fd8c00", "#dc0000", "#780000")

drycrop <- crop(dry, sp[sp$SOVEREIGNT %in% countries, ])
difcrop <- crop(dif, sp[sp$SOVEREIGNT %in% countries, ])

modpred <- levelplot(drycrop, xlim=c(21, 41), ylim=c(-27, -8),
                     at=seq(-0.4, 0, length.out = 30),
                     col.regions=col.mod,
                     xlab='', ylab='', 
                     margin=F, 
                     main="Predicted Change in Mean HAZ Scores",
                     maxpixels=1.5e6,
                     scales=list(draw=FALSE),
                     colorkey=list(labels=list(cex=2), space="bottom", height=0.5)) + 
  layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

fewspred <- levelplot(difcrop, xlim=c(21, 41), ylim=c(-27, -8),
                      at=c(-1.5, -0.5, 0.5, 1.5, 2.5, 3.5),
                      col.regions=col.fews,
                      xlab='', ylab='', 
                      margin=F, 
                      main='Observed change in IPC Phases',
                      maxpixels=1.5e6,
                      scales=list(draw=FALSE),
                      colorkey=list(labels=list(cex=2, labels=c('-1', '0', '1', '2', '3', '4')), space="bottom", height=0.5)) + 
  layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

png('G:/My Drive/Dissertation/Final Maps/SouthAfrica.png', 
    width=800, height=400)
print(grid.arrange(modpred, fewspred, ncol=2))
dev.off()


########################################
#Afghanistan Drought 2018
########################################
dry <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/Dry2018.tif')
dif <- raster('G://My Drive/DHS Spatial Covars/FEWS Validation/Difference Rasters/CA_201407_CS-CA_201806_CS.tifGTiff.grd')

countries <- c('Afghanistan')

col.mod <- c(colorRampPalette(c("#780000", "#dc0000", "#fd8c00", "#fdc500"))(29), "#DDDDDD") 
col.fews <- c("#DDDDDD", "#DDDDDD", "#fd8c00", "#dc0000", "#780000")

drycrop <- crop(dry, sp[sp$SOVEREIGNT %in% countries, ])
difcrop <- crop(dif, sp[sp$SOVEREIGNT %in% countries, ])

modpred <- levelplot(drycrop, xlim=c(60, 75), ylim=c(29, 39),
                     at=seq(-0.4, 0, length.out = 30),
                     col.regions=col.mod,
                     xlab='', ylab='', 
                     margin=F, 
                     main="Predicted Change in Mean HAZ Scores",
                     maxpixels=1.5e6,
                     scales=list(draw=FALSE),
                     colorkey=list(labels=list(cex=2), space="bottom", height=0.5)) + 
  layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

fewspred <- levelplot(difcrop, xlim=c(60, 75), ylim=c(29, 39),
                      at=c(-1.5, -0.5, 0.5, 1.5, 2.5, 3.5),
                      col.regions=col.fews,
                      xlab='', ylab='', 
                      margin=F, 
                      main='Observed change in IPC Phases',
                      maxpixels=1.5e6,
                      scales=list(draw=FALSE),
                      colorkey=list(labels=list(cex=2, labels=c('-1', '0', '1', '2', '3', '4')), space="bottom", height=0.5)) + 
  layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

png('G:/My Drive/Dissertation/Final Maps/Afghanistan.png', 
    width=800, height=400)
print(grid.arrange(modpred, fewspred, ncol=2))
dev.off()

