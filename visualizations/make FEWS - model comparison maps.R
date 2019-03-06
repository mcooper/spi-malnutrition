library(rasterVis)
library(rgdal)
library(dplyr)
library(gridExtra)


sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile',
              'ne_50m_admin_0_countries')

countries_ea <- c('Somalia', 'South Sudan', 'Sudan', 'Kenya', 'Ethiopia', 'Uganda')
countries_sa <- c('Zambia', 'Malawi', 'Zimbabwe', 'Mozambique')
countries_ca <- c('Afghanistan')
countries <- c(countries_ea, countries_sa, countries_ca)

########################################
#East Africa Drought 2017
########################################
dry_ea <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/Dry2017.tif')
dif_ea <- raster('G://My Drive/DHS Spatial Covars/FEWS Validation/Difference Rasters/EA_201307_CS-EA_201706_CS.tifGTiff.gri')
dif_ea[dif_ea < 0] <- 0

dif_ea <- resample(dif_ea, dry_ea)

col.mod <- c(colorRampPalette(c("#780000", "#dc0000", "#fd8c00", "#fdc500"))(36), rep("#DDDDDD", 3)) 
col.fews <- c("#DDDDDD", "#fd8c00", "#dc0000", "#780000")

drycrop_ea <- crop(dry_ea, sp[sp$SOVEREIGNT %in% countries_ea, ])
difcrop_ea <- crop(dif_ea, sp[sp$SOVEREIGNT %in% countries_ea, ])

modpred_ea <- levelplot(drycrop_ea, xlim=c(21, 52), ylim=c(-5, 24),
                 at=seq(-0.36, 0.02, length.out = 38),
                 col.regions=col.mod,
                 xlab='', ylab='', 
                 margin=F, 
                 main="",
                 maxpixels=1.5e5,
                 scales=list(draw=FALSE),
                 colorkey=list(labels=list(cex=1), space="bottom", height=0.75)) + 
  layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

fewspred_ea <- levelplot(difcrop_ea, xlim=c(21, 52), ylim=c(-5, 24),
                      at=c(-0.5, 0.5, 1.5, 2.5, 3.5),
                      col.regions=col.fews,
                      xlab='', ylab='', 
                      margin=F, 
                      main='',
                      maxpixels=1.5e5,
                      scales=list(draw=FALSE),
                      colorkey=list(labels=list(cex=1, labels=c("", '0', "", '1', "", '2', "", '3')), space="bottom", height=0.75)) + 
  layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

########################################
#South Africa Drought 2016
########################################
dry_sa <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/Dry2016.tif')
dif_sa <- raster('G://My Drive/DHS Spatial Covars/FEWS Validation/Difference Rasters/SA_201201_CS-SA_201602_CS.tifGTiff.grd')
dif_sa[dif_sa < 0] <- 0

dif_sa <- resample(dif_sa, dry_sa)

drycrop_sa <- crop(dry_sa, sp[sp$SOVEREIGNT %in% countries_sa, ])
difcrop_sa <- crop(dif_sa, sp[sp$SOVEREIGNT %in% countries_sa, ])

modpred_sa <- levelplot(drycrop_sa, xlim=c(21, 41), ylim=c(-27, -8),
                     at=seq(-0.4, 0, length.out = 30),
                     col.regions=col.mod,
                     xlab='', ylab='', 
                     margin=F, 
                     main="Prediced Change in HAZ Scores",
                     maxpixels=1.5e5,
                     scales=list(draw=FALSE),
                     colorkey=FALSE) + 
  layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

fewspred_sa <- levelplot(difcrop_sa, xlim=c(21, 41), ylim=c(-27, -8),
                      at=c( -0.5, 0.5, 1.5, 2.5, 3.5),
                      col.regions=col.fews,
                      xlab='', ylab='', 
                      margin=F, 
                      main='Observed Change in IPC Phases',
                      maxpixels=1.5e5,
                      scales=list(draw=FALSE),
                      colorkey=FALSE) + 
  layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))


########################################
#Afghanistan Drought 2018
########################################
# dry_ca <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/Dry2018.tif')
# dif_ca <- raster('G://My Drive/DHS Spatial Covars/FEWS Validation/Difference Rasters/CA_201407_CS-CA_201806_CS.tifGTiff.grd')
# 
# drycrop_ca <- crop(dry_ca, sp[sp$SOVEREIGNT %in% countries_ca, ])
# difcrop_ca <- crop(dif_ca, sp[sp$SOVEREIGNT %in% countries_ca, ])
# 
# modpred_ca <- levelplot(drycrop_ca, xlim=c(60, 75), ylim=c(29, 39),
#                      at=seq(-0.4, 0, length.out = 30),
#                      col.regions=col.mod,
#                      xlab='', ylab='', 
#                      margin=F, 
#                      main="Afghanistan",
#                      maxpixels=1.5e6,
#                      scales=list(draw=FALSE),
#                      colorkey=FALSE) + 
#   layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
#   layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))
# 
# fewspred_ca <- levelplot(difcrop_ca, xlim=c(60, 75), ylim=c(29, 39),
#                       at=c(-1.5, -0.5, 0.5, 1.5, 2.5, 3.5),
#                       col.regions=col.fews,
#                       xlab='', ylab='', 
#                       margin=F, 
#                       main='',
#                       maxpixels=1.5e6,
#                       scales=list(draw=FALSE),
#                       colorkey=FALSE) + 
#   layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
#   layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))
# 


pdf('G:/My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/figures/FEWSCompare.pdf', 
    width=10, height=11)
grid.arrange(modpred_sa, fewspred_sa, modpred_ea, fewspred_ea, ncol=2)
dev.off()
