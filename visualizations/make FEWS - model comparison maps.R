library(rasterVis)
library(rgdal)
library(dplyr)
library(grid)
library(cowplot)
library(gridExtra)
library(ggplotify)

sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile',
              'ne_50m_admin_0_countries')

countries_ea <- c('Somalia', 'South Sudan', 'Sudan', 'Kenya', 'Ethiopia', 'Uganda', 'Somaliland')
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


#Use same mask as we do for our predictions, and mask our predictions as FEWS does
builtup <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2017/builtup.tif')
bare  <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2017/bare.tif')
forest <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2017/forest.tif')

dif_ea <- dif_ea*(bare < 93) #Mask bare areas
dif_ea <- dif_ea*(builtup < 20)
dif_ea <- dif_ea*((bare + forest) > 0)
dif_ea[dif_ea < 0] <- 0

dry_ea[is.na(dif_ea)] <- NA

col.mod <- c(colorRampPalette(c("#780000", "#dc0000", "#fd8c00", "#fdc500"))(36), rep("#DDDDDD", 3)) 
col.fews <- c("#DDDDDD", "#fd8c00", "#dc0000", "#780000")

drycrop_ea <- crop(dry_ea, sp[sp$SOVEREIGNT %in% countries_ea, ])
difcrop_ea <- crop(dif_ea, sp[sp$SOVEREIGNT %in% countries_ea, ])

modpred_ea <- rasterVis::levelplot(drycrop_ea, xlim=c(21.7, 52), ylim=c(-5, 22.1),
                 at=seq(-0.36, 0.02, length.out = 38),
                 col.regions=col.mod,
                 xlab='', ylab='', 
                 margin=F, 
                 main="",
                 maxpixels=1.5e5,
                 scales=list(draw=FALSE),
                 colorkey=list(labels=list(cex=1), space="bottom", height=0.75)) + 
  latticeExtra::layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  latticeExtra::layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

fewspred_ea <- rasterVis::levelplot(difcrop_ea, xlim=c(21.7, 52), ylim=c(-5, 22.1),
                      at=c(-0.5, 0.5, 1.5, 2.5, 3.5),
                      col.regions=col.fews,
                      xlab='', ylab='', 
                      margin=F, 
                      main='',
                      maxpixels=1.5e5,
                      scales=list(draw=FALSE),
                      colorkey=list(labels=list(cex=1, labels=c("", '0', "", '1', "", '2', "", '3')), space="bottom", height=0.75)) + 
  latticeExtra::layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  latticeExtra::layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

########################################
#South Africa Drought 2016
########################################
dry_sa <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/Dry2016.tif')
dif_sa <- raster('G://My Drive/DHS Spatial Covars/FEWS Validation/Difference Rasters/SA_201201_CS-SA_201602_CS.tifGTiff.grd')

dif_sa[dif_sa < 0] <- 0
dif_sa <- resample(dif_sa, dry_sa)

#Use same mask as we do for our predictions, and mask our predictions as FEWS does
builtup <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2016/builtup.tif')
bare  <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2016/bare.tif')
forest <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2016/forest.tif')

dif_sa <- dif_sa*(bare < 93) #Mask bare areas
dif_sa <- dif_sa*(builtup < 20)
dif_sa <- dif_sa*((bare + forest) > 0)
dif_sa[dif_sa < 0] <- 0

dry_sa[is.na(dif_sa)] <- NA

drycrop_sa <- crop(dry_sa, sp[sp$SOVEREIGNT %in% countries_sa, ])
difcrop_sa <- crop(dif_sa, sp[sp$SOVEREIGNT %in% countries_sa, ])

modpred_sa <- rasterVis::levelplot(drycrop_sa, xlim=c(21.84, 41), ylim=c(-27, -8),
                     at=seq(-0.4, 0, length.out = 30),
                     col.regions=col.mod,
                     xlab='', ylab='', 
                     margin=F, 
                     main="Prediced Change in HAZ Scores",
                     maxpixels=1.5e5,
                     scales=list(draw=FALSE),
                     colorkey=FALSE) + 
  latticeExtra::layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  latticeExtra::layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

fewspred_sa <- rasterVis::levelplot(difcrop_sa, xlim=c(21.84, 41), ylim=c(-27, -8),
                      at=c( -0.5, 0.5, 1.5, 2.5, 3.5),
                      col.regions=col.fews,
                      xlab='', ylab='', 
                      margin=F, 
                      main='Observed Change in IPC Phases',
                      maxpixels=1.5e5,
                      scales=list(draw=FALSE),
                      colorkey=FALSE) + 
  latticeExtra::layer(sp.polygons(sp[sp$SOVEREIGNT %in% countries, ], col="#444444", fill='transparent')) + 
  latticeExtra::layer(sp.polygons(sp[!sp$SOVEREIGNT %in% countries, ], col="#444444", fill='#BBBBBB'))

#####################
#Write
####################
tmp <- tempdir()
setwd(tmp)

#modpred_sa
png("modpred_sa.png", width=5, height=5.5, units='in', res=500)
plot(modpred_sa)
dev.off()

#modpred_ea
png("modpred_ea.png", width=5, height=5.5, units='in', res=500)
plot(modpred_ea)
dev.off()

#fewspred_ea
png("fewspred_ea.png", width=5, height=5.5, units='in', res=500)
plot(fewspred_ea)
dev.off()

#fewspred_sa
png("fewspred_sa.png", width=5, height=5.5, units='in', res=500)
plot(fewspred_sa)
dev.off()

#system() isnt working with convert, but it works if you do it manually
#So figure out where tmp is, then go there and run these commands:
#convert -trim modpred_sa.png modpred_sa.png
#convert -trim modpred_ea.png modpred_ea.png
#convert -trim fewspred_ea.png fewspred_ea.png
#convert -trim fewspred_sa.png fewspred_sa.png

modpred_sa_png <- ggdraw() + draw_image(magick::image_read("modpred_sa.png", density=600))
modpred_ea_png <- ggdraw() + draw_image(magick::image_read("modpred_ea.png", density=600))
fewspred_ea_png <- ggdraw() + draw_image(magick::image_read("fewspred_ea.png", density=600))
fewspred_sa_png <- ggdraw() + draw_image(magick::image_read("fewspred_sa.png", density=600))

png('G:/My Drive/Papers/SPEI-Malnutrition/spi-malnutrition-tex/figures/FEWSCompare.png', 
    width=10, height=11, units='in', res=500)
plot_grid(modpred_sa_png, fewspred_sa_png, 
          modpred_ea_png, fewspred_ea_png, 
          ncol=2, labels='AUTO', label_x=c(0.01, 0.01, 0.01, 0.01), label_y=c(0.94, 0.94, 0.97, 0.97), label_size=16)
dev.off()

