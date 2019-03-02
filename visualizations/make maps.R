library(rasterVis)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(dplyr)

#########################
#Read Data
########################

sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile',
              'ne_50m_admin_0_countries')

spt <- spTransform(sp, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

data <- read.csv('G://My Drive/DHS Processed/PrecipIndices.csv') %>%
  group_by(latitude, longitude) %>%
  summarize(spei24=mean(spei24))

data$spei <- ifelse(data$spei24 > 1.5, "Wet",
                    ifelse(data$spei24 < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

spdat <- SpatialPointsDataFrame(coords=data[ , c('longitude', 'latitude')],
                                data=data, proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

spdat_t <- spTransform(spdat, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

spdat_t@data[ , c('longitude', 'latitude')] <- spdat_t@coords


final <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/Final_Raster.tif')
final[final < -0.4] <- -0.4

final <- projectRaster(final, crs=CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

####################
#Make Images
####################

setwd('G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/figures')

##################
#DHS Points
###################

plt2 <- spplot(spdat_t, "spei", 
               col.regions = c("#780000", "#780000", "#780000"), 
               cex = 0.1, 
               sp.layout=list('sp.polygons', spt, fill="#DDDDDD"))

plt2$legend$bottom$args$key$text[[1]] <- c("", "", "")
plt2$legend$bottom$args$key$points$cex <- c(0,0,0)

pdf("DHSPoints.pdf", width=8, height=6)
plot(plt2)
dev.off()

system("pdfcrop DHSPoints.pdf DHSPoints.pdf")


##################
#Drought
###################

col.l <- c(colorRampPalette(c("#780000", "#dc0000", "#fd8c00", "#fdc500", "#DDDDDD"))(29))

plt <- levelplot(final, xlim=c(-10900000, 15750000), ylim=c(-5351704, 5353646), col.regions=col.l,
          xlab='', ylab='', 
          margin=F, 
          #main=list(label="Expected Change in Mean HAZ Scores Under Drought (SPEI < -0.4)", cex=3),
          maxpixels=1.5e6,
          scales=list(draw=FALSE),
          colorkey=list(labels=list(cex=2), space="bottom", height=0.5)) + 
  layer(sp.polygons(spt, col="#444444"))

pdf("DroughtVulnerability.pdf", width=12, height=9)
plot(plt)
dev.off()

system("pdfcrop DroughtVulnerability.pdf DroughtVulnerability.pdf")
