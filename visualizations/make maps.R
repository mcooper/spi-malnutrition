library(rasterVis)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(dplyr)

dry <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/Dry.tif')
wet <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/Wet.tif')

sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile',
              'ne_50m_admin_0_countries')

##################
#Drought
###################
col.l <- c(colorRampPalette(c("#780000", "#dc0000", "#fd8c00", "#fdc500"))(29), "#DDDDDD") 

levelplot(dry, xlim=c(-100, 150), ylim=c(-40, 50), col.regions=col.l,
          xlab='', ylab='', 
          margin=F, 
          main="Expected Change in Mean HAZ Scores Under Drought",
          maxpixels=1.5e6) + 
  layer(sp.polygons(sp))

#################
#Too much rain
#################
col.l <- c(colorRampPalette(c("#15719f", "#528ab4", "#62a1c7", "#7bc7dd", "#95d6ea"))(29), "#DDDDDD") 

levelplot(wet, xlim=c(-100, 150), ylim=c(-40, 50), col.regions=col.l,
          xlab='', ylab='', 
          margin=F, 
          main="Expected Change in Mean HAZ Scores Under Excessive Rainfall",
          maxpixels=1.5e6) + 
  layer(sp.polygons(sp))

##################
#Show points
###################
This is too hard apparently


data <- read.csv('G://My Drive/DHS Processed/PrecipIndices.csv') %>%
  group_by(latitude, longitude) %>%
  summarize(spei24=mean(spei24))

data$spei <- ifelse(data$spei24 > 1.5, "Wet",
                    ifelse(data$spei24 < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

spdat <- SpatialPointsDataFrame(coords=data[ , c('longitude', 'latitude')],
                                data=data, proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

my.palette <- c("#222222", "#780000", "#15719f")

plt <- spplot(spdat, "spei", col.regions = my.palette, 
       cex = 0.1, 
       main = list(label="DHS Points Under Drought, Normal Conditions, and Excessive Rainfall", cex=5),
       sp.layout=list('sp.polygons', sp, fill="#DDDDDD"))


plt$legend$bottom$args$key$points$cex <- c(2,2,2)

plot(plt)
