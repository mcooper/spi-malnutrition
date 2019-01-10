library(rgdal)
library(dplyr)
library(raster)
library(rgeos)

yearlag <- 3

admin1 <- readOGR('.', 'gadm36_1') %>%
  spTransform(CRS("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

admin1$area_sqkm <- area(admin1)/1000000

data <- read.csv('ged181.csv')

sp <- SpatialPointsDataFrame(data[ , c('longitude', 'latitude')], data,
                             proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  spTransform(CRS("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

ref <- raster('mean_annual_precip.tif')

for (y in seq(2017, (1989 + yearlag))){
  print(y)
  
  sel <- sp[(sp$year <= y) & (sp$year > (y-yearlag)), ]
  
  pb <- txtProgressBar(1, nrow(admin1), style=3)
  
  for (i in 1:nrow(admin1)){
    
    poly <- admin1[i, ]
    
    points <- gContains(poly, sel, byid=T)
    
    admin1@data[i, as.character(y)] <- sum(sel@data[points[ , ], 'best'])
    
    setTxtProgressBar(pb, i)
  }
  
}

admin1 <- spTransform(admin1, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

admin1@data <- admin1@data %>%
  dplyr::select(-VARNAME_1, -NL_NAME_1, -TYPE_1, -ENGTYPE_1, -CC_1, -HASC_1) %>%
  mutate(`2017`=`2017`/area_sqkm, `2016`=`2016`/area_sqkm, `2015`=`2015`/area_sqkm, `2014`=`2014`/area_sqkm, 
         `2013`=`2013`/area_sqkm, `2012`=`2012`/area_sqkm, `2011`=`2011`/area_sqkm, `2010`=`2010`/area_sqkm, 
         `2009`=`2009`/area_sqkm, `2008`=`2008`/area_sqkm, `2007`=`2007`/area_sqkm, `2006`=`2006`/area_sqkm, 
         `2005`=`2005`/area_sqkm, `2004`=`2004`/area_sqkm, `2003`=`2003`/area_sqkm, `2002`=`2002`/area_sqkm, 
         `2001`=`2001`/area_sqkm, `2000`=`2000`/area_sqkm, `1999`=`1999`/area_sqkm, `1998`=`1998`/area_sqkm, 
         `1997`=`1997`/area_sqkm, `1996`=`1996`/area_sqkm, `1995`=`1995`/area_sqkm, `1994`=`1994`/area_sqkm, 
         `1993`=`1993`/area_sqkm, `1992`=`1992`/area_sqkm)

writeOGR(admin1, '.', 'admin1_data', driver='ESRI Shapefile', overwrite=T)

for (y in seq(2017, (1989 + yearlag))){
  print(y)
  
  admin1@data$new <- admin1@data[ , as.character(y)]/admin1@data$area_sqkm
  
  rasterize(admin1, ref, field='new', filename=paste0('bodycount2', y, '.tif'), format='GTiff', overwrite=T)
  
}

sp <- read.csv('~/dhsprocessed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  unique

all <- data.frame()
for (y in seq(1992, 2016)){
  print(y)
  
  if (y == 1992){
    sel <- sp %>%
      filter(interview_year <= 1992)
  } else{
    sel <- sp %>%
      filter(interview_year == y)
  }
  r <- raster(paste0('bodycount2', y, '.tif'))
  sel <- SpatialPointsDataFrame(sel[ , c('longitude', 'latitude')], data=sel)
  sel$bodycount <- extract(r, sel)
  all <- bind_rows(all, sel@data)
  print(y)
}

write.csv(all, '~/dhsprocessed/bodycount.csv', row.names=F)



