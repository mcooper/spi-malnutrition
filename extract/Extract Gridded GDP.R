library(ncdf4)
library(raster)
library(dplyr)

gdp <- nc_open('G://My Drive/DHS Spatial Covars/Gridded GDP and HDI/GDP_per_capita_PPP_1990_2015_v2.nc')

gdpvar <- ncvar_get(gdp, "GDP_per_capita_PPP")
lat <- ncvar_get(gdp, 'latitude')
long <- ncvar_get(gdp, 'longitude')

ref <- raster('G://My Drive/CHIRPS/Monthly/chirps-v2.0.1981.01.tif')

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  unique

newdata <- data.frame()
for (y in 1:26){
  year <- y + 1989
  
  print(year)
  
  if (year == 1990){
    sel <- data %>%
      filter(interview_year <= year)
  } else if(year == 2015){
    sel <- data %>%
      filter(interview_year >= year)
  } else{
    sel <- data %>%
      filter(interview_year == year)
  }
  
  slice <- gdpvar[ , , y]
  
  r <- raster(t(slice), xmn=-180, xmx=180, ymn=-90, ymx=90,
              crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  
  r <- resample(r, ref)
  
  r <- focal(r, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
  
  selsp <- SpatialPoints(sel[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
  
  sel$grid_gdp <- extract(r, selsp)
  
  newdata <- bind_rows(newdata, sel)
}

#Looks like there are NAs in San Andres and Providencia in Colombia
#I will assume that Providencia has the same GDP as San Andres
newdata$grid_gdp[is.na(newdata$grid_gdp)] <- 13784.34

write.csv(newdata, 'G://My Drive/DHS Spatial Covars/Gridded GDP and HDI/grid_gdp.csv', row.names=F)

for (year in seq(1990, 2020)){
  y <- year - 1989
  
  if (y > 26){
    y <- 26
  }
  
  slice <- gdpvar[ , , y]
  
  r <- raster(t(slice), xmn=-180, xmx=180, ymn=-90, ymx=90,
              crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  r <- resample(r, ref)
  writeRaster(r, paste0('G://My Drive/DHS Spatial Covars/Final Rasters/', year, '/grid_gdp.tif'), format='GTiff', overwrite=T)
}
