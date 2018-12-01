setwd('G://My Drive/DHS Spatial Covars/FAOSTAT/')

library(tidyverse)
library(countrycode)

crops <- read.csv('Production_Crops_E_All_Data.csv') %>%
  filter(Element=="Production" & Item %in% c('Cereals,Total', 'Roots and Tubers,Total')) %>%
  select(Country=Area, Item, Y1988, Y1989, Y1990, Y1991, Y1992, Y1993, Y1994, Y1995, Y1996, Y1997, Y1998, Y1999, 
         Y2000, Y2001, Y2002, Y2003, Y2004, Y2005, Y2006, Y2007, Y2008, Y2009, Y2010, Y2011, 
         Y2012, Y2013, Y2014, Y2015, Y2016) %>%
  mutate(Y2017=NA, Y2018=NA, Y2019=NA, Y2020=NA) %>%
  gather(interview_year, crop_prod, -Country, -Item) %>%
  group_by(Country, interview_year) %>%
  summarize(crop_prod=sum(crop_prod, na.rm=T))

crops$crop_prod[crops$crop_prod==0] <- NA

crops <- crops %>%
  mutate(interview_year=as.numeric(substr(interview_year, 2, 5))) %>%
  mutate(iso3c = countrycode(Country, 'country.name', 'iso3c')) %>%
  filter(!is.na(iso3c) & !(Country %in% c("Sudan (former)", "China", "Ethiopia PDR", "USSR"))) %>%
  arrange(desc(interview_year)) %>%
  group_by(Country) %>%
  fill(crop_prod) %>%
  arrange(interview_year) %>%
  group_by(Country) %>%
  fill(crop_prod)

population <- read.csv('../Population/API_SP.POP.TOTL_DS2_en_csv_v2_9908626.csv', skip = 4) %>%
  dplyr::select(-Country.Code, -Indicator.Name, -Indicator.Code) %>%
  mutate(X2018=NA, X2019=NA, X2020=NA) %>% 
  gather(interview_year, population, -Country.Name) %>%
  mutate(interview_year = as.numeric(gsub('X', '', interview_year)),
         population_interpolated = is.na(population),
         iso3c = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  filter(!is.na(interview_year)) %>%
  arrange(desc(interview_year)) %>%
  group_by(Country.Name) %>%
  fill(population) %>%
  arrange(interview_year) %>%
  group_by(Country.Name) %>%
  fill(population)

final <- merge(population, crops) %>%
  mutate(crop_prod=crop_prod/population)

final$crop_prod[final$Country.Name=="South Sudan" & final$interview_year < 2012] <- final$crop_prod[final$Country.Name=="Sudan" & final$interview_year < 2012]
final$crop_prod[final$Country.Name=="Timor-Leste" & final$interview_year < 2002] <- final$crop_prod[final$Country.Name=="Indonesia" & final$interview_year < 2002]

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  mutate(country = substr(code, 1, 2)) %>%
  unique %>%
  merge(read.csv('../Global Codes and Shapefile/DHS-WB-UN Country Codes.csv'), all.x=T, all.y=F)

data$iso3c <- countrycode(data$Country.or.Area, 'country.name', 'iso3c')

all <- merge(data, final, all.x=T, all.y=F) %>%
  dplyr::select(interview_year, latitude, longitude, code, crop_prod)

write.csv(all, '../../DHS Processed/Crop_production.csv', row.names=F)

#################
#Now make raster
#################

library(rgdal)
library(raster)

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$iso3c <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

for (year in seq(1990, 2020)){
  dat <- final %>%
    filter(interview_year == year & !is.na(iso3c))
  
  spres <- sp::merge(sp, dat)
  
  spres$imports_percap[spres$ADMIN == 'Western Sahara'] <- spres$imports_percap[spres$ADMIN == 'Morocco']
  spres$imports_percap[spres$ADMIN == 'Siachen Glacier'] <- spres$imports_percap[spres$ADMIN == 'Pakistan']
  
  #Need to decide on scale and make a template raster
  r <- raster('../Final Rasters/irrigation.tif')
  
  finalrast <- rasterize(spres, r, field="crop_prod", fun='mean', na.rm=TRUE, filename=paste0('../Final Rasters/', year, '/crop_prod.tif'), driver='GTiff', overwrite=T)
  
  print(year)
}