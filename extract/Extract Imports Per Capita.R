setwd('G://My Drive/DHS Spatial Covars/ImportsPerCapita/')

library(tidyverse)
library(countrycode)

imports <- read.csv('API_NE.IMP.GNFS.CD_DS2_en_csv_v2_10080803/API_NE.IMP.GNFS.CD_DS2_en_csv_v2_10080803.csv', skip = 4) %>%
  dplyr::select(-Country.Code, -Indicator.Name, -Indicator.Code) %>%
  gather(interview_year, imports, -Country.Name) %>%
  mutate(interview_year = as.numeric(gsub('X', '', interview_year)),
         imports_interpolated = is.na(imports),
         iso3c = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  filter(!is.na(interview_year)) %>%
  arrange(desc(interview_year)) %>%
  group_by(Country.Name) %>%
  fill(imports) %>%
  arrange(interview_year) %>%
  group_by(Country.Name) %>%
  fill(imports)

population <- read.csv('../Population/API_SP.POP.TOTL_DS2_en_csv_v2_9908626.csv', skip = 4) %>%
  dplyr::select(-Country.Code, -Indicator.Name, -Indicator.Code) %>%
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

final <- merge(population, imports) %>%
  mutate(imports_percap=imports/population)

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  mutate(country = substr(code, 1, 2)) %>%
  unique %>%
  merge(read.csv('../Global Codes and Shapefile/DHS-WB-UN Country Codes.csv'), all.x=T, all.y=F)

data$iso3c <- countrycode(data$Country.or.Area, 'country.name', 'iso3c')

all <- merge(data, final, all.x=T, all.y=F) %>%
  dplyr::select(interview_year, latitude, longitude, code, imports_percap)

write.csv(all, '../../DHS Processed/Imports_Per_Capita.csv', row.names=F)

#################
#Now make raster
#################

library(rgdal)
library(raster)

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$iso3c <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

final <- final %>%
  filter(interview_year == 2017 & !is.na(iso3c))

sp <- sp::merge(sp, final)

sp$imports_percap[sp$ADMIN == 'Western Sahara'] <- sp$imports_percap[sp$ADMIN == 'Morocco']
sp$imports_percap[sp$ADMIN == 'Siachen Glacier'] <- sp$imports_percap[sp$ADMIN == 'Pakistan']
sp$imports_percap[sp$ADMIN == 'North Korea'] <- 0

#Need to decide on scale and make a template raster
r <- raster('../Final Rasters/irrigation.tif')

finalrast <- rasterize(sp, r, field="imports_percap", fun='mean', na.rm=TRUE, filename='../Final Rasters/imports_percap.tif', driver='GTiff', overwrite=T)
