setwd('G://My Drive/DHS Spatial Covars/PrimaryEnrollment/')

library(tidyverse)
library(countrycode)

options(stringsAsFactors = F)

enrollment <- read.csv('API_SE.PRM.NENR_DS2_en_csv_v2_10182806.csv', skip = 4) %>%
  dplyr::select(-Country.Code, -Indicator.Name, -Indicator.Code) %>%
  gather(interview_year, enrollment, -Country.Name) %>%
  mutate(interview_year = as.numeric(gsub('X', '', interview_year)),
         enrollment_interpolated = is.na(enrollment),
         Country.Name = ifelse(Country.Name=='Eswatini', 'Swaziland', Country.Name),
         iso3c = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  filter(!is.na(interview_year)) %>%
  arrange(desc(interview_year)) %>%
  group_by(Country.Name) %>%
  fill(enrollment) %>%
  arrange(interview_year) %>%
  group_by(Country.Name) %>%
  fill(enrollment)

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  mutate(country = substr(code, 1, 2)) %>%
  unique %>%
  merge(read.csv('../Global Codes and Shapefile/DHS-WB-UN Country Codes.csv'), all.x=T, all.y=F)

data$iso3c <- countrycode(data$Country.or.Area, 'country.name', 'iso3c')

all <- merge(data, enrollment, all.x=T, all.y=F) %>%
  dplyr::select(interview_year, latitude, longitude, code, enrollment)

write.csv(all, '../../DHS Processed/Enrollment.csv', row.names=F)

#################
#Now make raster
#################

library(rgdal)
library(raster)

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$iso3c <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

final <- enrollment %>%
  filter(interview_year == 2017 & !is.na(iso3c))

sp <- sp::merge(sp, final)

sp$enrollment[sp$ADMIN == 'Western Sahara'] <- sp$enrollment[sp$ADMIN == 'Morocco']
sp$enrollment[sp$ADMIN == 'Bosnia and Herzegovina'] <- sp$enrollment[sp$ADMIN == 'Republic of Serbia']
sp$enrollment[sp$ADMIN == 'Czechia'] <- sp$enrollment[sp$ADMIN == 'Austria']
sp$enrollment[sp$ADMIN == 'Slovakia'] <- sp$enrollment[sp$ADMIN == 'Austria']
sp$enrollment[sp$ADMIN == 'Kosovo'] <- sp$enrollment[sp$ADMIN == 'Republic of Serbia']
sp$enrollment[sp$ADMIN == 'Siachen Glacier'] <- sp$enrollment[sp$ADMIN == 'Pakistan']
sp$enrollment[sp$ADMIN == 'Taiwan'] <- sp$enrollment[sp$ADMIN == 'China']
sp$enrollment[sp$ADMIN == 'Turkmenistan'] <- sp$enrollment[sp$ADMIN == 'Uzbekistan']

#Need to decide on scale and make a template raster
r <- raster('../Final Rasters/irrigation.tif')

finalrast <- rasterize(sp, r, field="enrollment", fun='mean', na.rm=TRUE, filename='../Final Rasters/enrollment.tif', driver='GTiff', overwrite=T)
