setwd('G://My Drive/DHS Spatial Covars/PrimaryEnrollment/')

library(tidyverse)
library(countrycode)

options(stringsAsFactors = F)

enrollment <- read.csv('API_SE.PRM.NENR_DS2_en_csv_v2_10182806.csv', skip = 4) %>%
  dplyr::select(-Country.Code, -Indicator.Name, -Indicator.Code) %>%
  mutate(X2018=NA, X2019=NA, X2020=NA) %>% 
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

enrollment$enrollment[enrollment$Country.Name=="South Sudan" & enrollment$interview_year < 2011] <- enrollment$enrollment[enrollment$Country.Name=="Sudan" & enrollment$interview_year < 2011]
enrollment$enrollment[enrollment$Country.Name=="Timor-Leste" & enrollment$interview_year < 2002] <- enrollment$enrollment[enrollment$Country.Name=="Indonesia" & enrollment$interview_year < 2002]

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
#Now make rasters
#################

library(rgdal)
library(raster)

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$iso3c <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

#Need to decide on scale and make a template raster
r <- raster('../Final Rasters/irrigation.tif')

for (year in seq(1990, 2020)){
  dat <- enrollment %>%
    filter(interview_year==year & !is.na(iso3c))
  
  spres <- sp::merge(sp, dat)
  
  spres$enrollment[spres$ADMIN == 'Western Sahara'] <- spres$enrollment[spres$ADMIN == 'Morocco']
  spres$enrollment[spres$ADMIN == 'Bosnia and Herzegovina'] <- spres$enrollment[spres$ADMIN == 'Republic of Serbia']
  spres$enrollment[spres$ADMIN == 'Czechia'] <- spres$enrollment[spres$ADMIN == 'Austria']
  spres$enrollment[spres$ADMIN == 'Slovakia'] <- spres$enrollment[spres$ADMIN == 'Austria']
  spres$enrollment[spres$ADMIN == 'Kosovo'] <- spres$enrollment[spres$ADMIN == 'Republic of Serbia']
  spres$enrollment[spres$ADMIN == 'Siachen Glacier'] <- spres$enrollment[spres$ADMIN == 'Pakistan']
  spres$enrollment[spres$ADMIN == 'Taiwan'] <- spres$enrollment[spres$ADMIN == 'China']
  spres$enrollment[spres$ADMIN == 'Turkmenistan'] <- spres$enrollment[spres$ADMIN == 'Uzbekistan']
  
  rasterize(spres, r, field="enrollment", fun='mean', na.rm=TRUE, filename=paste0('../Final Rasters/', year, '/enrollment.tif'), driver='GTiff', overwrite=T)

  print(year)
}