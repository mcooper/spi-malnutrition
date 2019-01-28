setwd('G://My Drive/DHS Spatial Covars/ImportsPerCapita/')

library(tidyverse)
library(countrycode)

imports <- read.csv('API_NE.IMP.GNFS.CD_DS2_en_csv_v2_10080803/API_NE.IMP.GNFS.CD_DS2_en_csv_v2_10080803.csv', skip = 4) %>%
  dplyr::select(-Country.Code, -Indicator.Name, -Indicator.Code) %>%
  mutate(X2018=NA, X2019=NA, X2020=NA) %>% 
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

final <- merge(population, imports) %>%
  mutate(imports_percap=imports/population)

final$imports_percap[final$Country.Name=="South Sudan" & final$interview_year < 2011] <- final$imports_percap[final$Country.Name=="Sudan" & final$interview_year < 2011]
final$imports_percap[final$Country.Name=="Timor-Leste" & final$interview_year < 2002] <- final$imports_percap[final$Country.Name=="Indonesia" & final$interview_year < 2002]

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

for (year in seq(1990, 2020)){
  dat <- final %>%
    filter(interview_year == year & !is.na(iso3c))
  
  spres <- sp::merge(sp, dat)
  
  spres$imports_percap[spres$ADMIN == 'Western Sahara'] <- spres$imports_percap[spres$ADMIN == 'Morocco']
  spres$imports_percap[spres$ADMIN == 'Siachen Glacier'] <- spres$imports_percap[spres$ADMIN == 'Pakistan']
  spres$imports_percap[spres$ADMIN == 'North Korea'] <- 0
  spres$imports_percap[spres$ADMIN == 'Luxembourg'] <- spres$imports_percap[spres$ADMIN == 'Switzerland']
  spres$imports_percap[spres$ADMIN == 'Andorra'] <- spres$imports_percap[spres$ADMIN == 'France']
  spres$imports_percap[spres$ADMIN == 'Taiwan'] <- spres$imports_percap[spres$ADMIN == 'China']
  spres$imports_percap[spres$ADMIN == 'Kosovo'] <- spres$imports_percap[spres$ADMIN == 'Republic of Serbia']
  
  #Need to decide on scale and make a template raster
  r <- raster('../Final Rasters/2000/irrig_aai.tif')
  
  finalrast <- rasterize(spres, r, field="imports_percap", fun='mean', na.rm=TRUE, filename=paste0('../Final Rasters/', year, '/imports_percap.tif'), driver='GTiff', overwrite=T)

  print(year)
}