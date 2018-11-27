setwd('G://My Drive/DHS Spatial Covars/DevelopmentAssistance/')

library(tidyverse)
library(countrycode)

options(stringsAsFactors = F)

assistance <- read.csv('API_DT.ODA.ODAT.PC.ZS_DS2_en_csv_v2_10226222.csv', skip = 4) %>%
  dplyr::select(-Country.Code, -Indicator.Name, -Indicator.Code) %>%
  mutate(X2018=NA, X2019=NA, X2020=NA) %>% 
  gather(interview_year, assistance, -Country.Name) %>%
  mutate(interview_year = as.numeric(gsub('X', '', interview_year)),
         assistance_interpolated = is.na(assistance),
         Country.Name = ifelse(Country.Name=='Eswatini', 'Swaziland', Country.Name),
         iso3c = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  filter(!is.na(interview_year)) %>%
  arrange(desc(interview_year)) %>%
  group_by(Country.Name) %>%
  fill(assistance) %>%
  arrange(interview_year) %>%
  group_by(Country.Name) %>%
  fill(assistance)

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  mutate(country = substr(code, 1, 2)) %>%
  unique %>%
  merge(read.csv('../Global Codes and Shapefile/DHS-WB-UN Country Codes.csv'), all.x=T, all.y=F)

data$iso3c <- countrycode(data$Country.or.Area, 'country.name', 'iso3c')

all <- merge(data, assistance, all.x=T, all.y=F) %>%
  dplyr::select(interview_year, latitude, longitude, code, assistance)

write.csv(all, '../../DHS Processed/Assistance.csv', row.names=F)

#################
#Now make rasters
#################

library(rgdal)
library(raster)

assistance$assistance[assistance$Country.Name == 'South Sudan' & assistance$interview_year < 2011] <- assistance$assistance[assistance$Country.Name == 'Sudan' & assistance$interview_year < 2011]

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$iso3c <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

#Need to decide on scale and make a template raster
r <- raster('../Final Rasters/irrigation.tif')

for (year in seq(1990, 2020)){
  dat <- assistance %>%
    filter(interview_year==year & !is.na(iso3c))
  
  spres <- sp::merge(sp, dat)
  
  spres$assistance[is.na(spres$assistance)] <- 0
  
  rasterize(spres, r, field="assistance", fun='mean', na.rm=TRUE, filename=paste0('../Final Rasters/', year, '/assistance.tif'), driver='GTiff', overwrite=T)
  
  print(year)
}