setwd('G://My Drive/DHS Spatial Covars/World Governance Indicators/')

library(tidyverse)
library(countrycode)

#Data from world bank world governance indicators

#Political Stability and Absence of Violence/Terrorism: Estimate
#Government Effectiveness: Estimate

wgi <- read.csv('WGIData.csv') %>%
  dplyr::select(-Country.Code, -Indicator.Name) %>%
  filter(Indicator.Code %in% c('GE.EST', 'PV.EST')) %>%
  gather(Year, Score, -ï..Country.Name, -Indicator.Code) %>%
  spread(Indicator.Code, Score) %>%
  mutate(Year = as.numeric(gsub('X', '', Year))) %>%
  dplyr::select(ï..Country.Name, interview_year=Year, government_effectiveness=GE.EST,
         stability_violence=PV.EST)

temp <- expand.grid(unique(wgi$ï..Country.Name), seq(1988, 2016))
names(temp) <- c("ï..Country.Name", "interview_year")

wgi <- merge(wgi, temp, all=T) %>%  
  mutate(Country.Code = countrycode(ï..Country.Name, 'country.name', 'iso3c'),
         wgi_impute = is.na(stability_violence)) %>%
  arrange(desc(interview_year)) %>%
  group_by(Country.Code) %>%
  fill(government_effectiveness, stability_violence)

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  mutate(country = substr(code, 1, 2)) %>%
  unique %>%
  merge(read.csv('../Global Codes and Shapefile/DHS-WB-UN Country Codes.csv'), all.x=T, all.y=F)

data$Country.Code <- countrycode(data$Country.or.Area, 'country.name', 'iso3c')

all <- merge(data, wgi, all.x=T, all.y=F) %>%
  dplyr::select(interview_year, latitude, longitude, code, government_effectiveness, stability_violence, wgi_impute)

write.csv(all, '../../DHS Processed/WorldGovernanceIndicators.csv', row.names=F)

#############################################################
#Now Create a Raster
######################################################

library(rgdal)
library(raster)

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$SOVEREIGNT_ISO_3C <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

wgi <- wgi %>%
  filter(interview_year == 2016) %>%
  mutate(SOVEREIGNT_ISO_3C = countrycode(ï..Country.Name, origin='country.name', destination = "iso3c")) %>%
  filter(!is.na(SOVEREIGNT_ISO_3C))

sp <- sp::merge(sp, wgi)

sp$government_effectiveness[which(sp$SOVEREIGNT_ISO_3C == 'ESH')] <- sp$government_effectiveness[which(sp$SOVEREIGNT_ISO_3C == 'MAR')]
sp$stability_violence[which(sp$SOVEREIGNT_ISO_3C == 'ESH')] <- sp$stability_violence[which(sp$SOVEREIGNT_ISO_3C == 'MAR')]

sp$government_effectiveness[which(sp$ADMIN == 'Kosovo')] <- sp$government_effectiveness[which(sp$SOVEREIGNT_ISO_3C == 'SRB')]
sp$stability_violence[which(sp$ADMIN == 'Kosovo')] <- sp$stability_violence[which(sp$SOVEREIGNT_ISO_3C == 'SRB')]

sp$government_effectiveness[which(sp$ADMIN == 'Siachen Glacier')] <- sp$government_effectiveness[which(sp$SOVEREIGNT_ISO_3C == 'PAK')]
sp$stability_violence[which(sp$ADMIN == 'Siachen Glacier')] <- sp$stability_violence[which(sp$SOVEREIGNT_ISO_3C == 'PAK')]


#Need to decide on scale and make a template raster
r <- raster('../Final Rasters/irrigation.tif')

rasterize(sp, r, field="government_effectiveness", fun='mean', na.rm=TRUE, filename='../Final Rasters/government_effectiveness.tif', driver='GTiff', overwrite=TRUE)
rasterize(sp, r, field="stability_violence", fun='mean', na.rm=TRUE, filename='../Final Rasters/stability_violence.tif', driver='GTiff', overwrite=TRUE)

