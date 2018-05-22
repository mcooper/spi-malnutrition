setwd('G://My Drive/DHS Spatial Covars/Ag as a perc of GDP/')

library(tidyverse)
library(countrycode)

ag <- read.csv('API_NV.AGR.TOTL.ZS_DS2_en_csv_v2_9911168.csv', skip = 4) %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code) %>%
  gather(interview_year, ag_pct_gdp, -Country.Name) %>%
  mutate(interview_year = as.numeric(gsub('X', '', interview_year)),
         ag_pct_gdp_interpolated = is.na(ag_pct_gdp),
         iso3c = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  filter(!is.na(interview_year)) %>%
  arrange(desc(interview_year)) %>%
  group_by(Country.Name) %>%
  fill(ag_pct_gdp) %>%
  arrange(interview_year) %>%
  group_by(Country.Name) %>%
  fill(ag_pct_gdp)

#missing data for Haiti, North Korea
#Will use CIA world factbooks 2017 estimate.
ag$ag_pct_gdp[ag$Country.Name == 'Haiti'] <- 21.9
ag$ag_pct_gdp[ag$Country.Name == 'Korea, Dem. Peopleâ???Ts Rep.'] <- 25.4
ag$ag_pct_gdp[ag$Country.Name == 'South Sudan'] <- 39 #Estimate for Sudan? IDK what else to use

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  select(latitude, longitude, code, interview_year) %>%
  mutate(country = substr(code, 1, 2)) %>%
  unique %>%
  merge(read.csv('../Global Codes and Shapefile/DHS-WB-UN Country Codes.csv'), all.x=T, all.y=F)

data$iso3c <- countrycode(data$Country.or.Area, 'country.name', 'iso3c')

all <- merge(data, ag, all.x=T, all.y=F) %>%
  select(interview_year, latitude, longitude, code, ag_pct_gdp, ag_pct_gdp_interpolated)

write.csv(all, '../../DHS Processed/WorldGovernanceIndicators.csv', row.names=F)

#################
#Now make raster
#################

library(rgdal)
library(raster)

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$iso3c <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

ag <- ag %>%
  filter(interview_year == 2016 & !is.na(iso3c))
  
sp <- sp::merge(sp, ag)

sp$ag_pct_gdp[sp$ADMIN == 'Comoros'] <- 49.5 #CIA
sp$ag_pct_gdp[sp$ADMIN == 'Western Sahara'] <- sp$ag_pct_gdp[sp$ADMIN == 'Morocco']
sp$ag_pct_gdp[sp$ADMIN == 'Taiwan'] <- 1.8

#Need to decide on scale and make a template raster
r <- raster('templateRaster.tif')

rasterize(sp, r, field="ag_pct_gdp", fun='mean')










