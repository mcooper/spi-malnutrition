setwd('G://My Drive/DHS Spatial Covars/Ag as a perc of GDP/')

library(tidyverse)
library(countrycode)

options(stringsAsFactors = F)

ag_pct_gdp <- read.csv('API_NV.AGR.TOTL.ZS_DS2_en_csv_v2_9911168.csv', skip = 4) %>%
  dplyr::select(-Country.Code, -Indicator.Name, -Indicator.Code) %>%
  mutate(X2018=NA, X2019=NA, X2020=NA) %>% 
  gather(interview_year, ag_pct_gdp, -Country.Name) %>%
  mutate(interview_year = as.numeric(gsub('X', '', interview_year)),
         ag_pct_gdp_interpolated = is.na(ag_pct_gdp),
         Country.Name = ifelse(Country.Name=='Eswatini', 'Swaziland', Country.Name),
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
ag_pct_gdp$ag_pct_gdp[ag_pct_gdp$Country.Name == 'Haiti'] <- 21.9
ag_pct_gdp$ag_pct_gdp[ag_pct_gdp$iso3c == 'PRK'] <- 25.4
ag_pct_gdp$ag_pct_gdp[ag_pct_gdp$Country.Name == 'South Sudan'] <- ag_pct_gdp$ag_pct_gdp[ag_pct_gdp$Country.Name == 'Sudan']
ag_pct_gdp$ag_pct_gdp[ag_pct_gdp$Country.Name == 'Comoros'] <- 49.5

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  mutate(country = substr(code, 1, 2)) %>%
  unique %>%
  merge(read.csv('../Global Codes and Shapefile/DHS-WB-UN Country Codes.csv'), all.x=T, all.y=F)

data$iso3c <- countrycode(data$Country.or.Area, 'country.name', 'iso3c')

all <- merge(data, ag_pct_gdp, all.x=T, all.y=F) %>%
  dplyr::select(interview_year, latitude, longitude, code, ag_pct_gdp)

write.csv(all, '../../DHS Processed/Ag_Pct_GDP.csv', row.names=F)

#################
#Now make rasters
#################

library(rgdal)
library(raster)

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$iso3c <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

#Need to decide on scale and make a template raster
r <- raster('../Final Rasters/2000/irrig_aai.tif')

for (year in seq(1990, 2020)){
  dat <- ag_pct_gdp %>%
    filter(interview_year==year & !is.na(iso3c))
  
  spres <- sp::merge(sp, dat)
  
  spres@data$ag_pct_gdp[spres@data$ADMIN=="Western Sahara"] <- spres@data$ag_pct_gdp[spres@data$ADMIN=="Morocco"]
  spres@data$ag_pct_gdp[spres@data$ADMIN=="Kosovo"] <- spres@data$ag_pct_gdp[spres@data$ADMIN=="Republic of Serbia"]
  spres@data$ag_pct_gdp[spres@data$ADMIN=="Siachen Glacier"] <- spres@data$ag_pct_gdp[spres@data$ADMIN=="Pakistan"]
  spres@data$ag_pct_gdp[spres@data$ADMIN=="Taiwan"] <- 1.6
  
  rasterize(spres, r, field="ag_pct_gdp", fun='mean', na.rm=TRUE, filename=paste0('../Final Rasters/', year, '/ag_pct_gdp.tif'), driver='GTiff', overwrite=T)
  
  print(year)
}