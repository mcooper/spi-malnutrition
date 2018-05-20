setwd('G://My Drive/DHS Spatial Covars/FAOSTAT')

library(tidyverse)
library(countrycode)

#Data from FAOSTAT on production total
#Data in 'tonnes'
# Tonnes == Tons?
# False!


#Lots of NAs - unclear if they are equal to 0?
#So I will just do Cereals and Roots/Tubers.  Since they have a lot of data.

# Tonnes = 1,000 kg
d <- read.csv('FAOSTAT_data_5-20-2018.csv') %>%
  filter(Year >= 1988) %>%
  select(Area, Item, Year, Value) %>%
  spread(Item, Value) %>%
  mutate(Country.Code=countrycode(Area, origin='country.name', destination='iso3c')) %>%
  filter(!is.na(Country.Code))

#Population density
e <- read.csv('../Population/API_SP.POP.TOTL_DS2_en_csv_v2_9908626.csv', skip = 4) %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code, -X) %>%
  gather(Year, population, -Country.Name) %>%
  mutate(Year=as.numeric(gsub('X', '', Year))) %>%
  filter(Year >= 1988) %>%
  mutate(Country.Code=countrycode(Country.Name, origin='country.name', destination='iso3c')) %>%
  filter(!is.na(Country.Code))

comb <- merge(d, e, all.x=T, all.y=F)


comb <- comb %>%
  mutate(Cereals = `Cereals,Total`/population,
         RootsandTubers = `Roots and Tubers,Total`/population,
         interview_year=Year)
         
data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  select(latitude, longitude, code, interview_year) %>%
  mutate(country = substr(code, 1, 2)) %>%
  unique

dhscountries <- read.csv('../Global Codes and Shapefile/DHS-WB-UN Country Codes.csv')

data <- merge(data, dhscountries, all.x=T, all.y=F)

all <- merge(data, comb, all.x=T, all.y=F)

all <- all %>%
  select(latitude, longitude, country, code, interview_year, Cereals,  RootsandTubers)

write.csv(all, '../../DHS Processed/FAOSTAT_TonnesPerCap.csv', row.names=F)


########################################################
#Prediction Raster
#####################################################

library(rgdal)
library(raster)

comb$SOVEREIGNT_ISO_3C <- comb$Country.Code

comb <- comb %>%
  filter(Year == 2016)

comb <- comb %>%
  select(Country.Code, Area, Cereals, 
         RootsandTubers)

#Deal with issues with Taiwan and China
comb$Cereals[comb$Country.Code=='TWN'] <- comb$Cereals[comb$Area=='China'] - comb$Cereals[comb$Area=='China, mainland']

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$SOVEREIGNT_ISO_3C <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

sp <- sp::merge(sp, comb)


#Need to decide on scale and make a template raster
r <- raster('templateRaster.tif')

for (field in c("Cereals", "CitrusFruit", "CoarseGrain", 
                "FibreCropsPrimary", "OilcropsCakeEquivalent", "OilcropsOilEquivalent", 
                "Pulses", "RootsandTubers", "Treenuts")){
  rasterize(sp, r, field=field, fun='mean')
}


#########TODO:

#These countries dont have pop data:
#-They arent necessary to have yearly (thank god) bc they are not in DHS.  But you will need to find estimates for 2016
# in order to get yield per cap

Cook Islands
Western Sahara
Guadeloupe
French Guiana
Montserrat
Martinique
Niue
Occupied Palestinian Territory
Saint Pierre and Miquelon
Tokelau
China, Taiwan Province of
Wallis and Futuna Islands
Saint Helena, Ascension and Tristan da Cunha
Kuwait
Eritrea




