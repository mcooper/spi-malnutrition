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
  dplyr::select(Area, Item, Year, Value) %>%
  spread(Item, Value) %>%
  mutate(Country.Code=countrycode(Area, origin='country.name', destination='iso3c')) %>%
  filter(!is.na(Country.Code))

eg <- expand.grid(seq(1988, 2016), unique(d$Country.Code))
names(eg) <- c("Year", "Country.Code")

d <- merge(d, eg, all=T)

d <- d %>%
  arrange(Year) %>%
  group_by(Country.Code) %>%
  fill(`Cereals,Total`, `Roots and Tubers,Total`)

#Population data from world bank
e <- read.csv('../Population/API_SP.POP.TOTL_DS2_en_csv_v2_9908626.csv', skip = 4) %>%
  dplyr::select(-Country.Code, -Indicator.Name, -Indicator.Code, -X) %>%
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
  dplyr::select(latitude, longitude, code, interview_year) %>%
  mutate(country = substr(code, 1, 2)) %>%
  unique

dhscountries <- read.csv('../Global Codes and Shapefile/DHS-WB-UN Country Codes.csv')

data <- merge(data, dhscountries, all.x=T, all.y=F)

all <- merge(data, comb, all.x=T, all.y=F)

all <- all %>%
  dplyr::select(latitude, longitude, country, code, interview_year, Cereals,  RootsandTubers)

write.csv(all, '../../DHS Processed/FAOSTAT_TonnesPerCap.csv', row.names=F)


########################################################
#Prediction Raster
#####################################################

library(rgdal)
library(raster)

comb <- comb %>%
  filter(Year == 2016) %>%
  dplyr::select(SOVEREIGNT_ISO_3C=Country.Code, Area, Cereals, `Cereals,Total`, `Roots and Tubers,Total`,
         RootsandTubers, population)

#Deal with issues with Taiwan and China
comb$population[comb$SOVEREIGNT_ISO_3C=='TWN'] <- 23556706
comb$population[comb$SOVEREIGNT_ISO_3C=='ERI'] <- 4954645
comb$population[comb$SOVEREIGNT_ISO_3C=='GUF'] <- 275688
comb$population[comb$SOVEREIGNT_ISO_3C=='ESH'] <- 538755

comb <- comb %>%
  mutate(Cereals = `Cereals,Total`/population,
         RootsandTubers = `Roots and Tubers,Total`/population) %>%
  filter(Area != 'China')

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$SOVEREIGNT_ISO_3C <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

sp <- sp::merge(sp, comb)

sp$Cereals[is.na(sp$Cereals)] <- 0
sp$RootsandTubers[is.na(sp$RootsandTubers)] <- 0

#Need to decide on scale and make a template raster
r <- raster('../Final Rasters/irrigation.tif')

rasterize(sp, r, field="Cereals", fun='mean', na.rm=TRUE, filename='../Final Rasters/Cereals.tif', driver='GTiff', overwrite=T)
rasterize(sp, r, field="RootsandTubers", fun='mean', na.rm=TRUE, filename='../Final Rasters/RootsandTubers.tif', driver='GTiff', overwrite=T)





