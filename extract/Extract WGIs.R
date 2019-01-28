setwd('G://My Drive/DHS Spatial Covars/World Governance Indicators/')

library(tidyverse)
library(countrycode)
library(readxl)

#Downloaded from http://info.worldbank.org/governance/wgi/#home
stability <- read_xlsx("wgidataset.xlsx", sheet = "Political StabilityNoViolence", skip=13, na = "#N/A") %>%
  dplyr::select(Country=X__1, WBCode=X__2, `1996`,`1998`, `2000`, `2002`, `2003`, 
         `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, 
         `2015`, `2016`, `2017`) %>%
  filter(Country != 'Country/Territory') %>%
  mutate(`1990` = NA, `1991`=NA, `1992`=NA, `1993`=NA, `1994`=NA, `1995`=NA, `1997`=NA, `1999`=NA,
         `2001` = NA, `2018`=NA, `2019`=NA, `2020`=NA, `1989`=NA, `1988`=NA) %>%
  gather(interview_year, stability_violence, -Country, -WBCode) %>%
  arrange(desc(interview_year)) %>%
  group_by(Country) %>%
  fill(stability_violence) %>%
  arrange(interview_year) %>%
  group_by(Country) %>%
  fill(stability_violence) %>%
  mutate(iso3c=countrycode(Country, 'country.name', 'iso3c'))
  
effectiveness <- read_xlsx("wgidataset.xlsx", sheet = "GovernmentEffectiveness", skip=13, na = "#N/A") %>%
  dplyr::select(Country=X__1, WBCode=X__2, `1996`,`1998`, `2000`, `2002`, `2003`, 
         `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, 
         `2015`, `2016`, `2017`) %>%
  filter(Country != 'Country/Territory') %>%
  mutate(`1990` = NA, `1991`=NA, `1992`=NA, `1993`=NA, `1994`=NA, `1995`=NA, `1997`=NA, `1999`=NA,
         `2001` = NA, `2018`=NA, `2019`=NA, `2020`=NA, `1989`=NA, `1988`=NA) %>%
  gather(interview_year, government_effectiveness, -Country, -WBCode) %>%
  arrange(desc(interview_year)) %>%
  group_by(Country) %>%
  fill(government_effectiveness) %>%
  arrange(interview_year) %>%
  group_by(Country) %>%
  fill(government_effectiveness) %>%
  mutate(iso3c=countrycode(Country, 'country.name', 'iso3c'))

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  mutate(country = substr(code, 1, 2)) %>%
  unique %>%
  merge(read.csv('../Global Codes and Shapefile/DHS-WB-UN Country Codes.csv'), all.x=T, all.y=F)

data$iso3c <- countrycode(data$Country.or.Area, 'country.name', 'iso3c')

all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)}, list(data, stability, effectiveness)) %>%
  dplyr::select(interview_year, latitude, longitude, code, government_effectiveness, stability_violence)

write.csv(all, '../../DHS Processed/WorldGovernanceIndicators.csv', row.names=F)

#############################################################
#Now Create a Raster
######################################################

library(rgdal)
library(raster)

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$iso3c <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

#Deal with South Sudan
stability$stability_violence[stability$Country=="South Sudan" & stability$interview_year < 2011] <- stability$stability_violence[stability$Country=="Sudan" & stability$interview_year < 2011]
effectiveness$government_effectiveness[effectiveness$Country=="South Sudan" & effectiveness$interview_year < 2011] <- effectiveness$government_effectiveness[effectiveness$Country=="Sudan" & effectiveness$interview_year < 2011]

#Deal with East Timor
stability$stability_violence[stability$Country=="Timor-Leste" & stability$interview_year < 2000] <- stability$stability_violence[stability$Country=="Indonesia" & stability$interview_year < 2000]
effectiveness$government_effectiveness[effectiveness$Country=="Timor-Leste" & effectiveness$interview_year < 2000] <- effectiveness$government_effectiveness[effectiveness$Country=="Indonesia" & effectiveness$interview_year < 2000]

stability$iso3c[stability$Country=='Korea, Dem. Rep.'] <- "PRK"
effectiveness$iso3c[effectiveness$Country=='Korea, Dem. Rep.'] <- "PRK"

stability$stability_violence <- as.numeric(stability$stability_violence)
effectiveness$government_effectiveness <- as.numeric(effectiveness$government_effectiveness)

#Need to decide on scale and make a template raster
r <- raster('G://My Drive/CHIRPS/Monthly/chirps-v2.0.1981.01.tif')

for (year in seq(1990, 2020)){
  
  st_sel <- stability %>%
    filter(interview_year == year & !is.na(iso3c))
  ef_sel <- effectiveness %>%
    filter(interview_year == year & !is.na(iso3c))
  
  st_res <- sp::merge(sp, st_sel)
  ef_res <- sp::merge(sp, ef_sel)
  
  st_res@data$stability_violence[st_res@data$ADMIN=="Western Sahara"] <- st_res@data$stability_violence[st_res@data$ADMIN=="Morocco"]
  st_res@data$stability_violence[st_res@data$ADMIN=="Kosovo"] <- st_res@data$stability_violence[st_res@data$ADMIN=="Republic of Serbia"]
  st_res@data$stability_violence[st_res@data$ADMIN=="Siachen Glacier"] <- st_res@data$stability_violence[st_res@data$ADMIN=="Pakistan"]
  
  ef_res@data$government_effectiveness[ef_res@data$ADMIN=="Western Sahara"] <- ef_res@data$government_effectiveness[ef_res@data$ADMIN=="Morocco"]
  ef_res@data$government_effectiveness[ef_res@data$ADMIN=="Kosovo"] <- ef_res@data$government_effectiveness[ef_res@data$ADMIN=="Republic of Serbia"]
  ef_res@data$government_effectiveness[ef_res@data$ADMIN=="Siachen Glacier"] <- ef_res@data$government_effectiveness[ef_res@data$ADMIN=="Pakistan"]
  
  rasterize(ef_res, r, field="government_effectiveness", fun='mean', na.rm=T, filename=paste0('../Final Rasters/', year, '/government_effectiveness.tif'), driver='GTiff', overwrite=TRUE)
  rasterize(st_res, r, field="stability_violence", fun='mean', na.rm=T, filename=paste0('../Final Rasters/', year, '/stability_violence.tif'), driver='GTiff', overwrite=TRUE)

  print(year)
}  
