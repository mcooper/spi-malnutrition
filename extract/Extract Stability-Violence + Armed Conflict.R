library(dplyr)
library(readxl)
library(tidyr)
library(rgdal)
library(countrycode)
library(foreign)
library(raster)

stability <- read_xlsx("G://My Drive/DHS Spatial Covars/World Governance Indicators/wgidataset.xlsx", sheet = "Political StabilityNoViolence", skip=13, na = "#N/A") %>%
  dplyr::select(Country=X__1, WBCode=X__2, `1996`,`1998`, `2000`, `2002`, `2003`, 
         `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, 
         `2015`, `2016`, `2017`) %>%
  filter(Country != 'Country/Territory') %>%
  mutate(`1990` = NA, `1991`=NA, `1992`=NA, `1993`=NA, `1994`=NA, `1995`=NA, `1997`=NA, `1999`=NA,
         `2001` = NA, `2018`=NA, `2019`=NA, `2020`=NA, `1989`=NA, `1988`=NA) %>%
  gather(Year, stability_violence, -Country, -WBCode) %>%
  mutate(stability_violence=as.numeric(stability_violence)) %>%
  arrange(desc(Year)) %>%
  group_by(Country) %>%
  fill(stability_violence) %>%
  arrange(Year) %>%
  group_by(Country) %>%
  fill(stability_violence) %>%
  mutate(iso3c=countrycode(Country, 'country.name', 'iso3c'))

stability$iso3c[stability$Country=='Korea, Dem. Rep.'] <- "PRK"

new <- read.dbf('G://My Drive/DHS Spatial Covars/UCDP-PRIO Armed Conflict Dataset/admin1_data.dbf', as.is=T) %>%
  dplyr::select(-NAME_1, -area_sqkm, -new) %>%
  gather(Year, Value, -GID_0, -GID_1, -NAME_0) %>%
  mutate(Value=log(1+Value),
         Year=as.numeric(substr(Year, 2, 5)),
         iso3c=countrycode(NAME_0, 'country.name', 'iso3c'))

comb <- merge(new, stability, all.x=F, all.y=F) %>%
  filter(!is.na(iso3c))

adjusted <- comb %>% group_by(Year) %>%
  mutate(YearMax=max(Value)) %>%
  group_by(Year, iso3c) %>%
  mutate(Adjusted=stability_violence + 2*(mean(Value/YearMax) - (Value/YearMax)))

sp <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(interview_year, latitude, longitude, code) %>% 
  unique

shape <- readOGR('G://My Drive/DHS Spatial Covars/UCDP-PRIO Armed Conflict Dataset', 'admin1_data')
shape <- shape[ , c('GID_0', 'NAME_0', 'GID_1')]

ref <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2017/stability_violence.tif')

sp_accum <- data.frame()
for (y in seq(1992, 2017)){
  
  if (y==1992){
    sp_sel <- sp %>% filter(interview_year <= 1992)
  }
  if (y==2017){
    sp_sel <- sp %>% filter(interview_year >=2017)
  }
  if (y < 2017 & y > 1992){
    sp_sel <- sp %>% filter(interview_year == y)
  }
  
  data <- adjusted %>% filter(Year==y)
  
  shape_merge <- sp::merge(shape, data)
  
  r <- rasterize(shape_merge, ref, field='Adjusted')

  writeRaster(r, filename = paste0('SV_AC', y, '.tif'), format="GTiff")
  
  spp <- SpatialPoints(sp_sel[, c('longitude', 'latitude')])
  
  sp_sel$SV_AC <- extract(r, spp)
  
  while(sum(is.na(sp_sel$SV_AC > 0))){
    r <- focal(r, matrix(rep(1, 9), ncol=3), fun=mean, pad=TRUE, na.rm=T, padValue=NA)
    sp_sel$SV_AC <- extract(r, spp)
  }
  
  sp_accum <- bind_rows(sp_accum, sp_sel)
  
}
