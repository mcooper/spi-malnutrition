library(dplyr)

setwd('G://My Drive/DHS Processed')

sp <- read.csv('sp_export.csv') %>%
  select(latitude, longitude, interview_year, code) %>%
  unique

ag <- read.csv('Ag_Pct_GDP.csv') %>%
  select(-ag_pct_gdp_interpolated)
avhrr <- read.csv('avhrr.csv')
irrig <- read.csv('irrigation.csv')
market <- read.csv('market_distance.csv')
gdp <- read.csv('country_gdp.csv')
fao <- read.csv('FAOSTAT_TonnesPerCap.csv')
wgi <- read.csv('WorldGovernanceIndicators.csv') %>%
  select(-wgi_impute)
precip <- read.csv('PrecipIndices.csv') %>%
  select(code, precip_10yr_mean, tmax_10yr_mean, tmin_10yr_mean) %>%
  unique
pop <- read.csv('population.csv')
admin <- read.csv('Admin_Areas.csv') %>%
  select(-nnAdm1, -nnAdm2)
fields <- read.csv('fieldsize.csv')
nut <- read.csv('nutritiondiversity.csv')
built <- read.csv('builtup.csv')
elev <- read.csv('elevation.csv')
rough <- read.csv('roughness.csv')
settle <- read.csv('settled.csv')
imports <- read.csv('Imports_Per_Capita.csv')

alldf <- Reduce(function(x,y){merge(x, y, all.x=T, all.y=F)}, list(sp, ag, avhrr, irrig, market, gdp, fao, wgi, precip, pop, admin, fields, nut, built, elev, rough, settle, imports))

alldf$crop_prod <- alldf$Cereals + alldf$RootsandTubers

#Simplify to get one metric per code
alldf <- alldf %>%
  group_by(code, Adm1, Adm2) %>%
  summarize(ag_pct_gdp=mean(ag_pct_gdp),
            bare=mean(bare),
            #precip_10yr_mean=mean(precip_10yr_mean),
            forest=mean(forest),
            gdp=mean(gdp),
            government_effectiveness=mean(government_effectiveness),
            irrigation=mean(irrigation),
            market_dist=mean(market_dist),
            ndvi=mean(ndvi),
            population=mean(population),
            stability_violence=mean(stability_violence),
            #tmax_10yr_mean=mean(tmax_10yr_mean),
            #tmin_10yr_mean=mean(tmin_10yr_mean),
            crop_prod=mean(crop_prod),
            fieldsize=mean(fieldsize),
            nutritiondiversity=mean(nutritiondiversity),
            builtup=mean(builtup),
            elevation=mean(elevation),
            high_settle=mean(high_settle),
            low_settle=mean(low_settle),
            roughness=mean(roughness),
            imports_percap=mean(imports_percap))

write.csv(alldf, 'SpatialCovars.csv', row.names=F)
