library(dplyr)

setwd('G://My Drive/DHS Processed')

sp <- read.csv('sp_export.csv') %>%
  select(latitude, longitude, interview_year, code) %>%
  unique

ag <- read.csv('Ag_Pct_GDP.csv')
avhrr <- read.csv('avhrr.csv')
irrig <- read.csv('irrigation.csv')
market <- read.csv('market_distance.csv')
gdp <- read.csv('country_gdp.csv')
fao <- read.csv('FAOSTAT_TonnesPerCap.csv')
wgi <- read.csv('WorldGovernanceIndicators.csv')
pop <- read.csv('population.csv')
# admin <- read.csv('Admin_Areas.csv') %>%
#   select(-nnAdm1, -nnAdm2)
fields <- read.csv('fieldsize.csv')
nut <- read.csv('nutritiondiversity.csv')
built <- read.csv('builtup.csv')
elev <- read.csv('elevation.csv')
rough <- read.csv('roughness.csv')
settle <- read.csv('settled.csv')
imports <- read.csv('Imports_Per_Capita.csv')
grid_gdp <- read.csv('grid_gdp.csv')
grid_hdi <- read.csv('grid_hdi.csv')
enrollment <- read.csv('enrollment.csv')
assistance <- read.csv('Assistance.csv')

alldf <- Reduce(function(x,y){merge(x, y, all.x=T, all.y=F)}, 
                list(sp, ag, avhrr, irrig, market, gdp, fao, wgi,
                     pop, fields, nut, built, elev, rough, settle, 
                     imports, grid_gdp, grid_hdi, enrollment,
                     assistance))

alldf$crop_prod <- alldf$Cereals + alldf$RootsandTubers

#Simplify to get one metric per code
alldf <- alldf %>%
  select(code, interview_year, ag_pct_gdp, bare, forest, gdp, government_effectiveness, 
         irrig_aai, irrig_aei, market_dist, ndvi, population, stability_violence, crop_prod, fieldsize, 
         nutritiondiversity, builtup, elevation, high_settle, low_settle, roughness,
         imports_percap, grid_gdp, grid_hdi, enrollment, assistance)

write.csv(alldf, 'SpatialCovars.csv', row.names=F)
