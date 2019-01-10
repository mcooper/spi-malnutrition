library(dplyr)

setwd('G://My Drive/DHS Processed')

sp <- read.csv('sp_export.csv') %>%
  select(latitude, longitude, interview_year, code) %>%
  unique

ag <- read.csv('Ag_Pct_GDP.csv')
avhrr <- read.csv('avhrr.csv')
bodycount <- read.csv('bodycount.csv')
irrig <- read.csv('irrigation.csv')
market <- read.csv('market_distance.csv')
gdp <- read.csv('country_gdp.csv')
crop_prod <- read.csv('Crop_production.csv')
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
                list(sp, ag, avhrr, irrig, market, gdp, crop_prod, wgi,
                     pop, fields, nut, built, elev, rough, settle, 
                     imports, grid_gdp, grid_hdi, enrollment,
                     assistance, bodycount))

#Simplify to get one metric per code
alldf <- alldf %>%
  select(-country, -latitude, -longitude)

write.csv(alldf, 'SpatialCovars.csv', row.names=F)
