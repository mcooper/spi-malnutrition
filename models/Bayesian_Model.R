setwd('G://My Drive/DHS Processed')

hh <- read.csv('hhvars.csv')
gdp <- read.csv('country_gdp.csv')
farm <- read.csv('FarmingSystems.csv')
lc <- read.csv('landcover.csv')
md <- read.csv('MarketDist.csv')
pop <- read.csv('PopPer100sqkm.csv')
spi <- read.csv('Coords&SPI.csv')

#Look at including World Governance Indicators at: http://info.worldbank.org/governance/wgi/#home

all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spi))
