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
precip <- read.csv('Coords&Precip.csv') %>%  #Because of different months within a year, in some cases 10-yr average has to different measurements for one site
  group_by(code, interview_year) %>%         #But differences are so trivially small, getting the mean for a site shouldn't matter
  summarise(precip_10yr_mean=mean(precip_10yr_mean),
            tmin_10yr_mean=mean(tmin_10yr_mean),
            tmax_10yr_mean=mean(tmax_10yr_mean)) %>%
  unique
pop <- read.csv('population.csv')

alldf <- Reduce(function(x,y){merge(x, y, all.x=T, all.y=F)}, list(sp, ag, avhrr, irrig, market, gdp, fao, wgi, precip, pop))

library(raster)

setwd('../DHS Spatial Covars/Final Rasters')

cropread <- function(r){
  r <- raster(r)
  r <- crop(r, extent(-180, 180, -50, 50))
  r
}

s <- sapply(list.files(pattern = '.tif$'), FUN=cropread) %>%
  stack

names(s) <- c("ag_pct_gdp", "bare", "Cereals", "precip_10yr_mean", "forest", "gdp", "government_effectiveness",
              "irrigation", "market_dist", "ndvi", "population", "RootsandTubers", "stability_violence", "tmax_10yr_mean",
              "tmin_10yr_mean")

#Combine Cerealas and Roots and Tubers
s[["crop_prod"]] <- s[["Cereals"]] + s[["RootsandTubers"]]
s <- dropLayer(s, which(names(s) == "RootsandTubers"))
s <- dropLayer(s, which(names(s) == "Cereals"))

alldf$crop_prod <- alldf$RootsandTubers + alldf$Cereals
alldf$RootsandTubers <- NULL
alldf$Cereals <- NULL

#Simplify to get one metric per code
alldf <- alldf %>%
  group_by(code) %>%
  summarize(ag_pct_gdp=mean(ag_pct_gdp),
            bare=mean(bare),
            precip_10yr_mean=mean(precip_10yr_mean),
            forest=mean(forest),
            gdp=mean(gdp),
            government_effectiveness=mean(government_effectiveness),
            irrigation=mean(irrigation),
            market_dist=mean(market_dist),
            ndvi=mean(ndvi),
            population=mean(population),
            stability_violence=mean(stability_violence),
            tmax_10yr_mean=mean(tmax_10yr_mean),
            tmin_10yr_mean=mean(tmin_10yr_mean),
            crop_prod=mean(crop_prod))

setwd('../../DHS Processed/')

write.csv(alldf, 'SpatialCovars.csv', row.names=F)
writeRaster(s, 'SpatialCovars.grd', format="raster")

