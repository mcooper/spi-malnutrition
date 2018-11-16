set.seed(100)

library(sp)
library(raster)
library(rgdal)
library(dplyr)

sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile', 'ne_50m_admin_0_countries')

#Get lat-longs for lots of developed countries
n <- 50000

US <- spsample(sp[sp$ADMIN=="United States of America", ], n=n*0.20, "random")@coords
JP <- spsample(sp[sp$ADMIN=="Japan", ], n=n*0.15, "random")@coords
AU <- spsample(sp[sp$ADMIN=="Australia", ], n=n*0.10, "random")@coords
NW <- spsample(sp[sp$ADMIN=="Norway", ], n=n*0.05, "random")@coords
IR <- spsample(sp[sp$ADMIN=="Ireland", ], n=n*0.05, "random")@coords
SW <- spsample(sp[sp$ADMIN=="Sweden", ], n=n*0.05, "random")@coords
CA <- spsample(sp[sp$ADMIN=="Canada", ], n=n*0.10, "random")@coords
IS <- spsample(sp[sp$ADMIN=="Israel", ], n=n*0.10, "random") @coords
HK <- spsample(sp[sp$ADMIN=="Hong Kong S.A.R.", ], n=n*0.05, "random")@coords
SG <- spsample(sp[sp$ADMIN=="Singapore", ], n=n*0.05, "random")@coords
SK <- spsample(sp[sp$ADMIN=="South Korea", ], n=n*0.10, "random")@coords

all <- Reduce(rbind, list(US, JP, AU, NW, IR, SW, CA, IS, HK, SG, SK))

rztpois <- function(n, lambda){
  #Zero-truncated Poisson
  #https://stat.ethz.ch/pipermail/r-help/2005-May/070683.html
  rpois(n, (lambda - -log(1 - runif(n)*(1 - exp(-lambda))))) + 1
}

all <- all %>%
  data.frame %>%
  select(longitude=x, latitude=y) %>%
  mutate(spei24 = rnorm(n, 0, 1),
         haz_dhs = rnorm(n, 0, 1),
         age = runif(n, 0, 60),
         calc_birthmonth=sample(seq(1, 12), n, replace = T),
         birth_order=rztpois(n, 1.8),
         hhsize=rztpois(n, 4),
         sex=sample(c('Male', 'Female'), n, replace=T),
         mother_years_ed=rnorm(n, 20, 2),
         toilet=sample(rep('Flush Toilet', n)),
         head_age=rnorm(n, 35, 5),
         head_sex=sample(c("Male", "Female"), n, replace=T),
         wealth_index="Richest")

setwd('G://My Drive/DHS Spatial Covars/Final Rasters')

pts <- SpatialPoints(all[ c('longitude', 'latitude')])

for (rast in c("ag_pct_gdp", "bare", "forest", "gdp", 
               "government_effectiveness", "irrig_aai", "irrig_aei", "market_dist", 
               "ndvi", "population", "stability_violence", "crop_prod", "fieldsize", 
               "nutritiondiversity", "builtup", "elevation", "high_settle", 
               "low_settle", "roughness", "imports_percap", "grid_gdp", "grid_hdi", 
               "enrollment", "precip_10yr_mean", "tmax_10yr_mean", 'tmin_10yr_mean')){
  all[ , rast] <- extract(raster(paste0(rast, '.tif')), pts)
}

write.csv(na.omit(all), 'G://My Drive/DHS Processed/SimulatedDevelopedData.csv', row.names=F)
