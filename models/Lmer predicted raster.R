library(dplyr)
library(lme4)
library(MASS)
library(broom)

setwd('~/dhsprocessed')
setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov)) %>%
  na.omit

#Try rescaling some geospatial covariates
all$precip_10yr_mean <- (all$precip_10yr_mean*12)/1000
all$gdp <- all$gdp/1000
all$grid_gdp <- all$grid_gdp/1000
all$market_dist <- all$market_dist/(24*7)
all$population <- all$population/1000
all$tmax_10yr_mean <- all$tmax_10yr_mean - 273.15
all$tmin_10yr_mean <- all$tmin_10yr_mean - 273.15
all$builtup <- all$builtup*100
all$elevation <- all$elevation/1000
all$imports_percap <- all$imports_percap/1000

all$gdp_l <- log(all$gdp)
all$grid_gdp_l <- log(all$grid_gdp)


library(raster)

setwd('G://My Drive/DHS Spatial Covars/Final Rasters')

setNAs <- function(raster, column){
  raster[raster > (max(all[ , column], na.rm=T) + sd(all[ , column], na.rm=T)/2)] <- NA
  raster[raster < (min(all[ , column], na.rm=T) - sd(all[ , column], na.rm=T)/2)] <- NA
  return(raster)
}

ag_pct_gdp <- raster('ag_pct_gdp.tif')

background <- ag_pct_gdp/ag_pct_gdp

builtup <- (raster('builtup.tif')*100)

crop_prod <- raster('crop_prod.tif')

elevation <- (raster('elevation.tif')/1000)

fieldsize <- raster('fieldsize.tif')

forest <- raster('forest.tif')

gdp <- (raster('gdp2020.tif')/1000) %>%
  setNAs('gdp')

base <- gdp/gdp

gdp_l <- log(gdp)

grid_gdp <- (raster('grid_gdp.tif')/1000)

grid_gdp_l <- log(grid_gdp)

grid_hdi <- raster('grid_hdi.tif')

government_effectiveness <- raster('government_effectiveness.tif')

high_settle <- raster('high_settle.tif')

imports_percap <- raster('imports_percap.tif')/1000

irrig_aei <- raster('irrig_aei.tif')

irrig_aai <- raster('irrig_aai.tif')

low_settle <- raster('low_settle.tif')

market_dist <- raster('market_dist.tif')/(24*7)

ndvi <- raster('ndvi.tif')

nutritiondiversity <- raster('nutritiondiversity.tif')

population <- (raster('population.tif')/1000)

precip_10yr_mean <- (raster('CHIRPS_10yr_avg.tif')*12/1000)

roughness <- raster('roughness.tif')

stability_violence <- raster('stability_violence.tif')

tmax_10yr_mean <- (raster('TMAX_10yr_avg.tif') - 273.15)

#Define Variables for mapping baseline
age <- mean(all$age)
birth_order <- mean(all$birth_order)
hhsize <- mean(all$hhsize)
mother_years_ed <- mean(all$mother_years_ed)
head_age <- mean(all$head_age)

makeRasts <- function(mod, term, usebase=FALSE, censor=TRUE){
  #mod is the model that has been fit
  #term can be either 'speiDry', 'speiWet', or ''
  #  'speiDry' and 'speiWet' will map estimated changes in HAZ scores during a wet or dry year
  #  '' will give a map of estimated HAZ scores during a normal year
  #usebase will use a raster of 0s and 1s to exclude certain areas.
  #censor will set values greater than 0 to NA
  
  s <- tidy(mod)
  
  coefs <- s[grepl(term, s$term), c('term', 'estimate')]
  
  coefs$term <- gsub(paste0(term, ':'), '', coefs$term)
  
  if(term == ''){
    coefs <- coefs[!grepl('spei', coefs$term), ]
  }
  
  if (usebase){
    rast <- base*coefs$estimate[1]
  } else{
    rast <- coefs$estimate[1]
  }
  for (i in 2:nrow(coefs)){
    if(exists(coefs$term[i])){
      tmp_rast <- get(coefs$term[i])
    } else{
      tmp_rast <- 0
    }
    suppressWarnings(rast <- rast + tmp_rast*coefs$estimate[i])
  }
  
  if (censor){
    rast[rast > 0] <- NA
  }
  
  return(rast)
}

#Make categorical
all$spei <- all$spei24

all$spei <- ifelse(all$spei > 1.5, "Wet",
                   ifelse(all$spei < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

mod <- lm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
            birth_order + hhsize + sex + mother_years_ed + toilet +
            head_age + head_sex + wealth_index + 
            
            #GDP related vars
            #spei*ag_pct_gdp +
            #spei*gdp_l +
            #spei*grid_gdp_l + 
            spei*grid_gdp + 
            #spei*grid_hdi + 
            #spei*imports_percap + 
            
            #Pop-Urban vars
            #spei*builtup +
            #spei*low_settle + #Slightly colinear with irrigation (0.50) so avoid this one
            #spei*high_settle + 
            spei*population +
            
            #Land Cover Vars
            #spei*precip_10yr_mean +
            #spei*forest +
            spei*ndvi +
            #spei*bare + 
            
            #Topographic Vars
            #spei*elevation +
            #spei*roughness + 
            spei*tmax_10yr_mean +
          
            #Totally Independat Vars
            spei*market_dist + #not the same before and after 2000
            #spei*fieldsize + 
            spei*crop_prod +
            spei*government_effectiveness +
            spei*irrig_aai +
            spei*nutritiondiversity +
            spei*stability_violence,
           data=all)

summary(mod)

#Baseline
plot(makeRasts(mod, ''))

#Dry
plot(makeRasts(mod, "speiDry"))

#Wet
plot(makeRasts(mod, "speiWet"))



writeRaster(rasts[[1]], 'Wet.tif', format='GTiff', overwrite=T)
writeRaster(rasts[[3]], 'Dry.tif', format='GTiff', overwrite=T)


library(raster)
library(rgdal)

rast <- makeRasts(mod, "Wet", usebase=TRUE)[[2]]
sp <- readOGR('G:/My Drive/DHS Spatial Covars/Global Codes and Shapefile', 
              'ne_50m_admin_0_countries')

plot(background,
     main='Expected Change in HAZ Scores From a 24-Month SPEI of < -1.5',
     ext=extent(c(-100, 150, -40, 50)),
     col='#A8A48F',
     axes=F,
     legend=F)
plot(rast, 
     add=T)
plot(sp, add=T)


