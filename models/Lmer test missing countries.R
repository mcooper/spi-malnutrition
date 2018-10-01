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

#Make categorical
all$spei <- all$spei24

all$spei <- ifelse(all$spei > 1.5, "Wet",
                   ifelse(all$spei < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

accum <- data.frame()
for (cc in unique(all$country)){

  mod <- rlm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
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
             data=all %>% filter(country != cc))
  
  res <- tidy(mod)
  
  res$cc <- cc
  
  accum <- bind_rows(accum, res)
  
  print(cc)
  
}

setwd("G:/My Drive/Dissertation/Missing Country Estimates")

library(ggplot2)

for (coef in unique(accum$term)){
  ggplot(aes(x = estimate) , data = accum %>% filter(term == coef)) + 
    geom_histogram(aes(fill=cc), colour="grey20", lwd=0.2) +
    stat_bin(geom="text", size=3.5,
             aes(label=cc, group=cc), position=position_stack(vjust=0.5)) + 
    geom_vline(xintercept=0, color="red", size=2) + 
    ggtitle(coef)

  ggsave(paste0(gsub(':', '', coef), '.png'))
  print(coef)
}


