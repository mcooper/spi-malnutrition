library(dplyr)
library(broom)
library(tidyr)
library(MASS)
library(lme4)

source('C://Git/spi-malnutrition/models/mod_utils.R')

setwd('~')
setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov)) %>%
  na.omit

#Make categorical
all$spei <- all$spei24

all$spei <- ifelse(all$spei > 1.4, "Normal",
                   ifelse(all$spei < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

sel <- all %>%
  filter(builtup < 20 & bare < 95 & spei24 <= 1.4)

transformations <- list(grid_gdp=function(x){log(x/1000)}
                        ,mean_annual_precip=function(x){log((x + 1)/1000)}
                        ,population=function(x){x/1000}
                        ,tmax_10yr_mean=function(x){x-273.15}
                        ,assistance=function(x){log(x+10)}
                        ,roughness=function(x){log(x+10)}
                        )


for (n in names(sel)){
  if (n %in% names(transformations)){
    sel[ , paste0(n, '_t')] <- transformations[[n]](sel[ , n])
  }
}


mod <- rlm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
             birth_order + hhsize + sex + mother_years_ed + toilet +
             head_age + head_sex + wealth_index +
             spei*ndvi +
             spei*government_effectiveness +
             spei*grid_gdp_t +
             spei*grid_hdi +
             spei*mean_annual_precip_t +
             spei*nutritiondiversity_mfad +
             spei*population_t +
             spei*roughness_t +
             spei*stability_violence +
             spei*irrig_aai + 
             spei*tmax_10yr_mean_t + 
             spei*assistance_t
           , data=sel)

summary(mod)

rast <- make_rasts_year(mod, "speiDry", 2017,
                     transformations,
                     mask=93);

writeRaster(rast, 'G://My Drive/DHS Spatial Covars/Final Rasters/Final_Raster.tif', format='GTiff', overwrite=T)

