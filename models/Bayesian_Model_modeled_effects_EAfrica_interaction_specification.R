library(dplyr)
library(rstanarm)

setwd('~/dhsprocessed')

hh <- read.csv('HH_data_A.csv')
spi <- read.csv('PrecipIndices.csv') %>%
  select(-precip_10yr_mean, -tmax_10yr_mean, -tmin_10yr_mean)
cov <- read.csv('SpatialCovars.csv')

################################
#Combine and clear workspace
################################
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spi, cov))

all <- all %>%
  filter(country %in% c('KE', 'RW', 'TZ', 'UG'))

#Relevel factors
all <- all %>%
  mutate(toilet=relevel(toilet, ref="No Facility"),
         wealth_index=relevel(wealth_index, ref="Poorest"))

all$spei36_cat <- ifelse(all$spei36 < -1.5, 'dry',
                         ifelse(all$spei36 > 1.5, 'wet', 'normal'))

options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

f <- haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
  head_age + head_sex + wealth_index + urban_rural + spei36_cat*market_dist + spei36_cat*forest + spei36_cat*bare + spei36_cat*ag_pct_gdp + 
  spei36_cat*precip_10yr_mean + spei36_cat*gdp + spei36_cat*government_effectiveness + spei36_cat*irrigation + 
  spei36_cat*ndvi + spei36_cat*population + spei36_cat*stability_violence + spei36_cat*tmax_10yr_mean + spei36_cat*tmin_10yr_mean + 
  spei36_cat*fieldsize + spei36_cat*nutritiondiversity

stan_mod <- stan_glm(f, data=all, 
                     prior = normal(), 
                     prior_intercept = normal(),
                     iter = 3000, chains = 4)








