setwd('G://My Drive/DHS Processed')

library(tidyverse)

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc)) %>%
  na.omit

all <- all %>% filter(country == 'KE')

fit <- CBPS(natural ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + wealth_index + grid_hdi + market_dist + population,
            data = all, ATT = TRUE, method='exact')
summary(fit)

m.out <- matchit(natural ~ fitted(fit), method = "nearest", data = all, replace = TRUE)
