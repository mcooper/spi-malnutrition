library(ggplot2)
library(dplyr)
library(lme4)
library(broom)

setwd('~/dhsprocessed')

hha <- read.csv('HH_data_A.csv')
lc <- read.csv('landcover_processed.csv')
spei <- read.csv('PrecipIndices.csv')
spei_ind <- read.csv('PrecipIndices_Individual.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, lc, spei, spei_ind, cov))

all$market_dist[all$market_dist==0] <- 0.1

all$lbuiltup <- log(all$builtup)
all$lmarket_dist <- log(all$market_dist)

mod3 <- lmer(haz_dhs ~ interview_year + (1|country/interview_month) + age + birth_order + hhsize + sex + mother_years_ed + toilet +
               head_age + head_sex + urban_rural + wealth_index + lbuiltup + market_dist + (1|surveycode) + (1|country), data=all)
