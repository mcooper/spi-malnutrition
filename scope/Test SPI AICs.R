library(ggplot2)
library(dplyr)
library(lme4)
library(foreach)
library(doParallel)
library(broom)

setwd('~/dhsprocessed')
setwd('G://My Drive/DHS Processed')

hh <- read.csv('hhvars.csv')
hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
spei_ind <- read.csv('PrecipIndices_Individual.csv')
cov <- read.csv('SpatialCovars.csv')

hh <- hh[ , c(names(hha), 'whz_dhs')] %>% na.omit

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, spei, spei_ind, cov))

all$spi_age_mix[all$age > 24] <- all$spi_age[all$age > 24]
all$spi_age_mix[all$age < 24] <- all$spi_ageutero[all$age < 24]
all$spi_gs_age_mix[all$age > 24] <- all$spi_gs_age[all$age > 24]
all$spi_gs_age_mix[all$age < 24] <- all$spi_gs_ageutero[all$age < 24]
all$spei_age_mix[all$age > 24] <- all$spei_age[all$age > 24]
all$spei_age_mix[all$age < 24] <- all$spei_ageutero[all$age < 24]
all$spei_gs_age_mix[all$age > 24] <- all$spei_gs_age[all$age > 24]
all$spei_gs_age_mix[all$age < 24] <- all$spei_gs_ageutero[all$age < 24]

df <- data.frame()
for (i in names(all)[grepl('sp', names(all))]){
  all <- all[!is.infinite(all[ , i]), ]
}

all <- na.omit(all)

for (i in names(all)[grepl('sp', names(all))]){
  print(i)
  
  all$precip <- all[ , i]
  
  all$precip <- as.factor(ifelse(all$precip < -1.5, "Low",
                       ifelse(all$precip > 1.5, "High", "Medium")))

  all$precip <- relevel(all$precip, ref="Medium")
  
  #Get Residuals
  mod <- lmer(haz_dhs ~ interview_year + (1|country/interview_month) + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                head_age + head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country) + precip, data=all[!is.infinite(all$precip), ])
  
  td <- tidy(mod)
  row.names(td) <- td$term
  
  df <- bind_rows(df, data.frame(index=i, AIC=AIC(mod), 
                                 high=td['precipHigh', 'estimate'],
                                 low=td['precipLow', 'estimate'],
                                 highstat=td['precipHigh', 'statistic'],
                                 lowstat=td['precipLow', 'estimate']))
}


