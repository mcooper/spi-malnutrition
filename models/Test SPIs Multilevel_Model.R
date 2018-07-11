library(ggplot2)
library(dplyr)
library(lme4)
library(broom)

setwd('~/dhsprocessed')

hh <- read.csv('hhvars.csv')
hha <- read.csv('HH_data_A.csv')
lc <- read.csv('landcover_processed.csv')
spei <- read.csv('PrecipIndices.csv')
spei_ind <- read.csv('PrecipIndices_Individual.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, lc, spei, spei_ind, cov))

all$spi_age_mix[all$age > 24] <- all$spi_age[all$age > 24]
all$spi_age_mix[all$age < 24] <- all$spi_ageutero[all$age < 24]
all$spi_gs_age_mix[all$age > 24] <- all$spi_gs_age[all$age > 24]
all$spi_gs_age_mix[all$age < 24] <- all$spi_gs_ageutero[all$age < 24]
all$spei_age_mix[all$age > 24] <- all$spei_age[all$age > 24]
all$spei_age_mix[all$age < 24] <- all$spei_ageutero[all$age < 24]
all$spei_gs_age_mix[all$age > 24] <- all$spei_gs_age[all$age > 24]
all$spei_gs_age_mix[all$age < 24] <- all$spei_gs_ageutero[all$age < 24]


#Paramaters to track:
# SPI > -1 or < 1
# Market Distance high or low
# 

sumdf <- data.frame()
for (i in names(all)[grepl('sp', names(all))]){
  print(i)
  
  tmp <- data.frame(var=i)
  
  df <- all
  df$testvar <- df[ , i]
  
  df$testvar <- ifelse(df[ ,i] < -1.5, "Dry",
                       ifelse(df[ ,i] > 1.5, "Wet", "Normal")) %>%
    as.factor %>%
    relevel(df$testvar, ref="Normal")
  
  
  baseline <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                              head_age + head_sex + urban_rural + wealth_index + (testvar|code) + (1|surveycode) + (1|country), 
                              data=df)
 
  baseline_es<- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                                  head_age + head_sex + urban_rural + wealth_index + testvar + natural + (1|surveycode) + (1|country), 
                                data=df %>% filter(market_dist > 24*14))
  
  rural<- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                            head_age + head_sex + urban_rural + wealth_index + testvar + (1|surveycode) + (1|country),
                          data=df %>% filter(market_dist > 24*14))

  urban<- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                            head_age + head_sex + urban_rural + wealth_index + testvar + (1|surveycode) + (1|country),
                          data=df %>% filter(market_dist < 24))

  #Baseline
  tmp$baseline_dry_B <- tidy(baseline) %>% filter(term == "testvarDry") %>% select(estimate) %>% as.numeric
  tmp$baseline_dry_p <- tidy(baseline) %>% filter(term == "testvarDry") %>% select(statistic) %>% as.numeric
  
  tmp$baseline_wet_B <- tidy(baseline) %>% filter(term == "testvarWet") %>% select(estimate) %>% as.numeric
  tmp$baseline_wet_p <- tidy(baseline) %>% filter(term == "testvarWet") %>% select(statistic) %>% as.numeric
  tmp$baseline_AIC <- AIC(baseline)

  #With Landcover
  tmp$baseline_es_dry_B <- tidy(baseline_es) %>% filter(term == "testvarDry") %>% select(estimate) %>% as.numeric
  tmp$baseline_es_dry_p <- tidy(baseline_es) %>% filter(term == "testvarDry") %>% select(statistic) %>% as.numeric

  tmp$baseline_es_wet_B <- tidy(baseline_es) %>% filter(term == "testvarWet") %>% select(estimate) %>% as.numeric
  tmp$baseline_es_wet_p <- tidy(baseline_es) %>% filter(term == "testvarWet") %>% select(statistic) %>% as.numeric

  tmp$baseline_es_es_B <- tidy(baseline_es) %>% filter(term == "natural") %>% select(estimate) %>% as.numeric
  tmp$baseline_es_es_p <- tidy(baseline_es) %>% filter(term == "natural") %>% select(statistic) %>% as.numeric
  tmp$baseline_es_AIC <- AIC(baseline_es)

  #Rural Areas
  tmp$rural_dry_B <- tidy(rural) %>% filter(term == "testvarDry") %>% select(estimate) %>% as.numeric
  tmp$rural_dry_p <- tidy(rural) %>% filter(term == "testvarDry") %>% select(statistic) %>% as.numeric

  tmp$rural_wet_B <- tidy(rural) %>% filter(term == "testvarWet") %>% select(estimate) %>% as.numeric
  tmp$rural_wet_p <- tidy(rural) %>% filter(term == "testvarWet") %>% select(statistic) %>% as.numeric
  tmp$rural_AIC <- AIC(rural)

  #Urban Areas
  tmp$urban_dry_B <- tidy(urban) %>% filter(term == "testvarDry") %>% select(estimate) %>% as.numeric
  tmp$urban_dry_p <- tidy(urban) %>% filter(term == "testvarDry") %>% select(statistic) %>% as.numeric

  tmp$urban_wet_B <- tidy(urban) %>% filter(term == "testvarWet") %>% select(estimate) %>% as.numeric
  tmp$urban_wet_p <- tidy(urban) %>% filter(term == "testvarWet") %>% select(statistic) %>% as.numeric
  tmp$urban_AIC <- AIC(urban)
  
  if(sum(is.na(tmp)) > 0){
    stop("NAs in tmp")
  }
  
  sumdf <- bind_rows(sumdf, tmp)
  
}

write.csv(sumdf, 'categorizedSPI.csv', row.names=F)