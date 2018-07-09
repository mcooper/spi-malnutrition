library(ggplot2)
library(dplyr)
library(lme4)
library(broom)

setwd('~/dhsprocessed')

hh <- read.csv('HH_data_A.csv')
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
  
  df <- df[!is.infinite(df$testvar), ]
  
  
  lowspi_baseline2 <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                              head_age + head_sex + urban_rural + wealth_index + testvar + (1|surveycode) + (1|country), 
                              data=df %>% filter(testvar < 0))
  
  highspi_baseline<- lm(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                                head_age + head_sex + urban_rural + wealth_index + testvar + surveycode + country, 
                              data=df %>% filter(testvar > 0 | market_dist > 24*14))
  
  lowspi_baseline_es<- lm(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                                  head_age + head_sex + urban_rural + wealth_index + testvar + natural + surveycode + country, 
                                data=df %>% filter(testvar < 0 | market_dist > 24*14))
  
  highspi_baseline_es<- lm(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                                   head_age + head_sex + urban_rural + wealth_index + testvar + natural + surveycode + country, 
                                 data=df %>% filter(testvar > 0 | market_dist > 24*14))
  # 
  # lowspi_rural<- lm(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
  #                           head_age + head_sex + urban_rural + wealth_index + testvar + surveycode + country, 
  #                         data=df %>% filter(testvar < 0 & market_dist > 24*14))
  # 
  # highspi_rural<- lm(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
  #                            head_age + head_sex + urban_rural + wealth_index + testvar + surveycode + country, 
  #                          data=df %>% filter(testvar > 0 & market_dist > 24*14))
  # 
  # lowspi_urban<- lm(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
  #                           head_age + head_sex + urban_rural + wealth_index + testvar + surveycode + country, 
  #                         data=df %>% filter(testvar < 0 & market_dist < 24))
  # 
  # highspi_urban<- lm(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
  #                            head_age + head_sex + urban_rural + wealth_index + testvar + surveycode + country, 
  #                          data=df %>% filter(testvar > 0 & market_dist < 24))
  
  tmp$lowspi_baseline_B <- tidy(lowspi_baseline) %>% filter(term == "testvar") %>% select(estimate) %>% as.numeric
  tmp$lowspi_baseline_p <- tidy(lowspi_baseline) %>% filter(term == "testvar") %>% select(p.value) %>% as.numeric
  
  tmp$highspi_baseline_B <- tidy(highspi_baseline) %>% filter(term == "testvar") %>% select(estimate) %>% as.numeric
  tmp$highspi_baseline_p <- tidy(highspi_baseline) %>% filter(term == "testvar") %>% select(p.value) %>% as.numeric
  
  tmp$lowspi_baseline_es_spi_B <- tidy(lowspi_baseline_es) %>% filter(term == "testvar") %>% select(estimate) %>% as.numeric
  tmp$lowspi_baseline_es_spi_p <- tidy(lowspi_baseline_es) %>% filter(term == "testvar") %>% select(p.value) %>% as.numeric
  
  tmp$lowspi_baseline_es_es_B <- tidy(lowspi_baseline_es) %>% filter(term == "natural") %>% select(estimate) %>% as.numeric
  tmp$lowspi_baseline_es_es_p <- tidy(lowspi_baseline_es) %>% filter(term == "natural") %>% select(p.value) %>% as.numeric
  
  tmp$highspi_baseline_es_spi_B <- tidy(highspi_baseline_es) %>% filter(term == "testvar") %>% select(estimate) %>% as.numeric
  tmp$highspi_baseline_es_spi_p <- tidy(highspi_baseline_es) %>% filter(term == "testvar") %>% select(p.value) %>% as.numeric
  
  tmp$lowspi_baseline_es_es_B <- tidy(lowspi_baseline_es) %>% filter(term == "natural") %>% select(estimate) %>% as.numeric
  tmp$lowspi_baseline_es_es_p <- tidy(lowspi_baseline_es) %>% filter(term == "natural") %>% select(p.value) %>% as.numeric
  # 
  # tmp$lowspi_rural_B <- tidy(lowspi_rural) %>% filter(term == "testvar") %>% select(estimate) %>% as.numeric
  # tmp$lowspi_rural_p <- tidy(lowspi_rural) %>% filter(term == "testvar") %>% select(p.value) %>% as.numeric
  # 
  # tmp$highspi_rural_B <- tidy(highspi_rural) %>% filter(term == "testvar") %>% select(estimate) %>% as.numeric
  # tmp$highspi_rural_p <- tidy(highspi_rural) %>% filter(term == "testvar") %>% select(p.value) %>% as.numeric
  # 
  # tmp$lowspi_urban_B <- tidy(lowspi_urban) %>% filter(term == "testvar") %>% select(estimate) %>% as.numeric
  # tmp$lowspi_urban_p <- tidy(lowspi_urban) %>% filter(term == "testvar") %>% select(p.value) %>% as.numeric
  # 
  # tmp$highspi_urban_B <- tidy(highspi_urban) %>% filter(term == "testvar") %>% select(estimate) %>% as.numeric
  # tmp$highspi_urban_p <- tidy(highspi_urban) %>% filter(term == "testvar") %>% select(p.value) %>% as.numeric
  
  if(sum(is.na(tmp)) > 0){
    stop("NAs in tmp")
  }
  
  sumdf <- bind_rows(sumdf, tmp)
  
}