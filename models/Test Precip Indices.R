library(dplyr)
library(lme4)
library(MASS)
library(broom)
library(stargazer)

setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')
spei_age <- read.csv('PrecipIndices_Individual.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, spei_age)) %>%
  filter(builtup < 20 & bare < 95) %>%
  na.omit

for (i in c("spei12", "spei24", "spei36", "spei12gs", "spei24gs", "spei36gs", 
            "spei_ageutero", "spei_gs_ageutero")){
  all <- all[!is.infinite(all[ , i]) & !is.na(all[ , i]) & !is.nan(all[ , i]), ]
}

for (i in c("spei12", "spei24", "spei36", "spei12gs", "spei24gs", "spei36gs", 
            "spei_ageutero", "spei_gs_ageutero")){
  all$spei <- all[ , i]
  
  print(i)
  
  assign(i, lmer(haz_dhs ~ age + birth_order + sex + as.factor(calc_birthmonth) + 
                   mother_years_ed + toilet + hhsize + 
         head_age + head_sex + wealth_index + (1|country) + (1|surveycode) + (1|interview_year) + 
         spei + spei^2, data=all))
}

stg1 <- stargazer(spei12, spei12gs, spei24, spei24gs, title="Modeling Child Nutrition With SPEI Calculated at Various Timeframes (Part 1)",
                 #out = 'G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/tables/S2.tex',
                 column.labels=c("12-Month", "12-Month Growing Season",
                                 "24-Month", "24-Month Growing Season"),
                 dep.var.labels.include=FALSE,
                 dep.var.caption='',
                 covariate.labels=c('Age', "Birth Order", "Child is Male", "Birthmonth - February", "Birthmonth - March",
                                    "Birthmonth - April", "Birthmonth - May", "Birthmonth - June", "Birthmonth - July", "Birthmonth - August", "Birthmonth - September",
                                    "Birthmonth - October", "Birthmonth - November", "Birthmonth - December", 
                                    "Mother's Years of Education",
                                    "Toilet - No Facility", "Toilet - Other", "Toilet - Pit Latrine", "Household Size", 
                                    "Household Head Age", "Household Head is Male", 
                                    "Wealth Index - Poorer", "Wealth Index - Poorest", "Wealth Index - Richer", "Wealth Index - Richest",
                                    "SPEI",
                                    "Intercept"))

#Need to make manual edits to table based on this:
#https://tex.stackexchange.com/questions/424435/help-with-long-table-from-stargazer

stg1 <- stg1[c(1, 2, 3, 7, 5, 6, seq(8, (length(stg1) - 1)))]
stg1 <- gsub('tabular', 'longtable', stg1)
stg1 <- paste0(stg1, collapse='\n')
cat(stg1, file = 'G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/tables/S2.tex')


stg2 <- stargazer(spei36, spei36gs, spei_gs_ageutero, spei_gs_ageutero, title="Modeling Child Nutrition With SPEI Calculated at Various Timeframes (Part 2)",
                  #out = 'G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/tables/S3.tex',
                  column.labels=c("36-Month", "36-Month Growing Season",
                                  "Child's Age", "Child's Age Growing Season"),
                  dep.var.labels.include=FALSE,
                  dep.var.caption='',
                  covariate.labels=c('Age', "Birth Order", "Child is Male", "Birthmonth - February", "Birthmonth - March",
                                     "Birthmonth - April", "Birthmonth - May", "Birthmonth - June", "Birthmonth - July", "Birthmonth - August", "Birthmonth - September",
                                     "Birthmonth - October", "Birthmonth - November", "Birthmonth - December", 
                                     "Mother's Years of Education",
                                     "Toilet - No Facility", "Toilet - Other", "Toilet - Pit Latrine", "Household Size", 
                                     "Household Head Age", "Household Head is Male", 
                                     "Wealth Index - Poorer", "Wealth Index - Poorest", "Wealth Index - Richer", "Wealth Index - Richest",
                                     "SPEI",
                                     "Intercept"))

#Need to make manual edits to table based on this:
#https://tex.stackexchange.com/questions/424435/help-with-long-table-from-stargazer

stg2 <- stg2[c(1, 2, 3, 7, 5, 6, seq(8, (length(stg2) - 1)))]
stg2 <- gsub('tabular', 'longtable', stg2)
stg2 <- paste0(stg2, collapse='\n')
cat(stg2, file = 'G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/tables/S3.tex')
