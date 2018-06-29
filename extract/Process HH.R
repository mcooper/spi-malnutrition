#####################################################################################
#'This script creates 3 different case-complete datasets
#'
#'Dataset 1: Only critical variables, mostly at the household level, with lots of records
#'
#'Dataset 2: More detailed individual level variables, at the expense of the sample size
#'
#'Dataset 3: Similar to dataset 1, but limited to surveys from after 2000 and includes
#'more covariates that were common in post-2000 DHS surveys.  Intended for use with 
#'RS data products that only exist after 2000, such as the Hansen forest cover data.
#'
#'The variables were selected based on what was used in previous literature,
#'as well as /scope/VariableScope.csv, created from /scope/Scope_vars.R
###################################################################################

setwd('G://My Drive/DHS Processed/')

library(dplyr)

hh <- read.csv('hhvars.csv')

data1 <- hh %>% 
  select(code, interview_year, haz_dhs, age, birth_order, hhsize, sex, mother_years_ed, toilet, 
         head_age, head_sex, urban_rural, wealth_index, surveycode, country) %>%
  na.omit

data2 <- hh %>% 
  select(code, interview_year, haz_dhs, age, birth_order, hhsize, sex, mother_years_ed, toilet, 
         head_age, head_sex, urban_rural, wealth_index, otherwatersource, ever_breastfed, diarrhea, 
         is_visitor, istwin, surveycode, country) %>%
  na.omit

data2000 <- hh %>% 
  filter(interview_year > 2000) %>%
  select(code, interview_year, haz_dhs, age, birth_order, hhsize, sex, mother_years_ed, toilet, 
         head_age, head_sex, urban_rural, wealth_index, relationship_hhhead, workers, dependents,
         drinkwatersource, surveycode, country) %>%
  na.omit

write.csv(data1, 'HH_data_A.csv', row.names=F)
write.csv(data2, 'HH_data_B.csv', row.names=F)
write.csv(data2000, 'HH_data_2000.csv', row.names=F)

###################################################
# Quick test of whether data1 is better than data2
###################################################

library(lme4)

#Compare RMSE, use different datasets

mod1 <- lmer(haz_dhs~interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet + 
             head_age + head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country), data=data1)

sqrt(mean(residuals(mod1)^2))
#[1] 148.7976


mod2 <- lmer(haz_dhs~interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet + 
               head_age + head_sex + urban_rural + wealth_index + otherwatersource + ever_breastfed + diarrhea + 
               istwin + (1|surveycode) + (1|country), data=data2 %>% filter(is_visitor == 0))

sqrt(mean(residuals(mod2)^2))
#[1] 146.3615

#looks like dataset 2 performs slightly better by RMSE

#Compare AIC, using both datasets
mod3 <- lmer(haz_dhs~interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet + 
               head_age + head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country), 
               data=data2 %>% filter(is_visitor == 0))

AIC(mod2)
#[1] 5645072
AIC(mod3)
#[1] 5648004

#Again, more covariates is better.  But we have many more records from mod1, so will likely stick to that.









