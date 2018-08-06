library(ggplot2)
library(dplyr)
library(lme4)
library(broom)

setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
hhb <- read.csv('HH_data_B.csv')

################################
#Compare A and B.
################################

#Create A by subsetting B
mod_simple <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + 
                     toilet + head_age + head_sex + urban_rural + wealth_index + 
                     (1|surveycode) + (1|country) + (1|surveycode/calc_birthmonth),
                   data=hhb)

mod_all <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + 
                  toilet + head_age + head_sex + urban_rural + wealth_index + otherwatersource + 
                  ever_breastfed + diarrhea + istwin + 
                  (1|surveycode) + (1|country) + (1|surveycode/calc_birthmonth),
                data=hhb)

AIC(mod_simple)
AIC(mod_all)

hhb$mod_simp_resid <- residuals(mod_simple)
hhb$mod_all_resid <- residuals(mod_all)

# > cor(hhb$mod_simp_resid, hhb$mod_all_resid)
# [1] 0.9966823

#Always use dataset A!

#####################################
#Lets see if Mother's height matters?
#####################################

hh <- read.csv('hhvars.csv')

hha_mom <- hh[ , c(names(hha), 'mother_haz', 'latitude', 'longitude')] %>% na.omit

mod_simple1 <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + 
                     toilet + head_age + head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country),
                   data=hha_mom)

mod_simple2 <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + 
                      toilet + head_age + head_sex + urban_rural + wealth_index + mother_haz + (1|surveycode) + (1|country),
                    data=hha_mom)

AIC(mod_simple1)
AIC(mod_simple2)

hha_mom$mod_simp_resid <- residuals(mod_simple1)
hha_mom$mod_mom_resid <- residuals(mod_simple2)


mod1 <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + 
                      toilet + head_age + head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country),
                    data=hha)


mod2 <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + 
               toilet + head_age + head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country) + (1|code),
             data=hha)

AIC(mod1)
AIC(mod2)
