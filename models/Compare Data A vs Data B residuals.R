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
mod_simple <- lm(haz_dhs ~  age + birth_order + hhsize + sex + mother_years_ed + 
                     toilet + head_age + head_sex + wealth_index + 
                   as.factor(calc_birthmonth),
                   data=hhb)

mod_all <- lm(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + 
                  toilet + head_age + head_sex + wealth_index + otherwatersource + 
                  ever_breastfed + diarrhea + istwin + 
                  as.factor(calc_birthmonth),
                data=hhb)

sqrt(mean(residuals(mod_simple)^2))
sqrt(mean(residuals(mod_all)^2))

AIC(mod_simple)
AIC(mod_all)

hhb$mod_simp_resid <- residuals(mod_simple)
hhb$mod_all_resid <- residuals(mod_all)

# > cor(hhb$mod_simp_resid, hhb$mod_all_resid)
# [1] 0.9966823

#Always use dataset A!

smp <- tidy(mod_simple)
names(smp)[2:5] <- paste0('simple_', names(smp)[2:5])
all <- tidy(mod_all)
names(all)[2:5] <- paste0('all_', names(all)[2:5])

comb <- merge(smp, all, all=T)

write.csv(comb, 'C://Users/matt/Desktop/CompareAvsB.csv', row.names=F)

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
