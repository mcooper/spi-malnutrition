library(ggplot2)
library(dplyr)
library(lme4)
library(broom)
library(stargazer)

setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
hhb <- read.csv('HH_data_B.csv')

################################
#Compare A and B.
################################

mod_simple <- lm(haz_dhs ~  age + birth_order + sex + 
                   as.factor(calc_birthmonth) + mother_years_ed + 
                   toilet + hhsize + head_age + head_sex + wealth_index,
                 data=hhb)

mod_all <- lm(haz_dhs ~  age + birth_order + sex + 
                as.factor(calc_birthmonth) + mother_years_ed + 
                toilet + hhsize + head_age + head_sex + wealth_index + 
                ever_breastfed + diarrhea + istwin + otherwatersource,
              data=hhb)

sqrt(mean(residuals(mod_simple)^2))
sqrt(mean(residuals(mod_all)^2))

AIC(mod_simple)
AIC(mod_all)

hhb$mod_simp_resid <- residuals(mod_simple)
hhb$mod_all_resid <- residuals(mod_all)

mean(abs(hhb$mod_simp_resid)) -
  mean(abs(hhb$mod_all_resid))

# > cor(hhb$mod_simp_resid, hhb$mod_all_resid)
# [1] 0.9966823

#Always use dataset A!

########################
#Write Results to Latex
########################

stg <- stargazer(mod_simple, mod_all, title="Comparison of Model with 10 vs 14 Covariates",
                 #out = 'G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/tables/S1.tex',
                 column.labels=c("10 Covariates", "14 Covariates"),
                 dep.var.labels.include=FALSE,
                 dep.var.caption='',
                 add.lines=list(c("MAE", round(mean(abs(residuals(mod_simple))), 3), round(mean(abs(residuals(mod_all))), 3)),
                                c("AIC", AIC(mod_simple), AIC(mod_all))),
                 covariate.labels=c('Age', "Birth Order", "Child is Male", "Birthmonth - February", "Birthmonth - March",
                                    "Birthmonth - April", "Birthmonth - May", "Birthmonth - June", "Birthmonth - July", "Birthmonth - August", "Birthmonth - September",
                                    "Birthmonth - October", "Birthmonth - November", "Birthmonth - December", 
                                    "Mother's Years of Education",
                                    "Toilet - No Facility", "Toilet - Other", "Toilet - Pit Latrine", "Household Size", 
                                    "Household Head Age", "Household Head is Male", 
                                    "Wealth Index - Poorer", "Wealth Index - Poorest", "Wealth Index - Richer", "Wealth Index - Richest",
                                    "Child Was Ever Breastfed", "Child Had Diarrhea in Previous Two Weeks", "Child Is Twin",
                                    "Other Water Source - Purchased", "Other Water Source - Surface Water", "Other Water Source - Tube Well",
                                    "Intercept"))

#Need to make manual edits to table based on this:
#https://tex.stackexchange.com/questions/424435/help-with-long-table-from-stargazer

stg <- stg[c(1, 2, 3, 7, 5, 6, seq(8, (length(stg) - 1)))]
stg <- gsub('tabular', 'longtable', stg)
stg <- paste0(stg, collapse='\n')
cat(stg, file = 'G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/tables/S1.tex')
