setwd('G://My Drive/DHS Processed/')

hh <- read.csv('hhvars.csv')
spi <- read.csv('Coords&Precip.csv')
cov <- read.csv('SpatialCovars.csv')

################################
#Combine and clear workspace
################################

all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spi, cov))

library(dplyr)

sumfun <- function(v){sum(is.na(v))/length(v)}

survey_summary <- all %>%
  group_by(surveycode) %>%
  summarize_at(c("toilet", "relationship_hhhead", "otherwatersource", "drinkwatersource", "age", "birth_order", 
                 "father_alive", "haz_dhs", "head_age", "head_sex", "height", "hhsize", "mother_alive", "sex",
                 "urban_rural", "watersource_dist", "waz_dhs", "wealth_factor", "wealth_index", "weight", "mother_age",
                 "mother_years_ed", "mother_height", "mother_haz", "father_age", "father_years_ed", "father_height", 
                 "father_haz", "workers", "dependents", "birth_weight", "breast_duration", "diarrhea", "fever", 
                 "is_visitor", "istwin", "mother_smokes", "parasite_drugs", "parents_years_ed", "years_in_location",
                 "ever_breastfed", "breastfeeding"), sumfun) %>%
  merge(all %>% group_by(surveycode) %>% summarize(interview_year=max(interview_year)))

write.csv(survey_summary, 'C://Git/spi-malnutrition/scope/VariableScope.csv', row.names=F)
