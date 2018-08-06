library(dplyr)
library(lme4)

setwd('G://My Drive/DHS Processed')

hh <- read.csv('HH_data_A.csv')
spi <-read.csv('Coords&Precip.csv')
wdpa <- read.csv('WDPA_extract.csv')

all <- Reduce(merge, list(hh, spi, wdpa))

all$PA <- ifelse(all$nearestPaDist > 25000 | is.na(all$nearestPaDist), "No PA", as.character(all$nearestPaClass)) %>%
  as.factor %>%
  relevel(ref = "No PA")

all$spei24 <- ifelse(all$spei24 > 1.5, "Wet", 
                     ifelse(all$spei24 < -1.5, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

mod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + 
              toilet + head_age + head_sex + wealth_index + (1|surveycode) + (1|country) + 
              (1|calc_birthmonth) + spei24*PA, data=all %>% filter(urban_rural=='Rural'))

summary(mod)
