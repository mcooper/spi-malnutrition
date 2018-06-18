setwd('G://My Drive/DHS Processed')

library(ggplot2)
library(dplyr)

hh <- read.csv('hhvars.csv')
spei <- read.csv('Coords&Precip.csv') %>%
  select(-precip_10yr_mean, -tmin_10yr_mean, -tmax_10yr_mean)
cov <- read.csv('SpatialCovars.csv')


################################
#Combine and clear workspace
################################
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spei, cov))

#rm(list=setdiff(ls(), "all")) #remove everything but our data

###########################################################
#Analyze missing data, determine which variables to keep
############################################################

all$haz_dhs <- all$haz_dhs/100

all <- all %>%
  filter(years_in_location >= 2 & is_visitor == 0)

all$related_hhhead <- all$relationship_hhhead != "Not Related"

library(hglm)

moddat <- all %>% dplyr::select(haz_dhs, age, interview_year, head_sex, hhsize, sex, population, mean_annual_precip, 
                                  head_age, market_dist, mother_years_ed, workers, related_hhhead, wealth_index, 
                                  istwin, diarrhea, fever, wealth_index, spei24, code, country) %>%
  filter(country == 'KE') %>%
  na.omit

mod <- hglm2(haz_dhs ~ age + interview_year + head_sex + hhsize + sex + population + mean_annual_precip +
               head_age + market_dist + mother_years_ed + workers + related_hhhead + wealth_index + 
               istwin + diarrhea + fever + wealth_index + spei24 + (spei24|code),
             rand.family = Gamma(link = "identity"), 
             data = moddat)

re <- mod$ranef

spei24 <- re[grepl('spei24', names(re))]
hist(spei24, 100)
