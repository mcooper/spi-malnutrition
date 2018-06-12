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

library(lme4)

combmod <- lmer(haz_dhs ~ age + interview_year + head_sex + hhsize + sex + gdp + population + mean_annual_precip +
               head_age + market_dist + mother_years_ed + workers + related_hhhead + wealth_index + 
               istwin + diarrhea + fever + wealth_index + (1 + spei24|code), data = all)

re <- ranef(combmod)$code
hist(re$spei24)
re$code <- rownames(re)
re$spei24_fix <- fixef(combmod)[['spei24']]

codesum <- all %>%
  group_by(code) %>%
  summarize(spei24_mean = mean(spei24, na.rm=T),
            haz_dhs_mean = mean(haz_dhs, na.rm=T))

re <- merge(re, codesum, all.x=T, all.y=F)

re$hazval <- ifelse(re$haz_dhs_mean < -2, "Low HAZ",
                    ifelse(re$haz_dhs_mean > 0, "High HAZ", "Average HAZ"))

re$speival <- ifelse(re$spei24_mean < -1.5, "Low SPEI",
                     ifelse(re$spei24_mean > 1.5, "High SPEI", "Average SPEI"))

ggplot(re) + geom_histogram(aes(x=spei24, fill=hazval), bins=100) + 
  ggtitle("Histogram of Random Effects for SPEI on HAZ, subset by HAZ Values")
ggplot(re) + geom_histogram(aes(x=spei24, fill=speival), bins=100) + 
  ggtitle("Histogram of Random Effects for SPEI on HAZ, subset by SPEI Values")

##############################
#Try for just Kenya
###############################

sel <- all %>%
  filter(all$country == 'KE')

kemod <- lmer(haz_dhs ~ age + interview_year + head_sex + hhsize + sex + gdp + population + mean_annual_precip +
                  head_age + market_dist + mother_years_ed + workers + related_hhhead + wealth_index + 
                  istwin + diarrhea + fever + wealth_index + (1|code) + spei24 + (spei24|code), data = sel)

ke_re <- ranef(kemod)$code

ke_re$code <- row.names(ke_re)

codesum <- sel %>%
  group_by(code) %>%
  summarize(spei24_mean = mean(spei24, na.rm=T),
            haz_dhs_mean = mean(haz_dhs, na.rm=T))

ke_re <- merge(ke_re, codesum, all.x=T, all.y=F)

ke_re$hazval <- ifelse(ke_re$haz_dhs_mean < -2, "Low HAZ",
                    ifelse(ke_re$haz_dhs_mean > 0, "High HAZ", "Average HAZ"))

ke_re$speival <- ifelse(ke_re$spei24_mean < -1.5, "Low SPEI",
                     ifelse(ke_re$spei24_mean > 1.5, "High SPEI", "Average SPEI"))


ggplot(ke_re) + geom_histogram(aes(x=spei24, fill=hazval), bins=100) + 
  ggtitle("Histogram of Random Effects for SPEI on HAZ, subset by HAZ Values in Kenya")
ggplot(ke_re) + geom_histogram(aes(x=spei24, fill=speival), bins=100) + 
  ggtitle("Histogram of Random Effects for SPEI on HAZ, subset by SPEI Values in Kenya")
