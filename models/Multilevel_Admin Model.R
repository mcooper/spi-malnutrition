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
               istwin + diarrhea + fever + wealth_index + spei24 + (1|country) + (1|code), data = all %>% filter(urban_rural == 'Rural'))

re <- ranef(combmod)$Adm1
hist(re$spei24)
re$Adm1 <- rownames(re)
re$spei24_fix <- fixef(combmod)[['spei24']]

codesum <- all %>%
  group_by(Adm1) %>%
  summarize(spei24_mean = mean(spei24, na.rm=T),
            haz_dhs_mean = mean(haz_dhs, na.rm=T),
            longitude=mean(longitude),
            latitude=mean(latitude))

re <- merge(re, codesum, all.x=T, all.y=F)

re$hazval <- ifelse(re$haz_dhs_mean < -1.75, "Low HAZ",
                    ifelse(re$haz_dhs_mean > -0.85, "High HAZ", "Average HAZ"))

re$speival <- ifelse(re$spei24_mean < -1, "Low SPEI",
                     ifelse(re$spei24_mean > 0.5, "High SPEI", "Average SPEI"))

ggplot(re) + geom_histogram(aes(x=spei24, fill=hazval), bins=100) + 
  ggtitle("Histogram of Random Effects for SPEI on HAZ, subset by HAZ Values")
ggplot(re) + geom_histogram(aes(x=spei24, fill=speival), bins=100) + 
  ggtitle("Histogram of Random Effects for SPEI on HAZ, subset by SPEI Values")

ggplot(re) + geom_point(aes(x=longitude, y=latitude, color=spei24), size=0.25) + 
  scale_color_gradient2(low="green", high="red", mid="yellow")


