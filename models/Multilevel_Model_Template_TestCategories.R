setwd('G://My Drive/DHS Processed')

library(ggplot2)
library(dplyr)
library(broom)

hh <- read.csv('HH_data_A.csv')
spei <- read.csv('Coords&Precip.csv') %>%
  select(code, interview_year, interview_month, spei24, spi24) %>%
  unique
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')

################################
#Combine
################################
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spei, cov, lc))

###########################################################
#Prep Data
############################################################

all$haz_dhs <- all$haz_dhs/100
all$whz_dhs <- all$whz_dhs/100

all$es <- as.factor(ifelse(all$natural > 0.75, "High", 
                 ifelse(all$natural > 0.5, "Medium-High",
                        ifelse(all$natural > 0.25, "Medium-Low", "Low"))))

all$precip <- as.factor(ifelse(all$spei24 > 1.5, "Flood", 
                     ifelse(all$spei24 < -1.5, "Drought", "Normal")))

all$precip <- relevel(all$precip, ref="Normal")

library(lme4)

mod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet + 
              head_age + head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country) + (precip|code),
            data=all)


summary(mod)




fedmod <- lmer(haz_dhs ~ age + interview_year + head_sex + hhsize + sex + gdp + population + mean_annual_precip +
                 head_age + market_dist + mother_years_ed + workers + related_hhhead + wealth_index + 
                 istwin + diarrhea + fever + wealth_index + spi24 + (1|country), data = moddat)

feremod <- lmer(haz_dhs ~ age + interview_year + head_sex + hhsize + sex + gdp + population + mean_annual_precip +
                   head_age + market_dist + mother_years_ed + workers + related_hhhead + wealth_index + 
                   istwin + diarrhea + fever + wealth_index + spi24 + (1|country) + (spi24|code), data = moddat)

remod <- lmer(haz_dhs ~ age + interview_year + head_sex + hhsize + sex + gdp + population + mean_annual_precip +
                head_age + market_dist + mother_years_ed + workers + related_hhhead + wealth_index + 
                istwin + diarrhea + fever + wealth_index + (1|country) + (spi24|code), data = moddat)


moddat$resid <- residuals(combmod)


ggplot(moddat) + geom_histogram(aes(x=resid, fill=as.factor(interview_year)))

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


