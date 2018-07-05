setwd('G://My Drive/DHS Processed')

library(ggplot2)
library(dplyr)
library(broom)

hh <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv') %>%
  select(-precip_10yr_mean, -tmax_10yr_mean, -tmin_10yr_mean) %>%
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

all$es <- as.factor(ifelse(all$natural >= 0.75, "High", 
                 ifelse(all$natural >= 0.5, "Medium-High",
                        ifelse(all$natural >= 0.25, "Medium-Low", "Low"))))

all$precip <- as.factor(ifelse(all$spi24 > 1.5, "Flood", 
                     ifelse(all$spi24 < -1.5, "Drought", "Normal")))

all$precip <- relevel(all$precip, ref="Normal")
all$es <- relevel(all$es, ref="Low")

all <- all %>% 
  filter(spi24 < 1.5)

###################################################
#Model
####################################################
library(lme4)
high_haz_mod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet + 
              head_age + head_sex + urban_rural + wealth_index + market_dist + es*spei24gs + (1|surveycode) + (1|country),
            data=all %>% filter(haz_dhs > 1))


low_haz_mod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet + 
              head_age + head_sex + urban_rural + wealth_index + market_dist + es*spei24gs+ (1|surveycode) + (1|country),
            data=all %>% filter(haz_dhs < -1))

summary(high_haz_mod)
summary(low_haz_mod)


moddata <- data.frame(spei24gs = seq(-3, 1.5, 0.05),
                      interview_year=1992,
                      age=60,
                      birth_order=10,
                      hhsize=25,
                      sex="Male",
                      mother_years_ed=10,
                      toilet="Pit Latrine",
                      head_age=30,
                      head_sex="Male",
                      urban_rural="Rural",
                      wealth_index="Poorer",
                      surveycode="AL-5-1",
                      country="AL",
                      market_dist=0)

low <- moddata
low$es <- "Low"
midlow <- moddata
midlow$es <- "Medium-Low"
midhigh <- moddata
midhigh$es <- "Medium-High"
high <- moddata
high$es <- "High"

preddat <- bind_rows(low, midlow, midhigh, high)

preddat$fits <- predict(low_haz_mod, preddat)

library(ggplot2)

ggplot(preddat) + geom_line(aes(x=spei24gs, y=fits, color=es)) + theme_bw()

reg <- read.csv('regions.csv')

sumry <- all %>% select(latitude, longitude, es, natural, market_dist, country, urban_rural) %>% unique

sumry <- merge(reg, sumry)

ggplot(sumry) + geom_histogram(aes(x=log(market_dist), fill=urban_rural))



write.csv(sumry, 'C://Users/matt/Desktop/ES.csv', row.names=F)
