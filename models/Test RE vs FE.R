setwd('G://My Drive/DHS Processed')

library(ggplot2)
library(dplyr)
library(broom)

hh <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
spei_ind <- read.csv('PrecipIndices_Individual.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')

################################
#Combine and clear workspace
################################
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spei, spei_ind, cov, lc))

#rm(list=setdiff(ls(), "all")) #remove everything but our data

###########################################################
#Analyze missing data, determine which variables to keep
############################################################

library(lme4)

all <- all %>% filter(!is.infinite(spei_age))

femod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + 
                mother_years_ed + toilet + head_age + head_sex + urban_rural + 
                wealth_index + spei_age + (1|surveycode) + (1|country), data = all %>% filter(spei_age < 1))

feremod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + 
                  mother_years_ed + toilet + head_age + head_sex + urban_rural + 
                  wealth_index + spei_age + (spei_age|code), data = all)

remod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + 
                mother_years_ed + toilet + head_age + head_sex + urban_rural + 
                wealth_index + (spei_age|code), data = all %>% filter(country %in% c("UG")))

dummod <- lm(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + 
                 mother_years_ed + toilet + head_age + head_sex + urban_rural + 
                 wealth_index + spei_age*code, data = all %>% filter(country %in% c("UG")))


AIC(femod)
AIC(feremod)
AIC(remod)
AIC(dummod)



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


