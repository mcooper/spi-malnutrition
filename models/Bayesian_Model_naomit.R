library(AzureSMR)
library(dplyr)

az <- read.csv('~/.azure/.AzureSMR', stringsAsFactors = F)

sc <- createAzureContext(tenantID = az$tenantid,
                         clientID = az$appid,
                         authKey = az$authkey)

az_csv <- function(csv){
  read.csv(text=azureGetBlob(sc, 
                            storageAccount=az$storage_acct, 
                            container="dhsprocessed",
                            blob=csv,
                            type="text",
                            storageKey = az$storageKey))
}

hh <- az_csv('hhvars.csv')
gdp <- az_csv('country_gdp.csv')
farm <- az_csv('FarmingSystems.csv')
md <- az_csv('MarketDist.csv')
pop <- az_csv('PopPer100sqkm.csv')
spi <- az_csv('Coords&Precip.csv')

#################################
#Process pop and market dist data
#################################
pop00 <- merge(select(pop, code, pop=pop00), data.frame(interview_year=seq(1988, 2002)))
pop05 <- merge(select(pop, code, pop=pop05), data.frame(interview_year=seq(2003, 2007)))
pop10 <- merge(select(pop, code, pop=pop10), data.frame(interview_year=seq(2008, 2012)))
pop15 <- merge(select(pop, code, pop=pop15), data.frame(interview_year=seq(2013, 2017)))

pop <- Reduce(bind_rows, list(pop00, pop05, pop10, pop15))


md00 <- merge(select(md, code, md=market2000), data.frame(interview_year=seq(1988, 2007)))
md15 <- merge(select(md, code, md=market2015), data.frame(interview_year=seq(2008, 2016)))

md <- Reduce(bind_rows, list(md00, md15))

################################
#Combine and clear workspace
################################
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, gdp, farm, md, pop, spi))

all$md <- all$md/24
all$gdp <- all$gdp/1000
all$mean_annual_precip <- all$mean_annual_precip/1000
all$pop <- all$pop/100
all$haz_dhs <- all$haz_dhs/100

all <- all %>%
  filter(is_visitor == 0 & years_in_location >= 2)

#Relevel factors
all <- all %>%
  mutate(toilet=relevel(toilet, ref="No Facility"),
         relationship_hhhead=relevel(relationship_hhhead, ref="Immediate Family"),
         otherwatersource=relevel(otherwatersource, ref="Surface Water"),
         drinkwatersource=relevel(drinkwatersource, ref="Surface Water"),
         wealth_index=relevel(wealth_index, ref="Poorest"))

all$related_hhhead <- all$relationship_hhhead == "Not Related"

sel <- all %>%
  select(code, surveycode, country, interview_year, toilet, relationship_hhhead, age, birth_order, haz_dhs, head_age, head_sex, hhsize,
         sex, wealth_index, diarrhea, fever, breast_duration, urban_rural,
         parents_years_ed, gdp, farm_system, md, pop, spei24, mean_annual_precip, latitude, longitude) %>%
  na.omit

###################################################
#First run GAM model to determine inflection point
###################################################

library(mgcv)
library(ggplot2)

mod <- gam(haz_dhs ~ s(spei24, bs='cr') + toilet + relationship_hhhead + age + birth_order + head_age + head_sex + hhsize + sex + wealth_index + diarrhea + fever + 
             breast_duration + urban_rural + parents_years_ed + gdp + md + pop + mean_annual_precip + surveycode, data=sel)  

moddata = data.frame(age = mean(sel$age, na.rm=T),
                     interview_year = 2007,
                     head_sex = 'Male',
                     hhsize = mean(sel$hhsize, na.rm=T),
                     sex = "Male",
                     gdp = mean(sel$gdp, na.rm=T),
                     pop = mean(sel$pop, na.rm=T),
                     head_age = mean(sel$head_age, na.rm=T),
                     md = mean(sel$md, na.rm=T),
                     wealth_index = "Middle", 
                     related_hhhead = TRUE,
                     diarrhea = mean(sel$diarrhea, na.rm=T),
                     fever = mean(sel$fever, na.rm=T),
                     country = 'SN', 
                     surveycode = 'SN-4-2',
                     mean_annual_precip=mean(sel$mean_annual_precip, na.rm=T),
                     spei24=seq(-2.5, 2.5, 0.1),
                     toilet="Pit Latrine",
                     relationship_hhhead="Immediate Family",
                     birth_order=3,
                     breast_duration=16,
                     urban_rural="Rural",
                     parents_years_ed=4)

fits = predict(mod, moddata, type='response', se=T)
predicts = data.frame(moddata, fits) %>% 
  mutate(lower = fit - 1.96*se.fit,
         upper = fit + 1.96*se.fit)

ggplot(aes(x=spei24,y=fit), data=predicts) +
  geom_ribbon(aes(ymin = lower, ymax=upper), fill='gray90') +
  geom_line(color='#1e90ff') + 
  theme_bw()

###################################################
#Then run GLMMs to determine MCMC starting points
###################################################

library(lme4)

drought <- sel %>% 
  filter(spei24 < -0.5)

plot(drought$longitude, drought$latitude, pch=16, cex=0.2)

flood <- sel %>%
  filter(spei24 > 1.25)

plot(flood$longitude, flood$latitude, pch=16, cex=0.2)

mod_d <- lmer(haz_dhs ~ spei24 + toilet + relationship_hhhead + age + birth_order + head_age + head_sex + hhsize + sex + wealth_index + diarrhea + fever + 
               breast_duration + urban_rural + parents_years_ed + gdp + md + pop + mean_annual_precip + (1 | code) + (spei24|code), data=drought)

mod_f <- lmer(haz_dhs ~ spei24  + toilet + relationship_hhhead + age + birth_order + head_age + head_sex + hhsize + sex + wealth_index + diarrhea + fever + 
               breast_duration + urban_rural + parents_years_ed + gdp + md + pop + mean_annual_precip + (1 | code) + (spei24|code), data=flood)

############################################
#Stan-tastic models
###########################################

library(rstan)

model.matrix(~sel$toilet)


drought_stan_code <- "
data {
  int<lower=1> N;
  int<lower=1> code_N;
  
  real<lower=-6, upper=6> haz_dhs[N];

  int<lower=0, upper=1> toiletFlushToilet[N];
  int<lower=0, upper=1> toiletOther[N];
  int<lower=0, upper=1> toiletPitLatrine[N];
  
  int<lower=0, upper=1> relationship_hhheadNotRelated
  int<lower=0, upper=1> relationship_hhheadRelative

  int<lower=0, upper=59> age[N];

  int<lower=0, upper=18> birth_order[N];

  int<lower=14, upper=97> head_age[N];

  int<lower=0, upper=1> head_sexMale[N];

  int<lower=0, upper=1> sexMale[N];

  int<lower=0, upper=1> wealth_indexMiddle[N];
  int<lower=0, upper=1> wealth_indexPoorer[N];
  int<lower=0, upper=1> wealth_indexRicher[N];
  int<lower=0, upper=1> wealth_indexRichest[N];

  int<lower=2, upper=53> hhsize[N];

  int<lower=0, upper=1> diarrhea[N];

  int<lower=0, upper=1> fever[N];

  int<lower=0, upper=59> breast_duration[N];

  int<lower=0, upper=1> urban_ruralRural [N];
  
  parents_years_ed[N];
  
  gdp[N];
  
  farm_system[N];
  
  md[N];
  
  pop[N];
  
  mean_annual_precip[N];


  spei24
}




















