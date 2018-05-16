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

all$md <- all$md/(24*7)
all$gdp <- all$gdp/1000
all$mean_annual_precip <- all$mean_annual_precip/1000
all$pop <- all$pop/100000

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
                     spei24=seq(-2.5, 2.5, 0.05),
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
  filter(spei24 < -0.45)

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

options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

init <- summary(mod_d)$coefficients[ , 1]
names(init)[names(init)=="(Intercept)"] <- "intercept"
names(init) <- gsub(' ', '', names(init))
init <- as.list(init)
init <- list(chain1=init, chain2=init, chain3=init, chain4=init)

codemap <- drought %>% select(code) %>% mutate(code_number=as.numeric(as.factor(as.character(code))))

stanDat <- list()
stanDat[["N"]] <- nrow(drought)
stanDat[["haz_dhs"]] <- drought$haz_dhs
stanDat[["toiletFlushToilet"]] <- drought$toilet == "Flush Toilet"
stanDat[["toiletOther"]] <- drought$toilet == "Other"
stanDat[["toiletPitLatrine"]] <- drought$toilet == "Pit Latrine"
stanDat[["relationship_hhheadNotRelated"]] <- drought$relationship_hhhead == "Not Related"
stanDat[["relationship_hhheadRelative"]] <- drought$relationship_hhhead == "Relative"
stanDat[["age"]] <- drought$age
stanDat[["birth_order"]] <- drought$birth_order
stanDat[["head_age"]] <- drought$head_age
stanDat[["head_sexMale"]] <- drought$head_sex == "Male"
stanDat[["sexMale"]] <- drought$sex == "Male"
stanDat[["wealth_indexMiddle"]] <- drought$wealth_index == "Middle"
stanDat[["wealth_indexPoorer"]] <- drought$wealth_index == "Poorer"
stanDat[["wealth_indexRicher"]] <- drought$wealth_index == "Richer"
stanDat[["wealth_indexRichest"]] <- drought$wealth_index == "Richest"
stanDat[["hhsize"]] <- drought$hhsize
stanDat[["diarrhea"]] <- drought$diarrhea
stanDat[["fever"]] <- drought$fever
stanDat[["breast_duration"]] <- drought$breast_duration
stanDat[["urban_ruralRural"]] <- drought$urban_rural == "Rural"
stanDat[["parents_years_ed"]] <- drought$parents_years_ed
stanDat[["gdp"]] <- drought$gdp
stanDat[["md"]] <- drought$md
stanDat[["pop"]] <- drought$pop
stanDat[["mean_annual_precip"]] <- drought$mean_annual_precip
stanDat[["spei24"]] <- drought$spei24

stanDat[["code_N"]] <- length(unique(drought$code))
stanDat[["code"]] <- as.numeric(as.factor(as.character(drought$code)))

drought_stan_code <- "
data {
  int<lower=1> N;
  
  real<lower=-600, upper=600> haz_dhs[N];

  int<lower=0, upper=1> toiletFlushToilet[N];
  int<lower=0, upper=1> toiletOther[N];
  int<lower=0, upper=1> toiletPitLatrine[N];
  int<lower=0, upper=1> relationship_hhheadNotRelated[N];
  int<lower=0, upper=1> relationship_hhheadRelative[N];
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
  int<lower=0, upper=1> urban_ruralRural[N];
  int<lower=0, upper=23> parents_years_ed[N];
  real<lower=0, upper=10> gdp[N];
  real<lower=0, upper=200> md[N];
  real<lower=0, upper=40000> pop[N];
  real<lower=0, upper=10> mean_annual_precip[N];
  real<lower=-3, upper=3> spei24[N];

  int<lower=1> code_N;       //number of sites

  int<lower=1> code[N];      //site id

}

parameters {
  real intercept;
  real toiletFlushToilet_beta;
  real toiletOther_beta;
  real toiletPitLatrine_beta;
  real relationship_hhheadNotRelated_beta;
  real relationship_hhheadRelative_beta;
  real age_beta;
  real birth_order_beta;
  real head_age_beta;
  real head_sexMale_beta;
  real sexMale_beta;
  real wealth_indexMiddle_beta;
  real wealth_indexPoorer_beta;
  real wealth_indexRicher_beta;
  real wealth_indexRichest_beta;
  real hhsize_beta;
  real diarrhea_beta;
  real fever_beta;
  real breast_duration_beta;
  real urban_ruralRural_beta;
  real parents_years_ed_beta;
  real gdp_beta;
  real md_beta;
  real pop_beta;
  real mean_annual_precip_beta;
  real spei24_beta;

  real<lower=0> sigma_e; //error sd

  vector<lower=0>[2] sigma_code; //code sd
  cholesky_factor_corr[2] L_code;
  matrix[2, code_N] z_code;
}

transformed parameters {
  matrix[2, code_N] w;

  w = diag_pre_multiply(sigma_code, L_code) * z_code; //site random effects
}


model {
  real mu;

  //priors
  L_code ~ lkj_corr_cholesky(2.0);
  to_vector(z_code) ~ normal(0,1);

  //likelihood
  for (i in 1:N){
    mu = intercept + w[1,code[i]] + (spei24_beta + w[2, code[i]]) * spei24[i] + toiletFlushToilet_beta*toiletFlushToilet[i] + toiletOther_beta*toiletOther[i] + toiletPitLatrine_beta*toiletPitLatrine[i] + relationship_hhheadNotRelated_beta*relationship_hhheadNotRelated[i] + relationship_hhheadRelative_beta*relationship_hhheadRelative[i] + age_beta*age[i] + birth_order_beta*birth_order[i] + head_age_beta*head_age[i] + head_sexMale_beta*head_sexMale[i] + sexMale_beta*sexMale[i] + wealth_indexMiddle_beta*wealth_indexMiddle[i] + wealth_indexPoorer_beta*wealth_indexPoorer[i] + wealth_indexRicher_beta*wealth_indexRicher[i] + wealth_indexRichest_beta*wealth_indexRichest[i] + hhsize_beta*hhsize[i] + diarrhea_beta*diarrhea[i] + fever_beta*fever[i] + breast_duration_beta*breast_duration[i] + urban_ruralRural_beta*urban_ruralRural[i] + parents_years_ed_beta*parents_years_ed[i] + gdp_beta*gdp[i] + md_beta*md[i] + pop_beta*pop[i] + mean_annual_precip_beta*mean_annual_precip[i];

    haz_dhs[i] ~ normal(mu, sigma_e);
  }
}
"


initmod <- stan(model_name="mode1", model_code = drought_stan_code, data=stanDat,
                iter = 2000, chains = 4, init=init)













