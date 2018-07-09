library(dplyr)

setwd('~/dhsprocessed')

hh <- read.csv('HH_data_A.csv')
spi <- read.csv('PrecipIndices.csv') %>%
  select(-precip_10yr_mean, -tmax_10yr_mean, -tmin_10yr_mean)
cov <- read.csv('SpatialCovars.csv')

################################
#Combine and clear workspace
################################
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spi, cov))

all <- all %>%
  filter(country %in% c('KE', 'RW', 'TZ', 'UG'))

#Relevel factors
all <- all %>%
  mutate(toilet=relevel(toilet, ref="No Facility"),
         wealth_index=relevel(wealth_index, ref="Poorest"))

all <- all %>% filter(spi24 < 1)

#Get beta estimates from LMER
library(lme4)

lmermod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet + head_age + 
                  head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country), data=all)

interview_year <- 2.883e+00
age <- -1.476e+00
birth_order <- -4.228e-01
hhsize <- 7.733e-01
sexMale <- -1.417e+01
mother_years_ed <- 1.767e+00
toiletFlushToilet <- -2.826e+00
toiletOther <- 8.994e+00
toiletPitLatrine <- -1.096e+01
head_age <- 1.889e-01
head_sexMale <- -1.244e+00
urban_ruralUrban <- 8.332e+00
wealth_indexMiddle <- 1.421e+01
wealth_indexPoorer <- 7.701e+00
wealth_indexRicher <- 2.988e+01
wealth_indexRichest <- 6.384e+01

ag_pct_gdp <- 20/mean(all$ag_pct_gdp)
forest <- -2.5/mean(all$forest)
gdp <- -10/mean(all$gdp)
government_effectiveness <- -10/mean(all$government_effectiveness)
irrigation <- -10/mean(all$irrigation)
market_dist <- 75/mean(all$market_dist)
ndvi <- -2.5/mean(all$ndvi)
population <- 20/mean(all$population)
crop_prod <- -5/mean(all$crop_prod)
fieldsize <- -5/mean(all$fieldsize)
nutritiondiversity <- -5/mean(all$nutritiondiversity)


all$spei_coef <- ag_pct_gdp*all$ag_pct_gdp + forest*all$forest + gdp*all$gdp + government_effectiveness*all$government_effectiveness + 
  irrigation*all$irrigation + market_dist*all$market_dist + ndvi*all$ndvi + population*all$population + 
  crop_prod*all$crop_prod + fieldsize*all$fieldsize + nutritiondiversity*all$nutritiondiversity + 50

all$haz_sim <- all$spei_coef*all$spei24 + age*all$age + birth_order*all$birth_order + hhsize*all$hhsize + 
  sexMale*as.numeric(all$sex=="Male") + interview_year*all$interview_year + mother_years_ed*all$mother_years_ed + 
  toiletFlushToilet*as.numeric(all$toilet == "Flush Toilet") + toiletOther*as.numeric(all$toilet == "Other") + 
  toiletPitLatrine*as.numeric(all$toilet == "Pit Latrine") + head_age*all$head_age + head_sexMale*as.numeric(all$head_sex == "Male") +
  urban_ruralUrban*as.numeric(all$urban_rural == "Urban") + wealth_indexMiddle*as.numeric(all$wealth_index=="Middle") + 
  wealth_indexPoorer*as.numeric(all$wealth_index=="Poorer") + wealth_indexRicher*as.numeric(all$wealth_index=="Richer") +
  wealth_indexRichest*as.numeric(all$wealth_index=="Richest") - 6000 + rnorm(nrow(all), 0, 15)

############################################
#Stan-tastic models
###########################################
library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

codemap <- all %>% 
  mutate(code_number=as.numeric(as.factor(as.character(Adm2))),
         survey_number=as.numeric(as.factor(as.character(surveycode)))) %>%
  group_by(surveycode, survey_number, code_number, latitude, longitude, urban_rural, ag_pct_gdp, 
           bare, precip_10yr_mean, forest, gdp, government_effectiveness, irrigation, market_dist, 
           ndvi, population, stability_violence, tmax_10yr_mean, tmin_10yr_mean, crop_prod,
           fieldsize, nutritiondiversity) %>% 
  summarize(size=n()) %>%
  data.frame()

stanDat <- list()
stanDat[["N"]] <- nrow(all)
stanDat[["haz_sim"]] <- all$haz_sim
stanDat[["toiletFlushToilet"]] <- all$toilet == "Flush Toilet"
stanDat[["toiletOther"]] <- all$toilet == "Other"
stanDat[["toiletPitLatrine"]] <- all$toilet == "Pit Latrine"
stanDat[["age"]] <- all$age
stanDat[["birth_order"]] <- all$birth_order
stanDat[["head_age"]] <- all$head_age
stanDat[["head_sexMale"]] <- all$head_sex == "Male"
stanDat[["sexMale"]] <- all$sex == "Male"
stanDat[["wealth_indexMiddle"]] <- all$wealth_index == "Middle"
stanDat[["wealth_indexPoorer"]] <- all$wealth_index == "Poorer"
stanDat[["wealth_indexRicher"]] <- all$wealth_index == "Richer"
stanDat[["wealth_indexRichest"]] <- all$wealth_index == "Richest"
stanDat[["hhsize"]] <- all$hhsize
stanDat[["urban_ruralRural"]] <- all$urban_rural == "Rural"
stanDat[["mother_years_ed"]] <- all$mother_years_ed
stanDat[["spei24"]] <- all$spei24

#Spatial Covariates
stanDat[["ag_pct_gdp"]] <- codemap$ag_pct_gdp
stanDat[["forest"]] <- codemap$forest
stanDat[["gdp"]] <- codemap$gdp
stanDat[["government_effectiveness"]] <- codemap$government_effectiveness
stanDat[["irrigation"]] <- codemap$irrigation
stanDat[["market_dist"]] <- codemap$market_dist
stanDat[["ndvi"]] <- codemap$ndvi
stanDat[["population"]] <- codemap$population
stanDat[["crop_prod"]] <- codemap$crop_prod
stanDat[["fieldsize"]] <- codemap$fieldsize
stanDat[["nutritiondiversity"]] <- codemap$nutritiondiversity

stanDat[["code_N"]] <- length(unique(all$code))
stanDat[["code"]] <- as.numeric(as.factor(as.character(all$code)))

stan_code <- "
data {
int<lower=1> N;

real<lower=-1500, upper=600> haz_sim[N];

int<lower=0, upper=1> toiletFlushToilet[N];
int<lower=0, upper=1> toiletOther[N];
int<lower=0, upper=1> toiletPitLatrine[N];
int<lower=0> age[N];
int<lower=0> birth_order[N];
int<lower=0> head_age[N];
int<lower=0, upper=1> head_sexMale[N];
int<lower=0, upper=1> sexMale[N];
int<lower=0, upper=1> wealth_indexMiddle[N];
int<lower=0, upper=1> wealth_indexPoorer[N];
int<lower=0, upper=1> wealth_indexRicher[N];
int<lower=0, upper=1> wealth_indexRichest[N];
int<lower=0> hhsize[N];
int<lower=0, upper=1> urban_ruralRural[N];
int<lower=0> mother_years_ed[N];

real<lower=-6, upper=6> spei24[N];

int<lower=1> code_N;       //number of sites

real<lower=0>  ag_pct_gdp [code_N];
real<lower=0>  forest [code_N];
real<lower=0>  gdp [code_N];
real<lower=-10, upper=10>  government_effectiveness [code_N];
real<lower=0>  irrigation [code_N];
real<lower=0>  market_dist [code_N];
real<lower=0>  ndvi [code_N];
real<lower=0>  population [code_N];
real<lower=0>  crop_prod [code_N];
real<lower=0>  fieldsize [code_N];
real<lower=0>  nutritiondiversity [code_N];

int<lower=1> code[N];      //site id

}

parameters {
real intercept;
real toiletFlushToilet_beta;
real toiletOther_beta;
real toiletPitLatrine_beta;
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
real urban_ruralRural_beta;
real mother_years_ed_beta;
real<lower=0> sigma_e; //error sd

real ag_pct_gdp_geobeta;
real forest_geobeta;
real gdp_geobeta;
real government_effectiveness_geobeta;
real irrigation_geobeta;
real market_dist_geobeta;
real ndvi_geobeta;
real population_geobeta;
real crop_prod_geobeta;
real fieldsize_geobeta;
real nutritiondiversity_geobeta;

real geo_intercept; 
real<lower=0> geo_sigma_e;


vector<lower=0>[code_N] re_spei24_beta;
vector[code_N] re_intercept;

real<lower=0> re_intercept_sigma;

}


model {
real mu;
real geo_mu;

sigma_e ~ cauchy(0, 2);
geo_sigma_e ~ cauchy(0, 2);


for (i in 1:N){

  geo_mu = geo_intercept + ag_pct_gdp[code[i]]*ag_pct_gdp_geobeta + forest[code[i]]*forest_geobeta + gdp[code[i]]*gdp_geobeta + government_effectiveness[code[i]]*government_effectiveness_geobeta + irrigation[code[i]]*irrigation_geobeta + market_dist[code[i]]*market_dist_geobeta + ndvi[code[i]]*ndvi_geobeta + population[code[i]]*population_geobeta + crop_prod[code[i]]*crop_prod_geobeta + fieldsize[code[i]]*fieldsize_geobeta + nutritiondiversity[code[i]]*nutritiondiversity_geobeta;

  re_spei24_beta[code[i]] ~ normal(geo_mu, geo_sigma_e);



  mu = intercept + re_intercept[code[i]] + re_spei24_beta[code[i]] * spei24[i] + toiletFlushToilet_beta*toiletFlushToilet[i] + toiletOther_beta*toiletOther[i] + toiletPitLatrine_beta*toiletPitLatrine[i] + age_beta*age[i] + birth_order_beta*birth_order[i] + head_age_beta*head_age[i] + head_sexMale_beta*head_sexMale[i] + sexMale_beta*sexMale[i] + wealth_indexMiddle_beta*wealth_indexMiddle[i] + wealth_indexPoorer_beta*wealth_indexPoorer[i] + wealth_indexRicher_beta*wealth_indexRicher[i] + wealth_indexRichest_beta*wealth_indexRichest[i] + hhsize_beta*hhsize[i] + urban_ruralRural_beta*urban_ruralRural[i] + mother_years_ed_beta*mother_years_ed[i];

  haz_sim[i] ~ normal(mu, sigma_e);
}

}
"

stanmod <- stan(model_name="mode1", model_code = stan_code, data=stanDat,
                iter = 2000, chains = 4)

################################
#Write Results
#####################################
time <- substr(Sys.time(), 1, 10)

modtitle <- 'modeled_effects_simulation'

save(list=c('stanDat', 'stan_code', 'stanmod'), file=paste0("~/stan-models/", time, modtitle))

###############################
#Extract Random Effects
###############################
sum <- summary(stanmod)$summary

spei24 <- sum[grepl('re_spei24_beta', row.names(sum), fixed=T), 'mean']

intercept <- sum[grepl('re_intercept[', row.names(sum), fixed=T), 'mean']

##Calculate Residuals and RSME
  hist(sum[grepl('re_spei24_beta[', row.names(sum), fixed=T), '2.5%'], 100)
  rhat <- sum[ , 'Rhat']
# sel <- sum[ , 'mean']
# re <- codemap[stanDat[['code']], ]
# pred <- sel['intercept'] + re$w1 + re$w2*stanDat[['spei24']] + sel['toiletFlushToilet_beta'] * stanDat[['toiletFlushToilet']] + sel['toiletOther_beta'] * stanDat[['toiletOther']] + sel['toiletPitLatrine_beta'] * stanDat[['toiletPitLatrine']] + sel['relationship_hhheadNotRelated_beta'] * stanDat[['relationship_hhheadNotRelated']] + sel['relationship_hhheadRelative_beta'] * stanDat[['relationship_hhheadRelative']] + sel['age_beta'] * stanDat[['age']] + sel['birth_order_beta'] * stanDat[['birth_order']] + sel['head_age_beta'] * stanDat[['head_age']] + sel['head_sexMale_beta'] * stanDat[['head_sexMale']] + sel['sexMale_beta'] * stanDat[['sexMale']] + sel['wealth_indexMiddle_beta'] * stanDat[['wealth_indexMiddle']] + sel['wealth_indexPoorer_beta'] * stanDat[['wealth_indexPoorer']] + sel['wealth_indexRicher_beta'] * stanDat[['wealth_indexRicher']] + sel['wealth_indexRichest_beta'] * stanDat[['wealth_indexRichest']] + sel['hhsize_beta'] * stanDat[['hhsize']] + sel['diarrhea_beta'] * stanDat[['diarrhea']] + sel['fever_beta'] * stanDat[['fever']] + sel['breast_duration_beta'] * stanDat[['breast_duration']] + sel['urban_ruralRural_beta'] * stanDat[['urban_ruralRural']] + sel['parents_years_ed_beta'] * stanDat[['parents_years_ed']] + sel['gdp_beta'] * stanDat[['gdp']] + sel['md_beta'] * stanDat[['md']] + sel['pop_beta'] * stanDat[['pop']] + sel['mean_annual_precip_beta'] * stanDat[['mean_annual_precip']] + sel['spei24_beta'] * stanDat[['spei24']]
# resid <- stanDat[['haz_sim']] - pred
# sqrt(mean(resid^2))
