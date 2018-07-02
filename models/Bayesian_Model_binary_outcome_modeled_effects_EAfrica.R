library(AzureSMR)
library(dplyr)

az <- read.csv('~/.azure/.AzureSMR', stringsAsFactors = F)

sc <- createAzureContext(tenantID = az$tenantid,
                         clientID = az$appid,
                         authKey = az$authkey)

az_read_csv <- function(csv, container="dhsprocessed"){
  read.csv(text=azureGetBlob(sc, 
                             storageAccount=az$storage_acct, 
                             container=container,
                             blob=csv,
                             type="text",
                             storageKey = az$storageKey))
}

az_write_blob <- function(blob, blobname, container='stan-models'){
  file <- paste0(blobname, '.Rdata')
  save(blob, file=file)
  system(paste0("az storage blob upload --container-name ", container, " --file ", file, " --name ", file,
                " --account-name ", az$storage_acct, " --account-key ", az$storageKey))
  system(paste0("rm ", file))
}

hh <- az_read_csv('hhvars.csv')
spi <- az_read_csv('Coords&Precip.csv')
cov <- az_read_csv('SpatialCovars.csv')

################################
#Combine and clear workspace
################################
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spi, cov))

all <- all %>%
  filter((is_visitor == 0 | is.na(is_visitor)) & (years_in_location >= 2 | is.na(years_in_location))) %>% 
  filter(country %in% c('KE', 'RW', 'TZ', 'UG'))

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
         sex, wealth_index, diarrhea, fever, breast_duration, urban_rural, Adm1, Adm2,
         parents_years_ed, spei24, spi24, latitude, longitude, 
         ag_pct_gdp, bare, precip_10yr_mean, forest, gdp, government_effectiveness, irrigation, 
         market_dist, ndvi, population, stability_violence, tmax_10yr_mean, tmin_10yr_mean, crop_prod) %>%
  na.omit

###################################################
#First run GAM model to determine inflection point
###################################################

library(mgcv)
library(ggplot2)

mod <- gam(haz_dhs ~ s(spi24, bs='cr') + toilet + relationship_hhhead + age + birth_order + head_age + head_sex + hhsize + sex + wealth_index + diarrhea + fever +
             breast_duration + urban_rural + parents_years_ed + country, data=sel)

moddata = data.frame(age = mean(sel$age, na.rm=T),
                     interview_year = 2007,
                     head_sex = 'Male',
                     hhsize = mean(sel$hhsize, na.rm=T),
                     sex = "Male",
                     head_age = mean(sel$head_age, na.rm=T),
                     wealth_index = "Middle",
                     related_hhhead = TRUE,
                     diarrhea = mean(sel$diarrhea, na.rm=T),
                     fever = mean(sel$fever, na.rm=T),
                     country = 'KE',
                     spi24=seq(-2.5, 2.5, 0.05),
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

ggplot(aes(x=spi24,y=fit), data=predicts) +
  geom_ribbon(aes(ymin = lower, ymax=upper), fill='gray90') +
  geom_line(color='#1e90ff') +
  theme_bw()

###################################################
#Then run GLMMs to determine MCMC starting points
###################################################



drought <- sel %>%
  filter(spei24 < -0.25)

# dsum <- drought %>%
#   group_by(code) %>%
#   summarize(size=n()) %>%
#   filter(size > 4)

# drought <- drought %>%
#   filter(code %in% dsum$code)

#plot(drought$longitude, drought$latitude, pch=16, cex=0.2)

flood <- sel %>%
  filter(spei24 > 1.25)

#plot(flood$longitude, flood$latitude, pch=16, cex=0.2)


mod_f <- lmer(haz_dhs ~ spei24  + toilet + relationship_hhhead + age + birth_order + head_age + head_sex + hhsize + sex + wealth_index + diarrhea + fever +
                breast_duration + urban_rural + parents_years_ed + (1 | code), data=flood)

############################################
#Stan-tastic models
###########################################

drought <- sel %>% filter(spei24 < 1)
library(lme4)

drought$drought <- spei < 
mod_d <- lmer(haz_dhs ~ spei24 + toilet + relationship_hhhead + age + birth_order + head_age + head_sex + hhsize + sex + wealth_index + diarrhea + fever +
                breast_duration + parents_years_ed + (1 | country), data=drought)

library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

init <- summary(mod_d)$coefficients[ , 1]
names(init)[names(init)=="(Intercept)"] <- "intercept"
names(init) <- gsub(' ', '', names(init))
init <- as.list(init)
init <- list(chain1=init, chain2=init, chain3=init, chain4=init)

codemap <- drought %>% 
  mutate(code_number=as.numeric(as.factor(as.character(Adm2))),
         survey_number=as.numeric(as.factor(as.character(surveycode)))) %>%
  group_by(surveycode, survey_number, code_number, latitude, longitude, urban_rural) %>% 
  summarize(size=n())

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
stanDat[["spei24"]] <- drought$spei24

#Spatial Covariates
stanDat[["ag_pct_gdp"]] <- drought$ag_pct_gdp
stanDat[["bare"]] <- drought$bare
stanDat[["precip_10yr_mean"]] <- drought$precip_10yr_mean
stanDat[["forest"]] <- drought$forest
stanDat[["gdp"]] <- drought$gdp
stanDat[["government_effectiveness"]] <- drought$government_effectiveness
stanDat[["irrigation"]] <- drought$irrigation
stanDat[["market_dist"]] <- drought$market_dist
stanDat[["ndvi"]] <- drought$ndvi
stanDat[["population"]] <- drought$population
stanDat[["stability_violence"]] <- drought$stability_violence
stanDat[["tmax_10yr_mean"]] <- drought$tmax_10yr_mean
stanDat[["tmin_10yr_mean"]] <- drought$tmin_10yr_mean
stanDat[["crop_prod"]] <- drought$crop_prod

stanDat[["code_N"]] <- length(unique(drought$Adm2))
stanDat[["code"]] <- as.numeric(as.factor(as.character(drought$Adm2)))

drought_stan_code <- "
data {
int<lower=1> N;

real<lower=-600, upper=600> haz_dhs[N];

int<lower=0, upper=1> toiletFlushToilet[N];
int<lower=0, upper=1> toiletOther[N];
int<lower=0, upper=1> toiletPitLatrine[N];
int<lower=0, upper=1> relationship_hhheadNotRelated[N];
int<lower=0, upper=1> relationship_hhheadRelative[N];
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
int<lower=0, upper=1> diarrhea[N];
int<lower=0, upper=1> fever[N];
int<lower=0> breast_duration[N];
int<lower=0, upper=1> urban_ruralRural[N];
int<lower=0> parents_years_ed[N];
real<lower=-3, upper=3> spei24[N];

int<lower=0, upper=1>  ag_pct_gdp [N];
int<lower=0, upper=1>  bare [N];
int<lower=0, upper=1>  precip_10yr_mean [N];
int<lower=0, upper=1>  forest [N];
int<lower=0, upper=1>  gdp [N];
int<lower=0, upper=1>  government_effectiveness [N];
int<lower=0, upper=1>  irrigation [N];
int<lower=0, upper=1>  market_dist [N];
int<lower=0, upper=1>  ndvi [N];
int<lower=0, upper=1>  population [N];
int<lower=0, upper=1>  stability_violence [N];
int<lower=0, upper=1>  tmax_10yr_mean [N];
int<lower=0, upper=1>  tmin_10yr_mean [N];
int<lower=0, upper=1>  crop_prod [N];

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
real<lower=0> sigma_e; //error sd

vector<lower=0>[code_N] re_spei24_beta;
vector[code_N] re_intercept;

real<lower=0> re_intercept_sigma;
real<lower=0> re_spei24_sigma;

real<lower=0> re_spei24_mean;

}


model {
real mu;

sigma_e ~ cauchy(0, 2);
re_intercept_sigma ~ cauchy(0, 1);
re_spei24_sigma ~ cauchy(0, 1);
re_spei24_mean ~ chi_square(1);

re_intercept ~ normal(0, re_intercept_sigma);
re_spei24_beta ~ normal(re_spei24_mean, re_spei24_sigma);


for (i in 1:N){
mu = intercept + re_intercept[code[i]] + re_spei24_beta[code[i]] * spei24[i] + toiletFlushToilet_beta*toiletFlushToilet[i] + toiletOther_beta*toiletOther[i] + toiletPitLatrine_beta*toiletPitLatrine[i] + relationship_hhheadNotRelated_beta*relationship_hhheadNotRelated[i] + relationship_hhheadRelative_beta*relationship_hhheadRelative[i] + age_beta*age[i] + birth_order_beta*birth_order[i] + head_age_beta*head_age[i] + head_sexMale_beta*head_sexMale[i] + sexMale_beta*sexMale[i] + wealth_indexMiddle_beta*wealth_indexMiddle[i] + wealth_indexPoorer_beta*wealth_indexPoorer[i] + wealth_indexRicher_beta*wealth_indexRicher[i] + wealth_indexRichest_beta*wealth_indexRichest[i] + hhsize_beta*hhsize[i] + diarrhea_beta*diarrhea[i] + fever_beta*fever[i] + breast_duration_beta*breast_duration[i] + urban_ruralRural_beta*urban_ruralRural[i] + parents_years_ed_beta*parents_years_ed[i];

haz_dhs[i] ~ normal(mu, sigma_e);
}
}
"

stanmod <- stan(model_name="mode1", model_code = drought_stan_code, data=stanDat,
                iter = 10000, chains = 4, init=init)

################################
#Write Results
#####################################
time <- substr(Sys.time(), 1, 10)

modtitle <- 'RE_mAdm2'

az_write_blob(list(stanDat, drought_stan_code, stanmod), paste0(time, modtitle))

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
# resid <- stanDat[['haz_dhs']] - pred
# sqrt(mean(resid^2))

##############################
#Bring in Spatial Covars
##########################

library(raster)
library(randomForest)

#Load objects alldf and s, which have the spatial covariate data
#Looking like were doing this from local bc the raster package cant seem to predict with in-memory rasters from blobs
s <- stack('SpatialCovars.grd')
alldf <- read.csv('SpatialCovars.csv')

codemap$random_effect <- spei24
codemap <- merge(codemap, alldf, all.x=T, all.y=F)

rfmod <- randomForest(spei24 ~ ag_pct_gdp + bare + precip_10yr_mean + forest + gdp + government_effectiveness + irrigation + 
                        population + stability_violence + tmax_10yr_mean + tmin_10yr_mean + crop_prod, data=codemap)

out <- predict(s, rfmod, progress='text')

plot(out)

haz_sum <- drought %>%
  group_by(code) %>%
  summarize(spei24_mean = mean(spei24, na.rm=T),
            haz_mean = mean(haz_dhs, na.rm=T))

codemap2 <- merge(codemap, haz_sum, all.x=T, all.y=F)

codemap2$hazlvl <- ifelse(codemap2$haz_mean < -200, "Low", 
                         ifelse(codemap2$haz_mean > 0, "High", "Medium"))

library(ggplot2)
ggplot(codemap2) + geom_histogram(aes(x=random_effect, fill=hazlvl), bins=100)

