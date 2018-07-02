library(dplyr)

setwd('~/dhsprocessed')

hh <- read.csv('hhvars.csv')
spi <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

spi2 <- spi %>%
  select(-calc_birthyear, -calc_birthmonth, -thousandday_month, -thousandday_year) %>%
  unique

################################
#Combine and clear workspace
################################
hh <- hh %>%
  filter(country %in% c('KE', "TZ", "UG"))

spi <- spi %>%
  select(-precip_10yr_mean, -tmin_10yr_mean, -tmax_10yr_mean)

all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spi, cov))

all <- all %>%
  filter(is_visitor == 0 & years_in_location >= 2 | is.na(is_visitor) | is.na(years_in_location))

#Relevel factors
all <- all %>%
  mutate(toilet=relevel(toilet, ref="No Facility"),
         relationship_hhhead=relevel(relationship_hhhead, ref="Immediate Family"),
         otherwatersource=relevel(otherwatersource, ref="Surface Water"),
         drinkwatersource=relevel(drinkwatersource, ref="Surface Water"),
         wealth_index=relevel(wealth_index, ref="Poorest"))

all$related_hhhead <- all$relationship_hhhead == "Not Related"

sel <- all %>%
  select(code, surveycode, country, Adm2, interview_year, toilet, related_hhhead, age, birth_order, haz_dhs, head_age, head_sex, hhsize,
         sex, wealth_index, diarrhea, fever, breast_duration, urban_rural, mother_height, latitude, longitude,
         parents_years_ed, market_dist, spei24) %>%
  na.omit

###################################################
#Then run GLMMs to determine MCMC starting points
###################################################

library(lme4)

mod <- lmer(haz_dhs ~ interview_year + toilet + related_hhhead + age + birth_order + head_age + head_sex + hhsize +
              sex + wealth_index + diarrhea + fever + mother_height + market_dist + 
                breast_duration + urban_rural + parents_years_ed + (1 | Adm2), data=sel)

############################################
#Stan-tastic models
###########################################

library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

init <- summary(mod)$coefficients[ , 1]
names(init) <- gsub(' ', '', names(init))
names(init) <- paste0(names(init), "_beta")
names(init)[names(init)=="(Intercept)_beta"] <- "intercept"
init <- as.list(init)
init <- list(chain1=init, chain2=init, chain3=init, chain4=init)

codemap <- sel %>% 
  mutate(code_number=as.numeric(as.factor(as.character(code))),
         surveycode_number=as.numeric(as.factor(as.character(surveycode)))) %>%
  group_by(surveycode, surveycode_number, code, code_number, latitude, longitude, urban_rural) %>% 
  summarize(size=n())

stanDat <- list()
stanDat[["N"]] <- nrow(sel)
stanDat[["haz_dhs"]] <- sel$haz_dhs
stanDat[["toiletFlushToilet"]] <- sel$toilet == "Flush Toilet"
stanDat[["toiletOther"]] <- sel$toilet == "Other"
stanDat[["toiletPitLatrine"]] <- sel$toilet == "Pit Latrine"
stanDat[["related_hhhead"]] <- sel$related_hhhead
stanDat[["age"]] <- sel$age
stanDat[["birth_order"]] <- sel$birth_order
stanDat[["head_age"]] <- sel$head_age
stanDat[["head_sexMale"]] <- sel$head_sex == "Male"
stanDat[["sexMale"]] <- sel$sex == "Male"
stanDat[["wealth_indexMiddle"]] <- sel$wealth_index == "Middle"
stanDat[["wealth_indexPoorer"]] <- sel$wealth_index == "Poorer"
stanDat[["wealth_indexRicher"]] <- sel$wealth_index == "Richer"
stanDat[["wealth_indexRichest"]] <- sel$wealth_index == "Richest"
stanDat[["hhsize"]] <- sel$hhsize
stanDat[["diarrhea"]] <- sel$diarrhea
stanDat[["fever"]] <- sel$fever
stanDat[["breast_duration"]] <- sel$breast_duration
stanDat[["urban_ruralRural"]] <- sel$urban_rural == "Rural"
stanDat[["parents_years_ed"]] <- sel$parents_years_ed
stanDat[["market_dist"]] <- sel$market_dist
stanDat[["mother_height"]] <- sel$mother_height
stanDat[["spei24"]] <- sel$spei24

stanDat[["Adm2_N"]] <- length(unique(sel$Adm2))
stanDat[["Adm2"]] <- as.numeric(as.factor(as.character(sel$Adm2)))

stan_code <- "
data {
int<lower=1> N;

real<lower=-600, upper=600> haz_dhs[N];

int<lower=0, upper=1> toiletFlushToilet[N];
int<lower=0, upper=1> toiletOther[N];
int<lower=0, upper=1> toiletPitLatrine[N];
int<lower=0, upper=1> related_hhhead[N];
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
real<lower=0> market_dist[N];
real<lower=0> mother_height[N];
real<lower=-6> spei24[N];

int<lower=1> Adm2_N;       //number of countries

int<lower=1> Adm2[N];      //countries id

}

parameters {
real intercept;
real toiletFlushToilet_beta;
real toiletOther_beta;
real toiletPitLatrine_beta;
real related_hhhead_beta;
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
real market_dist_beta;
real mother_height_beta;
real spei24_beta;

vector[Adm2_N] re_intercept;

real<lower=0> re_intercept_sigma;
real<lower=0> sigma_e;

}


model {
real mu;

re_intercept~ normal(0, re_intercept_sigma);

for (i in 1:N){
mu = intercept + re_intercept[Adm2[i]] + market_dist_beta*market_dist[i] + toiletFlushToilet_beta*toiletFlushToilet[i] + toiletOther_beta*toiletOther[i] + toiletPitLatrine_beta*toiletPitLatrine[i] + related_hhhead_beta*related_hhhead[i] + age_beta*age[i] + birth_order_beta*birth_order[i] + head_age_beta*head_age[i] + head_sexMale_beta*head_sexMale[i] + sexMale_beta*sexMale[i] + wealth_indexMiddle_beta*wealth_indexMiddle[i] + wealth_indexPoorer_beta*wealth_indexPoorer[i] + wealth_indexRicher_beta*wealth_indexRicher[i] + wealth_indexRichest_beta*wealth_indexRichest[i] + hhsize_beta*hhsize[i] + diarrhea_beta*diarrhea[i] + fever_beta*fever[i] + breast_duration_beta*breast_duration[i] + urban_ruralRural_beta*urban_ruralRural[i] + parents_years_ed_beta*parents_years_ed[i] + mother_height_beta*mother_height[i] + spei24_beta*spei24[i];

haz_dhs[i] ~ normal(mu, sigma_e);
}
}
"

stanmod <- stan(model_name="mode1", model_code = stan_code, data=stanDat,
                iter = 2000, chains = 4, init=init)

################################
#Write Results
#####################################
time <- substr(Sys.time(), 1, 10)

modtitle <- 'Just_EastAfrica_AdminIntercepts_wSPEI'

save(list=c("stanDat", "stan_code", "stanmod"), file=paste0('~/stan-models/', time, modtitle))

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
                          ifelse(codemap2$haz_mean > 0, "High", "Average"))

library(ggplot2)
ggplot(codemap2) + geom_histogram(aes(x=random_effect, fill=hazlvl), bins=100)

