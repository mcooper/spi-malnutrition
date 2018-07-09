library(ggplot2)
library(dplyr)
library(lme4)

setwd('G://My Drive/DHS Processed')

hh <- read.csv('HH_data_A.csv')
lc <- read.csv('landcover_processed.csv')
spei <- read.csv('PrecipIndices.csv')
spei_ind <- read.csv('PrecipIndices_Individual.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, lc, spei, spei_ind, cov))

all$spi_age_mix[all$age > 24] <- all$spi_age[all$age > 24]
all$spi_age_mix[all$age < 24] <- all$spi_ageutero[all$age < 24]
all$spi_gs_age_mix[all$age > 24] <- all$spi_gs_age[all$age > 24]
all$spi_gs_age_mix[all$age < 24] <- all$spi_gs_ageutero[all$age < 24]
all$spei_age_mix[all$age > 24] <- all$spei_age[all$age > 24]
all$spei_age_mix[all$age < 24] <- all$spei_ageutero[all$age < 24]
all$spei_gs_age_mix[all$age > 24] <- all$spei_gs_age[all$age > 24]
all$spei_gs_age_mix[all$age < 24] <- all$spei_gs_ageutero[all$age < 24]

###########################################################
#Analyze missing data, determine which variables to keep
#############################################################

all <- all %>% filter(market_dist > 24*4) #Lets just look at rural hhs

library(mgcv)

#Test for!
#cr and tp as the basis
basis <- c('cr', 'tp')

#spi and spei, each kind
precip <- names(all)[grepl('sp', names(all))]

df <- expand.grid(basis, precip, stringsAsFactors = F)

for (i in 1:nrow(df)){
  print(i)
  
  sel <- all[!is.infinite(all[ , df$Var2[i]]), ]
  
  expression <- as.formula(paste0("haz_dhs ~ s(", df$Var2[i], ", bs='", df$Var1[i], "') + interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet + head_age + head_sex + urban_rural + wealth_index + country"))
  
  mod <- gam(expression, data=sel)  
  
  moddata = data.frame(age = mean(all$age, na.rm=T),
                interview_year = 2007,
                birth_order=5,
                head_sex = 'Male',
                hhsize = mean(all$hhsize, na.rm=T),
                sex = "Male",
                head_age = mean(all$head_age, na.rm=T),
                wealth_index = "Middle", 
                mother_years_ed = mean(all$mother_years_ed, na.rm=T),
                country = 'SN', 
                precip=seq(-3, 3, 0.1),
                urban_rural="Rural",
                toilet="Other")
  
  names(moddata)[names(moddata)=='precip'] <- df$Var2[i]
  
  sm <- summary(mod)
  AIC <- AIC(mod)
  n <- sm$n
  pval <- sm$s.table[, 4]
  
  fits = predict(mod, moddata, type='response', se=T)
  predicts = data.frame(moddata, fits) %>% 
  mutate(lower = fit - 1.96*se.fit,
   upper = fit + 1.96*se.fit)
  
  ggplot(aes_string(x=df$Var2[i],y='fit'), data=predicts) +
  geom_ribbon(aes(ymin = lower, ymax=upper), fill='gray90') +
  geom_line(color='#1e90ff') + 
  ggtitle(paste0("pval:", pval, " AIC:", AIC)) +
  theme_bw()
  
  ggsave(paste0("G:/My Drive/Dissertation/GAM Outputs/", paste(rev(df[i, ]), collapse='-'), ".png"), height=8, width=10)
}



