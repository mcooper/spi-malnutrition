setwd('/home/mattcoop')

library(ggplot2)
library(dplyr)

hh <- read.csv('hhvars.csv')
gdp <- read.csv('country_gdp.csv')
farm <- read.csv('FarmingSystems.csv')
md <- read.csv('MarketDist.csv')
pop <- read.csv('PopPer100sqkm.csv')
spei <- read.csv('Coords&Precip.csv')

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

regions <- read.csv('regions.csv')


################################
#Combine and clear workspace
###############################
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, gdp, farm, md, pop, spei, regions))

#rm(list=setdiff(ls(), "all")) #remove everything but our data

###########################################################
#Analyze missing data, determine which variables to keep
#############################################################

all <- all %>% filter(urban_rural == 'Rural') #Lets just look at rural hhs

all$md <- all$md/24
all$gdp <- all$gdp/1000
all$mean_annual_precip <- all$mean_annual_precip/1000
all$pop <- all$pop/100

#Relevel factors
all <- all %>%
  mutate(toilet=relevel(toilet, ref="No Facility"),
         relationship_hhhead=relevel(relationship_hhhead, ref="Immediate Family"),
         otherwatersource=relevel(otherwatersource, ref="Surface Water"),
         drinkwatersource=relevel(drinkwatersource, ref="Surface Water"),
         wealth_index=relevel(wealth_index, ref="Poorest"))

all$related_hhhead <- all$relationship_hhhead == "Not Related"

na_summary <- colSums(is.na(all))/nrow(all)

all <- all %>%
  filter(!is.infinite(spi6) &
         !is.infinite(spi12) &
         !is.infinite(spi24) &
         !is.infinite(spi36) &
         !is.infinite(thousandday_spi33) &
         !is.infinite(thousandday_spi24) &
         !is.infinite(birthday_spi9) &
         !is.infinite(spei6) &
         !is.infinite(spei12) &
         !is.infinite(spei24) &
         !is.infinite(spei36) &
         !is.infinite(thousandday_spei33) &
         !is.infinite(birthday_spei9) &
         !is.infinite(thousandday_spei24))
  
library(mgcv)

#Test for!
#cr and tp as the basis
basis <- c('cr', 'tp')

#spi and spei, each kind
precip <- c("spei6", "spei12", "spei24", "spei36", "spi6", 
            "spi12", "spi24", "spi36", "birthday_spei9", 
            "birthday_spi9", "thousandday_spei33", "thousandday_spi33", "thousandday_spei24", 
            "thousandday_spi24")

#haz and whz
dep_var <- c('haz', 'whz', 'haz_dhs', 'whz_dhs')

#many vars and few vars
extras <- c('no_extra', 'extra')

df <- expand.grid(basis, precip, dep_var, extras, stringsAsFactors = F)

for (i in 1:nrow(df)){
print(i)

if (df$Var4[i] == "no_extra"){
extra <- ""
}else{
extra <- ' + fever + diarrhea + related_hhhead + istwin'
}

expression <- as.formula(paste0(df$Var3[i], " ~ s(", df$Var2[i], ", bs='", df$Var1[i], "') + age + head_sex + hhsize + sex + gdp + pop + interview_year + head_age + md + wealth_index + mother_years_ed + workers + country + mean_annual_precip", extra))

mod <- gam(expression, data=all)  

moddata = data.frame(age = mean(all$age, na.rm=T),
              interview_year = 2007,
              head_sex = 'Male',
              hhsize = mean(all$hhsize, na.rm=T),
              sex = "Male",
              gdp = mean(all$gdp, na.rm=T),
              pop = mean(all$pop, na.rm=T),
              head_age = mean(all$head_age, na.rm=T),
              md = mean(all$md, na.rm=T),
              wealth_index = "Middle", 
              mother_years_ed = mean(all$mother_years_ed, na.rm=T),
              workers = mean(all$workers, na.rm=T),
              related_hhhead = TRUE,
              istwin = mean(all$istwin, na.rm=T),
              diarrhea = mean(all$diarrhea, na.rm=T),
              fever = mean(all$fever, na.rm=T),
              country = 'SN', 
              mean_annual_precip=mean(all$mean_annual_precip, na.rm=T),
              precip=seq(-3, 3, 0.1))

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
ggtitle(paste0("AIC:", AIC, " n:", n, " pval:", pval)) +
theme_bw()

ggsave(paste0("GAMResults/", paste(df[i, ], collapse='-'), "-numyear.png"), height=4, width=5)
}

all$interview_year <- as.factor(all$interview_year)

for (i in 1:nrow(df)){
  print(i)
  
  if (df$Var4[i] == "no_extra"){
    extra <- ""
  }else{
    extra <- ' + fever + diarrhea + related_hhhead + istwin'
  }
  
  expression <- as.formula(paste0(df$Var3[i], " ~ s(", df$Var2[i], ", bs='", df$Var1[i], "') + age + head_sex + hhsize + sex + gdp + pop + interview_year + head_age + md + wealth_index + mother_years_ed + workers + country + mean_annual_precip", extra))
  
  mod <- gam(expression, data=all)  
  
  moddata = data.frame(age = mean(all$age, na.rm=T),
                       interview_year = '2007',
                       head_sex = 'Male',
                       hhsize = mean(all$hhsize, na.rm=T),
                       sex = "Male",
                       gdp = mean(all$gdp, na.rm=T),
                       pop = mean(all$pop, na.rm=T),
                       head_age = mean(all$head_age, na.rm=T),
                       md = mean(all$md, na.rm=T),
                       wealth_index = "Middle", 
                       mother_years_ed = mean(all$mother_years_ed, na.rm=T),
                       workers = mean(all$workers, na.rm=T),
                       related_hhhead = TRUE,
                       istwin = mean(all$istwin, na.rm=T),
                       diarrhea = mean(all$diarrhea, na.rm=T),
                       fever = mean(all$fever, na.rm=T),
                       country = 'SN', 
                       mean_annual_precip=mean(all$mean_annual_precip, na.rm=T),
                       precip=seq(-3, 3, 0.1))
  
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
    ggtitle(paste0("AIC:", AIC, " n:", n, " pval:", pval)) +
    theme_bw()
  
  ggsave(paste0("GAMResults/", paste(df[i, ], collapse='-'), "-dummyyear.png"), height=4, width=5)
}

