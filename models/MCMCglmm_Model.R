setwd('G://My Drive/DHS Processed')

library(broom)
library(ggplot2)
library(dplyr)

hh <- read.csv('hhvars.csv')
gdp <- read.csv('country_gdp.csv')
farm <- read.csv('FarmingSystems.csv')
lc <- read.csv('landcover.csv')
md <- read.csv('MarketDist.csv')
pop <- read.csv('PopPer100sqkm.csv')
spi <- read.csv('Coords&SPI.csv')

#Look at including World Governance Indicators at: http://info.worldbank.org/governance/wgi/#home

##############################
#Process landcover data
#################################
human <- paste0('cci_', c('10', '11', '12', '20', '30', '190', '200', '201', '202', '220'))
natural <- paste0('cci_', c('40', '50', '60', '61', '62', '70', '71', '80', '90', '100', '110', '120', '121', '122',
                            '130', '140', '150', '152', '153', '160', '170', '180', '210'))
nat_water <- 'cci_210'
nat_grass <- paste0('cci_', c('110', '120', '121', '122', '130', '140', '150', '152', '153', '180'))
nat_trees <- paste0('cci_', c('40', '50', '60', '61', '62', '70', '71', '80', '90', '100', '160', '170'))

getPercetCover <- function(selcols, allcolmatch, df){
  if(length(selcols) > 1){
    selcolsum <- rowSums(df[ , selcols[selcols %in% names(df)]], na.rm=T)
  } else{
    selcolsum <- df[ , selcols]
  }
  allcolsum <- rowSums(df[ , grepl(allcolmatch, names(df))], na.rm=T)
  return(selcolsum/allcolsum)
}

lc$human <- getPercetCover(human, 'cci_', lc)
lc$natural <- getPercetCover(natural, 'cci_', lc)
lc$nat_water <- getPercetCover(nat_water, 'cci_', lc)
lc$nat_grass <- getPercetCover(nat_grass, 'cci_', lc)
lc$nat_trees <- getPercetCover(nat_trees, 'cci_', lc)

lc <- lc %>%
  select(human, natural, interview_year, code, nat_water, nat_grass, nat_trees)

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
              list(hh, gdp, farm, lc, md, pop, spi))

#rm(list=setdiff(ls(), "all")) #remove everything but our data

###########################################################
#Analyze missing data, determine which variables to keep
#############################################################

all <- all %>% filter(!(haz < -7 | haz > 5)) #first filter extreme HAZs

all <- all %>% filter(urban_rural == 'Rural') #Lets just look at rural hhs

all$md <- all$md/24
all$gdp <- all$gdp/1000
all$mean_annual_precip <- all$mean_annual_precip/1000
all$pop <- all$pop/100

#Drop variables that will not be used in regression
all <- all %>%
  select(-interview_month, -interview_cmc, -calc_birthmonth, -calc_birthyear, -thousandday_month, -thousandday_year, 
         -latitude, -longitude, 
         -hhid, -householdno, -clusterid, 
         -father_line, -mother_line, -child_line_num, 
         -haz_dhs, -waz_dhs, -whz_dhs, 
         -haz_who, -waz_who, -whz_who, 
         -height, -weight, -how_measured, 
         -sampweight, -urban_rural, -parents_years_ed, 
         -wealth_factor, -dependents, -caseid, 
         -birthyear, -birthmonth, -birthday_cmc, 
         -fromKR, -filesource, 
         -waz, -whz, 
         -continent, -farm_system, -human, 
         -spi6, -spi12, -spi36, 
         -birthday_9monthtotal, -birthday_spi9,
         -preceeding_interval, -suceeding_interval #Need to figure out missing data vs Not Applicable
  )

#Relevel factors
all <- all %>%
  mutate(toilet=relevel(toilet, ref="No Facility"),
         relationship_hhhead=relevel(relationship_hhhead, ref="Immediate Family"),
         otherwatersource=relevel(otherwatersource, ref="Surface Water"),
         drinkwatersource=relevel(drinkwatersource, ref="Surface Water"),
         wealth_index=relevel(wealth_index, ref="Poorest"))

all$related_hhhead <- all$relationship_hhhead == "Not Related"

na_summary <- colSums(is.na(all))/nrow(all)

library(lme4)

all$spi24sq <- all$spi24^2
all$surveycode <- substr(all$code, 1, 6)



library(MCMCglmm)
library(postMCMCglmm)

sel <- all %>%
  select(haz, age, interview_year, head_sex, hhsize, sex, gdp, pop, mean_annual_precip,
           head_age, md, wealth_index, mother_years_ed, workers, related_hhhead, spi24, 
           istwin, diarrhea, fever, code) %>%
  na.omit

test <- MCMCglmm(haz ~ age + interview_year + head_sex + hhsize + sex + gdp + pop + mean_annual_precip +
                   head_age + md + wealth_index + mother_years_ed + workers + related_hhhead + spi24 + 
                   istwin + diarrhea + fever, random=code~spi24, 
                 data = sel, pr=TRUE)
qfun <- function(x,lev) unname(quantile(x,lev))
rsum <- as.data.frame(t(apply(ranef(test),1,
                              function(x) c(est=mean(x),
                                            min=qfun(x,0.025),max=qfun(x,0.975)))))

rsum$term <- reorder(factor(rownames(rsum)),
                     rsum$est)



