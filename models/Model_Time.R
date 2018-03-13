setwd('G://My Drive/DHS Processed')

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

lc <- lc %>%
  select(human, natural, interview_year, code)

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

rm(list=setdiff(ls(), "all")) #remove everything but our data

###########################################################
#Analyze missing data, determine which variables to keep
#############################################################

all <- all %>% filter(!(haz < -7 | haz > 5)) #first filter extreme HAZs

df <- data.frame()
for (i in names(all)){
  df <- bind_rows(df, data.frame(name=i, NAs=sum(is.na(all[ , i]))/nrow(all)))
}

df %>% arrange(NAs)

df <- df %>% filter(NAs < 0.20) #determine what fraction of NAs to remove for now

all <- all[ , c(df$name, 'thousandday_spi33')] %>% na.omit

###################
#Run model
###################

#Get a quick summary by country-year
su <- all %>% group_by(country, interview_year) %>% summarize(haz=mean(haz))

#look at distribution of year coefficients varying by country
mod <- lmer(haz ~ toilet + drinkwatersource + age + birth_order + head_age + head_sex + hhsize + mother_alive + sex + 
            watersource_dist + wealth_index + mother_years_ed + workers + natural + md + 
            mean_annual_precip + interview_year + (interview_year|country),
          data=all[all$urban_rural=='Rural', ])
co <- coef(mod)$country$interview_year #Not much variation

#Run again per country
df <- data.frame()
for (c in all$country %>% unique){
  mod <- lm(haz ~ toilet + drinkwatersource + age + birth_order + head_age + head_sex + hhsize + mother_alive + sex + 
          watersource_dist + wealth_index + mother_years_ed + workers + natural + md + 
          mean_annual_precip + interview_year,
        data=all[all$country==c & all$urban_rural=='Rural', ])
  df <- bind_rows(df, data.frame(country=c, coef=coef(mod)[['interview_year']],
                                 count=max(all[all$country==c, 'interview_year']) - min(all[all$country==c, 'interview_year'])))
}

#It looks like Haiti, Benin, and Namibia have a decrease in HAZ per year over a decently wide
#range of time, after controlling for other relevant factors

#Haiti being the posterchild for land degradation, it would be interesting to look at in relation to stunting
#maybe just look within Haiti, or maybe compare to DR?





