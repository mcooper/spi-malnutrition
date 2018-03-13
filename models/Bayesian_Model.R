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

all <- all %>% filter(urban_rural == 'Rural') #Lets just look at rural hhs

#Drop variables that will not be used in regression
all <- all %>%
  select(-interview_month, -latitude, -longitude, -calc_birthmonth, -calc_birthyear, -thousandday_month,
         -thousandday_year, -hhid, -child_line_num, -clusterid, -householdno, -father_line, -mother_line,
         -haz_dhs, -haz_who, -height, -how_measured, -interview_cmc, -sampweight, -urban_rural, -waz_dhs, 
         -waz_who, -wealth_factor, -weight, -whz_dhs, -whz_who, -dependents, -caseid, -birthday_cmc, -birthmonth,
         -birthyear, -parents_years_ed, -fromKR, -filesource, -waz,
         -whz, -continent, -farm_system, -human, -spi6, -spi12, -spi36, -birthday_9monthtotal, -birthday_spi9,
         mother_years_ed, mother_highest_year)

#Relevel factors
all <- all %>%
  mutate(toilet=relevel(toilet, ref="No Facility"),
         relationship_hhhead=relevel(relationship_hhhead, ref="Son/Daughter"),
         otherwatersource=relevel(otherwatersource, ref="Surface Water"),
         drinkwatersource=relevel(drinkwatersource, ref="Surface Water"),
         wealth_index=relevel(wealth_index, ref="Poorest"))
         
           
# 
# df <- data.frame()
# for (i in names(all)){
#   df <- bind_rows(df, data.frame(name=i, NAs=sum(is.na(all[ , i]))/nrow(all)))
# }
# 
# df %>% arrange(NAs)
# 
# df <- df %>% filter(NAs < 0.20)
# 
# all <- all[ , c(df$name, 'thousandday_spi33')] %>% na.omit

###################
#Run model
###################














