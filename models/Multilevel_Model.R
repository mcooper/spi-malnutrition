setwd('G://My Drive/DHS Processed')

library(broom)
library(ggplot2)
library(dplyr)

hh <- read.csv('hhvars.csv')
gdp <- read.csv('country_gdp.csv')
farm <- read.csv('FarmingSystems.csv')
md <- read.csv('MarketDist.csv')
pop <- read.csv('PopPer100sqkm.csv')
spi <- read.csv('Coords&Precip.csv')
regions <- read.csv('regions.csv')

#Look at including World Governance Indicators at: http://info.worldbank.org/governance/wgi/#home

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
              list(hh, gdp, farm, md, pop, spi, regions))

#rm(list=setdiff(ls(), "all")) #remove everything but our data

###########################################################
#Analyze missing data, determine which variables to keep
#############################################################

all <- all %>% filter(!(haz < -7 | haz > 5)) #first filter extreme HAZs

all <- all %>% filter(urban_rural == 'Rural') #Lets just look at rural hhs

#all <- all %>% filter(region == 'ssa')

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

#na_summary <- colSums(is.na(all))/nrow(all)

library(lme4)

all$spi24sq <- all$spi24^2
all$surveycode <- substr(all$code, 1, 6)

spimod <- lmer(haz ~ age + interview_year + head_sex + hhsize + sex + gdp + pop + mean_annual_precip +
                 head_age + md + wealth_index + mother_years_ed + workers + related_hhhead + spi24sq + spi24 + 
                 istwin + diarrhea + fever + (1|surveycode) + (1|code), 
               data = all)
summary(spimod)

natmod <- lmer(haz ~ age + interview_year + head_sex + hhsize + sex + gdp + pop + mean_annual_precip +
                 head_age + md + wealth_index + mother_years_ed + workers + related_hhhead + spi24sq + 
                 istwin + diarrhea + fever + natural + (1|country) + (1|surveycode) + (1|code), 
               data = all)
summary(natmod)

all$spi_pos <- all$spi24 + - min(all$spi24)
all$natural_center <- all$natural - 0.5
all$nat_grass_center <- all$nat_grass - 0.5
all$nat_water_center <- all$nat_water - 0.5
combmod <- lmer(haz ~ age + interview_year + head_sex + hhsize + sex + gdp + spi_pos + pop + mean_annual_precip +
               head_age + md + mother_years_ed + workers + related_hhhead + wealth_index + 
               istwin + diarrhea + fever + natural_center + natural_center*spi_pos + (wealth_index|surveycode) + (1|surveycode) + (1|code), data = all)


###Purty Graphs
setwd("G:/My Drive/Dissertation/Visualizations/")

labels <- data.frame(matrix(c('age', 'Child\'s Age', 1, 
                                 'sexMale', 'Child is Male', 2,
                                 'istwin', 'Child is Twin', 3,
                                 'diarrhea', 'Child had Diarrhea', 4,
                                 'fever', 'Child had Fever', 5, 
                                 'related_hhheadTRUE', 'Child Related HH Head', 6,
                                 'mother_years_ed', 'Mother Education (Years)', 7,
                                 'head_sexMale', 'HH Head is Male', 8, 
                                 'head_age', 'HH Head Age', 9,
                                 'hhsize', 'HH Size', 10, 
                                 'workers', 'Number HH Workers', 11,
                                 'wealth_indexPoorer', 'HH 2nd Wealth Quintile', 12,
                                 'wealth_indexMiddle', 'HH 3rd Wealth Quintile',13,
                                 'wealth_indexRicher', 'HH 4th Wealth Quintile', 14,
                                 'wealth_indexRichest', 'HH 5th Wealth Quintile', 15,
                                 'md', 'Market Distance (Days)', 16,
                                 'gdp', 'GDP (1000$)', 17,
                                 'pop', 'Population Density (pp/sqkm)', 18,
                                 'interview_year', 'Year', 19,
                                 'mean_annual_precip', 'Annual Precip (1000mm/yr)', 20,
                                 'spi24', '24-Month SPI', 21,
                                 'spi24sq', '24-Month SPI^2', 22, 
                                 '(Intercept)', 'Intercept', 23,
                                 'nat_water', 'Water Bodies', 24,
                                 'nat_grass', 'Shrub and Grassland', 25,
                                 'nat_trees', 'Forestland', 26,
                                 'natural', 'Natural Areas', 27,
                                 "spi24:natural", "SPI*Natural Areas", 30,
                                 "spi24sq:natural", "SPI*Natural Areas", 31,
                                 "spi_pos", "24-Month SPI", 21,
                                 "natural_center", "Natural Areas", 27,
                                 "spi_pos:natural_center", "SPI-Natural Interaction", 32),
                               ncol=3, byrow=T))
names(labels) <- c('term', 'label', 'rank')

labels$label <- factor(labels$label, levels=unique(labels$label[rev(order(as.numeric(as.character(labels$rank))))]), ordered=T)

spidf <- tidy(spimod, conf.int=TRUE) %>% 
  filter(!grepl('country', term) & term != '(Intercept)') %>%
  merge(labels, all.x=T) %>%
  na.omit

ggplot(spidf, aes(estimate, label)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high)) + 
  geom_vline(xintercept = 0) + 
  ylab('') + xlab('Estimate') + 
  ggtitle('Impacts of 24-Month SPI^2 on HAZ Scores in Africa') + 
  theme_bw()
ggsave('Impacts of 24-Month SPI^2 on HAZ Scores (Committee).png', width = 7, height=5)
  
natdf <- tidy(natmod, conf.int=TRUE) %>% 
  filter(!grepl('country', term) & term != '(Intercept)') %>%
  merge(labels, all.x=T) %>%
  na.omit

ggplot(natdf, aes(estimate, label)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high)) + 
  geom_vline(xintercept = 0) + 
  ylab('') + xlab('Estimate') + 
  theme_bw() + 
  ggtitle('Impacts of Natural Land Cover on HAZ Scores in Africa')
ggsave('Impacts of Natural Land Cover on HAZ Scores (Committee).png', width = 7, height=5)

combdf <- tidy(combmod, conf.int=TRUE) %>% 
  filter(!grepl('country', term) & term != '(Intercept)') %>%
  merge(labels, all.x=T) %>%
  na.omit

ggplot(combdf, aes(estimate, label)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high)) + 
  geom_vline(xintercept = 0) + 
  ylab('') + xlab('Estimate') + 
  theme_bw() + 
  ggtitle('Interactive Effocts of Natural Land Cover and SPI on HAZ Scores')
ggsave('Interactive Effocts of Natural Land Cover and SPI on HAZ Score.png', width = 7, height=5)


