setwd('D://Documents and Settings/mcooper/GitHub/spi-malnutrition/')

library(dplyr)

hh <- read.csv('data/hhvars.csv', stringsAsFactors = F) %>%
  mutate(rcode = paste(cc, num, subversion, sep='-'),
         code = paste(cc, num, subversion, hv001, sep='-')) %>%
  select(-num, -subversion, -survey)
sp <- read.csv('data/Coords&SPI.csv', stringsAsFactors = F) %>%
  select(code, month, year, spi6, spi12, spi24, spi36,
         LATNUM, LONGNUM, survey_code=rcode, URBAN_RURA)
mkt <- read.csv('data/MarketDist.csv', stringsAsFactors = F)

spmkt <- merge(sp, mkt, by='code', all.x=T, all.y=T)
all <- merge(hh, spmkt, by=c('code', 'month', 'year'), all.x=T, all.y=F)

rm(hh)
rm(sp)
rm(mkt)
rm(spmkt)

#Clean records to remove bad values
all <- all %>%
  filter(!is.infinite(spi12) &
           !is.na(spi24))
#somehow there were infinite spi values for one location in EG

#Select regression vars, rename
all <- all %>%
  mutate(hhid=paste(rcode, hhid, '-')) %>%
  select(haz,
         dhshaz=hc5,
         country=cc,
         survey=rcode,
         dhscode=code,
         indsex=sex,
         indage=hc1,
         year,
         border=hc64,
         hhid,
         hhsize=hv009,
         num_under5=hv014,
         urbanrural=hv025,
         wealth,
         hhhsex=hv219, #Shit why not harmonzied
         hhhage=hv220,
         market,
         spi6,
         spi12,
         spi24,
         spi36,
         LATNUM,
         LONGNUM) %>%
  na.omit

#Make factors
all$urbanrural <- as.factor(all$urbanrural)
all$country <- as.factor(all$country)
all$survey <- as.factor(all$survey)
all$dhscode <- as.factor(all$dhscode)
all$indsex <- as.factor(all$indsex)
all$hhid <- as.factor(all$hhid)
all$wealth <- as.factor(all$wealth)
all$hhhsex <- as.factor(all$hhhsex)

################################
###DHS Cluster Random Effect
library(lme4)

mod <- lmer(haz~indsex + indage + year + border + hhsize + num_under5 + urbanrural + wealth + hhhsex + hhhage + 
              market + spi24 + (spi24|dhscode) + (1|country) + (1|survey) + (1|dhscode), data=all[all$spi24 < 1, ])

dhscode <- row.names(coef(mod)$dhscode)

spiimpact <- coef(mod)$dhscode$spi24

year <- all[all$spi24 < 1, c('year', 'dhscode', 'LATNUM', 'LONGNUM', 'country')]

spidf <- data.frame(dhscode, spiimpact)

spidf <- merge(spidf, year) %>% unique

write.csv(spidf, 'results/random_effect.csv', row.names = F)

##Plot effect sizes

