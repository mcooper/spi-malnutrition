setwd('D://Documents and Settings/mcooper/GitHub/spi-malnutrition/')

library(dplyr)

hh <- read.csv('data/hhvars.csv', stringsAsFactors = F) %>%
  mutate(rcode = paste(cc, num, subversion, sep='-'),
         code = paste(cc, num, subversion, hv001, sep='-')) %>%
  select(-num, -subversion, -survey)
sp <- read.csv('data/Coords&SPI.csv', stringsAsFactors = F) %>%
  select(code=code.y, month, year, spi6, spi12, spi24, spi36,
         LATNUM, LONGNUM, survey_code=rcode.x, URBAN_RURA)
mk <- read.csv('data/MarketDist.csv', stringsAsFactors = F)
fs <- read.csv('data/FarmingSystems.csv', stringsAsFactors = F) %>%
  select(code, DESCRIPTIO, ORIG)

fsmkt <- merge(fs, mk, by='code', all.x=T, all.y=T)
spmkt <- merge(sp, fsmkt, by='code', all.x=T, all.y=T)
all <- merge(hh, spmkt, by=c('code', 'month', 'year'), all.x=T, all.y=F)

rm(fs)
rm(hh)
rm(sp)
rm(mk)
rm(spmkt)
rm(fsmkt)

#Clean records to remove bad values
all <- all %>%
  filter(hc5 < 2000 &
           hc11 < 2000 &
           hc27 != 9 &
           hc64 < 99 &
           !is.infinite(spi12) &
           !is.na(spi24))
#somehow there were infinite spi values for one location in EG

all$FarmSystem <- paste0(all$ORIG, '-' , all$DESCRIPTIO)

cnd <- all$hv025=='Urban' | all$URBAN_RURA=='U'
all$FarmSystem[cnd] <- paste0(all$ORIG[cnd], ' - Urban')


#Explore range of SPI by livelihood zone
summry <- all %>%
  group_by(FarmSystem) %>%
  summarize(n(),
            max(spi24),
            min(spi24))

all <- merge(all, summry)

#only get categories with over a thousand observations
all <- all %>% filter(`n()` > 1500)

#Recode Factors
all$hc27 <- ifelse(all$hc27 == 1 | all$hc27 == "male" , "Male", 
                   ifelse(all$hc27 == 2 | all$hc27 == "female" , "Female", all$hc27))
all$hv219 <- ifelse(all$hv219 == 1 | all$hv219 == "male" , "Male", 
                    ifelse(all$hv219 == 2 | all$hv219 == "female" , "Female", all$hv219))
all$hv025 <- ifelse(all$hv025 == 'rural', 'Rural',
                    ifelse(all$hv025 == 'urban', 'Urban', all$hv025))


#Make factors
all$hv025 <- as.factor(all$hv025)
all$hc27 <- as.factor(all$hc27)
all$hv219 <- as.factor(all$hv219)
all$hv001 <- as.factor(paste0(all$cc, all$hv001))
all$whhid <- as.factor(paste0(all$cc, all$whhid))
all$yearf <- as.factor(all$year)


################################
###DHS Cluster Random Effect
library(lme4)

mod <- lmer(hc5~hv009 + wealth + hc27 + hc1 + hc64 + hv219 + hv220 + spi24 + market + year + hv025 + (spi24|hv001) + (1|cc) + (1|FarmSystem) +(1|rcode), data=all)

dhslocations <- row.names(coef(mod)$hv001)

spiimpact <- coef(mod)$hv001$spi24

spidf <- data.frame(hv001=dhslocations, spiimpact)

latlongdf <- unique(all[all$spi24 < -1, c('hv001', 'LATNUM', 'LONGNUM')])

graphdf <- merge(latlongdf, spidf, all.x=T, all.y=F)

library(ggplot2)
ggplot(graphdf, aes(x=LONGNUM, y=LATNUM, color=spiimpact > 25)) + geom_point(size=0.01)

############################
###Farm System Random Effect
mod2 <- lmer(hc5~hv009 + wealth + hc27 + hc1 + hc64 + hv219 + hv220 + spi24 + market + year + hv025 + (spi24|FarmSystem) + (1|cc) +(1|rcode), data=all)

fslocations <- row.names(coef(mod2)$FarmSystem)

spiimpact <- coef(mod2)$FarmSystem$spi24

spidf <- data.frame(FarmSystem=fslocations, spiimpact)

latlongdf <- unique(all[ , c('FarmSystem', 'LATNUM', 'LONGNUM')])

graphdf <- merge(latlongdf, spidf, all.x=T, all.y=F)

ggplot(graphdf, aes(x=LONGNUM, y=LATNUM, color=spiimpact > 5)) + geom_point(size=0.01)













