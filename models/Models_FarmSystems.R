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


library(lme4)
library(lmerTest)

df <- data.frame()
for (fs in unique(all$FarmSystem)){
  print(fs)
  mod <- lmer(hc5~hv009 + wealth + hc27 + hc1 + hc64 + hv219 + hv220 + spi24 + market + year + (1|hv001), data=all[all$FarmSystem==fs, ])
  summry <- summary(mod)
  maxspi <- max(all$spi24[all$FarmSystem==fs], na.rm=T)
  minspi <- min(all$spi24[all$FarmSystem==fs], na.rm=T)
  meanspi <- mean(all$spi24[all$FarmSystem==fs], na.rm=T)
  n <- nrow(na.omit(all[all$FarmSystem==fs, c('hc5', 'hv009', 'wealth', 'hc27', 'hc1', 'hc64', 'hv219', 'hv220', 'spi24', 'market', 'year')]))
  p <- summry$coefficients['spi24', 5]
  b <- summry$coefficients['spi24', 1]
  temp <- data.frame(FarmSystem=fs, p=p, b=b, n=n, maxspi=maxspi, minspi=minspi, meanspi=meanspi)
  df <- bind_rows(df, temp)
  write.csv(df, 'results/model_results_FarmingSystem.csv', row.names=F)
}

df <- read.csv('results/model_results_FarmingSystem.csv', na.strings = 'NaN')

merged <- merge(all[ , c('FarmSystem', 'LATNUM', 'LONGNUM')] %>% unique, df)

library(rgdal)
fao <- readOGR('data', 'all_farming_systems')
merged$color <- (!(merged$b > 0 & merged$p < 0.06)) + 2

merged <- merged[!grepl('Urban', merged$FarmSystem), ]

plot(fao, xlim=c(min(merged$LONGNUM), max(merged$LONGNUM)), ylim=c(min(merged$LATNUM), max(merged$LATNUM)))
points(merged$LONGNUM, merged$LATNUM, col=merged$color, pch=18, cex=0.25)
