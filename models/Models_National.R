setwd('D://Documents and Settings/mcooper/GitHub/spi-malnutrition/')

library(dplyr)

hh <- read.csv('data/hhvars.csv', stringsAsFactors = F)
sp <- read.csv('data/Coords&SPI.csv', stringsAsFactors = F)
mk <- read.csv('data/MarketDist.csv', stringsAsFactors = F)

sp$hv001 <- sp$DHSCLUST
sp$code <- sp$code.y

spmkt <- merge(sp, mk, by='code', all.x=T, all.y=F)

all <- merge(hh, spmkt, all.x=T, all.y=F)

rm(hh)
rm(spmkt)
rm(sp)
rm(mk)

#Clean records to remove bad values
all <- all %>%
  filter(hc5 < 2000 &
         hc11 < 2000 &
         hc27 != 9 &
         hc64 < 99 &
         !is.infinite(spi12))
#somehow there were infinite spi values for one location in EG


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

# sum <- all %>%
#   group_by(cc) %>%
#   summarize(max24=max(spi24, na.rm=T),
#             min24=min(spi24, na.rm=T),
#             mean24=mean(spi24, na.rm=T),
#             max12=max(spi12, na.rm=T),
#             min12=min(spi12, na.rm=T),
#             mean12=mean(spi12, na.rm=T),
#             n())

all <- all %>%
  filter(all$hv025=="Rural")

df <- data.frame()
for (cc in rev(unique(all$cc))){
  print(cc)
  mod <- lm(hc5~hv009 + wealth + hc27 + hc1 + hc64 + hv219 + hv220 + spi24 + market, data=all[all$cc==cc, ])
  summry <- summary(mod)
  maxspi <- max(all$spi24[all$cc==cc], na.rm=T)
  minspi <- min(all$spi24[all$cc==cc], na.rm=T)
  meanspi <- mean(all$spi24[all$cc==cc], na.rm=T)
  n <- nrow(na.omit(all[all$cc==cc, c('hc5', 'hv009', 'wealth', 'hc27', 'hc1', 'hc64', 'hv219', 'hv220', 'spi24', 'market')]))
  p <- summry$coefficients['spi24', 4]
  b <- mod$coefficients['spi24']
  rsq <- summry$r.squared
  temp <- data.frame(cc=cc, p=p, b=b, n=n, rsq=rsq, maxspi=maxspi, minspi=minspi, meanspi=meanspi)
  df <- bind_rows(df, temp)
  write.csv(df, 'results/model_results.csv', row.names=F)
}

models <- read.csv('results/model_results.csv', na.strings = 'NaN')
mean(models$p < 0.05 & models$b > 0)

merged <- merge(all[ , c('cc', 'LATNUM', 'LONGNUM')], models)

merged$color <- (merged$p < 0.05 & merged$b > 0) + 1

library(maps)

map('world', xlim=c(min(merged$LONGNUM, na.rm=T), max(merged$LONGNUM, na.rm=T)), ylim=c(min(merged$LATNUM, na.rm=T), max(merged$LATNUM, na.rm=T)))
points(merged$LONGNUM, merged$LATNUM, col=merged$color, pch=18, cex=0.25)


