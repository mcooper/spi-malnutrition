library(dplyr)
library(lme4)
library(MASS)
library(broom)

setwd('~/dhsprocessed')
setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei)) %>%
  na.omit

for (i in c("spei12", "spei24", "spei36", "spi12", "spi24", "spi36", "spei12gs", 
            "spei24gs", "spei36gs", "spi12gs", "spi24gs", "spi36gs")){
  all <- all[!is.infinite(all[ , i]) & !is.na(all[ , i]) & !is.nan(all[ , i]), ]
}

df <- data.frame(term=character(0))
for (i in c("spei12", "spei24", "spei36", "spi12", "spi24", "spi36", "spei12gs", 
            "spei24gs", "spei36gs", "spi12gs", "spi24gs", "spi36gs")){

  all$spei <- all[ , i]

  mod <- lmer(haz_dhs ~ age + as.factor(calc_birthmonth) + 
                birth_order + hhsize + sex + mother_years_ed + toilet +
                head_age + head_sex + wealth_index + (1|country) + (1|surveycode) + 
                spei + spei^2, data=all[!is.infinite(all[ , i]) , ])
  
  sumry <- tidy(mod)
  
  sumry <- bind_rows(sumry, data.frame(estimate=AIC(mod), term='AIC'))
  
  names(sumry)[2:5] <- paste0(i, "_", names(sumry)[2:5])
  
  df <- merge(df, sumry, all=T)
  
  print(i)
}

write.csv(df, 'C://Users/matt/Desktop/SPI_regression_res.csv', row.names=F)
