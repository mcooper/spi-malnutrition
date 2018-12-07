library(ggplot2)
library(dplyr)
library(lme4)

setwd('~/dhsprocessed')

hh <- read.csv('HH_data_A.csv')
lc <- read.csv('landcover_processed.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, lc, spei, cov))

#Get Residuals
mod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + wealth_index + (1|surveycode) + (1|country), data=all)

all$residuals <- residuals(mod)


grp <- all %>% group_by(code) %>%
  summarize(residuals=mean(residuals))

plt <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(grp, lc, spei, cov)) %>%
  filter(market_dist > 24*7)


mod.urban <- loess(residuals ~ spei24, data = plt %>% filter(natural <= 0.6814218), span = 1)

mod.rural <- loess(residuals ~ spei24, data = plt %>% filter(natural > 0.6814218), span = 1)

mod <- c('urban', 'natural')
spei24 <- seq(-3, 3, len=100)

data <- expand.grid(mod, spei24)
names(data) <- c('mod', 'spei24')

pred <- function(spei24, mod){
  if (mod=='urban'){
    pred <- predict(mod.urban, newdata=data.frame(spei24=spei24))
  }
  if (mod=='natural'){
    pred <- predict(mod.natural, newdata=data.frame(spei24=spei24))
  }
  pred
}

data$prediction <- mapply(pred, mod=data$mod, spei24=data$spei24)

ggplot(data, aes(x=spei24, y=prediction, color=mod)) + 
  geom_line()