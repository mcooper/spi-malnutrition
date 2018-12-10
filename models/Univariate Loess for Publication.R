library(dplyr)
library(ggplot2)
library(doParallel)
library(lme4)

setwd('~/dhsprocessed')
setwd('G://My Drive/DHS Processed')

hh <- read.csv('HH_data_A.csv')
cov <- read.csv('SpatialCovars.csv')
spi <- read.csv('PrecipIndices.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, cov, spi))

#Get Residuals
mod <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + wealth_index + (1|country) + (1|surveycode), data=all)


all$residuals <- residuals(mod)

sel <- all %>%
  group_by(code, country) %>%
  summarize(residuals=mean(residuals),
            spei24=mean(spei24),
            market_dist=mean(market_dist)) %>%
  filter(!is.infinite(spei24)) %>%
  data.frame

pred <- function(predvar, mod.loess, varname){
  newdata <- data.frame(predvar=predvar)
  names(newdata) <- varname
  predict(mod.loess, newdata=newdata)
}

predvar <- seq(min(sel$market_dist), max(sel$market_dist), len=100)

data <- data.frame(predvar)

#market_dist
market_distmod <- loess(residuals ~ market_dist, data = sel, span = 0.75)
data$market_dist <- sapply(FUN=pred, X=data$predvar, mod.loess=market_distmod, varname='market_dist')

ggplot(data, aes(x=log(predvar/60), y=market_dist)) + 
  geom_line(size=1.5) +
  scale_x_continuous(labels=function(x){round(exp(x), 2)}) + 
  labs(title="Market Access and Predicted Child Heights",
       x="Hours to Travel to A City of > 50,000 (Log Scale)",
       y="Difference from Prediction (Residual)") +
  theme_bw()

ggsave('C://Users/matt/Desktop/Markets Distance.png', width=6, height=4.5)
