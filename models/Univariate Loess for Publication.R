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
            ag_pct_gdp=mean(ag_pct_gdp), 
            market_dist=mean(market_dist), 
            ndvi=mean(ndvi), 
            population=mean(population), 
            builtup=mean(builtup), 
            irrigation=mean(irrigation),
            haz=mean(haz_dhs),
            gdp=mean(gdp)) %>%
  filter(!is.infinite(spei24)) %>%
  data.frame

pred <- function(predvar, mod.loess, varname){
  newdata <- data.frame(predvar=predvar)
  names(newdata) <- varname
  predict(mod.loess, newdata=newdata)
}

predvar <- seq(min(sel$spei24), max(sel$spei24), len=100)

data <- data.frame(predvar)

#spei24
spei24mod <- loess(residuals ~ spei24, data = sel, span = 0.75)
data$spei24 <- sapply(FUN=pred, X=data$predvar, mod.loess=spei24mod, varname='spei24')

ggplot(data, aes(x=predvar, y=spei24)) + 
  geom_line(size=1.5) +
  labs(title="Rainfall and Predicted Child Heights",
       x="24-Month Standardized Precipitation Evapotranspiration Index",
       y="Difference from Prediction (Residual)") +
  theme_bw()

ggsave('C://Users/matt/Desktop/gdp and HAZ.png', width=6, height=4.5)
