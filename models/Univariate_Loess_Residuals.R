library(ggplot2)
library(dplyr)
library(lme4)
library(foreach)
library(doParallel)

setwd('~/dhsprocessed')
setwd('G://My Drive/DHS Processed')

hh <- read.csv('hhvars.csv')
hha <- read.csv('HH_data_A.csv')
cov <- read.csv('SpatialCovars.csv')
spi <- read.csv('Coords&Precip.csv')

hh <- hh[ , c(names(hha), 'whz_dhs')] %>% na.omit

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, cov, spi))

#Get Residuals
mod <- lmer(haz_dhs ~ interview_year + (1|country/calc_birthmonth) + age + birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + urban_rural + wealth_index, data=all)

all$residuals <- residuals(mod)

analyze <- function(df, var, outcome){
  df$predvar <- df[ , var]
  
  df <- df %>%
    select(residuals, predvar) %>%
    filter(!is.infinite(predvar)) %>%
    na.omit
  
  cat('\nModeling')
  
  mod.loess <- loess(residuals ~ predvar, data = df, span = 0.75)
  
  predvar <- seq(min(df$predvar), max(df$predvar), len=100)
  
  data <- data.frame(predvar)
  
  pred <- function(predvar){
    predict(mod.loess, newdata=data.frame(predvar=predvar))
  }
  
  cat('\nPredicting')
  
  data$prediction <- mapply(pred, predvar=data$predvar)
  
  cat('\nPlotting')
  
  ggplot(data, aes(x=predvar, y=prediction)) + 
    geom_line() +
    labs(title=var,
         x=var,
         y="Residual") +
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0),
          axis.title = element_text(face="bold"))
  
  ggsave(paste0(outcome, '-', var, '_ResidualLoess_NoIntercepts.png'), width=8, height=6)
}

cl <- makeCluster(4, outfile = '')
registerDoParallel(cl)

foreach(i=c("ag_pct_gdp", "bare", "precip_10yr_mean", 
            "forest", "gdp", "government_effectiveness", "irrigation", "market_dist", 
            "ndvi", "population", "stability_violence", "tmax_10yr_mean", 
            "tmin_10yr_mean", "crop_prod", "fieldsize", "nutritiondiversity", 
            "builtup", "elevation", "spi12", "spei12", 'spi24', 'spei24',
            'spi36', 'spei36', "spei12gs",
            "spei24gs", "spei36gs", "spi12gs", "spi24gs", "spi36gs"), .packages=c('ggplot2', 'dplyr')) %dopar% {
  cat('******************\n', i, '\n******************')
  analyze(df=all, i, 'haz')
}

#Get Residuals
mod <- lmer(whz_dhs ~ interview_year + (1|country/interview_month) + age + birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country) + (1|code), data=all)

all$residuals <- residuals(mod)

foreach(i=c("ag_pct_gdp", "bare", "precip_10yr_mean", 
            "forest", "gdp", "government_effectiveness", "irrigation", "market_dist", 
            "ndvi", "population", "stability_violence", "tmax_10yr_mean", 
            "tmin_10yr_mean", "crop_prod", "fieldsize", "nutritiondiversity", 
            "builtup", "elevation", "spi12", "spei12", 'spi24', 'spei24',
            'spi36', 'spei36', "spei12gs",
            "spei24gs", "spei36gs", "spi12gs", "spi24gs", "spi36gs"), .packages=c('ggplot2', 'dplyr')) %dopar% {
  cat('******************\n', i, '\n******************')
  analyze(df=all, i, 'whz')
}


