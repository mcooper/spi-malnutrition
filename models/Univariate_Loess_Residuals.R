library(ggplot2)
library(dplyr)
library(lme4)
library(foreach)
library(doParallel)

setwd('~/dhsprocessed')
setwd('G://My Drive/DHS Processed')

hh <- read.csv('hhvars.csv')
hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
spei_ind <- read.csv('PrecipIndices_Individual.csv')
cov <- read.csv('SpatialCovars.csv')

hh <- hh[ , c(names(hha), 'whz_dhs')] %>% na.omit

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, spei, spei_ind, cov))

all$spi_age_mix[all$age > 24] <- all$spi_age[all$age > 24]
all$spi_age_mix[all$age < 24] <- all$spi_ageutero[all$age < 24]
all$spi_gs_age_mix[all$age > 24] <- all$spi_gs_age[all$age > 24]
all$spi_gs_age_mix[all$age < 24] <- all$spi_gs_ageutero[all$age < 24]
all$spei_age_mix[all$age > 24] <- all$spei_age[all$age > 24]
all$spei_age_mix[all$age < 24] <- all$spei_ageutero[all$age < 24]
all$spei_gs_age_mix[all$age > 24] <- all$spei_gs_age[all$age > 24]
all$spei_gs_age_mix[all$age < 24] <- all$spei_gs_ageutero[all$age < 24]

#Get Residuals
mod <- lmer(haz_dhs ~ interview_year + (1|country/interview_month) + age + birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country) + (1|code), data=all)

all$residuals <- residuals(mod)

analyze <- function(df, var){
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
  
  ggsave(paste0(var, '_ResidualLoess_InterceptCode.png'), width=8, height=6)
}

cl <- makeCluster(4, outfile = '')
registerDoParallel(cl)

foreach(i=1:names(all)[grepl('sp', names(all))], .packages=c('ggplot2', 'dplyr')) %dopar% {
  cat('******************\n', i, '\n******************')
  analyze(df=all, i)
}



