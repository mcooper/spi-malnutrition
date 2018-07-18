library(ggplot2)
library(dplyr)
library(lme4)

setwd('~/dhsprocessed')

hh <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
spei_ind <- read.csv('PrecipIndices_Individual.csv')
cov <- read.csv('SpatialCovars.csv')

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
              head_age + head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country), data=all)

all$residuals <- residuals(mod)

all <- all[!is.na(all$builtup), ]

all$builtup <- log(all$builtup)

setwd('~/graphs/builtup')

analyze <- function(df, var){
  df$predvar <- df[ , var]
  
  df <- df %>%
    select(residuals, builtup, predvar) %>%
    filter(!is.infinite(predvar)) %>%
    na.omit
  
  cat('\nModeling')
  
  #http://polisci.msu.edu/jacoby/icpsr/regress3/lectures/week4/16.MultiLoess.pdf
  mod.loess <- loess(residuals ~ builtup + predvar, data = df, span = 0.75)
  
  builtup <- seq(-0.1, 4, len=100)
  predvar <- seq(-3, 3, len=100)
  
  data <- expand.grid(builtup, predvar)
  names(data) <- c('builtup', 'predvar')
  
  pred <- function(builtup, predvar){
    predict(mod.loess, newdata=data.frame(builtup=builtup, predvar=predvar))
  }
  
  cat('\nPredicting')
  
  data$prediction <- mapply(pred, builtup=data$builtup, predvar=data$predvar)
  
  cat('\nPlotting')
  
  ggplot(data, aes(x=builtup, y=predvar)) + 
    geom_tile(aes(fill=prediction)) + 
    scale_fill_gradient2(low = "red", high = "green", mid="white", 
                         guide = "colourbar", midpoint=mean(data$prediction, na.rm=T),
                         name='Z-Score') +
    #xlim(0, 1) + ylim(-2.5, 2.5) + 
    scale_x_continuous(expand = c(0, 0), labels=exp) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title=var,
         x="% Built Up",
         y="Precipitation Index") +
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0),
          axis.title = element_text(face="bold"))
  
  
  ggsave(paste0(var, 'vsBuiltup.png'), width=8, height=6)
  
  ggplot(data, aes(x=builtup, y=predvar)) + 
    geom_tile(aes(fill=prediction)) + 
    geom_point(data=df, aes(x=builtup, y=predvar), size=0.01) + 
    scale_fill_gradient2(low = "red", high = "green", mid="white", 
                         guide = "colourbar", midpoint=mean(data$prediction, na.rm=T),
                         name='Z-Score') +
    #xlim(0, 1) + ylim(-2.5, 2.5) + 
    scale_x_continuous(expand = c(0, 0), labels=exp) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title=var,
         x="% Built Up",
         y="Precipitation Index") +
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0),
          axis.title = element_text(face="bold"))
  
  
  ggsave(paste0(var, 'vsBuiltup_points.png'), width=8, height=6)
}


for (i in names(all)[grepl('sp', names(all))]){
  cat('******************\n', i, '\n******************')
  analyze(df=all, i)
}



