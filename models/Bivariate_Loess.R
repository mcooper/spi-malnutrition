library(ggplot2)
library(dplyr)
<<<<<<< HEAD

setwd('G://My Drive/DHS Processed')
=======
library(lme4)

setwd('~/dhsprocessed')
>>>>>>> b88f4c59aac4405e1f1095291e97aed370c54163

hh <- read.csv('HH_data_A.csv')
lc <- read.csv('landcover_processed.csv')
spei <- read.csv('PrecipIndices.csv')
spei_ind <- read.csv('PrecipIndices_Individual.csv')
cov <- read.csv('SpatialCovars.csv')
<<<<<<< HEAD

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, lc, spei, spei_ind, cov))
=======

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, lc, spei, spei_ind, cov))

all$spi_age_mix[all$age > 24] <- all$spi_age[all$age > 24]
all$spi_age_mix[all$age < 24] <- all$spi_ageutero[all$age < 24]
all$spi_gs_age_mix[all$age > 24] <- all$spi_gs_age[all$age > 24]
all$spi_gs_age_mix[all$age < 24] <- all$spi_gs_ageutero[all$age < 24]
all$spei_age_mix[all$age > 24] <- all$spei_age[all$age > 24]
all$spei_age_mix[all$age < 24] <- all$spei_ageutero[all$age < 24]
all$spei_gs_age_mix[all$age > 24] <- all$spei_gs_age[all$age > 24]
all$spei_gs_age_mix[all$age < 24] <- all$spei_gs_ageutero[all$age < 24]

market_cutoff=24*4
>>>>>>> b88f4c59aac4405e1f1095291e97aed370c54163

#First filter 
all <- all %>%
  filter(market_dist > market_cutoff)

#Get Residuals
mod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + urban_rural + wealth_index + (1|surveycode) + (1|country), data=all)

all$residuals <- residuals(mod)

setwd('~/graphs/')

analyze <- function(df, var){
  df$predvar <- df[ , var]
  
  cat('\nModeling')
  
  #http://polisci.msu.edu/jacoby/icpsr/regress3/lectures/week4/16.MultiLoess.pdf
  mod.loess <- loess(residuals ~ natural + predvar, data = df, span = 0.75)
  
  landcover <- seq(0, 1, len=100)
  predvar <- seq(-3, 3, len=100)
  
  data <- expand.grid(landcover, predvar)
  names(data) <- c('natural', 'predvar')
  
  pred <- function(natural, predvar){
    predict(mod.loess, newdata=data.frame(natural=natural, predvar=predvar))
  }
  
  cat('\nPredicting')
  
  data$prediction <- mapply(pred, natural=data$natural, predvar=data$predvar)
  data$natural <- data$natural*100
  
  sel <- df %>%
    filter(predvar > -3 & predvar < 3)
  
  cat('\nPlotting')
  
  ggplot(data, aes(x=natural, y=predvar)) + 
    geom_tile(aes(fill=prediction)) + 
    scale_fill_gradient2(low = "red", high = "green", mid="white", 
                         guide = "colourbar", midpoint=mean(data$prediction, na.rm=T),
                         name='Z-Score') +
    #xlim(0, 1) + ylim(-2.5, 2.5) + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title="HAZ Scores Across Gradients in SPI and Natural Land Cover",
         subtitle=expression('Modeled with a 2nd-Degree Polynomial Loess with '*alpha*'=0.75'),
         x="Fraction of Nearby Land With Natural Cover",
         y="24-Month Standardized Precipitation Index",
         caption="Source: DHS; CHIRPS; ESA-CCI Landcover") +
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0),
          axis.title = element_text(face="bold"))
  
  
  ggsave(paste0(var, 'vsNatural.png'), width=8, height=6)
  
  ggplot(data, aes(x=natural, y=predvar)) + 
    geom_tile(aes(fill=prediction)) + 
    geom_point(data=sel, aes(x=natural*100, y=predvar), size=0.01) + 
    scale_fill_gradient2(low = "red", high = "green", mid="white", 
                         guide = "colourbar", midpoint=mean(data$prediction, na.rm=T),
                         name='Z-Score') +
    #xlim(0, 1) + ylim(-2.5, 2.5) + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title="HAZ Scores Across Gradients in SPI and Natural Land Cover",
         subtitle=expression('Modeled with a 2nd-Degree Polynomial Loess with '*alpha*'=0.75'),
         x="Fraction of Nearby Land With Natural Cover",
         y="24-Month Standardized Precipitation Index",
         caption="Source: DHS; CHIRPS; ESA-CCI Landcover") +
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0),
          axis.title = element_text(face="bold"))
  
  
  ggsave(paste0(var, 'vsNatural_points.png'), width=8, height=6)
}


for (i in names(all)[grepl('sp', names(all))]){
  cat('******************\n', i, '\n******************')
  analyze(df=all, i)
}



