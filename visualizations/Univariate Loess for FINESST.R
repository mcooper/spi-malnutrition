library(dplyr)
library(ggplot2)
library(lme4)
library(cowplot)

setwd('G://My Drive/DHS Processed/')

hh <- read.csv('HH_data_A.csv')
cov <- read.csv('SpatialCovars.csv')
spi <- read.csv('PrecipIndices.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, cov, spi))

all <- all %>%
  filter(builtup < 20 & bare < 95)

#Get Residuals
# mod <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
#               head_age + head_sex + wealth_index +
#               as.factor(calc_birthmonth) + (1|country) + (1|surveycode), data=all)
# 
# 
# all$residuals <- residuals(mod)
# 
# #spei24
# spei24mod <- loess(residuals ~ spei24, data = all, span = 0.75)

#save("spei24mod", file="LOESSmod.Rdata")

load('LOESSmod.Rdata')

pred <- function(predvar, mod.loess, varname){
  newdata <- data.frame(predvar=predvar)
  names(newdata) <- varname
  predict(mod.loess, newdata=newdata)
}

predvar <- seq(min(-3), max(3), len=100)

data <- data.frame(predvar)
data$spei24 <- sapply(FUN=pred, X=data$predvar, mod.loess=spei24mod, varname='spei24')


library(RColorBrewer)

bckgd <- expand.grid(seq(-0.085, 0.02, len=100), seq(min(-3), max(3), len=100))

bckgd$Classification <- ifelse(bckgd$Var2 > 1.4, "Excessively Wet", ifelse(bckgd$Var2 < -0.4, "Drought", "Normal")) %>%
  factor(levels = c("Drought", "Normal", "Excessively Wet"))

ggplot() + 
  geom_tile(data=bckgd, aes(x=Var2, y=Var1, fill=Var2), alpha=0.4) + 
  scale_fill_gradient2(low = "#a6611a", high = "#018571", mid="#FFFFFF", 
                       guide = FALSE, midpoint=0) +
  scale_x_continuous(expand = c(0, 0), breaks=c(-3, -2, -1, 0, 1, 2, 3),
                     labels=c("-3", "-2", "-1", "0", "1", "2", "3")) +
  scale_y_continuous(expand = c(0, 0), breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02)) +
  xlab('24-Month Standardized Precipitation Evapotranspiration Index (SPEI)') + 
  ylab('Difference in HAZ Score from Prediction (Resiudal)') +  
  geom_line(data=data, aes(x=predvar, y=spei24), size=1.5) +
  theme_bw() #+ 

ggsave('G://My Drive/Proposals/FINESST/LoessCurve.png', width=6, height=5)
