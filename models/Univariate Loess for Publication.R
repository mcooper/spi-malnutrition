library(dplyr)
library(ggplot2)
library(lme4)

setwd('~/dhsprocessed')

hh <- read.csv('HH_data_A.csv')
cov <- read.csv('SpatialCovars.csv')
spi <- read.csv('PrecipIndices.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, cov, spi))

#Get Residuals
mod <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + wealth_index + 
              as.factor(calc_birthmonth) + (1|country) + (1|surveycode), data=all)


all$residuals <- residuals(mod)

pred <- function(predvar, mod.loess, varname){
  newdata <- data.frame(predvar=predvar)
  names(newdata) <- varname
  predict(mod.loess, newdata=newdata)
}


sel <- all %>%
  group_by(code) %>%
  summarize(spei24=mean(spei24),
            residuals=mean(residuals))

predvar <- seq(min(-3), max(3), len=100)

data <- data.frame(predvar)

#spei24
spei24mod <- loess(residuals ~ spei24, data = all, span = 0.75)
data$spei24 <- sapply(FUN=pred, X=data$predvar, mod.loess=spei24mod, varname='spei24')

library(RColorBrewer)

bckgd <- expand.grid(seq(-0.09, 0.02, len=100), seq(min(-3), max(3), len=100))

bckgd$Classification <- ifelse(bckgd$Var2 > 1.4, "Excessively Wet", ifelse(bckgd$Var2 < -0.4, "Drought", "Normal")) %>%
  factor(levels = c("Drought", "Normal", "Excessively Wet"))

ggplot(data, aes(x=predvar, y=spei24)) + 
  geom_tile(data=bckgd, aes(x=Var2, y=Var1, fill=Classification)) + 
  scale_fill_manual(values=c(Drought = "#dfc27d",  `Excessively Wet` = "#80cdc1", Normal="#F7F7F7")) + 
  geom_line(size=1.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab('24-Month Standardized Precipitation Evapotranspiration Index') + 
  ylab('Difference in HAZ Score from Prediction (Resiudal)') + 
  geom_hline(aes(yintercept=0, linetype="Predicted HAZ Scores")) + 
  scale_linetype_manual(name = "", values = 2, 
                        guide = guide_legend(override.aes = list(color = "Black"))) +
  ggtitle('Rainfall and Predicted Child Heights') +
  theme_classic()

ggsave('Rainfall and Predicted Child Heights.png', width = 8, height = 6, units="in")
ggsave('Rainfall and Predicted Child Heights.svg', width = 8, height = 6, units="in")

save("mod", file="LOESSmod.Rdata")
