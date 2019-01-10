library(ggplot2)
library(dplyr)
library(lme4)

setwd('G://My Drive/DHS Processed')

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
  summarize(residuals=median(residuals))

plt <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(grp, lc, spei, cov)) %>%
  filter(market_dist > 150)
# 
# plt <- all %>%
#   filter(market_dist > 150)

mod.urban <- loess(residuals ~ spei24, data = plt %>% filter(forest <= median(plt$forest)), span = 1)

mod.rural <- loess(residuals ~ spei24, data = plt %>% filter(forest > median(plt$forest)), span = 1)

mod <- c('> Median Forest Cover', '<= Median Forest Cover')
spei24 <- seq(-3, 3, len=100)

data <- expand.grid(mod, spei24)
names(data) <- c('mod', 'spei24')

pred <- function(spei24, mod){
  if (mod=='<= Median Forest Cover'){
    pred <- predict(mod.urban, newdata=data.frame(spei24=spei24))
  }
  if (mod=='> Median Forest Cover'){
    pred <- predict(mod.rural, newdata=data.frame(spei24=spei24))
  }
  pred
}

data$prediction <- mapply(pred, mod=data$mod, spei24=data$spei24)

ggplot(data, aes(x=spei24, y=prediction, color=mod)) + 
  geom_line(size=1.5) +
  labs(title="Rainfall and Predicted Child Heights - Song et al.",
       subtitle="For Households > 2.5 Hours From A Major City",
       x="24-Month Standardized Precipitation Evapotranspiration Index",
       y="Difference from Prediction (Residual)") +
  scale_color_manual(values=c('#4CA950', '#E6673E')) +
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position=c(0.05,0.05),
        legend.justification=c(0,0))

ggsave('C://Users/matt/Desktop/SongForest.png', width=6, height=4.5)
