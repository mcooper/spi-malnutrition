library(ggplot2)
library(dplyr)

setwd('G://My Drive/DHS Processed')

hh <- read.csv('HH_data_A.csv')
lc <- read.csv('landcover_processed.csv')
spei <- read.csv('PrecipIndices.csv')
spei_ind <- read.csv('PrecipIndices_Individual.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, lc, spei, spei_ind, cov))

all <- all %>%
  filter(spei24 > -3 & spei24 < 3 & urban_rural=='Rural')

sel <- all %>%
  filter(market_dist > 24*4)# & mean_annual_precip < 1000 & mean_annual_precip > 100)

#http://polisci.msu.edu/jacoby/icpsr/regress3/lectures/week4/16.MultiLoess.pdf

mod.loess <- loess(haz_dhs ~ natural + spei24, data = sel, span = 0.75)

#mod.loess <- readRDS('loess_model.rds')

landcover <- seq(0, 1, len=100)
spei <- seq(-2.5, 2.5, len=100)

data <- expand.grid(landcover, spei)
names(data) <- c('natural', 'spei24')

#fit.mod <- matrix(predict(mod.loess, data), 50, 50)
#persp(landcover, spi, fit.mod, theta=20)

pred <- function(natural, spei24){
  predict(mod.loess, newdata=data.frame(natural=natural, spei24=spei24))
}

data$prediction <- mapply(pred, natural=data$natural, spei24=data$spei24)
data$natural <- data$natural*100

sel <- sel %>%
  filter(spei24 > -3 & spei24 < 3)

ggplot(data, aes(x=natural, y=spei24)) + 
  geom_tile(aes(fill=prediction)) + 
  #geom_point(data=sel, aes(x=natural*100, y=spei24), size=0.05) + 
  #geom_text(aes(label=signif(prediction, 3))) + 
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
       caption="Source: DHS; CHIRPS; ESA-CCI Landcover; n=52,653") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        axis.title = element_text(face="bold"))
        

ggsave('G:/My Drive/Dissertation/Visualizations/SPEIvsNatural.png', 
       width=8, height=6)






