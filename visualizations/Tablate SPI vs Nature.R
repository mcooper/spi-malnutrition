library(ggplot2)
library(dplyr)

setwd('G://My Drive/DHS Processed')

hh <- read.csv('HH_data_A.csv')
lc <- read.csv('landcover_processed.csv')
spei <- read.csv('PrecipIndices.csv')
spei_ind <- read.csv('PrecipIndices_Individual.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, lc, spei, spei_ind, cov))

sel <- all %>%
  filter(-3 < spi24 & 3 > spi24 & !is.na(natural) & !is.na(spi24) &
           market_dist > 7*24 & market_dist < 31*24)

sel$naturalbin <- cut(sel$natural, breaks=seq(0, 1, 0.1))
sel$spibin <- cut(sel$spi24, breaks=c(-3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3))

sel$naturalbin[is.na(sel$naturalbin)] <- "(0.9,1]"

grp <- sel %>%
  group_by(naturalbin, spibin) %>%
  summarize(mean=mean(haz_dhs, na.rm=T),
            n=n(),
            surveys=length(unique(country)))

ggplot(grp %>% filter(n > 40), aes(x=naturalbin, y=spibin)) + geom_tile(aes(fill=mean), width=1, height=1) + 
  geom_text(aes(label=signif(mean, 3))) + 
  scale_fill_gradient(low = "red", high = "white", space = "Lab", na.value = "gray90", guide = "colourbar") +
  scale_x_discrete(expand = c(0, 0), position = "top") +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Fraction of Nearby Land With Natural Cover") +
  ylab("24-Month Standardized Precipitation Index") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA,color="gray90", size=0.5, linetype="solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="gray90"),
        plot.background = element_rect(fill="gray90"),
        axis.text = element_text(color="black", size=12),
        axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('G://My Drive/Dissertation/Visualizations/SPIvsNatural bins.png', width=8, height=6)

natsel1 <- sel$naturalbin %>% unique %>% .[1]
natsel10 <- sel$naturalbin %>% unique %>% .[10]
natsel5 <- sel$naturalbin %>% unique %>% .[5]
ggplot(sel, aes(spi24, haz_dhs)) + 
  geom_point(size=0.1) +
  geom_smooth(data=sel %>% filter(naturalbin == natsel1), aes(spi24, haz_dhs), color='red') +
  geom_smooth(data=sel %>% filter(naturalbin == natsel5), aes(spi24, haz_dhs), color='yellow') +
  geom_smooth(data=sel %>% filter(naturalbin == natsel10), aes(spi24, haz_dhs), color='green')
