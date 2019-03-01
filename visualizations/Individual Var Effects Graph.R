library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

setwd('G://My Drive/Dissertation/Visualizations/')

dat <- read.csv('individual_regressors.csv') %>%
  gather(Key, Value, -Variable) %>%
  mutate(Period=ifelse(grepl("Normal", Key), "Normal", "Drought"),
         Key=ifelse(grepl("SE", Key), "SE", "Estimate")) %>%
  spread(Key, Value) %>%
  mutate(max=Estimate + SE*2,
         min=Estimate - SE*2)

labels <- read.csv('individual_regressors_labels.csv')

dat <- merge(dat, labels)

dat$Variable <- factor(dat$Variable, levels=labels$Variable[rev(labels$Order)], ordered=T)

dat <- dat %>%
  rename(`Variables Characterizing`=VarType)

tex <- c('assistance'=expression(paste('Annual Overseas Development Assistance\n                                           ($10 per capita)')),
         'imports_percap'=expression(paste('                    Annual Value of Imports\n                               ($100 per capita)')),
         'grid_gdp'=expression(paste('                                    GDP PPP\n                      ($1000 per capita)')),
         'government_effectiveness'=expression(paste('                   Government Effectiveness\n                                      (10 percentile)')),
         'stability_violence'=expression(paste('Political Stability and Absence of Violence\n                                            (10 percentile)')),
         'population'=expression(paste('                                    Population\n         (400 individuals per sq km)')),
         'tmax_10yr_mean'=expression(paste('        Average Maximum Monthly Temperature\n                                             (1 degree celsius)')),
         'nutritiondiversity_h'=expression(paste('                   Crop Nutrition Diversity\n                                 (10 percentile)')),
         'elevation'=expression(paste('                                  Elevation\n                                   (100 m)')),
         'mean_annual_precip'=expression(paste('                  Mean Annual Precipitation\n                                               (10mm)')),
         'ndvi'=expression(paste('     Normalized Difference Vegetation Index\n                                              (10 percentile)')),
         'irrig_aai'=expression(paste('           Percent of Agriculture Irrigated\n                                        (10 percent)')))

ggplot(dat) + 
  geom_pointrange(aes(x=Variable, ymin=min, ymax=max, y=Estimate, color=`Variables Characterizing`),
                  size=0.5) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  scale_x_discrete(labels=tex) + 
  coord_flip() + 
  theme_bw() + 
  xlab('') + 
  ylab('Predicted Change in HAZ Scores') + 
  facet_grid(. ~ Period) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, -0.5), "cm"), legend.position="none")

ggsave("G://My Drive/Dissertation/Visualizations/Individual Variable Effects.png", width=6, height=7)
