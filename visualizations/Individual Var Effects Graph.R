library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)
library(grid)

setwd('G://My Drive/Dissertation/Visualizations/')

dat <- read.csv('individual_regressors.csv') %>%
  gather(Key, Value, -Variable) %>%
  mutate(Period=ifelse(grepl("Normal", Key), "Normal", "Drought"),
         Key=ifelse(grepl("SE", Key), "SE", "Estimate")) %>%
  spread(Key, Value) %>%
  mutate(maxval=Estimate + SE*2,
         minval=Estimate - SE*2)

labels <- read.csv('individual_regressors_labels.csv')

dat <- merge(dat, labels)

dat$Variable <- factor(dat$Variable, levels=labels$Variable[rev(labels$Order)], ordered=T)

tex <- c('assistance'=expression(paste('Official Development Assistance Per Capita\n                                                ($10 per capita)')),
         'grid_hdi'=expression(paste('                    Human Development Index\n                                        (10 percentile)')),
         'grid_gdp'=expression(paste('                            GDP (PPP) Per Capita\n                                  ($1000 per capita)')),
         'government_effectiveness'=expression(paste('                   Government Effectiveness\n                                     (10 percentile)')),
         'stability_violence'=expression(paste('Political Stability and Absence of Violence\n                                           (10 percentile)')),
         'population'=expression(paste('                              Population\n  (1000 individuals per sq km)')),
         'tmax_10yr_mean'=expression(paste('        Average Maximum Monthly Temperature\n                                           (1 degree celsius)')),
         'nutritiondiversity_mfad'=expression(paste('                   Agricultural Nutritional Diversity\n                                 (10 percentile)')),
         'roughness'=expression(paste('                  Topographic Roughness\n                                  (10 percentile)')),
         'mean_annual_precip'=expression(paste('                  Mean Annual Precipitation\n                                               (10mm)')),
         'ndvi'=expression(paste('  Normalized Difference Vegetation Index\n                                         (10 percentile)')),
         'irrig_aai'=expression(paste('           Percent of Agriculture Irrigated\n                                       (10 percent)')))

ggplot(dat) + 
  geom_bar(aes(x=Variable, y=Estimate, fill=VarType), stat='identity', width=0.9) +
  geom_errorbar(aes(ymin=minval, ymax=maxval, x=Variable), width=0.6) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  scale_fill_manual(values=c("#386cb0", "#7fc97f")) +
  scale_x_discrete(labels=tex) + 
  scale_y_continuous(breaks=seq(-0.05, 0.05, 0.05)) + 
  coord_flip() + 
  theme_bw() + 
  xlab('') + 
  ylab('Associated Change in HAZ Scores') + 
  facet_grid(. ~ Period) +
  theme(plot.margin = unit(c(0.1, 0.1, 0, -0.45), "cm"), legend.position="bottom",
        legend.title=element_blank())

ggsave("G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/figures/EffectSizes.pdf", width=7.42, height=8.31)
