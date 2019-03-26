library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)
library(grid)

setwd('G://My Drive/Dissertation/Visualizations/')

dat <- read.csv('individual_regressors.csv') %>%
  mutate(term=gsub('speiDry:', '', term),
         term=gsub('_t$', '', term))

labels <- read.csv('individual_regressors_labels.csv')

dat <- merge(dat, labels)

rank <- unique(dat[ , c("estimate", "term")])

dat$term <- factor(dat$term, levels=rank$term[order(rank$estimate)], ordered=T)

tex <- c('assistance'='Official Development Assistance Per Capita',
         'grid_hdi'='Human Development Index',
         'grid_gdp'='GDP (PPP) Per Capita',
         'market_dist'="Distance from A City of > 50,000",
         'government_effectiveness'='Government Effectiveness',
         'stability_violence'='Political Stability and Absence of Violence',
         'population'='Population Density',
         'tmax_10yr_mean'='Average Maximum Monthly Temperature',
         'nutritiondiversity_mfad'='Nutritional Diversity of Agriculture',
         'roughness'='Topographic Roughness',
         'mean_annual_precip'='Mean Annual Precipitation',
         'ndvi'='Normalized Difference Vegetation Index',
         'irrig_aai'='Percent of Agriculture Irrigated',
         'imports_percap'='Annual Import Value Per Capita',
         'crop_prod'='Annual Staple Crop Production',
         'enrollment'='Percent of Children in Primary School',
         'bare'='Percent of Bare Land Cover')

ggplot(dat) + 
  geom_bar(aes(x=term, y=estimate, fill=VarType), stat='identity', width=0.9) +
  geom_hline(aes(yintercept=0), linetype=3) + 
  scale_fill_manual(values=c("#386cb0", "#7fc97f")) +
  scale_x_discrete(labels=tex) + 
  coord_flip() + 
  theme_bw() + 
  xlab('') + 
  ylab('') + 
  theme(plot.margin = unit(c(0.1, 0.1, 0.7, -0.45), "cm"), 
        legend.position = c(0.1,-0.1), legend.direction = "horizontal",
        legend.title=element_blank())

ggsave("G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/figures/EffectSizes.pdf", width=5, height=4.5)
