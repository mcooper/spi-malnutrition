setwd('G://My Drive/DHS Processed/')

library(dplyr)
library(tidyr)
library(ggplot2)

lc <- read.csv('landcover.csv')
hh <- read.csv('hhvars.csv') %>%
  select(code, urban_rural) %>%
  unique

lc <- merge(lc, hh)

lc <- lc %>% filter(urban_rural == 'Rural') %>%
  select(-urban_rural)

lct <- lc %>% select(-interview_year) %>% gather(landcover, count, -code)

lcs <- lct %>% group_by(code) %>% summarize(total = sum(count))

lca <- merge(lct, lcs)

lca$percent <- lca$count/lca$total

labels <- read.csv('landcover_mapping.csv')

lca <- merge(labels, lca)

lca <- lca %>% group_by(class, code) %>%
  summarize(percent = sum(percent)) %>%
  group_by(class) %>%
  summarize(Percent = mean(percent))

lca$Percent <- lca$Percent*100

ggplot(lca) + 
  geom_bar(aes(x=class, y=Percent, fill=class), stat='Identity') + 
  xlab('') + 
  coord_flip() + 
  scale_fill_manual(values=c('Water'=rgb(0/255, 92/255, 230/255), 'Urban'="gray30", "Forest"=rgb(38/255, 115/255, 0/255),
                             "Grass and Shrubland"=rgb(152/255, 230/255, 0/255), "Agriculture"=rgb(255/255, 235/255, 175/255))) + 
  ggtitle('Land Cover Types Across Entire DHS') + 
  theme_bw() + 
  guides(fill=FALSE)
ggsave('G://My Drive/Dissertation/Visualizations/Land Cover Types.png',
       height=3, width=7)
  