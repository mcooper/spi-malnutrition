setwd('G://My Drive/DHS Processed')

library(dplyr)
library(tidyr)
library(ggplot2)

lc <- read.csv('landcover.csv')

lct <- lc %>% select(-interview_year) %>% gather(landcover, count, -code)

lcs <- lct %>% group_by(code) %>% summarize(total = sum(count))

lca <- merge(lct, lcs)

lca$percent <- lca$count/lca$total

ggplot(lca) + geom_bar(aes(x=landcover, y=percent), stat="sum") + coord_flip()
