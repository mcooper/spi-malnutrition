setwd("C://Git/spi-malnutrition/visualizations/")

library(tidyr)
library(dplyr)

res <- tidy(mod)

res$estimate <- signif(res$estimate, 2)
res$std.error <- signif(res$std.error, 2)
res$pval <- pnorm(-abs(res$statistic), 0, 1)*2
res$stars <- ifelse(res$pval < 0.001, "***",
                    ifelse(res$pval < .01, "**", 
                           ifelse(res$pval < 0.05, "*", "")))

labels <- read.csv('labels.csv')

res <- merge(res, labels)

hh_ind <- res %>%
  filter(level=='child') %>%
  arrange(label) %>%
  dplyr::select(label, estimate, std.error, stars) %>%
  write.csv('modres_child.csv')

hh_ind <- res %>%
  filter(level=='geo') %>%
  arrange(label) %>%
  dplyr::select(label, estimate, std.error, stars) %>%
  write.csv('modres_geo.csv')

geo_dry <- res %>%
  filter(grepl('speiDry:', term)) %>%
  dplyr::select(label, dryest=estimate, drysd=std.error, drystars=stars)

geo_wet <- res %>%
  filter(grepl('speiWet:', term)) %>%
  dplyr::select(label, wetest=estimate, wetsd=std.error, wetstars=stars)

geo <- merge(geo_dry, geo_wet) %>%
  arrange(label)

write.csv(geo, 'modres_spei.csv')
