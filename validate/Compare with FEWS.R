library(dplyr)

dat <- read.csv('G://My Drive/DHS Spatial Covars/FEWS Validation/FEWS_CS_SPEI_mod.csv')

dat$CS[dat$CS > 5] <- NA

dat_proc <- dat %>%
  select(-spei12, -spei36, -spi12, -spi24, -spi36) %>%
  mutate(drought=spei24 < -1,
         vulnerable=mod < -0.) %>%
  arrange(latitude, longitude, yearmonth) %>%
  group_by(latitude, longitude) %>%
  mutate(CS_increase = (CS - lag(CS)) > 0,
         drought_diff = drought - lag(drought)) %>%
  filter(drought_diff == 1)
  
table(dat_proc[ , c('vulnerable', 'CS_increase')])/sum(table(dat_proc[ , c('vulnerable', 'CS_increase')]))

library(ggplot2)

ggplot(aes(longitude, latitude, color=vulnerable), data=dat_proc) + 
  geom_point()
