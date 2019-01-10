library(dplyr)
library(tidyr)

setwd('G://My Drive/DHS Processed')

gl <- read.csv('globeland30.csv')

glp <- gl %>% gather(landcover, value, -interview_year, -code) %>%
  mutate(year=as.numeric(substr(landcover, nchar(landcover)-3, nchar(landcover))),
         type=substr(landcover, 1, nchar(landcover)-5)) %>%
  select(-landcover) %>%
  spread(type, value) %>%
  filter((year==2000 & interview_year <= 2005) | (year==2010 & interview_year >= 2006)) %>%
  select(-year)

glp$Perc_Forest <- glp$Forest/ rowSums(glp %>% select(-code, -interview_year))
glp$Perc_Grassland <- glp$Grassland/ rowSums(glp %>% select(-code, -interview_year))
glp$Perc_Water <- glp$Water/ rowSums(glp %>% select(-code, -interview_year))
glp$Perc_Natural <- rowSums(glp %>% select(Perc_Forest, Perc_Grassland, Perc_Water))

write.csv(glp, 'globeland30_processed.csv', row.names=F)