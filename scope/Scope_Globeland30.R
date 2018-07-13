setwd('G://My Drive/GlobeLand30/')

library(sp)
library(rgdal)
library(dplyr)


f2000 <- list.files('2000/2000_original/') %>%
  substr(1, 6)
f2010 <- list.files('2010_original/') %>%
  substr(1, 6)

sp <- readOGR('globemapsheet', 'GlobeMapSheet')

sp@data$f2000 <- sp@data$REMARK %in% f2000
sp@data$f2010 <- sp@data$REMARK %in% f2010
