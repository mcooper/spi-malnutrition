setwd('G://My Drive/GlobeLand30/')

library(sp)
library(rgdal)
library(dplyr)

clean <- function(s){
  paste0(substr(s,1,1),
         as.numeric(substr(s,2,3)),
         '_',
         as.numeric(substr(s,5,6)))
}

f2000 <- list.files('2000/2000_original/') %>%
  substr(1, 6) %>%
  sapply(clean)
f2010 <- list.files('2010_original/') %>%
  substr(1, 6) %>%
  sapply(clean)

sp <- readOGR('globemapsheet', 'GlobeMapSheet')

sp@data$f2000 <- sp@data$REMARK %in% f2000
sp@data$f2010 <- sp@data$REMARK %in% f2010

View(sp@data[ , c('REMARK', 'f2000', 'f2010')])
