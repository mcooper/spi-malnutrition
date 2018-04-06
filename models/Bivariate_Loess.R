library(dplyr)

setwd('G://My Drive/DHS Processed')

lc <- read.csv('landcover.csv')

human <- paste0('cci_', c('10', '11', '12', '20', '30', '190', '200', '201', '202', '220'))
natural <- paste0('cci_', c('40', '50', '60', '61', '62', '70', '71', '80', '90', '100', '110', '120', '121', '122',
                            '130', '140', '150', '152', '153', '160', '170', '180', '210'))

getPercetCover <- function(selcols, allcolmatch, df){
  if(length(selcols) > 1){
    selcolsum <- rowSums(df[ , selcols[selcols %in% names(df)]], na.rm=T)
  } else{
    selcolsum <- df[ , selcols]
  }
  allcolsum <- rowSums(df[ , grepl(allcolmatch, names(df))], na.rm=T)
  return(selcolsum/allcolsum)
}

lc$human <- getPercetCover(human, 'cci_', lc)
lc$natural <- getPercetCover(natural, 'cci_', lc)

lc <- lc %>%
  select(code, interview_year, human, natural)

spi <- read.csv('Coords&SPI.csv') %>%
  select(code, spi24, spi6, spi12, spi36, interview_month, interview_year) %>%
  unique

hh <- read.csv('hhvars.csv') %>%
  select(haz_dhs, whz_dhs, code, interview_year, interview_month, country, urban_rural, wealth_index)

all <- Reduce(function(x, y){merge(x,y,all.x=F, all.y=F)}, list(hh, lc, spi))

#http://polisci.msu.edu/jacoby/icpsr/regress3/lectures/week4/16.MultiLoess.pdf
mod.loess <- loess(haz_dhs ~ natural + spi24, data = all)
