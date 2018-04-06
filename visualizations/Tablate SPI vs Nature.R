library(ggplot2)
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

#hh <- hh %>% filter(wealth_index == 'Poorest')

all <- Reduce(function(x, y){merge(x,y,all.x=F, all.y=F)}, list(hh, lc, spi))

all <- all %>%
  filter((all$spi24 > -2.5 & all$spi24 < 2.5) & !is.na(natural) & !is.na(spi24))

all$naturalbin <- cut(all$natural, breaks=seq(0, 1, 0.1))
all$spibin <- cut(all$spi24, breaks=seq(-2.5, 2.5, 0.5))

all$naturalbin[is.na(all$naturalbin)] <- "(0.9,1]"

grp <- all %>%
  group_by(naturalbin, spibin) %>%
  summarize(mean=mean(haz_dhs, na.rm=T),
            n=n(),
            surveys=length(unique(country)))

ggplot(grp %>% filter(n > 0), aes(x=naturalbin, y=spibin)) + geom_tile(aes(fill=mean), width=1, height=1) + 
  geom_text(aes(label=signif(mean, 3))) + 
  scale_fill_gradient(low = "red", high = "white", space = "Lab", na.value = "gray90", guide = "colourbar") +
  scale_x_discrete(expand = c(0, 0), position = "top") +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Fraction of Nearby Land With Natural Cover") +
  ylab("24-Month Standardized Precipitation Index") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA,color="gray90", size=0.5, linetype="solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="gray90"),
        plot.background = element_rect(fill="gray90"),
        axis.text = element_text(color="black", size=12),
        axis.text.x = element_text(angle = 90, hjust = 1))

