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
  select(code, spi24, spi6, spi12, spi36, interview_month, interview_year, mean_annual_precip) %>%
  unique

md <- read.csv('MarketDist.csv')
gdp <- read.csv('country_gdp.csv')

md00 <- merge(select(md, code, md=market2000), data.frame(interview_year=seq(1988, 2007)))
md15 <- merge(select(md, code, md=market2015), data.frame(interview_year=seq(2008, 2016)))

md <- Reduce(bind_rows, list(md00, md15))

hh <- read.csv('hhvars.csv') %>%
  select(haz_dhs, whz_dhs, code, interview_year, interview_month, country, urban_rural, wealth_index)

#hh <- hh %>% filter(wealth_index == 'Poorest')

all <- Reduce(function(x, y){merge(x,y,all.x=F, all.y=F)}, list(hh, lc, spi, md, gdp)) %>%
  filter(urban_rural == 'Rural')

sel <- all %>%
  filter(-3 < spi24 & 3 > spi24 & !is.na(natural) & !is.na(spi24) &
           md > 7*24 & md < 31*24)

sel$naturalbin <- cut(sel$natural, breaks=seq(0, 1, 0.1))
sel$spibin <- cut(sel$spi24, breaks=c(-3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3))

sel$naturalbin[is.na(sel$naturalbin)] <- "(0.9,1]"

grp <- sel %>%
  group_by(naturalbin, spibin) %>%
  summarize(mean=mean(haz_dhs, na.rm=T),
            n=n(),
            surveys=length(unique(country)))

ggplot(grp %>% filter(n > 40), aes(x=naturalbin, y=spibin)) + geom_tile(aes(fill=mean), width=1, height=1) + 
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
ggsave('G://My Drive/Dissertation/Visualizations/SPIvsNatural bins.png', width=8, height=6)

natsel1 <- sel$naturalbin %>% unique %>% .[1]
natsel10 <- sel$naturalbin %>% unique %>% .[10]
natsel5 <- sel$naturalbin %>% unique %>% .[5]
ggplot(sel, aes(spi24, haz_dhs)) + 
  geom_point(size=0.1) +
  geom_smooth(data=sel %>% filter(naturalbin == natsel1), aes(spi24, haz_dhs), color='red') +
  geom_smooth(data=sel %>% filter(naturalbin == natsel5), aes(spi24, haz_dhs), color='yellow') +
  geom_smooth(data=sel %>% filter(naturalbin == natsel10), aes(spi24, haz_dhs), color='green')
