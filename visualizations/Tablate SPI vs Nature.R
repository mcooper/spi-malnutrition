library(ggplot2)
library(dplyr)

setwd('D://Documents and Settings/mcooper/GitHub/spi-malnutrition/')

lc <- read.csv('data/landcover.csv')

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

spi <- read.csv('data/Coords&SPI.csv') %>%
  select(code, spi24, spi6, spi12, spi36, interview_month=month, interview_year=year)

hh <- read.csv('data/hhvars.csv') %>%
  select(haz_dhs, whz_dhs, code, interview_year, interview_month, country, urban_rural, wealth_index)

hh <- hh %>%
  filter(wealth_index == 'Poorest')

all <- Reduce(function(x, y){merge(x,y,all.x=F, all.y=F)}, list(hh, lc, spi))

all <- all %>%
  filter((all$spi24 > -2.5 & all$spi24 < 2.5) & !is.na(natural))

all$naturalbin <- cut(all$natural, breaks=seq(0, 1, 0.1))
all$spibin <- cut(all$spi24, breaks=seq(-2.5, 2.5, 0.5))

all$naturalbin[is.na(all$naturalbin)] <- "(0.9,1]"

grp <- all %>%
  group_by(naturalbin, spibin) %>%
  summarize(mean=mean(haz_dhs, na.rm=T),
            n=n(),
            surveys=length(unique(country)))

ggplot(grp %>% filter(n > 0), aes(x=naturalbin, y=spibin)) + geom_tile(aes(fill=mean), width=1, height=1) + 
  geom_text(aes(label=signif(mean, 3)))


# ggplot(tab, aes(`Co-Occuring Word`, Keyword)) + geom_tile(aes(fill=PMI), width=0.9, height=0.9) + 
#   geom_text(data=tab, aes(`Co-Occuring Word`, Keyword, label = signif(WordCount, digits=2)), color="black", size=rel(4.5)) +
#   scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "gray90", guide = "colourbar") +
#   scale_x_discrete(expand = c(0, 0), position = "top") +
#   scale_y_discrete(expand = c(0, 0)) +
#   xlab("") + 
#   ylab("") +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(fill=NA,color="gray90", size=0.5, linetype="solid"),
#         axis.line = element_blank(),
#         axis.ticks = element_blank(),
#         panel.background = element_rect(fill="gray90"),
#         plot.background = element_rect(fill="gray90"),
#         axis.text = element_text(color="black", size=14), 
#         axis.text.x = element_text(angle = 90, hjust = 1))
