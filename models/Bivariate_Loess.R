library(dplyr)
library(ggplot2)

setwd('G://My Drive/DHS Processed')

lc <- read.csv('landcover.csv')
covars <- read.csv('SpatialCova')
gdp <- read.csv('country_gdp.csv')

md00 <- merge(select(md, code, md=market2000), data.frame(interview_year=seq(1988, 2007)))
md15 <- merge(select(md, code, md=market2015), data.frame(interview_year=seq(2008, 2016)))

md <- Reduce(bind_rows, list(md00, md15))

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
  select(code, spi24, spi6, spi12, spi36, interview_month, interview_year,
         mean_annual_precip) %>%
  unique

hh <- read.csv('hhvars.csv') %>%
  select(haz_dhs, whz_dhs, code, interview_year, interview_month, country, urban_rural, wealth_index,
         latitude, longitude)

all <- Reduce(function(x, y){merge(x,y,all.x=F, all.y=F)}, list(hh, lc, spi, md, gdp))

all <- all %>%
  filter(spi24 > -3 & spi24 < 3 & urban_rural=='Rural')

sel <- all %>%
  filter(md > 24*4)# & mean_annual_precip < 1000 & mean_annual_precip > 100)

#http://polisci.msu.edu/jacoby/icpsr/regress3/lectures/week4/16.MultiLoess.pdf

mod.loess <- loess(haz_dhs ~ natural + spi24, data = sel, span = 0.75)

#mod.loess <- readRDS('loess_model.rds')

landcover <- seq(0, 1, len=100)
spi <- seq(-2.5, 2.5, len=100)

data <- expand.grid(landcover, spi)
names(data) <- c('natural', 'spi24')

#fit.mod <- matrix(predict(mod.loess, data), 50, 50)
#persp(landcover, spi, fit.mod, theta=20)

pred <- function(natural, spi24){
  predict(mod.loess, newdata=data.frame(natural=natural, spi24=spi24))
}

data$prediction <- mapply(pred, natural=data$natural, spi24=data$spi24)
data$natural <- data$natural*100

sel <- sel %>%
  filter(spi24 > -3 & spi24 < 3)

ggplot(data, aes(x=natural, y=spi24)) + 
  geom_tile(aes(fill=prediction)) + 
  geom_point(data=sel, aes(x=natural*100, y=spi24), size=0.05) + 
  #geom_text(aes(label=signif(prediction, 3))) + 
  scale_fill_gradient2(low = "red", high = "green", mid="white", 
                       guide = "colourbar", midpoint=mean(data$prediction, na.rm=T),
                       name='Z-Score') +
  #xlim(0, 1) + ylim(-2.5, 2.5) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title="HAZ Scores Across Gradients in SPI and Natural Land Cover",
       subtitle=expression('Modeled with a 2nd-Degree Polynomial Loess with '*alpha*'=0.75'),
       x="Fraction of Nearby Land With Natural Cover",
       y="24-Month Standardized Precipitation Index",
       caption="Source: DHS; CHIRPS; ESA-CCI Landcover; n=52,653") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        axis.title = element_text(face="bold"))
        

ggsave('G:/My Drive/Dissertation/Visualizations/SPIvsNatural.png', 
       width=8, height=6)






