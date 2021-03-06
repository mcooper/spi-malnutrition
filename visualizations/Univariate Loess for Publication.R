library(dplyr)
library(ggplot2)
library(lme4)
library(cowplot)

setwd('G://My Drive/DHS Processed/')

hh <- read.csv('HH_data_A.csv')
cov <- read.csv('SpatialCovars.csv')
spi <- read.csv('PrecipIndices.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hh, cov, spi))

all <- all %>%
  filter(builtup < 20 & bare < 95)

#Get Residuals
# mod <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
#               head_age + head_sex + wealth_index +
#               as.factor(calc_birthmonth) + (1|country) + (1|surveycode), data=all)
# 
# 
# all$residuals <- residuals(mod)
# 
# #spei24
# spei24mod <- loess(residuals ~ spei24, data = all, span = 0.75)

#save("spei24mod", file="LOESSmod.Rdata")

load('LOESSmod.Rdata')

pred <- function(predvar, mod.loess, varname){
  newdata <- data.frame(predvar=predvar)
  names(newdata) <- varname
  predict(mod.loess, newdata=newdata)
}

predvar <- seq(min(-3), max(3), len=100)

data <- data.frame(predvar)
data$spei24 <- sapply(FUN=pred, X=data$predvar, mod.loess=spei24mod, varname='spei24')


library(RColorBrewer)

bckgd <- expand.grid(seq(-0.0925, 0.02, len=100), seq(min(-3), max(3), len=100))

bckgd$Classification <- ifelse(bckgd$Var2 > 1.4, "Excessively Wet", ifelse(bckgd$Var2 < -0.4, "Drought", "Normal")) %>%
  factor(levels = c("Drought", "Normal", "Excessively Wet"))

dry <- prettyNum(sum(all$spei24 < -0.4),big.mark=",")
normal <- prettyNum(sum(all$spei24 >= -0.4 & all$spei24 <= 1.4),big.mark=",")
wet <- prettyNum(sum(all$spei24 > 1.4),big.mark=",")

labs <- data.frame(x=c(-1.7, 0.5, 2.2), y=c(-0.0875, -0.0875, -0.0875), z=c(paste0("Dry\nn = ", dry), 
                                                                            paste0("Normal\nn = ", normal), 
                                                                            paste0("Wet\nn = ", wet)))

curve <- ggplot() + 
  geom_tile(data=bckgd, aes(x=Var2, y=Var1, fill=Var2), alpha=0.5) + 
  #scale_fill_manual(values=c(Drought = "#dfc27d",  `Excessively Wet` = "#80cdc1", Normal="#F7F7F7")) + 
  scale_fill_gradient2(low = "#a6611a", high = "#018571", mid="#FFFFFF", 
                       guide = FALSE, midpoint=0) +
  scale_x_continuous(expand = c(0, 0), breaks=c(-3, -2, -1, -0.4, 0, 1, 1.4, 2, 3),
                     labels=c("-3", "-2", "-1", "-0.4", "0", "1", "1.4", "2", "3")) +
  scale_y_continuous(expand = c(0, 0), breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02)) +
  #xlab('24-Month Standardized Precipitation Evapotranspiration Index (SPEI)') + 
  xlab(NULL) + 
  ylab('Difference in HAZ Score from Prediction\n(Resiudal)') + 
  geom_hline(aes(yintercept=0), linetype=2, color="#202020") + 
  geom_vline(aes(xintercept=-0.4), color="#202020", linetype=3) + 
  geom_vline(aes(xintercept=1.4), color="#202020", linetype=3) +
  geom_line(data=data, aes(x=predvar, y=spei24), size=1.5) +
  annotate('text', x=-1.7, y=-0.0825, label="bold(Dry)", parse=T, size=5) + 
  annotate('text', x=-1.7, y=-0.09, label=paste('italic(n) == ', deparse(dry)), parse=T, size=5) + 
  annotate('text', x=0.5, y=-0.0825, label="bold(Normal)", parse=T, size=5) + 
  annotate('text', x=0.5, y=-0.09, label=paste('italic(n) == ', deparse(normal)), parse=T, size=5) + 
  annotate('text', x=2.2, y=-0.0825, label="bold(Wet)", parse=T, size=5) + 
  annotate('text', x=2.2, y=-0.09, label=paste('italic(n) == ', deparse(wet)), parse=T, size=5) + 
  #geom_text(data=labs, aes(x=x, y=y, label=z)) +
  theme_classic() + 
  theme(axis.text = element_text(size=13.9),
        axis.title = element_text(size=13.9)) #+ 
  #theme(plot.margin = unit(c(0.25, 0.25, 0.1, 0.25), "cm"))
curve

bckgd2 <- expand.grid(seq(0, 14000, len=100), seq(min(-3), max(3), len=100))

bckgd2$Classification <- ifelse(bckgd$Var2 > 1.4, "Excessively Wet", ifelse(bckgd$Var2 < -0.4, "Drought", "Normal")) %>%
  factor(levels = c("Drought", "Normal", "Excessively Wet"))


hist <- ggplot() + 
  geom_tile(data=bckgd2, aes(x=Var2, y=Var1, fill=Var2), alpha=0.5) + 
  #scale_fill_manual(values=c(Drought = "#dfc27d",  `Excessively Wet` = "#80cdc1", Normal="#F7F7F7")) + 
  scale_fill_gradient2(low = "#a6611a", high = "#018571", mid="#FFFFFF", 
                       guide = FALSE, midpoint=0) +
  scale_x_continuous(expand = c(0, 0), breaks=c(-3, -2, -1, -0.4, 0, 1, 1.4, 2, 3),
                     labels=c("-3", "-2", "-1", "-0.4", "0", "1", "1.4", "2", "3")) +
  scale_y_continuous(expand = c(0, 0),
                     breaks=c(0, 10000)) +
  xlab('24 Month SPEI') + 
  ylab('Count of HAZ\nObservations') + 
  geom_vline(aes(xintercept=-0.4), color="#202020", linetype=3) + 
  geom_vline(aes(xintercept=1.4), color="#202020", linetype=3) +
  geom_histogram(data=all, aes(x=spei24), alpha=0.75, binwidth=0.05) +
  theme_classic() + 
  theme(plot.margin = unit(c(0, 0.25, 0.25, 0.25), "cm"),
        axis.text = element_text(size=13.9),
        axis.title = element_text(size=13.9))
hist

plot_grid(plotlist=list(curve, hist), align='v', ncol=1, nrow=2, label_x=c(0.155, 0.155), label_y=c(0.99, 1), labels='AUTO', rel_heights=c(1, 0.4))


#Width of actual latex document
w <- 3.41801194445

#Scale by two
w <- w*2

setwd("C://Users/matt/spi-malnutrition-tex/")

ggsave("RainfallHeights.png", width = w, height = w*(5/6), units="in")

system("convert RainfallHeights.png RainfallHeights.pdf")
