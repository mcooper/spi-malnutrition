setwd('G://My Drive/DHS Processed')

library(ggplot2)
library(dplyr)
library(broom)
library(lme4)

hh <- read.csv('hhvars.csv')
hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
spei_ind <- read.csv('PrecipIndices_Individual.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')
fs <- read.csv('FarmingSystems.csv')

hh <- hh[ , c(names(hha), 'whz_dhs')] %>% na.omit

################################
#Combine and clear workspace
################################
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spei, cov, lc, fs))

all$spi_age_mix[all$age > 24] <- all$spi_age[all$age > 24]
all$spi_age_mix[all$age < 24] <- all$spi_ageutero[all$age < 24]
all$spi_gs_age_mix[all$age > 24] <- all$spi_gs_age[all$age > 24]
all$spi_gs_age_mix[all$age < 24] <- all$spi_gs_ageutero[all$age < 24]
all$spei_age_mix[all$age > 24] <- all$spei_age[all$age > 24]
all$spei_age_mix[all$age < 24] <- all$spei_ageutero[all$age < 24]
all$spei_gs_age_mix[all$age > 24] <- all$spei_gs_age[all$age > 24]
all$spei_gs_age_mix[all$age < 24] <- all$spei_gs_ageutero[all$age < 24]

all$farm_system <- paste(all$continent, all$farm_system, sep='-')

#rm(list=setdiff(ls(), "all")) #remove everything but our data

###########################################################
#Analyze missing data, determine which variables to keep
############################################################

all$interview_month <- as.factor(all$interview_month)

all$builtup[all$builtup==0] <- 0.01

library(lme4)

makeMitigationMap <- function(index, market_distance){
  
  alldf <- data.frame()
  
  for (f in all$farm_system %>% unique){
    sel <- all %>%
      filter(farm_system == f & market_dist > market_distance)
    
    sel$precip <- sel[ , index]^2
    
    sel <- sel[!is.infinite(sel$precip), ]
    
    if(length(unique(sel$surveycode))==1 | nrow(sel) < 1000){
      next
    }
    
    whz <- lmer(whz_dhs ~ interview_year + interview_month + age + birth_order + hhsize + sex + mother_years_ed +
                  toilet + head_age + log(builtup) + market_dist + wealth_index + precip + (1|surveycode), data=sel)
    
    whz_es <- lmer(whz_dhs ~ interview_year + interview_month + age + birth_order + hhsize + sex + mother_years_ed +
                  toilet + head_age + log(builtup) + market_dist + wealth_index + precip + natural + (1|surveycode), data=sel)
  
    haz <- lmer(haz_dhs ~ interview_year + interview_month + age + birth_order + hhsize + sex + mother_years_ed +
                  toilet + head_age  + log(builtup) + market_dist + wealth_index + precip + (1|surveycode), data=sel)
    
    haz_es <- lmer(haz_dhs ~ interview_year + interview_month + age + birth_order + hhsize + sex + mother_years_ed +
                     toilet + head_age + log(builtup) + market_dist + wealth_index + precip + natural + (1|surveycode), data=sel)
    
    whz_b_change <- (tidy(whz) %>% filter(term == 'precip') %>% select(estimate) %>% abs -
      tidy(whz_es) %>% filter(term == 'precip') %>% select(estimate) %>% abs) %>% as.numeric
    
    whz_p_change <- (tidy(whz) %>% filter(term == 'precip') %>% select(statistic) %>% abs -
                tidy(whz_es) %>% filter(term == 'precip') %>% select(statistic) %>% abs) %>% as.numeric
  
    haz_b_change <- (tidy(haz) %>% filter(term == 'precip') %>% select(estimate) %>% abs -
                       tidy(haz_es) %>% filter(term == 'precip') %>% select(estimate) %>% abs) %>% as.numeric
  
    haz_p_change <- (tidy(haz) %>% filter(term == 'precip') %>% select(statistic) %>% abs -
                       tidy(haz_es) %>% filter(term == 'precip') %>% select(statistic) %>% abs) %>% as.numeric
    
    tmp <- data.frame(whz_b_change, whz_p_change, haz_b_change, haz_p_change)
    
    tmp$farmsystem <- f
    
    print(f)
    
    alldf <- rbind(alldf, tmp)
  }
  
  alldf$whz_mitigation <- alldf$whz_b_change > 0.5 & alldf$whz_p_change > 0
  alldf$haz_mitigation <- alldf$haz_b_change > 0.5 & alldf$whz_p_change > 0
  
  ######################################
  #Map results
  ######################################
  
  library(sp)
  library(rgdal)
  
  farms <- readOGR('G:/My Drive/DHS Spatial Covars/Farm Systems', 'all_farming_systems')
  
  farms@data <- data.frame(farms@data, alldf[match(farms@data[,'FarmSystem'], alldf[,'farmsystem']),])
  
  fort <- fortify(farms)
  
  dat <- cbind(fort, farms@data[fort$id, ])
  
  dat$Mitigation <- ifelse(is.na(dat$haz_mitigation), "Insufficient Data",
                             ifelse(!dat$haz_mitigation & !dat$whz_mitigation, "No Observed Effect", 
                                    ifelse(dat$whz_mitigation & !dat$haz_mitigation, "WHZ Effect Observed", 
                                           ifelse(!dat$whz_mitigation & dat$haz_mitigation, "HAZ Effect Observed",
                                                  ifelse(dat$whz_mitigation & dat$haz_mitigation, "WHZ and HAZ Effects Observed", "")))))
  
  
  ggplot(dat, aes(long, lat, group=group)) +
    geom_polygon(aes(fill=`Mitigation`))+
    scale_x_continuous(expand = c(0.0,0)) +
    scale_y_continuous(expand = c(0.0,0)) +
    scale_fill_manual(values=c("HAZ Effect Observed"="#23b9bc", 
                               "Insufficient Data"="#d3d3d3", 
                               "No Observed Effect"="#a8a8a8", 
                               "WHZ Effect Observed"="#bcaf23", 
                               "WHZ and HAZ Effects Observed"="#37bc23")) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  
  ggsave(paste0("G://My Drive/Dissertation/Mitigation Maps/", index, '-', market_distance, '-squared.png'))
}

for (i in names(all)[grepl('sp', names(all))]){
  cat('******************\n', i, '\n******************')
  makeMitigationMap(i, 24)
}




