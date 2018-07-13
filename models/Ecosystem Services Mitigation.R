setwd('G://My Drive/DHS Processed')

library(ggplot2)
library(dplyr)
library(broom)
library(lme4)

hh <- read.csv('hhvars.csv')
hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')
fs <- read.csv('FarmingSystems.csv')

hh <- hh[ , c(names(hha), 'whz_dhs')] %>% na.omit

################################
#Combine and clear workspace
################################
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spei, cov, lc, fs))

all$farm_system <- paste(all$continent, all$farm_system, sep='-')

#rm(list=setdiff(ls(), "all")) #remove everything but our data

###########################################################
#Analyze missing data, determine which variables to keep
############################################################

all <- all[!is.infinite(all$spei36) & !is.infinite(all$spei12gs), ]

all$interview_month <- as.factor(all$interview_month)

library(lme4)

alldf <- data.frame()

for (f in all$farm_system %>% unique){
  sel <- all %>%
    filter(farm_system == f & urban_rural=='Rural')
  
  if(length(unique(sel$surveycode))==1 | nrow(sel) < 1000){
    next
  }
  
  whz <- lmer(whz_dhs ~ interview_year + interview_month + age + birth_order + hhsize + sex + mother_years_ed +
                toilet + head_age + log(builtup) + market_dist + wealth_index + spei12gs + (1|surveycode), data=sel)
  
  whz_es <- lmer(whz_dhs ~ interview_year + interview_month + age + birth_order + hhsize + sex + mother_years_ed +
                toilet + head_age + log(builtup) + market_dist + wealth_index + spei12gs + natural + (1|surveycode), data=sel)

  haz <- lmer(whz_dhs ~ interview_year + interview_month + age + birth_order + hhsize + sex + mother_years_ed +
                toilet + head_age  + log(builtup) + market_dist + wealth_index + spei36 + (1|surveycode), data=sel)
  
  haz_es <- lmer(whz_dhs ~ interview_year + interview_month + age + birth_order + hhsize + sex + mother_years_ed +
                   toilet + head_age + log(builtup) + market_dist + wealth_index + spei36 + natural + (1|surveycode), data=sel)
  
  whz_b_change <- (tidy(whz) %>% filter(term == 'spei12gs') %>% select(estimate) %>% abs -
    tidy(whz_es) %>% filter(term == 'spei12gs') %>% select(estimate) %>% abs) %>% as.numeric
  
  whz_p_change <- (tidy(whz) %>% filter(term == 'spei12gs') %>% select(statistic) %>% abs -
              tidy(whz_es) %>% filter(term == 'spei12gs') %>% select(statistic) %>% abs) %>% as.numeric

  haz_b_change <- (tidy(haz) %>% filter(term == 'spei36') %>% select(estimate) %>% abs -
                     tidy(haz_es) %>% filter(term == 'spei36') %>% select(estimate) %>% abs) %>% as.numeric

  haz_p_change <- (tidy(haz) %>% filter(term == 'spei36') %>% select(statistic) %>% abs -
                     tidy(haz_es) %>% filter(term == 'spei36') %>% select(statistic) %>% abs) %>% as.numeric
  
  tmp <- data.frame(whz_b, whz_b_change, whz_p, whz_p_change, haz_b, haz_b_change, haz_p, haz_p_change)
  
  tmp$farmsystem <- f
  
  print(f)
  
  alldf <- rbind(alldf, tmp)
}

alldf$whz_mitigation <- alldf$whz_b_change > 0.5 & alldf$whz_p_change > 0
alldf$haz_mitigation <- alldf$haz_b_change > 0.5 & alldf$whz_p_change > 0

alldf$mitigation <- alldf$whz_mitigation | alldf$haz_mitigation

######################################
#Map results
######################################



library(sp)
library(rgdal)

farms <- readOGR('G:/My Drive/DHS Spatial Covars/Farm Systems', 'all_farming_systems')

farms@data <- data.frame(farms@data, alldf[match(farms@data[,'FarmSystem'], alldf[,'farmsystem']),])

fort <- fortify(farms)

dat <- cbind(fort, farms@data[fort$id, ])

dat$`HAZ Mitigation` <- ifelse(is.na(dat$haz_mitigation), "Insufficient Data",
                                 ifelse(!dat$haz_mitigation, "No Observed Effect", 
                                        ifelse(dat$haz_mitigation, "\"Safety Net\" Effect Observed", "")))

dat$`WHZ Mitigation` <- ifelse(is.na(dat$whz_mitigation), "Insufficient Data",
                                 ifelse(!dat$whz_mitigation, "No Observed Effect", 
                                        ifelse(dat$whz_mitigation, "\"Safety Net\" Effect Observed", "")))

dat$Mitigation <- ifelse(is.na(dat$mitigation), "Insufficient Data",
                           ifelse(!dat$mitigation, "No Observed Effect", 
                                  ifelse(dat$mitigation, "\"Safety Net\" Effect Observed", "")))

# Something is going wrong with the mapping
# 
# ggplot(dat, aes(long, lat, group=group)) + 
#   geom_polygon(aes(fill=FarmSystem=='ssa_fs_final-9. Maize'))+
#   scale_x_continuous(expand = c(0.0,0)) +
#   scale_y_continuous(expand = c(0.0,0)) + 
#   scale_fill_manual(values=c("#50dc2e", "#828881")) +#, "#dcbf2e")) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank())
# 
# 




