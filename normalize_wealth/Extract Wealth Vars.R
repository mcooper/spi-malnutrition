setwd('G://My Drive/DHS Data/')

library(foreign)
library(dplyr)
library(readstata13)

##############################
#Scope Available Datasets
#################################

kr <- list.files(pattern='^..(KR|kr).....(DTA|dta)$')
pr <- list.files(pattern='^..(PR|pr).....(DTA|dta)$')
wi <- list.files(pattern='^..(WI|wi).....(DTA|dta)$')

makeFileNameDf <- function(f){
  num <- substr(f, 5, 5)
  cc <- toupper(substr(f, 1, 2))
  subversion <- ifelse(toupper(substr(f, 6, 6)) %in% as.character(seq(0, 9)), 1,
                       ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[1:8], 2, 
                              ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[9:17], 3, 
                                     ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[18:26], 4))))
  data.frame(num, cc, subversion, file=f)
}

kr_df <- lapply(X = kr, FUN = makeFileNameDf) %>%
  Reduce(f = bind_rows) %>%
  rename(kr=file)
pr_df <- lapply(X = pr, FUN = makeFileNameDf) %>%
  Reduce(f = bind_rows) %>%
  rename(pr=file)
wi_df <- lapply(X = wi, FUN = makeFileNameDf) %>%
  Reduce(f = bind_rows) %>%
  rename(wi=file)

scope <- Reduce(function(x,y){merge(x, y, all.x=T, all.y=T)}, list(kr_df, pr_df, wi_df))
#It seems that there are some with only KR but every file with HR also has KR

#Use PR.  If it is missing use KR. Also grab WI if it is there
vardat <- read.csv('C://Git/spi-malnutrition/normalize_wealth/WealthVars.csv',
                   stringsAsFactors=F)

alldata <- data.frame()
for (i in 1:nrow(scope)){
  print(i/nrow(scope))
  
  if (!is.na(scope$pr[i])){
  #Get values from HR files
    dat <- read.dta13(scope$pr[i])
    
    dat <- dat[ , vardat$pr[vardat$pr %in% names(dat)]]
    
    for (n in 1:nrow(vardat)){
      names(dat)[names(dat)==vardat$pr[n]] <- vardat$variable[n]
    }
                            
  } else{
  #Get values from KR file in HR is missing
    dat <- read.dta13(scope$kr[i])
    
    dat <- dat[ , vardat$kr[vardat$kr %in% names(dat)]]
    
    for (n in 1:nrow(vardat)){
      names(dat)[names(dat)==vardat$kr[n]] <- vardat$variable[n]
    }
    
  }
  
  if (!is.na(scope$wi[i])){
  #Get wealth data if it is missing
    wi <- read.dta13(scope$wi[i])
    names(wi) <- c('hhid', 'wealth_factor', 'wealth_index')
    
    wi$wealth_factor <- as.character(wi$wealth_factor)
    wi <- wi[ , c('hhid', 'wealth_factor')]
    
    if (sum(!dat$hhid %in% wi$hhid) > 0){
      #If hhid doesnt match, try it with just hhno and clusterid
      padws <- function(vect, n=0){
        len <- max(nchar(vect))
        padded <- paste0("     ", vect)
        substr(padded, nchar(padded) - (len - 1), nchar(padded))
      }
      
      dat$hhid <- paste0(padws(dat$clusterid), padws(dat$householdno))
      idlen <- nchar(dat$hhid)[1]
      dat$hhid <- paste0(paste0(rep(" ", 12-idlen), collapse=''), dat$hhid)
      
    }
    
    if (sum(!dat$hhid %in% wi$hhid) > 0){
      #Check again for matching, if not, cat error
      cat('Mismatches in wealth data for ', scope$cc[i], scope$num[i], scope$subversion[i], '\n') 
    }
    
    dat <- merge(dat, wi, all.x=T, all.y=F)
  }
  
  dat$num <- scope$num[i]
  dat$cc <- scope$cc[i]
  dat$subversion <- scope$subversion[i]
  
  dat <- dat %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.integer, as.character)
  
  if ('wealth_factor' %in% names(dat)){
    if (sum(dat[ , 'wealth_factor']) < nrow(dat)){
      dat$wealth_factor <- as.integer(dat$wealth_factor)
      
      alldata <- bind_rows(alldata, unique(dat))
    }
  }
}

###############################
#Calculate Cutpoints
###############################

#High end anchoring cutpoints
alldata$telephone <- alldata$telephone %in% c('1', 'yes', 'Yes')
alldata$television <- alldata$television %in% c('1', 'yes', 'Yes')
alldata$car_truck <- alldata$car_truck %in% c('1', 'yes', 'Yes')
alldata$refrigerator <- alldata$refrigerator %in% c('1', 'yes', 'Yes')

#Low end anchoring cutpoints
setwd("C:/Git/spi-malnutrition/normalize_wealth/")

floor <- read.csv('floor_materials.csv')
walls <- read.csv('wall_materials.csv')
toilet <- read.csv('toilet_type.csv')
primary_school <- read.csv('primary_school.csv')
drinking_water <- read.csv('drinking_water.csv')

alldata <- Reduce(merge, list(alldata, floor, walls, toilet, primary_school, drinking_water))

alldata$urban <- alldata$urban_rural %in% c('urban', 'Urban')

alldata$inadequate_housing <- alldata$inadequate_floor | alldata$inadequate_walls
alldata$inadequate_sanitation <- alldata$inadequate_toilet | (alldata$inadequate_water_urban & alldata$urban) | (alldata$inadequate_water_urban & !alldata$urban)
alldata$crowding <- (as.numeric(alldata$hhsize)/as.numeric(alldata$sleeping_rooms)) > 3

getHHheadPrimary <- function(unfinished_primary, individual_number, household_head){
  unfinished <- unfinished_primary[individual_number==unique(household_head)]
  if (length(unfinished) > 1){
    return(NA)
  } else{
    return(unfinished)
  }
}

hh <- alldata %>% unique %>%
  group_by(hhid, householdno, cluserid, cc, num, subversion, television, refrigerator, car_truck, wealth_factor,
                           telephone, inadequate_housing, inadequate_sanitation, crowding) %>%
  #Need to fix this later
  summarize(head_noprimary=getHHheadPrimary(unfinished_primary, individual_number, household_head)) %>%
  #summarize(head_noprimary = unfinished_primary[individual_number==household_head[1]][1]) %>%
  data.frame

#Calculate Cutpoints
sdf <- hh %>% select(cc, num, subversion) %>% unique

baseline <- hh %>% filter(cc=='NG' & num=="5" & subversion==1)

others <- sdf %>% filter(!(cc=='NG' & num=="5" & subversion==1))

for (i in 1:nrow(others)){
  sel <- hh %>% filter(cc==others$cc[i] & num==others$num[i] & subversion==others$subversion[i])
  
  #Drop columns that are greater than half NA
  for (col in c("inadequate_housing", "inadequate_sanitation", "crowding", "head_noprimary",
              "television", "refrigerator", "car_truck", "wealth_factor", "telephone")){
    if (sum(is.na(sel[ , col])) > nrow(sel)/2){
      sel[ , col] <- NULL
    }
  }
  
  bsl <- baseline
  
  needs <- c("inadequate_housing", "inadequate_sanitation", "crowding", "head_noprimary")

  needs <- needs[needs %in% names(sel)]
  
  if (length(needs) > 1){
    bsl$deprivation <- rowSums(bsl[ , needs])
    sel$deprivation <- rowSums(sel[ , needs])
  } else{
    bsl$deprivation <- bsl[ , needs]
    sel$deprivation <- sel[ , needs]
  }
  
  bsl_anchors <- NULL
  sel_anchors <- NULL
  
  #First do deprivation, depending on the number of indicators of deprivation available
  if (length(needs) > 1){
    for (j in 1:length(needs)){
      bsl$depcut <- bsl$deprivation >= j
      sel$depcut <- sel$deprivation >= j
      
      moddep_sel <- glm(depcut ~ wealth_factor, data = sel, family = "binomial")
      moddep_bsl <- glm(depcut ~ wealth_factor, data = bsl, family = "binomial")
      
      bsl_anchors <- c(bsl_anchors, moddep_bsl$coefficients[1]/moddep_bsl$coefficients[2])
      sel_anchors <- c(sel_anchors, moddep_sel$coefficients[1]/moddep_sel$coefficients[2])
    }
  }
  
  #Then do Wealth Anchors
  #Television
  if ("television" %in% names(sel)){
    modtv_sel <- glm(television ~ wealth_factor, data = sel, family = "binomial")
    modtv_bsl <- glm(television ~ wealth_factor, data = bsl, family = "binomial")
    
    bsl_anchors <- c(bsl_anchors, modtv_bsl$coefficients[1]/modtv_bsl$coefficients[2])
    sel_anchors <- c(sel_anchors, modtv_sel$coefficients[1]/modtv_sel$coefficients[2])
  }
  
  #Telephone
  if ("telephone" %in% names(sel)){
    modphone_sel <- glm(telephone ~ wealth_factor, data = sel, family = "binomial")
    modphone_bsl <- glm(telephone ~ wealth_factor, data = bsl, family = "binomial")
    
    bsl_anchors <- c(bsl_anchors, modphone_bsl$coefficients[1]/modphone_bsl$coefficients[2])
    sel_anchors <- c(sel_anchors, modphone_sel$coefficients[1]/modphone_sel$coefficients[2])
  }
  
  #Car or Truck
  if ("car_truck" %in% names(sel)){
    modcar_sel <- glm(car_truck ~ wealth_factor, data = sel, family = "binomial")
    modcar_bsl <- glm(car_truck ~ wealth_factor, data = bsl, family = "binomial")
    
    bsl_anchors <- c(bsl_anchors, modcar_bsl$coefficients[1]/modcar_bsl$coefficients[2])
    sel_anchors <- c(sel_anchors, modcar_sel$coefficients[1]/modcar_sel$coefficients[2])
  }
  
  #Refrigerator
  if ("refrigerator" %in% names(sel)){
    modref_sel <- glm(refrigerator ~ wealth_factor, data = sel, family = "binomial")
    modref_bsl <- glm(refrigerator ~ wealth_factor, data = bsl, family = "binomial")
    
    bsl_anchors <- c(bsl_anchors, modref_bsl$coefficients[1]/modref_bsl$coefficients[2])
    sel_anchors <- c(sel_anchors, modref_sel$coefficients[1]/modref_sel$coefficients[2])
  }

  anchormod <- lm(bsl_anchors ~ sel_anchors)
  
  others[i, 'intercept'] <- anchormod$coefficients[1]
  others[i, 'slope'] <- anchormod$coefficients[2]
  
  print(i/nrow(others))
}

##########################################
#Model Cutputs in relation to Baseline
##########################################

#Baseline is Nigeria 5 1 from 2008.  Large sample size, complete records, wide variety of income levels

for (i in 1:nrow(others)){
  hh_ix <- hh$cc == others$cc[i] & hh$num == others$num[i] & hh$subversion == others$subversion[i]
  
  hh$wealth_factor[hh_ix] <- hh$wealth_factor[hh_ix]*others$slope[i] + others$aintercept[i]
  
}


