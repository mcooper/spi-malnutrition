setwd('G://My Drive/DHS Data/')

library(foreign)
library(dplyr)
library(readstata13)

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
for (i in 150:170){
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
    dat$wealth_factor <- as.integer(dat$wealth_factor)
    
    alldata <- bind_rows(alldata, dat)
  }
}

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

hh <- alldata %>% group_by(hhid, householdno, cluserid, cc, num, subversion, television, refrigerator, car_truck, wealth_factor,
                           telephone, inadequate_housing, inadequate_sanitation, crowding) %>%
  #Need to fix this later
  summarize(head_noprimary = unfinished_primary[individual_number==household_head[1]][1]) %>%
  data.frame

#Calculate Cutpoints
cutpoints <- data.frame()

sdf <- hh %>% select(cc, num, subversion) %>% unique

for (i in 1:nrow(sdf)){
  sel <- hh %>% filter(cc==sdf$cc[i] & num==sdf$num[i] & subversion==sdf$subversion[i])
  
  if (sum(is.na(sel$inadequate_housing)) < nrow(sel)){
    modhouse <- glm(inadequate_housing ~ wealth_factor, data = sel, family = "binomial")
    house <- modhouse$coefficients[1]/modhouse$coefficients[2]
  } else{
    house <- NA
  }
  if (sum(is.na(sel$crowding)) < nrow(sel)){
    modcrowding <- glm(crowding ~ wealth_factor, data = sel, family = "binomial")
    crowding <- modcrowding$coefficients[1]/modcrowding$coefficients[2]
  } else{
    crowding <- NA
  }
  if (sum(is.na(sel$head_noprimary)) < nrow(sel)){
    modhead_noprimary <- glm(head_noprimary ~ wealth_factor, data = sel, family = "binomial")
    head_noprimary <- modhead_noprimary$coefficients[1]/modhead_noprimary$coefficients[2]
  } else{
    head_noprimary <- NA
  }
  if (sum(is.na(sel$inadequate_sanitation)) < nrow(sel)){
    modsani <- glm(inadequate_sanitation ~ wealth_factor, data = sel, family = "binomial")
    sani <- modsani$coefficients[1]/modsani$coefficients[2]
  } else{
    sani <- NA
  }
  if (sum(is.na(sel$television)) < nrow(sel)){
    modtv <- glm(television ~ wealth_factor, data = sel, family = "binomial")
    tv <- modtv$coefficients[1]/modtv$coefficients[2]
  } else{
    tv <- NA
  }
  if (sum(is.na(sel$telephone)) < nrow(sel)){
    modphone <- glm(telephone ~ wealth_factor, data = sel, family = "binomial")
    phone <- modphone$coefficients[1]/modphone$coefficients[2]
  } else{
    phone <- NA
  }
  if (sum(is.na(sel$car_truck)) < nrow(sel)){
    modcar <- glm(car_truck ~ wealth_factor, data = sel, family = "binomial")
    car <- modcar$coefficients[1]/modcar$coefficients[2]
  } else{
    car <- NA
  }
  if (sum(is.na(sel$refrigerator)) < nrow(sel)){
    modfridge <- glm(refrigerator ~ wealth_factor, data = sel, family = "binomial")
    fridge <- modfridge$coefficients[1]/modfridge$coefficients[2]
  } else{
    fridge <- NA
  }

  df <- data.frame(cc=sdf$cc[i], num=sdf$num[i], subversion=sdf$subversion[i],
                   sani, house, head_noprimary, crowding, tv, phone, car, fridge)
  
  cutpoints <- bind_rows(df, cutpoints)
  
}


#Baseline is Nigeria 5 1 from 2008.  Large sample size, complete records, wide variety of income levels
baseline <- cutpoints %>% filter(cc=='NG' & num=="5" & subversion==1)
for (i in 1:nrow(sdf)){
  sel <- cutpoints %>% filter(cc==sdf$cc[i] & num==sdf$num[i] & subversion==sdf$subversion[i])
  
  df <- data.frame(baseline=unlist(baseline[4:11]), sel=unlist(sel[4:11]))
  
  mod <- lm(baseline~sel, data=df)
  
  cutpoints$b[cutpoints$cc==sdf$cc[i] & cutpoints$num==sdf$num[i] & cutpoints$subversion==sdf$subversion[i]] <- mod$coefficients[2]
  cutpoints$a[cutpoints$cc==sdf$cc[i] & cutpoints$num==sdf$num[i] & cutpoints$subversion==sdf$subversion[i]] <- mod$coefficients[1]
}

for (i in 1:nrow(cutpoints)){
  hh_ix <- hh$cc == cutpoints$cc[i] & hh$num == cutpoints$num[i] & hh$subversion == cutpoints$subversion[i]
  
  hh$wealth_factor <- hh$wealth_factor*cutpoints$b[i] + cutpoints$a[i]
  
}


