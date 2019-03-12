setwd('G://My Drive/DHS Data/')

library(foreign)
library(dplyr)

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
vardat <- read.csv('C://Git/spi-malnutrition/scope/WealthVars.csv',
                   stringsAsFactors=F)

alldata <- data.frame()
for (i in 1:nrow(scope)){
  print(i/nrow(scope))
  
  if (!is.na(scope$pr[i])){
  #Get values from HR files
    dat <- read.dta(scope$pr[i], convert.factors=F)
    
    dat <- dat[ , vardat$pr[vardat$pr %in% names(dat)]]
    
    for (n in 1:nrow(vardat)){
      names(dat)[names(dat)==vardat$pr[n]] <- vardat$variable[n]
    }
                            
  } else{
  #Get values from KR file in HR is missing
    dat <- read.dta(scope$kr[i], convert.factors=F)
    
    dat <- dat[ , vardat$kr[vardat$kr %in% names(dat)]]
    
    for (n in 1:nrow(vardat)){
      names(dat)[names(dat)==vardat$kr[n]] <- vardat$variable[n]
    }
    
  }
  
  if (!is.na(scope$wi[i])){
  #Get wealth data if it is missing
    wi <- read.dta(scope$wi[i])
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
  
  if ('wealth_factor' in names(dat)){
    dat$wealth_factor <- as.integer(dat$wealth_factor)
    
    alldata <- bind_rows(alldata, dat)
  }
}

sumna <- function(x){sum(is.na(x))/length(x)}

allsum <- alldata %>%
  group_by(cc, num, subversion) %>%
  summarize(wealth_factor=sumna(wealth_factor),
            hhid=sumna(hhid),
            householdno=sumna(householdno),
            cluserid=sumna(cluserid),
            country=sumna(country),
            wall_materials=sumna(wall_materials),
            floor_materials=sumna(floor_materials),
            sleeping_rooms=sumna(sleeping_rooms),
            toilet_type=sumna(toilet_type),
            drinking_water=sumna(drinking_water),
            urban_rural=sumna(urban_rural),
            primary_school=sumna(primary_school),
            hhsize=sumna(hhsize),
            age=sumna(age),
            television=sumna(television),
            refrigerator=sumna(refrigerator),
            car_truck=sumna(car_truck),
            telephone=sumna(telephone))

allsum$total <- rowSums(data.frame(allsum) %>% select(-cc, -num, -subversion) > 0.5)

#Baseline is Nigeria 5 1 from 2008.  Large sample size, complete records, wide variety of income levels

baseline <- alldata %>% filter(cc=='NG' & num=="5" & subversion==1)

