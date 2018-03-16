setwd('D://Documents and Settings/mcooper/GitHub/spi-malnutrition/')

library(foreign)
library(dplyr)

usefiles <- read.csv('scope/UseFiles.csv', stringsAsFactors = F) %>%
  filter(useable)

surveyvars <- read.csv('extract/headervars.csv',
                       stringsAsFactors = FALSE)

source('extract/Utils.R')

setwd('G://My Drive/DHS Data/')

all <- data.frame()
for (i in 1:nrow(usefiles)){
  cat(i, ':', usefiles$cc[i], usefiles$num[i], usefiles$subversion[i], nrow(all), '\n')
  
  #For surveys with only PR Files
  if (usefiles$PRgood[i] & is.na(usefiles$KRgood[i])){
    prfile <- usefiles$PR[i]
    
    res <- process_pr(prfile, surveyvars)
    data <- res[[1]]
    data$filesource <- "KR Only"
    data$fromKR <- FALSE
  }
  
  #For surveys with only KR Files
  if (usefiles$KRgood[i] & is.na(usefiles$PRgood[i])){
    prfile <- usefiles$KR[i]
    
    data <- process_kr(prfile, surveyvars, surveyvars$kr)
    data$filesource <- "PR Only"
    data$fromKR <- TRUE
  }
  
  #For surveys with KR and PR files
  if (!is.na(usefiles$KRgood[i]) & !is.na(usefiles$PRgood[i])){
    prfile <- usefiles$PR[i]
    krfile <- usefiles$KR[i]
    
    res <- process_pr(prfile, surveyvars)
    prdata <- res[[1]]
    
    krdata <- process_kr(krfile, surveyvars, c(res[[2]], 'v002', 'v001', 'b16'))
    krdata$fromKR <- TRUE
    
    initialsize <- nrow(prdata)
    #Have to find a way to merge these, use clusterid and hhno
    data <- merge(prdata, krdata, all.x=T, all.y=F)
    data$filesource <- "PR and KR"
    data$fromKR[is.na(data$fromKR)] <- FALSE
    
    if (initialsize != nrow(data)){
      cat("Mismatches in KR and PR.  Initial size:", initialsize, " now:", nrow(data), '\n')
    }
  }
  
  #For surveys with WI files
  if (usefiles$WI[i] != ''){
    wi <- read.dta(usefiles$WI[i])
    names(wi) <- c('hhid', 'wealth_factor', 'wealth_index')
    
    wi$wealth_factor <- as.character(wi$wealth_factor)
    wi$wealth_index <- as.character(wi$wealth_index)
    
    if (sum(!data$hhid %in% wi$hhid) > 0){
      #If hhid doesnt match, try it with just hhno and clusterid
      padws <- function(vect, n=0){
        len <- max(nchar(vect))
        padded <- paste0("     ", vect)
        substr(padded, nchar(padded) - (len - 1), nchar(padded))
      }
      
      data$hhid <- paste0(padws(data$clusterid), padws(data$householdno))
      idlen <- nchar(data$hhid)[1]
      data$hhid <- paste0(paste0(rep(" ", 12-idlen), collapse=''), data$hhid)
      
    }
    
    if (sum(!data$hhid %in% wi$hhid) > 0){
      #Check again for matching, if not, cat error
      cat('Mismatches in wealth data for ', usefiles$cc[i], usefiles$num[i], usefiles$subversion[i], '\n') 
    }
    
    data <- merge(data, wi, all.x=T, all.y=F)
    
  }
  
  #Now get spatial data
  spheadervars <- c('DHSCLUST', 'LATNUM', 'LONGNUM')
  
  out <- read.dbf(gsub('.shp', '.dbf', usefiles$GE[i]), as.is=TRUE)
  if (!all(spheadervars %in% names(out))){
    cat(usefiles$GE[i], 'is missing necessary column names\n')
  }else{
    out <- out[ , spheadervars]
    out$num <- substr(usefiles$GE[i], 5, 5)
    out$cc <- toupper(substr(usefiles$GE[i], 1, 2))
    out$subversion <- ifelse(toupper(substr(usefiles$GE[i], 6, 6)) %in% as.character(seq(0, 9)), 1,
                             ifelse(toupper(substr(usefiles$GE[i], 6, 6)) %in% LETTERS[1:8], 2, 
                                    ifelse(toupper(substr(usefiles$GE[i], 6, 6)) %in% LETTERS[9:17], 3, 
                                           ifelse(toupper(substr(usefiles$GE[i], 6, 6)) %in% LETTERS[18:26], 4, 99))))
    out$code <- paste(out$cc, out$num, out$subversion, out$DHSCLUST, sep='-')
    
    out <- out %>%
      select(latitude=LATNUM, longitude=LONGNUM, code=code)
  }
  
  initialsize <- nrow(data)
  data <- merge(data, out)
  if (initialsize != nrow(data)){
    cat("Mismatches in Spatial and Household data.  Initial size:", initialsize, " now:", nrow(data), '\n')
  }
  
  all <- bind_rows(all, data)
}

###################
#Ad hoc cleaning
###################
setwd('D://Documents and Settings/mcooper/GitHub/spi-malnutrition/')

#age
all$age <- as.numeric(all$age)

#birth_order
all$birth_order <- as.numeric(all$birth_order)
all$birth_order[all$birth_order == 99] <- NA

#birth_weight
all$birth_weight <- as.numeric(all$birth_weight)
all$birth_weight[all$birth_weight > 9000] <- NA

#birthmonth
all$birthmonth <- recode(all$birthmonth, `ashad`="6",`aswin`="9",`baisakh`="4",`bhadra`="8",`chaitra`="3",
                         `falgun`="2",`jestha`="5",`kartik`="10",`magh`="1",`mangsir`="11",`poush`="12",`srawan`="7")
all$birthmonth <- as.numeric(all$birthmonth)

#birthyear
all$birthyear <- as.numeric(all$birthyear)

all$birthyear[which(all$birthyear > 80 & all$birthyear < 1000)] <- all$birthyear[which(all$birthyear > 80 & all$birthyear < 1000)] + 1900
all$birthyear[which(all$birthyear == 0)] <- 2000

all$birthyear[which(all$birthyear > 2020 & all$birthmonth %in% seq(1, 9))] <- all$birthyear[which(all$birthyear > 2020 & all$birthmonth %in% seq(1, 9))] - 57
all$birthyear[which(all$birthyear > 2020 & all$birthmonth %in% seq(10, 12))] <- all$birthyear[which(all$birthyear > 2020 & all$birthmonth %in% seq(10, 12))] - 56

#breast_duration, breastfeeding and ever_breastfed
all$breast_duration <- recode(all$breast_duration, `ever breastfed, not currently breastfeeding`='93',
                              `never breastfed`='94', `still breastfeeding`='95')
all$breast_duration <- as.numeric(all$breast_duration)
all$breast_duration[all$breast_duration %in% c(99, 98, 97, 96) | (all$breast_duration > 60 & all$breast_duration < 90)] <- NA

all$ever_breastfed <- as.integer(all$breast_duration != 0 & all$breast_duration != 94)
all$breastfeeding <- as.integer(all$breast_duration != 0 & all$breast_duration != 94 & all$breast_duration != 93)

all$breast_duration[which(all$breast_duration == 95)] <- all$age[which(all$breast_duration == 95)]
all$breast_duration[all$breast_duration == 94] <- 0
all$breast_duration[all$breast_duration == 93] <- NA

#dependents and workers
all$dependents <- as.numeric(all$dependents)
all$dependents[all$dependents == 0] <- NA #How can there be children under 5 but no dependents?

all$workers <- as.numeric(all$workers)

#diarrhea
all$diarrhea <- recode(all$diarrhea, `0`="No", `1`="2weeks", `2`="24hours", `8`="NA", `9`="NA", `don't know`="NA", `no`="No",
                       `yes, last 2-14 days`="2weeks", `yes, last 24 hours`="24hours", `yes, last two weeks`="2weeks")
all$diarrhea[all$diarrhea=="NA"] <- NA
all$diarrhea <- as.integer(all$diarrhea != "No")

#drinkwatersource
#based on categores in Table 6 of supplement to Diarrhea & Forests Paper
watersources <- read.csv('extract/all_water_sources.csv', header=F, col.names=c('drinkwatersource', 'newcode'))

all <- merge(all, watersources, all.x=T, all.y=F)

all$drinkwatersource <- all$newcode
all$newcode <- NULL

#father_alive
all$father_alive[all$father_alive %in% c('8', '9', 'dk', 'DK', "don't know", "Don't know")] <- NA
all$father_alive <- as.integer(all$father_alive == '1' | all$father_alive == 'yes' | all$father_alive == 'Yes')

#father_years_ed
all$father_years_ed <- as.numeric(all$father_years_ed)
all$father_years_ed[all$father_years_ed > 97] <- NA

#fathers_age
all$father_age <- as.numeric(all$father_age)
all$father_age[all$father_age > 97 | all$father_age < 10] <- NA

#fever
all$fever[all$fever %in% c('8', '9', 'dk', "don't know")] <- NA
all$fever <- as.integer(all$fever == '1' | all$fever == 'yes')

#haz_dhs
all$haz_dhs <- as.numeric(all$haz_dhs)
all$haz_dhs[all$haz_dhs > 9000] <- NA

#haz_who
all$haz_who <- as.numeric(all$haz_who)
all$haz_who[all$haz_who > 9000] <- NA

#waz_dhs
all$waz_dhs <- as.numeric(all$waz_dhs)
all$waz_dhs[all$waz_dhs > 9000] <- NA

#waz_who
all$waz_who <- as.numeric(all$waz_who)
all$waz_who[all$waz_who > 9000] <- NA

#whz_dhs
all$whz_dhs <- as.numeric(all$whz_dhs)
all$whz_dhs[all$whz_dhs > 9000] <- NA

#whz_who
all$whz_who <- as.numeric(all$whz_who)
all$whz_who[all$whz_who > 9000] <- NA

#head_age
all$head_age <- as.numeric(all$head_age)
all$head_age[all$head_age > 97 | all$head_age < 10] <- NA

#head_sex
all$head_sex <- recode(all$head_sex, `1`="Male", `2`="Female", `female`="Female", `male`="Male")

#hhsize
all$hhsize <- as.numeric(all$hhsize)

#how_measured
all$how_measured <- recode(all$how_measured, `0`="NA", `1`="Lying", `2`="Standing", `3`="NA", `9`="NA", `lying`="Lying",
                           `not measured`="NA", `Not measured`="NA", `standing`="Standing")
all$how_measured[all$how_measured=="NA"] <- NA

#interview_month  
all$interview_month <- recode(all$interview_month, `ashad`="6",`aswin`="9",`baisakh`="4",`bhadra`="8",`chaitra`="3",
                              `falgun`="2",`jestha`="5",`kartik`="10",`magh`="1",`mangsir`="11",`poush`="12",`srawan`="7",
                              `March`="3", `May`="5", `november`="11", `october`="10", `september`="9", `july`="7", `July`="7",
                              `june`="6", `June`="6", `February`="2", `April`="4", `august`="8")
all$interview_month <- as.numeric(all$interview_month)  

#interview_year
all$interview_year <- 1900 + floor((as.numeric(all$interview_cmc) - 1)/12)
all$interview_year[which(all$interview_year > 2020 & all$interview_month %in% seq(1, 9))] <- all$interview_year[which(all$interview_year > 2020 & all$interview_month %in% seq(1, 9))] - 57
all$interview_year[which(all$interview_year > 2020 & all$interview_month %in% seq(10, 12))] <- all$interview_year[which(all$interview_year > 2020 & all$interview_month %in% seq(10, 12))] - 56

#istwin
all$istwin <- as.integer(all$istwin != "single birth")

#mother_alive
all$mother_alive[all$mother_alive %in% c('8', '9', 'dk', 'DK', "don't know", "Don't know")] <- NA
all$mother_alive <- as.integer(all$mother_alive == '1' | all$mother_alive == 'yes' | all$mother_alive == 'Yes')

#mother_smokes
all$mother_smokes[all$mother_smokes == "9"] <- NA
all$mother_smokes <- as.integer(all$mother_smokes == "0" | all$mother_smokes == "no")

#mother_years_ed
all$mother_years_ed[is.na(all$mother_years_ed) | all$mother_years_ed == "98" | all$mother_years_ed == "99"] <- all$parents_years_ed[is.na(all$mother_years_ed) | all$mother_years_ed == "98" | all$mother_years_ed == "99"]
all$mother_years_ed <- as.numeric(all$mother_years_ed)
all$mother_years_ed[all$mother_years_ed > 97] <- NA

#mother_age
all$mother_age <- as.numeric(all$mother_age)
all$mother_age[all$mother_age > 97 | all$mother_age < 10] <- NA

#otherwatersource
#based on categores in Table 6 of supplement to Diarrhea & Forests Paper
watersources <- read.csv('extract/all_water_sources.csv', header=F, col.names=c('otherwatersource', 'newcode'))

all <- merge(all, watersources, all.x=T, all.y=F)

all$otherwatersource <- all$newcode
all$newcode <- NULL

#parasite_drugs
all$parasite_drugs[all$parasite_drugs %in% c('8', '9', 'dk', 'DK', "don't know")] <- NA
all$parasite_drugs <- as.integer(all$parasite_drugs == '1' | all$parasite_drugs == 'yes')

#preceeding_interval
all$preceeding_interval <- as.numeric(all$preceeding_interval)
all$preceeding_interval[all$preceeding_interval > 500] <- NA

#relationship_hhhead
family_relations <- read.csv('extract/family_relations.csv', header=FALSE, col.names = c("relationship_hhhead", "newcode"))

all <- merge(all, family_relations, all.x=T, all.y=F)
all$relationship_hhhead <- all$newcode
all$newcode <- NULL

all$relationship_hhhead[all$relationship_hhhead %in% c("Head", "Parent", "Parent-in-law", "Wife or Husband", "Co-Spouse")] <- NA

#sex
all$sex <- recode(all$sex, `1`="Male", `2`="Female", `female`="Female", `male`="Male")
all$sex[all$sex=='9'] <- NA

#suceeding_interval
all$suceeding_interval <- as.numeric(all$suceeding_interval)
all$suceeding_interval[all$suceeding_interval > 500] <- NA

#toilet
#based on categores in Table 6 of supplement to Diarrhea & Forests Paper
toilets <- read.csv('extract/toiletcodes.csv', header=F, col.names=c('toilet', 'newcode'))

all <- merge(all, toilets, all.x=T, all.y=F)
all$toilet <- all$newcode
all$newcode <- NULL

#urban_rural
all$urban_rural <- recode(all$urban_rural, `rural`="Rural", `urban`='Urban')

#watersource_dist
all$watersource_dist <- as.numeric(all$watersource_dist)
all$watersource_dist[all$watersource_dist==996] <- 0
all$watersource_dist[all$watersource_dist %in% c(998, 999)] <- NA

#wealth_factor
all$wealth_factor <- as.numeric(all$wealth_factor)

#wealth_index
all$wealth_index <- recode(all$wealth_index, `fourth quintile`="Richer", `highest quintile`="Richest", `second quintile`="Poorer", 
                           `middle quintile`="Middle", `lowest quintile`="Poorest", `richest`="Richest", `middle`="Middle", 
                           `richer`="Richer", `highest`="Richest", `poorest`="Poorest", `poorer`="Poorer", `fourth`="Richer", 
                           `lowest`="Poorest", `second`="Poorer")

#is_visitor
all$is_visitor[all$is_visitor == 9] <- NA
all$is_visitor <- as.integer(all$is_visitor == 'visitor' | all$is_visitor == '2')

#years_in_location
all$years_in_location <- as.numeric(all$years_in_location)

all$years_in_location[all$years_in_location %in% c(97, 98, 99)] <- NA
all$years_in_location[all$years_in_location == 96] <- 0
all$years_in_location[all$years_in_location == 95] <- 50

#weight
all$weight <- as.numeric(all$weight)
all$weight[all$weight < 10 | all$weight > 320] <- NA
all$weight <- all$weight/10 #convert to kg

#height
all$height <- as.numeric(all$height)
all$height[all$height < 250 | all$height > 1300] <- NA
all$height <- all$height/10 #Convert to cm


###Calculate birthmonth and thousandday month/year
library(lubridate)

makedate <- function(year, month){
  ymd(paste(year, month, '15', sep='-'))
}

all$calc_birthmonth <- month(makedate(all$interview_year, all$interview_month) - months(all$age))
all$calc_birthyear <- year(makedate(all$interview_year, all$interview_month) - months(all$age))

all$thousandday_month <- month(makedate(all$calc_birthyear, all$calc_birthmonth) + months(24))
all$thousandday_year <- year(makedate(all$calc_birthyear, all$calc_birthmonth) + months(24))

under2_ind <- makedate(all$thousandday_year, all$thousandday_month) > makedate(all$interview_year, all$interview_month)
all$thousandday_month[under2_ind] <- NA
all$thousandday_year[under2_ind] <- NA

#Make country just CC for future merging
all$country <- substr(all$country, 1, 2)

#Remove any with latitude == 0 | longitude == 0, there are also some weird super small coordinates
all <- all %>%
  filter(!(longitude < 1 & longitude > -1 & latitude < 1 & latitude > -1))

#Calculate my own haz, whz and waz
# Based on scripts and WHO reference tables from the Vital Signs project

setwd('G://My Drive/DHS Processed/')

lenanthro <- read.table("lenanthro.txt", header=T)
weianthro <- read.table("weianthro.txt", header=T)
wfhanthro <- read.table("wfhanthro.txt", header=T)
wflanthro <- read.table("wflanthro.txt", header=T)

all$haz <- mapply(FUN=getHAZ, age=all$age, height=all$height, sex=all$sex, how_measured=all$how_measured, MoreArgs = list(lenanthro))
all$waz <- mapply(FUN=getWAZ, age=all$age, weight=all$weight, sex=all$sex, MoreArgs = list(weianthro))
all$whz <- mapply(FUN=getWHZ, age=all$age, weight=all$weight, height=all$height, sex=all$sex, how_measured=all$how_measured, MoreArgs = list(wflanthro, wfhanthro))

#Write coords with dates

write.csv(all[ , c('latitude', 'longitude', 'code', 'interview_year', 'interview_month',
                   'calc_birthyear', 'calc_birthmonth', 'thousandday_month', 'thousandday_year')] %>% unique,
          'sp_export.csv', row.names=F)

#Write all data
write.csv(all, 'hhvars.csv', row.names=F)









