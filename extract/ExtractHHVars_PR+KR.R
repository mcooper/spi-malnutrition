setwd('D://Documents and Settings/mcooper/Google Drive/DHS Data/')

library(foreign)
library(dplyr)

usefiles <- read.csv('../../GitHub/spi-malnutrition/scope/UseFiles.csv', stringsAsFactors = F) %>%
  filter(useable)

surveyvars <- read.csv('../../GitHub/spi-malnutrition/extract/headervars.csv',
                       stringsAsFactors = FALSE)

source('../../GitHub/spi-malnutrition/extract/Utils.R')

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

  all <- bind_rows(all, data)
}

###################
#Ad hoc cleaning
###################
table(substr(all$code[is.na(all$mother_years_ed) & !is.na(all$mother_level_ed)], 1, 6))
table(substr(all$code[is.na(all$father_years_ed) & !is.na(all$father_level_ed)], 1, 6))


table(substr(all$code[which((all$parents_years_ed != all$mother_years_ed))], 1, 6))
table(substr(all$code[which((all$parents_years_ed != all$mother_years_ed) & (all$parents_years_ed != all$father_years_ed))], 1, 6))
table(substr(all$code[is.na(all$father_years_ed) & !is.na(all$father_level_ed)], 1, 6))




#See if there are any without education years but with levels, if not drop the level columns
    