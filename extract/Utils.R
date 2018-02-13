process_pr <- function(prfile, surveyvars){
  suppressWarnings(pr <- read.dta(prfile))
  
  trykr <- c()
  prvars <- c('hhid')
  
  #Collect all easy vars
  for (i in 1:nrow(surveyvars)){
    var <- surveyvars$pr[i]
    
    #see if variable is missing or all NAs.  If so try it in the KR file
    if (!var %in% names(pr)){
      trykr <- c(trykr, surveyvars$kr[i])
      next
    }
    if(sum(is.na(pr[ , var])) == nrow(pr)){
      trykr <- c(trykr, surveyvars$kr[i])
      next
    }
    
    newcode <- surveyvars$newcode[i]
    prvars <- c(prvars, newcode)
    
    pr[ , newcode] <-  as.character(pr[ , var])
    
  }
  
  prp <- pr
  
  pr <- pr %>%
    filter(!is.na(hc1)) %>%
    .[ , prvars]
  
  #Collect parent and household data from PR file, then merge back
  if (!all(c('hhid', 'hv108', 'hv106', 'hv107', 'hv105') %in% names(prp))){
    cat("PR file is missing age or education or hhid in", prfile)
  }else{
    prp <- prp %>%
      select(hhid, hvidx, hv108, hv105, hv106, hv107)
    
    initialrownums <- nrow(pr)
    
    if (sum(is.na(pr$mother_line)) < nrow(pr) & !is.null(pr$mother_line)){
      prpm <- prp %>%
        select(hhid, mother_line=hvidx, mother_level_ed=hv106, mother_highest_year=hv107, mother_years_ed=hv108, mother_age=hv105) %>%
        mutate(mother_line = as.character(mother_line), mother_level_ed=as.character(mother_level_ed),
               mother_highest_year = as.character(mother_highest_year), mother_years_ed = as.character(mother_years_ed),
               mother_age=as.character(mother_age))
  
      pr <- merge(pr, prpm, all.x=T, all.y=F)
      
      if (nrow(pr) != initialrownums){
        stop("Bad merge on mothers line in ", prfile)
      }
    }
  
    if (sum(is.na(pr$father_line)) < nrow(pr) & !is.null(pr$father_line)){
      prpf <- prp %>%
        select(hhid, father_line=hvidx, father_level_ed=hv106, father_highest_year=hv107, father_years_ed=hv108, father_age=hv105) %>%
        mutate(father_line = as.character(father_line), father_level_ed=as.character(father_level_ed),
               father_highest_year = as.character(father_highest_year), father_years_ed = as.character(father_years_ed),
               father_age=as.character(father_age))
      
      pr <- merge(pr, prpf, all.x=T, all.y=F)
      
      if (nrow(pr) != initialrownums){
        stop("Bad merge on fathers line in ", prfile)
      }
    }
    
    if (sum(is.na(prp$hv105)) < nrow(prp) & !is.null(prp$hv105)){
      dr <- prp %>%
        group_by(hhid) %>%
        summarize(workers = sum(hv105 > 14 & hv105 < 66, na.rm=T),
                  dependents = sum(hv105 < 15 | hv105 > 65, na.rm=T))
      
      pr <- merge(pr, dr, all.x=T, all.y=F)
      
      if (nrow(pr) != initialrownums){
        stop("Bad merge on dependents in ", prfile)
      }
    }
  }
  
  num <- substr(prfile, 5, 5)
  cc <- toupper(substr(prfile, 1, 2))
  subversion <- ifelse(toupper(substr(prfile, 6, 6)) %in% as.character(seq(0, 9)), 1,
                           ifelse(toupper(substr(prfile, 6, 6)) %in% LETTERS[1:8], 2, 
                                  ifelse(toupper(substr(prfile, 6, 6)) %in% LETTERS[9:17], 3, 
                                         ifelse(toupper(substr(prfile, 6, 6)) %in% LETTERS[18:26], 4, 99))))
  
  pr$code <- paste(cc, num, subversion, pr$clusterid, sep='-')
  
  list(pr, trykr)
}


process_kr <- function(krfile, surveyvars, vars){
  suppressWarnings(kr <- read.dta(krfile) %>%
    filter(!is.na(hw1)))
  
  krvars <- c('caseid')
  
  #Collect all easy vars
  for (var in vars){
    
    #see if variable is missing or all NAs.  If so try it in the KR file
    if (!var %in% names(kr)){
      next
    }
    if(sum(is.na(kr[ , var])) == nrow(kr)){
      next
    }
    
    newcode <- surveyvars$newcode[surveyvars$kr == var]
    krvars <- c(krvars, newcode)
    
    kr[ , newcode] <-  as.character(kr[ , var])
    
  }
  
  kr <- kr[ , krvars]
  
  kr$hhid <- substr(kr$caseid, 1, 12)
  
  num <- substr(prfile, 5, 5)
  cc <- toupper(substr(prfile, 1, 2))
  subversion <- ifelse(toupper(substr(prfile, 6, 6)) %in% as.character(seq(0, 9)), 1,
                       ifelse(toupper(substr(prfile, 6, 6)) %in% LETTERS[1:8], 2, 
                              ifelse(toupper(substr(prfile, 6, 6)) %in% LETTERS[9:17], 3, 
                                     ifelse(toupper(substr(prfile, 6, 6)) %in% LETTERS[18:26], 4, 99))))
  
  kr$code <- paste(cc, num, subversion, kr$clusterid, sep='-')
  
  kr
}
