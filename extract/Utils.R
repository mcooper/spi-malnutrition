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
  
  dep_cp <- pr
  mother_cp <- pr
  father_cp <- pr
  pr <- pr %>%
    filter(!is.na(hc1)) %>%
    .[ , prvars]
  
  
  #Collect all parent vars
  mother_vars <- data.frame(newcode=c('hhid', 'mother_line', 'mother_age', 'mother_years_ed', 'mother_height', 'mother_haz'),
                            pr=c('hhid', 'hvidx', 'hv105', 'hv108', 'ha3', 'ha5'),
                            stringsAsFactors = F)
  father_vars <- data.frame(newcode=c('hhid', 'father_line', 'father_age', 'father_years_ed', 'father_height', 'father_haz'),
                            pr=c('hhid', 'hvidx', 'hv105', 'hv108', 'hb3', 'hb5'),
                            stringsAsFactors = F)
  
  #mother
  mvars <- NULL
  for (i in 1:nrow(mother_vars)){
    var <- mother_vars$pr[i]
    
    if (!var %in% names(mother_cp)){
      next  
    }
    if (sum(is.na(mother_cp[ , var])) == nrow(mother_cp)){
      next
    }
    newcode <- mother_vars$newcode[i]
    mvars <- c(mvars, newcode)
    mother_cp[ , newcode] <- as.character(mother_cp[ , var])
  }
  mother_cp <- mother_cp[ , mvars]
  
  if ('mother_line' %in% names(pr)){
    pr <- merge(pr, mother_cp, all.x=T, all.y=F)
  }
  
  #Father
  fvars <- NULL
  for (i in 1:nrow(father_vars)){
    var <- father_vars$pr[i]
    
    if (!var %in% names(father_cp)){
      next
    }
    if (sum(is.na(father_cp[ , var])) == nrow(father_cp)){
      next
    }
    newcode <- father_vars$newcode[i]
    fvars <- c(fvars, newcode)
    father_cp[ , newcode] <- as.character(father_cp[ , var])
  }
  father_cp <- father_cp[ , fvars]
  
  if ('father_line' %in% names(pr)){
    pr <- merge(pr, father_cp, all.x=T, all.y=F)
  }
    
  #Get dependants
  if (sum(is.na(dep_cp$hv105)) < nrow(dep_cp) & !is.null(dep_cp$hv105)){
    dr <- dep_cp %>%
      group_by(hhid) %>%
      summarize(workers = sum(hv105 > 14 & hv105 < 66, na.rm=T),
                dependents = sum(hv105 < 15 | hv105 > 65, na.rm=T))
    
    pr <- merge(pr, dr, all.x=T, all.y=F)
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

rounde <- function(x,digits=0) {
  expo<-10^digits
  return(ifelse(abs(x*expo) - floor(abs(x*expo)) < 0.5, sign(x*expo) * floor(abs(x*expo)), sign(x*expo) * (floor(abs(x*expo)) + 1))/expo)
}

getHAZ <- function(age, sex, height, how_measured, lenanthro){
  if (is.na(age) | is.na(sex) | is.na(height)){
    return(NA)
  }
  
  age.days <- rounde(age*30.4375)
  
  if (!is.na(how_measured) & how_measured == "Standing" & age.days < 731){
    height <- height + 0.7
  }
  if(!is.na(how_measured) & how_measured == "Lying" & age.days >= 731){
    height <- height - 0.7
  }
  
  sex <- (sex == "Female") + 1
  
  sel <- lenanthro[lenanthro$sex == sex & lenanthro$age == age.days, ]

  m <- sel$m
  s <- sel$s
  l <- sel$l
  
  haz <- (((height/m)^l) - 1)/(s*l)
  
  return(haz)
}

getWAZ <- function(age, sex, weight, weianthro){
  if (is.na(age) | is.na(sex) | is.na(weight)){
    return(NA)
  }
  
  age.days <- rounde(age*30.4375)
  
  sex <- (sex == "Female") + 1
  
  sel <- weianthro[weianthro$sex == sex & weianthro$age == age.days, ]
  
  m <- sel$m
  s <- sel$s
  l <- sel$l
  
  waz <- (((weight/m)^l) - 1)/(s*l)
  
  if(waz > 3) {
    sd3pos <- m*((1 + l*s*3)^(1/l))
    sd23pos <- sd3pos - m*((1 + l*s*2)^(1/l))
    waz <- 3 + ((weight - sd3pos) / sd23pos)
  }
  if(waz < -3) {
    sd3neg <- m*((1 + l*s*(-3))**(1/l))
    sd23neg <- m*((1 + l*s*(-2))**(1/l)) - sd3neg
    waz <- (-3) + ((weight - sd3neg) / sd23neg)
  }
  
  return(waz)
}

getWHZ <- function(age, sex, height, weight, how_measured, wflanthro, wfhanthro){
  if (is.na(age) | is.na(sex) | is.na(height) | is.na(weight)){
    return(NA)
  }
  
  age.days <- rounde(age*30.4375)
  
  if (!is.na(how_measured) & how_measured == "Standing" & age.days < 731){
    height <- height + 0.7
  }
  if(!is.na(how_measured) & how_measured == "Lying" & age.days >= 731){
    height <- height - 0.7
  }
  
  sex <- (sex == "Female") + 1
  
  low.len <- trunc(height * 10)/10
  upp.len <- trunc(height * 10 + 1)/10
  dif.len <- (height - low.len)/0.1
  
  if(age.days < 731){
    
    sel.low <- wflanthro[wflanthro$length == low.len & wflanthro$sex==sex, ]
    sel.upp <- wflanthro[wflanthro$length == upp.len & wflanthro$sex==sex, ]
      
  } else{
      
    sel.low <- wfhanthro[wfhanthro$height == low.len & wfhanthro$sex==sex, ]
    sel.upp <- wfhanthro[wfhanthro$height == upp.len & wfhanthro$sex==sex, ]

  }
  
  if(dif.len > 0){
    
    l <- sel.low$l + dif.len*(sel.upp$l - sel.low$l)
    m <- sel.low$m + dif.len*(sel.upp$m - sel.low$m)
    s <- sel.low$s + dif.len*(sel.upp$s - sel.low$s)
    
  } else{
    
    l <- sel.low$l
    m <- sel.low$m
    s <- sel.low$s
    
  }
  
  if (nrow(sel.low) == 0){
    return(NA)  
  }
  
  whz <- (((weight/m)^l)-1)/(s*l)
  
  if(whz > 3) {
    sd3pos <- m*((1 + l*s*3)^(1/l))
    sd23pos <- sd3pos - m*((1 + l*s*2)^(1/l))
    whz <- 3 + ((weight - sd3pos) / sd23pos)
  }
  if(whz < -3) {
    sd3neg <- m*((1 + l*s*-3)**(1/l))
    sd23neg <- m*((1 + l*s*-2)**(1/l)) - sd3neg
    whz <- -3 + ((weight - sd3neg) / sd23neg)
  }
  
  return(whz)
}
