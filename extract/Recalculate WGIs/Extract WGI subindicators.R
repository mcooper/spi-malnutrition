library(dplyr)
library(readxl)
library(tidyr)

setwd('G://My Drive/DHS Spatial Covars/World Governance Indicators/Raw data for subindicators')

splittoyears <- function(str){
  len <- nchar(str)
  res <- NULL
  
  while (len > 0){
    res <- c(res, substr(str, len-1, len))
    
    len <- len-2
  }
  
  res
}

read_combined <- function(source){
  read_xlsx(paste0(source, '.xlsx'), 'MERGEPublic', na=c('..', '#N/A')) %>%
    select(X__1, Code, contains("PV")) %>%
    gather(key, value, -X__1, -Code) %>%
    filter(!is.na(X__1)) %>%
    mutate(year=as.numeric(substr(key, 4, 5)),
           year=ifelse(year > 50, year + 1900, year + 2000)) %>%
    select(Country=X__1, Code, value, year) %>%
    mutate(Source=source)
}

read_by_year <- function(source, sheets){
  all <- data.frame()
  for (yr in c(seq(1990, 2018))){
    for (sheet in sheets){
      if (substr(yr, 3, 4) %in% splittoyears(sheet)){
        tmp <- read_xlsx(paste0(source, '.xlsx'), sheet, trim_ws=FALSE)
        skip <- min(which(!is.na(tmp$X__1))) - 1
        
        dat <- read_xlsx(paste0(source, '.xlsx'), sheet, na=c('..', '#N/A'), skip = skip, trim_ws=FALSE) 
        
        dat$na_ct <- rowSums(is.na(dat))
        
        dat <- dat %>%
          filter(na_ct < (ncol(.)-1)) %>%
          select(Code=X__1, Country=X__2, value=contains("PV")) %>%
          mutate(year=yr, Source=source)
        
        all <- bind_rows(all, dat)
      }
    }
  }
  all
}

read_wb <- function(source){
  sheets <- excel_sheets(paste0(source, '.xlsx'))
  
  if("MERGEPublic" %in% sheets){
    all <- read_combined(source)
  } else{
    all <- read_by_year(source, sheets)
  }
  return(all)
}

final <- Map(read_wb, list("EIU", "GCS", "HUM", "IJT", "IPD", "WCY", "WJP", "WMO", "PRS")) %>%
  Reduce(f=bind_rows)

write.csv(final, 'G://My Drive/DHS Spatial Covars/World Governance Indicators/Combined_Subindicators_PV.csv', row.names=F)
