setwd('D://Documents and Settings/mcooper/Google Drive/DHS Data/')

library(foreign)
library(dplyr)

fs <- list.files(pattern='^..(WI|wi|KR|kr|HR|hr|GE|ge|PR|pr|IR|ir).....(DTA|dta|SHP|shp)$')

df <- data.frame()

for (f in fs){
  num <- substr(f, 5, 5)
  cc <- toupper(substr(f, 1, 2))
  subversion <- ifelse(toupper(substr(f, 6, 6)) %in% as.character(seq(0, 9)), 1,
                          ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[1:8], 2, 
                                 ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[9:17], 3, 
                                        ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[18:26], 4))))
  if (nrow(df[df$num==num & df$cc==cc & df$subversion==subversion, ]) > 0){
    df[df$num==num & df$cc==cc & df$subversion==subversion, toupper(substr(f, 3, 4))] <- f
  }else{
    temp <- data.frame(num=num, cc=cc, subversion=subversion, WI='', KR='', HR='', GE='', PR='', IR='', stringsAsFactors = F)
    df <- bind_rows(df, temp)
    df[df$num==num & df$cc==cc & df$subversion==subversion, toupper(substr(f, 3, 4))] <- f
  }
  print(f)
}

for (f in df$KR[df$KR != '']){
  file <- read.dta(f)
  if (class(file$hw3) == 'integer'){
    df$KRheight[df$KR==f] <- TRUE
  }
  if (class(file$hw2) == 'integer'){
    df$KRweight[df$KR==f] <- TRUE
  }
  if (class(file$hw1) == 'integer'){
    df$KRage[df$KR==f] <- TRUE
  }
  if (class(file$hw1) == 'integer'){
    df$KRage[df$KR==f] <- TRUE
  }
  if (class(file$hw5) == 'integer'){
    df$KRhaz[df$KR==f] <- TRUE
  }
  if (class(file$hw11) == 'integer'){
    df$KRwhz[df$KR==f] <- TRUE
  }
  print(f)
}

for (f in df$PR[df$PR != '']){
  file <- read.dta(f)
  if (class(file$hc3) == 'integer'){
    df$PRheight[df$PR==f] <- TRUE
  }
  if (class(file$hc2) == 'integer'){
    df$PRweight[df$PR==f] <- TRUE
  }
  if (class(file$hc1) == 'integer'){
    df$PRage[df$PR==f] <- TRUE
  }
  if (class(file$hc1) == 'integer'){
    df$PRage[df$PR==f] <- TRUE
  }
  if (class(file$hc5) == 'integer'){
    df$PRhaz[df$PR==f] <- TRUE
  }
  if (class(file$hc11) == 'integer'){
    df$PRwhz[df$PR==f] <- TRUE
  }
  print(f)
}
  
  

#Only Use Surveys With Geospatial Data
df$nocoords <- df$GE == ''

#Don't Use Surveys With No Wealth Data ie, < DHS-IV and No WI file
df$nowealth <- df$WI == '' & df$num < 4

write.csv(df, '../../GitHub/spi-malnutrition/scope/UseFiles.csv', row.names=F)
