setwd('D://Documents and Settings/mcooper/Google Drive/DHS Data/')

library(foreign)
library(dplyr)

fs <- list.files(pattern='^..(WI|wi|KR|kr|GE|ge|PR|pr).....(DTA|dta|SHP|shp)$')

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
    temp <- data.frame(num=num, cc=cc, subversion=subversion, WI='', KR='', GE='', PR='', stringsAsFactors = F)
    df <- bind_rows(df, temp)
    df[df$num==num & df$cc==cc & df$subversion==subversion, toupper(substr(f, 3, 4))] <- f
  }
  print(f)
}

for (f in df$KR[df$KR != '']){
  file <- read.dta(f)
  if (sum(is.na(file$hw3)) < nrow(file) & !is.null(file$hw3)){
    df$KRheight[df$KR==f] <- TRUE
  }
  if (sum(is.na(file$hw1)) < nrow(file) & !is.null(file$hw1)){
    df$KRweight[df$KR==f] <- TRUE
  }
  if (sum(is.na(file$hw2)) < nrow(file) & !is.null(file$hw2)){
    df$KRage[df$KR==f] <- TRUE
  }
  print(f)
}

for (f in df$PR[df$PR != '']){
  file <- read.dta(f)
  if (sum(is.na(file$hc3)) < nrow(file) & !is.null(file$hc3)){
    df$PRheight[df$PR==f] <- TRUE
  }
  if (sum(is.na(file$hc3)) < nrow(file) & !is.null(file$hc3)){
    df$PRweight[df$PR==f] <- TRUE
  }
  if (sum(is.na(file$hc3)) < nrow(file) & !is.null(file$hc3)){
    df$PRage[df$PR==f] <- TRUE
  }
  print(f)
}

#A survey is useable IF 1) Had coords & 2) Has anthropometry in KR OR PR
df$useable <- df$GE != '' & ((df$KRheight & df$KRweight & df$KRage) | (df$PRheight & df$PRweight & df$PRage))

df$PRgood <- df$PRheight & df$PRweight & df$PRage
df$KRgood <- df$KRheight & df$KRweight & df$KRage

write.csv(df, '../../GitHub/spi-malnutrition/scope/UseFiles.csv', row.names=F)
