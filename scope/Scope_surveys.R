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

#Only Use Surveys With Geospatial Data
df <- filter(df, GE != '')

#Don't Use Surveys With No Wealth Data ie, < DHS-IV and No WI file
df <- filter(df, WI != '' | num > 4)

write.csv(df, '../Dissertation/UseFiles.csv', row.names=F)
