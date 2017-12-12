setwd('D://Documents and Settings/mcooper/Google Drive/DHS Data/')

library(foreign)
library(dplyr)
library(sp)

usefiles <- read.csv('../Dissertation/UseFiles.csv', stringsAsFactors = F)

fs <- usefiles$GE

spheadervars <- c('DHSCC', 'DHSCLUST', 'URBAN_RURA', 'LATNUM', 'LONGNUM')

spdata <- data.frame()
for (f in fs){
  out <- read.dbf(gsub('.shp', '.dbf', f), as.is=TRUE)
  if (!all(spheadervars %in% names(out))){
    cat(f, '\n')
    cat(spheadervars[!spheadervars %in% names(out)], '\n')
  } else{
    out <- out[ , spheadervars]
    out$survey <- gsub('.shp|.SHP', '', f)
    spdata <- bind_rows(spdata, out)
  }
}


fs <- usefiles$PR

headervars <- c('hv000', 'hv001', 'hv006', 'hv007', 'hv008')

svdata <- data.frame()
for (f in fs){
  out <- read.dta(f)
  if (!all(headervars %in% names(out))){
    cat(f, '\n')
    cat(headervars[!headervars %in% names(out)], '\n')
  } else{
    out <- out[ , headervars]
    out$hv001 <- as.integer(out$hv001)
    out$hv000 <- as.character(out$hv000)
    out$hv006 <- as.integer(out$hv006)
    out$hv007 <- as.integer(as.character(out$hv007))
    out$hv008 <- as.integer(out$hv008)
    out$survey <- gsub('.DTA|.dta', '', f)
    svdata <- bind_rows(svdata, unique(out))
  }
  print(f)
}

svdata$CMCyear <- 1900 + floor((svdata$hv008 - 1)/12)
svdata$CMCmonth <- svdata$hv008 - 12*(svdata$CMCyear - 1900)

#Look for errors
svdata$survey[svdata$CMCyear > 2017 | svdata$CMCyear < 1985] %>% unique

svdata$CMCyear[which(svdata$survey=='NPPR41FL')] <- svdata$CMCyear[which(svdata$survey=='NPPR41FL')] - 57
svdata$CMCyear[which(svdata$survey=='NPPR51FL')] <- svdata$CMCyear[which(svdata$survey=='NPPR51FL')] - 57
svdata$CMCyear[which(svdata$survey=='NPPR60FL')] <- svdata$CMCyear[which(svdata$survey=='NPPR60FL')] - 57

#Looking at months, there are three survyes that don't match on v006 and CMCmonth
#it seems that v006 makes the most sense!

svdata <- svdata %>% dplyr::select(month=hv006, year=CMCyear, DHSCLUST=hv001, survey) %>%
  unique

#Parse filenames for matching
svdata$num <- substr(svdata$survey, 5, 5)
svdata$cc <- toupper(substr(svdata$survey, 1, 2))

spdata$num <- substr(spdata$survey, 5, 5)
spdata$cc <- toupper(substr(spdata$survey, 1, 2))

#turns out sometimes a dhs will happen more than once in one country:
#https://dhsprogram.com/data/File-Types-and-Names.cfm
svdata$subversion <- ifelse(toupper(substr(svdata$survey, 6, 6)) %in% as.character(seq(0, 9)), 1,
                         ifelse(toupper(substr(svdata$survey, 6, 6)) %in% LETTERS[1:8], 2, 
                                ifelse(toupper(substr(svdata$survey, 6, 6)) %in% LETTERS[9:17], 3, 
                                       ifelse(toupper(substr(svdata$survey, 6, 6)) %in% LETTERS[18:26], 4, NA))))
                     
spdata$subversion <- ifelse(toupper(substr(spdata$survey, 6, 6)) %in% as.character(seq(0, 9)), 1,
                         ifelse(toupper(substr(spdata$survey, 6, 6)) %in% LETTERS[1:8], 2, 
                                ifelse(toupper(substr(spdata$survey, 6, 6)) %in% LETTERS[9:17], 3, 
                                       ifelse(toupper(substr(spdata$survey, 6, 6)) %in% LETTERS[18:26], 4, NA))))


#Analyze codes for validating the match
svdata$code <- paste(svdata$cc, svdata$num, svdata$subversion, svdata$DHSCLUST, sep='-')
spdata$code <- paste(spdata$cc, spdata$num, spdata$subversion, spdata$DHSCLUST, sep='-')

svdata$rcode <- paste(svdata$cc, svdata$num, svdata$subversion, sep='-')
spdata$rcode <- paste(spdata$cc, spdata$num, spdata$subversion, sep='-')


allmerge <- merge(svdata, spdata, by=c('num', 'cc', 'DHSCLUST', 'subversion'), all.x=F, all.y=F)


write.csv(allmerge, '../Dissertation/sp_export.csv', row.names = F)
