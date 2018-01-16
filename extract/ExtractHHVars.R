setwd('D://Documents and Settings/mcooper/Google Drive/DHS Data/')

library(foreign)
library(dplyr)

usefiles <- read.csv('../../GitHub/spi-malnutrition/scope/UseFiles.csv', stringsAsFactors = F) %>%
  filter(PR != '' & !nocoords & !nowealth & PRheight & PRage)

fs <- usefiles$PR

convert <- function(vect, type){
  if (type == 'character'){
    newvect <- as.character(vect)
  }
  else if (type == 'integer'){
    newvect <- as.integer(vect)
  }
  else{
    stop('You didn\'t specify the type correctly')
  }
  newvect
}

headervars <- read.csv('../../GitHub/spi-malnutrition/extract/headervars&types.csv', stringsAsFactors = F)

df <- data.frame()

for (f in fs){
  dat <- read.dta(f)
  sel <- dat[ , headervars$PR[headervars$PR %in% names(dat)]]
  for (n in names(sel)){
    sel[ , n] <- convert(sel[ , n], headervars$Type[headervars$PR==n])
  }
  sel$survey <- f
  
  #Keep only children
  sel <- sel[!is.na(sel$hc5), ]
  df <- bind_rows(df, sel)
  print(f)
}

df$num <- substr(df$survey, 5, 5)
df$cc <- toupper(substr(df$survey, 1, 2))
df$subversion <- ifelse(toupper(substr(df$survey, 6, 6)) %in% as.character(seq(0, 9)), 1,
                            ifelse(toupper(substr(df$survey, 6, 6)) %in% LETTERS[1:8], 2, 
                                   ifelse(toupper(substr(df$survey, 6, 6)) %in% LETTERS[9:17], 3, 
                                          ifelse(toupper(substr(df$survey, 6, 6)) %in% LETTERS[18:26], 4, 99))))

#Get Wealth Data
fs <- usefiles$WI[usefiles$WI!='']

wdf <- data.frame()

for (f in fs){
  dat <- read.dta(f)
  dat$survey <- f
  wdf <- bind_rows(wdf, dat)
  print(f)
}

wdf$num <- substr(wdf$survey, 5, 5)
wdf$cc <- toupper(substr(wdf$survey, 1, 2))
wdf$subversion <- ifelse(toupper(substr(wdf$survey, 6, 6)) %in% as.character(seq(0, 9)), 1,
                            ifelse(toupper(substr(wdf$survey, 6, 6)) %in% LETTERS[1:8], 2, 
                                   ifelse(toupper(substr(wdf$survey, 6, 6)) %in% LETTERS[9:17], 3, 
                                          ifelse(toupper(substr(wdf$survey, 6, 6)) %in% LETTERS[18:26], 4, 99))))


#Merge Wealth and PR
addPad <- function(str, n){
  str <- as.character(str)
  if (nchar(str) < n){
    str <- paste0(c(rep(' ', n - nchar(str)), str), collapse='')
  }
  str
}

df$whhid <- sapply(X=df$hhid, FUN=addPad, n=12)

wdf$survey <- NULL
all <- merge(df, wdf, all.x=T, all.y=F)

#Combine wealth indicators

all$wlthind5 <- ifelse(all$wlthind5=="lowest quintile", 1, 
                       ifelse(all$wlthind5=="second quintile", 2, 
                              ifelse(all$wlthind5=="middle quintile", 3,
                                     ifelse(all$wlthind5=="fourth quintile", 4,
                                            ifelse(all$wlthind5=="highest quintile", 5, NA)))))
all$wealth <- rowSums(all[ , c('hv270', 'wlthind5')], na.rm=T)

#Get year and month from CMC
all$year <- 1900 + floor((all$hv008 - 1)/12)
all$month <- all$hv006

all$year[which(all$survey=='NPPR41FL.DTA')] <- all$year[which(all$survey=='NPPR41FL.DTA')] - 57
all$year[which(all$survey=='NPPR51FL.dta')] <- all$year[which(all$survey=='NPPR51FL.dta')] - 57
all$year[which(all$survey=='NPPR60FL.DTA')] <- all$year[which(all$survey=='NPPR60FL.DTA')] - 57

##Calculate nutrition indicators
setwd('../../GitHub/spi-malnutrition')

# Restore reference data sets
lenanthro <- read.table("calculate-zscores/WHO Anthro reference tables/lenanthro.txt", header=T)

all$height <- all$hc3/10

all <- all[which(all$height < 200), ]

rounde <- function(x,digits=0) {
  expo<-10^digits
  return(ifelse(abs(x*expo) - floor(abs(x*expo)) < 0.5, sign(x*expo) * floor(abs(x*expo)), sign(x*expo) * (floor(abs(x*expo)) + 1))/expo)
}

all$age.days <- rounde(all$hc1*30.4375)

all$sex <- ifelse(all$hc27 %in% c(2, 'female', 'Female'), 2, 1)

getZscore <- function(age.days, sex, height, lenanthro){
  if (is.na(age.days) | is.na(sex) | is.na(height)){
    return(NA)
  }
  
  sel <- lenanthro[lenanthro$sex == sex & lenanthro$age == age.days, ]
  
  h <- height
  m <- sel$m
  s <- sel$s
  l <- sel$l
  
  #their method
  (((h/m)^l)-1)/(s*l)
}

all$haz <- mapply(FUN=getZscore, age.days=all$age.days, height=all$height, sex=all$sex, MoreArgs=list(lenanthro=lenanthro))

##Do a little bit of clean up
all$hv025 <- ifelse(all$hv025=='rural', 'Rural',
                    ifelse(all$hv025=='urban', 'Urban', all$hv025))
all$hv219 <- ifelse(all$hv219 %in% c('1', 'male'), 'Male',
                    ifelse(all$hv219 %in% c('2', 'female'), 'Female', all$hv219))


all <- all %>%
  filter(hc15 != 0 & hc15 != 9,
         hc64 != 99,
         haz < 6 & haz > -8)
         

#Look for errors
all$survey[all$year > 2017 | all$year < 1985] %>% unique

#No errors!

write.csv(all, 'data/hhvars.csv', row.names=F)







