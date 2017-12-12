setwd('D://Documents and Settings/mcooper/Google Drive/DHS Data/')

library(foreign)
library(dplyr)

usefiles <- read.csv('../Dissertation/UseFiles.csv', stringsAsFactors = F)

fs <- usefiles$PR

allpr <- data.frame(vars='hv000')
for (f in fs){
  out <- read.dta(f)
  vars <- names(out)
  l <- rep(TRUE, length(vars))
  df <- data.frame(vars=vars, temp=l)
  names(df)[2] <- f
  allpr <- merge(allpr, df, by='vars', all=T)
  print(f)
  print(dim(allpr))
}

allpr[is.na(allpr)] <- FALSE
allpr$PR_COUNT <- rowSums(allpr[fs])
allpr <- allpr %>% arrange(desc(PR_COUNT))

#Scope Childrens Recode Files
fs <- usefiles$PR

allkr <- data.frame(vars='v000')
for (f in fs){
  out <- read.dta(f)
  vars <- names(out)
  l <- rep(TRUE, length(vars))
  df <- data.frame(vars=vars, temp=l)
  names(df)[2] <- f
  allkr <- merge(allkr, df, by='vars', all=T)
  print(f)
  print(dim(allkr))
}

allkr[is.na(allkr)] <- FALSE

allkr$KR_COUNT <- rowSums(allkr[fs])

allkr <- allkr %>% arrange(desc(KR_COUNT))

allkr$KR_COUNT[allkr$vars=='hw8']
#[1] 236
allpr$PR_COUNT[allpr$vars=='hc8']
#[1] 164
#Looks like there are many more surveys with child health records in the KR files