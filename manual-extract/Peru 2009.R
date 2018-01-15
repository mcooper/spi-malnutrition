setwd('D://Documents and Settings/mcooper/Google Drive/DHS Processed/Peru 2009/')

library(foreign)
library(dplyr)

kr <- read.dta('PEKR5IFL.DTA')

#Look at table to determine how to calculate years of ed
kr[ , c('v106', 'v107')] %>% table
kr$parent_years_ed <- ifelse(kr$v106=='No education', 0,
                             ifelse(kr$v106=='Primary', kr$v107,
                                    ifelse(kr$v106=='Secondary', 6 + kr$v107,
                                           ifelse(kr$v106=='Higher', 11 + kr$v107, NA))))

kr$breastfeeding <- kr$m4==95
kr$m4[kr$m4==95] <- kr$age[kr$m4==95]
kr$m4[kr$m4==94] <- 0


krsel <- kr %>% select(
  parent_years_ed,
  breastfeeding,
  twin=b0,
  sex=b4,
  birth_order=bord,
  caseid=caseid,
  diarrhea=h11,
  fever=h22,
  parasite_drugs=h43,
  age=hw1,
  whz_dhs=hw11,
  weight=hw2,
  height=hw3,
  haz_dhs=hw5,
  haz_who=hw70,
  waz_who=hw71,
  whz_who=hw72,
  waz_dhs=hw8,
  birth_weight=m19,
  breast_duration=m4,
  hhid=v002,
  rlinenum=v003,
  sampweight=v005,
  month=v006,
  year=v007,
  interview_cmc=v008,
  birthmonth=v009,
  birthyear=v010,
  birthday_cmc=v011,
  urban_rural=v025,
  watersource=v113,
  watersource_dist=v115,
  sanitation_facility=v116,
  hhsize=v136,
  head_sex=v151,
  head_age=v152,
  wealth_index=v190,
  wealth_factor=v191,
  father_line=v034,
  mother_line=v003)



