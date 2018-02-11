setwd('D://Documents and Settings/mcooper/Google Drive/DHS Processed/Peru 2009/')

library(foreign)
library(dplyr)

####################################
#Extract childrens data from PR file
######################################
pr <- read.dta('PEPR5IFL.DTA') %>%
  filter(!is.na(hc1)) %>%
  select(interview_month=hv006,
         interview_year=hv007,
         interview_cmc=hv008,
         age=hc1,
         height=hc3,
         weight=hc2,
         how_measured=hc15,
         haz_dhs=hc5,
         whz_dhs=hc11,
         waz_dhs=hc8,
         haz_who=hc70,
         whz_who=hc72,
         waz_who=hc71,
         sex=hc27,
         relationship_hhhead=hv101,
         birth_order=hc64,
         interview_cmc=hv008,
         mother_alive=hv111,
         mother_line=hv112,
         father_alive=hv113,
         father_line=hv114,
         hhsize=hv009,
         urban_rural=hv025,
         wealth_index=hv270,
         wealth_factor=hv271,
         drinkwatersource=hv201,
         otherwatersource=hv202,
         watersource_dist=hv204,
         head_sex=hv219,
         head_age=hv220,
         sampweight=hv005,
         toilet=hv205,
         child_line_num=hc0,
         hhid,
         clusterid=hv001)

########################
#Check Every Variable
########################
table(pr$interview_month)

table(pr$interview_year)

table(pr$interview_cmc)

table(pr$age)

pr$height[pr$height == 9999] <- NA
table(pr$height)

pr$weight[pr$weight == 9999] <- NA
table(pr$weight)

table(pr$how_measured)

pr$haz_dhs[pr$haz_dhs %in% c(9998, 9999)] <- NA
table(pr$haz_dhs)

pr$whz_dhs[pr$whz_dhs %in% c(9998, 9999)] <- NA
table(pr$whz_dhs)

pr$waz_dhs[pr$waz_dhs %in% c(9998, 9999)] <- NA
table(pr$waz_dhs)

pr$haz_who[pr$haz_who %in% c(9996, 9997, 9998, 9999)] <- NA
table(pr$haz_who)

pr$whz_who[pr$whz_who %in% c(9996, 9997, 9998, 9999)] <- NA
table(pr$whz_who)

pr$waz_who[pr$waz_who %in% c(9996, 9997, 9998, 9999)] <- NA
table(pr$waz_who)

table(pr$sex)

table(pr$relationship_hhhead)

pr$birth_order[pr$birth_order == 99] <- NA
table(pr$birth_order)

pr$mother_alive[pr$mother_alive == 'DK'] <- NA
table(pr$mother_alive)

pr$father_alive[pr$father_alive == 'DK'] <- NA
table(pr$father_alive)

table(pr$hhsize)

table(pr$urban_rural)

table(pr$wealth_index)

table(pr$wealth_factor)

pr$drinkwatersource <- recode(pr$drinkwatersource, 
                              `PIPED WATER`="Pipe",
                              `Piped into dwelling`="Pipe",
                              `Piped outside dwelling but within buikding`="Pipe",
                              `Public tap/standpipe`="Pipe",
                              `TUBE WELL WATER`="Tube Well",
                              `Well inside dwelling`="Tube Well",
                              `Public well`="Tube Well",
                              `SURFACE WATER`="Surface Water",
                              `Spring`="Surface Water",
                              `Unprotected spring`="Surface Water",
                              `River/dam/lake/ponds/stream/canal/irirgation channel`="Surface Water",
                              `Rainwater`="Rainwater",
                              `Tanker truck`="Purchased",
                              `Bottled water`="Purchased",
                              `Other`="Other")
table(pr$drinkwatersource)

pr$otherwatersource <- recode(pr$otherwatersource, 
                              `PIPED WATER`="Pipe",
                              `Piped into dwelling`="Pipe",
                              `Piped outside dwelling but within buikding`="Pipe",
                              `Public tap/standpipe`="Pipe",
                              `TUBE WELL WATER`="Tube Well",
                              `Well inside dwelling`="Tube Well",
                              `Public well`="Tube Well",
                              `SURFACE WATER`="Surface Water",
                              `Spring`="Surface Water",
                              `Unprotected spring`="Surface Water",
                              `River/dam/lake/ponds/stream/canal/irirgation channel`="Surface Water",
                              `Rainwater`="Rainwater",
                              `Tanker truck`="Purchased",
                              `Bottled water`="Purchased",
                              `Other`="Other")
table(pr$otherwatersource)

pr$watersource_dist[pr$watersource_dist == 996] <- 0
pr$watersource_dist[pr$watersource_dist == 999] <- NA
table(pr$watersource_dist)

table(pr$head_sex)

table(pr$head_age)

table(pr$sampweight)

#RETURN TO THIS TOO
pr$toilet <- recode(pr$toilet,
                    `FLUSH TOILET`="Flush Toilet",
                    `Inside dwelling`="Flush Toilet",
                    `Outside dwelling`="Flush Toilet",
                    `PIT TOILET LATRINE`="Pit Latrine",
                    `Ventilated latrine`="Pit Latrine",
                    `Septic well`="Pit Latrine",
                    `Latrine (ciego o negro)`="Pit Latrine",
                    `Latrine over river/lake`="Pit Latrine",
                    `NO FACILITY`="No Toilet",
                    `River, canal`="No Toilet",
                    `No service`="No Toilet",
                    `OTHER`="Other")
table(pr$toilet)


####################################
#Extract childrens data from KR file
######################################
kr <- read.dta('PEKR5IFL.DTA') %>%
  filter(!is.na(hw1)) %>%
  select(krage=hw1,
         breast_duration=m4,
         birth_weight=m19,
         istwin=b0,
         preceeding_interval=b11,
         suceeding_interval=b12,
         mother_smokes=v463z,
         fever=h22,
         birthmonth=b1,
         birthyear=b2,
         birthday_cmc=b3,
         parasite_drugs=h43,
         diarrhea=h11,
         child_line_num=b16,
         clusterid=v001,
         hhnum=v002)

########################
#Check Every Variable
########################
kr$breast_duration[kr$breast_duration %in% c(97, 98, 99)] <- NA
kr$breastfeeding <- kr$breast_duration == 95

kr$breast_duration[which(kr$breast_duration == 95)] <- kr$krage[which(kr$breast_duration == 95)]
kr$breast_duration[which(kr$breast_duration == 94)] <- 0

table(kr$breastfeeding)

table(kr$breast_duration)

kr$birth_weight[kr$birth_weight %in% c(9996, 9997, 9998, 9999)] <- NA
table(kr$birth_weight)

kr$istwin <- kr$istwin != "Single birth"
table(kr$istwin)

table(kr$preceeding_interval)

table(kr$suceeding_interval)

kr$mother_smokes <- kr$mother_smokes == "No"
table(kr$mother_smokes)

kr$fever[kr$fever == 'DK'] <- NA
table(kr$fever)

table(kr$birthmonth)

table(kr$birthyear)

table(kr$birthday_cmc)

kr$parasite_drugs[kr$parasite_drugs == "Don't know"] <- NA
table(kr$parasite_drugs)

kr$diarrhea[kr$diarrhea=="Don't know"] <- NA
kr$diarrhea <- recode(kr$diarrhea,
                      `No`="No",
                      `Yes, last 24 hours`="24hours",
                      `Yes, last two weeks`="2weeks")
table(kr$diarrhea)

############################################################
#Extract parents data from PR file & Dependency ratio data
#############################################################
prp <- read.dta('PEPR5IFL.DTA') %>%
  select(hhid, hv108, hv105) %>%
  group_by(hhid) %>%
  mutate(linenumber=row_number())

prp$hv108[prp$hv108 %in% c(97, 98, 99)] <- NA
table(prp$hv108)

table(prp$hv105)

prpm <- prp %>%
  select(hhid, mother_line=linenumber, mother_years_ed=hv108, mothers_age=hv105)

prpf <- prp %>%
  select(hhid, father_line=linenumber, father_years_ed=hv108, fathers_age=hv105)

calcdr <- function(ages){
  workers <- sum(ages > 14 & ages < 66, na.rm=T)
  dependents <- sum(ages < 15 | ages > 65, na.rm=T)
  dependents/workers
}

dr <- prp %>%
  group_by(hhid) %>%
  summarize(workers = sum(hv105 > 14 & hv105 < 66, na.rm=T),
            dependents = sum(hv105 < 15 | hv105 > 65, na.rm=T))


##########################################
#Merge together all DHS survey variables
##########################################
kr$hhid <- paste0(substr(10000 + kr$clusterid, 2, 6), substr(1000 + kr$hhnum, 2, 5))

sum(!kr$hhid %in% pr$hhid)

initialrows <- dim(pr)[1]
pr <- merge(pr, kr, all.x=T, all.y=F)
sum(!prnew$age == prnew$krage, na.rm = T)

pr <- merge(pr, prpm, all.x=T, all.y=F)
pr <- merge(pr, prpf, all.x=T, all.y=F)
pr <- merge(pr, dr, all.x=T, all.y=F)

dim(pr)[1]==initialrows

#######################################
#Extract Latitude and Longitude
#######################################
sp <- read.dbf('PEGE5IFL.dbf') %>%
  select(clusterid = DHSCLUST,
         latitude = LATNUM,
         longitude = LONGNUM)

sum(!pr$clusterid %in% sp$clusterid)

pr <- merge(pr, sp, all.x=T, all.y=F)

dim(pr)[1]==initialrows

pr$code <- paste0('PE-5-3-', pr$clusterid)

write.csv(pr, '../Output/Peru2009.csv', row.names=F)
