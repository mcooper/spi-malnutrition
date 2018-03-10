setwd('G://My Drive/DHS Processed')

library(dplyr)
library(tidyr)

#This data is from the UN: http://data.un.org/Data.aspx?d=SNAAMA&f=grID%3A101%3BcurrID%3AUSD%3BpcFlag%3A1
#Initially tried data from the world bank: https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
#but it was missing Niger from early years

#dat <- read.csv('API_NY.GDP.PCAP.KD_DS2_en_csv_v2.csv', skip = 4, stringsAsFactors = F)

dat <- read.csv('UNdata_Export_20180310_200921296.csv')

#Mapping built from the table of DHS codes at https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
map <- read.csv('DHS-WB-UN Country Codes.csv')

gdp <- merge(map, dat, all.x=F, all.y=F) %>%
  #select(-`Country.Code`, -`Country.Name`, -`Indicator.Name`, -`Indicator.Code`, -X) %>% #World Bank data columns
  #gather(interview_year, gdp, -country) %>%
  #mutate(interview_year = substr(interview_year, 2, 5))
  
  select(country, interview_year=Year, gdp=Value)

write.csv(gdp, 'country_gdp.csv', row.names=F)
