library(AzureSMR)
library(dplyr)

az <- read.csv('~/.azure/.AzureSMR', stringsAsFactors = F)

sc <- createAzureContext(tenantID = az$tenantid,
                         clientID = az$appid,
                         authKey = az$authkey)

az_read_csv <- function(csv, container="dhsprocessed"){
  read.csv(text=azureGetBlob(sc, 
                             storageAccount=az$storage_acct, 
                             container=container,
                             blob=csv,
                             type="text",
                             storageKey = az$storageKey))
}

az_write_blob <- function(blob, blobname, container='stan-models'){
  file <- paste0(blobname, '.Rdata')
  save(blob, file=file)
  system(paste0("az storage blob upload --container-name ", container, " --file ", file, " --name ", file,
                " --account-name ", az$storage_acct, " --account-key ", az$storageKey))
  system(paste0("rm ", file))
}

hh <- az_read_csv('hhvars.csv')
spi <- az_read_csv('Coords&Precip.csv')
cov <- az_read_csv('SpatialCovars.csv')

################################
#Combine and clear workspace
################################
hh <- hh %>%
  filter(country %in% c('KE', "TZ", "UG"))

spi <- spi %>%
  select(-precip_10yr_mean, -tmin_10yr_mean, -tmax_10yr_mean)

all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, spi, cov))

all <- all %>%
  filter(is_visitor == 0 & years_in_location >= 2 | is.na(is_visitor) | is.na(years_in_location))

#Relevel factors
all <- all %>%
  mutate(toilet=relevel(toilet, ref="No Facility"),
         relationship_hhhead=relevel(relationship_hhhead, ref="Immediate Family"),
         otherwatersource=relevel(otherwatersource, ref="Surface Water"),
         drinkwatersource=relevel(drinkwatersource, ref="Surface Water"),
         wealth_index=relevel(wealth_index, ref="Poorest"))

all$related_hhhead <- all$relationship_hhhead == "Not Related"

sel <- all %>%
  select(code, surveycode, country, interview_year, toilet, related_hhhead, age, birth_order, haz_dhs, head_age, head_sex, hhsize,
         sex, wealth_index, diarrhea, fever, breast_duration, urban_rural, mother_haz, latitude, longitude,
         parents_years_ed, market_dist)

#################################
#Impute
###############################

library(mice)

t <- mice(sel %>% select(-toilet, -wealth_index, -country) %>% filter(surveycode=='TZ-4-1'), m=5)
