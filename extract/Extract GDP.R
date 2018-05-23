setwd('G://My Drive/DHS Spatial Covars/GDP')

library(dplyr)
library(tidyr)

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code, interview_year) %>%
  mutate(country = substr(code, 1, 2)) %>%
  unique

#This data is from the UN: http://data.un.org/Data.aspx?d=SNAAMA&f=grID%3A101%3BcurrID%3AUSD%3BpcFlag%3A1
#According to this doc it is in 2010 USD https://unstats.un.org/unsd/snaama/methodology.pdf
#And it is real GDP (not PPP)

#Initially tried data from the world bank: https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
#but it was missing Niger from early years

#dat <- read.csv('API_NY.GDP.PCAP.KD_DS2_en_csv_v2.csv', skip = 4, stringsAsFactors = F)

gdpdat <- read.csv('UNdata_Export_20180310_200921296.csv')

#Mapping built from the table of DHS codes at https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
map <- read.csv('../Global Codes and Shapefile/DHS-WB-UN Country Codes.csv')

gdp <- merge(map, gdpdat, all.x=F, all.y=F) %>%
  dplyr::select(country, interview_year=Year, gdp=Value)

gdp <- merge(data, gdp)

write.csv(gdp, '../../DHS Processed/country_gdp.csv', row.names=F)

########################################################
#Prediction Raster
#####################################################

#These IMF Projections http://www.imf.org/external/pubs/ft/weo/2018/01/weodata/weorept.aspx?pr.x=58&pr.y=12&sy=2018&ey=2023&scsm=1&ssd=1&sort=country&ds=.&br=1&c=512%2C946%2C914%2C137%2C612%2C546%2C614%2C962%2C311%2C674%2C213%2C676%2C911%2C548%2C193%2C556%2C122%2C678%2C912%2C181%2C313%2C867%2C419%2C682%2C513%2C684%2C316%2C273%2C913%2C868%2C124%2C921%2C339%2C948%2C638%2C943%2C514%2C686%2C218%2C688%2C963%2C518%2C616%2C728%2C223%2C836%2C516%2C558%2C918%2C138%2C748%2C196%2C618%2C278%2C624%2C692%2C522%2C694%2C622%2C142%2C156%2C449%2C626%2C564%2C628%2C565%2C228%2C283%2C924%2C853%2C233%2C288%2C632%2C293%2C636%2C566%2C634%2C964%2C238%2C182%2C662%2C359%2C960%2C453%2C423%2C968%2C935%2C922%2C128%2C714%2C611%2C862%2C321%2C135%2C243%2C716%2C248%2C456%2C469%2C722%2C253%2C942%2C642%2C718%2C643%2C724%2C939%2C576%2C644%2C936%2C819%2C961%2C172%2C813%2C132%2C726%2C646%2C199%2C648%2C733%2C915%2C184%2C134%2C524%2C652%2C361%2C174%2C362%2C328%2C364%2C258%2C732%2C656%2C366%2C654%2C734%2C336%2C144%2C263%2C146%2C268%2C463%2C532%2C528%2C944%2C923%2C176%2C738%2C534%2C578%2C536%2C537%2C429%2C742%2C433%2C866%2C178%2C369%2C436%2C744%2C136%2C186%2C343%2C925%2C158%2C869%2C439%2C746%2C916%2C926%2C664%2C466%2C826%2C112%2C542%2C111%2C967%2C298%2C443%2C927%2C917%2C846%2C544%2C299%2C941%2C582%2C446%2C474%2C666%2C754%2C668%2C698%2C672&s=NGDPDPC&grp=0&a=#notes

gdp20 <- read.table('weoreptc.aspx', sep='\t', header=T, quote='', nrows=193, na.strings = 'n/a', stringsAsFactors = F) %>%
  dplyr::select(Country, X2020) %>%
  mutate(X2020 = as.numeric(gsub(',', '', X2020)))

#Missing data for Egypt, Pakistan, Cuba, and Syria.  Use UN 2016 estimates

gdp20$X2020[gdp20$Country == 'Egypt'] <- gdpdat$Value[gdpdat$Country.or.Area == 'Egypt' & gdpdat$Year == 2016]
gdp20$X2020[gdp20$Country == 'Pakistan'] <- gdpdat$Value[gdpdat$Country.or.Area == 'Pakistan' & gdpdat$Year == 2016]
gdp20$X2020[gdp20$Country == 'Somalia'] <- gdpdat$Value[gdpdat$Country.or.Area == 'Somalia' & gdpdat$Year == 2016]
gdp20$X2020[gdp20$Country == 'Syria'] <- gdpdat$Value[gdpdat$Country.or.Area == 'Syrian Arab Republic' & gdpdat$Year == 2016]

gdp20 <- bind_rows(gdp20,
                   data.frame(Country = 'Cuba',
                              X2020 = gdpdat$Value[gdpdat$Country.or.Area == 'Cuba' & gdpdat$Year == 2016]))

library(rgdal)
library(raster)
library(countrycode)

gdp20$SOVEREIGNT_ISO_3C <- countrycode(gdp20$Country, origin='country.name', destination = 'iso3c')

sp <- readOGR('../Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp$SOVEREIGNT_ISO_3C <- countrycode(sp$SOVEREIGNT, origin='country.name', destination = "iso3c")

sp <- sp::merge(sp, na.omit(gdp20))

sp@data$X2020[sp@data$SOVEREIGNT == "Federated States of Micronesia"] <- 3387.28800
sp@data$X2020[sp@data$SOVEREIGNT == "Western Sahara"] <- sp@data$X2020[sp@data$ADMIN == "Mauritania"]
sp@data$X2020[sp@data$SOVEREIGNT == "North Korea"] <- gdpdat$Value[gdpdat$Country.or.Area == "Democratic People's Republic of Korea" & gdpdat$Year == 2016]
sp@data$X2020[sp@data$SOVEREIGNT == "Kashmir"] <- sp@data$X2020[sp@data$ADMIN == "India"]
sp@data$X2020[sp@data$ADMIN == "Kosovo"] <- 4899.30300
sp@data$X2020[sp@data$ADMIN == "Liechtenstein"] <- sp@data$X2020[sp@data$ADMIN == "Switzerland"]
sp@data$X2020[sp@data$ADMIN == "Andorra"] <- sp@data$X2020[sp@data$ADMIN == "France"]

names(sp@data)[names(sp@data) == 'X2020'] <- 'gdp2020'

#Need to decide on scale and make a template raster
r <- raster('../Irrigation/gmia_v5_aei_pct.asc')

rasterize(sp, r, field="gdp2020", fun='mean', na.rm=TRUE, filename='../Final Rasters/gdp2020.tif', driver='GTiff')





