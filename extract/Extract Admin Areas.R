library(rgdal)
library(raster)
library(dplyr)

setwd('G://My Drive/DHS Spatial Covars/Admin Areas')

options(stringsAsFactors = F)

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code) %>%
  unique

sp1 <- readOGR(dsn='.', layer='gadm36_1')
sp2 <- readOGR(dsn='.', layer='gadm36_2')

points <- SpatialPointsDataFrame(coords=data[ , c('longitude', 'latitude')], data=data, 
                                 proj4string = CRS(proj4string(sp1)))

data$Adm1 <- as.character(over(points, sp1)$GID_1)
data$Adm2 <- as.character(over(points, sp2)$GID_2)

#Fill in missing points with nearest neighbors
#Adm1
Adm1df <- data[!is.na(data$Adm1), ]
Adm1dfNA <- data[is.na(data$Adm1), ]

ix <- nn2(Adm1df[, c('longitude', 'latitude')], Adm1dfNA[, c('longitude', 'latitude')], k=1)[['nn.idx']]

Adm1dfNA$nn <- Adm1df[ix, 'code']

Adm1dfNA <- merge(Adm1dfNA, select(data, code, nnAdm1=Adm1), by.x='nn', by.y='code', all.x=T, all.y=F) %>%
  select(-nn)

data <- merge(data, Adm1dfNA, all.x=T, all.y=F)

#Adm2
Adm2df <- data[!is.na(data$Adm2), ]
Adm2dfNA <- data[is.na(data$Adm2), ]

ix <- nn2(Adm2df[, c('longitude', 'latitude')], Adm2dfNA[, c('longitude', 'latitude')], k=1)[['nn.idx']]

Adm2dfNA$nn <- Adm2df[ix, 'code']

Adm2dfNA <- merge(Adm2dfNA, select(data, code, nnAdm2=Adm2), by.x='nn', by.y='code', all.x=T, all.y=F) %>%
  select(-nn)

df <- merge(data, Adm2dfNA, all.x=T, all.y=F)

#Where 2 is empty because of distance, use 2nn
df$Adm2[is.na(df$Adm1)] <- df$nnAdm2[is.na(df$Adm1)]

#Where 1 is empty because of distance, use 1nn
df$Adm1[is.na(df$Adm1)] <- df$nnAdm1[is.na(df$Adm1)]

#Where 2 is empty because it does not exist, use 1
df$Adm2[is.na(df$Adm2)] <- df$Adm1[is.na(df$Adm2)]

#Where 2 is missing because DNE and 1 was missing beause of distance, use 1nn
ix <- which(substr(df$nnAdm1, 1, 3) != substr(df$nnAdm2, 1, 3))
df$Adm2[ix] <- df$Adm2[ix]

write.csv(df, 'G://My Drive/DHS Processed/Admin_Areas.csv', row.names=F)
