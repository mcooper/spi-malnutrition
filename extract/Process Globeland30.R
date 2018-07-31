library(dplyr)

setwd('G://My Drive/DHS Processed')

glob <- read.csv('globeland30.csv')
forest <- read.csv('forest_cover25000.csv')

glob$Total_2000 <- rowSums(glob[ , grepl('_2000', names(glob))])
glob$Total_2010 <- rowSums(glob[ , grepl('_2010', names(glob))])

types <- c('Agriculture', 'Artificial', 'Bare', 'Forest', 'Grassland', 
           'Ice', 'Ocean', 'Shrubland', 'Water', 'Wetland')

for (i in types){
  glob[ , paste0(i, '_2000_pct')] <- glob[ , paste0(i, '_2000')]/glob$Total_2000
  glob[ , paste0(i, '_2010_pct')] <- glob[ , paste0(i, '_2010')]/glob$Total_2010
}

glob %>%
  mutate(Natural_Grass10 = Grassland_2010_pct + Shrubland_2010_pct + Bare_2010_pct,
         Natural_Trees10 = Forest_2010_pct,
         Natural_Water10 = Water_2010_pct + Wetland_2010_pct + Ocean_2010_pct + Ice_2010_pct,
         Natural10 = Natural_Grass10 + Natural_Trees10 + Natural_Water10,
         Human10 = Agriculture_2010_pct + Artificial_2010_pct)

glob$Natural_Grass00
glob$Natural_Trees00
glob$Nautral_Water00
glob$Natural00
glob$Human00