library(ggplot2)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(dplyr)

options(stringsAsFactors=F)

setwd('G://My Drive/DHS Spatial Covars/FEWS Validation/Shapefiles/')

for (fs in list.files(pattern='LAC.*shp$')){
  
  ipc <- readOGR('.', gsub('.shp', '', fs))
  
  ipc$CS <- paste0('', ipc$CS)
  
  ipc$ipcid <- rownames(ipc@data)

  f <- fortify(ipc, region='ipcid')
  
  f$fid <- f$id
  
  f2 <- merge(f, ipc@data, by.x='fid', by.y='ipcid', all.x=T) %>%
    arrange(order)
  
  ggplot(f2, aes(long, lat, group=group, fill=CS)) + 
    geom_polygon(color='grey80') + 
    scale_fill_manual(values = c(`0`='#bebebe', `1`='#dcf0dc', 
                                 `2`='#fae61e', `3`='#e67800',
                                 `4`='#c80000', `5`='#640000', 
                                 `88`="#bebebe", `99`="#bebebe")) + 
    xlim(-92.5, -71.5) + ylim(13.5, 20.5) +
    ggtitle(paste0("IPC in ", substr(fs, 8, 9), ', ', substr(fs, 4, 7)))
  
  ggsave(paste0('G:/My Drive/Dissertation/Final Maps/FEWS annual pngs/',
                gsub('.shp', '.png', fs)),
         width=15, height=15)
}

#must run from terminal manually
#convert -delay 60 -loop 0 *.png droughtTS.gif'
