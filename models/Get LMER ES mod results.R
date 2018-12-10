library(lme4)
library(broom)
library(raster)
library(dplyr)

load('G://My Drive/Dissertation/ES Mod Results/mod.mdweek.nocovars')
load('G://My Drive/Dissertation/ES Mod Results/mod.mdweek.allcovars')
load('G://My Drive/Dissertation/ES Mod Results/mod.all.nocovars')
load('G://My Drive/Dissertation/ES Mod Results/mod.all.allcovars')

fao_sel <- read.csv('G:///My Drive/DHS Processed/FAO_sel.csv')

mod <- mod.all.allcovars

###############################################
# Get Overall Drought Vulnerability (minus ES)
###############################################

s <- coef(mod)$farm_system_id
s <- s[ , grepl('speiDry', names(s))]

speidf <- data.frame(coef(mod)$farm_system) %>%
  mutate(farm_system_id = as.numeric(row.names(.))) %>%
  dplyr::select(farm_system_id, speiDry)
speidf <- bind_rows(data.frame(farm_system_id=seq(0, 1662)[!seq(0, 1662) %in% speidf$farm_system_id],
                                speiDry=NA),
                    speidf)
fao_spei <- reclassify(fao, speidf)

transformations <- list(mean_annual_precip=function(x){x/1000},
                        imports_percap=function(x){x/1000},
                        builtup=function(x){x*100})

for (i in 5:ncol(s)){
  rast_name <- gsub('speiDry:', '', names(s)[i])
  
  tmp_rast <- raster(paste0('G://My Drive/DHS Spatial Covars/Final Rasters/2020/', rast_name, '.tif'))
  
  if (rast_name %in% names(transformations)){
    tmp_rast <- transformations[[rast_name]](tmp_rast)
  }
  
  suppressWarnings(fao_spei <- fao_spei + tmp_rast*unique(s[ , i]))
}


#######################################
#Get How ES Affect Drought Vulnerability
#######################################

fao <- raster('G:/My Drive/DHS Spatial Covars/Farm Systems/farm_system_id.tif')
trees <- raster('G:/My Drive/DHS Spatial Covars/ESA Land Cover/forest_resample.tif')
grass <- raster('G:/My Drive/DHS Spatial Covars/ESA Land Cover/grass_resample.tif')
water <- raster('G:/My Drive/DHS Spatial Covars/ESA Land Cover/water_resample.tif')

treesdf <- data.frame(coef(mod)$farm_system) %>%
  mutate(farm_system_id = as.numeric(row.names(.))) %>%
  dplyr::select(farm_system_id, nat_trees.speiDry) %>%
  filter(farm_system_id %in% fao_sel$farm_system_id[fao_sel$keep])
treesdf <- bind_rows(data.frame(farm_system_id=seq(0, 1662)[!seq(0, 1662) %in% treesdf$farm_system_id],
                                nat_trees.speiDry=NA),
                     treesdf)
fao_trees <- reclassify(fao, treesdf)
trees_pred <- trees*fao_trees

grassdf <- data.frame(coef(mod)$farm_system) %>%
  mutate(farm_system_id = as.numeric(row.names(.))) %>%
  dplyr::select(farm_system_id, nat_grass.speiDry) %>%
  filter(farm_system_id %in% fao_sel$farm_system_id[fao_sel$keep])
grassdf <- bind_rows(data.frame(farm_system_id=seq(0, 1662)[!seq(0, 1662) %in% grassdf$farm_system_id],
                                nat_grass.speiDry=NA),
                     grassdf)
fao_grass <- reclassify(fao, grassdf)
grass_pred <- grass*fao_grass

waterdf <- data.frame(coef(mod)$farm_system) %>%
  mutate(farm_system_id = as.numeric(row.names(.))) %>%
  dplyr::select(farm_system_id, nat_water.speiDry) %>%
  filter(farm_system_id %in% fao_sel$farm_system_id[fao_sel$keep])
waterdf <- bind_rows(data.frame(farm_system_id=seq(0, 1662)[!seq(0, 1662) %in% waterdf$farm_system_id],
                                nat_water.speiDry=NA),
                     waterdf)
fao_water <- reclassify(fao, waterdf)
water_pred <- water*fao_water

trees_pred[trees_pred < 0] <- 0
grass_pred[grass_pred < 0] <- 0
water_pred[water_pred < 0] <- 0

all_resources <- trees_pred + grass_pred + water_pred

drought_vulnerability <- all_resources + fao_spei

drought_vulnerability0 <- drought_vulnerability
drought_vulnerability0[drought_vulnerability0 > 0] <- 0

all_resources0 <- all_resources
all_resources0[all_resources0 < 0] <- 0

possible_improvement <- drought_vulnerability0*-1

resource_contribution <- min(stack(all_resources0, possible_improvement))

md <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2020/market_dist.tif')

remote_resources <- resource_contribution
remote_resources[(md < 180) & !is.na(remote_resources)] <- 0

md[!is.na(md)] <- 0

new <- max(stack(md, remote_resources), na.rm=T)

writeRaster(new, 'G://My Drive/Dissertation/Final Maps/AGU_map.tif', format='GTiff', overwrite=T)
