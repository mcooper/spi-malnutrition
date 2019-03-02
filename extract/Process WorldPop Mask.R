library(raster)

setwd('G:/My Drive/DHS Spatial Covars/WorldPop')

lac <- raster("LAC_PPP_2020_adj_v2.tif")
asia <- raster("Asia_PPP_2020_adj_v2.tif")
afr <- raster("AFR_PPP_2020_adj_v2.tif")

ref <- raster('../Final Rasters/2020/Population.tif')


res_lac <- resample(lac, ref, method='ngb')
res_asia <- resample(asia, ref, method='ngb')
res_afr <- resample(afr, ref, method='ngb')

res_lac[is.na(res_lac)] <- 10000
res_asia[is.na(res_asia)] <- 10000
res_afr[is.na(res_afr)] <- 10000

res <- res_lac + res_asia + res_afr

res <- res - 20000

writeRaster(res, '../Final Rasters/2020/population_mask.tif', format='GTiff')
