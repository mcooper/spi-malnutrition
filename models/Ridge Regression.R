library(dplyr)
library(broom)
library(tidyr)
library(glmnet)
library(xtable)

source('C://Git/spi-malnutrition/models/mod_utils.R')

setwd('~')
setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov)) %>%
  na.omit

#Make categorical
all$spei <- all$spei24

all$spei <- ifelse(all$spei > 1.4, "Normal",
                   ifelse(all$spei < -0.4, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

sel <- all %>%
  filter(builtup < 20 & bare < 95 & spei24 <= 1.4)

#Do some log transfromations where necessary
transformations <- list(mean_annual_precip=function(x){log((x + 1)/1000)}
                        ,tmax_10yr_mean=function(x){x-273.15}
                        ,assistance=function(x){log(x+10)}
                        ,roughness=function(x){log(x+10)}
                        ,crop_prod=function(x){log(x+1)}
                        ,imports_percap=function(x){log(x+1)}
                        ,bare=function(x){log(x+1)}
                        ,population=function(x){log(x/1000 + 1000)}
                        ,grid_gdp=function(x){log(x + 10)}
)


for (n in names(sel)){
  if (n %in% names(transformations)){
    sel[ , paste0(n, '_t')] <- transformations[[n]](sel[ , n])
  }
}

#Get values for centering and define function
centerdf <- data.frame()
for (c in c("ndvi", "government_effectiveness", "grid_gdp", "grid_hdi",
            "mean_annual_precip_t", "nutritiondiversity_mfad", "population",
            "roughness_t", "stability_violence", "irrig_aai", "tmax_10yr_mean_t",
            "assistance_t", "crop_prod_t", "imports_percap_t", "bare_t", "enrollment")){
  centerdf <- bind_rows(centerdf, data.frame(var=c, max=max(sel[ , c]), min=min(sel[ , c])))
}

centerfun <- function(values, var){
  values <- values - centerdf$min[centerdf$var==var]
  newmax <- centerdf$max[centerdf$var==var] - centerdf$min[centerdf$var==var]
  
  scale <- 1/newmax
  values <- values*scale
  
  values
}

#Center
for (c in c("ndvi", "government_effectiveness", "grid_gdp", "grid_hdi",
            "mean_annual_precip_t", "nutritiondiversity_mfad", "population",
            "roughness_t", "stability_violence", "irrig_aai", "tmax_10yr_mean_t",
            "assistance_t", "crop_prod_t", "imports_percap_t", "bare_t", "enrollment")){
  sel[ , c] <- centerfun(sel[ , c], c)
}


x <- model.matrix(haz_dhs ~ age + as.factor(calc_birthmonth) + 
                    birth_order + hhsize + sex + mother_years_ed + toilet +
                    head_age + head_sex + wealth_index +
                    spei*ndvi +
                    spei*government_effectiveness +
                    spei*grid_gdp +
                    spei*grid_hdi +
                    spei*mean_annual_precip_t +
                    spei*nutritiondiversity_mfad +
                    spei*population +
                    spei*roughness_t +
                    spei*stability_violence +
                    spei*irrig_aai + 
                    spei*tmax_10yr_mean_t + 
                    spei*assistance_t + 
                    spei*crop_prod_t + 
                    spei*imports_percap_t + 
                    spei*bare_t + 
                    spei*enrollment,
                  data=sel)

mod <- cv.glmnet(x, sel$haz_dhs, alpha=1)
mod$lambda.min

tmp_coeffs <- coef(mod, s = "lambda.min")
df <- data.frame(term = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], estimate = tmp_coeffs@x)

write.csv(df %>% filter(grepl('speiDry:', term)), 'G://My Drive/Dissertation/Visualizations/individual_regressors.csv', row.names=F)

labels <- read.csv('G://My Drive/Dissertation/Visualizations/rowlabels.csv')

sum_table <- merge(labels, df)

sum_table <- sum_table %>% arrange(order)

sum_table <- sum_table %>% dplyr::select(-term, -order) %>%
  rename(`Coefficient Estimate`=estimate)

tab <- print(xtable(sum_table, 
              caption="Predicting HAZ Scores, with Geographic Factors Moderating the Effects of Drought.  Note: Model was estimated using the LASSO method, which does not give SE estiamtes.",
              digits = 3,
              alight = "ll"), include.rownames=FALSE)
cat(tab, file = 'G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/tables/S5.tex')

rast <- make_rasts_year(df, "speiDry", 2020, 
                        transformations,
                        centerdf, 
                        mask=93);plot(rast)

for (year in c(1990, 2000, 2016, 2017, 2018, 2020)){
  rast <- make_rasts_year(df, "speiDry", year, 
                          transformations,
                          centerdf, 
                          mask=93)
  writeRaster(rast, paste0("G://My Drive/DHS Spatial Covars/Final Rasters/Predictions/Dry", year, ".tif"), format='GTiff', overwrite=T)
}

