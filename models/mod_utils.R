library(raster)

make_rasts_year <- function(mod, term, year, transformations, censor=TRUE, mask=TRUE){
  #mod is the model that has been fit
  #term can be either 'speiDry', 'speiWet', or ''
  #  'speiDry' and 'speiWet' will map estimated changes in HAZ scores during a wet or dry year
  #  '' will give a map of estimated HAZ scores during a normal year
  #year is the year to draw on predictor data from
  #transformations is a list of functions for how to transform rasters
  #   i.e. list(mean_annual_precip=function(x){x/1000})
  #censor will set values greater than 0 to NA
  
  setwd(paste0("G://My Drive/DHS Spatial Covars/Final Rasters/", year))
  
  s <- tidy(mod)
  
  coefs <- s[grepl(term, s$term), c('term', 'estimate')]
  
  coefs$term <- gsub(paste0(term, ':'), '', coefs$term)
  
  if(term == ''){
    #Define Variables for mapping baseline
    age <- mean(all$age)
    birth_order <- mean(all$birth_order)
    hhsize <- mean(all$hhsize)
    mother_years_ed <- mean(all$mother_years_ed)
    head_age <- mean(all$head_age)
    
    coefs <- coefs[!grepl('spei', coefs$term), ]
  }
  
  rast <- coefs$estimate[1]
  for (i in 2:nrow(coefs)){
    rast_name <- coefs$term[i]
    
    print(rast_name)
    
    tmp_rast <- raster(paste0(gsub('_t$', '', rast_name), '.tif'))
    
    if (grepl('_t$', rast_name)){
      tmp_rast <- transformations[[gsub('_t$', '', rast_name)]](tmp_rast)
    }

    suppressWarnings(rast <- rast + tmp_rast*coefs$estimate[i])
  }
  
  if (censor){
    rast[rast > 0] <- 0
  }
  
  if (mask){
    
    #Mask Bare areas, similar to https://www.nature.com/articles/nature25760#methods
    bare <- raster('bare.tif')
    rast <- rast*(bare < mask)
    
    #Mask extremely builtup areas, as they are urban
    builtup <- raster('builtup.tif')
    rast <- rast*(builtup < 20)
    
    #Mask areas that are neither forest nor bare (great lakes)
    forest <- raster('forest.tif')
    rast <- rast*((bare + forest) > 0)
    
    #Mask populated areas
    # pop <- raster('population_mask.tif')
    # rast <- rast*(pop > 0.1)
    
  }
  
  return(rast)
}

getValuesAtPoint <- function(mod, year, transformations, x, y){
  setwd(paste0("G://My Drive/DHS Spatial Covars/Final Rasters/", year))
  
  s <- tidy(mod)
  
  coefs <- s[grepl('spei', s$term), c('term', 'estimate')]
  
  coefs$term <- gsub('spei...:|spei...', '', coefs$term)
  
  coefs[1, 'Value'] <- 1
  for (i in 2:nrow(coefs)){
    rast_name <- coefs$term[i]
    
    print(rast_name)
    
    tmp_rast <- raster(paste0(gsub('_t$', '', rast_name), '.tif'))
    
    if (grepl('_t$', rast_name)){
      tmp_rast <- transformations[[gsub('_t$', '', rast_name)]](tmp_rast)
    }
    
    v <- raster::extract(tmp_rast, matrix(c(x, y), nrow = 1))
    
    coefs[i, 'Value'] <- v
  }
  
  coefs$Impact <- coefs$estimate*coefs$Value
  
  return(coefs)
}

setNAs <- function(raster, column, all){
  raster[raster > (max(all[ , column], na.rm=T) + sd(all[ , column], na.rm=T)/2)] <- NA
  raster[raster < (min(all[ , column], na.rm=T) - sd(all[ , column], na.rm=T)/2)] <- NA
  return(raster)
}