library(raster)

make_rasts_year <- function(mod, term, year, transformations, censor=TRUE){
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
    
    tmp_rast <- raster(paste0(rast_name, '.tif'))
    
    if (rast_name %in% names(transformations)){
      tmp_rast <- transformations[[rast_name]](tmp_rast)
    }

    suppressWarnings(rast <- rast + tmp_rast*coefs$estimate[i])
  }
  
  if (censor){
    rast[rast > 0] <- 0
  }
  
  return(rast)
}

getValuesAtPoint <- function(mod, year, transform, x, y){
  setwd(paste0("G://My Drive/DHS Spatial Covars/Final Rasters/", year))
  
  s <- tidy(mod)
  
  coefs <- s[grepl('spei', s$term), c('term', 'estimate')]
  
  coefs$extremeType <- ifelse(grepl('Dry', coefs$term), "Dry", "Wet")
  
  coefs$term <- gsub('spei...:|spei...', '', coefs$term)
  
  coefs <- coefs %>% spread(extremeType, estimate)
  
  coefs[1, 'Value'] <- 1
  for (i in 2:nrow(coefs)){
    rast_name <- coefs$term[i]
    
    tmp_rast <- raster(paste0(rast_name, '.tif'))
    
    if (rast_name %in% names(transformations)){
      tmp_rast <- transformations[[rast_name]](tmp_rast)
    }
    
    v <- raster::extract(tmp_rast, matrix(c(x, y), nrow = 1))
    
    coefs[i, 'Value'] <- v
  }
  
  coefs$WetImpact <- coefs$Wet*coefs$Value
  coefs$DryImpact <- coefs$Dry*coefs$Value
  
  cat("Dry:", sum(coefs$DryImpact), "Wet:", sum(coefs$WetImpact), '\n')
  
  return(coefs)
}

setNAs <- function(raster, column, all){
  raster[raster > (max(all[ , column], na.rm=T) + sd(all[ , column], na.rm=T)/2)] <- NA
  raster[raster < (min(all[ , column], na.rm=T) - sd(all[ , column], na.rm=T)/2)] <- NA
  return(raster)
}