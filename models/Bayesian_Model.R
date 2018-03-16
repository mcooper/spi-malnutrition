#setwd('G://My Drive/DHS Processed')

library(dplyr)

hh <- read.csv('hhvars.csv')
gdp <- read.csv('country_gdp.csv')
farm <- read.csv('FarmingSystems.csv')
lc <- read.csv('landcover.csv')
md <- read.csv('MarketDist.csv')
pop <- read.csv('PopPer100sqkm.csv')
spi <- read.csv('Coords&SPI.csv')

#Look at including World Governance Indicators at: http://info.worldbank.org/governance/wgi/#home

##############################
#Process landcover data
#################################
human <- paste0('cci_', c('10', '11', '12', '20', '30', '190', '200', '201', '202', '220'))
natural <- paste0('cci_', c('40', '50', '60', '61', '62', '70', '71', '80', '90', '100', '110', '120', '121', '122',
                            '130', '140', '150', '152', '153', '160', '170', '180', '210'))

getPercetCover <- function(selcols, allcolmatch, df){
  if(length(selcols) > 1){
    selcolsum <- rowSums(df[ , selcols[selcols %in% names(df)]], na.rm=T)
  } else{
    selcolsum <- df[ , selcols]
  }
  allcolsum <- rowSums(df[ , grepl(allcolmatch, names(df))], na.rm=T)
  return(selcolsum/allcolsum)
}

lc$human <- getPercetCover(human, 'cci_', lc)
lc$natural <- getPercetCover(natural, 'cci_', lc)

lc <- lc %>%
  select(human, natural, interview_year, code)

#################################
#Process pop and market dist data
#################################
pop00 <- merge(select(pop, code, pop=pop00), data.frame(interview_year=seq(1988, 2002)))
pop05 <- merge(select(pop, code, pop=pop05), data.frame(interview_year=seq(2003, 2007)))
pop10 <- merge(select(pop, code, pop=pop10), data.frame(interview_year=seq(2008, 2012)))
pop15 <- merge(select(pop, code, pop=pop15), data.frame(interview_year=seq(2013, 2017)))

pop <- Reduce(bind_rows, list(pop00, pop05, pop10, pop15))


md00 <- merge(select(md, code, md=market2000), data.frame(interview_year=seq(1988, 2007)))
md15 <- merge(select(md, code, md=market2015), data.frame(interview_year=seq(2008, 2016)))

md <- Reduce(bind_rows, list(md00, md15))

################################
#Combine and clear workspace
################################
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
              list(hh, gdp, farm, lc, md, pop, spi))

#rm(list=setdiff(ls(), "all")) #remove everything but our data

###########################################################
#Analyze missing data, determine which variables to keep
#############################################################

all <- all %>% filter(!(haz < -7 | haz > 5)) #first filter extreme HAZs

all <- all %>% filter(urban_rural == 'Rural') #Lets just look at rural hhs

#Drop variables that will not be used in regression
all <- all %>%
  select(-interview_month, -interview_cmc, -calc_birthmonth, -calc_birthyear, -thousandday_month, -thousandday_year, 
         -latitude, -longitude, 
         -hhid, -householdno, -clusterid, 
         -father_line, -mother_line, -child_line_num, 
         -haz_dhs, -waz_dhs, -whz_dhs, 
         -haz_who, -waz_who, -whz_who, 
         -height, -weight, -how_measured, 
         -sampweight, -urban_rural, -parents_years_ed, 
         -wealth_factor, -dependents, -caseid, 
         -birthyear, -birthmonth, -birthday_cmc, 
         -fromKR, -filesource, 
         -waz, -whz, 
         -continent, -farm_system, -human, 
         -spi6, -spi12, -spi36, -birthday_9monthtotal, -birthday_spi9,
         -preceeding_interval, -suceeding_interval #Need to figure out missing data vs Not Applicable
         )

#Relevel factors
all <- all %>%
  mutate(toilet=relevel(toilet, ref="No Facility"),
         relationship_hhhead=relevel(relationship_hhhead, ref="Immediate Family"),
         otherwatersource=relevel(otherwatersource, ref="Surface Water"),
         drinkwatersource=relevel(drinkwatersource, ref="Surface Water"),
         wealth_index=relevel(wealth_index, ref="Poorest"))

#Well stan is crashing in the cloud, so lets reduce the number of variables, eh?
all <- all %>%
  select(haz, code, interview_year, country, hhsize, spi24)

#Pivot out factors 
pivotFactor <- function(df, column){
  if (!column %in% names(df)){
    return(df) 
  }
  fact <- df[ , column]
  for (l in levels(fact)[2:length(levels(fact))]){
    df[ , gsub(" |/", "", paste0(column, l))] <- as.integer(fact == l)
  }
  df[ , column] <- NULL
  df
}

all <- pivotFactor(all, "toilet")
all <- pivotFactor(all, "relationship_hhhead")
all <- pivotFactor(all, "otherwatersource")
all <- pivotFactor(all, "drinkwatersource")
all <- pivotFactor(all, "wealth_index")
all <- pivotFactor(all, "head_sex")
all <- pivotFactor(all, "sex")

#Turn grouping variables into numeric
#Because some factors may have dropped out, we have to do factor -> character -> factor -> int
#To ensure the maximum possible value is equal to length of unique values
all$code <- as.integer(as.factor(as.character(all$code)))
all$country <- as.integer(as.factor(as.character(all$country)))


############################
#Create data list and prep code
############################

data <- "data {\n\tint<lower=0> N;"

parameters <- "parameters {"

tparameters <- "transformed parameters{"

#Add grouping vars to data and datalist
data <- paste0(data, "\n\tint<lower=1> country_N;\n\tint<lower=1> code_N;\n\tint<lower=1, upper=country_N> country[N];\n\tint<lower=1, upper=code_N> code[N];")

datalist <- list(N=nrow(all), country_N=length(unique(all$country)), code_N=length(unique(all$code)), 
                 country=all$country, code=all$code)

#Create data and parameters for variables that will need imputation
getclass <- function(vect){
  ifelse(class(vect)=="integer", "int",
     ifelse(class(vect)=="numeric", "real", warning("Class is not int or real!")))
}

for (i in names(all)[!names(all) %in% c('code', 'country')]){
  var <- all[ , i]
  class <- getclass(var)
  
  data <- paste0(data, "\n\n\t//", i) 
  
  if (sum(is.na(var))==0){
    datalist[[i]] <- var
    
    if (class=="int"){
      data <- paste0(data, "\n\tint<lower=0> ", i, "[N];")
    } else{
      data <- paste0(data, "\n\treal ", i, "[N];")
    }
      
  } else{
    datalist[[paste0(i, '_N_mis')]] <- sum(is.na(var))
    data <- paste0(data, "\n\tint<lower=0> ", i, "_N_mis;")
    
    datalist[[paste0(i, '_N_obs')]] <- sum(!is.na(var))
    data <- paste0(data, "\n\tint<lower=0> ", i, "_N_obs;")
    
    datalist[[paste0(i, '_i_mis')]] <- which(is.na(var))
    data <- paste0(data, "\n\tint<lower=0, upper=N> ", i, "_i_mis[", i, "_N_mis];")
    
    datalist[[paste0(i, '_i_obs')]] <- which(!is.na(var))
    data <- paste0(data, "\n\tint<lower=0, upper=N> ", i, "_i_obs[", i, "_N_obs];")
    
    datalist[[paste0(i, '_obs')]] <- all[!is.na(var), i]
    if (class=="int"){
      data <- paste0(data, "\n\tint<lower=0> ", i, "_obs[", i, "_N_obs];")
    } else{
      data <- paste0(data, "\n\treal ", i, "_obs[", i, "_N_obs];")
    }
    
    parameters <- paste0(parameters, "\n\n\t//", i, "\n\treal", " ", i, "_mis[", i, "_N_mis];")
    
    #For transformed parameters, must declare all variables at the beginning, then do operations on them
    tparameters <- paste0(tparameters, "\n\treal", " ", i, "[N];")
    
  }
}

#Now add the variable transformations, since they have ll been declared
for (i in names(all)[!names(all) %in% c('code', 'country')]){
  var <- all[ , i]
  if (!sum(is.na(var))==0){
    tparameters <- paste0(tparameters, "\n\n\t", i, "[", i, "_i_obs] = ", i, "_obs;\n\t",
                          i, "[", i, "_i_mis] = ", i, "_mis;")
    
  }
}

data <- paste0(data, "\n}")
tparameters <- paste0(tparameters, "\n}")

#Create intercept, betas and other parameters
parameters <- paste0(parameters, "\n\n\t//betas and intercetp\n\treal intercept;")

for (i in names(all)[!names(all) %in% c('country', 'code', 'haz')]){
  parameters <- paste0(parameters, "\n\treal beta_", i, ";")
}

parameters <- paste0(parameters, "\n\tvector[country_N] country_intercept;\n\tvector[code_N] code_intercept;")

parameters <- paste0(parameters, "\n\treal<lower=0> sigma_e;\n\treal<lower=0> sigma_country;\n\treal<lower=0> sigma_code;\n}")

#Create model
model <- "model {
\treal mu;
\tcountry_intercept ~ normal(0, sigma_country);
\tcode_intercept ~ normal(0, sigma_code);
\tfor (i in 1:N){
\t\tmu = intercept + country_intercept[country[i]] + code_intercept[code[i]] "

for (i in names(all)[!names(all) %in% c('country', 'code', 'haz')]){
  model <- paste0(model, "+ beta_", i, "* ", i, "[i]")
}

model <- paste0(model, ";\n\t\thaz[i] ~ normal(mu, sigma_e);\n\t}\n}\n")

cat(paste(data, parameters, tparameters, model, sep="\n\n"), file="TestBHS Model.stan")

###################
#Run model
###################

library(rstan)
library(parallel)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit <- stan(file = "TestBHS Model.stan", data=datalist, iter=1000, warmup=100, thin=25, chains=1)



#Also look into the sample_file argument to stan() if you are calling from R, which will write draws to a file rather than holding everything in RAM. Just be sure to index the filename by chain_id to avoid having multiple processes write to the same file!

#https://groups.google.com/forum/#!topic/stan-users/9-VJ0UhQ9ZM












