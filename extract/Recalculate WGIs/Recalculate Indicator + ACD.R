library(dplyr)
library(readxl)
library(tidyr)
library(rgdal)
library(countrycode)

all <- read.csv('G://My Drive/DHS Spatial Covars/World Governance Indicators/Combined_Subindicators_PV.csv',
                stringsAsFactors = F)

new <- readOGR('G://My Drive/DHS Spatial Covars/UCDP-PRIO Armed Conflict Dataset', 'admin1_data', 
               stringsAsFactors = F)@data

new <- new %>% 
  select(Country=NAME_0, Code=GID_0, Admin_Code=GID_1, ACD=X2017)

#Need to map Timor-Leste (TMP), DRC (ZAR), Western Sahara (NA?), Romania (ROM), 

new$Code[new$Country=='Timor-Leste'] <- 'TMP'
new$Code[new$Country=='Democratic Republic of the Congo'] <- 'ZAR'
new$Code[new$Country=='Romania'] <- 'ROM'

dat <- all %>% filter(year==2017 & !Source %in% c('WCY', "WJP")) %>%
  na.omit %>%
  select(Code, Score=value, Source) %>%
  merge(new %>% select(-ACD, -Country)) %>%
  bind_rows(new %>% mutate(Source='ACD') %>% select(Code, Score=ACD, Source, Admin_Code))

a_eiu=mean(dat$Score[dat$Source=='EIU'], na.rm=T); 
a_gcs=mean(dat$Score[dat$Source=='GCS'], na.rm=T); 
a_hum=mean(dat$Score[dat$Source=='HUM'], na.rm=T); 
a_ijt=mean(dat$Score[dat$Source=='IJT'], na.rm=T); 
a_ipd=mean(dat$Score[dat$Source=='IPD'], na.rm=T); 
a_wmo=mean(dat$Score[dat$Source=='WMO'], na.rm=T); 
a_prs=mean(dat$Score[dat$Source=='PRS'], na.rm=T)

b_eiu=0.2; b_gcs=0.09; b_hum=0.023; b_ijt=0.21; b_ipd=0.17; b_wmo=0.17; b_prs=0.07
s_eiu=0.54; s_gcs=1.27; s_hum=0.69; s_ijt=0.5; s_ipd=1.1; s_wmo=0.35; s_prs=0.79

a_acd <- mean(dat$Score[dat$Source=='ACD'], na.rm=T)
b_acd <- 0.5
s_acd <- 0.5

nll <- function(a_eiu, a_gcs, a_hum, a_ijt, a_ipd, a_wmo, a_prs,
                b_eiu, b_gcs, b_hum, b_ijt, b_ipd, b_wmo, b_prs,
                s_eiu, s_gcs, s_hum, s_ijt, s_ipd, s_wmo, s_prs,
                a_acd, b_acd, s_acd){
  LL <- 0
  
  for (code in names(table(dat$Admin_Code))[table(dat$Admin_Code) > 2]){
    ix <- dat$Admin_Code == code
    
    sources_ix <- dat$Source[ix]
    sources_ps <- c("EIU", "GCS", "HUM", "IJT", "IPD", "WMO", "PRS", "ACD")
    
    y <- dat$Score[ix]
    
    as <- c(a_eiu, a_gcs, a_hum, a_ijt, a_ipd, a_wmo, a_prs, a_acd)[sources_ps %in% sources_ix]
    bs <- c(b_eiu, b_gcs, b_hum, b_ijt, b_ipd, b_wmo, b_prs, b_acd)[sources_ps %in% sources_ix]
    ss <- c(s_eiu^2, s_gcs^2, s_hum^2, s_ijt^2, s_ipd^2, s_wmo^2, s_prs^2, s_acd^2)[sources_ps %in% sources_ix]
    
    if (length(bs) > 1){
      B <- diag(as)
      sigma <- diag(ss)
    } else{
      B <- matrix(as)
      sigma <- matrix(ss)
    }
    beta <- matrix(bs)
    alpha <- matrix(as)
    
    omega <- beta %*% t(beta) + B %*% sigma %*% t(B)
    
    LL <- LL + log(det(omega)) + t(y - alpha) %*% omega^-1 %*% (y - alpha)
    
  }
  
  cat(-LL, '\n')
  
  -LL
}

res <- stats4::mle(minuslogl = nll, start = list(a_eiu=a_eiu, a_gcs=a_gcs, a_hum=a_hum, a_ijt=a_ijt, a_ipd=a_ipd, a_wmo=a_wmo, a_prs=a_prs,
                                          b_eiu=b_eiu, b_gcs=b_gcs, b_hum=b_hum, b_ijt=b_ijt, b_ipd=b_ipd, b_wmo=b_wmo, b_prs=b_prs,
                                          s_eiu=s_eiu, s_gcs=s_gcs, s_hum=s_hum, s_ijt=s_ijt, s_ipd=s_ipd, s_wmo=s_wmo, s_prs=s_prs,
                                          a_acd=a_acd, b_acd=b_acd, s_acd=s_acd))

res <- res@coef

w_denominator <- (1 + res[['s_eiu']]^-2 + res[['s_gcs']]^-2 + res[['s_hum']]^-2 + res[['s_ijt']]^-2 + res[['s_ipd']]^-2 + res[['s_wmo']]^-2 + res[['s_prs']]^-2 + res[['s_acd']])

res <- as.list(res)

res[['w_eiu']] <- res$s_eiu^-2/(w_denominator)
res[['w_gcs']] <- res$s_gcs^-2/(w_denominator)
res[['w_hum']] <- res$s_hum^-2/(w_denominator)
res[['w_ijt']] <- res$s_ijt^-2/(w_denominator)
res[['w_ipd']] <- res$s_ipd^-2/(w_denominator)
res[['w_wmo']] <- res$s_wmo^-2/(w_denominator)
res[['w_prs']] <- res$s_prs^-2/(w_denominator)
res[['w_acd']] <- res$s_acd^-2/(w_denominator)

resdf <- data.frame(unlist(res), names(res))
names(resdf) <- c('value', 'name')
resdf$variable <- substr(resdf$name, 1, 1)
resdf$Source <- toupper(substr(resdf$name, 3, 5))
resdf$name <- NULL
resdf <- resdf %>% spread(variable, value)

comb <- merge(dat, resdf)
comb$est <- ((comb$Score - comb$a)/comb$b)*comb$w

final <- comb %>%
  group_by(Admin_Code) %>%
  summarize(pv=sum(est))







sel <- all %>% filter(year==2017 & Source %in% c('WCY', "WJP")) %>%
  na.omit %>%
  select(Code, Score=value, Source)

nonrep <- merge(final, sel)

wjp_res <- summary(lm(pv~Score, data=nonrep %>% filter(Source=='WJP')))
