library(dplyr)
library(readxl)
library(tidyr)

dat <- read.csv('G://My Drive/DHS Spatial Covars/World Governance Indicators/Combined_Subindicators_PV.csv',
                stringsAsFactors = F)

dat <- dat %>% filter(year==2017 & !Source %in% c('WCY', "WJP")) %>%
  na.omit %>%
  select(Code, Score=value, Source)

a_eiu=mean(dat$Score[dat$Source=='EIU'], na.rm=T); 
a_gcs=mean(dat$Score[dat$Source=='GCS'], na.rm=T); 
a_hum=mean(dat$Score[dat$Source=='HUM'], na.rm=T); 
a_ijt=mean(dat$Score[dat$Source=='IJT'], na.rm=T); 
a_ipd=mean(dat$Score[dat$Source=='IPD'], na.rm=T); 
a_wmo=mean(dat$Score[dat$Source=='WMO'], na.rm=T); 
a_prs=mean(dat$Score[dat$Source=='PRS'], na.rm=T)

b_eiu=0.2; b_gcs=0.09; b_hum=0.023; b_ijt=0.21; b_ipd=0.17; b_wmo=0.17; b_prs=0.07
s_eiu=0.54; s_gcs=1.27; s_hum=0.69; s_ijt=0.5; s_ipd=1.1; s_wmo=0.35; s_prs=0.79


nll <- function(a_eiu, a_gcs, a_hum, a_ijt, a_ipd, a_wmo, a_prs,
                b_eiu, b_gcs, b_hum, b_ijt, b_ipd, b_wmo, b_prs,
                s_eiu, s_gcs, s_hum, s_ijt, s_ipd, s_wmo, s_prs){
  LL <- 0
  
  for (code in names(table(dat$Code))[table(dat$Code) > 2]){
    ix <- dat$Code == code
    
    sources_ix <- dat$Source[ix]
    sources_ps <- c("EIU", "GCS", "HUM", "IJT", "IPD", "WMO", "PRS")
    
    y <- dat$Score[ix]
    
    as <- c(a_eiu, a_gcs, a_hum, a_ijt, a_ipd, a_wmo, a_prs)[sources_ps %in% sources_ix]
    bs <- c(b_eiu, b_gcs, b_hum, b_ijt, b_ipd, b_wmo, b_prs)[sources_ps %in% sources_ix]
    ss <- c(s_eiu^2, s_gcs^2, s_hum^2, s_ijt^2, s_ipd^2, s_wmo^2, s_prs^2)[sources_ps %in% sources_ix]
    
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
  
  -LL
}

c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
  "Brent")


stats4::mle(minuslogl = nll, start = list(a_eiu=a_eiu, a_gcs=a_gcs, a_hum=a_hum, a_ijt=a_ijt, a_ipd=a_ipd, a_wmo=a_wmo, a_prs=a_prs,
                                          b_eiu=b_eiu, b_gcs=b_gcs, b_hum=b_hum, b_ijt=b_ijt, b_ipd=b_ipd, b_wmo=b_wmo, b_prs=b_prs,
                                          s_eiu=s_eiu, s_gcs=s_gcs, s_hum=s_hum, s_ijt=s_ijt, s_ipd=s_ipd, s_wmo=s_wmo, s_prs=s_prs))

