require(brms)
require(tidyverse)
setwd("/Users/bridgerhuhn/Documents/Research/SugarBeets/UWYO_SugarBeets")
set.seed(1991)
d <- read.csv("Alberts_data/2017_AL_2treatments.csv")
d$topdw <- replace(d$topdw, d$topdw == 0, NA)
#### standardizes whole data set
d$logDWT <- log(d$topdw+d$rootdw)
d$logleaf.area <- log(d$leafarea)
d$logBG <- log(d$rootdw)
d$logAG <- log(d$topdw)
# d$rootshoot <- d$logAG/d$logBG

ds <- d %>% mutate_if(is.numeric, scale)
# ds$harvest <- d$harvest
###functions####

RGR <- function(bm1, bm2, time1, time2){
  (mean(log(bm2))-mean(log(bm1)))/(time2-time1)
} 
RGR2 <- function(NAR, LAR){NAR * LAR}
NAR <- function(A){A} #mg/cm^2/day^2
LAR <- function(LMR, SLA){LMR*SLA} #cm^2/gplant
SLA <- function(LA, leafBM){LA/leafBM} #cm^2/gleaf
LMR <- function(leafBM, plantBM){leafBM/plantBM} # gleaf/ gplant
RLPR <- function(numleaves1, numleaves2, time1, time2){
  (ln(numleaves2)-ln(numleaves1))/(time2-time1)
} #leaf/leaf/day

### Leaf trait colums ####
d$SLA <- SLA(LA = d$leafarea, leafBM = d$topdw)
d$LMR <- LMR(leafBM = d$topdw, plantBM = (d$rootdw+ d$topdw))
SLAMOD <- bf(spla ~ leafarea * topdw)
LMRMOD <- bf(lmr ~ topdw * (rootdw + topdw))
#### models ####

#################biomass ~    SLA                +   LMR
SEM_fin_4 <- brm(logDWT ~ (logleaf.area * logAG) + (topdw * (rootdw + topdw)),
                 data=ds,
                 cores=2,
                 chains = 4)
plot(SEM_fin_4)

pp_check(SEM_fin_4, resp="logBG", nsamples = 200)

