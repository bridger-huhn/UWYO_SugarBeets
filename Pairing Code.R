require(tidyverse)
require(brms)
setwd("/Users/bridgerhuhn/Documents/Research/SugarBeets/UWYO_SugarBeets")
set.seed(1991)
d <- read.csv("Alberts_data/2017_AL_2treatments.csv")
d <- d[!is.na(d$rootdw), ]

dat <- d[1,]
dat <- dat[-1,]

i <- 2
for (i in 1: length(unique(d$DAP))) {
  temp <- d %>% 
    filter(DAP == unique(d$DAP)[i], trt =="Grass")
  v <- seq(1:nrow(temp))
  temp <- temp[c(sample(v, 15)),]
  temp2 <- d %>% 
    filter(DAP == unique(d$DAP)[i], trt =="No grass")
  v2 <- seq(1:nrow(temp2))
  temp2 <- temp2[c(sample(v2, 15)),]
  dat <- rbind(dat, temp, temp2)
}

plot(d$rootfw ~d$DAP)

function()

  