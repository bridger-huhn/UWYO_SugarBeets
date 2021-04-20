
require(plotly)
require(lubridate)
require(tidyverse)
# This experiment was planted on 7-18 
# Harvest 1 was on 8-23
# Harvest 2 was on 9-8
# Harvest 3 was on 9-25
#

##### Curves####

#Harvest 1

Harvest_1 <- read.delim("./Marios_data/2018/2018_IRGA_Harvest01.csv", sep = ",",na.strings = "NAN")
Harvest_1 <- Harvest_1[-1,]

LRC1 <- Harvest_1 %>% 
  filter(Comment == "LRC")

plots1 <- LRC1[c("Bucket","PARi","Photo")] %>% 
  mutate(PARi, PARi = as.numeric(as.character(PARi))) %>% 
  mutate(Photo, Photo = as.numeric(as.character(Photo))) %>% 
  mutate(Bucket, Bucket = as.integer(as.character(Bucket)))
plot(plots1$Photo~plots1$PARi, col = "red")

##Harvest 2

Harvest_2 <- read.delim("./Marios_data/2018/2018_IRGA_Harvest02.csv", sep = ",",na.strings = "NAN")
Harvest_2 <- Harvest_2[-1,]

LRC2 <- Harvest_2 %>% 
  filter(Comment == "LRC")

plots2 <- LRC2[c("Bucket","PARi","Photo")] %>% 
  mutate(PARi, PARi = as.numeric(as.character(PARi))) %>% 
  mutate(Photo, Photo = as.numeric(as.character(Photo))) %>% 
  mutate(Bucket, Bucket = as.integer(as.character(Bucket)))
points(plots2$Photo~plots2$PARi, col = "blue")
##Harvest 3

Harvest_3 <- read.delim("./Marios_data/2018/2018_IRGA_Harvest03.csv", sep = ",",na.strings = "NAN")
Harvest_3 <- Harvest_3[-1,]

LRC3 <- Harvest_3 %>% 
  filter(Comment == "LRC")

plots3 <- LRC3[c("Bucket","PARi","Photo")] %>% 
  mutate(PARi, PARi = as.numeric(as.character(PARi))) %>% 
  mutate(Photo, Photo = as.numeric(as.character(Photo))) %>% 
  mutate(Bucket, Bucket = as.integer(as.character(Bucket)))
points(plots3$Photo~plots3$PARi, col = "green")

df1$Name <- df2$Name[match(df1$Id,df2$Id)]

### light curve models

# aPhoto^2 +bPhoto+ c = 0
# a = theta
# b = -(Pmax + alphaI -theta_Rd)
#c = alphaI(Pmax-(1-theta)Rd)-RdPmax