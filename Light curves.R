require(tidyverse)
require(plotly)
require(lubridate)

# This experiment was planted on 7-18 
# Harvest 1 was on 8-23
# Harvest 2 was on 9-8
# Harvest 3 was on 9-25
#

##### Curves####

#Harvest 1
#setwd("C:/Users/bridg/Dropbox/Sugar beets/SugarBeet/Data/2018")## laptop
setwd("C:/Users/bhuhn/Dropbox/Sugar beets/SugarBeet/Data/2018")##
Harvest_1 <- read.delim("2018_IRGA_Harvest01.csv", sep = ",",na.strings = "NAN")
Harvest_1 <- Harvest_1[-1,]

LRC1 <- Harvest_1 %>% 
  filter(Comment == "LRC")

plots1 <- LRC1[c("Bucket","PARi","Photo")] %>% 
  mutate(PARi, PARi = as.numeric(as.character(PARi))) %>% 
  mutate(Photo, Photo = as.numeric(as.character(Photo))) %>% 
  mutate(Bucket, Bucket = as.integer(as.character(Bucket)))
plot(plots1$Photo~plots1$PARi, col = "red")

##Harvest 2

Harvest_2 <- read.delim("2018_IRGA_Harvest02.csv", sep = ",",na.strings = "NAN")
Harvest_2 <- Harvest_2[-1,]

LRC2 <- Harvest_2 %>% 
  filter(Comment == "LRC")

plots2 <- LRC2[c("Bucket","PARi","Photo")] %>% 
  mutate(PARi, PARi = as.numeric(as.character(PARi))) %>% 
  mutate(Photo, Photo = as.numeric(as.character(Photo))) %>% 
  mutate(Bucket, Bucket = as.integer(as.character(Bucket)))
points(plots2$Photo~plots2$PARi, col = "blue")
##Harvest 3

Harvest_3 <- read.delim("2018_IRGA_Harvest03.csv", sep = ",",na.strings = "NAN")
Harvest_3 <- Harvest_3[-1,]

LRC3 <- Harvest_3 %>% 
  filter(Comment == "LRC")

plots3 <- LRC3[c("Bucket","PARi","Photo")] %>% 
  mutate(PARi, PARi = as.numeric(as.character(PARi))) %>% 
  mutate(Photo, Photo = as.numeric(as.character(Photo))) %>% 
  mutate(Bucket, Bucket = as.integer(as.character(Bucket)))
points(plots3$Photo~plots3$PARi, col = "green")

df1$Name <- df2$Name[match(df1$Id,df2$Id)]

#puts PAR into rounded bins
plots1$RoundedPAR <- cut(plots1$PARi,breaks = c(-100,50,150,350,450,700,900,1100,1300,1500,1700, 1900), labels = c(0,100,200,400,600,800,1000,1200,1400,1600,1800))
plots1$RoundedPAR <- as.numeric(as.character(plots1$RoundedPAR))
plots2$RoundedPAR <- cut(plots2$PARi,breaks = c(-100,50,150,350,450,700,900,1100,1300,1500,1700, 1900), labels = c(0,100,200,400,600,800,1000,1200,1400,1600,1800))
plots2$RoundedPAR <- as.numeric(as.character(plots2$RoundedPAR))
plots3$RoundedPAR <- cut(plots3$PARi,breaks = c(-100,50,150,350,450,700,900,1100,1300,1500,1700, 1900), labels = c(0,100,200,400,600,800,1000,1200,1400,1600,1800))
plots3$RoundedPAR <- as.numeric(as.character(plots3$RoundedPAR))

##### Biomass data ####

bmdat1<-read.csv("2018_Data_8-23-2018.csv", skip = 1)
bmdat1$DWT <- as.numeric(as.character(bmdat1$AG))+as.numeric(as.character(bmdat1$BG.1))
names(bmdat1)[1] <- "bucket"
names(bmdat1)[24] <- "leaf.area"
bmdat2<-read.csv("2018_Data_9-8-2018 (711 OMITED).csv", skip = 1)
bmdat2$DWT <- as.numeric(as.character(bmdat2$AG))+as.numeric(as.character(bmdat2$BG.1))
names(bmdat2)[1] <- "bucket"
names(bmdat2)[24] <- "leaf.area"
bmdat3<-read.csv("2018_Data_9-25-2018.csv", skip = 1)
bmdat3$DWT <- as.numeric(as.character(bmdat3$AG))+as.numeric(as.character(bmdat3$BG.1))
names(bmdat3)[1] <- c("bucket")
names(bmdat3)[24] <- "leaf.area"

boxplot(bmdat1$DWT, bmdat2$DWT, bmdat3$DWT, xlab = "Time point", ylab = "Biomass")

bm1dat1 <- filter(bmdat1, bucket %in% unique(plots1$Bucket))
bm1dat2 <- filter(bmdat2, bucket %in% unique(plots2$Bucket))
bm1dat3 <- filter(bmdat3, bucket %in% unique(plots3$Bucket))

### Getting meta and light data####

met <- read.csv("2018_Met.csv")
met$TIMESTAMP <- as.POSIXct(strptime(met$TIMESTAMP,format ="%m/%d/%Y %H:%M"))

#filters data into two time steps and makes data numeric
metTP0 <- met[met$TIMESTAMP >= "2018-07-18 00:00:00 MDT" & met$TIMESTAMP < "2018-08-23 00:00:00 MDT",]
metTP0$PAR_Den_Avg <- as.numeric(as.character(metTP0$PAR_Den_Avg))
metTP0$RECORD <- as.numeric(as.character(metTP0$RECORD))

metTP1 <- met[met$TIMESTAMP >= "2018-08-23 00:00:00 MDT" & met$TIMESTAMP < "2018-09-08 00:00:00 MDT",]
metTP1$PAR_Den_Avg <- as.numeric(as.character(metTP1$PAR_Den_Avg))
metTP1$RECORD <- as.numeric(as.character(metTP1$RECORD))

metTP2 <- met[met$TIMESTAMP >= "2018-09-08 00:00:00 MDT" & met$TIMESTAMP < "2018-09-25 00:00:00 MDT",]
metTP2$PAR_Den_Avg <- as.numeric(as.character(metTP2$PAR_Den_Avg))
metTP2$RECORD <- as.numeric(as.character(metTP2$RECORD))

### set a scaling number to each record in the "scaler" column
metTP0$count <- seq(1,length(metTP0$TIMESTAMP),1)
metTP0$scaler <- ((length(metTP0$count)-metTP0$count)/length(metTP0$count))
metTP1$count <- seq(1,length(metTP1$TIMESTAMP),1)
metTP1$scaler <- ((length(metTP1$count)-metTP1$count)/length(metTP1$count))
metTP1$scaler2<- 1-metTP1$scaler
metTP2$count <- seq(1,length(metTP2$TIMESTAMP),1)
metTP2$scaler <- ((length(metTP2$count)-metTP2$count)/length(metTP2$count))
metTP2$scaler2<- 1-metTP2$scaler

##### Single Light to Photo Models ####
bm1dat1$RGR <- bm1dat1$DWT/36
bm1dat2$RGR <- bm1dat2$DWT/52
bm1dat3$RGR <- bm1dat3$DWT/69
plot(bm1dat1$RGR, col = "red", ylim = c(0,1))
points(bm1dat2$RGR, col = "green")
points(bm1dat3$RGR, col = 'Blue')

##puts RGR values in light curve data
plots1$RGR <- bm1dat1$RGR[match(plots1$Bucket,bm1dat1$bucket)]
plots2$RGR <- bm1dat2$RGR[match(plots2$Bucket,bm1dat2$bucket)]
plots3$RGR <- bm1dat3$RGR[match(plots3$Bucket,bm1dat3$bucket)]

##makes individual models
a <- 14
b <- 469.2429 
c <- 1


##model for harvest 1
dfOut1 <- data.frame()
plots1sum<- plots1 %>% 
  group_by(RoundedPAR, Bucket) %>% 
  summarize_if(is.numeric, mean) %>% 
  as.data.frame()
for (i in 1:length(unique(plots1sum$Bucket))) {
  m1ind <- nls(Photo ~ ((a* PARi)/(b + PARi )) - c,
               data = plots1sum[plots1sum$Bucket == unique(plots1sum$Bucket)[i],], 
               start = list(a=a,b=b,c=c))
  
  dfOut1 <- rbind(dfOut1, data.frame(
    Pot = unique(plots1sum$Bucket)[i],
    a = coef(m1ind)[1],
    b = coef(m1ind)[2],
    c = coef(m1ind)[3]
  ))
}

ggplot(dat = dfOut1, aes(x = Pot, y = b, col = as.factor(Pot)))+
  geom_point()+
  geom_point(aes(x = Pot, y = a))+
  geom_point(aes(x = Pot, y = c))
##model for harvest 2
dfOut2 <- data.frame()
plots2sum<- plots2 %>% 
  group_by(RoundedPAR, Bucket) %>% 
  summarize_if(is.numeric, mean) %>% 
  as.data.frame()

for (i in 1:length(unique(plots2sum$Bucket))) {
  m1ind <- nls(Photo ~ ((a* PARi)/(b + PARi )) - c,
               data = plots2sum[plots2sum$Bucket == unique(plots2sum$Bucket)[i],], 
               start = list(a=a,b=b,c=c))
  
  dfOut2 <- rbind(dfOut2, data.frame(
    Pot = unique(plots2sum$Bucket)[i],
    a = coef(m1ind)[1],
    b = coef(m1ind)[2],
    c = coef(m1ind)[3]
  ))
}

##model for harvest 3
dfOut3 <- data.frame()
plots3sum<- plots3 %>% 
  group_by(RoundedPAR, Bucket) %>% 
  summarize_if(is.numeric, mean) %>% 
  as.data.frame()

for (i in 1:length(unique(plots3sum$Bucket))) {
  m1ind <- nls(Photo ~ ((a* PARi)/(b + PARi )) - c,
               data = plots3sum[plots3sum$Bucket == unique(plots3sum$Bucket)[i],], 
               start = list(a=a,b=b,c=c))
  
  dfOut3 <- rbind(dfOut3, data.frame(
    Pot = unique(plots3sum$Bucket)[i],
    a = coef(m1ind)[1],
    b = coef(m1ind)[2],
    c = coef(m1ind)[3]
  ))
}

#### Predict Individual plant models ####
####Individual curves Harvest 1####
ImetTP0 <- metTP0
PARi <- ImetTP0$PAR_Den_Avg
TP1individ.modeled <- data.frame() 
dfOut1$a
for (i in 1:length(dfOut1$a)) {
   TP1individ.modeled <- rbind(TP1individ.modeled, data.frame(
    Pot = unique(dfOut1$Pot)[i],
    A = ((dfOut1$a[i])* ImetTP0$PAR_Den_Avg)/((dfOut1$b[i]) + ImetTP0$PAR_Den_Avg ) - dfOut1$c[i],
    TIMESTAMP = ImetTP0$TIMESTAMP,
    PARi = PARi
  ))
}

Harv1_IMOD1 <- TP1individ.modeled

ggplotly(ggplot(data = TP1individ.modeled, aes(y=A,x = TIMESTAMP, color = as.factor(Pot)))+
  geom_line()+
  #xlim(c(TP1individ.modeled$TIMESTAMP[1],TP1individ.modeled$TIMESTAMP[1000]))+
  scale_color_manual(values=c("green", "red", "blue", "purple"))+
  labs(title = "Harvest 1",
       y = "A",
       x = "date",
       color = "Pot")
)

####Individual curves Harvest 2####
ImetTP0 <- met[met$TIMESTAMP >= "2018-07-18 00:00:00 MDT" & met$TIMESTAMP < "2018-09-08 00:00:00 MDT",]
ImetTP0$PAR_Den_Avg <- as.numeric(as.character(ImetTP0$PAR_Den_Avg))
ImetTP0$RECORD <- as.numeric(as.character(ImetTP0$RECORD))

dfOut1<-dfOut2
PARi <- ImetTP0$PAR_Den_Avg
TP1individ.modeled <- data.frame() 
for (i in 1:length(dfOut1$a)) {
  TP1individ.modeled <- rbind(TP1individ.modeled, data.frame(
    Pot = unique(dfOut1$Pot)[i],
    A = ((dfOut1$a[i])* ImetTP0$PAR_Den_Avg)/((dfOut1$b[i]) + ImetTP0$PAR_Den_Avg ) - dfOut1$c[i],
    TIMESTAMP = ImetTP0$TIMESTAMP,
    PARi = PARi
  ))
}

Harv2_IMOD1 <- TP1individ.modeled

ggplotly(ggplot(data = TP1individ.modeled, aes(y=A,x = TIMESTAMP, color = as.factor(Pot)))+
           geom_line()+
           #xlim(c(TP1individ.modeled$TIMESTAMP[1],TP1individ.modeled$TIMESTAMP[1000]))+
           scale_color_manual(values=c("green", "red", "blue", "purple"))+
           labs(title = "Harvest 2",
                y = "A",
                x = "date",
                color = "Pot")
)
####Individual curves Harvest 3####
ImetTP0 <- met[met$TIMESTAMP >= "2018-07-18 00:00:00 MDT" & met$TIMESTAMP < "2018-09-25 00:00:00 MDT",]
ImetTP0$PAR_Den_Avg <- as.numeric(as.character(ImetTP0$PAR_Den_Avg))
ImetTP0$RECORD <- as.numeric(as.character(ImetTP0$RECORD))
dfOut1<-dfOut3

PARi <- ImetTP0$PAR_Den_Avg
TP1individ.modeled <- data.frame()
for (i in 1:length(dfOut1$a)) {
  TP1individ.modeled <- rbind(TP1individ.modeled, data.frame(
    Pot = unique(dfOut1$Pot)[i],
    A = ((dfOut1$a[i])* ImetTP0$PAR_Den_Avg)/((dfOut1$b[i]) + ImetTP0$PAR_Den_Avg ) - dfOut1$c[i],
    TIMESTAMP = ImetTP0$TIMESTAMP,
    PARi = PARi
  ))
}

Harv3_IMOD1 <- TP1individ.modeled

ggplotly(ggplot(data = TP1individ.modeled, aes(y=A,x = TIMESTAMP, color = as.factor(Pot)))+
           geom_line()+
           #xlim(c(TP1individ.modeled$TIMESTAMP[1],TP1individ.modeled$TIMESTAMP[1000]))+
           scale_color_manual(values=c("green", "red", "blue", "purple"))+
           labs(title = "Harvest 3",
                y = "A",
                x = "date",
                color = "Pot")
)
##### Mean curves ####

MeanPlots1 <- plots1 %>% 
  group_by(RoundedPAR) %>% 
  summarise(Photo = mean(Photo))
MeanPlots2 <- plots2 %>% 
  group_by(RoundedPAR) %>% 
  summarise(Photo = mean(Photo))
MeanPlots3 <- plots3 %>% 
  group_by(RoundedPAR) %>% 
  summarise(Photo = mean(Photo))

lst1 <- mget(ls(pattern = '^MeanPlots\\d+$'))

a <- 31
b <- 350 
c <- 0.4
### Making harvest models ####
Dmodel1 <- nls(Photo ~ ((a* PARi)/(b + PARi )) - c,
          data = plots1, start = list(a=a,b=b,c=c))
Dmodel2 <- nls(Photo ~ ((a* PARi)/(b + PARi )) - c,
               data = plots2, start = list(a=a,b=b,c=c))
Dmodel3 <- nls(Photo ~((a* PARi)/(b + PARi )) - c, 
               data = plots3, start = list(a=a,b=b,c=c))

plot(coef(Dmodel1), type = "l")
lines(coef(Dmodel2))
lines(coef(Dmodel3))
moddat1<- data.frame(PARi = seq(0,2000,100))
require(propagate)
pred1 <- predictNLS(Dmodel1, newdata = moddat1, interval = "prediction")
pred2 <- predictNLS(Dmodel2, newdata = moddat1, interval = "prediction")
pred3 <- predictNLS(Dmodel3, newdata = moddat1, interval = "prediction")
##prop.mean is the response of the data or Assimilation values based on range of par these plots compare the models
plot(pred1$summary$Prop.Mean.1, pred2$summary$Prop.Mean.1, xlim = c(-5,30),ylim = c(-5,30))
abline(0,1)
abline(lm(pred2$summary$Prop.Mean.1 ~ pred1$summary$Prop.Mean.1), col = "green")
par(new = T)
plot(pred3$summary$Prop.Mean.1, pred2$summary$Prop.Mean.1, xlim = c(-5,30),ylim = c(-5,30))
abline(0,1)
abline(lm(pred2$summary$Prop.Mean.1 ~ pred3$summary$Prop.Mean.1), col = "blue")
par(new = T)
plot(pred1$summary$Prop.Mean.1, pred3$summary$Prop.Mean.1, xlim = c(-5,30),ylim = c(-5,30))
abline(0,1)
abline(lm(pred3$summary$Prop.Mean.1 ~ pred1$summary$Prop.Mean.1), col = "red")


###this shows the respiration between the three time points

plot(moddat1$PARi, pred2$summary$Prop.Mean.1, type = "l", col = "blue", xlim = c(0,40), ylim = c(-3,1))
lines(moddat1$PARi, pred1$summary$Prop.Mean.1, col = 'green')
lines(moddat1$PARi, pred3$summary$Prop.Mean.1, col = 'red')
abline(h=0)
abline(v=0)


#### Predictions for timepoints #####
#predicts A values based on PAR from met sensors between harvest 1 and 2
PARi <- metTP1$PAR_Den_Avg
metTP1$ predA <- ((coef(Dmodel1)[1]* PARi)/(coef(Dmodel1)[2] + PARi )) - coef(Dmodel1)[3]
metTP1$ scaledpredA <- metTP1$scaler*metTP1$predA
plot(metTP1$ scaledpredA)
metTP1$ predB <- ((coef(Dmodel2)[1]* PARi)/(coef(Dmodel2)[2] + PARi )) - coef(Dmodel2)[3]
metTP1$ scaledpredB <- metTP1$scaler2*metTP1$predB
plot(metTP1$ scaledpredB)
metTP1$predF <- metTP1$scaledpredA+metTP1$scaledpredB
plot(metTP1$predF)

#predicts A values based on PAR from meta sensors between harvest 2 and 3
PARi <- metTP2$PAR_Den_Avg
metTP2$ predA <- ((coef(Dmodel2)[1]* PARi)/(coef(Dmodel2)[2] + PARi )) - coef(Dmodel2)[3]
metTP2$ scaledpredA <- metTP2$scaler*metTP2$predA
plot(metTP2$ scaledpredA)
metTP2$ predB <- ((coef(Dmodel3)[1]* PARi)/(coef(Dmodel3)[2] + PARi )) - coef(Dmodel3)[3]
metTP2$ scaledpredB <- (metTP2$scaler2)*metTP2$predB
plot(metTP2$ scaledpredB)
metTP2$predF <- metTP2$scaledpredA+metTP2$scaledpredB
plot(metTP2$predF, col = "red", type = "l")
lines(metTP1$predF, col = "green")

met <- rbind(metTP1,metTP2)
plot(met$predF, type = "l")
abline(h=0)

#converts umols to grams
met$AbyTP <-(met$predF)*60*15
gramsA <- sum(met$AbyTP)/(1000*44.0095)

###deals with leaf area
LeafArea <- data.frame(index = seq(1,1000))
LeafArea$scalerH1 <- ((1000-LeafArea$index)/(1000))*mean(bmdat1$X, na.rm = TRUE)
LA2<- data.frame(index = seq(1,1000))
LA2$scalerH2 <- ((1-(1000-LeafArea$index)/(1000)))*mean(bmdat2$X, na.rm = TRUE)
LAT <- as.data.frame(LeafArea$scalerH1+LA2$scalerH2)

LeafArea <- data.frame(index = seq(1,1000))
LeafArea$scalerH1 <- ((1000-LeafArea$index)/(1000))*mean(bmdat2$X, na.rm = TRUE)
LA2<- data.frame(index = seq(1,1000))
LA2$scalerH2 <- ((1-(1000-LeafArea$index)/(1000)))*mean(bmdat3$X, na.rm = TRUE)
LAT2 <- as.data.frame(LeafArea$scalerH1+LA2$scalerH2)
LAmod <- rbind(LAT, LAT2)
plot(LAmod$`LeafArea$scalerH1 + LA2$scalerH2`)

mean_LA_Cm<- mean(LAmod$`LeafArea$scalerH1 + LA2$scalerH2`)
meanbmtotals <- mean(bmdat3$DWT, na.rm = TRUE)-mean(bmdat1$DWT, na.rm = TRUE)
##### simulated BM by real BM####
mean_LA_Cm*.0001*gramsA
meanbmtotals

#### individ models graphed####
Harv1_IMOD1$Harvest <- 1

Harv2_IMOD1$Harvest <- 2
Harv3_IMOD1$Harvest <- 3
individ_asym <- rbind(Harv1_IMOD1,Harv2_IMOD1)
individ_asym <- rbind(individ_asym, Harv3_IMOD1)

i <- 1
individ_A <- data.frame()
for (i in 1:length(unique(individ_asym$Pot))) {
  dat <- subset(individ_asym, individ_asym$Pot == unique(individ_asym$Pot)[i])
  dat$AbyTP <-(dat$A)*60*15
  gramsA <- sum(dat$AbyTP)/(1000*44.0095)
  Har <- unique(dat$Harvest)[1]
  individ_A <- rbind(individ_A, data.frame(
    Pot = unique(individ_asym$Pot)[i],
    Grams.Meter = gramsA,
    Harvest = Har)
  )
}
bmtotals <- data.frame()
bmtotals <- rbind(bm1dat1,bm1dat2)                     
bmtotals <- rbind(bmtotals, bm1dat3)
names(bmtotals)[1] <- "Pot"
bmtotals <- merge(individ_A, bmtotals, by = "Pot")
bmtotals$mod_BM.Grams <- bmtotals$Grams.Meter*bmtotals$X

ggplotly(ggplot(data = bmtotals, aes(x = Grams.Meter, y = RGR, color = as.factor(Harvest)))+
  geom_point()+
  scale_color_manual(values=c("green", "red", "blue")))

ggplotly(ggplot(data = bmtotals, aes(x = mod_BM.Grams, y = RGR, color = as.factor(Harvest)))+
           geom_point()+
           scale_color_manual(values=c("green", "red", "blue")))


#### LA as a function of BM ####
a <- 0.00000915637
b <- 0.01337061
c <- -1.261032 
d <- 2
bmdat1$harvest <- 1
bmdat2$harvest <- 2
bmdat3$harvest <- 3
bmdatT <- rbind(bmdat1,bmdat2,bmdat3)
BM.fun.T <- nls(DWT~a*(leaf.area)^d+b*(leaf.area)+c,
                data = bmdatT, start = list(a=a,b=b,c=c, d=d), control = 
                  nls.control(maxiter = 500))
bmdatT$predDWT<-coef(BM.fun.T)[1]*(bmdatT$leaf.area)^2+(coef(BM.fun.T)[2]*(bmdatT$leaf.area))+coef(BM.fun.T)[3]
bmdatT$residual <- bmdatT$DWT-bmdatT$predDWT
#### for above ground only

bmdatT <- rbind(bmdat1,bmdat2,bmdat3)
BM.fun.T <- nls(AG~a*(leaf.area)^d+b*(leaf.area)+c,
                data = bmdatT, start = list(a=a,b=b,c=c, d=d), control = 
                  nls.control(maxiter = 500))
bmdatT$predAG<-coef(BM.fun.T)[1]*(bmdatT$leaf.area)^(coef(BM.fun.T)[4])+(coef(BM.fun.T)[2]*(bmdatT$leaf.area))+coef(BM.fun.T)[3]
bmdatT$residual <- bmdatT$AG-bmdatT$predAG

ggplot(data = bmdatT, aes(x = leaf.area, y = residual, color = as.factor(harvest)))+
  geom_point()
ggplot(data = bmdatT, aes(x = leaf.area, y = AG, color = as.factor(harvest)))+
  geom_point()+
  geom_line(data = bmdatT, aes(x = leaf.area, y = predAG, col = "Predicted Harvests 1 & 2"))

#### for below ground only

bmdatT <- rbind(bmdat1,bmdat2,bmdat3)
BM.fun.T <- nls(DWT~a*(leaf.area)^d+b*(leaf.area)+c,
                data = bmdatT, start = list(a=a,b=b,c=c, d=d), control = 
                  nls.control(maxiter = 500))
bmdatT$predAG<-coef(BM.fun.T)[1]*(bmdatT$leaf.area)^(coef(BM.fun.T)[4])+(coef(BM.fun.T)[2]*(bmdatT$leaf.area))+coef(BM.fun.T)[3]
bmdatT$residual <- bmdatT$AG-bmdatT$predAG

ggplot(data = bmdatT, aes(x = leaf.area, y = residual, color = as.factor(harvest)))+
  geom_point()
ggplotly(ggplot(data = bmdatT, aes(x = leaf.area, y = AG, color = as.factor(harvest)))+
  geom_point()+
  geom_line(data = bmdatT, aes(x = leaf.area, y = predAG, col = "Predicted Harvests 1 & 2")))


##using a linear model
BM.fun.1_2 <- lm(log(bmdatT$DWT)~log(bmdatT$leaf.area))
plot(BM.fun.1_2)


BM.fun.1 <- nls(DWT~a*(leaf.area)^2+b*(leaf.area)+c,
                  data = bmdat1, start = list(a=a,b=b,c=c))
bmdat1$predDWT<-coef(BM.fun.1)[1]*(bmdat1$leaf.area)^2+(coef(BM.fun.1)[2]*(bmdat1$leaf.area))+coef(BM.fun.1)[3]
bmdat1$residual <- bmdat1$DWT-bmdat1$predDWT

BM.fun.2 <- nls(DWT~a*(leaf.area)^2+b*(leaf.area)+c,
                data = bmdat2, start = list(a=a,b=b,c=c))
bmdat2$predDWT<-coef(BM.fun.2)[1]*(bmdat2$leaf.area)^2+(coef(BM.fun.2)[2]*(bmdat2$leaf.area))+coef(BM.fun.2)[3]
bmdat2$residual <- bmdat2$DWT-bmdat2$predDWT

BM.fun.3 <- nls(DWT~a*(leaf.area)^2+b*(leaf.area)+c,
                data = bmdat3, start = list(a=a,b=b,c=c))
bmdat3$predDWT<-coef(BM.fun.3)[1]*(bmdat3$leaf.area)^2+(coef(BM.fun.3)[2]*(bmdat3$leaf.area))+coef(BM.fun.3)[3]
bmdat3$residual <- bmdat3$DWT-bmdat3$predDWT

##making model for just harvest 1 and 2
bmdatH1_H2 <- rbind(bmdat1, bmdat2)

BM.fun.H1_H2 <- nls(DWT~a*(leaf.area)^d+b*(leaf.area)+c,
                data = bmdatH1_H2, start = list(a=a,b=b,c=c, d=d))
bmdatH1_H2$predDWT<-coef(BM.fun.H1_H2)[1]*(bmdatH1_H2$leaf.area)^coef(BM.fun.H1_H2)[1]+(coef(BM.fun.H1_H2)[2]*(bmdatH1_H2$leaf.area))+coef(BM.fun.H1_H2)[3]
bmdatH1_H2$residual <- bmdatH1_H2$DWT-bmdatH1_H2$predDWT

##making model for just harvest 2 and 3
bmdatH3_H2 <- rbind(bmdat2, bmdat3)

BM.fun.H3_H2 <- nls(DWT~a*(leaf.area)^d+b*(leaf.area)+c,
                    data = bmdatH3_H2, start = list(a=a,b=b,c=c))
bmdatH3_H2$predDWT<-coef(BM.fun.H3_H2)[1]*(bmdatH3_H2$leaf.area)^2+(coef(BM.fun.H3_H2)[2]*(bmdatH3_H2$leaf.area))+coef(BM.fun.H3_H2)[3]
bmdatH3_H2$residual <- bmdatH3_H2$DWT-bmdatH3_H2$predDWT

##plots this model that only has harvest 1 and 3 for the model
ggplot(data = bmdatH3_H2, aes(x = leaf.area, y = residual, color = as.factor(harvest)))+
           geom_point()
ggplot(data = bmdatH3_H2, aes(x = leaf.area, y = DWT, color = as.factor(harvest)))+
           geom_point()+
           geom_line(data = bmdatH3_H2, aes(x = leaf.area, y = predDWT, col = "Predicted Harvests 1 & 2"))

##plots this model that only has harvest 2 and 3 for the model
ggplot(data = bmdatH1_H2, aes(x = leaf.area, y = residual, color = as.factor(harvest)))+
           geom_point()
ggplot(data = bmdatH1_H2, aes(x = leaf.area, y = DWT, color = as.factor(harvest)))+
           geom_point()+
           geom_line(data = bmdatH1_H2, aes(x = leaf.area, y = predDWT, col = "Predicted Harvests 1 & 2"))



##plots for bm to leaf area

ggplotly(ggplot(dat = bmdat3, aes(x = leaf.area, y = DWT, col = "Harvest_3"))+
           geom_point()+
           geom_point(aes(x = bmdat2$leaf.area, y = bmdat2$DWT, col = "Harvest_2"))+
           geom_point(data = bmdat1, aes(x = leaf.area, y = DWT, col = "Harvest_1"))+
           labs(x = "Leaf Area (cm^2)",
                y = "Dry Weight (g)")+
           geom_line(data = bmdatT, aes(x = leaf.area, y = predDWT, col = "Predicted over all three Harvests"))+
           geom_line(data = bmdat1, aes(x = leaf.area, y = predDWT, col = "Predicted Harvest_1"))+
           geom_line(data = bmdat2, aes(x = leaf.area, y = predDWT, col = "predicted Harvest_2"))+
           geom_line(data = bmdat3, aes(x = leaf.area, y = predDWT, col = "predicted Harvest_2"))
           )
ggplotly(ggplot(data = bmdatT, aes(x = AG, y = predDWT, color = as.factor(harvest)))+
  geom_point())
ggplotly(ggplot(data = bmdatT, aes(x = leaf.area, y = residual, col = as.factor(harvest)))+
  geom_point()+
  ggtitle("Residuals for Whole model"))
p1 <- ggplot(data = bmdat1, aes(x = leaf.area, y = residual, col = Weed))+
  geom_point()+
  ggtitle("Harvest 1")+
  theme(legend.position = "none")
p2 <- ggplot(data = bmdat2, aes(x = leaf.area, y = residual, col = Weed))+
  geom_point()+
  ggtitle("Harvest 2")+
  theme(legend.position = "none")
p3 <- ggplot(data = bmdat3, aes(x = leaf.area, y = residual, col = Weed))+
  geom_point()+
  ggtitle("Harvest 3")

gridExtra::grid.arrange(p1,p2,p3, nrow = 1)
#### GM ~ sum of modeled Bm ####

sumsh2 <- filter(Harv2_IMOD1, Harv2_IMOD1$Pot == unique(Harv2_IMOD1$Pot)[2])
scaler_constant <- ((60*15*42.0095)/1000000)
sumsh2$A.grams <- sumsh2$A*scaler_constant####converts umols/m^2s to grams/15sec


## scalers for leaf area
sumsh2$scaler.index <- (1:length(sumsh2$A))
sumsh2$scaled.leaf.area.pre <- sumsh2$scaler.index/length(sumsh2$A)
sumsh2$scaled.leaf.area.LA <- (bm1dat2[1,]$leaf.area)
sumsh2$scaled.leaf.area <- (sumsh2$scaled.leaf.area.LA)*(sumsh2$scaled.leaf.area.pre)
sumsh2$scaled.leaf.area <- sumsh2$scaled.leaf.area/10000
sumsh2$preA <- sumsh2$scaled.leaf.area*sumsh2$A.grams
mod.bm.grams.h2 <- (sum(sumsh2$preA))
##leaf area calc

mod.bm.grams.h2-mean(bmdat1$DWT, na.rm = TRUE)
mean(bmdat2$DWT, na.rm = TRUE)-mean(bmdat1$DWT, na.rm = TRUE)
bm1dat2$bucket

bm1dat1$DAP <- 36 ## days after planting column
bm1dat2$DAP <- 52 ## days after planting column
bm1dat3$DAP <- 69 ## days after planting column

LAbm <- rbind(bm1dat1,bm1dat2,bm1dat3)
ggplot(data = LAbm, aes(x = DAP, y = leaf.area))+
  geom_point()
LAm1 <- lm(leaf.area~ DAP, data = LAbm)
summary(LAm1)
plot(x = LAbm$DAP, y = LAbm$leaf.area )
lines(LAbm$DAP, coef(LAm1)[1]+coef(LAm1)[2]*LAbm$DAP)
