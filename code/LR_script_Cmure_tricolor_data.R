## Bayesian Estimate of PS paramters from  LR curves ###

###   Chris Mure Amaranthus tricolor #####

###setwd("/Users/bridgerhuhn/Documents/Pleban Param mod")   ### Change 4 server

### package for Bayesian Sampleing
library("rjags") ### Change 4 server
###  Bayesian LR model
source("LR_model.R")


#### Set up for rjags #########
parameters = c("Amax", "Rd",
               "phiCO2","tau") ### pars to be monitored
adaptSteps = 1000             # Number of steps to "tune" the samplers.
burnInSteps = 1000            # Number of steps to "burn-in" the samplers.
nChains = 4                   # Number of chains to run.
DICsteps= 2000                # Number of steps of sample DIC
numSavedSteps= 1000        # Total number of steps in chains to save.
thinSteps=50                   # Number of steps to "thin" (1=keep every step).
nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.datalist1<-list(N=N, An=A[,1], Ca=CP[,1], g=g[,1], Jf=Jf[,1], O=O[,1])


######  load data
library(data.table)
## data from two measuremnts days (7_28_16 and 8_04_16) compiled into CM_tricolor_data_clean_08_12_16.txt
LR_1 <- read.delim("CMure_tricolor_clean_compiled_data_093016_clean.txt",sep="\t", header=TRUE)

### data 7_28-16 Tri Base REd curve has min value of -13 and two poor data points
## consider redoing that curve


names(LR_1)

### set up Identifter for Posterior outputs
IGT<-unique(LR_1[,2:6][c("Date","Plant", "Leaf_Section", "Color", "ID")]) 
Plant<-IGT$Plant; Leaf_Section<-IGT$Leaf_Section;Color<-IGT$Color; IDs<-IGT$ID; N<-length(IDs)
Date<-IGT$Date

###  FOR Plotting Raw data 
### PARi rounded to produce mean values for Plant types
LR_1$PARi_grp<-(round(LR_1$PARi, -1))
###  Plant group, combination of Species, Sectino and color
LR_1$Plant_grp<-paste(LR_1$Plant, LR_1$Leaf_Section,LR_1$Color, sep='_')
#### mean values based on plant/leaf type and PARi_group
LR_means<-aggregate(Photo ~ PARi_grp * Plant_grp, data=LR_1, FUN=mean)
LR_means$SD<-aggregate(Photo ~ PARi_grp * Plant_grp, data=LR_1, FUN=sd)[,3]


datalist<-vector("list", N)
for(j in 1:N){
  datalist[[j]]<-list(N=length(LR_1[LR_1$ID==IDs[j],]$PARi),
                         An=c(LR_1[LR_1$ID==IDs[j],]$Photo), Q=c(LR_1[LR_1$ID==IDs[j],]$PARi))
}

# 1 curve
datalist[[1]]

### establishing all models & doing burn in
models<-vector("list", N)
for(j in 1:N){
models[[j]] <- jags.model(textConnection(LR_model), 
                     data = datalist[[j]], n.chains=nChains , n.adapt=adaptSteps)
      update(models[[j]], burnInSteps)
}


### sampling all models and calc sigma and converting to martrix form 
mcmcsamples<-vector("list", nChains); 
for(j in 1:N){
  mcmcsamples[[j]] <- coda.samples(models[[j]],
                                   variable.names=parameters,
                                   n.iter=nPerChain , thin=thinSteps )
}

mcmcChain<-vector("list", N); sigma<-vector("list", N)
  for(i in 1:N){
    mcmcChain[[i]] <- as.matrix( mcmcsamples[[i]])
    sigma[[i]] =1  / sqrt( mcmcChain[[i]][, "tau" ] )
    mcmcChain[[i]] = as.data.frame(cbind( mcmcChain[[i]], sigma[[i]] ))
  }

##### pull out medain values estimates and write to file
meds<-matrix(, nrow = N, ncol = 5)
for(j in 1:N){
meds[j,]<- apply(mcmcChain[[j]], 2, median)
}
Medians<-cbind(IGT[,1:5], meds)
colnames(Medians)<-c( "Date", "Plant", "Leaf_Section", "Color" ,"ID","Amax", "Rd", "phiCO2", "tau", "sigma")
### write table for Median values

write.table(Medians, "medians_LR_trait_estimates_tricolor_alldata_09_26_16.txt", sep="\t", col.name=TRUE,row.names=FALSE)

######  to do , add HDI and add to data set datatalbe plus add plots and get IDs linked to leaf type and regin

if (Medians$Plant=="Tri_Tri" && Medians$Color=="Red") {Medians$newvar=1} else {Medians$newvar=0}

### add IGT names to mcmcChain list
for(i in 1:N){
  mcmcChain[[i]] <-as.data.frame(cbind( mcmcChain[[i]], IGT[i,]  ))
}

names(mcmcChain) <- paste(IGT[,1],"_", IGT[,2],"_",IGT[,3], sep = "") 




### plotting 
### R500 phiCO2
plot(density(mcmcChain[[1]][,"phiCO2"]),  lwd=3)
lines(density(mcmcChain[[2]][,"phiCO2"]),  lwd=3, col=2)
lines(density(mcmcChain[[2]][,"phiCO2"]),  lwd=3, col=3)

plot(density(mcmcChain[[2]][,"Amax"]),  lwd=3, xlim=c(0,70), ylim=c(0,.5))
lines(density(mcmcChain[[1]][,"Amax"]),  lwd=3, col=2)
lines(density(mcmcChain[[9]][,"Amax"]),  lwd=3, col=2)
lines(density(mcmcChain[[11]][,"Amax"]),  lwd=3, col=2)

plot(density(mcmcChain[[2]][,"phiCO2"]),  lwd=3)
lines(density(mcmcChain[[1]][,"phiCO2"]),  lwd=3, col=2)
lines(density(mcmcChain[[9]][,"phiCO2"]),  lwd=3, col=2)
lines(density(mcmcChain[[11]][,"phiCO2"]),  lwd=3, col=2)





library("ggplot2")

p <- ggplot(LR_1, aes(PARi,Photo)) 
p + geom_point(aes(colour = factor(Plant))) 



p <- ggplot(LR_means, aes(PARi_grp,Photo)) 
p1<- p + geom_line(aes(colour = factor(Plant_grp)))+ geom_point(aes(colour = factor(Plant_grp))) +
  scale_color_manual(values = c("firebrick", "firebrick1", "deepskyblue","deepskyblue2","deepskyblue3",
  "gold1", "gold2", "gold3")) 
 p1 + geom_errorbar(aes(ymin=Photo-SD, ymax=Photo+SD), width=30) 



### view plot of each ID
p <- ggplot(LR_1, aes(PARi,Photo)) 
p + geom_point(aes(colour = factor(ID))) + geom_line(aes(colour = factor(ID))) 







