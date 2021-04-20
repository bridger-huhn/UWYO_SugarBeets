
###############################
###      Bayesian Model    ####
### For light response curve ####
####
######  & no temp dependency ###
###############################
LR_model <- "model {
for (i in 1:N){
An[i] ~ dnorm( mu.A[i] , tau )
#######  light responce model based on Ogren PLant
mu.A[i] <-  ((Q[i]*phiCO2 + Amax - sqrt((phiCO2*Q[i] +Amax)^2-4*Q[i]*phiCO2*thetaJ*Amax))/(2*thetaJ))-Rd
}
Amax ~ dnorm(35, 0.0025)
Rd ~ dnorm(3, 0.111)
phiCO2 ~ dnorm(0.044, 40000)
thetaJ ~ dunif(.5,1)
tau ~ dgamma(.001 , .001)
}
" 