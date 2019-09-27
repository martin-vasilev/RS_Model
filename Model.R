
rm(list= ls())

load("data/Return_sweep.Rda")

#RS<- subset(RS, sub<4)
RS<- subset(RS, cond==1 | cond==3)

# Parameters:
VA<- 0.2953               # visual angle per letter in the Experiment
sys_err_mu_X0<- -0.271    # intercept of systematic error (in deg)
sys_err_mu_X<- -0.058     # slope of systematic error (in deg)
rand_sigmaX0<- 0.20       # intercept of random error SD
rand_sigmaX<- 0.05        # slope of random error SD
pCorr_mu<- 15             # Mean of CDF for prob. of making a corrective saccade
pCorr_sigma<- 10          # SD of CDF for prob. of making a corrective saccade



RS$Model<- NA
RS$LPM<- NA
RS$M_undersweep<- NA

for(i in 1:nrow(RS)){
  dist<- RS$prevChar[i]*VA    #intended saccade distance (left margin) 
  sys_error<- sys_err_mu_X0+ dist*sys_err_mu_X  # systematic return sweep error
  rand_error<- rnorm(n = 1, mean = 0, sd = rand_sigmaX0+ rand_sigmaX*dist)
  
  sacc_len<- dist + sys_error + rand_error
  RS$Model[i]<- sacc_len
  RS$LPM[i]<- dist- sacc_len
}


mean(RS$launchDistVA, na.rm=T); sd(RS$launchDistVA, na.rm=T)
mean(RS$Model, na.rm=T); sd(RS$Model, na.rm=T)

mean(RS$LandStartVA); sd(RS$LandStartVA)
mean(RS$LPM, na.rm= T); sd(RS$LPM, na.rm= T)

hist(RS$launchDistVA, breaks= 30, col= "steelblue")
hist(RS$Model, breaks= 30, col= "darkred", add=T)

library(grDevices)

hist(RS$LandStartVA, breaks= 30, col= adjustcolor( "steelblue", alpha.f = 1),
     xlab= 'Return sweep landing position (deg)', freq= F)
hist(RS$LPM, breaks= 30, col= adjustcolor( "darkred", alpha.f = 1), add=T, freq = F)
legend(x = 10, y = 0.25, legend = c('Data', "Model"), fill= c('steelblue', 'darkred'))

sd<-(RS$LPM[which(!is.na(RS$LPM))]- RS$LandStartVA[which(!is.na(RS$LPM))])^2
sqrt(sum(sd)/length(sd))


