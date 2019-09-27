
rm(list= ls())

load("data/Return_sweep.Rda")

#RS<- subset(RS, sub<4)
RS<- subset(RS, cond==1 | cond==3)
RS<- subset(RS, !is.na(RS$prevChar))


# Parameters:
VA<- 0.2953               # visual angle per letter in the Experiment
sys_err_mu_X0<- -0.271    # intercept of systematic error (in deg)
sys_err_mu_X<- -0.058     # slope of systematic error (in deg)
rand_sigmaX0<- 0.20       # intercept of random error SD
rand_sigmaX<- 0.05        # slope of random error SD
pCorr_mu<- 2             # Mean of CDF for prob. of making a corrective saccade (in letters)
pCorr_sigma<- 7         # SD of CDF for prob. of making a corrective saccade  (in letters)



RS$Model<- NA
RS$LPM<- NA
RS$M_UND<- NA

for(i in 1:nrow(RS)){
  dist<- RS$prevChar[i]*VA    #intended saccade distance (left margin) 
  sys_error<- sys_err_mu_X0+ dist*sys_err_mu_X  # systematic return sweep error
  rand_error<- rnorm(n = 1, mean = 0, sd = rand_sigmaX0+ rand_sigmaX*dist)
  
  sacc_len<- dist + sys_error + rand_error
  RS$Model[i]<- sacc_len
  RS$LPM[i]<- dist- sacc_len
  
  # Probability of making a corrective saccade
  probCorrSacc<- pnorm(q = RS$LPM[i]/VA, mean =  pCorr_mu, sd = pCorr_sigma)
  RandDraw<- runif(1)

  if(RandDraw<= probCorrSacc){
    RS$M_UND[i]<- 1
  }else{
    RS$M_UND[i]<- 0
  }

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

sd<-(RS$LPM- RS$LandStartVA)^2
sqrt(sum(sd)/length(sd))




library(reshape)
DesCorr<- melt(RS, id=c('sub', 'item', 'cond', 'LandStartLet'), 
                measure=c("undersweep_prob", 'M_UND'), na.rm=TRUE)
mCorr<- cast(DesCorr, item ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

plot(mCorr$item, mCorr$undersweep_prob_M, col= 'steelblue', type= 'l', xlab= 'Experimental item',
     ylab= 'p(Corrective saccade)', cex.axis= 1.2, cex.lab= 1.4, lwd= 1.5)
lines(mCorr$item, mCorr$M_UND_M, col= 'darkred', lwd= 1.5)

mean(RS$undersweep_prob); sd(RS$undersweep_prob)
mean(RS$M_UND); sd(RS$M_UND)
