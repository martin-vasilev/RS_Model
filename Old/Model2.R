
rm(list= ls())

load("data/Font_size.Rda")

#RS<- subset(RS, cond==1 | cond==3)
RS<- subset(RS, !is.na(RS$prevChar))

RS<- subset(RS, prevVA>0)  
  
# Parameters:
#VA<- 0.2953               # visual angle per letter in the Experiment
RS_target<- 1.5            # Return sweep saccade target (in deg from line start)
sys_err_mu_X0<- 0.07712    # intercept of systematic error (in deg)
sys_err_mu_X<- -0.07710     # slope of systematic error (in deg)
rand_sigmaX0<- 0.11840 #0.2      # intercept of random error SD
rand_sigmaX<- 0.08248#0.05      # slope of random error SD
pCorr_mu<- 2             # Mean of CDF for prob. of making a corrective saccade (in letters)
pCorr_sigma<- 7         # SD of CDF for prob. of making a corrective saccade  (in letters)
#Len_corr_sacc_sigma<- 0.8     # SD of the length of corrective saccades (in deg)


RS$M_launchDistVA<- NA
RS$M_landStartVA<-NA
RS$M_landStartLet<- NA
RS$M_UND<- NA
RS$M_next_LP<- NA
RS$M_next_sacc_len<- NA

for(i in 1:nrow(RS)){
  left_margin<- RS$prevVA[i] 
  dist<- RS$prevVA[i] - RS_target*RS$VA[i]   # intended saccade distance
  sys_error<- sys_err_mu_X0+ dist*sys_err_mu_X  # systematic return sweep error
  rand_error<- rnorm(n = 1, mean = 0, sd = rand_sigmaX0+ rand_sigmaX*dist)
  
  RS$M_launchDistVA[i]<- dist + sys_error + rand_error
  RS$M_landStartVA[i]<- left_margin- RS$M_launchDistVA[i]
  RS$M_landStartLet[i]<- ceiling(RS$M_landStartVA[i]/RS$VA[i])
  
  # Probability of making a corrective saccade
  probCorrSacc<- pnorm(q = RS$M_landStartLet[i]- RS_target, mean =  pCorr_mu, sd = pCorr_sigma)
  #probCorrSacc<- pnorm(q = RS$LPM[i]/VA, mean =  pCorr_mu, sd = pCorr_sigma)
  RandDraw<- runif(1)

  if(RandDraw<= probCorrSacc){
    RS$M_UND[i]<- 1
  }else{
    RS$M_UND[i]<- 0
  }
  
  # length of corrective saccade
  if(RS$M_UND[i]== 1){
    
   #next landing position (in deg):
   # RS$M_next_LP[i]<- rnorm(n = 1, mean = 0, sd = Len_corr_sacc_sigma)
   # RS$M_next_sacc_len[i]<- RS$M_next_LP[i]- RS$LPM[i] # next sacc length (negative mean corr)
    next_sacc_dist<- RS$M_landStartVA[i]- RS_target*RS$VA[i] 
    sys_error_next<- sys_err_mu_X0+ next_sacc_dist*sys_err_mu_X  # systematic return sweep error
    rand_error_next<- rnorm(n = 1, mean = 0, sd = rand_sigmaX0+ rand_sigmaX*next_sacc_dist)
    
    RS$M_next_sacc_len[i]<- next_sacc_dist + sys_error_next + rand_error_next
    RS$M_next_LP[i]<- (RS$M_landStartVA[i]- RS$M_next_sacc_len[i])/RS$VA[i]
  }

}

save(RS, file= 'results/RS2.Rda')

a<- RS[, c('prevVA', 'launchDistVA', 'M_launchDistVA', 'LandStartVA', 'M_landStartVA')]
#a$diff_D<- a$prevVA- a$launchDistVA
#a$diff_land<- a$LandStartVA- a$diff_D

#a$diff_M<- a$prevVA- a$M_launchDistVA
#a$diff_land_M<- a$M_landStartVA- a$diff_M

a$diff_Dist<- a$launchDistVA- a$M_launchDistVA
a$diff_Land<- a$M_landStartVA- a$LandStartVA
a$com_diff<- abs(a$diff_Dist)- abs(a$diff_Land)

#write.csv(a, 'test.csv')




#plot(RS$launchDistVA, RS$M_launchDistVA)
#cor(RS$launchDistVA, RS$M_launchDistVA)

#plot(RS$LandStartVA, RS$M_landStartVA)
#cor(RS$LandStartLet, RS$M_landStartLet)

mean(RS$launchDistVA, na.rm=T); sd(RS$launchDistVA, na.rm=T)
mean(RS$M_launchDistVA, na.rm=T); sd(RS$M_launchDistVA, na.rm=T)

mean(RS$LandStartVA, na.rm= T); sd(RS$LandStartVA, na.rm= T)
mean(RS$M_landStartVA, na.rm= T); sd(RS$M_landStartVA, na.rm= T)

hist(RS$launchDistVA, breaks= 30, col= "steelblue")
hist(RS$M_launchDistVA, breaks= 30, col= "darkred", add=T)

library(grDevices)

hist(RS$LandStartVA, breaks= 30, col= adjustcolor( "steelblue", alpha.f = 1),
     xlab= 'Return sweep landing position (deg)', freq= F)
hist(RS$M_landStartVA, breaks= 30, col= adjustcolor( "darkred", alpha.f = 0.5), add=T, freq = F)
legend(x = 10, y = 0.25, legend = c('Data', "Model"), fill= c('steelblue', 'darkred'))

sd<-(RS$M_landStartVA- RS$LandStartVA)^2
sqrt(sum(sd)/length(sd))



###################################
# corrective saccade probability  #
###################################

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

length(which(RS$undersweep_prob== RS$M_UND))/nrow(RS)

########################################
# corrective saccade length & land pos #
########################################

USP<- subset(RS, undersweep_prob==1)
MUSP<- subset(RS, M_UND==1)

mean(USP$next_land_pos); sd(USP$next_land_pos)
mean(MUSP$M_next_LP, na.rm= T); sd(MUSP$M_next_LP, na.rm= T)

hist(USP$next_land_pos, breaks= 30, col= adjustcolor( "steelblue", alpha.f = 1),
     xlab= 'Corrective saccade landing position (deg)', freq= F, main= 'Corrective saccade landing position')
hist(MUSP$M_next_LP, breaks= 30, col= adjustcolor( "darkred", alpha.f = 0.5), add=T, freq = F)
legend(x = 7, y = 0.5, legend = c('Data', "Model"), fill= c('steelblue', adjustcolor( "darkred", alpha.f = 0.5)))



mean(USP$next_sacc_deg); sd(USP$next_sacc_deg, na.rm= T)
mean(MUSP$M_next_sacc_len, na.rm=T); sd(MUSP$M_next_sacc_len, na.rm= T)

hist(USP$next_sacc_deg, breaks= 30, col= adjustcolor( "steelblue", alpha.f = 1),
     xlab= 'Corrective saccade length (deg)', freq= F, main= 'Corrective saccade length')
hist(MUSP$M_next_sacc_len, breaks= 30, col= adjustcolor( "darkred", alpha.f = 0.5), add=T, freq = F)
legend(x = 7, y = 0.5, legend = c('Data', "Model"), fill= c('steelblue', adjustcolor("darkred", alpha.f = 0.5)))


##################################################
#          Accurate next saccades                #
##################################################
#ACC<- subset(RS, undersweep_prob==0)

#hist(ACC$next_sacc_deg, breaks= 30, col= adjustcolor( "steelblue", alpha.f = 1),
#     xlab= 'saccade length (deg)', freq= F, main= 'Accurate saccade length ')

