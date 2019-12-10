
rm(list= ls())

# Font size:
load("data/Font_size.Rda")
load("data/L2_word_pos_FS.Rda")
RS<- subset(RS, !is.na(RS$prevChar))
RS<- subset(RS, prevVA>0)


# Bold study (OZ)
# load("data/Bold_OZ.Rda")
# RS<- RSb; rm(RSb)
# RS<- subset(RS, Condition=="Normal")
# RS<- subset(RS, !is.na(RS$prevChar))
# RS<- subset(RS, prevVA>0)
# load("data/L2_word_pos_FS.Rda")


# # Comprehension data
# load("data/Comprehension_data.Rda")
# RS<- RSc; rm(RSc)
# RS<- subset(RS, !is.na(RS$prevChar))
# RS<- subset(RS, prevVA>0)
# load("data/L2_word_pos_FS.Rda")



#------------------------------------------------------------------------------------------------------  
# Model Parameters:
#------------------------------------------------------------------------------------------------------
RS_target<- 1.5               # Return sweep saccade target (in deg from line start)
S_X0<- 0.07712                # intercept of systematic error (in deg)
S_X<- -0.07710                # slope of systematic error (in deg)
R_X0<- 0.11840                # intercept of random error SD
R_X<- 0.08248                 # slope of random error SD
pCorr_mu<- 2                  # Mean of CDF for prob. of making a corrective saccade (in letters)
pCorr_sigma<- 7               # SD of CDF for prob. of making a corrective saccade  (in letters)
#------------------------------------------------------------------------------------------------------


# variables for saving results from model:
RS$M_launchDistVA<- NA        # return-sweep saccade length in deg
RS$M_landStartVA<-NA          # return-sweep landing position in deg (relative to line start)
RS$M_landStartLet<- NA        # return-sweep landing position in letters (relative to line start)
RS$M_UND<- NA                 # undersweep probability
RS$M_next_LP<- NA             # landing position in deg of the next saccade following return-sweep
RS$M_next_sacc_len<- NA       # saccade length in deg of of the next saccade following return-sweep


#------------------------------------------------------------------------------------------------------
# Model simulations:
#------------------------------------------------------------------------------------------------------

for(i in 1:nrow(RS)){
  L<- RS$prevVA[i] - RS_target*RS$VA[i]   # intended saccade distance
  sys_error<- S_X0+ L*S_X  # systematic return sweep error
  rand_error<- rnorm(n = 1, mean = 0, sd = R_X0+ R_X*L)
  
  RS$M_launchDistVA[i]<- L + sys_error + rand_error
  RS$M_landStartVA[i]<- L- RS$M_launchDistVA[i]
  RS$M_landStartLet[i]<- ceiling(RS$M_landStartVA[i]/RS$VA[i])
  gain<- RS$M_launchDistVA[i]/ L # return-sweep saccade gain
  
  
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
    sys_error_next<- S_X0+ next_sacc_dist*S_X  # systematic return sweep error
    rand_error_next<- rnorm(n = 1, mean = 0, sd = R_X0+ R_X*next_sacc_dist)
    
    RS$M_next_sacc_len[i]<- next_sacc_dist + sys_error_next + rand_error_next
    RS$M_next_LP[i]<- (RS$M_landStartVA[i]- RS$M_next_sacc_len[i])/RS$VA[i]
  }else{ # PROGRESSIVE SACCADE
    # find out current word position:
    curr_sent<- RS$item[i] # current item
    word_bounds<- unname(unlist(word_pos[toString(curr_sent)])) # empty spaces (for word position)
    word_bounds<- c(0, word_bounds) # 0 is start of 1st word (technically, empty space before 1st letter)
    
    # where did we land?
    diff_pos<- abs(RS$M_landStartLet[i]- word_bounds)
    min_diff_pos<- which(diff_pos== min(diff_pos))
    
    # where is the next word?
    W1_start<- word_bounds[min_diff_pos+1]+1 # start of next word (+1 to get past empty space)
    W1_end<- word_bounds[min_diff_pos+2]-1 # -1 to get the last character of word
    W1_OVP<- W1_start + (W1_end- W1_start)/2
   
    next_sacc_dist<- RS$M_landStartVA[i]+ (W1_OVP+0.5)*RS$VA[i]
    sys_error_next<- S_X0+ next_sacc_dist*S_X  # systematic return sweep error
    rand_error_next<- rnorm(n = 1, mean = 0, sd = R_X0+ R_X*next_sacc_dist)
    
    RS$M_next_sacc_len[i]<- next_sacc_dist + sys_error_next + rand_error_next
    RS$M_next_LP[i]<- (RS$M_next_sacc_len[i]-RS$M_landStartVA[i])/RS$VA[i]
    
  }

}

save(RS, file= 'results/RS_v1.Rda')

#-------------------------------------------------------------------------------------------------------------



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



mean(USP$next_sacc_deg); sd(USP$next_sacc_deg)
mean(MUSP$M_next_sacc_len, na.rm=T); sd(MUSP$M_next_sacc_len, na.rm=T)

hist(USP$next_sacc_deg, breaks= 30, col= adjustcolor( "steelblue", alpha.f = 1),
     xlab= 'Corrective saccade length (deg)', freq= F, main= 'Corrective saccade length')
hist(MUSP$M_next_sacc_len, breaks= 30, col= adjustcolor( "darkred", alpha.f = 0.5), add=T, freq = F)
legend(x = 7, y = 0.5, legend = c('Data', "Model"), fill= c('steelblue', adjustcolor( "darkred", alpha.f = 0.5)))


##################################################
#          Accurate next saccades                #
##################################################
ACC<- subset(RS, undersweep_prob==0)

mean(ACC$next_land_let, na.rm= T); sd(ACC$next_land_let, na.rm= T)
mean(ACC$M_next_LP, na.rm= T); sd(ACC$M_next_LP, na.rm= T)


hist(ACC$next_land_let, breaks= 30, col= adjustcolor( "steelblue", alpha.f = 1),
     xlab= 'saccade length (deg)', freq= F, main= 'Accurate saccade length ')

hist(ACC$M_next_LP, breaks= 30, col= adjustcolor( "darkred", alpha.f = 1),
     xlab= 'saccade length (deg)', freq= F, main= 'Accurate saccade length ', add=T)

