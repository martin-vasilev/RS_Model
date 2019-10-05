
get_pred <- function(parms,data) {
  
  M_UND<- NULL
  
  VA<- 0.2953      # visual angle per letter
  sys_err_mu_X0<- -0.271 #parms[1]#    # intercept of systematic error (in deg)
  sys_err_mu_X<- -0.058 #parms[2] #     # slope of systematic error (in deg)
  
  rand_sigmaX0<- 0.20#parms[1] # intercept of random error SD
  rand_sigmaX<- 0.05#parms[1] # slope of random error SD
  
  pCorr_mu<- parms[1]             # Mean of CDF for prob. of making a corrective saccade (in letters)
  pCorr_sigma<- parms[2]
  
  for(i in 1:nrow(data)){
    dist<- data[i,2]*VA    #intended saccade distance (left margin) 
    sys_error<- sys_err_mu_X0+ dist*sys_err_mu_X
    rand_error<- rnorm(n = nrow(data), mean = 0, sd = rand_sigmaX0+ rand_sigmaX*dist)
    
    sacc_len<- dist + sys_error + rand_error
    land_pos<- dist- sacc_len
    
    predict <- land_pos
    #predict <- sacc_len
    
    # Probability of making a corrective saccade
    probCorrSacc<- pnorm(q = land_pos/VA, mean =  pCorr_mu, sd = pCorr_sigma)
    RandDraw<- runif(1)
    
    if(RandDraw<= probCorrSacc){
      M_UND[i]<- 1
    }else{
      M_UND[i]<- 0
    }
    
  }

  #return(predict)
  return(M_UND)
}