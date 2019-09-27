
get_pred <- function(parms,data) {
  
  VA<- 0.2953      # visual angle per letter
  sys_err_mu_X0<- -0.271 #parms[1]#    # intercept of systematic error (in deg)
  sys_err_mu_X<- -0.058 #parms[2] #     # slope of systematic error (in deg)
  
  rand_sigmaX0<- 0.20#parms[1] # intercept of random error SD
  rand_sigmaX<- parms[1] # slope of random error SD
  
  dist<- data[,2]*VA    #intended saccade distance (left margin) 
  sys_error<- sys_err_mu_X0+ dist*sys_err_mu_X
  rand_error<- rnorm(n = nrow(data), mean = 0, sd = rand_sigmaX0+ rand_sigmaX*dist)
  
  sacc_len<- dist + sys_error + rand_error
  land_pos<- dist- sacc_len
  
  predict <- land_pos
  #predict <- sacc_len
  
  return(predict)
}