
normalF <- function(parvec) {
  # Log of likelihood of a normal distribution
  # parvec[1] - mean
  # parvec[2] - standard deviation
  # x - set of observations. Should be initialized before MLE
  sum ( -0.5* log(parvec[2]) - 0.5*(x - parvec[1])^2/parvec[2] )
}

x = c(1,2,3,4) # set of observations
normalF(c(1,1)) # log likelihood function value for given x and mu=sd=1 


set.seed(1729)
x = rnorm(100,2,4) # a hundred numbers with mean 2 and sd 4
MLE = optim(c(0.1,0.1), # initial values for mu and sigma
            fn = normalF, # function to maximize
            method = "L-BFGS-B", # this method lets set lower bounds
            lower = 0.00001, # lower limit for parameters
            control = list(fnscale = -1), # maximize the function
            hessian = T # calculate Hessian matricce because we will need for confidence intervals
)
MLE

library(bbmle)
m <- mle2(x~dnorm(mean=mu,sd=sd),start=list(mu=0.1,sd=0.1),data=data.frame(x))
m



##############################################################################

x<- RS$LandStartVA[which(!is.na(RS$prevChar))]
char<- RS$prevChar[which(!is.na(RS$prevChar))]

MLF<- function(){
 
}




RS$Model<- NA
RS$LPM<- NA

for(i in 1:nrow(RS)){
  dist<- RS$prevChar[i]*VA    #intended saccade distance (left margin) 
  sys_error<- sys_err_mu_X0+ dist*sys_err_mu_X
  rand_error<- rnorm(n = 1, mean = 0, sd = rand_sigmaX0+ rand_sigmaX*dist)
  
  sacc_len<- dist + sys_error + rand_error
  RS$Model[i]<- sacc_len
  RS$LPM[i]<- dist- sacc_len
}




