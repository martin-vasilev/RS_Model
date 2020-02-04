
#' Model simulations with ReturnSweeper 1
#' 
#' @author Martin R. Vasilev
#' @param RS_target Return sweep saccade target (in deg from line start)
#' @param word_pos Location of words on next line
#' @param S_X0 Intercept of systematic error (in deg)
#' @param S_X Slope of systematic error (in deg)
#' @param R_X0 Intercept of random error SD
#' @param R_X Slope of random error SD
#' @param pCorr_mu Mean of CDF for prob. of making a corrective saccade (in letters)
#' @param pCorr_sigma SD of CDF for prob. of making a corrective saccade  (in letters)
#' @param diff_cond a logical indicating whether the text differed between conditions
#' @param nsim Number of monte carlo simulations to run


Return_sweeper1<- function(data, word_pos, RS_target= 0, S_X0= 0.07712, S_X= -0.07710, R_X0= 0.11840, R_X= 0.08248,
                           pCorr_mu= 2, pCorr_sigma= 7, diff_cond= FALSE, nsim=10, aggregate=T){
  
  all<- NULL
  
  cat("Simulation: ")
  
  for (s in 1:nsim){ # for each simulation iteration
    # Set-up variables for saving results from the model:
    data$M_launchDistVA<- NA        # return-sweep saccade length in deg
    data$M_landStartVA<-NA          # return-sweep landing position in deg (relative to line start)
    data$M_landStartLet<- NA        # return-sweep landing position in letters (relative to line start)
    data$M_UND<- NA                 # undersweep probability
    data$M_next_LP<- NA             # landing position in deg of the next saccade following return-sweep
    data$M_next_sacc_len<- NA       # saccade length in deg of of the next saccade following return-sweep
    
    #------------------------------------------------------------------------------------------------------
    # Model simulations:
    #------------------------------------------------------------------------------------------------------
    
    for(i in 1:nrow(data)){
      left_margin<- data$prevVA[i] # distance to left margin from line-final fixation (in deg)
      
      # Calculate intended saccade distance:
      if(is.numeric(RS_target)){
        dist<- data$prevVA[i] - RS_target*data$VA[i]   
      }else{
        if(RS_target== "OVP"){
          
          if(diff_cond){ # text differs between conditions; subset by cond too
            loc<- which(word_pos$item== data$item[i] &  word_pos$cond== data$cond[i] & word_pos$line== data$line[i] & word_pos$word== 1)
          }else{
            loc<- which(word_pos$item== data$item[i] & word_pos$line== data$line[i] & word_pos$word== 1)
          }
          
          W1_OVP<- word_pos$OVP[loc]
          curr_target<- W1_OVP # this is the equiv. to RS_target for this iteration!
          dist<- data$prevVA[i] - curr_target*data$VA[i]
        }else{
          stop('Unknown input to "RS_target"!')
        }
      }
      
      sys_error<- S_X0+ dist*S_X  # systematic return sweep error
      rand_error<- rnorm(n = 1, mean = 0, sd = R_X0+ R_X*dist)
      
      data$M_launchDistVA[i]<- dist + sys_error + rand_error
      data$M_landStartVA[i]<- left_margin- data$M_launchDistVA[i]
      data$M_landStartLet[i]<- ceiling(data$M_landStartVA[i]/data$VA[i])
      
      # Probability of making a corrective saccade
      if(RS_target== "OVP"){
        probCorrSacc<- pnorm(q = data$M_landStartLet[i]- curr_target, mean =  pCorr_mu, sd = pCorr_sigma)
      }else{
        probCorrSacc<- pnorm(q = data$M_landStartLet[i]- RS_target, mean =  pCorr_mu, sd = pCorr_sigma)
        
      }
      
      RandDraw<- runif(1)
      
      if(RandDraw<= probCorrSacc){
        data$M_UND[i]<- 1
      }else{
        data$M_UND[i]<- 0
      }
      
      # length of corrective saccade
      if(data$M_UND[i]== 1){
        
        #next landing position (in deg):
        if(RS_target== "OVP"){
          next_sacc_dist<- data$M_landStartVA[i]- curr_target*data$VA[i]
        }else{
          next_sacc_dist<- data$M_landStartVA[i]- RS_target*data$VA[i]
        }
        
        sys_error_next<- S_X0+ next_sacc_dist*S_X  # systematic return sweep error
        rand_error_next<- rnorm(n = 1, mean = 0, sd = R_X0+ R_X*next_sacc_dist)
        
        data$M_next_sacc_len[i]<- next_sacc_dist + sys_error_next + rand_error_next
        data$M_next_LP[i]<- (data$M_landStartVA[i]- data$M_next_sacc_len[i])/data$VA[i]
      }
      
    } # end of i loop (dataframe)
    
    data$Sim<- s
    all<- rbind(all, data)
    
    cat(s); cat(" ")
  } # end of s loop (nsim)
  
  if(aggregate){
   cat("Aggregating data...")
    
   data$M_launchDistVA<- NA 
   data$M_landStartVA<-NA
   data$M_landStartLet<- NA
   data$M_UND<- NA
   data$M_next_LP<- NA
   data$M_next_sacc_len<- NA 
    
   for (i in 1:nrow(data)){
     n<- subset(all, sub== data$sub[i] & item== data$item[i])
     
     data$M_launchDistVA[i]<- mean(n$M_launchDistVA, na.rm=T)
     data$M_landStartVA[i]<- mean(n$M_landStartVA, na.rm= T)
     data$M_landStartLet[i]<- mean(n$M_landStartLet, na.rm= T)
     data$M_UND[i]<- mean(n$M_UND, na.rm= T)
     data$M_next_LP[i]<- mean(n$M_next_LP, na.rm= T)
     data$M_next_sacc_len[i]<- mean(n$M_next_sacc_len, na.rm= T)
     
   }
   
   data$Sim<- NULL
   cat("Done")
     
    return(data)
  }else{
    return(all)
  }
     
}
