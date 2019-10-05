#R function to replace Listing 3.2. Because R permits passing of multiple parameters to goodness-of-fit
#  function, no wrapper4fmin is required. Instead, Listing 3.1 calls the optimization function directly 
#  and passes this function ('rmsd') as a handle together with all other required parameters
rmsd <-function(parms, d) {
  predictions<-get_pred(parms, d)
  sd<-(predictions-d[ ,3])^2
  return(sqrt(sum(sd)/length(sd)))
}