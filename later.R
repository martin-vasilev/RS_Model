
rm(list= ls())

erf.inv <- function(x) qnorm((x + 1)/2)/sqrt(2)


load("D:/R/RS_Model/data/Bold_OZ.Rda")
RSb<- subset(RSb, fix_dur>80 & fix_dur<1000)

FD<- RSb$fix_dur

hist(FD, col= 'steelblue')

hist(1/FD, col= 'steelblue')

cpp<- NULL

ecdf_FD<- ecdf(FD) 

for(i in 1:length(FD)){
  cpp[i]<- ecdf_FD(FD[i])
}


plot(FD, erf.inv(cpp)*100, col= 'steelblue')
