
data<- data.frame(y= RS$LandStartVA[which(!is.na(RS$prevChar))],
         char= RS$prevChar[which(!is.na(RS$prevChar))])

source('functions/rsmd.R')
source('functions/get_pred.R')

startParms <- c(0.1, 0.05)

xout <- optim(startParms, rmsd, gr=NULL, method="SANN", d=data)
xout
