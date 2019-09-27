
# from https://www.r-bloggers.com/normal-distribution-functions/

set.seed(3000)
xseq<-seq(1, 60,.1)
densities<-dnorm(xseq, 0,1)
cumulative<-pnorm(xseq, 2, 7)
plot(xseq, cumulative, col="darkorange", xlab="Distance to target (letters)", ylab="Cumulative Probability",type="l",
     lwd=2, cex=2, main="Corrective saccade probability",  
     cex.axis=.8, family= 'serif', cex.axis= 1.2, cex.lab=1.4)

