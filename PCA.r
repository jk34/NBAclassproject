modeldata<-read.csv("Teams2013_14.csv") 
modeldata<-modeldata[,names(modeldata)[-c(1,5)]]
pr.out = prcomp(modeldata , scale=TRUE)
pr.val = pr.out$sdev^2
pve=pr.val/sum(pr.val)
plot(pve, xlab="Prin Comp", ylab="Proportion of Var Explained", ylim=c(0,.3), type='b')
plot(cumsum(pve), xlab="Prin Comp", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), type='b')
#see pg. 383, figure 10.4 for meaning of these plots and how to determine optimal number of principal components