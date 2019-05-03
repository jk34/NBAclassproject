library(R2OpenBUGS)
library ("arm")
#modeldata<-read.csv("Teams2014_15.csv") 
modeldata<-read.csv("Teams2013_14.csv") 

Winpct<-modeldata$Winpct                 
#Tspct<-modeldata$Tspct   
#OppeFGpct<-modeldata$OppeFGpct  
#OppToRatio<-modeldata$OppToRatio
TORatio<-modeldata$TORatio
#don't use PFD because we got NEGATIVE correlation coefficient
#PFD<-modeldata$PFD

#using new data, best predictors are
#X2Ppct, ASTpTO, OppFTARate, Opp3Ppct,Opp2Ppct
OppFTARate<-modeldata$OppFTARate
X2Ppct<-modeldata$X2Ppct
ASTpTO<-modeldata$ASTpTO
Opp3Ppct<-modeldata$Opp3Ppct
Opp2Ppct<-modeldata$Opp2Ppct

ORBpct <-modeldata$ORBpct
FtpFGA <-modeldata$FtpFGA 
OppTOVpct <-modeldata$OppTOVpct
#if using 2013_14 data, use the following 6 predictors
#X3Ppct, X2Ppct, OppASTpTO, TOVpct, Opp3Ppct, Opp2Ppct


y<-Winpct
n <- length(Winpct) 

#new_data <- list (n=n, y=y, OppToRatio=OppToRatio, TORatio=TORatio, PFD=PFD, Tspct=Tspct, OppeFGpct=OppeFGpct, OppFTARate=OppFTARate)
previousseason <- list (n=n, y=y, X2Ppct=X2Ppct, TORatio=TORatio, ORBpct=ORBpct, FtpFGA=FtpFGA, OppTOVpct=OppTOVpct, Opp3Ppct=Opp3Ppct, Opp2Ppct=Opp2Ppct)

#Try JAGS below
library(rjags)
inits = function ()
{
  # list('b.0' = rnorm(1, 0, 100), 'b.PFD' = rnorm(1, 0, 100), 'b.TORatio' = rnorm(1, 0, 100),
  # 'b.Tspct'= rnorm(1, 0, 100), 'b.OppeFGpct'= rnorm(1, 0, 100), 'b.OppFTARate'= rnorm(1, 0, 100),'b.OppToRatio' = rnorm(1, 0, 100))
  list('b.0' = rnorm(1, 0, 100), 'b.X2Ppct' = rnorm(1, 0, 100), 'b.TORatio' = rnorm(1, 0, 100),
       'b.ORBpct'= rnorm(1, 0, 100), 'b.FtpFGA'= rnorm(1, 0, 100), 'b.OppTOVpct'= rnorm(1, 0, 100),'b.Opp3Ppct' = rnorm(1, 0, 100),'b.Opp2Ppct' = rnorm(1, 0, 100))
}
#n.chains=length(inits)

# model <- jags.model('modelNBAjags.txt', data=new_data, inits=inits, n.chains=3)
model <- jags.model('modelNBAjags.txt', data=previousseason, inits=inits, n.chains=3)
#thining=10?
update(model, 20000)
#as.mcmc(x) in the SocialProject pdf converts a BUGS object `x` to an MCMC object
mcmc_samples <- coda.samples(model, variable.names=c('b.0','b.X2Ppct', 'b.TORatio', 'b.ORBpct', 'b.FtpFGA','b.OppTOVpct', 'b.Opp3Ppct', 'b.Opp2Ppct'),n.iter=200000,thin=100)
post_samples <- jags.samples(model, variable.names=c('b.0','b.X2Ppct', 'b.TORatio', 'b.ORBpct', 'b.FtpFGA','b.OppTOVpct', 'b.Opp3Ppct', 'b.Opp2Ppct'),n.iter=200000,thin=100)
#mcmc_samples <- coda.samples(model, variable.names=c('b.0','b.OppToRatio', 'b.TORatio', 'b.Tspct', 'b.OppeFGpct', 'b.OppFTARate', 'b.PFD'),n.iter=200000,thin=100)
#post_samples <- jags.samples(model, variable.names=c('b.0','b.OppToRatio', 'b.TORatio', 'b.Tspct', 'b.OppeFGpct', 'b.OppFTARate', 'b.PFD'),n.iter=200000,thin=100)
#jags.samples extracts random samples from the posterior dist of the parameters b.0, b.X3Ppct, ..., b.ASTpTO
#coda.samples provides output from JAGS in the format necessary for `plot()`
post.b0<-as.mcmc.list(post_samples$b.0)
post.bTORatio<-as.mcmc.list(post_samples$b.TORatio)
#post.bOppToRatio<-as.mcmc.list(post_samples$b.OppToRatio)
#post.bTspct<-as.mcmc.list(post_samples$b.Tspct)
#post.bOppeFGpct<-as.mcmc.list(post_samples$b.OppeFGpct)
#post.bOppFTARate<-as.mcmc.list(post_samples$b.OppFTARate)
#post.bPFD<-as.mcmc.list(post_samples$b.PFD)

post.bX2Ppct<-as.mcmc.list(post_samples$b.X2Ppct)
post.bORBpct<-as.mcmc.list(post_samples$b.ORBpct)
post.bFtpFGA<-as.mcmc.list(post_samples$b.FtpFGA)
post.bOppTOVpct<-as.mcmc.list(post_samples$b.OppTOVpct)
post.bOpp3Ppct<-as.mcmc.list(post_samples$b.Opp3Ppct)
post.bOpp2Ppct<-as.mcmc.list(post_samples$b.Opp2Ppct)


x11() 
par(mfrow=c(2,2))
plot(post.b0, auto.layout=FALSE, main='b.0')
plot(post.bTORatio, auto.layout=FALSE, main='b.TORatio')
plot(post.bORBpct, auto.layout=FALSE, main='b.ORBpct')
plot(post.bFtpFGA, auto.layout=FALSE, main='b.FtpFGA')
plot(post.bOppTOVpct, auto.layout=FALSE, main='b.OppTOVpct')
plot(post.bOpp3Ppct, auto.layout=FALSE, main='b.Opp3Ppct')
plot(post.bOpp2Ppct, auto.layout=FALSE, main='b.Opp2Ppct')
plot(post.bX2Ppct, auto.layout=FALSE, main='b.X2Ppct')
#plot(mcmc_samples)
dev.off()