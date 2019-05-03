setwd("C:/users/jerry/desktop/MCMC/NBAproject")
teamdata<-read.csv("Teams2013_14.csv") 
teamdata<-teamdata[,names(teamdata)[-c(1,5)]]
#get rid of FT since possible multicollinearity with FTARate

#library('leaps')
regfit.full=regsubsets(Winpct~.,data=teamdata, nvmax=19)
#CROSS-VALIDATION in order to see which model (which predictors) gives the smallest Test error
#see p. 248 - ISL
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(teamdata),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Winpct~.,data=teamdata[train,],nvmax=19)

test.mat=model.matrix(Winpct~.,data=teamdata[test,])

val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((teamdata$Winpct[test]-pred)^2)
}

val.errors
which.min(val.errors)

predict.regsubsets=function(object,newdata ,id,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

#Finally, we perform best subset selection on the full data set, and select
#the best four-variable model.

regfit.best=regsubsets(Winpct~.,data=teamdata ,nvmax=19)
coef(regfit.best,6)

k=10
set.seed(1)
folds=sample(1:k,nrow(teamdata),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

for(j in 1:k){
  best.fit=regsubsets(Winpct~.,data=teamdata[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,teamdata[folds==j,],id=i)
    cv.errors[j,i]=mean( (teamdata$Winpct[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors ,2,mean)
mean.cv.errors

#for 2013_14.csv
1           2           3           4           5           6 
0.023654147 0.008975656 0.003983391 0.006751332 0.005593474 0.005498073 
7           8           9          10          11          12 
0.010571788 0.009097546 0.011195531 0.007929521 0.005056688 0.004040728 
13          14          15          16          17          18 
0.004429338 0.010391728 0.014798460 0.016371971 0.017660808 0.016352067 
19 

coef(reg.best,3)
(Intercept)      X2Ppct      TOVpct    Opp2Ppct 
2.00430408  4.50989383 -0.06310244 -5.84552158

coef(reg.best,6)
(Intercept)      X2Ppct   OppASTpTO   OppSTLpct      TOVpct    Opp3Ppct 
3.30637533  3.39547537 -0.26042134  0.05483490 -0.08580349 -2.12807221 
Opp2Ppct 
-5.27533704 
#so X2Ppct, OppASTpTO, Opp3Ppct, TOVpct, Opp2Ppct make the most sense
#value for OppSTLpct doesn't make sense
coef(reg.best,7)
(Intercept)       X3Ppct       X2Ppct    OppASTpTO    OppSTLpct 
1.671129535  1.421974792  3.631205574 -0.266366653  0.048995993 
TOVpct       ORBpct     Opp2Ppct 
-0.073666937  0.008751935 -5.450283452

coef(reg.best,13)
(Intercept)      X2Ppct      ASTpTO    ASTRatio   OppSTLpct  OppTORatio 
0.38582095  3.57385652  0.61452000 -0.04381129  0.06708212 -0.12734038 
OppPFDRate     FTARate      ORBpct      FtpFGA   OppTOVpct     DREBpct 
0.02451814 -3.48822781  0.01109911  3.08148559  0.14395547  0.01293936 
Opp3Ppct    Opp2Ppct 
-5.22387040 -3.59965149 

coef(reg.best,12)
(Intercept)      X2Ppct   OppSTLpct     TORatio  OppTORatio  OppPFDRate 
1.30504073  3.47093488  0.06086710 -0.06307434 -0.13861626  0.02412662 
FTARate      ORBpct      FtpFGA   OppTOVpct     DREBpct    Opp3Ppct 
-3.19490157  0.01067831  2.87799305  0.15511350  0.01307258 -5.16387979 
Opp2Ppct 
-3.53716270 




#FORWARD
regfit.fwd=regsubsets(Winpct~.,data=teamdata ,nvmax=26,method="forward")
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(teamdata),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Winpct~.,data=teamdata[train,],nvmax=26, method="forward")

test.mat=model.matrix(Winpct~.,data=teamdata[test,])
val.errors=rep(NA,26)
for(i in 1:26){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((teamdata$Winpct[test]-pred)^2)
}

predict.regsubsets=function(object,newdata ,id,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(Winpct~.,data=teamdata ,nvmax=19, method="forward")

k=10
set.seed(1)
folds=sample(1:k,nrow(teamdata),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets(Winpct~.,data=teamdata[folds!=j,],nvmax=19,method="forward")
  for(i in 1:19){
    pred=predict(best.fit,teamdata[folds==j,],id=i,method="forward")
    cv.errors[j,i]=mean( (teamdata$Winpct[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors ,2,mean)
mean.cv.errors

#for 2013_14 data
1           2           3           4           5           6 
0.023654147 0.013347695 0.008087036 0.006137742 0.006740964 0.005961516 
7           8           9          10          11          12 
0.005748461 0.005520564 0.005715412 0.006791946 0.007507901 0.008300739 
13          14          15          16          17          18 
0.009471581 0.009639363 0.009098126 0.008809407 0.009419909 0.009954780 
19 
0.011627600
#for 2013_14 BEST SUBSET
1           2           3           4        

coef(reg.best,8)
(Intercept)      X2Ppct OppASTRatio   OppSTLpct  OppTORatio      TOVpct 
3.25855529  2.99795762 -0.02215567  0.05279916 -0.05866614 -0.07852734 
OppTOVpct    Opp3Ppct    Opp2Ppct 
0.08099205 -3.24641504 -4.36542359



#BACKWARD
regfit.bwd=regsubsets(Winpct~.,data=teamdata ,nvmax=19,method="backward")
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(teamdata),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Winpct~.,data=teamdata[train,],nvmax=19, method="backward")

test.mat=model.matrix(Winpct~.,data=teamdata[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((teamdata$Winpct[test]-pred)^2)
}
predict.regsubsets=function(object,newdata ,id,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(Winpct~.,data=teamdata ,nvmax=19, method="backward")

100.384566    2.917006   -2.980225    5.076143 -125.688196   -5.918251

k=10
set.seed(1)
folds=sample(1:k,nrow(teamdata),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets(Winpct~.,data=teamdata[folds!=j,],nvmax=19,method="backward")
  for(i in 1:19){
    pred=predict(best.fit,teamdata[folds==j,],id=i,method="backward")
    cv.errors[j,i]=mean( (teamdata$Winpct[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors ,2,mean)
mean.cv.errors
#for 2013_14
1           2           3           4           5           6 
0.017613062 0.009642307 0.010113384 0.010214831 0.012937501 0.011529505 
7           8           9          10          11          12 
0.010291896 0.009334778 0.008531694 0.008524540 0.009156070 0.011321479 
13          14          15          16          17          18 
0.014102065 0.020085936 0.030336185 0.039643232 0.046983573 0.051149184 
19 
0.051331942

#for 2013_14
coef(reg.best,10)
(Intercept)      X2Ppct      ASTpTO    ASTRatio  OppTORatio     FTARate 
1.60355140  3.68039189  0.36013930 -0.01696854 -0.09133311 -2.85438311 
ORBpct      FtpFGA   OppTOVpct    Opp3Ppct    Opp2Ppct 
0.01548484  3.48122850  0.10375384 -4.00536918 -4.12320996
#X2Ppct, ASTpTO, ORBpct, FTpFGA, OppTOVpct, OPP3pPCT, Opp2Ppct

coef(reg.best,6)
(Intercept)      X2Ppct   OppASTpTO   OppSTLpct      TOVpct    Opp3Ppct 
3.30637533  3.39547537 -0.26042134  0.05483490 -0.08580349 -2.12807221 
Opp2Ppct 
-5.27533704 
#so X2Ppct, OppASTpTO, Opp3Ppct, TOVpct, Opp2Ppct make the most sense

#2013_14 forward
coef(reg.best,8)
(Intercept)      X2Ppct OppASTRatio   OppSTLpct  OppTORatio      TOVpct 
3.25855529  2.99795762 -0.02215567  0.05279916 -0.05866614 -0.07852734 
OppTOVpct    Opp3Ppct    Opp2Ppct 
0.08099205 -3.24641504 -4.36542359
#X2Ppct, OppASTRatio, TOVpct, OppTOVpct, Opp3Ppct,Opp2Ppct
#2013_14 BEST SUBSET
coef(reg.best,13)
(Intercept)      X2Ppct      ASTpTO    ASTRatio   OppSTLpct  OppTORatio 
0.38582095  3.57385652  0.61452000 -0.04381129  0.06708212 -0.12734038 
OppPFDRate     FTARate      ORBpct      FtpFGA   OppTOVpct     DREBpct 
0.02451814 -3.48822781  0.01109911  3.08148559  0.14395547  0.01293936 
Opp3Ppct    Opp2Ppct 
-5.22387040 -3.59965149 
#X2Ppct,ASTpTO, OppTORatio, ORBpct, FTpFGA, OppTOVpct,DREBpct,Opp3Ppct,Opp2Ppct
coef(reg.best,12)
(Intercept)      X2Ppct   OppSTLpct     TORatio  OppTORatio  OppPFDRate 
1.30504073  3.47093488  0.06086710 -0.06307434 -0.13861626  0.02412662 
FTARate      ORBpct      FtpFGA   OppTOVpct     DREBpct    Opp3Ppct 
-3.19490157  0.01067831  2.87799305  0.15511350  0.01307258 -5.16387979 
Opp2Ppct 
-3.53716270
#X2Ppct, TORatio,ORBpct, FTpFGA, OppTOVpct,DREBpct, Opp3Ppct,Opp2Ppct



#Backward (7Var):X2Ppct, ASTpTO, ORBpct, FTpFGA, OppTOVpct, OPP3pPCT, Opp2Ppct

#Forward (6var):X2Ppct, OppASTRatio, TOVpct, OppTOVpct, Opp3Ppct,Opp2Ppct

#BestSubset (8):
#X2Ppct,ASTpTO, OppTORatio, ORBpct, FTpFGA, OppTOVpct,DREBpct,Opp3Ppct,Opp2Ppct
#X2Ppct, TORatio,         ORBpct, FTpFGA, OppTOVpct,DREBpct, Opp3Ppct,Opp2Ppct


#overall (7-var): 
#X2Ppct, TORatio/TOVpct, ORBpct,FTpFGA,OppTOVpct, Opp3Ppct, Opp2Ppct, ASTpTO, 
#BUT, high multicollinearity with ASTpTO and TORatio/TOVpct
#so throw out ASTpTO


gelman.diag(mcmc_samples)
Potential scale reduction factors:
  
  Point est. Upper C.I.
b.0               1.02       1.03
b.FtpFGA          1.00       1.00
b.ORBpct          1.00       1.00
b.Opp2Ppct        1.03       1.08
b.Opp3Ppct        1.02       1.06
b.OppTOVpct       1.00       1.01
b.TORatio         1.01       1.02
b.X2Ppct          1.00       1.00

crosscorr(mcmc_samples)
b.0    b.FtpFGA    b.ORBpct  b.Opp2Ppct  b.Opp3Ppct
b.0          1.00000000  0.05766023 -0.42744971 -0.19704154 -0.60977122
b.FtpFGA     0.05766023  1.00000000 -0.31750182  0.26491754 -0.21066915
b.ORBpct    -0.42744971 -0.31750182  1.00000000 -0.21082334  0.30115538
b.Opp2Ppct  -0.19704154  0.26491754 -0.21082334  1.00000000 -0.46669010
b.Opp3Ppct  -0.60977122 -0.21066915  0.30115538 -0.46669010  1.00000000
b.OppTOVpct  0.05504873  0.05380531  0.07393901 -0.33349218  0.04296359
b.TORatio   -0.24052911  0.01940115 -0.09121575  0.07513257 -0.05138771
b.X2Ppct    -0.61699526 -0.43205487  0.42443801 -0.12137566  0.42409657
b.OppTOVpct   b.TORatio   b.X2Ppct
b.0          0.05504873 -0.24052911 -0.6169953
b.FtpFGA     0.05380531  0.01940115 -0.4320549
b.ORBpct     0.07393901 -0.09121575  0.4244380
b.Opp2Ppct  -0.33349218  0.07513257 -0.1213757
b.Opp3Ppct   0.04296359 -0.05138771  0.4240966
b.OppTOVpct  1.00000000 -0.08525286 -0.2903157
b.TORatio   -0.08525286  1.00000000 -0.1288827
b.X2Ppct    -0.29031574 -0.12888270  1.0000000