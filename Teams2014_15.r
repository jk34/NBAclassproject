
teamdata<-read.csv("Teams2014_15.csv") 
teamdata<-teamdata[,names(teamdata)[-c(1,20, 22,23, 31,32,34, 3,11,12,13,14,15,16,18,19,29)]]
#remove PF because of high multicollinearity with oppFTARate
#remove Opp3PtPct because of multicollinearity with OppeFGpct
#remove X3PtPct also
#remove FG-3, FTA-11, OREB-12,DREB-13,REB-14,AST-15,TOV-16,BLK-18,BLKA-19,REBpct-29

#below shows the high multicollinearity between PF and OppFTARate
library('car')
teamdata<-teamdata[,names(teamdata)[-c(1, 19,20)]]
fit<-lm(Winpct~ OppeFGpct + PF+ STL + TORatio + DREB + OppFTARate + Tspct, data=teamdata)
> vif(fit)
OppeFGpct         PF        STL    TORatio       DREB OppFTARate      Tspct 
3.662531   7.057335   2.087927   1.662730   4.555613   5.860920   1.380682 
> fit<-lm(Winpct~ OppeFGpct +  STL + TORatio + DREB + OppFTARate + Tspct, data=teamdata)
> vif(fit)
OppeFGpct        STL    TORatio       DREB OppFTARate      Tspct 
3.081950   1.803087   1.458861   3.549523   1.112671   1.324822


library('leaps')
=======
teamdata<-teamdata[,names(teamdata)[-c(1,19,20)]]
library('leaps')

#Forward Stepwise Selection - P.247 of ISL
regfit.fwd=regsubsets(Winpct~.,data=teamdata ,nvmax=15,method="forward")
summary(regfit.fwd)
#1-var: eFGpct
#2-var: eFGpct, OppeFGpct
#3: eFGpct, OppeFGpct, OppFTARate, 
#4: TORatio
#5: OppToRatio
#6: FTM
#7: PFD
#8: AST
#9: PF
#10: ASTRatio

#Best Subset Selection - p.244 ISL
regfit.full=regsubsets(Winpct~.,data=teamdata, nvmax=10)
summary(regfit.full)
<<<<<<< HEAD
#eFGpct, OppeFTpct, TORatio, OppToRatio, OppFTARate, OREBpct, FTM
=======
#1.eFGpct
#2. Tspct and OppeFGpct
#3. TORatio
#4. Tspct, OppeFGpct, TOV, STL
#5. Tspct, OppeFGpct, TOV, STL, OppFTARate
#6. Tspct, OppeFGpct, TOV, STL, OppFTARate, PF
#7. Tspct, OppeFGpct, TOV, STL, OppFTARate, PF, DREB
#8. Tspct, OppeFGpct, TOV, OppFTARate, PF, FTA, PFD, OppToRatio
#9. Tspct, OppeFGpct, X3Ppct, AST, STL, OppFTARate, FTA, ASTRatio, FTARate
>>>>>>> a97e4df9a374431f09cbbd7af0cd7f6ff2c1de45

#Now want to select the best overall model
#To determine that, we need to look at the RSS, adjusted-R^2, C_p and BIC values
reg.summary = summary(regfit.full)
#First, let's look at the R-squared for each model
reg.summary$rsq
#it appears the biggest jump occurs from 3->4, so perhaps 4 predictors is best

jpeg('modelNBA_RSS_AdjrR2_Cp_BIC.jpg')
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
#which.max identifies the location of the maximum point of a vector
#points() works like plot(), except it adds points to an existing plot
#We will plot a red dot to indicate the model with largest adj-R^2
points(10,reg.summary$adjr2[10], col="red",cex=2,pch=20)

#similarly, plot for C_p and BIC statistics, and look for the smallest statistic instead of max
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
#error since cp values are infinity
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
which.min(reg.summary$bic)
points(10,reg.summary$bic[10],col="red",cex=2,pch=20)
dev.off()

#regsubsets() has a built-in plot() command that can display the selected variables
#for the best model with a given number of predictors, ranked according to BIC, Cp, Adj-R2,or AIC
x11() 
jpeg('modelNBA_Best_RSS_AdjrR2_Cp_BIC.jpg')
par(mfrow=c(2,2))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
dev.off()

#see the coefficient estimates associated with a model
coef(regfit.full,6)

<<<<<<< HEAD
#CROSS-VALIDATION in order to see which model (which predictors) gives the smallest Test error
=======
#Cross-validation in order to see which model (which predictors) gives the smallest Test error
>>>>>>> a97e4df9a374431f09cbbd7af0cd7f6ff2c1de45
#see p. 248 - ISL
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(teamdata),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Winpct~.,data=teamdata[train,],nvmax=10)
#above, we subset the teamdata in order to access only the training subset of the data
#with `teamdata[train,]`

#compute the validation set error for the best model of each model size
#First, make a model matrix from the test data
test.mat=model.matrix(Winpct~.,data=teamdata[test,])

#Now, run a loop and for each i, we extract the coefficients from `regfit.best`
#for the best model of that size, multiply them into the columns of the test model matrix
#to form the predictions, and compute the test MSE
val.errors=rep(NA,19)
for(i in 1:10){
    coefi=coef(regfit.best,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    val.errors[i]=mean((teamdata$Winpct[test]-pred)^2)
}

val.errors
which.min(val.errors)
#since 4 variables has the smallest MSE of 47.244, it is the best model
coef(regfit.best,4)
(Intercept)        DREB      ASTpTO       Tspct   OppeFGpct 
 206.612667   -3.153975   14.304484    5.166170   -7.123686
 #Now, DREB and ASTpTO replace TOV/STL in Best Subset, and 
 #OppFTARate/TORatio in Forward Step. This is for TRAINING set. To get best predictors
 #for FULL data set, see below
 
 #since we'll use the steps above again, better to write as function
predict.regsubsets=function(object,newdata ,id,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

#Finally, we perform best subset selection on the full data set, and select
#the best four-variable model. It is important that we make use of the full
#data set in order to obtain more accurate coefficient estimates. Note that
#we perform best subset selection on the full data set and select the best four-
#variable model, rather than simply using the variables that were obtained
#from the training set, because the best four-variable model on the full data
#set may differ from the corresponding model on the training set

regfit.best=regsubsets(Winpct~.,data=teamdata ,nvmax=10)
coef(regfit.best,4)
(Intercept)         TOV         STL       Tspct   OppeFGpct 
  63.604511   -3.500783    3.302484    5.149053   -5.323032
#So for the FULL data set, TOV and STL replace DREB and ASTpTO from the TRAINING set

#We now try to choose among the models of different sizes using CROSS=VALIDATION.
# This approach is somewhat involved, as we must perform best subset selection
#within EACH OF THE K TRAINING sets. Despite this, we see that with its clever 
#subsetting syntax,R makes this job quite easy. First, we create a vector that 
#allocates each observation to one of k= 10 folds, and we create a matrix in 
#which we will store the results.
k=10
set.seed(1)
folds=sample(1:k,nrow(teamdata),replace=TRUE)
cv.errors=matrix(NA,k,10, dimnames=list(NULL, paste(1:10)))

#Now we write a for loop that performs cross-validation. In the jth fold, the
#elements of folds that equal j are in the test set, and the remainder are in
#the training set. We make our predictions for each model size (using our
#new predict() method), compute the test errors on the appropriate subset,
#and store them in the appropriate slot in the matrix cv.errors

for(j in 1:k){
  best.fit=regsubsets(Winpct~.,data=teamdata[folds!=j,],nvmax=10)
  for(i in 1:10){
     pred=predict(best.fit,teamdata[folds==j,],id=i)
     cv.errors[j,i]=mean( (teamdata$Winpct[folds==j]-pred)^2)
  }
}

#This has given us a 10×10 matrix, of which the (i,j)th element corresponds
#to the test MSE for the ith cross-validation fold for the best j-variable
#model. We use the apply() function to average over the columns of this
#matrix in order to obtain a vector for which the jth element is the cross-
#validation error for the j-variable model.

mean.cv.errors=apply(cv.errors ,2,mean)
mean.cv.errors
<<<<<<< HEAD
1        2        3        4        5        6        7        8 
74.58313 32.13637 31.29649 34.82345 23.44176 24.86903 27.17196 36.98108 
9       10 
40.22767 25.99365

#We see that Cross-validation chooses 5,6,7-variable model as having the lowest MSE
#We now perform best subset selection on the full data set in order to obtain 
#the 6-variable model
#(it was 7-variable when we included PF)
reg.best=regsubsets(Winpct~.,data=teamdata ,nvmax=10)
coef(reg.best,6)
(Intercept)     OREBpct     TORatio       Tspct   OppeFGpct  OppFTARate 
65.0293128   0.9374941  -3.5462944   5.0997441  -5.5850464 -99.8070993 
OppToRatio 
3.0963607
#so OppToRatio and Opp3PtPct replace DREB, STL, PF
#OREB replaces Opp3PtPct
fit<-lm(Winpct~ TOV + Tspct + OppeFGpct + OppFTARate + OppToRatio + Opp3PtPct, data=teamdata)
vif(fit)
TOV      Tspct        OppeFGpct OppFTARate OppToRatio  Opp3PtPct 
1.390274   1.405444   1.751603   1.122477   1.390001   1.931328 

fit<-lm(Winpct~ TOV + Tspct + OppeFGpct + OppFTARate + OppToRatio + ASTpct, data=teamdata)
vif(fit)
TOV      Tspct  OppeFGpct OppFTARate OppToRatio     ASTpct 
1.308806   1.288849   1.385033   1.122409   1.668434   1.464255

fit<-lm(Winpct~ TOV + Tspct + OppeFGpct + OppFTARate + OppToRatio + OREBpct, data=teamdata)
vif(fit)
TOV      Tspct  OppeFGpct OppFTARate OppToRatio    OREBpct 
1.768197   1.275253   1.221233   1.127918   1.751025   1.499768


coef(reg.best,4)
(Intercept)         TOV         STL       Tspct   OppeFGpct 
63.604511   -3.500783    3.302484    5.149053   -5.323032
coef(reg.best,7)
(Intercept)         FTM         TOV         PFD      eFGpct   OppeFGpct 
129.754087    2.846712   -3.192193   -1.563803    4.471030   -5.866392 
OppFTARate  OppToRatio 
-100.580631    3.090134 
#PFD is NEGATIVE

coef(reg.best,8)
(Intercept)         FTA         AST    ASTRatio       Tspct     FTARate 
-56.905081   13.791843  -14.246318   21.950828    5.380879 -987.766531 
OppeFGpct  OppFTARate  OppToRatio 
-6.054870 -107.964732    3.179230
#AST is NEGATIVE

#Below is when we included PF
coef(reg.best,7)
(Intercept)        DREB         TOV         STL          PF       Tspct    
 191.162568   -1.661630   -4.226332    2.744893    3.917890    5.183251   
OppeFGpct OppFTARate 
-6.556204 -288.363519 
#This matches the 7-variable from Best Subset from above
coef(reg.best,4)
(Intercept)     TORatio      eFGpct   OppeFGpct  OppFTARate 
65.257327   -1.717163    5.348427   -4.524709 -111.907581 

coef(reg.best,9)
(Intercept)          FTM          AST           PF          PFD 
132.5592986    2.5888811   -0.7949304    1.4773621   -1.8448728 
TORatio       eFGpct    OppeFGpct   OppFTARate   OppToRatio 
-3.4375954    4.9584623   -5.9048069 -182.4997495    3.0983874


#FORWARD STEPWISE Selection - P.247 of ISL
regfit.fwd=regsubsets(Winpct~.,data=teamdata ,nvmax=15,method="forward")
summary(regfit.fwd)
#1-var: eFGpct
#2-var: eFGpct, OppeFGpct
#3: eFGpct, OppeFGpct, OppFTARate, 
#4: TORatio
#5: OppToRatio
#6: FTM
#7: PFD
#8: AST
#9: BLK
reg.summary = summary(regfit.fwd)
#First, let's look at the R-squared for each model
reg.summary$rsq
#Biggest jump occurs from 6->7, so let 7-variable model be best


#Cross-validation in order to see which model (which predictors) gives the smallest Test error
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(teamdata),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Winpct~.,data=teamdata[train,],nvmax=10, method="forward")

test.mat=model.matrix(Winpct~.,data=teamdata[test,])
val.errors=rep(NA,19)
for(i in 1:10){
    coefi=coef(regfit.best,id=i)
     pred=test.mat[,names(coefi)]%*%coefi
     val.errors[i]=mean((teamdata$Winpct[test]-pred)^2)
 }
[1] 108.72182  59.04488  78.11365  71.12247  61.85793  56.03586  72.47842
[8]  67.68373  81.51628  92.47196        NA        NA        NA        NA
[15]        NA        NA        NA        NA        NA
coef(regfit.best,6)
(Intercept)          FG         FTM        DREB      ASTpTO      eFGpct 
155.936636   -2.771442    1.311126   -2.179168   16.138853    6.802628 
OppeFGpct 
-5.970812
#This is for TRAINING set. To get best predictors
#for FULL data set, see below
predict.regsubsets=function(object,newdata ,id,...){
       form=as.formula(object$call [[2]])
       mat=model.matrix(form,newdata)
       coefi=coef(object,id=id)
       xvars=names(coefi)
       mat[,xvars]%*%coefi
   }
regfit.best=regsubsets(Winpct~.,data=teamdata ,nvmax=10, method="forward")
coef(regfit.best,7)
(Intercept)         FTM     TORatio      eFGpct   OppeFGpct  OppFTARate 
103.551575    1.693229   -2.870347    4.631748   -5.555406 -117.913919 
OppToRatio 
2.572595

k=10
set.seed(1)
folds=sample(1:k,nrow(teamdata),replace=TRUE)
cv.errors=matrix(NA,k,10, dimnames=list(NULL, paste(1:10)))
for(j in 1:k){
    best.fit=regsubsets(Winpct~.,data=teamdata[folds!=j,],nvmax=10,method="forward")
for(i in 1:10){
    pred=predict(best.fit,teamdata[folds==j,],id=i,method="forward")
cv.errors[j,i]=mean( (teamdata$Winpct[folds==j]-pred)^2)
}
}

mean.cv.errors=apply(cv.errors ,2,mean)
mean.cv.errors
1        2        3        4        5        6        7        8 
74.58313 38.15766 29.05867 28.08718 31.45481 30.31790 33.27533 29.93813 
9       10 
27.29356 29.89028
#We see that Cross-validation chooses 4,5,6,8,9-variable model as having the lowest MSE
#We now perform Forward Selection on the full data set in order to obtain 
#the 4,5,6,8,9-variable model. 
(For Best Subset, we got)
mean.cv.errors
1        2        3        4        5        6        7        8 
74.58313 32.13637 31.29649 34.82345 23.44176 24.86903 27.17196 36.98108 
9       10 
40.22767 25.99365

reg.best=regsubsets(Winpct~.,data=teamdata ,nvmax=10, method="forward")
coef(reg.best,4)
(Intercept)     TORatio      eFGpct   OppeFGpct  OppFTARate 
65.257327   -1.717163    5.348427   -4.524709 -111.907581 

coef(reg.best,6)
(Intercept)         FTM     TORatio      eFGpct   OppeFGpct  OppFTARate 
103.551575    1.693229   -2.870347    4.631748   -5.555406 -117.913919 
OppToRatio 
2.572595

#from Best Subset
coef(reg.best,6)
(Intercept)     OREBpct     TORatio       Tspct   OppeFGpct  OppFTARate 
65.0293128   0.9374941  -3.5462944   5.0997441  -5.5850464 -99.8070993 
OppToRatio 
3.0963607
#So Forward predicts FTM whereas Best Subset predicts OREBpct
#They both share OppToRatio, Tspct, OppeFGpct, OppFTARate in common

coef(reg.best,7)
(Intercept)         FTM         PFD     TORatio      eFGpct   OppeFGpct 
125.098862    2.691369   -1.586590   -2.993883    4.525797   -5.691632 
OppFTARate  OppToRatio 
-108.060301    2.881523
#PFD is NEGATIVE

coef(reg.best,8)
(Intercept)          FTM          AST          PFD      TORatio 
144.8026719    2.7515830   -0.8198252   -1.8466521   -3.2949985 
eFGpct    OppeFGpct   OppFTARate   OppToRatio 
4.8425021   -5.9905684 -106.9074940    3.2771342
#AST is NEGATIVE


#BACKWARD
regfit.bwd=regsubsets(Winpct~.,data=teamdata ,nvmax=15,method="backward")
summary(regfit.bwd)
#TSpct, OppFGpct, TORatio, OppFTARate, STL, AST

#Cross-validation in order to see which model (which predictors) gives the smallest Test error
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(teamdata),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Winpct~.,data=teamdata[train,],nvmax=10, method="backward")

test.mat=model.matrix(Winpct~.,data=teamdata[test,])
val.errors=rep(NA,19)
for(i in 1:10){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((teamdata$Winpct[test]-pred)^2)
}
val.errors
[1] 319.3990 118.4934 130.3669 106.0873 131.9460 135.4946 145.8060 145.8652
[9] 186.5859 280.6110       NA       NA       NA       NA       NA       NA
[17]       NA       NA       NA
> coef(regfit.best,5)
(Intercept)          FG        X3PA         REB         AST      ASTpct 
-673.348941   10.458673    2.461344    3.231382   -6.775207    3.528318
#This is for TRAINING set. To get best predictors
#for FULL data set, see below
predict.regsubsets=function(object,newdata ,id,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(Winpct~.,data=teamdata ,nvmax=10, method="backward")
coef(regfit.best,5)
(Intercept)         STL     TORatio       Tspct  OppFTARate    OppFGpct 
100.384566    2.917006   -2.980225    5.076143 -125.688196   -5.918251

k=10
set.seed(1)
folds=sample(1:k,nrow(teamdata),replace=TRUE)
cv.errors=matrix(NA,k,10, dimnames=list(NULL, paste(1:10)))
for(j in 1:k){
  best.fit=regsubsets(Winpct~.,data=teamdata[folds!=j,],nvmax=10,method="backward")
  for(i in 1:10){
    pred=predict(best.fit,teamdata[folds==j,],id=i,method="backward")
    cv.errors[j,i]=mean( (teamdata$Winpct[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors ,2,mean)
mean.cv.errors
1        2        3        4        5        6        7        8 
76.22233 49.99106 57.92054 69.64193 55.41638 44.50080 41.64711 41.44090 
9       10 
40.51737 51.37477
#for forward, it was
1        2        3        4        5        6        7        8 
74.58313 32.13637 31.29649 34.82345 23.44176 24.86903 27.17196 36.98108 
9       10 
40.22767 25.99365
#For Best Subset, we got)
1        2        3        4        5        6        7        8 
74.58313 32.13637 31.29649 34.82345 26.15444 18.01482 24.43030 38.34086 
9       10 
29.79905 33.77756
#We see that Cross-validation chooses 6-, 7- 8-, or 9- variable model as having the lowest MSE
#We now perform Backward Selection on the full data set in order to obtain 
#the6-variable model. 

reg.best=regsubsets(Winpct~.,data=teamdata ,nvmax=10, method="backward")
coef(reg.best,6)
(Intercept)          AST          STL      TORatio        Tspct 
125.9734335   -0.9702697    3.7646231   -3.4823070    5.3821421 
OppFTARate     OppFGpct 
-126.5668265   -6.3517084 

coef(reg.best,7)
(Intercept)          REB          AST          STL      TORatio 
79.2588747    0.5100890   -0.9900413    4.0849729   -3.4739406 
Tspct   OppFTARate     OppFGpct 
5.4685400 -124.8270815   -5.9645142 

> coef(reg.best,8)
(Intercept)         REB         AST         STL    ASTRatio     TORatio 
-53.547350    1.811923   -5.555354    5.689857    7.366679   -3.693101 
Tspct  OppFTARate    OppFGpct 
5.400470 -107.391884   -5.028727

#from forward
coef(reg.best,7)
 FTM,PFD

> coef(reg.best,8)
FTM, AST, PFD

#from best subset
coef(reg.best,7)
FTM, TOV , PFD

coef(reg.best,8)
FTA, AST, ASTRatio, FTARate


BestSubset, Forward, Backward
6: OREBpct, FTM, AST
7: FTM/PFD, FTM/PFD, REB/AST #but AST/PFD were NEGATIVE
8: FTA/AST/ASTRatio/FTARate, FTM/AST/PFD, REB/AST/ASTRatio
   
=======
       1        2        3        4        5        6        7        8        9 
74.58313 32.13637 31.29649 34.82345 26.15444 25.12098 19.06032 24.41770 43.71770 
      10 
39.70691

#We see that Cross-validation chooses 7-variable model as having the lowest MSE
#We now perform best subset selection on the full data set in order to obtain 
#the 7-variable model
reg.best=regsubsets(Winpct~.,data=teamdata ,nvmax=10)
coef(reg.best,7)
(Intercept)        DREB         TOV         STL          PF       Tspct   OppeFGpct 
 191.162568   -1.661630   -4.226332    2.744893    3.917890    5.183251   -6.556204 
 OppFTARate 
-288.363519 
#This matches the 7-variable from Best Subset from above

