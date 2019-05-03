modeldata<-read.csv("Teams2013_14.csv") 
modeldata<-modeldata[,names(modeldata)[-c(1,5)]]
y=modeldata$Winpct
x<-modeldata[,names(modeldata)[-c(1)]]
x<-as.matrix(x)
fit.lasso=glmnet(x,y)
cv.lasso=cv.glmnet(x,y)

x11()
pdf('Lasso_lambda.pdf')
plot(fit.lasso, xvar="lambda", label=TRUE)
dev.off()
x11()
pdf('Lasso_CV.pdf')
plot(cv.lasso)
dev.off()


fit.ridge=glmnet(x,y,alpha=0)
cv.ridge=cv.glmnet(x,y,alpha=0)
x11()
pdf('Ridge_lambda.pdf')
plot(fit.ridge, xvar="lambda",label=TRUE)
dev.off()
x11()
pdf('Ridge_cv.pdf')
plot(cv.ridge, xvar="lambda",label=TRUE)
dev.off()