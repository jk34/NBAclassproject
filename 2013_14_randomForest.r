modeldata<-read.csv("Teams2013_14.csv") 
Winpct<-modeldata$Winpct
set.seed(1)
formula = as.formula(Winpct~.)
rf = randomForest(formula, data=modeldata, ntree=10000, importance=TRUE)
importance(rf)

newmodeldata = data.frame(type=colnames(modeldata[-c(1)]), importance(rf), check.names=F)
newmodeldata$type = reorder(newmodeldata$type, newmodeldata$`%IncMSE`)

x11()
pdf('2013_14_rfplot.pdf')
ggplot(data=newmodeldata, aes(x=type, y=`%IncMSE`)) + geom_bar(stat='identity') + geom_hline(yintercept=abs(min(newmodeldata$`%IncMSE`)), col=2, linetype='dashed') + coord_flip()
dev.off()