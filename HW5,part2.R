library(mgcv)
trainset = read.table("train.csv", sep=",", header=T)
fullrow = rep(FALSE,nrow(trainset))
for(i in 1:nrow(trainset))
{
  fullrow[i] = !any(is.na(trainset[i,29:147]))
}
RetsOnly = trainset[fullrow, 29:147]
varnames = c(paste("Ret_", 2:120, sep=""))
fulltrainset=trainset[fullrow,]
Ret_PlusOne=fulltrainset[208]

gamfullform = as.formula(paste("Ret_PlusOne ~ ",paste(paste("s(",varnames,")"),collapse="+")))
fitgam = gam(gamfullform, data=RetsOnly)

testsetsub=read.table("testsetsub.csv", sep=",", header=T)
mypreds=predict(fitgam,newdata=testsetsub)
ConvertSubmission(mypreds) 


AIC(fitgam)
plot(fitgam$fit, fitgam$residuals,xlab="Fitted Values", ylab="Residuals",pch=16)
plot(predict(fitgam),Ret_PlusOne,pch=16,cex=0.7,xlab="Predicted Value",
     ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=2)

fullform = as.formula(paste("Ret_PlusOne ~ ",paste(varnames,collapse="+")))
fitppr=ppr(fullform,nterms=4,data=RetsOnly)
summary(fitppr)
plot(fitppr$fit, fitppr$residuals,xlab="Fitted Values", ylab="Residuals",pch=16)
plot(predict(fitppr),Ret_PlusOne,pch=16,cex=0.7,xlab="Predicted Value",
     ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=2)
mypreds2=predict(fitppr,newdata=testsetsub)
ConvertSubmission(mypreds2)

library(MASS)
library(nnet)
nnetfullform = as.formula(paste("Ret_PlusOne ~ ",paste(paste("scale(",varnames,")"),collapse="+")))
fitnnet=nnet(nnetfullform,data=RetsOnly,size=10,linout=TRUE,decay=0.001,maxit=1000,MaxNWts=1211)
plot(fitnnet$fit, fitnnet$residuals,xlab="Fitted Values", ylab="Residuals",pch=16)
plot(predict(fitnnet),Ret_PlusOne,pch=16,cex=0.7,xlab="Predicted Value",
     ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=2)
mypreds3=predict(fitnnet,newdata=testsetsub)
ConvertSubmission(mypreds3)
