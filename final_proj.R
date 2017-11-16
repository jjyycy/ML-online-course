#library
library(mgcv)
library(tree)
source("http://www.stat.cmu.edu/~cschafer/MSCF/cv.tree.full.txt")
library(glmnet)
library(fANCOVA)
library(MASS)
library(nnet)
library(car)
source("http://www.stat.cmu.edu/~cschafer/MSCF/CVforppr.R")
source("http://www.stat.cmu.edu/~cschafer/MSCF/CVfornnet.R")
library(randomForest)

#data
trainset = read.table("train.csv", sep=",", header=T)
fullrow = rep(FALSE,nrow(trainset))
for(i in 1:nrow(trainset))
{
  fullrow[i] = !any(is.na(trainset[i,29:147]))
}
RetsOnly = trainset[fullrow, 29:147]
varnames = c(paste("Ret_", 2:120, sep=""))
fulltrainset=trainset[fullrow,]
Ret_PlusOne=fulltrainset$Ret_PlusOne

# First 3 models
# Mode 1 - fully linear model containing only the parametric predictors
fullform = as.formula(paste("Ret_PlusOne ~ ",paste(varnames,collapse="+")))
mod1 = lm(fullform, data=trainset, subset=fullrow)
summary(mod1)
AIC1=AIC(mod1)
plot(predict(mod1),Ret_PlusOne,pch=16,cex=0.7,xlab="Predicted Value",ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=2)

# Model 2 - additive nonparametric model
# use gam model here
gamfullform = as.formula(paste("Ret_PlusOne ~ ",paste(paste("s(",varnames,")"),collapse="+")))
mod2 = gam(gamfullform, data=RetsOnly)
summary(mod2)
AIC2=AIC(mod2)
plot(predict(mod2),Ret_PlusOne,pch=16,cex=0.7,xlab="Predicted Value",
     ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=2)

# Model 3 - tree-based model
# use regression tree model here
mod3=tree(fullform,data=RetsOnly,mindev=0.01,minsize=2)
cvout=cv.tree.full(mod3,mindev=0.002)
plot(cvout)
optalpha=cvout$k[which.min(cvout$dev)]
opttree=prune.tree(mod3,k=optalpha)
plot(opttree)
text(opttree,cex=0.75)
summary(opttree)
plot(predict(opttree),Ret_PlusOne,pch=16,cex=0.7,xlab="Predicted Value",
     ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=2)

# Develop my best model
# Step 1 Variable Select
xmatrix=as.matrix(RetsOnly)
glmnetout=glmnet(xmatrix,Ret_PlusOne)
# cross validation to find lambda
cvglmout=cv.glmnet(xmatrix,Ret_PlusOne)
plot(cvglmout)
optlambda=cvglmout$lambda.1se
optilambdapos=which(cvglmout$glmnet.fit$lambda==optlambda)
glmnetout$beta[glmnetout$beta[,optilambdapos]!=0,optilambdapos]
# selected predictor data matrix
selectedx=as.data.frame(cbind(RetsOnly$Ret_4,RetsOnly$Ret_9,RetsOnly$Ret_10,RetsOnly$Ret_22,RetsOnly$Ret_23,RetsOnly$Ret_27,RetsOnly$Ret_29,RetsOnly$Ret_34,RetsOnly$Ret_36,RetsOnly$Ret_37,RetsOnly$Ret_42,RetsOnly$Ret_44,RetsOnly$Ret_49,RetsOnly$Ret_50,RetsOnly$Ret_53,RetsOnly$Ret_57,RetsOnly$Ret_62,RetsOnly$Ret_64,RetsOnly$Ret_66,RetsOnly$Ret_68,RetsOnly$Ret_72,RetsOnly$Ret_74,RetsOnly$Ret_75,RetsOnly$Ret_80,RetsOnly$Ret_85,RetsOnly$Ret_86,RetsOnly$Ret_89,RetsOnly$Ret_90,RetsOnly$Ret_100,RetsOnly$Ret_101,RetsOnly$Ret_102,RetsOnly$Ret_103,RetsOnly$Ret_104,RetsOnly$Ret_105,RetsOnly$Ret_107,RetsOnly$Ret_109,RetsOnly$Ret_113,RetsOnly$Ret_115,RetsOnly$Ret_116,RetsOnly$Ret_117,RetsOnly$Ret_119,RetsOnly$Ret_120))
# scale the data
selectedx=as.data.frame(scale(selectedx))

# Step 2 Smoothing the Variables
nonpara_fitted = matrix(nrow = 22390, ncol = 42)
newvarnames= c(paste("V", 1:42, sep=""))
for(i in 1:42)
{
  nonpara_fitted[,i] = loess(Ret_PlusOne ~ get(newvarnames[i]), data = selectedx, span = 0.5, degree = 1,parametric = FALSE,family="symmetric")$fitted
}
nonpara_fitted=as.data.frame(nonpara_fitted)
newform=as.formula(paste("Ret_PlusOne ~ ",paste(newvarnames,collapse = "+")))

# Step 3 Trying models
# Model 4 - Robust Regression
mod4 = rlm(newform,data=nonpara_fitted)
plot(predict(mod4),Ret_PlusOne,pch=16,cex=0.7,xlab="Predicted Value",
     ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=2)

# Model 5 - Projection Pursuit
pprCV = matrix(0,nrow=10,ncol=4)
for(j in 1:ncol(pprCV))
{
  set.seed(j)
  for(i in 1:nrow(pprCV))
  {
    pprCV[i,j] = CVforppr(newform, nterms=i, numfolds=10, data=nonpara_fitted, 
                        sm.method="gcvspline")
  }
}
# decide to choose M=nterms=2
mod5 = ppr(newform,nterms=2,data=nonpara_fitted)
summary(mod5)
plot(predict(mod5),Ret_PlusOne,pch=16,cex=0.7,xlab="Predicted Value",
     ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=2)

# Model 6 - Neural Network
nnetfullform = as.formula(paste("Ret_PlusOne ~ ",paste(paste("scale(",newvarnames,")"),collapse="+")))
nnetCV = array(0,dim=c(5,5,4))
decaylist = c(0,0.0001,0.001, 0.01, 0.02)
Mlist = c(5,10,15,25,50)
for(k in 1:dim(nnetCV)[[3]])
{
  set.seed(k)
  for(i in 1:length(decaylist))
  {
    for(j in 1:length(Mlist))
    {
      nnetCV[i,j,k] = CVfornnet(nnetfullform, size=Mlist[j], 
                                decay=decaylist[i], numfolds=10, data=selectedx)
    }
  }
}
#  decide to choose M=size=10, lambda=decay=0.0001
mod6 = nnet(nnetfullform,data=nonpara_fitted,size=25,linout=TRUE,decay=0.001,maxit=000,MaxNWts=8000)
plot(predict(mod6),Ret_PlusOne,pch=16,cex=0.7,xlab="Predicted Value",
     ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=2)

# Model 7 - Random Forest
tuneRF(selectedx,Ret_PlusOne)
# decide to choose mtry=14
# mod7 = randomForest(newform,data=selectedx,ntree=500,nodesize=5,mtry=14)
# after plotting mod7 above, I found that the OOB error leveled off by B=50, so using 500 trees was likely excessive
mod7 = randomForest(newform,data=selectedx,ntree=50,nodesize=5,mtry=14)
plot(predict(mod7),Ret_PlusOne,pch=16,cex=0.7,xlab="Predicted Value",
     ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=2)

# Diagnostic Plots
# plot of residuals vs. fitted values
residsmod7 = Ret_PlusOne - predict(mod7)
plot(predict(mod7), residsmod7, xlab="Fitted Values", ylab="Residuals", cex.axis=1.3, cex.lab=1.3, pch=16, cex=0.7)

# normal probability plot
qqnorm(as.numeric(residsmod7),cex.axis=1.3,cex.lab=1.3,pch=16,main="")
qqline(as.numeric(residsmod7))

# Prediction using test set
testsetsub=read.table("testsetsub.csv", sep=",", header=T)
# conduct the same data processing on testset
# select the 42 variabls out of testset
test_selectedx=as.data.frame(cbind(testsetsub$Ret_4,testsetsub$Ret_9,testsetsub$Ret_10,testsetsub$Ret_22,testsetsub$Ret_23,testsetsub$Ret_27,testsetsub$Ret_29,testsetsub$Ret_34,testsetsub$Ret_36,testsetsub$Ret_37,testsetsub$Ret_42,testsetsub$Ret_44,testsetsub$Ret_49,testsetsub$Ret_50,testsetsub$Ret_53,testsetsub$Ret_57,testsetsub$Ret_62,testsetsub$Ret_64,testsetsub$Ret_66,testsetsub$Ret_68,testsetsub$Ret_72,testsetsub$Ret_74,testsetsub$Ret_75,testsetsub$Ret_80,testsetsub$Ret_85,testsetsub$Ret_86,testsetsub$Ret_89,testsetsub$Ret_90,testsetsub$Ret_100,testsetsub$Ret_101,testsetsub$Ret_102,testsetsub$Ret_103,testsetsub$Ret_104,testsetsub$Ret_105,testsetsub$Ret_107,testsetsub$Ret_109,testsetsub$Ret_113,testsetsub$Ret_115,testsetsub$Ret_116,testsetsub$Ret_117,testsetsub$Ret_119,testsetsub$Ret_120))
test_selectedx=as.data.frame(scale(test_selectedx))

# prediction
mypreds=predict(mod7,newdata=test_selectedx)
ConvertSubmission(mypreds) 

# save file
write(mypreds, file = "jingyig1.txt", sep = "\n")
