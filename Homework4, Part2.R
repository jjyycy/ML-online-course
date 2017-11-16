library(fANCOVA)
trainset = read.table("/Users/apple/Desktop/ML/train.csv", sep=",", header=T)
fullrow = rep(FALSE,nrow(trainset))
for(i in 1:nrow(trainset))
{
  fullrow[i] = !any(is.na(trainset[i,29:147]))
}
varnames = c(paste("Ret_",2:120, sep = ""))


nonpara_fitted = matrix(nrow = 22390, ncol = 119)
for(i in 1:119)
{
  nonpara_fitted[,i] = loess(Ret_PlusOne ~ get(varnames[i]), data = trainset, subset = fullrow, span = 0.4, degree = 1,parametric = FALSE)$fitted
}
nonpara_fitted=as.data.frame(nonpara_fitted)


y=trainset[208][fullrow,]
newvarnames=c(paste("V",1:119, sep = ""))
# Here V1, V2, ..., V119 refer to nonparametric fitted values of Ret_2, Ret_3, ..., 
# Ret_120 respectively, and y refer to Ret_PlusOne.
newform=as.formula(paste("y ~ ",paste(newvarnames,collapse = "+")))
lmfitmodel = lm(newform,data=nonpara_fitted)
summary(lmfitmodel)
AIC(lmfitmodel)

