################################################################################################
## A simple spam classifier: logistic regression
################################################################################################
################################################################################################
## Homework 4
################################################################################################
load("spam.RData")
install.packages("pROC")
install.packages("ROCR")
install.packages("rms")
library(pROC)
library(DiagnosisMed)
library(rms)
library(ROCR)
ls()

names(dat)

table(train)

n <- dim(dat)[1]

## Take a look at some data
summary(dat[,1:5])
table(dat$spam)

##capTot
par(mfrow=c(2,2))
plot(spam~capTot,data=dat)
plot(spam~log(capTot),data=dat)
boxplot(capTot~spam,data=dat)
boxplot(capTot~spam,data=dat,outline=FALSE)

##capAvg
par(mfrow=c(2,2))
plot(spam~capAvg,data=dat)
plot(spam~log(capAvg),data=dat)
boxplot(capAvg~spam,data=dat)
boxplot(capAvg~spam,data=dat,outline=FALSE)

##credit
par(mfrow=c(2,2))
plot(spam~credit,data=dat)
plot(spam~log(credit),data=dat)
boxplot(credit~spam,data=dat)
boxplot(credit~spam,data=dat,outline=FALSE)

##punc_dollar
par(mfrow=c(2,2))
plot(spam~credit,data=dat)
plot(spam~log(credit),data=dat)
boxplot(credit~spam,data=dat)
boxplot(credit~spam,data=dat,outline=FALSE)

################################################################################################
## Choose which to use -> use prediction to check which one worked
################################################################################################

## Set up testing data
spam.test <- dat[!train,]$spam
par(mfrow=c(2,2))
##
## Logistic regression on capTot
fit <- glm(spam~capTot,data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y, main = "capTot")
##
## Logistic regression on capAvg
fit <- glm(spam~capAvg,data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y, main = "capAvg")
##
## Logistic regression on capAvg
fit <- glm(spam~credit,data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y, main = "credit")
##
## Logistic regression on capAvg
fit <- glm(spam~punc_dollar,data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y, main = "punc_dollar")
## that wasnt that great. 
fit <- glm(spam~capLong,data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y, main = "capLong")

################################################################################################
##final set of covariates
################################################################################################
## Set up testing data
spam.test <- dat[!train,]$spam
par(mfrow=c(2,2))
##
## Logistic regression on capTot
fit <- glm(spam~capTot,data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y, main = "capTot")
##
## Logistic regression on capAvg
fit <- glm(spam~capAvg,data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y, main = "capAvg")
#
##caplong
fit <- glm(spam~capLong,data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y, main = "capLong")
#
## Logistic regression on capAvg
fit <- glm(spam~punc_dollar,data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y, main = "punc_dollar")

################################################################################################
##Checking combined list of covariates
################################################################################################
fit <- glm(spam~(capLong+punc_dollar+remove+receive+money),data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y, main = "Regression B")

##A
fit <- glm(spam~(capLong),data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y, main = "Regression A")
################################################################################################
##Creating a confution matrix A
################################################################################################
#model A confusion matrix
## Predicted probabilities
fit <- glm(spam~(capLong),data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
## Predicted probabilities
fit.probs <- predict(fit,newdata=dat[!train,],type="response")
cbind(dat[!train,]$capLong,fit.probs)[1:10,]
plot(fit.probs~dat[!train,]$capLong)
table(fit.pred,spam.test)
## Classification rule
fit.pred <- rep(0,sum(!train))
fit.pred[fit.probs > 0.5] <- 1
## Confusion matrix
table(fit.pred,spam.test)
# proportion of correct predictions
mean(fit.pred == spam.test)
# error rate
mean(fit.pred != spam.test)
# True positive rate
mean(fit.pred[spam.test == 1])
# False positive rate
mean(fit.pred[spam.test == 0])
################################################################################################
##Creating a confusion matrix B
################################################################################################
fit <- glm(spam~(capLong+punc_dollar+remove+receive+money),data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
fit.probs <- predict(fit,newdata=dat[!train,],type="response")
cbind(dat[!train,]$fit,fit.probs)[1:20,]
#plot(fit.probs~dat[!train,]$capLong)
## Classification rule
fit.pred <- rep(0,sum(!train))
fit.pred[fit.probs > 0.5] <- 1
## Confusion matrix
table(fit.pred,spam.test)
# proportion of correct predictions
mean(fit.pred == spam.test)
# error rate
mean(fit.pred != spam.test)
# True positive rate
mean(fit.pred[spam.test == 1])
# False positive rate
mean(fit.pred[spam.test == 0])
################################################################################################
##Produce ROC curves for both model A and model B
################################################################################################
##
## ROC curve for A
##
fit <- glm(spam~(capLong),data=dat,family=binomial,subset=train) # family = binomial => logistic
fit.probs <- predict(fit,newdata=dat[!train,],type="response")
plot(c(0,1),c(0,1),xlim=c(0,1),ylim=c(0,1),type="l",xlab="FPR",ylab="TPR")
ROC.point <- function(p){
  cr <- rep(0,length(spam.test))
  cr[fit.probs > p] <- 1
  TPR <- sum(cr == 1 & spam.test == 1) / sum(spam.test == 1)
  FPR <- sum(cr == 1 & spam.test == 0) / sum(spam.test == 0)
  return(c(FPR,TPR))
}
ROC.point(0.5)

p <- 0.5
points(ROC.point(p)[1],ROC.point(p)[2])

p <- 0.25
points(ROC.point(p)[1],ROC.point(p)[2])

for(p in seq(0,1,by=0.01)){
  text(ROC.point(p)[1],ROC.point(p)[2],labels=p,cex=0.5)
  points(ROC.point(p)[1],ROC.point(p)[2])
}
##
## ROC curve for B
##
fit <- glm(spam~(capLong+punc_dollar+remove+receive+money),data=dat,family=binomial,subset=train) # family = binomial => logistic
fit.probs <- predict(fit,newdata=dat[!train,],type="response")
plot(c(0,1),c(0,1),xlim=c(0,1),ylim=c(0,1),type="l",xlab="FPR",ylab="TPR")
ROC.point <- function(p){
  cr <- rep(0,length(spam.test))
  cr[fit.probs > p] <- 1
  TPR <- sum(cr == 1 & spam.test == 1) / sum(spam.test == 1)
  FPR <- sum(cr == 1 & spam.test == 0) / sum(spam.test == 0)
  return(c(FPR,TPR))
}
ROC.point(0.5)

p <- 0.5
points(ROC.point(p)[1],ROC.point(p)[2])

p <- 0.25
points(ROC.point(p)[1],ROC.point(p)[2])

for(p in seq(0,1,by=0.01)){
  text(ROC.point(p)[1],ROC.point(p)[2],labels=p,cex=0.5)
  points(ROC.point(p)[1],ROC.point(p)[2])
}

################################################################################################
###calculate AUC for both models
################################################################################################
#AUC model A
fit <- glm(spam~(capLong),data=dat,family=binomial,subset=train) # family = binomial => logistic
fit.probs <- predict(fit,newdata=dat[!train,],type="response")
# True positive rate
M1 = mean(fit.pred[spam.test == 1])
# False positive rate
M2 = mean(fit.pred[spam.test == 0])
M2/M1
#AUC model B
fit <- glm(spam~(capLong+punc_dollar+remove+receive+money),data=dat,family=binomial,subset=train) # family = binomial => logistic
fit.probs <- predict(fit,newdata=dat[!train,],type="response")



################################################################################################
###find optimal thresholds
################################################################################################

################################################################################################
###Repeat part C
################################################################################################
#model A
fit <- glm(spam~(capLong),data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
fit.probs <- predict(fit,newdata=dat[!train,],type="response")
cbind(dat[!train,]$fit,fit.probs)[1:20,]
#plot(fit.probs~dat[!train,]$capLong)
## Classification rule
fit.pred <- rep(0,sum(!train))
fit.pred[fit.probs > 0.42] <- 1
## Confusion matrix
table(fit.pred,spam.test)
# proportion of correct predictions
mean(fit.pred == spam.test)
# error rate
mean(fit.pred != spam.test)
# True positive rate
mean(fit.pred[spam.test == 1])
# False positive rate
mean(fit.pred[spam.test == 0])
#Mode B
fit <- glm(spam~(capLong+punc_dollar+remove+receive+money),data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)
fit.probs <- predict(fit,newdata=dat[!train,],type="response")
cbind(dat[!train,]$fit,fit.probs)[1:20,]
#plot(fit.probs~dat[!train,]$capLong)
## Classification rule
fit.pred <- rep(0,sum(!train))
fit.pred[fit.probs > 0.45] <- 1
## Confusion matrix
table(fit.pred,spam.test)
# proportion of correct predictions
mean(fit.pred == spam.test)
# error rate
mean(fit.pred != spam.test)
# True positive rate
mean(fit.pred[spam.test == 1])
# False positive rate
mean(fit.pred[spam.test == 0])
################################################################################################
###Find Brier scores
################################################################################################

#model A
fit <- glm(spam~(capLong),data=dat,family=binomial,subset=train) # family = binomial => logistic
fit.probs <- predict(fit,newdata=dat[!train,],type="response")

mean( (dat[!train,]$spam - fit.probs)^2 )


#model B
fit <- glm(spam~(capLong+punc_dollar+remove+receive+money),data=dat,family=binomial,subset=train) # family = binomial => logistic
fit.probs <- predict(fit,newdata=dat[!train,],type="response")

mean( (dat[!train,]$spam - fit.probs)^2 )
