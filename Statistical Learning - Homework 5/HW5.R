#support vectr machines
#load csv
dat1 = read.csv("forestfires.csv")
names(dat1)
dat = dat1
#load library
library(e1071)
#create training set / test set
index <- 1:nrow(dat1)
testindex <- sample(index, trunc(length(index)/3))
testset <- dat1[testindex,]
trainset <- dat1[-testindex,]
#dataset sizes
N = 517
Ntest = 172
Ntrain = 345
#what we want to predict
y <- trainset[,c(13)]
#set up dummies for classifiers
col <- rep(NA,Ntrain)
pch <- rep(NA,Ntrain)
#convert to binary classifiers
pch[y>.51111111] <- 1
pch[y<.51111111] <- -1
y = pch
#convert to factors y
y <- factor(y)
#create vector to store data for each model
testlist <- list()
count  = 1
#generate model
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Lower dimentional fit (2d grid)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
for(i in 5:12)
{
      X<-trainset[,c(i)]
      dat <- data.frame(X=X,y=as.factor(y))
      #------------------------------------------------------------------------------
      fit3.svc <- svm(y~.,data=dat,kernel="radial",cost=1)
      tune.out <- tune(svm,y~.,data=dat,kernel="radial",scale=FALSE,
                       ranges=list(cost=seq(0.1,5,length.out=10)))
      names(tune.out)
      tune.out$sampling
      tune.out$best.parameters
      best.model <- tune.out$best.model
      best.model
      is(best.model)
      testlist[[count]]<-best.model
      count = count+1
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Higher dimentional SVC (plane)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
for(i in 5:12)
{
  for(j in 5:12)
  {
    for(k in 5:12)
    {
    if (j!=i&&j!=k&&i!=k)
    {
      X<-trainset[,c(i,j,k)]
      dat <- data.frame(X=X,y=as.factor(y))
      #------------------------------------------------------------------------------
      fit3.svc <- svm(y~.,data=dat,kernel="radial",cost=1)
      tune.out <- tune(svm,y~.,data=dat,kernel="radial",scale=FALSE,
                       ranges=list(cost=seq(0.1,10,length.out=10)))
      names(tune.out)
      tune.out$sampling
      tune.out$best.parameters
      best.model <- tune.out$best.model
      is(best.model)
      testlist[[count]]<-best.model
      count = count+1
    }
    }
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#using all covariates
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
X<-trainset[,c(5,6,7,8,9,10,11,12)]
dat <- data.frame(X=X,y=as.factor(y))
#------------------------------------------------------------------------------
fit3.svc <- svm(y~.,data=dat,kernel="radial",cost=1)
tune.out <- tune(svm,y~.,data=dat,kernel="radial",scale=FALSE,
                 ranges=list(cost=seq(0.1,20,length.out=20)))
names(tune.out)
tune.out$sampling
tune.out$best.parameters
best.model <- tune.out$best.model
best.model
is(best.model)
testlist[[count]]<-best.model
count = count+1
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#now we test models against test data set 
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#vector of tuned models 
testlist
#vector of data to predict
trainset
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#what we want to predict
y2 <- dat1[,c(13)]
#set up dummies for classifiers
pch <- rep(NA,N)
#convert to binary classifiers
pch[y2>0] <- 1
pch[y2==0] <- -1
y2 = pch
#convert to factors y
y2 <- factor(y2)
grd <- expand.grid(Ntest = 517)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#generate surface plot for each prediction(accuracy, false posative rate )
#------------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#generate row matrix
C = matrix(nrow=count, ncol=3)
ac <- rep(NA,N)
fp <- rep(NA,N)
fn <- rep(NA,N)
totfp <- rep(NA,N)
totfn <- rep(NA,N)
for(i in 1:count)
{
best.model <-testlist[[i]]
newpred <- predict(best.model,grd=y2)
#accuracy
ac[y2==newpred]<- 1
ac[y2!=newpred]<- 0
accuracy <- sum(ac)/N
accuracy 
C[i, 1] <-accuracy 
#false posative rate
fp[y2==1]<- 1
fp[y2!=1]<- 0
totfp[y2!=1&newpred==1]<- 1
totfp[y2!=1&newpred!=1]<- 0
totfp[y2==1] <- 0
totfp
falsePos <- sum(totfp)/sum(fp)
falsePos
C[i, 2] <-falsePos
#false negative rate
fn[y2==1&newpred!=1]<- 1
fn[y2==1&newpred==1]<- 0
fn[y2!=1]<-0
totfn[y2!=1]<- 1
totfn[y2==1]<- 0
falseNeg <- sum(fn)/sum(totfn)
falseNeg 
C[i, 3] <- falseNeg 
}
#we want the one that best minimizes fn and accuracy. 
#return the index that was closest to min and use that. 
a <- which.max(C[,1])
a
testlist[[a]]
a <- which.min(C[,3])
a
testlist[[a]]
X<-trainset[,c(i,j,k)]
dat <- data.frame(X=X,y=as.factor(y))
#------------------------------------------------------------------------------
X<-trainset[,c(5,8)]
dat <- data.frame(X=X,y=as.factor(y))
fit3.svc <- svm(y~.,data=dat,kernel="radial",cost=1)
tune.out <- tune(svm,y~.,data=dat,kernel="radial",scale=FALSE,
                 ranges=list(cost=seq(0.1,25,length.out=25)))
names(tune.out)
tune.out$sampling
tune.out$best.parameters
best.model <- tune.out$best.model
is(best.model)
testlist[[count]]<-best.model
count = count+1

