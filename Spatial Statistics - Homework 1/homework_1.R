#==========================================================================
# question 2
#==========================================================================
#set working directory
setwd("/Users/wilder/Desktop/Homework1 spatial stats")
#read im file 
info = read.table("snake.txt", header = TRUE)
#set variables
info = apply(info, 1, as.numeric)
X = info[1,]
Y = info[2,]
#plot X against Y
plot(Y~X)
#create fit
fit = lm(Y~0+X)
#plot fit over data
plot(Y~X,main="Water Content vs Water Yield",xlab="Water Content",ylab="Water Yield")
abline(fit)
#plot the fit info
plot(fit)
#get 95 % confindence intervals for the data
Y.lm = lm(Y ~ 0 + X)
predict(Y.lm, X[1], interval="confidence")
# only need confidence interval for Beta
summary(fit)
confint(Y.lm, 'X', level=0.95)
summary(fit)$sigma
#==========================================================================
# question 3
#==========================================================================
# simulate I for n = 4,10,100
par(mfrow = c(3,1))
# n = 4
simulations = 4
#intiialize W
n = simulations
W = matrix(nr = n,nc = n)
W = (row(W) == col(W) + 1) + (col(W) == row(W) + 1)
I = numeric(1000)
for (ii in 1:1000)
{
  Z = rnorm(simulations,0,1)
  topm = (t(Z-mean(Z)))%*%W%*%(Z-mean(Z))
  botm = (t(Z-mean(Z)))%*%(Z-mean(Z))
  I[ii] = topm/botm
}
hist(I,main = "Historgram of I with n = 4")
# n = 10
simulations = 10
#intiialize W
n = simulations
W = matrix(nr = n,nc = n)
W = (row(W) == col(W) + 1) + (col(W) == row(W) + 1)
I = numeric(1000)
for (ii in 1:1000)
{
  Z = rnorm(simulations,0,1)
  topm = (t(Z-mean(Z)))%*%W%*%(Z-mean(Z))
  botm = (t(Z-mean(Z)))%*%(Z-mean(Z))
  I[ii] = topm/botm
}
hist(I,main = "Historgram of I with n = 10")
# n = 100
simulations = 100
n = simulations
#intiialize W
W = matrix(nr = n,nc = n)
W = (row(W) == col(W) + 1) + (col(W) == row(W) + 1)
I = numeric(1000)
for (ii in 1:1000)
{
  Z = rnorm(simulations,0,1)
  topm = (t(Z-mean(Z)))%*%W%*%(Z-mean(Z))
  botm = (t(Z-mean(Z)))%*%(Z-mean(Z))
  I[ii] = topm/botm
}
hist(I,main = "Historgram of I with n = 100")










