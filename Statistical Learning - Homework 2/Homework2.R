#read in the data
mydata = read.csv("ChessRatingComparison.csv")
is(mydata)
names(mydata)

################################################################################################
## Simple regression 
################################################################################################
#plot pairwise comparisons
ChessRate <- pairs(mydata[7:3])
#plot of USCF vs Quick rating
summary(mydata[7:3])
plot(USCF.Regular.Rating~Chess.com.Live.Standard.Rating,data=mydata)
#simple linear regression 
mydata.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Standard.Rating,data=mydata)
#summery of data
summary(mydata.lm)
#create fitlines 
#pairwise complete 
plot(USCF.Regular.Rating~Chess.com.Live.Standard.Rating,data=mydata)
abline(coef(mydata.lm))
################################################################################################
## Simple regression diagnostics
################################################################################################

## Residuals vs. fitted values
plot(mydata.lm$resid~mydata.lm$fitted,main = "Residuals vs. fitted values")
abline(h=0)
##qq plots
hist(mydata.lm$resid)
qqnorm(mydata.lm$resid)
qqline(mydata.lm$resid)
#confidence interval
## CI for slope
confint(mydata.lm, parm=2, level=0.95)
##Leverage statistics. 

##check which rows have missing data
mydata[!complete.cases(mydata),]
##remove NULL data from USCF.Quick.Rating 
newdata0 <- matrix(c(mydata[,3], mydata[,7]), nrow = 410,ncol = 2)
newdata <- na.omit(newdata0)
##remove NULL data from USCF.Quick.Rating
leverages <- 1/50 + (newdata$newdata[,7] - mean(newdata$newdata[,7] ))^2 /
  sum( (newdata$newdata[,7] - mean(newdata$newdata[,7] ))^2 )
##or duck that, theres a built in
cbind(leverages,hatvalues(mydata.lm))
cbind(sort(hatvalues(mydata.lm)))
##
t(hatvalues(mydata.lm))
##find index and max
max(hatvalues(mydata.lm))
which.max(hatvalues(mydata.lm))
##remove
newdata = mydata.lm
newdata = mydata[-339,]
newdata = mydata[-272,]

################################################################################################
## new regression, without high leverage pionts
################################################################################################

#plot pairwise comparisons
ChessRate <- pairs(newdata[7:3])
#plot of USCF vs Quick rating
summary(newdata[7:3])
plot(USCF.Regular.Rating~Chess.com.Live.Standard.Rating,data=newdata)
#simple linear regression 
newdata.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Standard.Rating,data=newdata)
#summery of data
summary(newdata.lm)
hatvalues(newdata)#create fitlines 
#pairwise complete 
plot(USCF.Regular.Rating~Chess.com.Live.Standard.Rating,data=newdata)
abline(coef(newdata.lm))
abline(coef(mydata.lm))
## recalculate hat values 
cbind(sort(hatvalues(newdata.lm)))
##max leverage piont is not significatly higher then others so I will keep it

##redo diagnostics
## Residuals vs. fitted values
plot(newdata.lm$resid~newdata.lm$fitted,main = "Residuals vs. fitted values")
abline(h=0)
##qq plots
hist(newdata.lm$resid)
qqnorm(newdata.lm$resid)
qqline(newdata.lm$resid)
#confidence interval
## CI for slope
confint(newdata.lm, parm=2, level=0.95)

################################################################################################
## Prediction
################################################################################################

## generate prediction plus bouds

newdata1 = data.frame(Chess.com.Live.Standard.Rating=1700)
pred1 = predict(newdata.lm, newdata1, interval="predict") 

##Plot
summary(newdata)

plot(newdata1)
lines(newdata1,pred1[,1],col="blue",lwd=2,lty=2)
lines(newdata1,pred1[,3],col="blue",lwd=2,lty=2)

################################################################################################
## new regression, without poblem pionts
################################################################################################
#nextdata = newdata[-272,]
plot(USCF.Regular.Rating~Chess.com.Live.Standard.Rating,data=nextdata)
#simple linear regression 
nextdata.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Standard.Rating,data=nextdata)
#summery of data
summary(nextdata.lm)
#create fitlines 
#pairwise complete 
plot(USCF.Regular.Rating~Chess.com.Live.Standard.Rating,data=nextdata)
abline(coef(nextdata.lm))
abline(coef(newdata.lm))
abline(coef(mydata.lm))
## recalculate hat values 
cbind(leverages,hatvalues(nextdata.lm))
cbind(sort(hatvalues(nextdata.lm)))

## Residuals vs. fitted values
plot(nextdata.lm$resid~nextdata.lm$fitted,main = "Residuals vs. fitted values")
abline(h=0)
##qq plots
hist(nextdata.lm$resid)
qqnorm(nextdata.lm$resid)
qqline(nextdata.lm$resid)
#confidence interval
## CI for slope
confint(nextdata.lm, parm=2, level=0.95)

nextdata1 = data.frame(Chess.com.Live.Standard.Rating=1700)
pred2 = predict(nextdata.lm, nextdata1, interval="predict") 

summary(pred2)

################################################################################################
## Now check which of the other stats to see which has the best prediction
################################################################################################

summary(mydata)
##Chess.com.Live.Blitz.Rating
plot(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=mydata)
mydata.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=mydata)
plot(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=mydata)
abline(coef(mydata.lm))
summary(mydata.lm)
#
cbind(sort(hatvalues(mydata.lm)))
somedat1 = mydata[-272,]
#
plot(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=somedat1)
somedat1.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=somedat1)
plot(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=somedat1)
abline(coef(somedat1.lm))
abline(coef(mydata.lm))
summary(somedat1.lm)

##Chess.com.Turn.Based.Standard
plot(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=mydata)
mydata.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=mydata)
plot(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=mydata)
abline(coef(mydata.lm))
summary(mydata.lm)
#
cbind(sort(hatvalues(mydata.lm)))
somedat2 = mydata[-293,]
#
plot(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=somedat2)
somedat2.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=somedat2)
plot(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=somedat2)
abline(coef(somedat2.lm))
summary(somedat2.lm)
##USCF.Quick.Rating
plot(USCF.Regular.Rating~USCF.Quick.Rating,data=mydata)
mydata.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=mydata)
plot(USCF.Regular.Rating~USCF.Quick.Rating,data=mydata)
abline(coef(mydata.lm))
summary(mydata.lm)
#
cbind(sort(hatvalues(mydata.lm)))
somedat3 = mydata[-272,]
plot(USCF.Regular.Rating~USCF.Quick.Rating,data=somedat3)
somedat3.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating,data=somedat3)
plot(USCF.Regular.Rating~USCF.Quick.Rating,data=somedat3)
abline(coef(somedat3.lm))
abline(coef(mydata.lm))
summary(somedat3.lm)

##FIDE.Regular.Rating
plot(USCF.Regular.Rating~FIDE.Regular.Rating,data=mydata)
mydata.lm <- lm(USCF.Regular.Rating~FIDE.Regular.Rating,data=mydata)
plot(USCF.Regular.Rating~FIDE.Regular.Rating,data=mydata)
abline(coef(mydata.lm))
summary(mydata.lm)
cbind(sort(hatvalues(mydata.lm)))

#
cbind(sort(hatvalues(mydata.lm)))
somedat4 = mydata[-c(272,328,304),]
somedat4.lm <- lm(USCF.Regular.Rating~FIDE.Regular.Rating,data=somedat4)
plot(USCF.Regular.Rating~FIDE.Regular.Rating,data=somedat4)
cbind(sort(hatvalues(somedat4.lm)))
abline(coef(mydata.lm))
abline(coef(somedat4.lm))



plot(USCF.Regular.Rating~FIDE.Regular.Rating,data=somedat3)
somedat3.lm <- lm(USCF.Regular.Rating~FIDE.Regular.Rating,data=somedat3)
plot(USCF.Regular.Rating~FIDE.Regular.Rating,data=somedat3)
abline(coef(somedat3.lm))
abline(coef(mydata.lm))



##Chess.com.Live.Bullet.Rating
plot(USCF.Regular.Rating~Chess.com.Live.Bullet.Rating,data=mydata)
mydata.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Bullet.Rating,data=mydata)
plot(USCF.Regular.Rating~Chess.com.Live.Bullet.Rating,data=mydata)
abline(coef(mydata.lm))
summary(mydata.lm)
#
cbind(sort(hatvalues(mydata.lm)))
somedat4 = mydata[-c(304,100,85),]
cbind(sort(hatvalues(somedat.lm)))
#
plot(USCF.Regular.Rating~Chess.com.Live.Bullet.Rating,data=somedat4)
somedat4.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Bullet.Rating,data=somedat4)
plot(USCF.Regular.Rating~Chess.com.Live.Bullet.Rating,data=somedat4)
abline(coef(somedat4.lm))
abline(coef(mydata.lm))







