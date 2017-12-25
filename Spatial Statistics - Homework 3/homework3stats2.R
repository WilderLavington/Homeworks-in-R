##
## Problem 3: homework 3, Spatial stats 
## (Remote sensing temperature estimates)
#==========================================================================
# (a) Based on only the training data, provide a binned semivariogram. Does it look like
# there is evidence of a nugget effect?
#==========================================================================
library(fields)
library(maps)
load("SatelliteExample.RData")
grd <- as.matrix(cbind(lon,lat))
#Empirical variogram
v <- vgram(loc=grd,y=T, N=30, lon.lat=TRUE)
#plot variogram
plot(v$stats["mean",]~v$centers,main="Binned Semi-variogram",xlab="Bin centers", ylab=" Mean values")
#==========================================================================
# (b) If you believe there is a mean trend, remove it and plot a binned semivariogram based
# on the detrended data. Is there evidence of a nugget effect?
#==========================================================================
# fit linear model 
fit <- lm(T~lat+lon)
plot(T~lat+lon)
#create variogram based on residuals
v_new <- vgram(loc=grd,y=fit$residuals, N=30, lon.lat=TRUE)
#plot variogram without mean trend
plot(v_new$stats["mean",]~v_new$centers,main="Binned Semi-variogram, without mean trend",xlab="Bin centers", ylab=" Mean values")
#==========================================================================
#(c) Decide on an appropriate statistical model for the training data. Estimate any spatial
# parameters using the empirical semivariogram. Include the fitted line on the binned
# empirical semivariogram.
#==========================================================================
ns <- 15 # number of locations 
grd <- seq(0,20,length.out=ns) #spatial grid
# fit covariance model
exponential_ <- function(coeffs,r){
  val = coeffs[[1]]^2*(1 - exp(-r/coeffs[[2]])) + coeffs[[3]]^2
  return(val)
}
#initialize cost function variables
cost_function_variogram <- v_new
y <- v_new$stats["mean", ]
x <- v_new$centers
N <- v_new$stats["N", ]
#cost function
cost_function <- function(coeffs){
  #find number of pionts
  dimention <- length(v$centers)
  #set counter
  current_error <- 0
  for (ii in 1:dimention){
    true_val <- exponential_(coeffs,x[ii])
    current_val <- y[ii]
    if (!is.na(current_val)){ # includes nugget effect and weighted least squares
      current_error = current_error + N[ii]/y[ii]*(true_val-current_val)^2 
    }
  }
  return(current_error)
}
#nlm to fit parameters
coeff_guess <- nlm(cost_function,c(1,20,2))
vals <- numeric(495)
#re-plot over data
for (ii in 1:500){
  vals[ii] <- exponential_(c(coeff_guess$estimate),ii)
}
plot(v_new$stats["mean",]~v_new$centers,main="Binned Semi-variogram, without mean trend",xlab="Bin centers", ylab=" Mean values")
lines(vals)

#==========================================================================
#(d)  Krige the training data to the testing locations and produce a quilt plot (similar to
# below) based on both the kriged data and also on the held-out true data (T.test).
# How well do your kriged estimates reproduce the overall structure in the held-out data?
#==========================================================================
load("SatelliteExample.RData")
pred.grd <- as.matrix(cbind(lon.test,lat.test))
lon.lat <- cbind(lon.test,lat.test)
grd <- as.matrix(cbind(lon,lat))
sigma <- coeff_guess$estimate[1]
a <- coeff_guess$estimate[2]
nugget <- coeff_guess$estimate[3]
## Matrices for kriging
dist0.mat <- rdist.earth(grd,pred.grd)
Sigma0 <- exponential_(coeff_guess$estimate,dist0.mat)
dist.mat <- rdist.earth(grd)
Sigma <- exponential_(coeff_guess$estimate,dist.mat)
## Simple kriging predictor
T.test.hat <- t(Sigma0) %*% solve(Sigma) %*% fit$residuals
## add back in mean 
mean_func <- fit$coefficients[1] + fit$coefficients[2]*lat.test + fit$coefficients[3]*lon.test
T.test.hat <- T.test.hat+mean_func
#plot
par(mfrow=c(1,2))
quilt.plot(pred.grd,T.test,zlim=c(30,55),main = "Test Data")
quilt.plot(pred.grd,T.test.hat,zlim=c(30,55), main = "Kriging Prediction")
error = abs(T.test.hat - T.test)
quilt.plot(pred.grd,error,zlim=c(30,55),main = "Error")
#==========================================================================
#(e) At each prediction location in the test data, you have a predictive uncertainty based
# on your model. Calculate the coverage percentage of the 95% prediction interval – do
# these intervals contain the truth about 95% of the time?
#==========================================================================
## Kriging variance and standard error
T.test.hat.var <- ((sigma)^2 + (nugget)^2 - diag(t(Sigma0) %*% solve(Sigma) %*% Sigma0))
T.test.hat.var[T.test.hat.var < 0] <- 0
T.test.hat.se <- sqrt(T.test.hat.var)
T.test.hat.up <- T.test.hat + 1.96 * T.test.hat.se
T.test.hat.dn <- T.test.hat - 1.96 * T.test.hat.se
#plot variance over grid 
quilt.plot(pred.grd,T.test.hat.var,main = "Variance of estimated Krige")
par(mfrow=c(1,2))
# plot upper guess 
quilt.plot(pred.grd,T.test.hat.up,zlim=c(30,55),main = "Upper bound")
# plot lower guess
quilt.plot(pred.grd,T.test.hat.dn,zlim=c(30,55),main = "Lower bound")
#calculate percent coverage
upper_bound <- T.test.hat.up - T.test
lower_bound <- T.test- T.test.hat.dn
#find percentage of pionts
(sum(  upper_bound < 0  ) + sum(  lower_bound < 0  ))/ (length(T.test))
par(mfrow=c(1,2))
#quilt plot upper/upper
quilt.plot(pred.grd,T.test.hat.up, main= "Upper bound of 95 % confidence interval")
quilt.plot(pred.grd,T.test.hat.dn, main= "Upper bound of 95 % confidence interval")
#quilt plot differences 
par(mfrow=c(1,2))
quilt.plot(pred.grd,abs(lower_bound),main= "Differrence between lower bound and Test")
quilt.plot(pred.grd,upper_bound, main= "Differrence between upper bound and Test")
#finally plot error 
error = abs(T.test.hat - T.test)
quilt.plot(grd,error,zlim=c(30,55),main = "Error")
#==========================================================================
#(e) (f) Produce a gridded map of predicted temperatures, along with associated 
# standard errors.
#==========================================================================
#set up grid to krige on 
temp1 <- lon.test
temp2 <- lat.test
lon.test = seq(min(lon), max(lon),length.out = 100)
lat.test = seq(min(lat), max(lat),length.out = 100)
pred.grd <- as.matrix(expand.grid(lon.test,lat.test))
lon.lat <- cbind(lon.test,lat.test)
grd <- as.matrix(cbind(lon,lat))
sigma <- coeff_guess$estimate[1]
a <- coeff_guess$estimate[2]
nugget <- coeff_guess$estimate[3]
## Matrices for kriging
dist0.mat <- rdist.earth(grd,pred.grd)
Sigma0 <- exponential_(coeff_guess$estimate,dist0.mat)
dist.mat <- rdist.earth(grd)
Sigma <- exponential_(coeff_guess$estimate,dist.mat)
## Simple kriging predictor
T.test.hat <- t(Sigma0) %*% solve(Sigma) %*% fit$residuals
## add back in mean 
mean_func <- fit$coefficients[1] + fit$coefficients[2]*lat.test + fit$coefficients[3]*lon.test
T.test.hat <- T.test.hat+mean_func
#plot
quilt.plot(grd,T,zlim=c(30,55),main = "Training Data")
quilt.plot(pred.grd,T.test.hat,zlim=c(30,55), main = "Gridded Plot")
lon.test <- temp1 
lat.test <- temp2
#plotting
T.test.hat.var <- ((sigma)^2 + (nugget)^2 - diag(t(Sigma0) %*% solve(Sigma) %*% Sigma0))
T.test.hat.var[T.test.hat.var < 0] <- 0
T.test.hat.se <- sqrt(T.test.hat.var)
T.test.hat.up <- T.test.hat + 1.96 * T.test.hat.se
T.test.hat.dn <- T.test.hat - 1.96 * T.test.hat.se
quilt.plot(pred.grd,T.test.hat.se, main = "Standard Error")
quilt.plot(pred.grd,T.test.hat.var,main = "Variance of estimated Krige")
par(mfrow=c(1,2))
# plot upper guess 
quilt.plot(pred.grd,T.test.hat.up,zlim=c(30,55),main = "Upper bound")
# plot lower guess
quilt.plot(pred.grd,T.test.hat.dn,zlim=c(30,55),main = "Lower bound")
#calculate percent coverage
upper_bound <- T.test.hat.up - T.test
lower_bound <- T.test- T.test.hat.dn
#find percentage of pionts
(sum(  upper_bound < 0  ) + sum(  lower_bound < 0  ))/ (length(T.test))
par(mfrow=c(1,2))
#quilt plot upper/upper
quilt.plot(pred.grd,T.test.hat.up, main= "Upper bound of 95 % confidence interval")
quilt.plot(pred.grd,T.test.hat.dn, main= "Upper bound of 95 % confidence interval")
#quilt plot differences 
par(mfrow=c(1,2))
quilt.plot(pred.grd,abs(lower_bound),main= "Differrence between lower bound and Test")
quilt.plot(pred.grd,upper_bound, main= "Differrence between upper bound and Test")


