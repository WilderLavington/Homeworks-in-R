#============================================================================
# Test 2: Spatial Statistcs
#============================================================================
#============================================================================
#Auxiliary functions
#============================================================================
# loss function # 1
ell_1 <- function(p){
  # as a function of log(a)
  Sigma.c <- t(chol(Matern(dist.mat,range=p[1],nu=p[2]))) # lower triangular
  out <- forwardsolve(Sigma.c,z)
  quad.form <- sum(out^2)
  det.part <- 2*sum(log(diag(Sigma.c)))
  # up to the normalizing constants
  return (det.part + quad.form)
}
# loss function # 2
ell_2 <- function(p){
  # as a function of log(a)
  Sigma.c <- t(chol(Matern(dist.mat,range=p[1],nu=p[2]))) # lower triangular
  out <- forwardsolve(Sigma.c,z)
  quad.form <- sum(out^2)
  det.part <- 2*sum(log(diag(Sigma.c)))
  # up to the normalizing constants
  return (det.part + quad.form)
}
# covariance models
exponential_ <- function(coeffs,r){
  val = coeffs[[1]]^2*(1 - exp(-r/coeffs[[2]])) + coeffs[[3]]^2
  return(val)
}
exponential_2 <- function(coeffs,r){
  val = coeffs[1]^2*(exp(-r/coeffs[2])) + diag(dim(dist.mat)[1])*coeffs[3]^2
  return(val)
}
###================================================================================
#Packages
###================================================================================
library(fields)
library(maps)
###================================================================================
#(1) How useful is the Cholesky Decomposition
###================================================================================
v = c(100,500,1500,2000,2500,3000)
#(a,1) det(sigma)
det_sigma_1 <- rep(0,6)
for (ii in 1:6){
  ns <- v[ii] # number of locations to simulate
  grd <- seq(0,10,length.out=ns) #spatial grid
  x <- seq(0,10,length.out=ns) #sample vector for kriging
  a <- 1 # parameter for exponential covariance
  tau <- 2 # nugget 
  sigma <- sd(rdist(grd,x)) #standard dev
  Sigma <- sigma^2 * exp(-rdist(grd)/a) + tau^2
  # Start the clock!
  ptm <- proc.time()
  # Calculation 
  determinant(Sigma,log = TRUE)$mod
  # Stop the clock
  det_sigma_1[ii] <- proc.time() - ptm
}
plot(v,det_sigma_1,main = "Run Time of Method 1, a",ylab = "Run Time (sec)",xlab = "Martix Dimention")
lines(v,det_sigma_1)
#(a,2) det(sigma)
det_sigma_2 <- rep(0,6)
for (ii in 1:6){
  ns <- v[ii] # number of locations to simulate
  grd <- seq(0,10,length.out=ns) #spatial grid
  x <- seq(0,10,length.out=ns) #sample vector for kriging
  a <- 1 # parameter for exponential covariance
  tau <- 2 # nugget 
  sigma <- sd(rdist(grd,x)) #standard dev
  Sigma <- sigma^2 * exp(-rdist(grd)/a) + tau^2
  # Start the clock!
  ptm <- proc.time()
  # Calculation 
  2*sum(log(diag(chol(Sigma))))
  # Stop the clock
  det_sigma_2[ii] <- proc.time() - ptm
}
plot(v,det_sigma_2,main = "Run Time of Method 1, b",ylab = "Run Time (sec)",xlab = "Martix Dimention")
lines(v,det_sigma_2)
# look at ratio
q <- det_sigma_1/det_sigma_2
plot(v,q,main = "Ratio of Run Time of Method a vs b",ylab = "Run Time (sec)",xlab = "Martix Dimention")
lines(v,q)
#(b,1) y^t sigma^-1 y
det_1 <- rep(0,6)
for (ii in 1:6){
  ns <- v[ii]+1 # number of locations to simulate
  grd <- seq(0,10,length.out=ns) #spatial grid
  x <- seq(0,10,length.out=ns) #sample vector for kriging
  a <- 1 # parameter for exponential covariance
  tau <- 2 # nugget 
  sigma <- sd(rdist(grd,x)) #standard dev
  Sigma <- sigma^2 * exp(-rdist(grd)/a) + tau^2
  z <- rnorm(ns, mean = 0, sd = 1);
  # Start the clock!
  ptm <- proc.time()
  # Calculation 
  t(z)%*%solve(Sigma)%*%z
  # Stop the clock
  det_1[ii] <- proc.time() - ptm
}
plot(v,det_1,main = "Run Time of Method 2, a",ylab = "Run Time (sec)",xlab = "Martix Dimention")
lines(v,det_1)
#(b,2) y^t sigma^-1 y
det_2 <- rep(0,6)
for (ii in 1:6){
  ns <- v[ii] # number of locations to simulate
  grd <- seq(0,10,length.out=ns) #spatial grid
  x <- seq(0,10,length.out=ns) #sample vector for kriging
  a <- 1 # parameter for exponential covariance
  tau <- 2 # nugget 
  sigma <- sd(rdist(grd,x)) #standard dev
  Sigma <- sigma^2 * exp(-rdist(grd)/a) + tau^2
  z <- rnorm(ns, mean = 0, sd = 1)
  # Start the clock!
  ptm <- proc.time()
  # Calculation
  sum(forwardsolve(t(chol(Sigma)),z)^2)
  # Stop the clock
  det_2[ii] <- proc.time() - ptm
}
plot(v,det_2,main = "Run Time of Method 2, b",ylab = "Run Time (sec)",xlab = "Martix Dimention")
lines(v,det_2)
# look at ratio
q <- det_1/det_2
plot(v,q,main = "Ratio of Run Time of Method a vs b",ylab = "Run Time (sec)",xlab = "Martix Dimention")
lines(v,q)
###================================================================================
#(2) Matern Estimation
###================================================================================
set.seed(3)
#(a) Simulate Gaussian process interval 0,10 then find mles
#simulate Gaussian proccess with matern covariance
ns <- 50 # number of locations to simulate
grd <- seq(0,10,length.out=ns) #spatial grid
dist.mat <- rdist(grd) 
# covariance matricese
Sigma <-  Matern(dist.mat,range=0.2,smoothness=1.5)
Sigma_c <- chol(Sigma) # Cholesky factor in R
z <- t(Sigma_c) %*% rnorm(ns)
plot(z~grd, main = "Simulation of Matern Covariance: (0,10)", ylab = "simulated value", xlab = "grid values", ylim = c(-4,4))
lines(z~grd)
###
#dist.mat <- rdist(grd)
# optimize
out <- optim(par=c(0.5,1),fn=ell_1,hessian=TRUE,method="L-BFGS-B",lower=c(0.01,0.01),
             upper=c(1,2))
out$par
#plot optimized parameters
Sigma <-  Matern(dist.mat,range=out$par[1],smoothness=out$par[2])
Sigma_c <- chol(Sigma) # Cholesky factor in R
z1 <- t(Sigma_c) %*% rnorm(ns)
plot(z~grd, main = "Simulation of Matern Covariance: (0,10)", ylab = "simulated value", xlab = "grid values", ylim = c(-4,4))
lines(z1~grd,col='red')
lines(z~grd)
# 95 percent confidence intervals
## MLEs with standard errors
I.inv <- solve(out$hessian)
SEs <- sqrt(diag(I.inv))
# confidence distribution
CIs <- cbind(out$par - 1.96*SEs,out$par + 1.96*SEs)
rownames(CIs) <- c("range","nu")
colnames(CIs) <- c("2.5 %","97.5 %")
CIs # truth is (0.2, 1.5)
# note the approximated correlation matrix between range/smoothness
I.inv # covariance
cov2cor(I.inv) # correlation
#(b) Simulate Gaussian process interval 0,1 then find mles
#simulate Gaussian proccess with matern covariance
set.seed(3)
ns <- 50 # number of locations to simulate
grd <- seq(0,1,length.out=ns) #spatial grid
dist.mat <- rdist(grd) 
# covariance matricese
Sigma <-  Matern(dist.mat,range=0.2,smoothness=1.5)
Sigma_c <- chol(Sigma) # Cholesky factor in R
z <- t(Sigma_c) %*% rnorm(ns)
plot(z~grd, main = "Simulation of Matern Covariance: (0,1)", ylab = "simulated value", xlab = "grid values", ylim = c(-2,2))
lines(z~grd)
###
# optimize
out <- optim(par=c(0.5,1),fn=ell_2,hessian=TRUE,method="L-BFGS-B",lower=c(0.01,0.01),
             upper=c(1,2))
out$par
Sigma <-  Matern(dist.mat,range=out$par[1],smoothness=out$par[2])
Sigma_c <- chol(Sigma) # Cholesky factor in R
z1 <- t(Sigma_c) %*% rnorm(ns)
plot(z~grd, main = "Simulation of Matern Covariance: (0,1)", ylab = "simulated value", xlab = "grid values", ylim = c(-2,2))
lines(z1~grd,col='red')
lines(z~grd)
# 95 percent confidence intervals 
## MLEs with standard errors
I.inv <- solve(out$hessian)
SEs <- sqrt(diag(I.inv))
# confidence distribution
CIs <- cbind(out$par - 1.96*SEs,out$par + 1.96*SEs)
rownames(CIs) <- c("range","nu")
colnames(CIs) <- c("2.5 %","97.5 %")
CIs # truth is (0.2, 1.5)
# note the approximated correlation matrix between range/smoothness
I.inv # covariance
cov2cor(I.inv) # correlation
###================================================================================
#(3) OLS vs. ML/GLS
###================================================================================
load("SatelliteExample.RData")
#(a) using ordinary least squares 
grd <- as.matrix(cbind(lon,lat))
#Empirical variogram
v <- vgram(loc=grd,y=T, N=30, lon.lat=TRUE)
# fit linear model 
fit <- lm(T~lat+lon)
#create variogram based on residuals
v_new <- vgram(loc=grd,y=fit$residuals, N=30, lon.lat=TRUE)
#fit MLE parameters
ns <- 15 # number of locations 
grd <- seq(0,20,length.out=ns) #spatial grid
#initialize cost function variables
cost_function_variogram <- v_new
y <- v_new$stats["mean", ]
x <- v_new$centers
N <- v_new$stats["N", ]
#cost function 1
cost_function_1 <- function(coeffs){
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
coeff_guess <- nlm(cost_function_1,c(1,20,2))
coeff_guess$estimate
#(b) using generalized least squares 
x <- as.matrix(cbind(lon,lat,rep(1,length(lon)))) 
#initialize cost function variables
y <- T
#nlm to fit parameters
# cost function # 2 
dist.mat <- rdist.earth(cbind(lon,lat))
cost_function_2 <- function(coeffs){
  # covariance matricese
  Sigma <-  exponential_2(coeffs,dist.mat) 
  Sigma.c <- t(chol(Sigma))
  out <- solve(Sigma)
  #b_gls <- solve(t(x)%*%inv_sigma%*%x)%*%t(x)%*%inv_sigma%*%y
  #negative log likelyhood
  func_eval <- (sum(log(diag(Sigma.c))) + .5%*%t(y)%*%(out-out%*%x%*%solve(t(x)%*%out%*%x)%*%t(x)%*%out)%*%y)
  #func_eval <- sum(log(diag(t(Sigma.c)))) + .5*sum(forwardsolve(Sigma.c,y)^2)
  return(func_eval)
}
out <- optim(par=c(1,1,1),cost_function_2,hessian = TRUE,lower = c(.01,.01,.01),method= "L-BFGS-B")
#out <- nlm(cost_function_2,c(1,1,1),lower = c(.01,.01,.01),method= "L-BFGS-B")
out$par
#calculate beta
Sigma <-  exponential_2(out$par,dist.mat) 
beta_GLS <- solve(t(x)%*%solve(Sigma)%*%x)%*%(t(x)%*%solve(Sigma)%*%y)
#(4) Graduate Problem 
# no coding!


