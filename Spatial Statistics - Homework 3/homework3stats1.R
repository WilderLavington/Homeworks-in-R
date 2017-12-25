##
## Problem 2: homework 3, Spatial stats
##  (Building intuition)
#==========================================================================
# (a) Equally spaced observation locations
#==========================================================================
library(fields)
set.seed(10)
# (1) exponential covariance
ns <- 15 # number of locations to simulate
grd <- seq(0,20,length.out=ns) #spatial grid
x <- seq(0,20,length.out = 5000) #sample vector for kriging
a <- 20 # parameter for exponential covariance
tau <- 10 # nugget 
sigma <- sd(rdist(grd,x)) #standard dev
Sigma0 <- sigma^2 * exp(-rdist(grd,x)/a) + tau^2
Sigma <- sigma^2 * exp(-rdist(grd)/a) + tau^2
Sigma_c <- chol(Sigma) # Cholesky factor in R
sim <- t(Sigma_c) %*% rnorm(ns)
plot(sim~grd, main = "Simulation of Exponential Covariance", ylab = "simulated value", xlab = "grid values", ylim = c(-10,2))
rug(grd)
# weights for kriging 
w <- t(t(Sigma0) %*% solve(Sigma))
#kriging values 
sk <- t(w) %*% sim
lines(sk~x)
#prediction intervals 
sk.var <- sigma^2 + tau^2 - diag(t(Sigma0) %*% solve(Sigma) %*% Sigma0)
sk.var[sk.var < 0] <- 0
sk.se <- sqrt(sk.var)
sk.up <- sk + 1.96 * sk.se
sk.dn <- sk - 1.96 * sk.se
lines(sk~x,col="black")
lines(sk.up~x,col="blue")
lines(sk.dn~x,col="blue")

# (2) generalied cauchy
ns <- 15 # number of locations to simulate
grd <- seq(0,20,length.out=ns) #spatial grid
x <- seq(0,20,length.out = 5000) #sample vector for kriging
# parameters
alpha <- 1.8
tau <- .6
eta <- .9
sigma <- sd(rdist(grd,x)) #standard dev
Sigma0 <- sigma^2 * (1+(alpha*rdist(grd,x))^eta)^(-tau/eta)
Sigma <-  sigma^2 * (1+(alpha*rdist(grd))^eta)^(-tau/eta)
Sigma_c <- chol(Sigma) # Cholesky factor in R
sim <- t(Sigma_c) %*% rnorm(ns)
plot(sim~grd, main = "Simulation of Generalized Cauchy Covariance", ylab = "simulated value", xlab = "grid values", ylim = c(-17,8))

rug(grd)
# weights for kriging 
w <- t(t(Sigma0) %*% solve(Sigma))
#kriging values 
sk <- t(w) %*% sim
lines(sk~x)
#prediction intervals 
sk.var <- sigma^2 - diag(t(Sigma0) %*% solve(Sigma) %*% Sigma0)
sk.var[sk.var < 0] <- 0
sk.se <- sqrt(sk.var)
sk.up <- sk + 1.96 * sk.se
sk.dn <- sk - 1.96 * sk.se
lines(sk~x,col="black")
lines(sk.up~x,col="blue")
lines(sk.dn~x,col="blue")

# (3) sine-power
ns <- 15 # number of locations to simulate
grd <- seq(0,20,length.out=ns) #spatial grid
x <- seq(0,20,length.out = 5000) #sample vector for kriging
# parameters
eta <- 1
sigma <- sd(rdist(grd,x)) #standard dev
Sigma0 <- sigma^2 * (1 - (sin(rdist(grd,x)/2))^eta)
Sigma <-  sigma^2 * (1 - (sin(rdist(grd)/2))^eta)
Sigma_c <- chol(Sigma) # Cholesky factor in R
sim <- t(Sigma_c) %*% rnorm(ns)
plot(sim~grd, main = "Simulation of Exponential Covariance", ylab = "simulated value", xlab = "grid values", ylim = c(0,10),xlim = c(0,20))
rug(grd)
# weights for kriging 
w <- t(t(Sigma0) %*% solve(Sigma))
#kriging values 
sk <- t(w) %*% sim
lines(sk~x)
#prediction intervals 
sk.var <- sigma^2 - diag(t(Sigma0) %*% solve(Sigma) %*% Sigma0)
sk.var[sk.var < 0] <- 0
sk.se <- sqrt(sk.var)
sk.up <- sk + 1.96 * sk.se
sk.dn <- sk - 1.96 * sk.se
lines(sk~x,col="black")
lines(sk.up~x,col="blue")
lines(sk.dn~x,col="blue")
#==========================================================================
# (b) Unequally spaced observation locations.
#==========================================================================
set.seed(14)
# (1) exponential covariance
ns <- 15 # number of locations to simulate
grd <- runif(ns,0,20) #spatial grid
x <- seq(0,20,length.out = 5000) #sample vector for kriging
a <- 20 # parameter for exponential covariance
sigma <- sd(rdist(grd,x)) #standard dev
Sigma0 <- sigma^2 * exp(-rdist(grd,x)/a)
Sigma <- sigma^2 * exp(-rdist(grd)/a)
Sigma_c <- chol(Sigma) # Cholesky factor in R
sim <- t(Sigma_c) %*% rnorm(ns)
plot(sim~grd, main = "Simulation of Exponential Covariance", ylab = "simulated value", xlab = "grid values", ylim = c(-17,8))
rug(grd)
# weights for kriging 
w <- t(t(Sigma0) %*% solve(Sigma))
#kriging values 
sk <- t(w) %*% sim 
lines(sk~x)
#prediction intervals 
sk.var <- sigma^2 - diag(t(Sigma0) %*% solve(Sigma) %*% Sigma0)
sk.var[sk.var < 0] <- 0
sk.se <- sqrt(sk.var)
sk.up <- sk + 1.96 * sk.se
sk.dn <- sk - 1.96 * sk.se
lines(sk~x,col="black")
lines(sk.up~x,col="blue")
lines(sk.dn~x,col="blue")

# (2) generalied cauchy
ns <- 15 # number of locations to simulate
grd <- runif(ns,0,20) #spatial grid
x <- seq(0,20,length.out = 5000) #sample vector for kriging
# parameters
alpha <- 1.8
tau <- .6
eta <- .9
sigma <- sd(rdist(grd,x)) #standard dev
Sigma0 <- sigma^2 * (1+(alpha*rdist(grd,x))^eta)^(-tau/eta)
Sigma <-  sigma^2 * (1+(alpha*rdist(grd))^eta)^(-tau/eta)
Sigma_c <- chol(Sigma) # Cholesky factor in R
sim <- t(Sigma_c) %*% rnorm(ns)
plot(sim~grd, main = "Simulation of Generalized Cauchy Covariance", ylab = "simulated value", xlab = "grid values", ylim = c(-17,8))
rug(grd)
# weights for kriging 
w <- t(t(Sigma0) %*% solve(Sigma))
#kriging values 
sk <- t(w) %*% sim
lines(sk~x)
#prediction intervals 
sk.var <- sigma^2 - diag(t(Sigma0) %*% solve(Sigma) %*% Sigma0)
sk.var[sk.var < 0] <- 0
sk.se <- sqrt(sk.var)
sk.up <- sk + 1.96 * sk.se
sk.dn <- sk - 1.96 * sk.se
lines(sk~x,col="black")
lines(sk.up~x,col="blue")
lines(sk.dn~x,col="blue")

# (3) sine-power
ns <- 15 # number of locations to simulate
grd <- runif(ns,0,20) #spatial grid
x <- seq(0,20,length.out = 5000) #sample vector for kriging
# parameters
eta <- 1
sigma <- sd(rdist(grd,x)) #standard dev
Sigma0 <- sigma^2 * (1 - (sin(rdist(grd,x)/2))^eta)
Sigma <-  sigma^2 * (1 - (sin(rdist(grd)/2))^eta)
Sigma_c <- chol(Sigma) # Cholesky factor in R
sim <- t(Sigma_c) %*% rnorm(ns)
plot(sim~grd, main = "Simulation of Power Sine Covariance", ylab = "simulated value", xlab = "grid values", ylim = c(-17,13))

rug(grd)
# weights for kriging 
w <- t(t(Sigma0) %*% solve(Sigma))
#kriging values 
sk <- t(w) %*% sim
lines(sk~x)
#prediction intervals 
sk.var <- sigma^2 - diag(t(Sigma0) %*% solve(Sigma) %*% Sigma0)
sk.var[sk.var < 0] <- 0
sk.se <- sqrt(sk.var)
sk.up <- sk + 1.96 * sk.se
sk.dn <- sk - 1.96 * sk.se
lines(sk~x,col="black")
lines(sk.up~x,col="blue")
lines(sk.dn~x,col="blue")

