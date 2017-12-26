#A1
#1. The relationship (1) holds
#2. εi are iid (independent and identically distributed) ~
# N(0, σ2 for all i = 1, . . . , n
b0 = 1
b1 = 1
sigma2 = 2
n = 1000
# first simulate all x values and find there mean
x = rnorm(n,0,1)
# find least squared error
Bx = sum(x)/n
LSE = 0
for (i in 1:n)
  LSE = (x[[i]]-Bx)^2 +LSE
end
#solve for sd of b0
sd1 = sigma2/n+ (sigma2*Bx^2)/LSE
#use sd1 to generate normal of b0
b0h = rnorm(n, mean = b0, sd = sd1)
# solve for b1 sd
sd2 = sigma2/LSE
#generate b1
b1h = rnorm(n,b1,sd= sd2)
#generate errors
erh = rnorm(n,0,sigma2)
#now generate y values 
y = numeric(n)
for (j in 1:n)
  y[j] = b0h+b1h*x[j]+erh[j]
end
plot(x,y, main = 'A1 Assumptions', sub  = 'normally distributed x')
abline(lsfit(x,y))
#clear workspace
closeAllConnections()
rm(list=ls())
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#A2 
#1. The relationship (1) holds
#2. Eεi = 0
#3. Varεi = σ
#4. εi and εj are iid
b0 = 1
b1 = 1
sigma2 = 100
n = 1000
#generate non-normal error distribution- piosson(10)
erh = rpois(n,10)-10
x = rnorm(n,0,1)
# find least squared error
Bx = sum(x)/n
LSE = 0
for (i in 1:n)
  LSE = (x[[i]]-Bx)^2 +LSE
end
#solve for sd of b0
sd1 = sigma2/n+ (sigma2*Bx^2)/LSE
#use sd1 to generate normal of b0
b0h = rnorm(n, mean = b0, sd = sd1)
# solve for b1 sd
sd2 = sigma2/LSE
#generate b1
b1h = rnorm(n,b1,sd= sd2)
#generate errors
erh = rpios(n,0)
#now generate y values 
y = numeric(n)
for (j in 1:n)
  y[j] = b0h+b1h*x[j]+erh[j]
end
plot(x,y, main = 'A2 Assumptions:', sub = 'normally distributed x, Poisson Distributed error')
abline(lsfit(x,y))
#clear workspace
closeAllConnections()
rm(list=ls())
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#A3
#1. The relationship (1) holds
#2. Eεi = 0
#3. Varεi = σ
#4. εi and εj are uncorrelated for i 6= j
#need to generate non IID variance parameter
b0 = 1
b1 = 1
n = 1000
rsigma2 = numeric(n)
for (i in 1:n)
  #indicator
{ xI = runif(1,0,10)
 rvI = xI
rsigma2[i]=rvI*rvI+i
}
#plot(rsigma2)
#now run sim lik a2
x = rnorm(n,-10,10)
# find least squared error
Bx = sum(x)/n
LSE = 0
for (i in 1:n)
  LSE = (x[i]-Bx)^2 +LSE
end
#solve for sd of b0
sd1 = numeric(n)
for (i in 1:n)
{sd1[i] = rsigma2[i]/n+(rsigma2[i]*Bx^2)/LSE}
#use sd1 to generate normal of b0
b0h = numeric(n)
for (i in 1:n)
{b0h[i] = rnorm(n, mean = b0, sd = sd1[i])}
# solve for b1 sd
sd2 = numeric(n)
for (i in 1:n)
{sd2[i] = rsigma2[i]/LSE}
#generate b1
b1h = numeric(n)
for (i in 1:n)
{b1h[i] = rnorm(1,b1,sd= sd2[i])}
#generate errors
#erh = runif(n,rsigma2)
rsigma2 = runif(n,-1,1)*rsigma2
erh = numeric(n)
for (i in 1:n)
{erh[i] = runif(1,-rsigma2[i],rsigma2[i])}
#now generate y values 
y = numeric(n)
for (j in 1:n)
  {y[j] = b0h[j]+b1h[j]*x[j]+erh[j]}
plot(x,y, main = 'A3 Assumptions:', sub = 'normally distributed x, Uncorrelated uniform error ')
abline(lsfit(x,y))
#clear workspace
closeAllConnections()
rm(list=ls())
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#A4 
#1. The relationship (1) holds
#2. Eεi = 0
#3. Varεi < ∞.
#now Im going to have an indicator that chooses a distribution for the error, 
#initialize variables 
b0 = 1
b1 = 1
n = 1000
rsigma2 = numeric(n)
#generate random variance
for (i in 1:n)
  #indicator
{ xI = runif(1,0,1)
if(xI<=.5)
{rvI = rnorm(1,0,xI)}
if(xI>.5)
{rvI = runif(1,-xI,xI)}
rsigma2[i]=rvI*100*rvI+20
}
#generate x values 
x = rnorm(n,-10,10)
# find least squared error
Bx = sum(x)/n
LSE = 0
for (i in 1:n)
 {LSE = (x[i]-Bx)^2 +LSE}
#solve for sd1 of b0h
sd1 = numeric(n)
for (i in 1:n)
{sd1[i] = (rsigma2[i])/n+(rsigma2[i]*Bx^2)/LSE}
#use sd1 to generate normal of b0
b0h = numeric(n)
for (p in 1:n)
{
  sdd1 = sd1[p]
  b0h[p] = rnorm(1, mean = b0, sd = sdd1)
}
# solve for b1 sd
sd2 = numeric(n)
for (i in 1:n)
{sd2[i] = rsigma2[i]/LSE}
#generate b1
b1h = numeric(n)
for (i in 1:n)
{b1h[i] = rnorm(1,b1,sd= sd2[i])}
# generate mixed distribution of errors
erh = numeric(n)
for (k in 1:n)
{
  Indicator = runif(1,0,1)
  if (Indicator<=.5)
  {erh[k] = runif(1,-rsigma2[k],rsigma2[k])}
  else
  {erh[k] = rnorm(1,0,rsigma2[k]) }
} 
#now generate y values 
y = numeric(n)
for (j in 1:n)
{y[j] = b0h[j]+b1h[j]*x[j]+erh[j]}
plot(x,y, main = 'A4 Assumptions:', sub = 'normally distributed x, Randomly distributed variance (< infinity)')
abline(lsfit(x,y))
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
