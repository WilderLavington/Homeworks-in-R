# run simulation 10,000 times
infomat1 = numeric(10000)
infomat2 = numeric(10000)
infomat3 = numeric(10000)
infomat4 = numeric(10000)
infomat5 = numeric(10000)
for ( j in 1:10000)
{
#simulate a HPP
lambda = 2
t = 3
T <- numeric(100)
i = 1
bool = 2
while (bool==2)
{ 
  i <- (i+1)
  T[i] <- T[i-1] - log(runif(1,0,1))/lambda
  if (T[i] >t)
  {
    N <- i
    bool = 1;
  }
}
infomat1[j] <- N-1
infomat2[j] <-T[i]
infomat3[j] <- -log(runif(1,0,1))/lambda +3
infomat4[j] <-T[i]-t
infomat5[j] <-T[i]-log(runif(1,0,1))/lambda
}
#generate histograms for all three values in matrix
hist(infomat1, 
     main="Histogram of the Random Quantity N", 
     ylab="Frequency", xlab="N")
hist(infomat3,main="T(N+1)|t vs exponential",density=6,ylab="Frequency",
     xlab="T(N+1)",prob = TRUE)
lines(density(infomat3), col = "red", lwd = 2)
#sim2<-rexp(10000, 2)*10000
lines(density(infomat3), col = "red", lwd = 2)
hist(infomat5,main="T(N+1)-T(N) vs gamma",ylab="Frequency",
     xlab="T(N+1)-T(N)",prob = TRUE)
lines(density(infomat5), col = "red", lwd = 2)
hist(infomat4,main="Histogram of the Random Quantity T(N+1)-t",ylab="Distribution",
     xlab="Distribution",prob = TRUE)
lines(density(infomat4), col = "red", lwd = 2)
#superimpose histograms with predicted distributions 
#find optimized parameters for speculated distribution
#plot ovelay 
#simulate piosson with paremter t*lambda
sim1 <- rpois(10000,6)
hist(infomat1, density=10, prob=TRUE, ylab="frequency", xlab="Distribution", 
     main="Poisson curve vs N")
lines(density(sim1, bw=1), col="red",lwd=2) 
#simulate infomat3
hist(infomat3, prob=TRUE, xlab="frequency", ylim=c(0,1),
     main="Gamma curve vs T(N+1)")
#lines(density(infomat3,bw = 1/8), col = "blue", lwd = 2)
sim2<-rgamma(10000, rate = 6)
lines(dist(sim2), col = "red", lwd = 2)
#simulate and fit infomat4
hist(infomat4, density=10, prob=TRUE, xlab="frequency", ylim=c(0,2),
     main="Exponential curve vs T(N+1)-T(N)")
x=infomat4
lines(density(x), col = "blue", lwd = 2)
# sim2<-rexp(10000, 2)
# lines(density(sim2), col = "red", lwd = 2)
#simulate and fit infomat4
hist(infomat4, density=10, prob=TRUE, xlab="frequency",ylim=c(0,2), 
     main="Exponential curve vs T(N+1)-t")
 sim2<-rexp(10000, 2)
 lines(density(sim2), col = "red", lwd = 2)
#-----------------------------------------------
#-----------------------------------------------
#-----------------------------------------------
#simulate NHPP
#first simulate HPP 
# run simulation 10,000 times
c = 10;
infomat6 = numeric(10000)
infomat1 = numeric(10000)
infomat2 = numeric(10000)
infomat3 = numeric(10000)
infomat4 = numeric(10000)
infomat5 = numeric(10000)
for ( j in 1:10000)
{
  #simulate a HPP
  lambda = 2
  t = 3
  T <- numeric(100)
  i = 1
  bool = 2
   
  while (bool==2)
  { 
    j <- (j+1)
    T[j] <- T[j-1] - log(runif(1,0,1))/lambda
    if (T[j] >t)
    {
      N <- j;
      bool = 1;
    }
  }
  infomat1[j] <- N 
  infomat2[j] <-T[j]
  infomat3[j] <-T[j]-log(runif(1,0,1))/lambda
  infomat4[j] <-T[j]-t
  infomat5[j] <- -log(runif(1,0,1))/lambda
  #thin HPP
  ThinNcount = 0
  for (z in 1:N)
  {
  U = runif(1)2
  SHPP = (T[z]^2-6*T[z]+10)/c
  if (U < SHPP)
  { 
    ThinNcount = ThinNcount+1
  }
  #store data from NHPP
  }
  infomat6[q] <- ThinNcount
}
hist(infomat6)

#-----------------------------------------------
#-----------------------------------------------
#-----------------------------------------------
# run simulation 10,000 times
infomat6 = numeric(10000)
for (n in 1:10000)
{
ThinNcount = 0
# run HPP simulation times
infomat1 = numeric(10000)
infomat2 = numeric(10000)
infomat3 = numeric(10000)
infomat4 = numeric(10000)
infomat5 = numeric(10000)
ThinNcount =0
#simulate a HPP
  lambda = 10
  t = 6
  T <- numeric(100)
  i = 1
  bool = 2
  while (bool==2)
  { 
    i <- (i+1)
    T[i] <- T[i-1] - log(runif(1,0,1))/lambda
    if (T[i] >t)
    {
      N <- i
      bool = 1;
    }
  }
  infomat1[n] <- N-1 #arrivals by time t
  infomat2[n] <-T[n] #ith arrival time
  infomat3[n] <-T[n]-log(runif(1,0,1))/lambda #(i+1)th arrival time
  infomat4[n] <-T[n]-t
  infomat5[n] <- -log(runif(1,0,1))/lambda
#simulate piosson thinning proccess
for (z in 1:N)
{
  U = runif(1)
  p = T[z]
  SHPP = ((p)^2-6*p+10)/10
  if (U < SHPP)
  { 
    ThinNcount = ThinNcount+1
  }
}
infomat6[n] <- ThinNcount-1
}
hist(infomat6, xlab="frequency", 
     main="Piosson(24) vs. W(t)",density = 10,prob = TRUE)
#fitdistr(infomat6,"poisson",density = 10)
sim1 <- rpois(10000,24)
lines(density(sim1),lwd=2) 
hist(infomat3, xlab="distributions", 
     main="T(N+1) Given t time has passed vs shifted exp.",density = 10,prob = TRUE)
#fitdistr(infomat6,"poisson",density = 10)
sim1 <- (rexp(10000,3)+3)
lines(density(sim1),lwd=2) 
