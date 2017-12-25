#part 1
#question 1.1
#question 1.2
#a)create distribution
#initialize sigma
sigma <- c(2, 5, 1, 4, 3)
#initialize vector to store data
data <- vector(integer,100)
i <- (1)
count <- (0)
k1 <- (0)
for (k in 1:100)
{
count <- (0)
i <- (1)
while(i==1)
{
  u <- runif(5);
  v <- order(u);
  if(all(v==sigma))
  {
    i<-(0);
  }
  else
  {
    i<-(1);
  }
  count <- count+1;
}

k1 <- (k1+1)
vect[k1]<-(count)
}
Permutations<-vect
hist(Permutations, main = "Experimental Permutations")
#question 1.3
#number of times sigma is observed in n runs of the algorithm
sigma <- c(2, 5, 1, 4, 3)
n <- (5)
Y <-  numeric(100)
counter <- (0)
for (p in 1:1000)
{
success <- (0)
for (i in 1:1000)
{
  u <- runif(5);
  v <- order(u);
  if(all(v==sigma))
  {
    success <- (success+1)
  }
}
counter <- (counter+1)
Y[counter]<-(success)
}
hist(Y, main = "Experimental Permutations", xlab="Y = i")
#question 4
#experimental data
sigma <- c(2, 5, 1, 4, 3)
i <- (1)
data <- numeric(100)
count <- (0)
k1 <- (0)
for (k in 1:100)
{
  count <- (0)
  i <- (1)
  while(i==1)
  {
    u <- runif(5);
    v <- order(u);
    if(all(v==sigma))
    {
      i<-(0);
    }
    else
    {
      i<-(1);
    }
    count <- count+1;
  }
  k1 <- (k1+1)
  data[k1]<-(count)
  
}
#hist(data, main = "Experimental Distribution",xlab="X = i")
#theoretical distribution
#bernouli with p=1/n!
m <- 100
p <- 1/factorial(5)
data1 <- numeric(100)
for (z in 1:1000)
{
x[z] <- z
}
#hist(data1, main = "Theoretical Distribution", xlab = "X = i")
#make graph of both overlayed
#combined data
hist(data, prob=TRUE, col="grey", main = "Experimental Histigram vs Theoretical Distribution", xlab = "X=i")# prob=TRUE for probabilities not counts
lines(dgeom(x,p), col="blue", lwd=2) #
#question 5
#number of times sigma is observed in n runs of the algorithm
#experimental 
sigma <- c(2, 5, 1, 4, 3)
n <- (5)
counter <- (0)
data2 <- numeric(100)
for (p in 1:100)
{
  success <- (0)
  for (i in 1:1000)
  {
    u <- runif(5);
    v <- order(u);
    if(all(v==sigma))
    {
      success <- (success+1)
    }
  }
  counter <- (counter+1)
  data2[counter]<-(success)
}
#theoretical 
m <- 100
p <- 1/factorial(5)
data3 <- numeric(100)
x <- numeric(100)
for (z in 1:100)
{
  data3[z]<-rbinom(100,1000,p)
}
hist(data3, main = "Theoretical histogram", xlab="Y=i")
#combined data
hist(data2, prob=TRUE, col="grey", main = "Experimental Histigram vs Theoretical Distribution", xlab = "Y=i")# prob=TRUE for probabilities not counts
lines(dbinom(x, 1000, p, log = FALSE), col="blue", lwd=2) #
set.seed(50)
p1 <- hist(data2)                     # centered at 4
p2 <- hist(data3)                     # centered at 6
plot( p1, main= "Histogram of Y",col=rgb(0,0,1,1/4), xlim=c(0,20))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,20), add=T)  # second
