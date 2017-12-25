#lab 2 
# part 3.2

mu <- c(1/6,2/6,3/6)
p1 <- c(0,2/3,1/3,1/3,1/3,1/3,1/3,2/3,0)
p  <- t(matrix(p1, nrow = 3,ncol = 3))
q1 <- c(1,1/2,0,0,1/2,1)
q  <-matrix(q1, nrow = 3,ncol = 2)

# # y given x = q
# #x1 given x0 = p
# #simulate random variable x;
# # first create a distribution for x
# n = 100
# G <- matrix(nrow = n,ncol = 3)
# q1 <- p
# for (k in 1:n)
# {
#   g <- mu%*%q
#   G[k,1] <- g[[1]]
#   G[k,2] <- g[[2]]
#   G[k,3] <- g[[3]]
#   q1 <- q1%*%p
# }
# #generate a uniform RV then assighn state for x
# u <- runif(1)
# X <- c(numeric(n))
# for (j in 1:n)
# {
#   u <- runif(1)
#   if (u[[1]]<G[[j,1]])
#   {
#     X[j] <- 1
#   }
#   else if (u[[1]]<(G[j,1]+G[j,2]))
#   {
#     X[j]<- 2 
#   }
#   else
#   {
#     X[j] <- 3
#   }
# }
# X
# second Simulation method for X
St <- mu
X1 <- c(numeric(n))
for (z in 1:n)
{
  u <- runif(1)
  if (u[[1]]<St[[1]])
  {
    X1[z] <- 1
  }
  else if (u[[1]]<(St[[1]]+St[[2]]))
  {
    X1[z]<- 2 
  }
  else
  {
    X1[z] <- 3
  }
  St[1] <- p[X1[z],1]
  St[2] <- p[X1[z],2]
  St[3] <- p[X1[z],3]
}
X1
# part 2 Simluate RV Y (2 state observable)
# use fact that you van simulate y by looking at value X
Y1 <- c(numeric(n))
for (x in 1:n)
{
  u <- runif(1)
  if ((q[X1[x],1])>u)
  {
    Y1[[x]] <- 1
  }
  else
  {
    Y1[[x]] <- 2
  }
}
Y1
