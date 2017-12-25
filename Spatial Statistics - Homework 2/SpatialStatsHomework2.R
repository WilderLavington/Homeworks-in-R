#===================================
#problem 2
#===================================
#simulate 3 standard normals 
#x = runif(4,0,1)
x = rnorm(4,0,1)
#create grid
grid = seq(0,1,length=1000)
#create stochatstic proccess
Z = x[1] + x[2]*grid+x[3]*grid*grid+x[4]*grid*grid*grid
#plot
xmin = 0
xmax = 1000
ymin = -3
ymax = 3
#plot(Z, main = "Z(s) = W_0 + W_1*s + W_2*s^2 + W_3*s^3", xlab = "x", ylab = "Z",xlim=c(xmin, xmax), ylim=c(ymin, ymax))
#points(Z,col = "yellow")
#points(Z,col = "green")
#points(Z,col = "blue")
points(Z,col = "purple")
#points(Z,col = "red")
#===================================
#problem 3
#===================================
#load data set/library
library(fields)
load('ThreeSurfaces2.RData')
#plot data set
image.plot(dat1,main = "Data Set 1") 
image.plot(dat2,main = "Data Set 2") 
image.plot(dat3,main = "Data Set 3") 
#create matrix of all pairwise distances
pairwise_dist = rdist(expand.grid(1:80,1:80))
#function to calculate empirical covariances
empirical_covariance <- function(pairwise_dist, dat){
  #initialize C
  C = numeric(length = 40)
  N = numeric(length = 40)
  #iterate over values of C
  for (h in 1:40){
    #sum over all possible combinations of (Z(s_i)-Z_bar)(Z(s_j)-Z_bar)
    #such that |s_i-s_j| = h
    #initialize sum
    for (x in 1:6399){
      for (y in 1:6399){
        distance = pairwise_dist[x,y] 
        if (distance == h){
          current_position1 = c(x%/%80+1,x%%80+1)
          current_position2 = c(y%/%80+1,y%%80+1)
          N[h] <- N[h] + 1
          C[h] <- C[h] + (dat[current_position1[1],current_position1[2]]-mean(dat))*(dat[current_position2[1],current_position2[2]]-mean(dat))
          }
      }
    }
    print("Percent Complete:")
    print(floor(100*(h*x*y/1638400000)))
    #divide by normalization term (number of such pairs)
    C[h] <- C[h]/N[h]
    C[h] <- C[h]
  }
  return(C)
  }
empirical_covariance_data1 = empirical_covariance(pairwise_dist,dat1)
empirical_covariance_data2 = empirical_covariance(pairwise_dist,dat2)
empirical_covariance_data3 = empirical_covariance(pairwise_dist,dat3)
#plot 
plot(empirical_covariance_data1,main= "Empirical Covariance vs Distance (data set 1)", xlab = "Distance",ylab = "empirical covariance")
plot(empirical_covariance_data2,main= "Empirical Covariance vs Distance (data set 2)", xlab = "Distance",ylab = "empirical covariance")
plot(empirical_covariance_data3,main= "Empirical Covariance vs Distance (data set 3)", xlab = "Distance",ylab = "empirical covariance")








