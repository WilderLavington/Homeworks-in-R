#part 2 LAb 1
#method A 
#simulate random permutation
values<-numeric(10000)
count<-(0)
#find the expcted number of direct search operations to find index
for(i in 1:10000)
{
count <- (count+1)
u <- runif(128)
v <- order(u)
d<-which.min(v)[[1]]
values[count]<-(d)
}
#part 2 simluate binary search 10000 times
# Performance test
