##===================================================================
#exam 1
##===================================================================
#problem 1
# (a) Provide a plot with all sightings with the US map overlaid – do you notice anything
# about locations of sightings?
load("UFO_scrubbed.RData")
ufo$duration.seconds = log(ufo$duration.seconds)
library(maps)
title_ = "Illinois"
st = "il"
map("state", main = "UFO Sitings across the US")
points(ufo$longitude,ufo$latitude,pch = 21,col = "red", cex = .25)
#(b) Make heat plots of log-duration of sightings
library(fields)
state_ = subset(ufo,ufo$state == st)
quilt.plot(state_$longitude, state_$latitude, state_$duration.seconds,main= title_,xlab="Longitude", ylab="Latitude")
US(add=TRUE)
#(c) create binned semi variogramms
#look at duration
state_duration <- state_$duration.seconds
grd <- as.matrix(cbind(state_$latitude,state_$longitude))
#Empirical variogram
v <- vgram(loc=grd,y=state_duration, N=60, lon.lat=TRUE)
#plot emperical variogram 
plot(v$stats["mean",]~v$centers,main= title_,xlab="Bin centers", ylab=" Mean values")

#(d) Using OLS or WLS, estimate a relevant spatial model, Calculate the nugget-to-sill ratio
#exponential function
exponential_ <- function(coeffs,r){
  val = coeffs[1]^2*(1 - exp(-r/coeffs[2])) + coeffs[3]^2
  return(val)
}

#initialize cost function variables
cost_function_variogram <- v
y <- v$stats["mean", ]
x <- v$centers
N <- v$stats["N", ]
#cost functions
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
lines(vals)
#print values
print(title_)
print("nugget-to-sill ratio")
print(vals[1]/vals[500])
print("Model Parameters")
print(coeff_guess$estimate)







