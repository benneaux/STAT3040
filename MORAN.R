# Benjamin Moran
# c3076448

require(astsa)


# Submission:
# You should email your R-code to `Ali.Eshragh@newcastle.edu.au' by Sunday, 
# October 23, 11:55pm.  Write your full name and student ID No.  on the top of 
# your code and your  file name must be your complete surname.

# 1.  Find  the  theoretical  autocorrelation  function  of  the   first  AR  
# model  given  on  the  top  of  Slide 40, Lecture Slides Chapter 3, explicitly, 
# and compare its value in lag 5 with the corresponding output of ARMAacf.

# First, we find the roots of the model using polyroot.

z <- c(1,-0.7,0.12)
Roots <- polyroot(z)
Roots <- Re(Roots)
Roots

# Next, we use these results to solve for the Autocorrelation Function.

A <- matrix(nrow = 2, ncol = 2)
A[,1] <- c(1,Roots[1])
A[,2] <- c(1,Roots[2])
B <- c(1, 1/1.12)
Sol <- solve(A,B)
Sol

# Now we can compare the results.

# Explicit answer
Sol[1]*Roots[1]^(-5) + Sol[2]*Roots[2]^(-5)
# Output from ARMAacf
ACF4AR2 <- ARMAacf(ar=c(0.7,-0.12), ma=0, 100)
ACF4AR2[6]

# The answer is close, but not exactly correct.

# 2.  Plot the theoretical and sample PACF of all three 
# AR models given in Slide 40, Lecture Slides Chapter 3.

# 1st Model

PACF4AR2A <- ARMAacf(ar=c(0.7,-0.12), ma=0, 50, pacf=TRUE)
plot(PACF4AR2A, type="h")
abline(h=0)
AR2A <- arima.sim(list(order=c(2,0,0), ar=c(0.7,-0.12)), n=1000)
pacf(AR2A, 50)

# 2nd Model

PACF4AR2B <- ARMAacf(ar=c(1,-0.25), ma=0, 50, pacf=TRUE)
plot(PACF4AR2B, type="h")
abline(h=0)
AR2B <- arima.sim(list(order=c(2,0,0), ar=c(1,-0.25)), n=1000)
pacf(AR2B, 50)

# 3rd Model

PACF4AR2C <- ARMAacf(ar=c(1.5,-0.75), ma=0, 50, pacf=TRUE)
plot(PACF4AR2C, type="h")
abline(h=0)
AR2C <- arima.sim(list(order=c(2,0,0), ar=c(1.5,-0.75)), n=1000)
pacf(AR2C, 50)
