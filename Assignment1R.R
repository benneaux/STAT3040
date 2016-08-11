
# c3076448; Benjamin G. Moran.
# STAT3040 - R Assignment 1

# I've used the broom package so that I can easily store the output from
# correlation and normality tests as datasets (Lines 58, 64 & 111). This then 
# makes it simple to recall things like p-values from those tests and print them
# to the console, independent of the rest of the model output. The package is 
# only 1.5Mb and I don't believe that it has many dependencies. 
# Running Lines 7 - 27 will check to see if the package is already installed and
# either load/install the package as required.

required_packages <- c("broom")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

load_packages <- function(x) {
  lapply(x,
    FUN = function(x) {
      do.call(
        "require",
        list(x)
      )
    }
  )
}

if (length(new_packages)) {
  install.packages(new_packages)
  load_packages(required_packages)
} else {
  load_packages(required_packages)
}


# 1. Define the 1000 × 2 matrix z.

  z <- matrix(
    nrow = 1000,
    ncol = 2
    )
  
  summary(z)

# 2. Generate two sets of 1000 Standard Normal random variates by using 
#  equations z1 and z2 given in item 20 and store each of them in one 
#  column of matrix z.

  set.seed(2936)
  
  u1 <- runif(1000,0,1)
  u2 <- runif(1000,0,1)
  
  z1 = sqrt(-2*log(u1))*cos(2*pi*u2)
  z2 = sqrt(-2*log(u1))*sin(2*pi*u2)

  z[,1] <- z1
  z[,2] <- z2

  summary(z)

# 3. Test, statistically, if each column of matrix z is normally distributed at the significance level of
# 5%.
  # Run the test and use broom to store output.
  normtest.z1 <- broom::tidy(
    shapiro.test(
      z[,1]
      )
    )
  
  normtest.z2 <- broom::tidy(
    shapiro.test(
      z[,2]
      )
    )

  z1.p <- normtest.z1$p.value
  if(z1.p > 0.05) {
    print(
      paste(
        "z1 p-value =",
        z1.p,
        " - therefore, z1 is normally distributed"
        )
      )
  } else{
    print(
      paste(
        "z1 p-value =",
        z1.p,
        " - therefore, z1 is not normally distributed"
        )
      )
  }
  
  z2.p <- normtest.z2$p.value
  if(normtest.z2$p.value > 0.05) {
    print(
      paste(
        "z2 p-value =",
        z2.p,
        " - therefore, z2 is normally distributed"
        )
      )
  } else{
    print(
      paste(
        "z2 p-value =",
        z2.p,
        " - therefore, z2 is not normally distributed"
        )
      )
  }
  
##  4. Test, statistically, if two columns of matrix $z$ are uncorrelated at the 
##  significance level of $5\%$.
  
  cortest.z <- broom::tidy(
    cor.test(z[,1],z[,2])
    )
  
  z.corr <- cortest.z$p.value
  if(cortest.z$p.value > 0.05) {
    print(
      paste(
        "z.corr p-value =",
        z.corr,
        " - therefore, z1 and z2 are uncorrelated"
      )
    )
  } else{
    print(
      paste(
        "z.corr p-value =",
        z.corr,
        " - therefore, z1 and z2 are correlated"
      )
    )
  }


##  5. Store both columns of matrix $z$ in a long vector, say $v$.
  
  # Create the vector v
    v <- c(z[,1], z[,2])
  
  # Print summary and lenght to show that the vector has been created correctly.
    summary(v)
    length(v)



##  6. Find the average and variance of the generated data in $v$.

  mean(v)
  var(v)
  
#  7. Define a new vector   $w = 2v + 5$. 
#  Show, numerically, that the average of the sample data in $w$ is equal to 
#  two times the average of the sample data in $v + 5$, and the variance of the 
#  sample data in $w$ is four times the variance of the sample data in $v$.

  # Create the vector and print a short summary to show it exits.
    w <-  2*v + 5 
    summary(w) 

  # Create new variables to store the mean of w and v separately
    avg.w <- mean(w)
    avg.v <- mean(v)

  # Use those variables to show that the stated values are equal.
    diff.avg <- 2*avg.v + 5 - avg.w
    diff.avg

  # Repeat the process for the variance of w and v  
    var.w <- var(w)
    var.v <- var(v)

    diff.var <- 4*var.v - var.w
    diff.var

  