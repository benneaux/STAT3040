# Name: Benjamin G. Moran.
# ID No.: c3076448
# STAT3040 - R Assignment 2

require(astsa)

#1. How many data are stored in the variable jj?

  length(jj)

#2. What are the average and variance of the Johnson & Johnson quarterly 
#   earnings per share from 1960 to 1984?

  mean(jj)
  var(jj)

  # OR ...
  var(jj)*(length(jj)/(length(jj)-1))

#3. Plot the time series of the Johnson & Johnson quarterly earnings per share 
#   from 1960 to 1984. The labels of the horizontal and vertical axes should be 
#   in bold-style with the titles ‘Quarterly Earnings per Share’ and ‘Time’, 
#   respectively.

  plot.ts(
    jj,
    xlab=expression(
      bold("Quarterly Earnings per Share")
    ),
    ylab=expression(
      bold("Time")
    ),
    main="Johnson & Johnson Quarterly Earnings per Share: \n 1960 - 1984"
  )

#4. Plot the autocovariance function of this time series within 16 lags. The 
#   main title of this plot should be ‘Johnson & Johnson Quarterly Earnings per 
#   Share’.
  
  # Type = Covariance
  acf(jj, 16, type=c("covariance"), main="Johnson & Johnson Quarterly Earnings per Share")
  
  # Type = Correlation
  acf(jj, 16, type=c("correlation"), main="Johnson & Johnson Quarterly Earnings per Share")
  
  # Type = Partial
  acf(jj, 16, type=c("partial"), main="Johnson & Johnson Quarterly Earnings per Share")
