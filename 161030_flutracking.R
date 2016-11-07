# Read in data
library(astsa)

# Set file location

  file.loc <- "Project/Data/fludata3.csv"

  CItestfunc <- function( # Calcs if 95% of acf/pacf within blue CI bars.
    x,
    cnf.int = qnorm({1 + 0.95}/2)/sqrt(as.integer(length(x)))) { 
  ans <- mean(abs(x)<cnf.int)
  ans2 <- ifelse(ans<0.95,"No","Yes")
  print(ans)
  print(ans2)
}

# FY - I verified how they computed the blue line values for the acf (and pacf)
# plots by running 'getS3method("plot", "acf")'

  fludata <- read.csv(file.loc, header=TRUE)
  head(fludata)

# Define as a time series
  
  x <- ts(fludata$ILI_Unvaccinated, start=1, end=nrow(fludata))

### Point 5: Correct Problem Value ====
# Looking at the plot of the differenced time series, it looks better but there 
# is a very clear outlier from July 20, 2014. Most data are between 1% and 5%, 
# but this one is 0.04%. Ali suggested changing this point by using the weighted 
# average of the three values to either side 
# (i.e. (X_{t-3}+2*X_{t-2}+3*X_{t-1}+3*X{t+1}+2*X{t+2}+X_{t+3}. This makes it 
# 3.4% and the plot looks much nicer.

  pr.ix <- which.min(x); x[pr.ix]
  x[pr.ix] <- {x[pr.ix-3] + 2*x[pr.ix-2] + 3*x[pr.ix-1] + 3*x[pr.ix+1] + 2*x[pr.ix+2] + x[pr.ix+3]}/12
  x[pr.ix]
  plot(x) 

### Point 2 ====
# The function acf2() plots both the ACF and PACF on the same scale, so use that.

  n <- length(x)-1 # length of acf/pacf
  acfplot <- acf2(x, n)

# Do 95% of the acf bars lie in the CI range?

  CItestfunc(acfplot[,1]) # Nope

# Do 95% of the pacf bars lie in the CI range?

  CItestfunc(acfplot[,2]) # Yep

### Point 1 ====
# Nick and I were wondering about how to choose parameters p and q for an ARMA(p,q) 
# model. The answer is that there is no easy answer, basically you just need to try 
# a bunch of different ones and compare them (mainly through the output of the sarima() 
# function). A starting point, though, is to take p to be the largest non-zero lag 
# of the PACF, and q to be the largest non-zero lag of the ACF.

  pacfdata <- sort(abs(acfplot[,2]), index.return = TRUE, decreasing = TRUE)
  
  acfdata <- sort(abs(acfplot[,1]), index.return = TRUE, decreasing = TRUE)

# Estimate of p for the ARMA model
# Note: ix = index, x = data.

  p.lag <- pacfdata$ix[1]
  p.acf <- pacfdata$x[1]

# Estimate of q for the ARMA model
  
  q.lag <- acfdata$ix[1]
  q.acf <- acfdata$x[1]
  
  ARx <- arima(x, c(p.lag,1,q.lag))
  ARx
  ARxCoefs <- ARx$coef

# Don't know what the coefficients from here on should be.
  
  sarima(x,1,0,0)

  t.fludata <- ts.intersect(x, flu1=lag(x, -1))
  fitx <- lm(x~flu1, data=t.fludata, na.action=NULL)
  summary(fitx)
  ts.plot(cbind(x[2:n-1], fitted(fitx)), col=c(1,4), main = "Time Series x: no differencing")
  predict(ARx, 3)

### Point 4: Difference in lag 1 ====
# Plotting our time series shows it is not stationary, so we difference by 1 lag.

  z1 <- x-lag(x, 1)
  n <- length(z1)-1
  plot(z1)
  acfplot <- acf2(z1, n)

  CItestfunc(acfplot[,1]) # Nope, but almost (~92%)

  CItestfunc(acfplot[,2]) # Yep

  pacfdata <- sort(abs(acfplot[,2]), index.return = TRUE, decreasing = TRUE)

  acfdata <- sort(abs(acfplot[,1]), index.return = TRUE, decreasing = TRUE)

# Estimate of p for the ARMA model
# Note: ix = index, x = data.
  
  p.lag <- pacfdata$ix[1]
  p.acf <- pacfdata$x[1]

# Estimate of q for the ARMA model

  q.lag <- acfdata$ix[1]
  q.acf <- acfdata$x[1]

# CHECK THE COEFFICIENTS 
  
  ARz1 <- arima(z1, c(p.lag,1,q.lag))
  ARz1
  ARz1Coefs <- ARz1$coef

  sarima(z1, 1,0,0)

# Now plot the fitted values against the actual values.
  
  t.fludata <- ts.intersect(z1, flu1=lag(z1, -1))
  fitz1 <- lm(z1~flu1, data=t.fludata, na.action=NULL)
  summary(fitz1)
  ts.plot(cbind(z1[2:n-1], fitted(fitz1)), col=c(1,4), main = "Time Series z1: 1 lag")
  predict(ARz1, 3)

### Point 6: Difference in lag 2 ====
# The ACF and PACF do not look nice though, with random spikes of non-zero lags 
# appearing as far out as 50. So we try differencing the original time series by 
# 2 lags.
  
  z2 <- x-lag(x, 2)
  n <- length(z2)-1
  plot(z2)
  acfplot <- acf2(z2, n)
  
  CItestfunc(acfplot[,1]) # Nope
  
  CItestfunc(acfplot[,2]) # Nope
  
  pacfdata <- sort(abs(acfplot), index.return = TRUE)
  
  acfdata <- sort(abs(acfplot), index.return = TRUE)
  
# Estimate of p for the ARMA model
# Note: ix = index, x = data.
  
  p.lag <- pacfdata$ix[which.max(pacfdata$x)]
  p.acf <- pacfdata$x[pacfdata$ix == p.lag]
  
# Estimate of q for the ARMA model

  q.lag <- acfdata$ix[which.max(acfdata$x)]
  q.acf <- acfdata$x[acfdata$ix == q.lag]
  
# CHECK THE COEFFICIENTS
  
  ARz2 <- arima(z2, c(p.lag,1,q.lag))
  ARz2
  ARz2Coefs <- ARz2$coef
  
  sarima(z1, 1,0,0)
  
  t.fludata <- ts.intersect(z2, flu1=lag(z2, -1))
  fitz2 <- lm(z2~flu1, data=t.fludata, na.action=NULL)
  summary(fitz2)
  ts.plot(cbind(z1[2:n-2], fitted(fitz2)), col=c(1,4), main = "Time Series z2: 2 lags")
  predict(ARz2, 3)
  
# Plot them all together
  
  par(mfrow = c(3,1))
  ts.plot(cbind(x[2:length(fitted(fitx))-1], fitted(fitx)), col=c(1,4), main = "Time Series x: no differencing")
  ts.plot(cbind(z1[2:length(fitted(fitz1))-1], fitted(fitz1)), col=c(1,4), main = "Time Series z1: 1 lag")
  ts.plot(cbind(z2[2:length(fitted(fitz2))-1], fitted(fitz2)), col=c(1,4), main = "Time Series z2: 2 lags")
  
  ## PART TWO: PREDICTION ===================================================
  
# Define a time series y that is the first 95 values of x
  dev.off()
  y <- ts(fludata$ILI_Unvaccinated, start=1, end=nrow(fludata)-20)
  y
  fit2 <- arima(y, c(2,0,0))
  predicted <- predict(fit2, 20)
  plot(x, xlim=c(0, nrow(fludata)), ylim=(c(0.009,max(x))), main="Unvaccinated respondents", ylab="% reporting flu-related symptoms")
  lines(predicted$pred, col=2)
  lines(predicted$pred+1.96*predicted$se, col=2, lty="dotted")
  lines(predicted$pred-1.96*predicted$se, col=3, lty="dotted")
  
# This plot shows the actual time series in black. The red line shows what our model predicts the
# final 20 values to be, using a different model which is generated from the same time series but
# omitting the final 20 values. 95% confidence intervals for the predicted values are shown.
  
# Let's fit a MA(1) model and compare the AIC of both
 
  MA1 <- arima(x, c(0,0,1))
  MA1
  AIC(ARx)
  AIC(ARz1)
  AIC(ARz2)
  AIC(MA1)
  
# AIC is lower for the AR(1) model we came up with first, which we expect because we had no reason
# to use a MA(1) model.
  
# AIC for the ARMA(1,1) model is -1166.21, not as good as -1190.45 for the AR(1) model.
# Let's just use the AR(1) model.
  
  x2 <- ts(fludata$ILI_Vaccinated, start=1, end=nrow(fludata))
  x3 <- ts(fludata$ILI_wAbsence_Vaccinated, start=1, end=nrow(fludata))
  x4 <- ts(fludata$ILI_wAbsence_Unvaccinated, start=1, end=nrow(fludata))
  
  plot(x2, main="Influenza-like illness reported by vaccinated")
  plot(x3, main="Influenza-like illness and work absence reported by vaccinated")
  plot(x4, main="Influenza-like illness and work absence reported by unvaccinated")
  
# Convenient to say they're all stationary, so: they're all stationary. Plot ACF and PACF

  n <- nrow(fludata)-1
  
  acf2(x2, n)
  CItestfunc(acf2(x2, n)[,1]) # Nope
  CItestfunc(acf2(x2, n)[,2]) # Yep
  
  acf2(x3,n)
  CItestfunc(acf2(x3, n)[,1]) # Nope
  CItestfunc(acf2(x3, n)[,2]) # Yes
  
  acf2(x4,n)
  CItestfunc(acf2(x4, n)[,1]) # Nope
  CItestfunc(acf2(x4, n)[,2]) # Yes
  
  x2.AR1 <- arima(x2, c(1,0,0))
  x2.AR2 <- arima(x2, c(2,0,0))
  x2.AR3 <- arima(x2, c(3,0,0))
  x2.AR4 <- arima(x2, c(4,0,0))
  x2.AR5 <- arima(x2, c(5,0,0))
  x2.AR6 <- arima(x2, c(6,0,0))
  x2.AR7 <- arima(x2, c(7,0,0))
  x2.AR1$aic
  x2.AR2$aic
  x2.AR3$aic
  x2.AR4$aic
  x2.AR5$aic
  x2.AR6$aic
  x2.AR7$aic
  
  x2.ARMA500 <- arima(x2, c(5,1,0))
  x2.ARMA500$aic
  x2.ARMA512 <- arima(x2, c(5,1,20))
  x2.ARMA512$aic
  
# Look at another time series, representing Google search popularity for the term "vaccination".
  
  vac <- ts(fludata$Vaccination_Search, start=1, end=nrow(fludata))
  vac
  vac2 <- vac-lag(vac, -1)
  plot(vac2)
  acf2(vac2, 50)
  
