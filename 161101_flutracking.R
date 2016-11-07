library(astsa)
library(ggplot2)
library(plyr)
library(xts)
library(ggthemes)
# Read in data

file.loc <- "fludata3.csv"
fludata <- read.csv(file.loc, header=TRUE)
head(fludata)

x <- fludata$ILI_Unvaccinated
pr.ix <- which.min(x); x[pr.ix]
x[pr.ix] <- {x[pr.ix-3] + 2*x[pr.ix-2] + 3*x[pr.ix-1] + 3*x[pr.ix+1] + 2*x[pr.ix+2] + x[pr.ix+3]}/12
x[pr.ix]
x.data <- as.data.frame(cbind(x=x,y=y))
y <- seq(1:length(x))
x.data <- cbind(x,y)

plot.ts(x)
p <- qplot(x = y,y = x) + 
  geom_line() +
  scale_x_continuous() +
  labs(list(x = "Time", 
            y = "ILI Rate (%)",
            title = "Self-reported Inluenza-like Illness Rates Amongst Flutracking Survey Participants \n 2012-2016")) +
  theme_hc()
p
# Not stationary (why?), so difference by 1 lag.
Dx <- diff(x)
y <- seq(1:length(Dx))
plot.ts(Dx)
p <- qplot(x = y,y = Dx) + 
  geom_line() +
  scale_x_continuous() +
  labs(list(x = "Time", 
            y = "ILI Rate (%)",
            title = "First Difference of ILI Timeseries \n 2012-2016")) +
  theme_hc()
p

# And if we difference by 2 lags:
D2x <- diff(x, differences=2)
plot.ts(D2x)
# Plot ACF and PACF
acf2(Dx, 100)
acf2(D2x, 100)
# Let's try different ARMA(p,q) models for Dx:
sarima(x, 18,1,1) # AICc = -9.846757
sarima(x, 18,1,47)
# Looking at the ACF for Dx, we note that there does appear to be seasonality.
# Each flu season has 23 weeks, and we observe spikes in the ACF every 23 weeks.
# So let's try to account for seasonality.
D23x <- diff(Dx, 23)
plot.ts(D23x)
acf2(D23x, 90)
# Let's try to fit a SARIMA model for D23x:
# Using good/bad to say whether final coefficients are significant. i.e. if the model
# is arima(2,1,3), I only need the AR(2) and MA(3) coefficients to be significant.
# Also, for better reasoning on choosing p,d,q,P,D,Q,S, look further down the page for
# where we fit a model to the data from vaccinated respondents.
sarima(x, p=2, d=1, q=2, P=0, D=1, Q=1, S=23) # Bad
sarima(x, p=1, d=1, q=2, P=0, D=1, Q=1, S=23) # Good
sarima(x, p=0, d=1, q=2, P=0, D=1, Q=1, S=23) # Bad
sarima(x, p=2, d=1, q=1, P=0, D=1, Q=1, S=23) # Bad
sarima(x, p=1, d=1, q=1, P=0, D=1, Q=1, S=23) # Bad
sarima(x, p=0, d=1, q=1, P=0, D=1, Q=1, S=23) # Bad
sarima(x, p=2, d=1, q=0, P=0, D=1, Q=1, S=23) # Bad
sarima(x, p=1, d=1, q=0, P=0, D=1, Q=1, S=23) # Bad
sarima(x, p=0, d=1, q=0, P=0, D=1, Q=1, S=23) # Good...
# Cases where PACF cuts off at lag 17
sarima(x, 17,1,2, P=0, D=1, Q=1, S=23) # Bad
sarima(x, 17,1,6, P=0, D=1, Q=1, S=23) # Bad
sarima(x, 17,1,5, P=0, D=1, Q=1, S=23) # Very good
sarima(x, 17,1,0, P=0, D=1, Q=1, S=23) # Error
# Let's try different PDQ
sarima(x, 17,1,5, P=0, D=1, Q=1, S=23)
# No, the very good model was best.

fit <- arima(x, c(17,1,5), seasonal = list(order=c(0,1,1), period=23))

predicted <- predict(fit, 10) # Probably predict less than 10 ahead. Can't check these anyway
predicted

# Let's try to fit a model omitting the final 2 values, then predict the next
# 2 values and compare to the actual ones
fit2 <- arima(x[1:113], c(17,1,5), seasonal = list(order=c(0,1,1), period=23))
predict(fit2, 2)
x[114:115]
# Do the actual 114th and 115th values of the time series lie within the 95%
# bounds of our predicted 114th and 115th values? (Make this look nicer)
c(predict(fit2, 2)$pred[1]-1.96*predict(fit2, 2)$se[1], x[114], predict(fit2, 2)$pred[1]+1.96*predict(fit2, 2)$se[1])
c(predict(fit2, 2)$pred[2]-1.96*predict(fit2, 2)$se[2], x[115], predict(fit2, 2)$pred[2]+1.96*predict(fit2, 2)$se[2])
# Yes (above shows the lower CI, actual value, upper CI)
# Plotting actual vs predicted:
plot.ts(x, ylim=c(0.01, 0.05)) # Label title and axes
lines(c(114:115), as.numeric(predict(fit2, 2)$pred), col="red")
lines(c(114:115), as.numeric(predict(fit2, 2)$pred-1.96*predict(fit2, 2)$se), col="red", lty="dashed")
lines(c(114:115), as.numeric(predict(fit2, 2)$pred+1.96*predict(fit2, 2)$se), col="red", lty="dashed")
legend("topright", legend=c("Actual", "Predicted"), lty=1, col=1:2, bty="n")
# As you can see from this plot, the next two values the time series (red line) predicts 
# are very close to the actual values (black line). Certainly, the actual values lie well
# within the 95% confidence bounds (dotted red lines).

# Note: We can also predict value for Oct 16, 2016, which we have the data for but
# omitted so that we had 23 weeks per season.

## Pre-whitening: (NB: This did not work. Do we still include? Maybe in report, not poster.)
# Let's take a look at the Google Trends data:
y <- fludata$Vaccination_Search
plot.ts(y)
# Not stationary. Try differencing.
Dy <- diff(y)
plot.ts(Dy)
# Fit an ARMA model to the differenced data.
acf2(Dy, 80)
sarima(y, 2,1,2, no.constant = TRUE) # Yep, AICc = 2.527995
sarima(y, 2,1,0, no.constant = TRUE) # Yep, AICc = 2.599798
sarima(y, 0,1,2, no.constant = TRUE) # Nope
# Similar AICc, but we want a model with no MA terms. So use the 2nd.

fit3 <- arima(y, c(2,1,0))
fit3$coef
# Filter the time series x using the model for y
pwx <- filter(x, filter = c(1,-fit3$coef[2]), sides = 1)
pwy <- fit3$residuals
ccf(pwy, pwx, na.action=na.omit)
# Google Trends and Flutracking data do not appear to be correlated, so we shouldn't use
# the Google Trends data to predict the Flutracking data.

# We should also try fitting a model to the vaccinated survey respondents (i.e. % of 
# vaccinated survey respondents who report flu-related symptoms during a given week).
z <- fludata$ILI_Vaccinated
plot.ts(z)
# Not stationary (why?), so difference by one lag
Dz <- diff(z)
plot.ts(Dz)
# There is an outlier. Difference by 2 lags?
D2z <- diff(z, differences = 2)
plot.ts(D2z)
# Same outlier. We will use Dz for now, but this is bad.
acf2(Dz, 100)
# There is still a seasonal effect every 23 weeks. So let's account for seasonality.
D23z <- diff(Dz, 23)
acf2(D23z, 90)
# Looking at the seasons (every 23 lags), the ACF has a spike at 23 (i.e. Q=1). We can say
# either the ACF tails off, in which case we can choose a value for P (say P=1, as there
# is a spike in the PACF at 23); or we can say the ACF cuts off, in which case P=0 by
# definition. Looking within each 23 week season, we can say that: the ACF cuts off at 1
# (q=1 & p=0); we can say the ACF trails off with a spike at 1 and the PACF trails off
# with a spike at 1 (q=1 & p=1); we can say that the PACF cuts off at 1 (p=1 & q=0).
# Also consider possible spikes in the ACF and PACF at lag 9.
# Cases where P=0 and Q=1
sarima(z, p=0, d=1, q=1, P=0, D=1, Q=1, S=23) # Good, AICc = -10.63721
sarima(z, p=1, d=1, q=1, P=0, D=1, Q=1, S=23) # Bad
sarima(z, p=1, d=1, q=0, P=0, D=1, Q=1, S=23) # Good, AICc = -10.67432. Also has an AR term
                                              # instead of an MA term, which is good.
# Cases where P=1 and Q=1
sarima(z, p=0, d=1, q=1, P=1, D=1, Q=1, S=23) # Bad
sarima(z, p=1, d=1, q=1, P=1, D=1, Q=1, S=23) # Bad
sarima(z, p=1, d=1, q=0, P=1, D=1, Q=1, S=23) # Bad

fit4 <- arima(z, c(1,1,0), seasonal=list(order=c(0,1,1), period=23))

predicted2 <- predict(fit4, 10) # Probably predict less than 10 ahead. Can't check these anyway
predicted2

# Let's try to fit a model omitting the final 2 values, then predict the next
# 2 values and compare to the actual ones
fit5 <- arima(z[1:105], c(1,1,0), seasonal = list(order=c(0,1,1), period=23))
predict(fit5, 7)
z[114:115]
# Do the actual 114th and 115th values of the time series lie within the 95%
# bounds of our predicted 114th and 115th values? (Make this look nicer)
c(predict(fit5, 10)$pred[1]-1.96*predict(fit5, 5)$se[1], z[114], predict(fit5, 2)$pred[1]+1.96*predict(fit5, 2)$se[1])
c(predict(fit5, 5)$pred[2]-1.96*predict(fit5, 5)$se[2], z[115], predict(fit5, 2)$pred[2]+1.96*predict(fit5, 2)$se[2])
# Yes (above shows the lower CI, actual value, upper CI)
# Plotting actual vs predicted:
plot.ts(z, ylim=c(0, 0.05)) # Label title and axes
lines(c(106:115), as.numeric(predict(fit5, 10)$pred), col="red")
lines(c(106:115), as.numeric(predict(fit5, 10)$pred-predict(fit5, 10)$se), col="red", lty="dashed")
lines(c(106:115), as.numeric(predict(fit5, 10)$pred+predict(fit5,10)$se), col="red", lty="dashed")
legend("topright", legend=c("Actual", "Predicted"), lty=1, col=1:2, bty="n")
# As you can see from this plot, the next two values the time series (red line) predicts 
# are close to the actual values (black line). The predicted values are slighly lower than 
# expected, but the actual values do lie within the 95% confidence bounds (dotted red lines).


fit2 <- arima(x[1:105], c(6,1,5), seasonal = list(order=c(0,0,1), period=23))


plot.ts(x, 
        ylim=c(0, 0.06), 
        main="Predicting Influenza using Flutracking Survey Response Data",
        xlab = "Time",
        ylab = "ILI Rate (%)") #Label title and axes
lines(c(106:115), as.numeric(predict(fit2, 10)$pred), col="red")
lines(c(106:115), as.numeric(predict(fit2, 10)$pred-2*predict(fit2, 10)$se), col="red", lty="dashed")
lines(c(106:115), as.numeric(predict(fit2, 10)$pred+2*predict(fit2, 10)$se), col="red", lty="dashed")
legend("topright", legend=c("Actual", "Predicted (+/-2SE)"), lty=1, col=1:2, bty="n")

fit5 <- arima(z[1:110], c(1,1,0), seasonal = list(order=c(0,1,1), period=23))


plot.ts(z, ylim=c(0, 0.06)) #Label title and axes
lines(c(111:115), as.numeric(predict(fit5, 5)$pred), col="red")
lines(c(111:115), as.numeric(predict(fit5, 5)$pred-predict(fit5, 5)$se), col="red", lty="dashed")
lines(c(111:115), as.numeric(predict(fit5, 5)$pred+predict(fit5, 5)$se), col="red", lty="dashed")
legend("topright", legend=c("Actual", "Predicted"), lty=1, col=1:2, bty="n")
