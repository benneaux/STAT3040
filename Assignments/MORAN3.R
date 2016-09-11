# Name: Benjamin G. Moran.
# ID No.: c3076448
# STAT3040 - R Assignment 3

### Intro ======================================================================

# I chose to examine a timeseries consisting of search rankings for the words
# 'Sugar' and 'Fat' in the United States of America between 2004 and 2016.

### Setup ======================================================================
  
  # 1. Set Working Directory
  
      setwd("Enter your wd here")
  
  # 2.	Store the data from the file to a variable in R:
    
      SF.US <- readRDS("GTSugarFatTS.rds")
    
  # Note: I downloaded the .csv file, cleaned it and then saved it as an .rds,
  # which is an R data object. You don't notice the benefits as much here - the 
  # .csv was only 2.2KB vs the .rds at 1.2KB - but it can help with bigger 
  # datasets
  
  # 3. Look at the structure of the variable: 
  
      head(SF.US)

### Linear Regression in R =====================================================

  # 1.	Find the time range of dataset:
  
      SF.US[1,1]
    
      dim(SF.US)
    
      SF.US[152,1]
  
  # 2.	Construct two time series for Sugar and Fat searches in the United States 
  #     within the range of observation times:
  
      Sugar.TS <- ts(
        SF.US[,2],
        start = c(2004,1),
        end = c(2016,8),
        frequency = 12
        ) 
      
      Sugar.TS
      
      Fat.TS <- ts(
        SF.US[,3],
        start = c(2004,1),
        end = c(2016,8),
        frequency = 12
        ) 
      
      Fat.TS
  
  #3.	Plot the time series of Sugar.TS and Fat.TS:
  
    # TS plot for the sugar data.
      plot.ts(
        Sugar.TS,
        type="l"
        ) 
    
    # TS plot for the fat data.  
      plot.ts(
        Fat.TS,
        type="l"
        ) 
    
    # One on top of the other.
      plot.ts(
        cbind(
          Sugar.TS,
          Fat.TS
          ),
        type="l"
        ) 
    
    # Plotted on the same axes.
      ts.plot(
        cbind(
          Sugar.TS,
          Fat.TS
          ),
        type="l"
        )

# These ts datasets do seem to have some kind of relationship - whether or not
# this is specious reasoning, I can't tell without something more than the 
# cusrsory analysis. Unlike the Wine and Beer from the Lab examples, this data
# can't as easily be extrapolated to any inference about their respective 
# consumption rates, but perhaps it points to the different ways that these 
# terms are contextualised: Sugar for the holiday season (cooking and enjoying, 
# etc.); Fat immediately afterwards and again in the (Northern) summer (guilt?).
# Obviously I have no evidence, but the cyclical nature of both ts and the 
# way that they seem to lead/lag each other is intriguing.

  #4.	Construct a time variable for observation times:
  
      TimeTS <- time(Sugar.TS) 
      TimeTS
  
  
  #5.	Plot the scatterplot matrix:
  
      data <- data.frame(
        Sugar = Sugar.TS,
        Time = TimeTS,
        Fat = Fat.TS
        )
      
      pairs(
        cbind(
          Sugar = Sugar.TS,
          Time = TimeTS,
          Fat = Fat.TS
          )
        )
    
# The overall increase in searches over time seems to dominate; all three
# interactios seem fairly linearly correlated (positively).

  #6. Fit two regression models
  
      fit1 <- lm(Sugar.TS ~ TimeTS + Fat.TS) 
      
      TimeTS2 <- TimeTS^2
      
      fit2 <- lm(Sugar.TS ~ TimeTS + TimeTS2 + Fat.TS)
  
# I just used the two models presented in class. I believe this data displays
# an underlying seasonal trend, but I haven't learnt how to incorporate sine and
# cosine terms yet, so I didn't try. Regardless, you'll see that both models 
# do a pretty good job of fitting the data.

  #7.	Summary of the regression parameters estimations: 
  
    # Fit 1: lm(Sugar.TS ~ TimeTS + Fat.TS)  
    
      summary(fit1) # R^2 = 0.7474...
    
    # Fit 2: lm(Sugar.TS ~ TimeTS + TimeTS2 + Fat.TS)  
    
      summary(fit2) # R^2 = 0.7674...
  
# A small improvement from fit 1 to fit 2.
  
  #8.	Compare the values of AIC for two models:
  
      AIC(fit1)/152 - log(2*pi) # = 4.848111
    
      AIC(fit2)/152 - log(2*pi) # = 4.778691
    
# Again, a small improvement from fit 1 to fit 2.

### Extra ======================================================================
# 
# # If you have the following packages installed.
# 
#   req.packs <- list(
#     "tidyr", # separate function: turns a single char col into multiple cols.
#     "ggfortify", # fortify function: converts ts data to regular data.
#     "ggplot2", # for the fancy plots
#     "magrittr", # adds the %>% pipe.
#     "broom" # Converts Statistical Analysis Objects into Tidy Data Frames
#     )
#   
#   lapply(req.packs, require, character.only = TRUE)
#   
# # Note: the tidy and glance functions from the 'broom' package produce more
# #       concise output:
#   
#   # Fit 1: lm(Sugar.TS ~ TimeTS + Fat.TS)
#   
#   cbind(
#     tidy(fit1),
#     glance(fit1)
#   )
#   
#   # Fit 2: lm(Sugar.TS ~ TimeTS + TimeTS2 + Fat.TS)
#   
#   cbind(
#     tidy(fit2),
#     glance(fit2)
#   )
# 
# # Plotting the searches against each month - rather than as a continuous time 
# # series - really shows up when Americans search for fat and sugar: for fat, 
# # they seem to care more in February and June/July; for sugar, the end of year
# # dominates.
# 
# Fat.TS2 <- fortify(Fat.TS) %>% 
#   separate(Index, c("Year", "Month", "Day")) %>%
#   subset(select = -Day)
# 
# Sugar.TS2 <- fortify(Sugar.TS) %>% 
#   separate(Index, c("Year", "Month", "Day")) %>%
#   subset(select = -Day)
# 
# ggplot(Fat.TS2, aes(Month, Data, group = Year, colour = Year)) + 
#   geom_line() + 
#   labs(
#     list(
#       title = "Google Trends \n 'Fat' Search Term Trend Rating",
#       x = "Month (Calendar Year)",
#       y = "Search Trend Rating: (0:100)"
#       )
#     )
# 
# ggplot(Sugar.TS2, aes(Month, Data, group = Year, colour = Year)) + 
#   geom_line() + 
#   labs(
#     list(
#       title = "Google Trends \n 'Sugar' Search Term Trend Rating",
#       x = "Month (Calendar Year)",
#       y = "Search Trend Rating: (0:100)"
#     )
#   )