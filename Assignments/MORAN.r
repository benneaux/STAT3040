# Name: Benjamin G. Moran.
# ID No.: c3076448
# STAT3040 - R Assignment 2

# Setup =====================================================

  require(astsa)

#  Q1. ======================================================

  # How many data are stored in the variable jj?
  
    len.jj <- length(jj) #No. of datum in jj
    print(
      paste(
        "The no. of datum stored in the Johnson and Johnson Dataset:",
        len.jj
        )
      )
    

#  Q2. ======================================================

  #  What are the average and variance of the Johnson & Johnson quarterly
  #  earnings per share from 1960 to 1984?
  
    mean(jj) # The mean of JJ's quarterly earings from 1960:1984
    var(jj)  # The variance of the same earnings
    

#  Q3. ======================================================
  
  #  Plot the time series of the Johnson & Johnson quarterly earnings per share 
  #  from 1960 to 1984. The labels of the horizontal and vertical axes should be 
  #  in bold-style with the titles ‘Quarterly Earnings per Share’ and ‘Time’, 
  #  respectively.
    
    plot.ts(
      jj, # The time-series data: Johnson & Johnson (astsa)
      ylab=expression(
        bold("Quarterly Earnings per Share")),
      xlab=expression(
            bold("Time")),
      main="Johnson & Johnson Quarterly Earnings per Share:\n 1960 - 1984"
      )
    

#  Q3. Extra ================================================
  
  #Note: if you have ggplot2, ggfortify and ggthemes installed, 
  #      run the code below: It will produce a much nicer plot.
  # require(ggplot2)
  # require(ggfortify)
  # require(ggthemes)
  # 
  # ggplot(fortify(jj), aes(Index,Data, colour = cycle(jj))) +
  #   geom_line(size=1.5) +
  #   labs(
  #     list(
  #       title="Johnson & Johnson Quarterly Earnings per Share:\n 1960 - 1984",
  #       x=expression(bold("Quarterly Earnings per Share")),
  #       y=expression(bold("Time"))
  #       )) +
  #   scale_colour_gradient2_tableau() +
  #   guides(colour= guide_legend("Quarter Change")) +
  #   theme_light()
    

#  Q4. ======================================================

  #  Plot the autocovariance function of this time series within 16 lags.
  #  The main title of this plot should be ‘Johnson & Johnson Quarterly 
  #  Earnings per Share’.
  
    acf(
      jj, # The time-series data: Johnson & Johnson (astsa)
      16, # No. of Lags
      type=c("covariance"),
      main="Johnson & Johnson Quarterly Earnings per Share:\n ACF"
      )

#  References ===============================================
#  R.H. Shumway and D.S. Stoffer, Time Series Analysis and Its Applications With R Examples,
#  Springer, New York, 2010.
