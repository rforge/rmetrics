#
# Examples from the forthcoming Monograph:
#   Rmetrics - Financial Engineering and Computational Finance
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 4.1
#   Trading and Forecasting with Regression Models:
#
# List of Examples, Exercises and Code Snippets:
#    
#   Example:
#
#   *** This list is not yet complete ***
#
# Author:
#   (C) 2002-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################

# xmpTradingIndicators        Implements More Trading Indicator Functions
# xmpTradingPatternCreation   Creates Trading Pattern
# xmpTradingPatternTests      Performs Windowed Correlation Tests
# xmpTradingModels            Trades with MACD and Trend Indicators

# Rolling Analysis

################################################################################


### Example: More Trading Indicator Functions

    # Description:
    #   Thanks to my students, in an assignment from SS 2001
    #   we have implemented further trading indicators. I (DW) 
    #   have changed notation, so that the naming convention 
    #   and the names of the arguments follow more closely
    #   to the indicators availalbe in SPlus FinMetrics.
    #
    # Details:
    #   The added trading indicators in alphabetical order are:
    #       accelTA
    #       adiTA
    #       adoscillatorTA
    #       bollingerTA
    #       chaikinoTA
    #       chaikinvTA
    #       garmanKlassTA
    #       macdTA
    #       medpriceTA
    #       momentumTA
    #       nviTA
    #       obvTA
    #       pviTA
    #       pvtrendTA
    #       rocTA
    #       rsiTA
    #       stochasticTA
    #       typicalPriceTA
    #       wcloseTA
    #       williamsadTA
    #       williamsrTA
    #
    # Notes:
    #   The R functions can be found in "funMultivar.R"
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Create Trading Patterns

    # This example shows how to calculate, to diplay and to write data 
    # to a file with selected indicators including responses and predictors
    ###

    # Create Pattern:
    # Settings:
    data(spc1970)
    dosave = FALSE
    file = "spc1970-pattern.csv"
    ###
    
    # Work with Logarithmic Data:
    H = log(spc1970[, 3])   # log High
    L = log(spc1970[, 4])   # log Low
    C = log(spc1970[, 5])   # log Close
    R = c(0, diff(C, 1))    # log Return
    ###
    
    # Plot Closing Prices and Returns:
    par(mfcol = c(2, 2))
    plot(C, main = "Log(Close)", type = "l")
    plot(R, main = "Returns", type = "l")
    ###
    
    # Select a Piece of the Time Series to Display:
    n1 = 5900; n2 = 6025
    plot(x = n1:n2, y = C[n1:n2], main = "Window - Log(Close)", type = "l")
    plot(x = n1:n2, y = R[n1:n2], main = "Window - Returns", type = "l")
    ###
    
    # Responses:
    # Tomorrows returns - Shift One Day Back
    response = c(diff(C), 0) 
    ###
    
    # Predictors: 
    # Calculate and Plot(Window) some Selected Indicators:
    par(mfrow = c(3, 2))
    p01 = fpkTA(C, H, L, 12)
      plot(p01[n1:n2], main = "Predictor: %K[12]", type = "l")
    p02 = fpkTA(C, H, L, 12) - fpdTA(C, H, L, 12, 3)
      plot(p02[n1:n2], main = "Predictor: %K[12]-%D[12, 3]", type = "l")
    p03 = rsiTA(C, 6)-rsiTA(C, 12)
      plot(p03[n1:n2], main = "Predictor: RSI[6]-RSI[12]", type = "l")
    p04 = oscTA(C,3,10)
      plot(p04[n1:n2], main = "Predictor: OSC[C,3,6]", type = "l")
    p05 = cdoTA(C,11, 26, 9)
      plot(p05[n1:n2], main = "Predictor: CDO[C,12,26,9]", type = "l")
    p06 = wprTA(C, H, L, 5)
      plot(p06[n1:n2], main = "Predictor: WPR[C,H,L,5]", type = "l")
    ###
    
    # Save Responses and Predictors Pattern:
    if (dosave) {
      z = cbind.data.frame(response, p01, p02, p03, p04, p05, p06)
      names(z) = c("R[NYSE|-1]", "%K[12]", "%K[12]-%D[12|3]",
        "RSI[6]-RSI[12]", "OSC[C|3|6]", "CDO[C|12|26|9]", "WPR[C|H|L|5]")
      write.table(z, file, sep = ",", dimnames.write = "colnames") 
    }
    ### 

    
# ------------------------------------------------------------------------------


### Example: Pattern Tests - Identify Good Indicators

    # Perform correlation tests on windowed indicators 
    # to identify good indicators
    #   Results fromprevious example are required
    ###
    
    # Settings:
    # Read file from example xmpIndicatos.ssc
    data(spcindis)
    data = spcindis
    ###
    
    # Length of Window in Days:
    win.length = 5*252  # 5 years windows
    win.shift =   2*21  # bi-monthly shifted
    ###
    
    # Number of Window Cycles:
    iw = floor((length(data[,1])-win.length)/win.shift) - 1
    statistics = p.values = 
      matrix(rep(0, times = 6*iw), byrow = TRUE, ncol = 6)
    ###
    
    # Loop over all Windows:            
    n1 = 1 - win.shift
    n2 = win.length - win.shift
    for ( i in 1:iw ) {
      n1 = n1 + win.shift  # Start Window
      n2 = n2 + win.shift  # End Window
      for (j in 1:6) {
        result = cor.test(data[,j+1][n1:n2], data[,1][n1:n2], 
          method = "pearson")
        statistic = as.numeric(result$statistic)
        p.value = as.numeric(result$p.value)
        statistics[i,j] = statistic
        p.values[i,j] = p.value 
      }
    cat(i, "out of", iw, ":", n1,n2,"\n") 
    }
    ###

    # Plot Result:
    par(mfrow = c(4, 3), cex = 0.5)
    for (i in 1:6) 
      plot(statistics[,i], type = "l", main = "Statistics")
    for (i in 1:6) 
      plot(p.values[,i], type = "l", main = "p.value")
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Trading Models - Trade with the MACD Oscillator

    # Description:
    #   Compare two simple trading strategies based on trades with 
    #   the MACD Oscillator and on trades in the direction of the trend.


    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Read Data:
    data(spc1970)
    data = spc1970
    index = "spc1970"
    ### 
    
    # Work with Logarithmic Closings:
    O = log(data[,2]) # log Opening Price
    H = log(data[,3]) # log High Price
    L = log(data[,4]) # log Low Price
    C = log(data[,5]) # log Closing Price
    time = 1:length(C)
    date = 1970 + time/252 # Approximate decimal date
    par(mfrow = c(2, 2))
    plot(x = date, y = 100*(C-C[1]), col = "steelblue",  
        type = "l", main = paste("Index:", index))
    grid()
    ###
    
    # Trading Positions and Average Trade Lengths:
    # MACD Oscillator
    position = (c(0, diff(cdoTA(C, 5, 34, 7)))) 
    position = sign(position)
    signals = abs(c(0,diff(position),0))
    tl = emaTA(diff((1:length(signals))[signals>0.5]), 
        2/1261)
    plot(tl, type = "l", xlab = "Number of Trades", 
      col = "steelblue", ylab = "Length in Days", 
      main = "Averaged Trade Length")   
    grid()
    ###
        
    # Cumulated Return:
    returns = sign(position)*c(diff(C), 0)
    cumret = 100*cumsum(returns)
    plot(x = date, y = cumret, col = "steelblue",
        type = "l", main = "Cumulative Return")
    grid()
    ###
    
    # Annualized Returns:
    annualized = 100*252*emaTA(returns, 2/1261)
    plot(x = date, y = annualized, col = "steelblue",
        type = "l", main = "Annualized Returns")
    grid()
    ###

    
# ------------------------------------------------------------------------------


### Example: Trading Models - Trade in the Direction of the Trend   

    # Read Data:
    data(spc1970)
    data = spc1970
    index = "spc1970"   
    ###
    
    # Work with Logarithmic Closings:
    C = log(data[,5])      # log Opening Price
    time = 1:length(C)
    date = 1970 + time/252 # Approximate decimal date
    par(mfrow = c(2, 2))
    plot(x = date, y = 100*(C-C[1]), main = paste("Index:", index), 
        xlab = "Year", type = "l", col = "steelblue")
    grid()
    ###
        
    # Averaged Trade Lengths:
    # Trade for tomorrow in today's direction - go with the trend
    position = sign(c(0, diff(C)))
    signals = abs(diff(position))
    tl = emaTA(diff((1:length(signals))[signals > 0.5]), 2/1261)
    plot(tl, type = "l", xlab = "Number of Trades", ylab = "Length in Days",
      main = "Averaged Trade Length", col = "steelblue")
    grid()
    ###
        
    # Cumulated Return:
    returns = sign(position)*c(diff(C), 0)
    cumret = 100*cumsum(returns)
    plot(x = date, y = cumret, main = "Cumulative Return", 
        xlab = "Year", type = "l", col = "steelblue")
    grid()
    ### 
    
    # Annualized Returns:
    annualized = 100*252*emaTA(returns, 2/1261)
    plot(x = date, y = annualized, col = "steelblue",
      xlab = "Year", main = "Annualized Returns", type = "l")
    grid()
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Write Moving Average and Exponential MA functions

    # Description:
    #   Write two simple functions computing a simple Moving
    #   Average and an exponential Moving Average. Hint: Use
    #   the function "rollFun" for the SMA and "filter" for
    #   the function EWMA

    # The R functions can be found in "funSeries.R"

    
################################################################################

        