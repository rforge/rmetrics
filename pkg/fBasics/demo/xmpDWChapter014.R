#
# WARNING - NOT YET UPDATED TO R 2.4.0, THIS MAY RESULT IN ERRORS
#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 1.4
#   Stylized Facts, Structures and Dependencies
#
# List of Examples, Exercises and Code Snippets:
#
#       * Example: Plot Autocorrelation and Partial Autocorrelations
#   1.4.1 Example: Short-Term Autocorrelations
#   1.4.2 Example: Short-Term Partial Autocorrelations
#   1.4.3 Example: Long Memory Autocorrelation Function 
#   1.4.5 Example: Display the Taylor Effect
#   1.4.6 Example: Compare with Normal Rvs and AR Model
#   1.4.7 Example: Absolute Value Scaling
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################


### Load Packages:

    # require(fBasics)
    ###
    

# ------------------------------------------------------------------------------


### Example: Plot Autocorrelation and Partial Autocorrelations

    # This example plots the autocorrelation function and the 
    # partial autocorrelation function using acf() and pacf()
    # functions from R's base package - The pedestrian way ...

    # Settings:
    data(nyse)
    class(nyse)
    head(nyse)
    ###

    # Autocorrelations and Partial Autocorrelations:
    # ACF and PACF of NYSE log Returns - Use the second column
    # of the data frame which is the data column
    par(mfrow = c(2, 2), cex = 0.7)
    acf(diff(log(nyse[, 2])))
    pacf(diff(log(nyse[, 2])))
    ###
    
    
# ------------------------------------------------------------------------------


### 1.4.1  Example: Short-Term Autocorrelations

    # Plot the autocorrelation function for USDDEM FX BID returns 
    # for 30 min lags. The demo data file "usddem30u" contains the 
    # FX BID and ASK Rates. 
     
    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Load 30m USDDEM Data in Business Time:
    USDDEM.RET = returnSeries(as.timeSeries(data(usddem30u)))
    ###
    
    # Plot the ACF of the Return Series:
    acfPlot(USDDEM.RET[, "BID"], lag.max = 8, ylim = c(-0.05, 0.20),
        labels = FALSE)
    title(main = "Short Term ACF\n30 Minutes USDDEM", 
        xlab = "30 min Lags", ylab = "ACF")
    ###
    
    
# ------------------------------------------------------------------------------


### 1.4.1  Example: Short-Term Autocorrelations
    
    # Plot the PACF of the Return Series:
    pacfPlot(USDDEM.RET[, "BID"], lag.max = 8, ylim = c(-0.05, 0.20),
        labels = FALSE)
    title(main = "Short Term PACF\n30 Minutes USDDEM", xlab = "30 min Lags",
        ylab = "ACF")
    ###
    
    # Show the range of the data
    start(USDDEM.RET)
    end(USDDEM.RET)
    ###
   
 
# ------------------------------------------------------------------------------


### 1.4.2 Example: Long Memory Autocorrelation Function

    # Make a simple plot which displays the long-memory behavior of 
    # the 30 min USDDEM returns. 
    
    # Graphics Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
        
    # 30m USDDEM Exchange Rates in Business Time:
    USDDEM.RET = returnSeries(as.timeSeries(data(usddem30u)))
    # Volatility Series of Bid Prices:
    USDDEM.RET = USDDEM.RET[, "BID"]
    lmacfPlot(USDDEM.RET, lag.max = 48*14)
    # Output:
    # Long Memory Autocorrelation Function:
    # Maximum Lag        672
    # Cut-Off ConfLevel  0.006853398
    # Plot-Intercept     -1.65672
    # Plot-Slope         -0.1892507
    # Hurst Exponent     0.9053746
    ###
        
    # Daily NYSE Composite Index Series:
    NYSE.RET = returnSeries(as.timeSeries(data(nyse)))
    # Remove Return Value from Index Redefinition:
    NYSE.RET = outlier(NYSE.RET)
    lmacfPlot(NYSE.RET, lag.max = 63, type = "acf")
    title(main = "\n\nNYSE INDEX")
    grid()
    lmacfPlot(NYSE.RET, lag.max = 63, type = "hurst")
    title(main = "\n\nNYSE INDEX")
    grid()
    # Output:
    # Long Memory Autocorrelation Function:
    # Maximum Lag        63
    # Cut-Off ConfLevel  0.01978551
    # Plot-Intercept     -1.280071
    # Plot-Slope         -0.2805422
    # Hurst Exponent     0.8597289
    ###
    
    
# ------------------------------------------------------------------------------


### 1.4.3 Example: Plot USDDEM and NYSE Lagged Correlations

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
        
    # Load USDDEM Data, Convert to 'timeSeries' Object
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/"
    SRC = "Wuertz/data/usddem30u.csv"
    DATA = paste(URL, SRC, sep = "") 
    download.file(DATA, destfile = "usddem30u.csv")
    USDDEM = readSeries("usddem30u.csv")
    print(USDDEM[1,])
    print(end(USDDEM))
    # Extract Bid Series:
    USDDEM.BID = USDDEM[, "BID"]
    ###
    
    # Plot Lagged Correlations:
    lacfPlot(USDDEM.BID, n = 6, lag.max = 15)
    title(main = "\n\nUSDDEM: 30 min - 3 Business hours")
    lacfPlot(USDDEM.BID, n = 22, lag.max = 15)
    title(main = "\n\nUSDDEM 30 min day - 1 Business Day")
    ###
    
    # Load NYSE Data and Convert to timeSeries Object:
    NYSE = as.timeSeries(data(nyse))
    # Use only Data Before the Index Definition was Changed: 
    NYSE = cut(NYSE, "1966-01-01", "2002-12-31")
    ###
    
    # Plot Lagged Correlations:
    lacfPlot(NYSE, n = 5, lag.max = 15)
    title(main = "\n\nNYSE: 1 day - 1 week")
    lacfPlot(NYSE, n = 20, lag.max = 15)
    title(main = "\n\nNYSE: 1 day - 1 month")
    ###
    
    
# ------------------------------------------------------------------------------
    
    
### 1.4.3 Example: Plot Lagged Correlations from Simulated Series

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Load NYSE Data and Convert to timeSeries Object:
    NYSE = as.timeSeries(data(nyse))
    # Use only Data Before the Index Definition was Changed: 
    NYSE = cut(NYSE, "1966-01-01", "2002-12-31")
    ###
    
    # Simulate Index by Normal Series:
    set.seed(4711)
    nyse.ret = as.vector(returnSeries(NYSE))
    Mean = mean(nyse.ret)
    SD = sd(nyse.ret)
    nyse.norm = rnorm(length(nyse.ret), mean = Mean, sd = SD)
    nyse.norm = exp(cumsum(nyse.norm))
    lacfPlot(nyse.norm, n = 5, lag.max = 15)
    title(main = "\n\nNormal Series: 1 day - 1 week")
    ###
    
    # Simulate Index by AR(1) autoregresssive Series:
    set.seed(4711)
    ar = pacf(nyse.ret, plot = FALSE)$acf[1]
    nyse.ar = arima.sim(length(nyse.ret), model = list(ar = ar))
    nyse.ar = ((nyse.ar-mean(nyse.ar))/sd(nyse.ar) ) * SD + Mean
    nyse.ar = exp(cumsum(nyse.ar))
    lacfPlot(nyse.ar, n = 5, lag.max = 15)
    title(main = "\n\nAR(1) Series: 1 day - 1 week")
    ###
    
   
# ------------------------------------------------------------------------------


### 1.4.4 Example: Display the Taylor Effect

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Taylor Effect - NYSE Data:
    NYSE = as.timeSeries(data(nyse))
    NYSE.RET = outlier(returnSeries(NYSE))
    teffectPlot(NYSE.RET, deltas = seq(from = 0.2, to = 3, by = 0.1))
    title(main = "\n\nNYSE Composite Index", cex = 0.5)
    ### 
    
    # Taylor Effect - USDCHF Data:
    USDCHF.RET = returnSeries(as.timeSeries(data(usdchf)))
    teffectPlot(USDCHF.RET, deltas = seq(from = 0.2, to = 3, by = 0.1))
    title(main = "\n\nUSDCHF Exchange Rate", cex = 0.5)
    ###
    
    
# ------------------------------------------------------------------------------


### 1.4.5 Example: Compare with Normal Rvs and AR Model

    # Normal RVs:
    Mean = mean(as.vector(NYSE.RET)); SD = sd(as.vector(NYSE.RET))
    RNORM = rnorm(9813, mean = Mean, sd = SD)
    teffectPlot(RNORM, deltas = seq(from = 0.2, to = 3, by = 0.1))
    title(main = "\n\nNYSE - Normal RVs", cex = 0.5)
    ###
        
    # Simulated AR Model - Standardised :
    AR = arima.sim(9813, model = list(ar = c(0.124, -0.033)))
    AR = ( (AR-mean(AR))/sd(AR) ) * SD + Mean
    teffectPlot(AR, deltas = seq(from = 0.2, to = 3, by = 0.1))
    title(main = "\n\nNYSE - AR Model", cex = 0.5)
    ###
    
    
# ------------------------------------------------------------------------------


### 1.4.6 Example: Absolute Value Scaling

    # Absolute Value Scaling of daily NYSE Index:
    NYSE = outlier(returnSeries(as.timeSeries(data(nyse))))
    scalinglawPlot(NYSE, span = 6)$fit$coefficients
    title(main = "\n\nNYSE")
    # Output:
    # Intercept         X
    # 4.1767239 0.5277384
    ###
    
    # Absolute Value Scaling of 30m USDCHF Rates:
    USDCHF = returnSeries(as.timeSeries(data(usdchf)))
    scalinglawPlot(x = USDCHF, span = 6)$fit$coefficients
    title(main = "\n\nUSDCHF")
    # Output:
    # Intercept         X
    # 3.7083449 0.5225638
    ###
    
    
# ------------------------------------------------------------------------------


### CodeSnippet: 'scalinglawPlot' Function

    # Function:
    .scalinglawPlot = function (x, span = 6)
    {
        # We expect a Return series from the Input:
        x = as.vector(x); y = (x - mean(x))
        logprices = cumsum(y)

        # Internal Scaling Function - Absolute Value Scaling:
        scale = function(n, logprices) {
            sum(abs(diff(logprices, lag = (2^n)))) }

        # Aggregate on log(2) Scale:
        x = (0:span) * log(2)
        y = log(apply(matrix(0:span), 1, scale, logprices))

        # Fit a straight Line:
        fit = lsfit(x, y)$coefficient[[2]]

        # Return Value:
        c(d = fit, alpha = 1/fit)
    }
    ###
    
    # Try:
    .scalinglawPlot(x = NYSE)
    # Output:
    #         d     alpha
    # 0.5277384 1.8948784
    ###
    
    
################################################################################

