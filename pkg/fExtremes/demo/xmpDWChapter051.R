#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 5.1
#   Exploratory data Analysis of Extremes
#
# List of Examples, Exercises and Code Snippets:
#
#   5.1.1 Example: Quantile-Quantile Plot
#       * Code Snipptet: qqPlot
#       * Example: Create Figure 5.1.1 - DAX and BMW Data
#   5.1.2 Example: Mean Excess Function Plot - Create Figure 5.1.2
#       * Code Snippet: mxfPlot
#   5.1.3 Example: Mean Residual Life Plot - Create Figure 5.1.3
#       * Code Snippet: mrlPlot
#       * Code Snippet: ssrecordsPlot
#   5.1.4 Example: Subsample Records Plot
#   5.1.5 Example: Records Plot
#   5.1.6 Example: Ratio of Maximum and Sum Plot
#   5.1.7 Example: Laws of Large Numbers
#   5.1.8 Example: ACF of Exceedences
#       * Addon: Extreme Data Preprocessing
#
#
# Author:
#   (C) 2002-2004, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################


### Load Library:

    # Load:
    require(fExtremes)
    ###
    
    
# ------------------------------------------------------------------------------


### 5.1.1 Example: Quantile-Quantile Plot

    # Load and Plot the Data 
    DAX.RET = as.timeSeries(data(daxRet))
    class(DAX.RET)
    head(DAX.RET)
    qqPlot(DAX.RET, pch = 19, col = "steelblue", cex = 0.7)
    ###
    
    
# ------------------------------------------------------------------------------


### Code Snipptet: qqPlot
    
    # Quantile-Quantile Plot:
    .qqPlot = function(x, ...)
    {
        x = as.vector(x)
        qqnorm(x, ...)
        qqline(x)
        invisible()
    }
    DAX.RET = as.timeSeries(data(daxRet))
    .qqPlot(DAX.RET)
    ###
    
  
# ------------------------------------------------------------------------------


### Example: Create Figure 5.1.1 - DAX and BMW Data

    # Graph Frame:
    par(mfcol = c(2, 2), cex = 0.7)
    ###
    
    # Load and Plot DAX Data 
    DAX.RET = as.timeSeries(data(daxRet))  
    plot(DAX.RET, main = "DAX Daily log Returns", ylab = "log Return")
    qqPlot(DAX.RET)
    ###

    # Load and Plot BMW Data 
    BMW.RET = as.timeSeries(data(bmwRet))
    plot(BMW.RET, main = "BMW Daily log Returns", ylab = "log Return")
    qqPlot(BMW.RET)
    ###
    
    
# ------------------------------------------------------------------------------
    
 
### 5.1.2 Example: Mean Excess Function Plot - Create Figure 5.1.2

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Exponential Variates:
    set.seed(4711)
    mxfPlot(rexp(1000, rate = 2), tail = 0.20)
    title(main = "\n\nExponential DF")
    abline(0.5, 0)
    ###

    # Normal Variates:
    set.seed(4711)
    mxfPlot(rlnorm(1000, meanlog = 0, sdlog = 2), tail = 0.20)
    title(main = "\n\nLognormal DF")
    ###

    # Symmetric Stable Variates:
    set.seed(4711)
    mxfPlot(rsymstb(1000, alpha = 1.7), tail = 0.20)
    title(main = "\n\n1.7 stable DF")
    abline(0, 0.7)
    ###
    
    # DAX log Returns:
    mxfPlot(-100*DAX.RET, tail = 0.20)
    title(main = "\n\nDAX log Returns %")
    ###
    
    
# ------------------------------------------------------------------------------
   

### Code Snippet: mxfPlot

    # Function:
    .mxfPlot = function(x, tail = 0.05, main = "me-Plot", ...)
    {
        # Compute u and e(u)
        u = rev(sort(x))
        n = length(x)
        u = u[1:floor(tail*n)]
        n = length(u)
        e = (cumsum(u)-(1:n)*u)/(1:n)

        # Generate Plot and Return u and e(u):
        plot (u, e, main = main, ...)
        list(x = u, y = e)
    }
    ###

    # Try:
    par(mfrow = c(2, 2), cex = 0.7)
    .mxfPlot(rnorm(1000), tail = 0.10)
    grid()
    .mxfPlot(rt(1000, df = 4), tail = 0.10)
    grid()
    ###


# ------------------------------------------------------------------------------
 
    
### 5.1.3 Example: Mean Residual Life Plot - Create Figure 5.1.3

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7) 
    ###
    
    # Settings: 
    seed = c(456, 745, 145, 120)        
    mains = c(
        "MRL Plot - Sample 1", "MRL Plot - Sample 2", 
        "MRL Plot - Sample 3", "MRL Plot - Sample 4") 
    n = 5000

    # Create Plots With Your Own Labels and Title:
    for (i in 1:4) {
        set.seed(seed[i])
        mrlPlot(rsymstb(n, alpha = 1.7), nint = 100, 
            labels = FALSE, plottype = "", xlim = c(0, 60), 
            ylim = c(-50, 150))
        title(xlab = "u", ylab = "e", main = mains[i])  
        grid()      
    } 
    ###
    
    
# ------------------------------------------------------------------------------
   

### Code Snippet: mrlPlot

    # Function:
    .mrlPlot = function(x, conf = 0.95, nint = 100, ...)
    {
        sx = xu = xl = rep(NA, nint)
        u = seq(mean(x), max(x), length = nint)
        for(i in 1:nint) {
            x = x[x >= u[i]]; sx[i] = mean(x - u[i])
            sigma = qnorm((1 + conf)/2) * sqrt(var(x))
            xu[i] = sx[i] + sigma/sqrt(length(x))
            xl[i] = sx[i] - sigma/sqrt(length(x)) }

        # Plot:
        plot(u[!is.na(xl)], sx[!is.na(xl)], type = "l",
            ylim = range(c(xl, xu), na.rm = TRUE), ...)
        lines(u[!is.na(xl)], xl[!is.na(xl)], col = "steelblue")
        lines(u[!is.na(xu)], xu[!is.na(xu)], col = "steelblue")
    }
    ###

    # Try:
    par(mfrow = c(1, 1))
    .mrlPlot(rt(5000, 4), xlab = "u", ylab = "e(u)")
    title(main = "Mean Residual Live Plot")
    grid()
    ###
    

# ------------------------------------------------------------------------------
   

### Code Snippet: ssrecordsPlot

    # Function:
    .ssrecordsPlot = function (x, subsamples = 10)
    {
        cluster = floor(length(x)/subsamples)
        save = x
        records = c()
        for (i in 1:subsamples) {
            x = save[((i-1)*cluster+1):(i*cluster)]
            y = 1:length(x)
            u = x[1]; v = x.records = 1
            while (!is.na(v)) {
                u = x[x > u][1]; v = y[x > u][1]
                if(!is.na(v)) x.records = c(x.records, v)
            }
            if (i == 1) {
                nc = 1:length(x)
                csmean = cumsum(1/nc)
                cssd = sqrt(cumsum(1/nc-1/(nc*nc)))
                ymax = csmean[length(x)]+2*cssd[length(x)]
                plot (nc, csmean+cssd, type = "l", ylim = c(0, ymax))
                lines(nc, csmean); lines(nc, csmean-cssd)
            }
            y.records = 1:length(x.records)
            x.records = x.records[y.records < ymax]
            points(x.records, y.records[y.records < ymax], col = i)
            records[i] = y.records[length(y.records)]
        }
        records
    }
    ###
    
    # Try:
    par(mfrow = c(1, 1))
    .ssrecordsPlot(rnorm(1000), 10)
    title(main = "Subrecords Plot")
    ###
    
    
    
# ------------------------------------------------------------------------------

        
### Example: Mean Excess Function Plot
    
    # Exponential distribution function:
    set.seed(7138)
    mxfPlot(rexp(n, rate = 2), tail = 1, labels = FALSE)
    title(xlab = "Threshold: u", ylab = "Mean Excess: e",
        main = "Exponential DF")
    abline(0.5, 0)
    ###
    
    # Lognormal distribution function
    set.seed(6952)
    mxfPlot(rlnorm(n, meanlog = 0, sdlog = 2), tail = 1,
        xlim = c(0, 90), ylim = c(0, 150), labels = FALSE)
    title(xlab = "Threshold: u", ylab = "Mean Excess: e",
        main = "Lognormal DF")
    ###
        
    # Alpha-stable distribution function:
    set.seed(9835)
    mxfPlot(rsymstb(n, alpha = 1.7), tail = 0.1,
        xlim = c(0, 10), ylim = c(0, 6), labels = FALSE)
    title(xlab = "Threshold: u", ylab = "Mean Excess: e",
        main = "1.7 stable DF")
    abline(0, 0.7)
    ###

    
# ------------------------------------------------------------------------------

     
### Example 5.1.4: Subsample Records Plot

    
    # Graph Frame:
    par(mfrow = c(3, 2), cex = 0.7)
    ###
    
    # Simulate Stable Data:
    stable = rsymstb(n = 8000, alpha = 1.7)
    ###

    # Load BMW and NYSE Data:
    data(bmwres)
    data(nyseres)
    ###

    # Plot on Logarithmic Scale:
    ssrecordsPlot(stable, subsamples = 8, plottype = "log")
    title(main = "\n\n1.7-stable Returns")
    ssrecordsPlot(bmwres, subsamples = 6, plottype = "log")
    title(main = "\n\nBMW Returns")
    ssrecordsPlot(nyseres, subsamples = 8, plottype = "log")
    title(main = "\n\nNYSE Returns")
    ###

    # Plot on Linear Scale:
    ssrecordsPlot(stable, subsamples = 8, plottype = "lin")
    title(main = "\n\n1.7-stable Returns")
    ssrecordsPlot(bmwres, subsamples = 6, plottype = "lin")
    title(main = "\n\nBMW Returns")
    ssrecordsPlot(nyseres, subsamples = 8, plottype = "lin")
    title(main = "\n\nNYSE Returns")
    ###
    
  
# ------------------------------------------------------------------------------

  
### Example 5.1.5: Plot of Records

    # Graph Frame:
    par(mfrow = c(1, 1))
    ###
    
    # Normal Records Plot:
    ans = recordsPlot(rnorm(50000))
    print(ans)
    ###

    
# ------------------------------------------------------------------------------


### Example 5.1.6: Ratio of Maximum and Sum Plot

    # Graph Frame:
    par(mfrow = c(3, 2), cex = 0.7)
    
    # Load Data:
    data(bmwres)
    data(nyseres)
    ###

    # Create Plots:
    msratioPlot (rnorm(1000))
    title(main = "\n\nStandard Normal")
    msratioPlot (rexp(10000))
    title(main = "\n\nExponential")
    msratioPlot (rt(10000, 4))
    title(main = "\n\nStudent-t")
    msratioPlot (rsymstb(1000, 1))
    title(main = "\n\nCauchy")
    msratioPlot (bmwres)
    title(main = "\n\nBMW Returns")
    msratioPlot (nyseres)
    title(main = "\n\nNYSE Returns")
    ###
    
    
# ------------------------------------------------------------------------------


#   5.1.7 Example: Laws of Large Numbers

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Kolmogorov's Strong Law of Large Numbers:
    # Revert the Series for the Loss Tail ...
    sllnPlot(-as.timeSeries(data(daxRet)))
    ###

    # Hartman-Wintner's Law of the Iterated Logarithm
    lilPlot(-as.timeSeries(data(daxRet)))
    ###

    
# ------------------------------------------------------------------------------


#   5.1.8 Example: ACF of Exceedences

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # ACF of Exceedences - Gains:
    xacfPlot(as.timeSeries(data(daxRet)))
    ###
    
    # ACF of Exceedences - Losses:
    xacfPlot(as.timeSeries(data(daxRet)))
    ###
    
    
# ------------------------------------------------------------------------------


#   Data Preprocessing:

    # BMW Losses:
    x = -as.timeSeries(data(bmwRet))
    
    # Monthly Losses:
    blockMaxima(x, block = "monthly", doplot = FALSE) 
    # 20 Day Losses:
    blockMaxima(x, block = 20, doplot = FALSE) 
    ###
 
    # 5% Threshold Values:
    numberOfPeaks = floor(0.05*length(as.vector(x)))
    print(numberOfPeaks)
    findThreshold(x, n = numberOfPeaks)
    xPP = pointProcess(x, n = numberOfPeaks)
    par(mfrow = c(2, 2), cex = 0.7)
    deCluster(x = xPP, run = 15, doplot = TRUE)
    # Compare with other 'run' lengths ...
    ###
    
 
################################################################################

   