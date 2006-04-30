
#
# Description:
#   Portfolio Assets Modelling
#
# Description:
#   This example shows how to select the four most dissimilar assets 
#   from Berndt's data set and how to fit the distributional paramaters 
#   for these assets. Finally we show how to simulate an artificial 
#   data set of assets with distributions from fitted parameters.
#
# Content:
#   1. Select the 4 most dissimilar assets from Berndt's data set
#   2. Fit the distributional parameters for Berndt's data set
#   3. Simulate an artificial data set of assets
#
# Last Check:
#   2006-02-05 ok
#
# Author:
#   (C) 2003-2006, Diethelm Wuertz, GPL
#


################################################################################
# Requirements:

    
    # Load Packages:
    require(fPortfolio)
    ###
    

################################################################################
# 1. Select the 4 most dissimilar assets from Berndt's data set


    # The data set "berndtInvest" is from Berndt's textbook 
    # "The Practice of Econometrics". It is a data.frame consisting
    # of 18 columns with the following entries:
    #  [1] %d/%B/%y "CITCRP" "CONED"  "CONTIL" "DATGEN" "DEC"      
    #  [7] "DELTA"  "GENMIL" "GERBER" "IBM"    "MARKET" "MOBIL"    
    # [13] "PANAM"  "PSNH"   "TANDY"  "TEXACO" "WEYER"  "RKFREE"  
    # The first column holds the date, the 11th the market rate,
    # and the last (the 18th) the risk free rate.
    ###
        
    
    # Load the Data and Create an Object of Class 'timeSeries':
    data(berndtInvest)
    berndtInvest = as.timeSeries(berndtInvest)
    class(berndtInvest)
    head(berndtInvest)
    ###
    
    
    # Exclude the Date, Market Returns and Interest Rate Columns 
    # from the data frame, then multiply by 100 for percentual returns ...
    allAssets = 100 * berndtInvest[, -c(1, 10, 17)]
    class(allAssets)
    head(allAssets)
    ###
        
    
    # Graph Frame:
    par(mfrow = c(2, 1), cex = 0.7)
    ###
    
    
    # Select the "n" Most Dissimilar Assets from 
    # Hierarchical Clustering:
    n = 4
    args(assetsSelect)
    clustered = assetsSelect(allAssets, doplot = TRUE)
    # Create my Assets Set from the "n" selected Symbols:
    myAssets = allAssets[, c(clustered$order[1:n])]
    colnames(myAssets)
    # Print the Column Return:
    mu.vec = colAvgs(myAssets)
    mu.vec
    # or ...
    mu.vec = colMeans(myAssets@Data)
    mu.vec
    # Print the Covariance Matrix:
    cov.mat = cov(myAssets@Data)
    cov.mat
    ###
          
      
    # Plot Cumulated Returns of the Assets:
    ts.plot(colCumsums(myAssets), col = 1:4)
    grid()
    legend(0, 300, legend = colnames(myAssets), pch = "----", col = 1:4)
    title(main = "Cumulated Returns", ylab = "Cumulated Returns")
    abline(h = 0, lty = 3)
    ###


################################################################################
# 2. Fit the distributional parameters for Berndt's data set


    # How do we classify a Portfolio ?
    # We classify a portfolio by three parameters:
    # 1)  the location - mean vector, named "alpha"
    # 2)  the scale    - covariance matrix, named "Omega"
    # 3)  the shape    - skewness vector, named "beta" 
    # 4)               - degrees of freedom (kurtosis)
    # The function "assetsFit" fits the parameters of the distribution
    # by maximum log-likelihood estimation.
    ###
    
    
    # Data:
    data(berndtInvest)
    stocks = c("CONTIL", "MOBIL", "TEXACO", "GERBER")
    myAssets = 100 * as.timeSeries(berndtInvest)[, stocks]
    ###
    
    
    # Fit myAsssets Data Set:   
    # Fit a multivariate normal distribution:
    fit.norm  = assetsFit(x = myAssets, method = "norm")
    fit.norm
    ###
    
    
    # Fit a multivariate skewed normal distribution:
    fit.snorm = assetsFit(x = myAssets, method = "snorm") 
    fit.snorm
    ###
    
    
    # Fit a multivariate skewed Student-t distribution:
    fit.st = assetsFit(x = myAssets, method = "st")
    fit.st
    ###
    
    
    # Show Class and Slot Names:
    class(fit.st)
    slotNames(fit.st)
    print(fit.st@model)
    class(fit.st@model)
    ###
    
        
    # Investigate the fitted Parameters:
    par(mfrow = c(1, 1), cex = 0.7)
    # Extract mu, Omega, beta and df:
    alpha = fit.st@model$alpha
    Omega = fit.st@model$Omega
    mu = c(fit.st@model$mu, fit.st@model$df)
    # Create a matrix "z" where the last row holds the returns,
    # the most right column the skewness values, and the last
    # element the number of degrees of freedom:
    z = cbind(rbind(sqrt(Omega), alpha), mu)
    # Create entries x, y, z for Plotting ...
    m = 4
    n = m + 1
    x = rep(1:n, times = n)
    y = rep(1:n, each = n)
    z = as.vector(z)
    # Plot:
    plot(x, y, type = "n", xlim = c(-0.5, n+0.5), ylim = c(0, n+0.5),
        xlab = "", ylab = "")
    abline(h = n - 0.5, lty = 3)
    abline(v = n - 0.5, lty = 3)
    abline(h = 0.5, lty = 3)
    abline(v = 0.5, lty = 3)
    symbols(x, y, squares = abs(z), inches = 0.25, add = TRUE, 
        bg = "steelblue4" )
    text(1:n, rep(0, n), c(colnames(myAssets), "skew") )
    text(rep(0, n), 1:n, c(colnames(myAssets), "mean") )
    title(main = "Parameter Plot")
    text(n+0.5, n+0.5, "df")
    text(1:n, rep(n, n)-0.25, as.character(round(mu, 3)), col = "orange")
    ###
        
    # Use methods:
    print(fit.st)
    plot(fit.st, which = 1)
    ###
    

################################################################################
# 3. Simulate an artificial data set of assets with the same distributional
#    properties as Berndt's data set


    # Two ways for Simulation:
    # 1 We classify a market Portfolio using the fitted
    #   parameters as innput for the simulation 
    # 2 We start from scratch, define location, scale and shape 
    #   parameters, and generate rvs from the skewed Normal or 
    #   Student-t (fat tailed) multivariate distribution.   
    # For details we refer to Rmetric's 'Multivariate Distribution'
    ###

    
    # Data:
    data(berndtInvest)
    units = c("CONTIL", "MOBIL", "TEXACO", "GERBER")
    myAssets = 100 * as.timeSeries(berndtInvest)[, units]
    ###
    
    
    # Method 1:
    simulatedAssets = assetsSim(
        n = dim(myAssets)[1], 
        model = fit.st@model)
    simulatedAssets
    ###
    
    
    # Method 2:
    mu = fit.st@model$mu
    Omega = fit.st@model$Omega
    alpha = fit.st@model$alpha
    assetsSim(
        n = 120, 
        model = list(mu = mu, Omega = Omega, alpha = alpha, df = Inf))
    ###
    
    
    # Plot Cumulated Returns of the Assets:
   
    assetsSeriesPlot =
    function(x, labels = TRUE)
    {   # A function implemented by Diethelm Wuertz
    
        # Description:
        #   Plots the series of prices or indices of a set of assets
        #   given their returns.
        
        # Arguments:
        #   x - a timeSeries object or any other rectangular object
        #       which can be transformed by the function as. matrix
        #       into a numeric matrix.
        #   labels - a logical flag. Should labels and a legend added 
        #       to the plot?
        
        # FUNCTION:
        
        # Plot:
        x = as.matrix(x)
        X = colCumsums(x)
        nCols = dim(x)[2]
        ts.plot(X, col = 1:nCols, xlab = "", ylab = "", main = "")
        abline(h = 0, lty = 3) 
        
        # Add Labels:
        if (labels) {
            legend(0, max(X), legend = colnames(x), col = 1:nCols, lty = 1)
            title(
                main = "Cumulated Returns", 
                xlab = "Time",
                ylab = "Cumulated Returns")
        }     
        
        # Return Value:
        invisible()
    }
    
    par(mfrow = c(1, 1))
    assetsSeriesPlot(myAssets)
    ###
    
    
################################################################################
# Robust Mean and Covarianre Estimates:
    
   
    # Data:
    data(berndtInvest)
    units = c("CONTIL", "MOBIL", "TEXACO", "GERBER")
    myAssets = 100 * as.timeSeries(berndtInvest)[, units]
    class(myAssets)
    summary(myAssets)
    ###
    
    
    # Load MASS Library:
    require(MASS)
    corpcorBuiltin()
    covrobustBuiltin()  
    ### 

    
    # Classical Robust Estimation: 
    assetsMeanCov(myAssets, "cov")
    assetsMeanCov(myAssets, "mve")
    assetsMeanCov(myAssets, "mcd")
    assetsMeanCov(myAssets, "nnve")
    ###
    
    
    # Histogram Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    assetsHistPlot(myAssets, "cov")
    assetsHistPlot(myAssets, "mve")
    assetsHistPlot(myAssets, "mcd")
    ###
    
    
    par(mfrow = c(1, 1))
    assetsPairsPlot(myAssets)
    assetsCorTestPlot
    ###
            
    
    par(mfrow = c(2, 2), cex = 0.7)
    assetsQQNormPlot(myAssets)
    assetsQQNormPlot(myAssets, which = 2, labels = FALSE, ylim = c(-50, 50))
    ###


################################################################################
# Mahalanobis

    # Data:
    data(berndtInvest)
    units = c("CONTIL", "MOBIL", "TEXACO", "GERBER")
    myAssets = as.timeSeries(berndtInvest)[, units]
    
    
    par (mfrow = c(2, 2), cex = 0.7)
    
    MV = assetsMeanCov(myAssets, "mcd")
    X = mahalanobis(as.matrix(myAssets), center = MV$mu, cov = MV$Omega)
    plot(X, type = "h", ylim = c(0, 200), pch = 19)
    # points(X, pch=19, cex = 0.5, col = "steelblue")
    
    MV = assetsMeanCov(as.matrix(myAssets), "cov")
    X = mahalanobis(as.matrix(myAssets), center = MV$mu, cov = MV$Sigma)
    plot(X, type = "h", ylim = c(0, 200), pch = 19)
    # points(X, pch=19, cex = 0.5, col = "red")
    
    
   




assetsBeta =
function(x,  columns = NULL,  method = c("lm", "lms", "lts"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Estimation of Beta using classical regression or alternatively
    #   regression to the good points in the dataset, thereby achieving 
    #   a regression estimator with a high breakdown point.
    
    # Details:
    #   Uses one of the functions lm() lmsreg() or ltsreg().
    
    # Example:
    #   x = yahooSeries(symbols = c("^DJI", "IBM"), getReturns = TRUE)
    #   assetsBeta(x, columns = c("^DJI.Close", "IBM.Close"))
    #   assetsBeta(x)
    
    # FUNCTION:
    
    # Settings:
    method = match.arg(method)
    
    # Columns
    if (is.null(columns)) {
        X = as.data.frame(x[, c(1, 2)])
    } else {
        X = as.data.frame(x[, columns])
    }    
    
    # Allow formula expression for colnames with ^ prefix:
    colnames(X) = UNITS = gsub("\\^", "", colnames(X))
    
    # Fit:
    fun = c(lm = "lm", lms = "lmsreg", lts = "ltsreg")
    FUN = match.fun(fun[method])
    fit = FUN(formula = as.formula(paste(UNITS[1], "~", UNITS[2])), data = X)
    fit$call = match.call()
    Beta = c(Beta = fit$coef[2])
    
    # Add Control Attroibute:
    attr(Beta, "control") = c(method = method)
    
    # Return Value:
    Beta
}




assetsBeta(x)


    # Data:
    EVST = yahooSeries(symbol = "EVST", from = "1996-12-01", to = "2001-12-31", 
        quote = "Close", aggregation = "m", getReturn = TRUE)
    plot(EVST, type = "o")
    
    EVST@Data[EVST@Data>2, ] = 6.88
    plot(EVST, type = "o", pch = 19, cex = 0.7, main = "EVST Returns",
        ylab = "RETURNS", xlab = "YEAR")
  

    mean(EVST@Data)
    sd(EVST@Data)
    
    EVST[EVST@Data>3.3, ]
    
    1-pnorm(max(EVST@Data), mean(EVST@Data), sd(EVST@Data))
    pnorm(max(EVST@Data), mean(EVST@Data), sd(EVST@Data), lower.tail = FALSE)
    
    
    qqnorm(EVST@Data, pch = 19, cex = 0.7)
    qqline(EVST@Data)
    grid()
    
    qqnorm(EVST@Data[EVST@Data < 2], pch = 19, cex = 0.7)
    qqline(EVST@Data)
    grid()
    
    median(EVST@Data)
    mean(EVST@Data, trim = 0.1)
    mean(EVST@Data[EVST@Data < 2])
    
    
    
    
    # Chapter 6.4
    
    ZIF = yahooSeries(symbol = "ZIF", from = "1996-12-01", to = "2001-12-31", 
        quote = "Close", aggregation = "m", getReturn = TRUE)
    plot(ZIF, type = "o", pch = 19)
    
    
    
    assetsHistPlot(ZIF, labels = FALSE, xlim = c(-0.3, 0.3), n = 20)
    
    assetsMeanCov(ZIF, method = "cov")
    assetsMeanCov(ZIF, method = "mve")
    assetsMeanCov(ZIF, method = "mcd")

    
################################################################################
# 6.5 Robust Betas 



