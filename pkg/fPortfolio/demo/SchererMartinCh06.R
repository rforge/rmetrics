




################################################################################
# 6.1 Outliers and Non-Normal Returns


    # Data:
    EVST = yahooSeries(symbol = "EVST", from = "1996-12-01", to = "2001-12-31", 
        quote = "Close", aggregation = "m", getReturn = TRUE)
     
    # Figure 6.1        
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
    
    
    
    


################################################################################
# 6.2 Robust Statistics versus Classical Statistics


    

################################################################################
# 6.3 Robust Estimates of Mean Returns
  

    # Data:
    EVST = yahooSeries(symbol = "EVST", from = "1996-12-01", to = "2001-12-31", 
        quote = "Close", aggregation = "m", getReturn = TRUE)
        
    # Median:  
    median(EVST@Data)
    
    # Trimmed Mean:
    mean(EVST@Data, trim = 0.1)
    
    # Outlier adjusgted Mean:
    mean(EVST@Data[EVST@Data < 2])


################################################################################
# 6.4 Robust Estimates of Volatility


    # Load Data:
    ZIF = yahooSeries(symbol = "ZIF", from = "1996-12-01", to = "2001-12-31", 
        quote = "Close", aggregation = "m", getReturn = TRUE)
        
    # Figure 6.10:
    par(mfrow = c(2, 1), cex = 0.7)
    plot(ZIF, type = "o", pch = 19)
    assetsHistPlot(ZIF, labels = FALSE, xlim = c(-0.3, 0.3), n = 20)
    
    
    assetsMeanCov(ZIF, method = "cov")
    assetsMeanCov(ZIF, method = "mve")
    assetsMeanCov(ZIF, method = "mcd")
    

################################################################################
# 6.5 Robust Betas


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


################################################################################
# 6.6 Robust Correlations and Covariances


################################################################################
# 6.7 Robust Distances for Determining Normal Times versus Hectic Times


    # Data:
    require(MASS)
    data(berndtInvest)
    units = c("CONTIL", "MOBIL", "TEXACO", "GERBER")
    myAssets = as.timeSeries(berndtInvest)[, units]

    assetsMahalabonisPlot =
    function(x, method = c("mve", "mcd", "cov"))
    {
        X = as.matrix(x)
        method = match.arg(method)
        
        # Mean and Covariance:
        MV = assetsMeanCov(X, method = method)
        
        # Mahalabonis Distance:
        M = mahalanobis(X, center = MV$mu, cov = MV$Omega)
        
        # Plot:
        plot(M, type = "h", ylim = c(0, 200), pch = 19)
        grid()
        
        # Return Value:
        M
    }
    
    par (mfrow = c(2, 2), cex = 0.7)
    assetsMahalabonisPlot(myAssets, "cov")  # Error!
    assetsMahalabonisPlot(myAssets, "mve")
    assetsMahalabonisPlot(myAssets, "mcd")


################################################################################
# 6.8 Robust Covariances and Distances with Different Return Histories


################################################################################
# 6.9 Robust Portfolio Optimization


################################################################################
# 6.10 Conditional Value-at-Risk Frontiers: Classical and Robust


################################################################################
# 6.11 Influence Functions for Portfolios

  
   
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
    assetsmeanCov(myAssets, "mcd")
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


