
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                   TIME SERIES ASSETS PLOTS:
#  assetsReturnPlot            Displays time series of individual assets
#  assetsCumulatedPlot         Displays time series of individual assets
#  assetsSeriesPlot            Displays time series of individual assets
#  assetsHistPlot              Displays histograms of individual assets 
#  assetsLogDensityPlot        Displays a pdf plot on logarithmic scale 
#  assetsQQNormPlot            Displays normal qq-plots of individual assets
# FUNCTION:                   MULTIVARIATE RISK PLOTS:
#  assetsRiskReturnPlot        Displays risk-return giagram of assets 
#  assetsNIGShapeTrianglePlot  Displays NIG Shape Triangle
# FUNCTION:                   BIVARIATE HISTOGRAM PLOTS:
#  assetsHistPairsPlot         Displays bivariate Histogram Plot
################################################################################


################################################################################
#  assetsReturnPlot            Displays time series of individual assets
#  assetsCumulatedPlot         Displays time series of individual assets
#  assetsSeriesPlot            Displays time series of individual assets
#  assetsHistPlot              Displays histograms of individual assets 
#  assetsLogDensityPlot        Displays a pdf plot on logarithmic scale 
#  assetsQQNormPlot            Displays normal qq-plots of individual assets


assetsReturnPlot =
function(x, col = "steelblue", ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays histograms of individual assets 
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    
    # FUNCTION:

    # Settings:
    n = ncol(x)
    if (length(col) == 1) col = rep(col, times = n)
    
    # Plot:
    seriesPlot(x, ylab = "Returns", col = col, ...)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsCumulatedPlot =
function(x, col = "steelblue", ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays histograms of individual assets 
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    
    # FUNCTION:

    # Settings:
    n = ncol(x)
    if (length(col) == 1) col = rep(col, times = n)
    
    # Plot:
    x = exp(colCumsums(x))
    seriesPlot(x, ylab = "Cumulated Returns", col = col, ...)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsSeriesPlot =
function(x, col = "steelblue", ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays histograms of individual assets 
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    
    # FUNCTION:

    # Settings:
    n = ncol(x)
    if (length(col) == 1) col = rep(col, times = n)
    
    # Plot:
    seriesPlot(x, ylab = "Series", col = col, ...)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsHistPlot =
function(x, col = "steelblue", skipZeros = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays histograms of individual assets 
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix. 
    
    # FUNCTION:

    # Settings:
    n = ncol(x)
    if (length(col) == 1) col = rep(col, times = n)
    
    # Plot:
    for (i in 1:n) {
        X = x[, i]
        if (skipZeros) X = X[X@Data != 0]
        histPlot(X, ylab = "Cumulated Returns", col = col[i], ...)
    }
        
    # Return Value:
    invisible()
} 


# ------------------------------------------------------------------------------


assetsLogDensityPlot = 
function(x, estimator = c("hubers", "sample", "both"), 
labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays a pdf plot on logarithmic scale 
    
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
    #   estimator - the type of estimator to fit the mean and variance 
    #       of the density.
    #   doplot - a logical flag, by default TRUE. Should a plot be 
    #       displayed?
    #   labels - a logical flag, by default TRUE. Should a default main  
    #       title and labels addet to the plot?
    #   ... - 
    
    # Details:
    #   Returns a pdf plot on a lin-log scale in comparison to a Gaussian 
    #   density plot Two type of fits are available: a normal density with
    #   fitted sample mean and sample standard deviation, or a normal 
    #   density with Hubers robust mean and standard deviation corfrected
    #   by the bandwidth of the Kernel estimator.
    
    # FUNCTION:
    
    # Settings:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    Units = colnames(x)
    doplot = TRUE
    
    # Select Type:
    estimator = match.arg(estimator)
    
    # Labels:
    if (labels) {
        main = "log PDF"
        xlab = "x"
        ylab = "log PDF"    
    } else {
        main = xlab = ylab = ""
    }
    
    X = x
    
    for (i in 1:ncol(x)) {
        
        # Transform Data:
        x = as.vector(X[, i])
        if (labels) main = Units[i]
                
        # Kernel and Histogram Estimators: 
        Density = density(x)
        Histogram = hist(x, breaks = "FD", plot = FALSE)
        result = list(density = Density, hist = Histogram)
        
        # Plot:
        if (doplot) {  
            # Plot Frame:
            plot(Histogram$mids, log(Histogram$density), type = "n",
                lwd = 5, main = Units[i], xlab = xlab, ylab = ylab,
                xlim = range(Density$x), ylim = log(range(Density$y)),
                col = "red", ...)

            # Plot Density:
            points(Density$x, log(Density$y), pch = 19, col = "darkgrey",
                cex = 0.7)
            
            # Sample Line Fit:
            s = seq(min(Density$x), max(Density$x), length = 1001)
            if (estimator == "sample" || estimator == "both") {
                lines(s, log(dnorm(s, mean(x), sd(x))), col = "red", lwd = 2)
            } 
            
            # Robust Huber Line Fit:
            if (estimator == "hubers" || estimator == "both") {
                h = MASS::hubers(x)
                logDensity = log(dnorm(s, 
                    mean = h[[1]], 
                    sd = sqrt(h[[2]]^2+Density$bw^2)))
                minLogDensity = log(min(Density$y))
                lines(
                    x = s[logDensity > minLogDensity], 
                    y = logDensity[logDensity > minLogDensity], 
                    col = "orange", lwd = 2)
            }
            
            # Plot Histogram:
            points(Histogram$mids, log(Histogram$density), pch = 19,
                col = "steelblue", ...)
              
            # Grid:
            if (labels) grid()
        }    
    }
    
    # Return Value:
    invisible(result)
}


# ------------------------------------------------------------------------------


assetsQQNormPlot =
function(x, col = "steelblue", skipZeros = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays normal qq-plots of individual assets
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix. 
    #   which - an integer value or vector specifying the number(s)
    #       of the assets which are selected to be plotted. 
    
    # FUNCTION:
    
    # Settings:
    n = ncol(x)
    if (length(col) == 1) col = rep(col, times = n)
    
    # Plot:
    for (i in 1:n) {
        X = x[, i]
        if (skipZeros) X = X[X@Data != 0]
        qqnormPlot(X, col = col[i], ...)
    }
        
    # Return Value:
    invisible()
}


################################################################################
# FUNCTION:                   MULTIVARIATE RISK PLOTS:
#  assetsRiskReturnPlot        Displays risk-return giagram of assets 
#  assetsNIGShapeTrianglePlot  Displays NIG Shape Triangle


# assetsQQNIGPlot ...


# ------------------------------------------------------------------------------


assetsRiskReturnPlot <- 
    function(x, col = "steelblue", 
    percentage = FALSE, scale = 252, labels = TRUE, add = TRUE, ...) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays risk-return giagram of assets 
    
    # Arguments:
    #   x - a multivariate 'timeSeries' object
     
    # FUNCTION:
    
    # Compute Return and Risk:
    if (percentage) index = 100 else index = 1
    
    # Compute Return and Risk:
    y = as.matrix(x)
    
    # Sample:
    Risk1 = index*sqrt(scale)* colStdevs(y)
    Return1 = index*scale*colMeans(y)
    
    # Huber(s):
    mu2 = mu3 = s2 = s3 = NULL
    for (i in 1:ncol(y)) {
        MeanSd2 = MASS::huber(y[, i])
        mu2 = c(mu2, MeanSd2$mu)
        s2 = c(s2, MeanSd2$s)
        # MeanSd3 = MASS::hubers(y[, i])
        # mu3 = c(mu3, MeanSd3$mu)
        # s3 = c(s3, MeanSd3$s)
    }
    Risk2 = index*sqrt(scale)*s2
    Return2 = index*scale*mu2
    # Risk3 = index*sqrt(scale)*s3
    # Return3 = index*scale*mu3
    
    # Colors: 
    n = ncol(x)
    if (length(col) == 1) col = rep(col, times = n)

    # Create Graph Frame:
    riskRange = range(c(Risk1, Risk2)) 
    riskRange[1] = 0
    riskRange[2] = riskRange[2] + 0.10*diff(riskRange) 
    returnRange = range(c(Return1, Return2)) 
    returnRange[1] = returnRange[1] - 0.10*diff(returnRange) 
    returnRange[2] = returnRange[2] + 0.10*diff(returnRange) 
    
    if (labels) {
        plot(x = riskRange, y = returnRange, 
            xlab = "Risk", ylab = "Return", type = "n")
    } else {
        plot(x = riskRange, y = returnRange, 
            xlab = "", ylab = "", type = "n")
    }
    mtext("Sample versus Robust Estimates", line = 0.5, cex = 0.7)
        
    # Add all Points:
    colNames = colnames(x)
    for (i in 1:length(Risk1)) {
        points(Risk1[i], Return1[i], col = col[i], cex = 1.5, ...)
        if (add) {
            points(Risk2[i], Return2[i], col = col[i], cex = 1.1, ...)
        }
        text(
            Risk1[i] + diff(riskRange/50), 
            Return1[i] + diff(returnRange/50), 
            colNames[i], adj = 0, col = col[i])
    }
    if (labels) grid(col = "darkgrey")
    
    # Result:
    result = rbind(Risk1, Risk2, Return1, Return2)
    
    # Return Value:
    invisible(result)
}


# ------------------------------------------------------------------------------


assetsNIGShapeTrianglePlot <- 
    function(x, col = "steelblue", ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays NIG Shape Triangle
    
    # Arguments:
    #   x - a multivariate 'timeSeries' object
     
    # FUNCTION:
    
    # Settings:
    n = ncol(x)
    if (length(col) == 1) col = rep(col, times = n)
    colNames = colnames(x)
    
    # Shape Triangle:
    for (i in 1:n) {
        fit = nigFit(100*x[, i], doplot = FALSE)
        nigShapeTriangle(fit, add = as.logical(i-1), col = col[i], ...) 
        
        par = fit@fit$estimate
        alpha = par[1]
        beta = par[2]
        delta = par[3]
        mu = par[4]
        zeta = 1/sqrt(1 + delta * sqrt(alpha^2 - beta^2))
        chi = zeta * (beta/alpha)
        text(chi+0.01, zeta-0.01, colNames[i], adj = 0, col = col[i])
    }
    
    # Return Value:
    invisible()
}


################################################################################
# FUNCTION:                   BIVARIATE HISTOGRAM PLOTS:
#  assetsHistPairsPlot         Displays bivariate Histogram Plot


assetsHistPairsPlot <- 
    function(x, bins = 30, method = c("square", "hex"), ...) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays bivariate Histogram Plot
    
    # FUNCTION:
    
    # Match Arguments:
    method = match.arg(method)
    
    # Check:
    stopifnot(ncol(x) == 2)
    
    # Histogram Plot:
    X = as.vector(x[, 1])
    Y = as.vector(x[, 2])
    if (method == "square") {
        ans = squareBinning(x = X, y= Y, bins = bins)
    } else if (method == "hex") {
        ans = hexBinning(x = X, y = Y, bins = bins)
    }
    
    # Plot:
    plot(ans, ...)
    
    # Return Value:
    invisible(ans)
}


################################################################################

