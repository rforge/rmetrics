
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
# FUNCTION:             TIME SERIES ASSETS PLOTS:
#  assetsPlot            Displays an overview of single assets
#  assetsSeriesPlot      Displays time series of individual assets
#  assetsHistPlot        Displays histograms of individual assets
#  assetsDensityPlot     Displays density plots of individual assets 
#  assetsQQNormPlot      Displays normal qq-plots of individual assets
# FUNCTION:             BIVARIATE ASSETS PLOTS:
#  assetsPairsPlot       Displays pairs of scatterplots of individual assets
#  assetsCorTestPlot     Displays and tests pairwise correlations of assets
# FUNCTION:             BIVARIATE CORRELATION PLOTS:
#  .assetsCorgramPlot
#  .assetsCorEigenPlot
################################################################################


assetsPlot =
function(x, title = NULL, ...)
{    
    # Description:
    #   Displays an overview of single assets
    
    # Arguments:
    #   x a multivariate 'timeSeries' object of financial returns
    
    # FUNCTION:
    
    # Settings:
    nRecords <<- dim(x)[1]
    nAssets <<- dim(x)[2]
    assetNames <<- x@units

    # Graph Frame:
    dots = list(...)
    oma = if("oma" %in% dots) dots$oma else oma = NULL
    if (is.null(oma)) oma = rep(4, 4)
    mar = if("mar" %in% dots) dots$mar else mar = NULL
    if (is.null(mar)) mar = rep(2, 4)
    par(mfrow = c(nAssets, 5), mar = mar, oma = oma, cex = 0.7)    
    
    # Plot:
    fit = list()
    counter = 0
    for (i in 1:nAssets) {
        
        # Settings:
        X = as.vector((x@Data[, i]))
        counter = counter + 1
        assetName = assetNames[i]
        
        # 1. Return Series Plot:
        .retAssetsPlot(X)
        if (counter == 1) {
            title(main = "Returns")
            mtext(title, line = 2, side = 3, adj = 0, cex = 1.25)
        }
        mtext(assetName, line = 2.5, side = 2)
        
        # 2. Cumulated Return Series Plot:
        .retcumulatedAssetsPlot(X)
        if (counter == 1) title(main = "Cumulated")
    
        # 3. Garch(1,1) Volatility Plot:
        fit[[i]] = .volatilityAssetsPlot(X)
        if (counter == 1) title(main = "Volatility")
    
        # 4. Histogram Plot:
        .rethistAssetsPlot(X)
        if (counter == 1) title(main = "Returns")
        
        # 5. Normal Quantile Plot:
        .retqqnormAssetsPlot(X)
        if (counter == 1) title(main = "QQ-Plot") 
        
        # 6. Autocrrelation Plot:
        # acf(X, lag.max = 10, main = "")
        # if (counter == 1) title(main = "ACF")
        
        # 7. Autocrrelation Plot:
        # pacf(X, lag.max = 10, main = "")
        # if (counter == 1) title(main = "PACF")    

        mtext(assetName, side = 4, line = 1.2)
    }
    
    # Return Value:
    invisible(fit)    
}


# ------------------------------------------------------------------------------

.retAssetsPlot = 
function(X) 
{
    # Return Series Plot:
    plot(x = X, type = "h", col = "steelblue", 
        main = "", xlab = "", ylab = "")
    abline(h = 0, col ="grey")
    invisible()
}


# ------------------------------------------------------------------------------


.retcumulatedAssetsPlot =
function(X)
{
    # Cumulated Return Series Plot:
    plot(x = colCumsums(X), type = "l", col = "steelblue",
        xlab = "", ylab = "")
    abline(h = 0, col = "grey")
    invisible()
}
    

# ------------------------------------------------------------------------------


.volatilityAssetsPlot =
function(X)
{
    # Garch(1,1) Volatility Plot:
    fit = garchFit(~garch(1,1), X, trace = FALSE)
    plot(abs(fit@data$x), type = "h", col = "steelblue", ylab = "x", main = "")
    abline(h = 0, col ="grey")
    #for (ci in c(+2, -2)) 
    lines(2 * fit@sigma.t, col = "brown")
    invisible(fit)
}
    

# ------------------------------------------------------------------------------


.rethistAssetsPlot =
function(X)
{        
    # Return Histogram:
    mean = mean(X)
    median = median(X)
    sd = sd(X)
    result = .hist(X, nbins = 15)
    plot(result, col = "steelblue", border = "white", 
        freq = FALSE, main = "")
    box()
    # Add Fit:
    s = seq(min(X), max(X), length = 201)
    lines(s, dnorm(s, mean, sd), lwd = 2, col = "brown")
    abline(v = mean, lwd = 2, col = "orange")
    abline(v = median(X), lwd = 2, col = "darkgreen")
    abline(h = 0, col = "grey") 
    invisible()
}
    

# ------------------------------------------------------------------------------


.retqqnormAssetsPlot =
function(X)
{  
    # 5. Normal Quantile Plot:
    p = (1:nRecords)/(nRecords + 1)
    S = sort((X - mean(X))/sqrt(var(X)))
    z = qnorm(p)
    plot(z, S, pch = 19, col = "steelblue", 
        xlab = "", ylab = "", main = "")
    abline(0, 1, col = "grey")
    s = 1.96 * sqrt(p * (1 - p)/nRecords)
    pl = p - s
    i = pl < 1 & pl > 0
    lower = quantile(S, probs = pl[i])
    lines(z[i], lower, col = "brown")
    pl = p + s
    i = pl < 1 & pl > 0
    upper = quantile(S, probs = pl[i])
    lines(z[i], upper, col = "brown")  
    invisible()
}


# ------------------------------------------------------------------------------


.hist = 
function (x, nbins) 
{   
    nclass = nbins+1
    n = length(x)
    xname <- paste(deparse(substitute(x), 500), collapse = "\n")
    
    breaks <- seq(min(x), max(x), length = nclass)  
    nB <- length(breaks)
    h <- diff(breaks)
    
    counts <- .C("bincount", 
        as.double(x), 
        as.integer(n), 
        as.double(breaks), 
        as.integer(nB), 
        counts = integer(nB - 1), 
        right = FALSE, 
        include = TRUE, 
        naok = FALSE, 
        NAOK = FALSE, 
        DUP = FALSE, 
        PACKAGE = "base")$counts
             
    dens <- counts/(n * h)
    mids <- 0.5 * (breaks[-1] + breaks[-nB])

    r <- structure(list(
        breaks = breaks, 
        counts = counts, 
        intensities = dens, 
        density = dens, 
        mids = mids, 
        xname = xname, 
        equidist = TRUE), 
        class = "histogram")
    
}   

         
# ------------------------------------------------------------------------------


assetsSeriesPlot =
function(x, which = 1:dim(x)[2], ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays histograms of individual assets 
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   which - an integer value or vector specifying the number(s)
    #       of the assets which are selected to be plotted. 
    
    # FUNCTION:

    # Plot:
    for (i in which) {
        seriesPlot(x[, i], ...)
    }
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsHistPlot =
function(x, method = c("cov", "mve", "mcd", "nnve", "shrink", "bagged"), 
which = 1:dim(x)[2], xlim = NULL, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays histograms of individual assets 
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   method - the method to be used. 
    #       "cov",
    #       "mve", minimum volume ellipsoid,
    #       "mcd", minimum covariance determinant method,  
    #       "nnve", 
    #       "shrink", 
    #       "bagged" .
    #   which - an integer value or vector specifying the number(s)
    #       of the assets which are selected to be plotted. 
    
    # FUNCTION:
    
    # Settings:
    method = match.arg(method)
    x = as.matrix(x)
    
    # Robust Estimation:
    covRob = assetsMeanCov(x, method, ...)
    
    # Plot:
    for (i in which) {
        # Classical Histogram:
        histPlot(x[, i], ...)
        
        # Robust Gaussian Fit:
        xlim = range(x[, i])
        u = seq(xlim[1], xlim[2], length = 201)
        v = dnorm(u, mean = covRob$mu[i], sd = sqrt(covRob$Sigma[i, i]))
        abline(v = covRob$mu[i], col = "darkgreen")
        lines(u, v, col = "darkgreen", lwd = 2)
    }
        
    # Return Value:
    invisible()
} 


# ------------------------------------------------------------------------------


assetsDensityPlot =
function(x, method = c("cov", "mve", "mcd", "nnve", "shrink", "bagged"), 
which = 1:dim(x)[2], ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays density plots of individual assets
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix. 
    #   method - the method to be used. 
    #       "cov",
    #       "mve", minimum volume ellipsoid,
    #       "mcd", minimum covariance determinant method,  
    #       "nnve", 
    #       "shrink", 
    #       "bagged" .
    #   which - an integer value or vector specifying the number(s)
    #       of the assets which are selected to be plotted. 
    
    # FUNCTION:
    
    # Settings:
    method = match.arg(method)
    
    # Robust Estimation:
    covRob = assetsMeanCov(x, method, ...)
    
    # Plot:
    for (i in which) {
        densityPlot(x[, i], ...)
                
        # Robust Gaussian Fit:
        xlim = range(x[, i])
        u = seq(xlim[1], xlim[2], length = 201)
        v = dnorm(u, mean = covRob$mu[i], sd = sqrt(covRob$Sigma[i, i]))
        abline(v = covRob$mu[i], col = "darkgreen")
        lines(u, v, col = "darkgreen", lwd = 2)
    }
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsQQNormPlot =
function(x, which = 1:dim(x)[2], ...)
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
    
    # Plot:
    for (i in which) {
        qqnormPlot(x[, i], ...)
    }
        
    # Return Value:
    invisible()
}


################################################################################


assetsPairsPlot =
function(x, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays pairs of scatterplots of individual assets
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   labels - a logical flag. Should default labels be printed? 
    #       Not implemented.
    
    # FUNCTION:
    
    # Settings:
    x = as.matrix(x)
    
    # Plot:
    pairs(x, ...)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsCorTestPlot = 
function(x, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays and tests pairwise correlations of assets
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   labels - a logical flag. Should default labels be printed?
    #       Not implemented.
    
    # FUNCTION:
    
    # Settings:
    x = as.matrix(x)
 
    # Upper Plot Function:
    cortestPanel <-
    function(x, y, cex, col, ...)
    {
        if (missing(col)) col = NULL
        usr = par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r = abs(cor(x, y))
        txt = format(c(r, 0.123456789), digits = 3)[1]
        test = cor.test(x, y)
        Signif = symnum(test$p.value, corr = FALSE, na = FALSE,
            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
            symbols = c("*** ", "** ", "* ", ". ", "  "))
        text(0.5, 0.5, txt, cex = 1, col = NULL, ...)
        text(0.8, 0.8, Signif, cex = 1.5, col = col, ...)
    }
    
    # Lower Plot Function:
    lowessPanel <-  
    function (x, y, ...) 
    {
        points(x, y, ...)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) lines(lowess(x[ok], y[ok]), col = "brown")
    }

    # Plot:
    pairs(x, 
        lower.panel = lowessPanel, 
        upper.panel = cortestPanel, ...)
        
    # Return Value:
    invisible()
}


################################################################################


.assetsCorgramPlot =
function(x, labels = TRUE, method = c("pie", "shade"), ...)
{
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   labels - a logical flag. Should default labels be printed?
    #       Not implemented.
    
    # Example:
    #   assetsCorgramPlot(x=100*as.timeSeries(data(LPP2005REC)))

    # FUNCTION:
    
    # Settings:
    method <<- match.arg(method)
    stopifnot(is.timeSeries(x))
    x = seriesData(x)
    
    # Internal Function:
    .panel.both = function(x, y, ...) {
        if (method == "pie") {
            .panel.pie(x, y, ...)
        } else if (method == "shade") {
            .panel.shade(x, y, ...)
        }
        .panel.pts(x, y, ...) 
    } 
        
    # Plot Corellogram - Pies and Ellipses:    
    .corrgram(x, 
        order = TRUE,
        lower.panel = .panel.both,
        upper.panel = .panel.ellipse, 
        text.panel = .panel.txt, ...)
        
    # Return Value:
    invisible()
}


 

# ------------------------------------------------------------------------------
 
  
.assetsCorEigenPlot =
function(x, method = c("pearson", "kendall", "spearman"))
{
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    
    # Example:
    #   assetsCorEigenPlot(x=100*as.timeSeries(data(LPP2005REC)))
    
    # FUNCTION:
    
    # Settings:
    stopifnot(is.timeSeries(x))
    x = seriesData(x)
    method = match.arg(method)
       
    # Plot:
    x.cor <- cor(x, use = 'pair', method = method)
    x.eig <- eigen(x.cor)$vectors[, 1:2]
    e1 <- x.eig[, 1]
    e2 <- x.eig[, 2]
    plot(e1, e2, col = 'white', xlim = range(e1, e2), ylim = range(e1, e2))
    arrows(0, 0, e1, e2, cex = 0.5, col = "steelblue", length = 0.1)
    text(e1, e2, rownames(x.cor))
    mtext(method, side = 4, adj = 0, cex = 0.7, col = "grey")
    
    # Return Value:
    invisible()
}


################################################################################

