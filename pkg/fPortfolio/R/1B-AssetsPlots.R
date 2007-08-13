
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
#  assetsReturnSurvey          Displays time series survey of assets
#  assetsReturnPlot            Displays time series of individual assets
#  assetsCumulatedPlot         Displays time series of individual assets
#  assetsHistPlot              Displays histograms of individual assets 
#  assetsQQNormPlot            Displays normal qq-plots of individual assets
# FUNCTION:
#  assetsRiskReturnPlot
#  assetsNIGShapeTrianglePlot
# FUNCTION:                   DENSITY BOX PLOTS:
#  assetsBoxPlot               Producess standard box plots
#  assetsBoxPercentilePlot     Producess side-by-side box-percentile plots
# FUNCTION:                   BIVARIATE ASSETS PLOTS:
#  assetsCorgramPlot           Displays correlations between assets
#  assetsPairsPlot             Displays pairs of scatterplots of assets
#  assetsCorTestPlot           Displays and tests pairwise correlations
# FUNCTION:                   BIVARIATE CORRELATION PLOTS:
#  assetsCorEigenPlot          Displays ratio of the largest two eigenvalues
#  assetsTreePlot              Displays minimum spanning tree of assets
#  assetsDendrogramPlot        Displays hierarchical clustering dendrogram
# FUNCTION:                   SPECIAL SEGMENT PLOTS:
#  .assetsStarPlot             Draws star or segment diagrams of a data set
#  .assetsStatsBoxPlot         Produces segment star plot of box plot statistics
################################################################################


################################################################################
# FUNCTION:                   TIME SERIES ASSETS PLOTS:
#  assetsReturnSurvey          Displays time series survey of assets
#  assetsReturnPlot            Displays a return series of individual assets
#  assetsCumulatedPlot         Displays a cumulated return series of  assets
#  assetsHistPlot              Displays a return histogram of individual assets 
#  assetsQQNormPlot            Displays normal qq-plots of individual assets


assetsReturnSurvey = 
function(x, col = "steelblue", ...)
{   
    # Description:
    #   Displays time series survey of assets
    
    # FUNCTION:
    
    # Settings:
    assetNames = colnames(x)
    nAssets = ncol(x)
    if (length(col) == 1) col = rep(col, times = n)
    
    # Survey:
    for (i in 1:nAssets) {
        # Return Plot:
        assetsReturnPlot(x[, i], col[i], labels = FALSE, ...)
        title(main = assetNames[i], xlab = "", ylab = "Returns")
        mtext("Returns", line = 0.5, col = "black", cex = 0.7)
        
        # Cumulated Return Plot:
        assetsCumulatedPlot(x[, i], col[i], ...)
        mtext("Cumulated Returns", line = 0.5, col = "black", cex = 0.7)
        
        # Histogram Plot:
        assetsHistPlot(x[, i], col = col[i], ...)
        title(main = main[i], xlab = "Returns", ylab = "Density")
        mtext("Histogram of Returns", col = "black", line = 0.5, cex = 0.7)
        
        # Quantile-Quantile Plot:
        assetsQQNormPlot(x[, i], scale = FALSE, col = col[i], ...)
        mtext("Normal Quantile-Quantile Plot", col = "black", 
            line = 0.5, cex = 0.7)
    }
    
    # Return Value:
    invisible()
} 


# ------------------------------------------------------------------------------


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
        
    # Return Value:
    invisible()
} 


# ------------------------------------------------------------------------------


assetsQQNormPlot =
function(x, col = "steelblue", skipZeroes = FALSE, ...)
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
        qqnormPlot(X, col = col, ...)
    }
        
    # Return Value:
    invisible()
}


################################################################################


# assetsQQNIGPlot ...


# ------------------------------------------------------------------------------


assetsRiskReturnPlot =
function(x, col = "steelblue", percentage = FALSE, scale = 252, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #
    
    # Arguments:
    #   x - a multivariate 'timeSeries' object
     
    # FUNCTION:
    
    # Compute Return and Risk:
    if (percentage) index = 100 else index = 1
    Risk = index*colStdevs(X)*sqrt(scale)
    Return = index*colMeans(as.matrix(x))*scale 
    n = ncol(x)
    if (length(col) == 1) col = rep(col, times = n)

    # Create Graph Frame:
    riskRange = range(Risk)
    returnRange = range(Return)
    riskRange[1] = 0
    riskRange[2] = riskRange[2] + diff(riskRange)/10
    returnRange[2] = returnRange[2] + diff(returnRange)/10
    plot(x = riskRange, y = returnRange, 
        xlab = "Risk", ylab = "Return", type = "n", ...)
        
    # Add all Points:
    colNames = names(Risk)
    for (i in 1:length(Risk)) {
        points(Risk[i], Return[i], pch = 19, col = col[i], cex = 1.5, ...)
        text(
            Risk[i]+diff(riskRange/50), 
            Return[i]+diff(returnRange/50), 
            colNames[i], adj = 0, col = col[i])
    }
    grid(col = "darkgrey")
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsNIGShapeTrianglePlot =
function(x, col = "steelblue", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #
    
    # Arguments:
    #   x - a multivariate 'timeSeries' object
     
    # FUNCTION:
    
    # Settings:
    n = ncol(x)
    if (length(col) == 1) col = rep(col, times = n)
    
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


.assetsHistPairsPlot = 
function(x, bins = 30, method = c("square", "hex"), ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays bivariate Histogram Plot
    
    # FUNCTION:
    
    # Match Arguments:
    method = match.arg(method)
    
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


assetsBoxPlot =
function(x, col = "bisque", ...) 
{   # A function Implemented by Diethelm Wuertz

    # Description:
    #   Producess standard box plots
    
    # Arguments:
    #   x - a 'timeSeries' object or any other rectangular object
    #       which cab be transformed by the function as.matrix into 
    #       a numeric matrix.
    
    # FUNCTION:
    
    # Settings:
    x = as.matrix(x)
    assetNames = colnames(x)
    
    # Plot:
    ans = boxplot(as.data.frame(x), col = col, ...)
    colnames(ans$stats) = ans$names
    rownames(ans$stats) = c("lower whisker", "lower hinge", "median", 
        "upper hinge", "upper whisker")
    abline(h = 0 , lty = 3)
    
    # Return Value:
    invisible(ans)
}   


# ------------------------------------------------------------------------------


assetsBoxPercentilePlot = 
function(x, col = "bisque", ...) 
{   # A modified copy from Hmisc

    # Description:
    #   Producess side-by-side box-percentile plots
    
    # Details:
    #   Box-percentile plots are similiar to boxplots, except box-percentile 
    #   plots supply more information about the univariate distributions. At 
    #   any height the width of the irregular "box" is proportional to the 
    #   percentile of that height, up to the 50th percentile, and above the 
    #   50th percentile the width is proportional to 100 minus the percentile. 
    #   Thus, the width at any given height is proportional to the percent of 
    #   observations that are more extreme in that direction. As in boxplots, 
    #   the median, 25th and 75th percentiles are marked with line segments 
    #   across the box. [Source: Hmisc]
    
    # Arguments:
    #   x - a 'timeSeries' object or any other rectangular object
    #       which cab be transformed by the function as.matrix into 
    #       a numeric matrix.
    
    # FUNCTION:
    
    # Settings:
    x = as.matrix(x)
    assetNames = colnames(x)
    n = ncol(x)
    all.x = list()
    for (i in 1:n) all.x[[i]] = as.vector(x[, i])
    centers = seq(from = 0, by = 1.2, length = n)
    ymax = max(sapply(all.x, max, na.rm = TRUE))
    ymin = min(sapply(all.x, min, na.rm = TRUE))
    xmax = max(centers) + 0.5
    xmin = -0.5
    
    # Plot:
    plot(c(xmin, xmax), c(ymin, ymax), type = "n",  
        xlab = "", ylab = "", xaxt = "n", ...)
    xpos = NULL
    for (i in 1:n) {
        # plot.values = .bpxAssetsPlot(all.x[[i]], centers[i])
        y = all.x[[i]]
        offset = centers[i]
        y = y[!is.na(y)]
        n = length(y)
        delta = 1/(n + 1)
        prob = seq(delta, 1 - delta, delta)
        quan = sort(y)
        med = median(y)
        q1 = median(y[y < med])
        q3 = median(y[y > med])
        first.half.p = prob[quan <= med]
        second.half.p = 1 - prob[quan > med]
        plotx = c(first.half.p, second.half.p)
        options(warn = -1)
        qx = approx(quan, plotx, xout = q1)$y
        q1.x = c(-qx, qx) + offset
        qx = approx(quan, plotx, xout = q3)$y
        options(warn = 0)
        q3.x = c(-qx, qx) + offset
        q1.y = c(q1, q1)
        q3.y = c(q3, q3)
        med.x = c(-max(first.half.p), max(first.half.p)) + offset
        med.y = c(med, med)
        plot.values = list(x1 = (-plotx) + offset, y1 = quan, x2 = plotx + 
            offset, y2 = quan, q1.y = q1.y, q1.x = q1.x, q3.y = q3.y, 
            q3.x = q3.x, med.y = med.y, med.x = med.x)      
        # Continue:
        xpos = c(xpos, mean(plot.values$med.x))
        x.p = c(plot.values$x1, plot.values$x2)
        y.p = c(plot.values$y1, plot.values$y2)
        polygon(x.p, y.p, col = col, border = "grey")
        lines(plot.values$x1, plot.values$y1)
        lines(plot.values$x2, plot.values$y2)
        lines(plot.values$q1.x, plot.values$q1.y)
        lines(plot.values$q3.x, plot.values$q3.y)
        lines(plot.values$med.x, plot.values$med.y) 
    }
    axis(side = 1, at = xpos, labels = assetNames)
    abline(h = 0, lty = 3, col = "black")
   
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.bpxAssetsPlot = 
function (y, offset) 
{   # A copy from Hmisc

    # Description:
    #   Internal function called by assetsBoxPercentilePlot()
    
    # FUNCTION:
    
    # bpx:
    y = y[!is.na(y)]
    n = length(y)
    delta = 1/(n + 1)
    prob = seq(delta, 1 - delta, delta)
    quan = sort(y)
    med = median(y)
    q1 = median(y[y < med])
    q3 = median(y[y > med])
    first.half.p = prob[quan <= med]
    second.half.p = 1 - prob[quan > med]
    plotx = c(first.half.p, second.half.p)
    options(warn = -1)
    qx = approx(quan, plotx, xout = q1)$y
    q1.x = c(-qx, qx) + offset
    qx = approx(quan, plotx, xout = q3)$y
    options(warn = 0)
    q3.x = c(-qx, qx) + offset
    q1.y = c(q1, q1)
    q3.y = c(q3, q3)
    med.x = c(-max(first.half.p), max(first.half.p)) + offset
    med.y = c(med, med)
    ans = list(x1 = (-plotx) + offset, y1 = quan, x2 = plotx + 
        offset, y2 = quan, q1.y = q1.y, q1.x = q1.x, q3.y = q3.y, 
        q3.x = q3.x, med.y = med.y, med.x = med.x)
    
    # Return Value:
    ans
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
    lowessPanel =  
    function (x, y, ...) 
    {
        points(x, y, ...)
        ok = is.finite(x) & is.finite(y)
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


assetsCorgramPlot =
function(x, labels = TRUE, method = c("pie", "shade", "hist"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays correlations between assets
    
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
    .panel.lower = function(x, y, ...) 
    {
        if (method[1] == "pie") {
            .panel.pie(x, y, ...)
            .panel.pts(x, y, ...) 
        } else if (method[1] == "shade") {
            .panel.shade(x, y, ...)
            .panel.pts(x, y, ...) 
        } else if (method[1] == "hist") {
            .panel.shade(x, y, ...)
            .panel.hist(x, y, ...)
        }
    } 
    .panel.upper = function(x, y, ...) 
    {
        .panel.ellipse(x, y, ...)
    }
        
    # Plot Corellogram - Pies and Ellipses:    
    .corrgram(x, 
        order = TRUE,
        lower.panel = .panel.lower,
        upper.panel = .panel.upper, 
        text.panel = .panel.txt, ...)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsPairCopulaPlot =
function(x, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays correlations between assets
    
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
    stopifnot(is.timeSeries(x))
    x = seriesData(x)
   
    # Plot Corellogram - Pies and Ellipses:    
    .corrgram(x, 
        order = TRUE,
        lower.panel = .panel.copula,
        upper.panel = .panel.hist, 
        text.panel = .panel.txt, ...)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------
 
  
assetsCorEigenPlot =
function(x, method = c("pearson", "kendall", "spearman"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays ratio of the largest two eigenvalues
    
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
    x.cor = cor(x, use = 'pair', method = method)
    x.eig = eigen(x.cor)$vectors[, 1:2]
    e1 = x.eig[, 1]
    e2 = x.eig[, 2]
    plot(e1, e2, col = 'white', 
        xlim = range(e1, e2), ylim = range(e1, e2), ...)
    abline(h = 0, lty = 3, col = "grey")
    abline(v = 0, lty = 3, col = "grey")
    arrows(0, 0, e1, e2, cex = 0.5, col = "steelblue", length = 0.1)
    text(e1, e2, rownames(x.cor))
    mtext(method, side = 4, adj = 0, cex = 0.7, col = "grey")
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsTreePlot = 
function(x, method = "euclidian", seed = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays minimum spanning tree of assets
    
    # FUNCTION:
    
    # Settings:
    Main = substitute(x)
    
    # Compute Distance Matrix:
    Order = NULL
    if (class(x) == "dist") {
        DIST = x
    } else {
        # Rank Seed:
        x = seriesData(x)
        if (is.null(seed)) {
            Order = sample(1:ncol(x))
            x = x[, Order]
        }
        DIST = dist(t(x), method[1])
    }
    method = attr(DIST, "method")
       
    # Compute Minimum Spanning Tree"
    MST = .mst(DIST)
      
    # Plot Tree:
    .mstPlot(MST, ".nsca", main = Main)
    mtext(paste("Distance Method:", method), 
        side = 4, line = 0.1, adj = 0, col = "darkgrey", cex = 0.7)
    
    # Return Value:
    invisible(list(mst = MST, dist = DIST, order = Order))
}


# ------------------------------------------------------------------------------


assetsDendrogramPlot =
function(x, method = c(dist = "euclidian", clust = "complete"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays hierarchical clustering dendrogram
    
    # FUNCTION:
    
    # Compute Distance Matrix:
    if (class(x) == "dist") {
        DIST = x
    } else {
        X = t(seriesData(x))
        DIST = dist(X, method[1])
    }

    # Hierarchical Clustering:
    ans = hclust(DIST, method = method[2]) 
    
    # Plot Dendrogram:
    # main = substitute(x)
    plot(ans, xlab = "", main = "", sub = "")
    mtext(paste(
        "Distance Method:", method[1], " | ",
        "Clustering Method:", method[2]),
        side = 4, line = 0.1, adj = 0, col = "darkgrey", cex = 0.7)  
    box()
    
    # Return Value:
    invisible(list(dist = DIST, hclust = ans))
}


################################################################################


.assetsStarPlot =
function(x, method = c("segments", "stars"), keyOffset = c(0, 0), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Draws segment or star diagrams of a multivariate data set. 
    
    # Arguments
    #   x - a numeric feature matrix of assets. Each column represents
    #       an individual asset.
    
    # Example:
    #   x = as.timeSeries(data(LPP2005REC))          
    #   X = basicStats(x)[-(1:2), 1:6]   
    #   assetsStarPlot(X, main = "Basic Statistics", keyOffset = -0.5)
    
    # FUNCTION:
    
    # Settings:
    method = match.arg(method)
    if (method == "segments") draw.segments = TRUE else draw.segments = FALSE
    
    # Compute Locations:
    xCol = ncol(x)
    yCol = nrow(x)
    NY = NX = ceiling(sqrt(xCol))
    
    if (NX*NY == xCol) NY = NY + 1
    
    
    loc = NULL
    for (nx in 1:NY)
        for (ny in 1:NX)
            loc = rbind(loc, c(nx, ny))
    loc = loc[1:xCol, ]   
    loc[, 2] = NY + 1 - loc[, 2]
    
    # Stars:
    palette(rainbow(12, s = 0.6, v = 0.75))
    ans = stars(t(x), mar = c(4, 2.8, 2.8, 4),
        locations = loc,
        len = 0.4, 
        xlim = c(1, NX+0.5), 
        ylim = c(0, NY+1), 
        key.loc = c(NX + 1, 1) + keyOffset, 
        draw.segments = draw.segments, ... )
    box()
    
    # Return Value:
    invisible(ans)
}


################################################################################


.assetsStatsBoxPlot = 
function(x, 
    oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), 
    main = "Assets Statistics", 
    title = "Assets", 
    titlePosition = c(3, 3.65), 
    description = "Box Plot Statistics", 
    descriptionPosition = c(3, 3.50))
{   # A function Implemented by Diethelm Wuertz

    # Description:
    #   Displays a segment plot of box plot statistics
    
    # Note:
    #    The Default Settings are made for a portfolio with
    #       7 to 9 assets.
    
    # FUNCTION:
    
    # Plot:
    par(mfrow = c(1, 1), oma = oma, mar = mar) 
    bp = assetsBoxPlot(x, doplot = FALSE)
    .assetsStarPlot(bp$stats, keyOffset = keyOffset)                          
    text(titlePosition[1], titlePosition[2], adj = 0, 
        title, cex = 1.25)
    text(descriptionPosition[1], descriptionPosition[2], adj = 0, 
        description, cex = 1.1)
    title(main = main) 
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.assetsBasicStatsPlot = 
function(x, 
    oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), 
    main = "Assets Statistics", 
    title = "Assets", 
    titlePosition = c(3, 3.65), 
    description = "Basic Returns Statistics", 
    descriptionPosition = c(3, 3.50))
{   # A function Implemented by Diethelm Wuertz

    # Description:
    #   Displays a segment plot of basic return statistics
    
    # Note:
    #    The Default Settings are made for a portfolio with
    #       7 to 9 assets.
    
    # FUNCTION:
    
    # Plot:
    par(mfrow = c(1, 1), oma = oma, mar = mar) 
    X = basicStats(x)[-(1:2), ] 
    .assetsStarPlot(X, keyOffset = keyOffset)                          
    text(titlePosition[1], titlePosition[2], adj = 0, 
        title, cex = 1.25)
    text(descriptionPosition[1], descriptionPosition[2], adj = 0, 
        description, cex = 1.1)
    title(main = main) 
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.assetsMomentsPlot = 
function(x, 
    oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), 
    main = "Assets Statistics", 
    title = "Assets", 
    titlePosition = c(3, 3.65), 
    description = "Moments Statistics", 
    descriptionPosition = c(3, 3.50))
{   # A function Implemented by Diethelm Wuertz

    # Description:
    #   Displays a segment plot of distribution moments
    
    # Note:
    #    The Default Settings are made for a portfolio with
    #       7 to 9 assets.
    
    # FUNCTION:
    
    # Plot:
    par(mfrow = c(1, 1), oma = oma, mar = mar) 
    param = NULL
    for (i in 1:dim(x)[2]) {
        X = as.vector(seriesData(x[, i]))
        fit = c(mean = mean(X), stdev = sd(X), 
            skewness = skewness(X), kurtosis = kurtosis(X))
        param = cbind(param, fit)
    }
    colnames(param) = colnames(x)
    .assetsStarPlot(param, keyOffset = keyOffset)
    text(titlePosition[1], titlePosition[2], adj = 0, 
        title, cex = 1.25)
    text(descriptionPosition[1], descriptionPosition[2], adj = 0, 
        description, cex = 1.1)
    title(main = main) 
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.assetsNIGFitPlot =
function(x,
    oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), 
    main = "Assets Statistics", 
    title = "Assets", 
    titlePosition = c(3, 3.65), 
    description = "NIG  Parameters", 
    descriptionPosition = c(3, 3.50))
{   # A function Implemented by Diethelm Wuertz

    # Description:
    #   Displays a segment plot NIG parameter estimates
    
    # Note:
    #    The Default Settings are made for a portfolio with
    #       7 to 9 assets.
    
    # FUNCTION:
    
    # Plot: 
    param = NULL
    for (i in 1:dim(x)[2]) {
        fit = nigFit(x[, i], doplot = FALSE)
        param = cbind(param, fit@fit$estimate)
    }
    par(mfrow = c(1, 1), oma = oma, mar = mar)
    colnames(param) = colnames(x)
    rownames(param) = c("alpha", "beta", "delta", "mu")
    .assetsStarPlot(param, keyOffset = keyOffset)
    text(titlePosition[1], titlePosition[2], adj = 0, 
        title, cex = 1.25)
    text(descriptionPosition[1], descriptionPosition[2], adj = 0, 
        description, cex = 1.1)
        title(main = main) 
    
    # Return Value:
    invisible()
} 


################################################################################


.hist = 
function (x, nbins) 
{   
    nclass = nbins+1
    n = length(x)
    xname = paste(deparse(substitute(x), 500), collapse = "\n")
    
    breaks = seq(min(x), max(x), length = nclass)  
    nB = length(breaks)
    h = diff(breaks)
    
    counts = .C("bincount", 
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
             
    dens = counts/(n * h)
    mids = 0.5 * (breaks[-1] + breaks[-nB])

    r = structure(list(
        breaks = breaks, 
        counts = counts, 
        intensities = dens, 
        density = dens, 
        mids = mids, 
        xname = xname, 
        equidist = TRUE), 
        class = "histogram")
    
}   


################################################################################

