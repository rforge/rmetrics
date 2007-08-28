
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received A copy of the GNU Library General 
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
# FUNCTION:               BASIC STATISTICS:
#  basicStats              Returns a basic statistics summary
# FUNCTION:               TAILORED PLOT FUNCTIONS:     
#  seriesPlot              Returns a tailored return series plot
#  cumulatedPlot           Returns a tailored cumulated return series plot
#  histPlot                Returns a tailored histogram plot
#  densityPlot             Returns a tailored kernel density estimate plot
#  qqnormPlot              Returns a tailored normal quantile-quantile plot
#  qqnigPlot               Returns a tailored NIG quantile-quantile plot
# FUNCTION:               BOX PLOTS:
#  boxPlot                 Produces a side-by-side standard box plot
#  boxPercentilePlot       Produces a side-by-side box-percentile plot
# FUNCTION:               GRAPHICAL USER INTERFACE:
#  returnSeriesGUI         Opens a GUI for return series plots
################################################################################


################################################################################
# FUNCTION:               BASIC STATISTICS:
#  basicStats              Returns a basic statistics summary


basicStats = 
function(x, ci = 0.95) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Calculates Basic Statistics
    
    # Arguments:
    #   x - an object which can be transformed by the function
    #       as.matrix() into an object of class matrix. 
    #   ci - a numeric value setting the confidence interval.
    
    # Value:
    #   a two-column data frame, where the first column takes the 
    #   value of the statistics, and the second its name, e.g.
    #   "nobs", "NAs",  "Minimum", "Maximum", "1. Quartile",  
    #   "3. Quartile",  "Mean", "Median", "Sum",  "SE Mean", 
    #   "LCL Mean", "UCL Mean", "Variance", "Stdev", "Skewness", 
    #   "Kurtosis")

    # FUNCTION:
    
    # Univariate/Multivariate:
    y = as.matrix(x)
    
    
    # Handle Column Names:
    if (is.null(colnames(y))) {
        Dim = dim(y)[2]
        if (Dim == 1) {
            colnames(y) = paste(substitute(x), collapse = ".")
        } else if (Dim > 1) {
            colnames(y) = 
                paste(paste(substitute(x), collapse = ""), 1:Dim, sep = "")
        }
    }
    
    # Internal Function - CL Levels:    
    cl.vals = function(x, ci) {
        x = x[!is.na(x)]
        n = length(x)
        if(n <= 1) return(c(NA, NA))
        se.mean = sqrt(var(x)/n)
        t.val = qt((1 - ci)/2, n - 1)
        mn = mean(x)
        lcl = mn + se.mean * t.val
        ucl = mn - se.mean * t.val
        c(lcl, ucl)
    }        
    
    # Basic Statistics:
    nColumns = dim(y)[2]
    ans = NULL
    for (i in 1:nColumns) {
        X = y[, i]     
        # Observations:
        X.length = length(X)
        X = X[!is.na(X)]
        X.na = X.length - length(X)
        # Basic Statistics:
        z = c(
            X.length, X.na, min(X), max(X),
            as.numeric(quantile(X, prob = 0.25, na.rm = TRUE)), 
            as.numeric(quantile(X, prob = 0.75, na.rm = TRUE)), 
            mean(X), median(X), sum(X), sqrt(var(X)/length(X)), 
            cl.vals(X, ci)[1], cl.vals(X, ci)[2], var(X), 
            sqrt(var(X)), skewness(X), kurtosis(X) )    
        # Row Names:
        znames = c(
            "nobs", "NAs",  "Minimum", "Maximum", 
            "1. Quartile",  "3. Quartile",  "Mean", "Median", 
            "Sum",  "SE Mean", "LCL Mean", "UCL Mean", 
            "Variance", "Stdev", "Skewness", "Kurtosis")   
        # Output as data.frame
        result = matrix(z, ncol = 1)
        row.names(result) = znames    
        ans = cbind(ans, result) 
    }
    
    # Column Names:
    colnames(ans) = colnames(y)
    
    # Return Value:
    data.frame(round(ans, digits = 6))
}


################################################################################    
# FUNCTION:               TAILORED PLOT FUNCTIONS:
#  seriesPlot              Returns a tailored return series plot
#  cumulatedPlot           Returns a tailored cumulated return series plot
#  histPlot                Returns a tailored histogram plot
#  densityPlot             Returns a tailored kernel density estimate plot
#  qqnormPlot              Returns a tailored normal quantile-quantile plot
#  qqnigPlot               Returns a tailored NIG quantile-quantile plot


seriesPlot = 
function(x, labels = TRUE, type = "l", col = "steelblue", ylab = "Returns",
rug = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns time series graphs in a common plot
  
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
    
    # Example:
    # tS=timeSeries(cbind(rnorm(12),rt(12,4)),timeCalendar(),units=c("N","T"))
    # seriesPlot(tS)
    
    # FUNCTION:

    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    Units = x@units
    DIM = dim(x@Data)[2]
    if (length(col) == 1) col = rep(col, times = DIM)
    
    # Series Plots:
    for (i in 1:DIM) {
        X = x[, i]
        if (labels) {
            plot(x = X, type = type, col = col[i], 
                main = Units[i], ylab = ylab, xlab = "Time", ...)
            # grid()
        } else {
            plot(x = X, type = type, col = col[i], 
                main = "", ylab = "", xlab = "", ...)   
        }
        abline(h = 0, col = "grey")
        if (rug) rug(as.vector(X), ticksize = 0.01, side = 4, quiet = TRUE)
            
    }
         
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


cumulatedPlot = 
function(x, index = 100, labels = TRUE, type = "l", col = "steelblue", 
ylab = "Index", rug = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns time series graphs in a common plot
  
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
    
    # Example:
    # tS=timeSeries(cbind(rnorm(12),rt(12,4)),timeCalendar(),units=c("N","T"))
    # seriesPlot(tS)
    
    # FUNCTION:

    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    x = index * exp(colCumsums(x))
    Units = x@units
    DIM = dim(x@Data)[2]
    if (length(col) == 1) col = rep(col, times = DIM)
    
    # Series Plots:
    for (i in 1:DIM) {
        X = x[, i]
        if (labels) {
            plot(x = X, type = type, col = col[i], 
                main = Units[i], ylab = ylab, xlab = "Time", ...)
            # grid()
        } else {
            plot(x = X, type = type, col = col[i], 
                main = "", ylab = "", xlab = "", ...)   
        }
        abline(h = 0, col = "grey")
        if (rug) rug(as.vector(X), ticksize = 0.01, side = 4, quiet = TRUE)
            
    }
         
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


histPlot = 
function(x, labels = TRUE, col = "steelblue", add.fit = TRUE, rug = TRUE, 
skipZeros = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a probability histogram plot for each column of a 
    #   timeSeries object
    
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
   
    # FUNCTION:
    
    # Settings:
    xlim = NULL
    
    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    Units = x@units
    DIM = dim(x@Data)[2]
    if (length(col) == 1) col = rep(col, times = DIM)
      
    # Construct output list:
    ans = paste( " hist", 1:DIM, " = NULL", sep = "", collapse = ",")
    ans = paste( "list(",  ans, ")" )
    ans = eval(parse(text = ans))
    
    # Histogram Plots:
    for (i in 1:DIM) {
        
        # Histogram:
        Values = as.vector(x@Data[, i])
        if (skipZeros) Values = Values[Values != 0]
        mean = mean(Values)
        median = median(Values)
        sd = sd(Values)
                    
        # Plot:
        if (labels) {
            xlim = c(qnorm(0.001, mean, sd), qnorm(0.999, mean, sd)) 
            result = hist(x = Values, col = col[i], 
            border = "white", breaks = "FD", main = Units[i], 
            xlim = xlim, probability = TRUE, ...) 
            box()
        } else {
            result = hist(x = Values, probability = TRUE, main = "", 
                xlab = "", ylab = "", col = col[i], ...)
        }
             
        # Add Fit:  
        if (add.fit) {
            s = seq(xlim[1], xlim[2], length = 201)
            lines(s, dnorm(s, mean, sd), lwd = 2, col = "brown")
        }
        ans[[i]] = result  
        
        # Add Mean/Median:
        abline(v = mean, lwd = 2, col = "orange")
        abline(v = median(Values), lwd = 2, col = "darkgreen")
        Text = paste("Median:", round(median, 2), "| Mean:", signif(mean, 3))
        if (skipZeros) Text = paste(Text, "| Zeros skipped")
        mtext(Text, side = 4, adj = 0, col = "darkgrey", cex = 0.7)
  
        # Add Zero Line:
        abline(h = 0, col = "grey")
    
        # Add Rug Plot:
        if(rug) rug(Values, ticksize = 0.01, quiet = TRUE)
    }
    
    # Return Value:
    invisible()
}  


# ------------------------------------------------------------------------------


densityPlot = 
function(x, labels = TRUE, col = "steelblue", add.fit = TRUE, rug = TRUE, 
skipZeros = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns density plots for each column of a timeSeries object
    
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.

    # FUNCTION:
    
    # Transform 'timeSeries':
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    units = x@units
    DIM = dim(x@Data)[2]
    xlim = NULL
    if (length(col) == 1) col = rep(col, times = DIM)
      
    # Construct output list:
    ans = paste( " hist", 1:DIM, " = NULL", sep = "", collapse = ",")
    ans = paste( "list(",  ans, ")" )
    ans = eval(parse(text = ans))
    
    # Histogram Plots:
    for (i in 1:DIM) {
        
        # Density:
        Values = as.vector(x@Data[, i])
        if (skipZeros) Values = Values[Values != 0]
        mean = mean(Values)
        median = median(Values)
        sd = sd(Values)
        if (is.null(xlim)) 
            xlim = c(qnorm(0.001, mean, sd), qnorm(0.999, mean, sd)) 
        Density = density(Values, ...)
        plot(x = Density, xlim = xlim, col = col[i], type = "l", 
            lwd = 2, main = Units[i], ...)  
        ans[[i]] = Density  
        
        # Grid:
        grid()
        
        # Add Fit:
        if (add.fit) {
            s = seq(xlim[1], xlim[2], length = 201)
            lines(s, dnorm(s, mean, sd), lwd = 2, col = "brown")
        }
        
        # Add Mean/Median:
        abline(v = mean, lwd = 2, col = "orange")
        abline(v = median(Values), lwd = 2, col = "darkgreen")
        Text = paste(
            "Median:", round(median, 2), 
            "| Mean:", round(mean, 2),
            "| Bandwidth:", round(Density$bw, 3) )
        if (skipZeros) Text = paste(Text, "| Zeros skipped")
        mtext(Text, side = 4, adj = 0, col = "darkgrey", cex = 0.7)
  
        # Add Zero Line:
        abline(h = 0, col = "grey")
        
        # Add Rug Plot:
        if(rug) rug(Values, ticksize = 0.01, quiet = TRUE)     
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


qqnormPlot = 
function(x, labels = TRUE, col = "steelblue", rug = TRUE, scale = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Example of a Normal quantile plot of data x to provide a visual
    #   assessment of its conformity with a normal (data is standardised    
    #   first).

    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
    
    # Details:
    #   The ordered data values are posterior point estimates of the 
    #   underlying quantile function. So, if you plot the ordered data 
    #   values (y-axis) against the exact theoretical quantiles (x-axis),   
    #   you get a scatter that should be close to a straight line if the 
    #   data look like a random sample from the theoretical distribution. 
    #   This function chooses the normal as the theory, to provide a 
    #   graphical/visual assessment of how normal the data appear.
    #   To help with assessing the relevance of sampling variability on 
    #   just "how close" to the normal the data appears, we add (very) 
    #   approximate posterior 95% intervals for the uncertain quantile 
    #   function at each point (Based on approximate theory) .

    # Author:
    #   Based on code written by Mike West, mw@stat.duke.edu 
    
    # Note:
    #   Source from
    #   http://www.stat.duke.edu/courses/Fall99/sta290/Notes/

    # Example:
    #   x = rnorm(100); qqnormPlot(x); qqnormPlot(x, labels = FALSE)
    
    # FUNCTION:
    
    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    DIM = dim(x@Data)[2]
    Main = x@units
    if (length(col) == 1) col = rep(col, times = DIM)
    
    # QQ Plots:
    X = x
    for (i in 1:DIM) {

        # Settings:
        mydata = as.vector(X[, i])
        n = length(mydata) 
        p = (1:n)/(n+1)
        if (scale) x = (mydata-mean(mydata))/sqrt(var(mydata)) else x = mydata
        x = sort(x)
        if (scale) z = z = qnorm(p) else z = qnorm(p, mean(x), sd(x))
     
        # Plot:
        if (labels) {
            main = Main[i]
            xlab = "Normal Quantiles"
            ylab = paste(Main[i], "Ordered Data")
            plot(z, x, pch = 19, col = col[i], 
                xlab = xlab, ylab = ylab, main = main, ...)
                Text = "Confidence Intervals: 95%"
            mtext(Text, side = 4, adj = 0, col = "darkgrey", cex = 0.7)
            grid()  
        } else {
            plot(z, x, col = col[i], ...)
        }
        abline(0, 1, col = "grey")
        if(rug) rug(z, ticksize = 0.01, side = 3, quiet = TRUE)
        if(rug) rug(x, ticksize = 0.01, side = 4, quiet = TRUE)
      
        # 95% Intervals:
        s = 1.96*sqrt(p*(1-p)/n)
        pl = p-s
        i = pl<1&pl>0
        lower = quantile(x, probs = pl[i])
        lines(z[i], lower, col = "brown")
        pl = p+s
        i = pl < 1 & pl > 0
        upper = quantile(x, probs = pl[i])
        lines(z[i], upper, col = "brown")
        abline (h = mean(x), col = "grey")
        abline(v = mean(x), col = "grey")
    }
    
    # Return Value:
    invisible()  
}


# ------------------------------------------------------------------------------


qqnigPlot = 
function (x, labels = TRUE, col = "steelblue", rug = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
    
    # Example:
    #   qqnigPlot(rnig(100))
    
    # FUNCTION:
       
    # Settings:
    if (!is.timeSeries(x)) {
        x = as.timeSeries(x)
        stopifnot(isUnivariate(x)) 
        Main = x@units
    }
    x = as.vector(x)
    
    # Fit:
    fit = nigFit(x, doplot = FALSE)
    par = fit@fit$estimate
    names(par) = c("alpha", "beta", "delta", "mu")
    
    # Plot:
    x = qnig(ppoints(x), par[1], par[2], par[3], par[4])
    z = sort(x)
    if (labels) {
        main = "NIG QQ Plot"
        xlab = "Theoretical Quantiles"
        ylab = "Sample Quantiles"
        plot(z, x, main = main, xlab = xlab, ylab = ylab, 
            pch = 19, col = "steelblue")
        grid()  
        rpar = signif(par, 3)
        text = paste(
            "alpha =", rpar[1], 
            "| beta =", rpar[2], 
            "| delta =", rpar[3], 
            "| mu =", rpar[4])  
        mtext(text, side = 4, adj = 0, col = "grey", cex = 0.7)
    } else {
        plot(z, x, ...)
    }
    abline(lsfit(z, x))
    if(rug) rug(z, ticksize = 0.01, side = 3, quiet = TRUE)
    if(rug) rug(x, ticksize = 0.01, side = 4, quiet = TRUE)
    
    # Result:
    .DEBUG <<-fit
    ans = list(x = z, y = x)
    attr(ans, "control")<-par
    
    # Return Value:
    invisible(ans)
}


################################################################################
# FUNCTION:               BOX PLOTS:
#  boxPlot                 Produces a side-by-side standard box plot
#  boxPercentilePlot       Produces a side-by-side box-percentile plot


boxPlot =
function(x, col = "steelblue", ...) 
{   # A function Implemented by Diethelm Wuertz

    # Description:
    #   Produces a standard box plot
    
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.matrix()' into an object of class 'matrix'.
    
    # Optional Arguments:
    #   las, oma - allows to change style of X labels and creates 
    #       required space below plot. 
    #       Try: e.g. las = 3, and oma = c(9, 0, 0, 0)
    
    
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


boxPercentilePlot = 
function(x, col = "steelblue", ...) 
{   # A modified copy from Hmisc

    # Description:
    #   Produces a side-by-side box-percentile plot
    
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
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.matrix()' into an object of class 'matrix'.
    
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
    if (length(col) == 1) col = rep(col, times = n)
    plot(c(xmin, xmax), c(ymin, ymax), type = "n",  
        xlab = "", ylab = "", xaxt = "n")
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
        polygon(x.p, y.p, col = col[i], border = "grey", ...)
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


################################################################################
# FUNCTION:               GRAPHICAL USER INTERFACE:
#  returnSeriesGUI         Opens a GUI for return series plots


returnSeriesGUI = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Descriptions:
    #   Opens a GUI for return series plots
    
    # Arguments:
    #   x - an uni- or multivariate timeSeries object
    
    # FUNCTION:
    
    # Check:
    stopifnot(class(x) == "timeSeries")
    
    # Settings:
    N = ceiling(sqrt(ncol(x)))
    mfrow = c(N, N)
    
    returnSeriesRefreshCode = 
    function(...)
    {
        # Settings:
        selectedAsset  = .tdSliderMenu(no = 1)
        type = as.integer(.tdSliderMenu(obj.name = "returnSeriesType"))
        Unit = colnames(x)
        
        # Print Basic Return Statistics:
        if (type == 1) {
            if (selectedAsset == 0) {
                print(basicStats(x))
            } else {
                print(basicStats(x[, selectedAsset]))
            }
        }
        
        # Return Series Plot:
        if (type == 2) {
            if (selectedAsset == 0) {
                par(mfrow = mfrow)
                seriesPlot(x)
            } else {
                par(mfrow = c(1, 1))
                seriesPlot(x[, selectedAsset])
            }
        }
        
        # Cumulate Return Series Plot
        if (type == 3) {
            if (selectedAsset == 0) {
                par(mfrow = mfrow)
                seriesPlot(100*exp(colCumsums(x)))   
            } else {
                par(mfrow = c(1, 1))
                seriesPlot(100*exp(colCumsums(x[, selectedAsset])))   
                abline(h = 100, col = "grey")
            }
        }    
        
        # Histogram Plot:
        if (type == 4) {
            if (selectedAsset == 0) {
                par(mfrow = mfrow)
                histPlot(x, skipZeros = TRUE)
            } else {
                par(mfrow = c(1, 1))
                histPlot(x[, selectedAsset], skipZeros = TRUE)
            } 
        }  
        
        # Density Plot:
        if (type == 5) {
            if (selectedAsset == 0) {
                par(mfrow = mfrow)
                densityPlot(x)
            } else {
                par(mfrow = c(1, 1))
                densityPlot(x[, selectedAsset])
            }
        }
        
        # Normal QQ Plot:
        if (type == 6) {
            if (selectedAsset == 0) {
                par(mfrow = mfrow)
                qqnormPlot(x)
            } else {
                par(mfrow = c(1, 1))
                qqnormPlot(x[, selectedAsset]) 
            }   
        }    
        
    } 

    nAssets = dim(x)[2]
    
    .tdSliderMenu(
        returnSeriesRefreshCode,
        
        names       = c("Selected Asset"),
        minima      = c(      0),
        maxima      = c(      nAssets),
        resolutions = c(      1),
        starts      = c(      0),
        
        but.functions = list(
            function(...){
                .tdSliderMenu(obj.name = "returnSeriesType", obj.value = "1")
                returnSeriesRefreshCode()},
            function(...){
                .tdSliderMenu(obj.name = "returnSeriesType", obj.value = "2")
                returnSeriesRefreshCode()}, 
            function(...){
                .tdSliderMenu(obj.name = "returnSeriesType", obj.value = "3")
                returnSeriesRefreshCode()},
            function(...){
                .tdSliderMenu(obj.name = "returnSeriesType", obj.value = "4")
                returnSeriesRefreshCode()},  
            function(...){
                .tdSliderMenu(obj.name = "returnSeriesType", obj.value = "5")
                returnSeriesRefreshCode()},
            function(...){
                .tdSliderMenu(obj.name = "returnSeriesType", obj.value = "6")
                returnSeriesRefreshCode()}
        ),
        
        but.names = c(
            "1 Basic Return Statistics",
            "2 Return Series Plot", 
            "3 Cumulated Return Series Plot", 
            "4 Histogram Plot",
            "5 Density Plot",
            "6 Normal Quantile-Quantile Plot"),
     
        title = "Return Series GUI"
        )        
            
   .tdSliderMenu(obj.name = "type", obj.value = "1", no = 1)
   
   # Return Value()
   invisible()
}


################################################################################

