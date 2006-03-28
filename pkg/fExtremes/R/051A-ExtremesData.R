
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
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION          EXPLORATIVE DATA ANALYSIS:
#  emdPlot           Plots empirical distribution function
#  qqPlot            Creates a quantile-quantile plot
#  qqbayesPlot       Creates qq-Plot with 95 percent intervals
#  qPlot             Creates exploratory QQ plot for EV analysis
#  mePlot            Creates a sample mean excess plot
#   mxfPlot           Plots the mean excess function
#   mrlPlot           Returns a mean residual life plot with confidence levels
#  recordsPlot       Plots records development
#   ssrecordsPlot     Plots records development of data subsamples
#  msratioPlot       Plots ratio of maximums and sums
#  sllnPlot          Verifies Kolmogorov's Strong Law of large numbers
#  lilPlot           Verifies Hartman-Wintner's Law of the iterated logarithm
#  xacfPlot          Plots autocorrelations of exceedences
################################################################################


################################################################################
# FUNCTION:         PLOT UTILITIES:
#  interactivePlot   Plots several graphs interactively
#  gridVector        Creates from two vectors rectangular grid points
################################################################################


################################################################################
# FUNCTION          DATA PREPROCESSING:
#  blockMaxima       Returns block maxima from a time series
#  findThreshold     Upper threshold for a given number of extremes 
#  pointProcess      Returns peaks over a threshold from a time series
#  deCluster         Declusters a point process
################################################################################


################################################################################
# EXPLORATIVE DATA ANALYSIS:


emdPlot = 
function(x, doplot = TRUE, plottype = c("", "x", "y", "xy"),
labels = TRUE, ...)
{   # A function imported from R-package evir

    # Description:
    #   Plots empirical distribution function
    
    # Arguments:
    #   plottype - which axes should be on a log scale: 
    #       "x" denotes x-axis only; "y" denotes y-axis only,
    #       "xy" || "yx" both axes, "" denotes neither of the 
    #       axis

    # FUNCTION:
    
    # Settings:
    alog = plottype[1]
        
    # Convert from univariate 'timSeries':
    if (is.timeSeries(x)) x = as.vector(x)
    
    # Convert x to a vector, if the input is a data.frame.
    if (is.data.frame(x)) x = x[,1] 
    xs = x = sort(as.numeric(x))
    ys = y = 1 - ppoints(x)
    
    if (plottype == "x") {
        xs = x[x > 0]
        ys = y[x > 0] 
    }
    if (plottype == "y") {
        xs = x[y > 0]
        ys = y[y > 0] 
    }
    if (plottype == "xy") {
        xs = x[x > 0 & y > 0]
        ys = y[x > 0 & y > 0] 
    }
    
    # Plot:
    if (doplot) {
        if (labels) {
            xlab = "x"
            ylab = "1-F(x)"
            main = "Empirical Distribution" 
        } else {
            xlab = ""
            ylab = ""
            main = "" }
        plot(xs, ys, log = alog, xlab = xlab, ylab = ylab, main = main, ...)    
        if (labels) grid()
    }           
    
    # Result:
    result = data.frame(x, y)
    
    # Return Value:
    if (doplot) return(invisible(result)) else return(result)   
}


# ------------------------------------------------------------------------------


qqPlot = 
function (x, doplot = TRUE, labels = TRUE, ...) 
{   # A function written by Diethelm Wuertz
    
    # Description:
    #   Creates Quantile-Quantile Plot
    
    # FUNCTION:
    
    # Convert from univariate 'timSeries':
    if (is.timeSeries(x)) x = as.vector(x)
    
    # Convert x to a vector, if the input is a data.frame.
    if (is.data.frame(x)) x = x[, 1] 
    
    # Plot:
    if (doplot) {
        if (labels) {
            xlab = "Normal Quantiles"
            ylab = "Empirical Quantiles"
            main = "Normal QQ-Plot" 
        } else {
            xlab = ""
            ylab = ""
            main = "" 
        }
        qqnorm(x, xlab = xlab, ylab = ylab, main = main, ...) 
        qqline(x) 
        if (labels) grid() 
    }
    
    # Return Value:
    if (doplot) return(invisible(x)) else return(x)
}


# ------------------------------------------------------------------------------


qqbayesPlot = 
function(x, doplot = TRUE, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Example of a Normal quantile plot of data x to provide a visual
    #   assessment of its conformity with a normal (data is standardised    
    #   first).

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
    #   Prof. Mike West, mw@stat.duke.edu 
    
    # Note:
    #   Source from
    #   http://www.stat.duke.edu/courses/Fall99/sta290/Notes/

    # FUNCTION:
    
    # Convert from univariate 'timSeries':
    if (is.timeSeries(x)) x = as.vector(x)
    
    # Settings:
    mydata = x
    n = length(mydata) 
    p = (1:n)/(n+1)
    x = (mydata-mean(mydata))/sqrt(var(mydata))
    x = sort(x)
    z = qnorm(p)
 
    # Plot:
    if (doplot) {
        if (labels) {
            xlab = "Standard Normal Quantiles"
            ylab = "Ordered Data"
            main = "Normal QQ-Plot with 95% Intervals" }
        else {
            xlab = ""
            ylab = ""
            main = "" }
        plot(z, x, xlab = xlab, ylab = ylab, main = main, ...)
        abline(0, 1, col = "steelblue")
        if (labels) grid() 
    }
  
    # 95% Intervals:
    s = 1.96*sqrt(p*(1-p)/n)
    pl = p-s; i = pl<1&pl>0
    lower = quantile(x, probs = pl[i])
    if (doplot) lines(z[i], lower, col = 3)
    pl = p+s; i = pl < 1 & pl > 0
    upper = quantile(x, probs = pl[i])
    if (doplot) lines(z[i], upper, col = 3)
    
    # Result:
    result = data.frame(lower, upper)
    
    # Return Value:
    if (doplot) return(invisible(result)) else return(result)
}     


# ------------------------------------------------------------------------------


qPlot = 
function(x, xi = 0, trim = NA, threshold = NA, doplot = TRUE, 
labels = TRUE, ...)
{   # A function imported from R-package evir
    
    # Description:
    #   Creates an exploratory QQplot for Extreme Value Analysis.

    # FUNCTION:
    
    # Convert from univariate 'timSeries':
    if (is.timeSeries(x)) x = as.vector(x)
    
    # Settings:
    line = TRUE
    
    # Convert x to a vector, if the input is a data.frame.
    if(is.data.frame(x)) x = x[,1] 
    x = as.numeric(x)
    if (!is.na(threshold)) x = x[x >= threshold]
    if (!is.na(trim)) x = x[x < trim]
    if (xi == 0) {
        y = qexp(ppoints(x)) 
    }
    if( xi != 0) {
        y = qgpd(ppoints(x), xi = xi) 
    }
    
    # Plot:
    if (doplot) {
        if (labels) {
            xlab = "Ordered Data"
            ylab = "Quantiles"
            if (xi == 0) {
                ylab = paste("Exponential", ylab) 
            }
            if (xi != 0) {
                ylab = paste("GPD(xi=", xi, ") ",  ylab, sep = "") 
            }
            main = "Exploratory QQ Plot" 
        } else {
            xlab = ""
            ylab = ""
            main = "" 
        }
        plot(sort(x), y, xlab = xlab, ylab = ylab, main = main, ...)
        if (line) abline(lsfit(sort(x), y)) 
        if (labels) grid()
    }
    
    # Result:
    result = data.frame(x = sort(x), y)
    
    # Return Value:
    if (doplot) return(invisible(result)) else return(result)
}


# ------------------------------------------------------------------------------


mxfPlot = 
function (x, tail = 0.05, doplot = TRUE, labels = TRUE, ...)     
{   # A function written by D. Wuertz
    
    # Description:
    #   Creates a simple mean excess function plot.
    
    # FUNCTION:
    
    # Convert from univariate 'timSeries':
    if (is.timeSeries(x)) x = as.vector(x)
    
    # Convert x to a vector, if the input is a data.frame.
    if(is.data.frame(x)) x = x[,1] 
    u = rev(sort(x))
    n = length(x)
    u = u[1:floor(tail*n)]
    n = length(u)
    e = (cumsum(u)-(1:n)*u)/(1:n)
    
    # Plot
    if (doplot) {
        if (labels) {
            xlab = "Threshold: u"
            ylab = "Mean Excess: e"
            main = "Mean Excess Function" 
        } else {
            xlab = ""
            ylab = ""
            main = "" 
        }
        plot (u, e, xlab = xlab, ylab = ylab, main = main, ...) 
        if (labels) grid()
    }
    
    # Result:
    result = data.frame(threshold = u, excess = e)
    
    # Return Values:
    if (doplot) return(invisible(result)) else return(result)
}


# ------------------------------------------------------------------------------


mrlPlot = 
function(x, conf = 0.95, umin = NA, umax=NA, nint = 100, 
doplot = TRUE, plottype = c("autoscale", ""), labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Create a mean residual life plot with
    #   confidence intervals.
    
    # Note:
    #   "autoscale" added by DW.
    
    # References:
    #   A function originally written by S. Coles
    
    # FUNCTION:
    
    # Convert from univariate 'timSeries':
    if (is.timeSeries(x)) x = as.vector(x)
    
    # Settings:
    plottype = plottype[1]
    if (plottype == "autoscale") {
        autoscale = TRUE 
    } else {
        autoscale = FALSE 
    }
    
    # Convert x to a vector, if the input is a data.frame.
    if (is.data.frame(x)) x = x[,1] 
    if (is.na(umin)) umin = mean(x)
    if (is.na(umax)) umax = max(x)
    sx = xu = xl = rep(NA, nint)
    u = seq(umin, umax, length = nint)
    for(i in 1:nint) {
        x = x[x >= u[i]]
        sx[i] = mean(x - u[i])
        sdev = sqrt(var(x))
        n = length(x)
        xu[i] = sx[i] + (qnorm((1 + conf)/2) * sdev)/sqrt(n)
        xl[i] = sx[i] - (qnorm((1 + conf)/2) * sdev)/sqrt(n) 
    }
    
    # Plot:
    if (doplot) {
        if (labels) {
            xlab = "Threshold: u"
            ylab = "Mean Excess: e"
            main = "Mean Residual Live Plot" 
        } else {
            xlab = ""
            ylab = ""
            main = "" 
        }
        if (autoscale) {
            ylim = c(min(xl[!is.na(xl)]), max(xu[!is.na(xu)]))
            plot(u, sx, type = "l", lwd = 2, xlab = xlab, 
                ylab = ylab, ylim = ylim, main = main, ...) 
        } else {
            plot(u[!is.na(xl)], sx[!is.na(xl)], type = "l", 
                lwd = 2, xlab = xlab, ylab = ylab, main = main, ...) 
        } 
        lines(u[!is.na(xl)], xl[!is.na(xl)], col = "steelblue")
        lines(u[!is.na(xu)], xu[!is.na(xu)], col = "steelblue")
        if (labels) grid() 
    }
    
    # Result
    result = data.frame(threshold = u, mrl = sx)
    
    # Return Value:
    if (doplot) return(invisible(result)) else return(result)
}


# ------------------------------------------------------------------------------


mePlot = 
function(x, doplot = TRUE, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Create a Mean Excess Plot
    
    # Reference:
    #   A function imported from R-package evir
    
    # FUNCTION:
    
    # Convert from univariate 'timSeries':
    if (is.timeSeries(x)) x = as.vector(x)
    
    # Settings:
    omit = 0
    
    # Internal Function:
    myrank = function(x, na.last = TRUE){
        ranks = sort.list(sort.list(x, na.last = na.last))
        if(is.na(na.last))
            x = x[!is.na(x)]
        for(i in unique(x[duplicated(x)])) {
            which = x == i & !is.na(x)
            ranks[which] = max(ranks[which]) 
        }
        ranks 
    }
    
    # Convert x to a vector, if the input is a data.frame.
    if(is.data.frame(x)) x = x[,1] 
    x = as.numeric(x)
    n = length(x)
    x = sort(x)
    n.excess = unique(floor(length(x) - myrank(x)))
    points = unique(x)
    nl = length(points)
    n.excess = n.excess[-nl]
    points = points[-nl]
    excess = cumsum(rev(x))[n.excess] - n.excess * points
    y = excess/n.excess
    xx = points[1:(nl-omit)] 
    yy = y[1:(nl-omit)]
    
    # Plot:
    if (doplot) {
        if (labels) {
            xlab = "Threshold: u"
            ylab = "Mean Excess: e"
            main = "Mean Excess Plot" 
        } else {
            xlab = ""
            ylab = ""
            main = "" }
        plot(xx, yy, xlab = xlab, ylab = ylab, main = main, ...) 
        if (labels) grid()
    }
    
    # Results:
    result = data.frame(threshold = xx, me = yy)
    
    # Return Value:
    if (doplot) return(invisible(result)) else return(result)
    
}


# -----------------------------------------------------------------------------


recordsPlot = 
function(x, conf = 0.95, doplot = TRUE, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Creates a records plot.
    
    # Note:
    #   A function imported from R-package evir,
    #   original name in EVIR: records

    # FUNCTION:
    
    # Convert from univariate 'timSeries':
    if (is.timeSeries(x)) x = as.vector(x)
    
    # Settings:
    conf.level = conf
    
    # Convert x to a vector, if the input is a data.frame.
    if (is.data.frame(x)) x = x[,1] 
    
    # Records:
    record = cummax(x)
    expected = cumsum(1/(1:length(x)))
    se = sqrt(expected - cumsum(1/((1:length(x))^2)))
    trial = (1:length(x))[!duplicated(record)]
    record = unique(record)
    number = 1:length(record)
    expected = expected[trial]
    se = se[trial]
    
    # Plot:
    if (doplot) {
        if (labels) {
            xlab = "Trials"
            ylab = "Records"
            main = "Plot of Record Development" 
        } else {
            xlab = ""
            ylab = ""
            main = "" 
        }     
        ci = qnorm(0.5 + conf.level/2)
        upper = expected + ci * se
        lower = expected - ci * se
        lower[lower < 1] = 1
        yr = range(upper, lower, number)    
        plot(trial, number, log = "x", ylim = yr, 
                xlab = xlab, ylab = ylab, main = main, ...) 
        lines(trial, expected)
        lines(trial, upper, lty = 2)
        lines(trial, lower, lty = 2) 
        if (labels) grid()
    }
        
    # Result:
    result = data.frame(number, record, trial, expected, se)
    
    # Return Value:
    if (doplot) return(invisible(result)) else return(result)
}


# ------------------------------------------------------------------------------


ssrecordsPlot = 
function (x, subsamples = 10, doplot = TRUE, plottype = c("lin", "log"),
labels = TRUE,  ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a plot of records on subsamples.
    
    # note:
    #   Changes:
    #   2003/09/06 - argument list made consistent
    
    # FUNCTION:
    
    # Convert from univariate 'timSeries':
    if (is.timeSeries(x)) x = as.vector(x)
    
    # Convert x to a vector, if the input is a data.frame.
    if(is.data.frame(x)) x = x[, 1] 
    
    # Plot type:
    plottype = plottype[1]
    
    # Records:
    save = x 
    cluster = floor(length(save)/subsamples)
    records = c()
    for (i in 1:subsamples) {
        x = save[((i-1)*cluster+1):(i*cluster)]
        y = 1:length(x)
        u = x[1]
        v = x.records = 1
        while (!is.na(v)) {
            u = x[x > u][1]
            v = y[x > u][1]
            if(!is.na(v)) x.records = c(x.records, v) 
        }   
        if (i == 1) {
            nc = 1:length(x)
            csmean = cumsum(1/nc)
            cssd = sqrt(cumsum(1/nc-1/(nc*nc)))
            ymax = csmean[length(x)]+2*cssd[length(x)]
            # Plot:
            if (doplot) {
                if (plottype == "log") {
                    nc = log(nc)
                }
                if (labels) {
                    if (plottype == "lin") xlab = "n"
                    if (plottype == "log") xlab = "log(n)"
                    ylab = "N(n)" 
                }
                main = "Subsample Records Plot"
                plot (nc, csmean+cssd, type = "l", ylim = c(0, ymax),
                    xlab = xlab, ylab = ylab, main = main, ...) 
                grid()
            } else {
                plot (nc, csmean+cssd, type = "l", ylim = c(0, ymax), ...) 
            } 
            lines(nc, csmean, col = "steelblue")  
            lines(nc, csmean-cssd, col = "steelblue") 
        } 
        y.records = 1:length(x.records)
        x.records = x.records[y.records < ymax]
        if (doplot) {
            if (plottype == "log") x.records = log(x.records)
            points(x.records, y.records[y.records<ymax], pch = i) 
        }
        records[i] = y.records[length(y.records)]
    }
    
    # Result:
    subsample = 1:subsamples
    result = data.frame(subsample, records)
    
    # Return Value:
    if (doplot) return(invisible(result)) else return(result)
}


# ------------------------------------------------------------------------------


msratioPlot = 
function (x, p = 1:4, doplot = TRUE, plottype = c("autoscale", ""),
labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Creates a Plot of maximum and sum ratio.
    
    # FUNCTION:
    
    # Convert from univariate 'timSeries':
    if (is.timeSeries(x)) x = as.vector(x)
    
    # Settings:
    plottype = plottype[1]
    if (plottype == "autoscale") {
        autoscale = TRUE 
    } else {
        autoscale = FALSE 
    }
    if (autoscale) {
        ylim = c(0,1)
    }
    
    # Convert x to a vector, if the input is a data.frame.
    if(is.data.frame(x)) x = x[,1] 
    
    # Plot:
    if (doplot) {
        if (labels) {
            xlab = "Trials"
            ylab = "Records"
            main = "Plot of Maximum/Sum Ratio" 
        } else {
            xlab = ""
            ylab = ""
            main = "" 
        }             
        if (autoscale) {
            plot(c(0, length(x)), y = ylim, xlab = xlab, 
                    ylab = ylab, main = main, type = "n", ...) 
        } else {
            plot(c(0, length(x)), xlab = xlab, 
                    ylab = ylab, main = main, type = "n", ...) 
        }
        if (labels) grid()
    }
    
    # Color numbering:
    i = 1
    
    # Suppress warnings for points outside the frame:
    ratios = matrix(rep(0, times = length(x)*length(p)), byrow = TRUE, 
        ncol = length(p))
    if (doplot) par(err = -1)
    
    # Loop over all exponents p:
    for (q in p) {
        rnp = cummax(abs(x)^q) / cumsum(abs(x)^q)
        i = i + 1
        ratios[,q] = rnp
        if (doplot) lines (rnp, col = i) 
    }

    # Result:
    result = data.frame(ratios)
    
    # Return Value:
    if (doplot) return(invisible(result)) else return(result)
}


# ------------------------------------------------------------------------------


sllnPlot =  
function(x, doplot = TRUE, ...)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Verifies Kolmogorov's strong law of large numbers
    
    # Arguments:
    #   x - sequence of iid non-degenerate rvs.
    
    # References:
    #   Embrechts et al. p. 61, Theorem 2.1.3
    
    # FUNCTION:
    
    # Verify SLLN:
    x = as.vector(x)
    if (is.null(mean)) mean=mean(cumsum(x)/(1:length(x)))
    nx  =  length(x)
    
    # Plot:
    y  = cumsum(x)/(1:nx)
    mean = mean(x)
    if (doplot) {
        plot(y, xlab = "n", ylab = "x", type = "l", main = "SLLN", 
            col = "steelblue", ...)
        lines(c(0, nx), c(mean, mean), col = "brown")
        grid()
    }
    
    # Return Value:
    invisible(y)
}


# ------------------------------------------------------------------------------


lilPlot =  
function (x, doplot = TRUE, ...)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Verifies Hartman-Wintner's Law of the iterated logarithm
            
    # Arguments:
    #   x - sequence of iid non-degenerate rvs.

    # References:
    #   Embrechts et al. p. 67. Theorem 2.1.13
    
    # FUNCTION:
    
    # Verify LIL:
    x = as.vector(x)
    lx = length(x)
    nx = 1:lx
    fact = sqrt(2*nx*log(log(nx)))
    mu = mean(x)  
    sdev = sqrt(var(x))
    y = (cumsum(x)-mu*nx)/(fact*sdev)
    
    # Plot:
    if (doplot) {
        plot(x = nx, y = y, xlab = "n", ylab = "x", 
            ylim = range(y[!is.na(y)], -1, 1), type = "l", 
            main = "LIL", col = "steelblue", ...)
        lines(c(0,lx), c(1,1), col = "brown")
        lines(c(0,lx), c(-1,-1), col = "brown")
        grid()
    }
    
    # Return Value:
    invisible(y)
}


# ------------------------------------------------------------------------------


xacfPlot = 
function(x, threshold = 0.95, lag.max = 15, doplot = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates plots of exceedences, one for the
    #   heights and one for the distances.
    
    # FUNCTION:
    
    # Convert from univariate 'timSeries':
    if (is.timeSeries(x)) x = as.vector(x)
    
    # Settings:
    # Sorry, user specified labels not yet implemented.
    labels = TRUE
    if (labels) {
        xlab = c("Index", "Lag")
        ylab = c("Heights", "Distances", "ACF")
        main = c("Heights over Threshold", "Distances between Heights", 
            "Series Heights", "Series Distances") 
    }
            
    # Convert x to a vector, if the input is a data.frame.
    if (is.data.frame(x)) x = x[,1] 
    # Heights/Distances
    threshold = sort(x)[round(threshold*length(x))]
    Heights = (x-threshold)[(x-threshold)>0]
    Distances = diff((1:length(x))[(x-threshold)>0])
    
    # Plot:
    if (doplot) {
        plot (Heights, type="h", xlab = xlab[1], ylab = ylab[1], 
            main = main[1], ...)
        plot (Distances,type="h", xlab = xlab[1], ylab = ylab[2], 
            main = main[2], ...) }
    
    # Correlations:
    Heights = as.vector(acf(Heights, lag.max=lag.max, plot = doplot, 
        xlab = xlab[2], ylab = ylab[3], main = main[3], ...)$acf)
    Distances = as.vector(acf(Distances, lag.max=lag.max, plot = doplot, 
        xlab = xlab[2], ylab = ylab[3], main = main[4], ...)$acf)

    # Result:
    lag = as.vector(0:(lag.max))
    result = data.frame(lag, Heights, Distances)

    # Return Value:
    if (doplot) return(invisible(result)) else return(result)
}


################################################################################
# PLOT UTILITIES:
        
interactivePlot = 
function(x, choices = paste("Plot", 1:9), 
plotFUN = paste("plot.", 1:9, sep = ""), which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "template".
    
    # Arguments:
    #   x - an object to be plotted
    #   choices - the character string for the choice menu
    #   plotFUN - the names of the plot functions
    #   which - plot selection, which graph should be 
    #     displayed. If a character string named "ask" the 
    #     user is interactively asked which to plot, if
    #     a logical vector of length N, those plots which
    #     are set "TRUE" are displayed, if a character string
    #     named "all" all plots are displayed.
    
    # Note:
    #   At maximum 9 plots are supported.

    # FUNCTION:
    
    # Some cecks:
    if (length(choices) != length(plotFUN)) 
        stop("Arguments choices and plotFUN must be of same length.")
    if (length(which) > length(choices)) 
        stop("Arguments which has incorrect length.")
    if (length(which) > length(plotFUN)) 
        stop("Arguments which has incorrect length.")
    if (length(choices) > 9)
        stop("Sorry, only 9 plots at max are supported.")
    
    # Internal "askPlot" Function:                
    multPlot = function (x, choices, ...) 
    {
        # Selective Plot:
        selectivePlot = function (x, choices, FUN, which){
            # Internal Function:
            askPlot = function (x, choices, FUN) {
                # Pick and Plot:
                pick = 1; n.plots = length(choices)
                while (pick > 0) { pick = menu (
                    choices = paste("plot:", choices), 
                    title = "\nMake a plot selection (or 0 to exit):")
                    if (pick > 0) match.fun(FUN[pick])(x) } }                   
            if (as.character(which[1]) == "ask") {
                askPlot(x, choices = choices, FUN = FUN, ...) }
            else { 
                for (i in 1:n.plots) if (which[i]) match.fun(FUN[i])(x) }
            invisible() }  
        # match Functions, up to nine ...
        if (length(plotFUN) < 9) plotFUN = 
            c(plotFUN, rep(plotFUN[1], times = 9 - length(plotFUN)))
        plot.1 = match.fun(plotFUN[1]); plot.2 = match.fun(plotFUN[2]) 
        plot.3 = match.fun(plotFUN[3]); plot.4 = match.fun(plotFUN[4]) 
        plot.5 = match.fun(plotFUN[5]); plot.6 = match.fun(plotFUN[6]) 
        plot.7 = match.fun(plotFUN[7]); plot.8 = match.fun(plotFUN[8]) 
        plot.9 = match.fun(plotFUN[9])      
        pick = 1
        while (pick > 0) { pick = menu (
            ### choices = paste("plot:", choices),
            choices = paste(" ", choices), 
            title = "\nMake a plot selection (or 0 to exit):")
            # up to 9 plot functions ...
            switch (pick, plot.1(x), plot.2(x), plot.3(x), plot.4(x), 
                plot.5(x), plot.6(x), plot.7(x), plot.8(x), plot.9(x) ) 
        } 
    }
                              
    # Plot:
    if (is.numeric(which)) {
        Which = rep(FALSE, times = length(choices))
        Which[which] = TRUE
        which = Which
    }
    if (which[1] == "all") {
        which = rep(TRUE, times = length(choices))
    }
    if (which[1] == "ask") {
        multPlot(x, choices, ...) 
    } else {
        for ( i in 1:length(which) ) {
            FUN = match.fun(plotFUN[i])
            if (which[i]) FUN(x) 
        } 
    }
            
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


gridVector = 
function(x, y)
{   # A function implemented by Diethelm Wuertz, GPL

    # Description:
    #   Creates from two vectors x and y all grid points
    
    # Details: 
    #   The two vectors x and y span a rectangular grid with nx=length(x) 
    #   times ny=length(y) points which are returned as a matrix of size
    #   (nx*ny) times 2.
    
    # Arguments:
    #   x, y - two numeric vectors of length m and n which span the 
    #   rectangular grid of size m times n.
    
    # Value:
    #   returns a list with two elements X and Y each of length m 
    #   times n
    
    # Example:
    #   > gridVector(1:3, 1:2)
    #             [,1] [,2]
    #       [1,]    1    1
    #       [2,]    2    1
    #       [3,]    3    1
    #       [4,]    1    2
    #       [5,]    2    2
    #       [6,]    3    2

    # FUNCTION: 
    
    # Prepare for Input:
    nx = length(x)
    ny = length(y)
    xoy = cbind(rep(x, ny), as.vector(matrix(y, nx, ny, byrow = TRUE)))
    X = matrix(xoy, nx * ny, 2, byrow = FALSE)
    
    # Return Value:
    list(X = X[,1], Y = X[,2])
}


################################################################################
# DATA PREPROCESSING:


blockMaxima =
function (x, block = c("monthly", "quarterly"), doplot = FALSE) 
{   # A function implemented by Diethelm Wuertz, GPL

    # Description:
    #   Compute block maxima from a time series or numeric vector

    # FUNCTION:
    
    # Maxima:
    block = block[1]
    colFun = function(x) colStats(x, FUN = max)
    if (class(x) == "timeSeries") {
        if (is.numeric(block)) {
            data = as.vector(x)
            nblocks = (length(data) %/% block) + 1
            grouping = rep(1:nblocks, rep(block, nblocks))[1:length(data)]
            newdata = tapply(data, grouping, FUN = max) 
            pos = as.character(seriesPositions(x))
            newpos = tapply(pos, grouping, FUN = function(x) x[length(x)])
            ans = timeSeries(data = t(t(newdata)), charvec = newpos, 
                units = x@units, format = x@format, FinCenter = x@FinCenter)
        } else if (block == "monthly") {
            from = unique(timeFirstDayInMonth(seriesPositions(x)))
            to = unique(timeLastDayInMonth(seriesPositions(x)))
            ans = applySeries(x, from, to, FUN = colFun)
        } else if (block == "quarterly") {
            from = unique(timeFirstDayInQuarter(seriesPositions(x)))
            to = unique(timeLastDayInQuarter(seriesPositions(x)))
            ans = applySeries(x, from, to, FUN = colFun)
        } else {
            stop("Unknown block size for timeSeries Object")
        }
    } else {
        if (is.numeric(block)) {
            data = as.vector(x)
            nblocks = (length(data) %/% block) + 1
            grouping = rep(1:nblocks, rep(block, nblocks))[1:length(data)]
            ans = as.vector(tapply(data, grouping, FUN = max))
            ans = as.ts(ans)
        } else {
            stop("For non-timeSeries Objects blocks must be numeric")
        }
    }
    if (doplot) {
        plot(ans, type = "h", col = "steelblue", main = "Block Maxima")
        grid()
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


findThreshold =
function(x, n = floor(0.05*length(as.vector(x))))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Upper threshold for a given number of extremes
    
    # Arguments:
    #   x - a univariate time series object or numeric vector
    #   n - a numeric value or vector giving number of extremes 
    #       above the threshold, by default 5%.
    
    # Example:
    #   findThreshold(x = as.timeSeries(data(bmwRet)), 
    #      n = floor(c(0.05, 0.10)*length(as.vector(x))))

    # FUNCTION:
    
    # Settings:
    x = as.vector(x)
    
    # Continue:
    x = rev(sort(x))
    thresholds = unique(x)
    indices = match(x[n], thresholds)
    indices = pmin(indices + 1, length(thresholds)) 
    
    # Result:
    ans = thresholds[indices]
    names(ans) = paste("n=", as.character(n), sep = "")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


pointProcess =
function(x, n = floor(0.05*length(as.vector(x))))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns peaks over a thrshold from a time series or numeric vector
    
    # Arguments:
    #   x - a univariate time series object or numeric vector
    #   n - a numeric value or vector giving number of extremes 
    #       above the threshold, by default 5%.
 
    # FUNCTION:
    
    # Point Process:
    threshold = findThreshold(x = x, n = n)
    if (class(x) == "timeSeries") {
        newdata = as.vector(x@Data[, 1])
        newx = newdata[newdata > threshold]
        pos = as.character(seriesPositions(x))
        newpos = pos[newdata > threshold]
        ans = timeSeries(data = t(t(newx)), charvec = newpos, 
            units = x@units, format = x@format, FinCenter = x@FinCenter)
    } else {
        ans = as.ts(x[x > threshold[1]])
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


deCluster = 
function(x, run, doplot = FALSE)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Declusters a clustered point process from a 'timeSeries' object
    
    # Note:
    #   Imported from R-package evir/EVIS.
    
    # Example:
    # xPP = pointProcess(-as.timeSeries(data(bmwRet)), n = 300)
    # deCluster(x = xPP, run = 15, doplot = TRUE)
    
    # FUNCTION:
    
    # Settings:
    labels = TRUE
    times =  as.POSIXct(seriesPositions(x))
    
    # What follows is from package 'evir' by Alexander Mc Neil.
    # This Requires a reimplementation.
    series = as.vector(x)
    picture = doplot
    n = length(as.numeric(series)) 
    # times = attributes(series)$times
    # if (is.null(times)) stop("`series' must have a `times' attribute")
    as.posix = is.character(times) || inherits(times, "POSIXt") || 
        inherits(times, "date") || inherits(times, "dates")
    if (as.posix) {
        gaps = as.numeric(difftime(as.POSIXlt(times)[2:n], 
            as.POSIXlt(times)[1:(n - 1)], units = "days"))
    } else {
        gaps = as.numeric(diff(times))
    }
    longgaps = gaps > run
    if (sum(longgaps) <= 1) {
        stop("Decluster parameter too large")
    }
    cluster = c(0, cumsum(longgaps))
    cmax = tapply(as.numeric(series), cluster, max)
    newtimes = times[match(cmax, series)]
    newseries = structure(series[match(cmax, series)], times = newtimes)
    n = length(as.numeric(newseries))
    if (as.posix) {
        newgaps = as.numeric(difftime(as.POSIXlt(newtimes)[2:n], 
            as.POSIXlt(newtimes)[1:(n - 1)], units = "days"))
        times = as.POSIXlt(times)
        newtimes = as.POSIXlt(newtimes) 
    } else {
        newgaps = as.numeric(diff(newtimes)) 
    }
    
    # Plot:
    if (doplot) {
        # cat("Declustering picture...\n")
        # cat(paste("Data reduced from", length(as.numeric(series)), 
        #     "to", length(as.numeric(newseries)), "\n"))
        # par(mfrow = c(2, 2))
        if (labels) {
            plot(times, series, type = "h", main = "Original Series",
                col = "steelblue")
            qPlot(gaps, pch = 19, col = "steelblue")
            plot(newtimes, newseries, type = "h", main = "Declustered Series",
                col = "steelblue")
            qPlot(newgaps, pch = 19, col = "steelblue") 
        }
    }
    
    # Result - back to Rmetrics:
    ans = timeSeries(
        data = t(t(as.vector(newseries))), charvec = attr(,newseries, "times"), 
        units = x@units, format = x@format, FinCenter = x@FinCenter)
        
    # Return Value:
    ans
}   

 
################################################################################

