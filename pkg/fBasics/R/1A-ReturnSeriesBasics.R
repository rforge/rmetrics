
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
#  histPlot                Returns a tailored histogram plot
#  densityPlot             Returns a tailored kernel density estimate plot
#  qqnormPlot              Returns a tailored normal quantile-quantile plot
#  qqnigPlot               Returns a tailored NIG quantile-quantile plot
# FUNCTION:               GUI:
#  .returnSeriesGUI        Opens a GUI for return series plots
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
#  histPlot                Returns a tailored histogram plot
#  densityPlot             Returns a tailored kernel density estimate plot
#  qqnormPlot              Returns a tailored normal quantile-quantile plot
#  qqnigPlot               Returns a tailored NIG quantile-quantile plot


seriesPlot = 
function(x, type = "l", col = "steelblue", main = x@units, rug = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns time series graphs in a common plot
  
    # Arguments:
    #   x - an univariate time series
    
    # Example:
    # tS=timeSeries(cbind(rnorm(12),rt(12,4)),timeCalendar(),units=c("N","T"))
    # seriesPlot(tS)
    
    # FUNCTION:

    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    units = x@units
    DIM = dim(x@Data)[2]
    
    # Series Plots:
    for (i in 1:DIM) {
        X = x[, i]
        plot(x = X, type = type, col = col, 
            main = main[i], ylab = X@units, ...)
        grid()
        abline(h = 0, col = "grey")
        if (rug) rug(as.vector(X), ticksize = 0.01, side = 4)
    }
         
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


histPlot = 
function(x, labels = TRUE, main = x@units, add.fit = TRUE, rug = TRUE, 
    skipZeros = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a probability histogram plot for each column of a 
    #   timeSeries object
   
    # FUNCTION:
    
    # Settings:
    xlim = NULL
    
    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    units = x@units
    DIM = dim(x@Data)[2]
      
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
            result = hist(x = Values, col = "steelblue", 
            border = "white", breaks = "FD", main = main[i], 
            xlim = xlim, probability = TRUE, ...) 
            box()
        } else {
            result = hist(x = Values, probability = TRUE, main = "", 
                xlab = "", ylab = "", ...)
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
        if(rug) rug(Values)
    }
    
    # Return Value:
    invisible()
}  


# ------------------------------------------------------------------------------


densityPlot = 
function(x, col = "steelblue", main = x@units, add.fit = TRUE, rug = TRUE,
    skipZeros = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns density plots for each column of a timeSeries object

    # FUNCTION:
    
    # Transform 'timeSeries':
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    units = x@units
    DIM = dim(x@Data)[2]
    xlim = NULL
      
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
        plot(x = Density, xlim = xlim, col = col, type = "l", 
            lwd = 2, main = main[i], ...)  
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
        if(rug) rug(Values)     
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


qqnormPlot = 
function(x, labels = TRUE, col = "steelblue", main = x@units, rug = TRUE, ...) 
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
    Main = main
    
    # QQ Plots:
    X = x
    for (i in 1:DIM) {
        x = X[, i]
    
        # Settings:
        mydata = as.vector(X[, i])
        n = length(mydata) 
        p = (1:n)/(n+1)
        x = (mydata-mean(mydata))/sqrt(var(mydata))
        x = sort(x)
        z = qnorm(p)
     
        # Plot:
        if (labels) {
            xlab = "Standard Normal Quantiles"
            ylab = paste(Main[i], "Ordered Data")
            main = paste(Main[i], "with 95% CI") 
            plot(z, x, pch = 19, col = col, 
                xlab = xlab, ylab = ylab, main = main, ...)
            grid()  
        } else {
            plot(z, x, ...)
        }
        abline(0, 1, col = "grey")
        if(rug) rug(z, ticksize = 0.01, side = 3)
        if(rug) rug(x, ticksize = 0.01, side = 4)
      
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
    }
    
    # Return Value:
    invisible()  
}


# ------------------------------------------------------------------------------


qqnigPlot = 
function (x, labels = TRUE, col = "steelblue", main = x@units, rug = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Example:
    #   qqnigPlot(rnig(100))
    
    # FUNCTION:
       
    # Settings:
    if (!is.timeSeries(x)) {
        x = as.timeSeries(x)
        stopifnot(isUnivariate(x)) 
        Main = main
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
    if(rug) rug(z, ticksize = 0.01, side = 3)
    if(rug) rug(x, ticksize = 0.01, side = 4)
    
    # Result:
    .DEBUG <<-fit
    ans = list(x = z, y = x)
    attr(ans, "control")<-par
    
    # Return Value:
    invisible(ans)
}


################################################################################


.returnSeriesGUI = 
function(x, mfrow = c(3, 3))
{   # A function implemented by Diethelm Wuertz

    # Descriptions:
    #   Opens a GUI for return series plots
    
    # FUNCTION:
    
    returnSeriesRefreshCode = 
    function(...)
    {
        # Settings:
        selectedAsset  = .tdSliderMenu(no = 1)
        type = as.integer(.tdSliderMenu(obj.name = "returnSeriesType"))
        Unit = colnames(x)
        
        # Basic Return Statistics:
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
        
        # NIG QQ Plot:
        if (type == 7) {
            if (selectedAsset == 0) {
                par(mfrow = mfrow)
                qqnigPlot(x)  
            } else {
                par(mfrow = c(1, 1))
                qqnigPlot(x[, selectedAsset])  
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
                returnSeriesRefreshCode()},
            function(...){
                .tdSliderMenu(obj.name = "returnSeriesType", obj.value = "7")
                returnSeriesRefreshCode()}
        ),
        
        but.names = c(
            "1 Basic Return Statistics",
            "2 Return Series Plot", 
            "3 Cumulated Return Series Plot", 
            "4 Histogram Plot",
            "5 Density Plot",
            "6 Normal Quantile-Quantile Plot", 
            "7 NIG Quantile-Quantile Plot"),
     
        title = "Return Series GUI"
        )        
            
   .tdSliderMenu(obj.name = "type", obj.value = "1", no = 1)
   
   # Return Value()
   invisible()
}


################################################################################

