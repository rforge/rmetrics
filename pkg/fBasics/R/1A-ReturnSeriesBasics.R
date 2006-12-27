
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
# FUNCTION:               TAILORED PLOT FUNCTIONS:     
#  seriesPlot              Returns a tailored return series plot
#  histPlot                Returns a tailored histogram plot
#  densityPlot             Returns a tailored kernel density estimate plot
#  qqnormPlot              Returns a tailored normal quantile-quantile plot
#  .qqbayesPlot            Wraps function qqnormPlot
# FUNCTION:               BASIC STATISTICS:
#  .basicStatsUnivariate   Calculates Basic Statistics
#  basicStats              Returns a basic statistics summary
# FUNCTION:               DESCRIPTION:
#  .distCheck              Checks consistency of distributions
#  .bootMean               Boottraps the population mean
# FUNCTION:               SPLUS FUNCTIONALITY:
#  stdev                   S-PLUS: Returns the standard deviation of a vector
################################################################################


################################################################################    
#  seriesPlot              Returns a tailored return series plot
#  histPlot                Returns a tailored histogram plot
#  densityPlot             Returns a tailored kernel density estimate plot
#  qqnormPlot              Returns a tailored normal quantile-quantile plot
#  .qqbayesPlot            Wraps function qqnormPlot


seriesPlot = 
function(x, col = "steelblue", main = x@units, ...) 
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
    stopifnot(is.timeSeries(x))
    units = x@units
    DIM = dim(x@Data)[2]
    
    # Series Plots:
    for (i in 1:DIM) {
        X = x[, i]
        plot(x = X, type = "l", col = col, main = main[i], ylab = X@units, ...)
        grid()
        abline(h = 0, col = "grey")
        rug(as.vector(X), ticksize = 0.01, side = 4)
    }
         
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


histPlot = 
function(x, col = "steelblue", main = x@units, add.fit = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a probability histogram plot for each column of a 
    #   timeSeries object
   
    # FUNCTION:
    
    # Settings:
    xlim = NULL
    
    # timeSeries:
    stopifnot(is.timeSeries(x))
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
        mean = mean(Values)
        median = median(Values)
        sd = sd(Values)
        if (is.null(xlim)) 
            xlim = c(qnorm(0.001, mean, sd), qnorm(0.999, mean, sd)) 
        result = hist(x = Values, col = col, border = "white", 
            breaks = "FD", main = main[i], xlim = xlim, probability = TRUE,
            ...) 
             
        # Add Fit:  
        if (add.fit) {
            s = seq(xlim[1], xlim[2], length = 201)
            lines(s, dnorm(s, mean, sd), lwd = 2, col = "brown")
        }
        ans[[i]] = result  
        
        # Add Mean/Median:
        abline(v = mean, lwd = 2, col = "orange")
        abline(v = median(Values), lwd = 2, col = "darkgreen")
        Text = paste("Median:", round(median, 2), "| Mean:", round(mean, 2))
        mtext(Text, side = 4, adj = 0, col = "darkgrey", cex = 0.7)
  
        # Add Zero Line:
        abline(h = 0, col = "grey")
        
        # Add Rug Plot:
        rug(Values)
    }
    
    # Return Value:
    invisible()
}  


# ------------------------------------------------------------------------------


densityPlot = 
function(x, col = "steelblue", main = x@units, add.fit = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns density plots for each column of a 
    #   timeSeries object

    # FUNCTION:
    
    # Transform 'timeSeries':
    stopifnot(is.timeSeries(x))
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
        mtext(Text, side = 4, adj = 0, col = "darkgrey", cex = 0.7)
  
        # Add Zero Line:
        abline(h = 0, col = "grey")
        
        # Add Rug Plot:
        rug(Values)     
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


qqnormPlot = 
function(x, col = "steelblue", main = x@units, labels = TRUE, ...) 
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

    # FUNCTION:
    
    # timeSeries:
    stopifnot(is.timeSeries(x))
    DIM = dim(x@Data)[2]
    Main = main
    
    # QQ Plots:
    X = x
    for (i in 1:DIM) {
        x = X[, i]
    
        # Settings
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
        } else {
            main = xlab = ylab = ""
        }
        if (labels) {
            plot(z, x, pch = 19, col = col, 
                xlab = xlab, ylab = ylab, main = main, ...)
            abline(0, 1, col = "grey")
            grid() 
        } else {
            plot(z, x, 
                xlab = xlab, ylab = ylab, main = main, ...)
            abline(0, 1, col = "grey")
        }
        rug(z, ticksize = 0.01, side = 3)
        rug(x, ticksize = 0.01, side = 4)
      
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


.qqbayesPlot = 
function(x, col = "steelblue", main = x@units, labels = TRUE, ...) 
{
    qqnormPlot(x, col, main, labels, ...) 
}


################################################################################
# FUNCTION:               BASIC STATISTICS:
#  .basicStatsUnivariate   Calculates Basic Statistics
#  basicStats              Returns a basic statistics summary
# FUNCTION:               DESCRIPTION:
#  .distCheck              Checks consistency of distributions
#  .bootMean               Boottraps the population mean
# FUNCTION:               SPLUS FUNCTIONALITY:
#  stdev                   S-PLUS: Returns the standard deviation of a vector
################################################################################


.basicStatsUnivariate = 
function(x, ci = 0.95) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Calculates Basic Statistics
 
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)  
    
    # CL Levels:    
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
    
    # Observations:
    x.length = length(x)
    x = x[!is.na(x)]
    x.na = x.length - length(x)
    
    # Basic Statistics:
    z = c(
        x.length, x.na, min(x), max(x),
        as.numeric(quantile(x, prob = 0.25, na.rm = TRUE)), 
        as.numeric(quantile(x, prob = 0.75, na.rm = TRUE)), 
        mean(x), median(x), sum(x), sqrt(var(x)/length(x)), 
        cl.vals(x, ci)[1], cl.vals(x, ci)[2], var(x), 
        sqrt(var(x)), skewness(x), kurtosis(x) )    
    
    # Row Names:
    znames = c(
        "nobs", "NAs",  "Minimum", "Maximum", 
        "1. Quartile",  "3. Quartile",  "Mean", "Median", 
        "Sum",  "SE Mean", "LCL Mean", "UCL Mean", 
        "Variance", "Stdev", "Skewness", "Kurtosis")
        
    # Output as data.frame
    ans = matrix(z, ncol = 1)
    row.names(ans) = znames
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


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
    x = as.matrix(x)
    
    # basic Statistics:
    nColumns = dim(x)[2]
    ans = NULL
    for (i in 1:nColumns) {
        ans = cbind(ans, .basicStatsUnivariate(x[, i], ci))
    }
    colNames = colnames(x)
    if (!is.null(colNames)) 
    colnames(ans) = colNames  

    # Return Value:
    data.frame(round(ans, digits = 6))
}


# ------------------------------------------------------------------------------
#  .distCheck            Checks consistency of distributions
#  .bootMean             Boottraps the population mean


.distCheck = 
function(fun = "norm", n = 1000, seed = 4711, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Checks consistency of distributions
    
    # Arguments:
    #   fun - name of distribution
    #   ... - distributional parameters
    
    # Examples:
    #   .distCheck("norm", mean = 1, sd = 1)
    #   .distCheck("t", df = 4)
    #   .distCheck("exp", rate = 2)
    #   .distCheck("weibull", shape = 1)

    # FUNCTION:
    
    # Distribution Functions:
    cat("\nDistribution Check for:", fun, "\n ")
    CALL = match.call()
    cat("Call: ")
    cat(paste(deparse(CALL), sep = "\n", collapse = "\n"), "\n", sep = "") 
    dfun = match.fun(paste("d", fun, sep = ""))
    pfun = match.fun(paste("p", fun, sep = ""))
    qfun = match.fun(paste("q", fun, sep = ""))
    rfun = match.fun(paste("r", fun, sep = ""))
    
    # Range:
    xmin = qfun(p = 0.01, ...)
    xmax = qfun(p = 0.99, ...)
    
    # Check 1:
    NORM = integrate(dfun, lower = -Inf, upper = Inf, 
        subdivisions = 5000, stop.on.error = FALSE, ...)
    cat("\n1. Normalization Check:\n NORM ")
    print(NORM)
    normCheck = (abs(NORM[[1]]-1) < 0.01)
    
    # Check 2:
    cat("\n2. [p-pfun(qfun(p))]^2 Check:\n ")
    p = c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999)
    P = pfun(qfun(p, ...), ...)
    cat("PROB = 0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999\n")
    RMSE = sd(p-P)
    print(c(RMSE = RMSE))
    rmseCheck = (abs(RMSE) < 0.0001)
    
    # Check 3:
    set.seed(seed)
    cat("\n3. r(", n, ") Check:\n", sep = "")
    r = rfun(n = n, ...)
    SAMPLE.MEAN = mean(r)
    SAMPLE.VAR = var(r)
    SAMPLE = data.frame(t(c(MEAN = SAMPLE.MEAN, "VAR" = SAMPLE.VAR)), 
        row.names = "SAMPLE")
    print(signif(SAMPLE, 3))
    fun1 = function(x, ...) { x * dfun(x, ...) }
    fun2 = function(x, M, ...) { x^2 * dfun(x, ...) }   
    MEAN = integrate(fun1, lower = -Inf, upper = Inf, 
        subdivisions = 5000, stop.on.error = FALSE,...)
    cat("   X   ")
    print(MEAN)
    VAR = integrate(fun2, lower = -Inf, upper = Inf, 
        subdivisions = 5000, stop.on.error = FALSE, ...)  
    cat("   X^2 ")
    print(VAR)
    EXACT = data.frame(t(c(MEAN = MEAN[[1]], "VAR" = VAR[[1]] - MEAN[[1]]^2)),
        row.names = "EXACT ")
    print(signif(EXACT, 3))
    meanvarCheck = (abs(SAMPLE.VAR-EXACT$VAR)/EXACT$VAR < 0.1)
    cat("\n")
    
    # Done:
    ans = list(
        normCheck = normCheck, rmseCheck = rmseCheck, meanvarCheck = meanvarCheck)
    unlist(ans)
}


# ------------------------------------------------------------------------------

 
.bootMean =
function(x, B = 1000, ci = 0.95, na.rm = TRUE, reps = FALSE)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Boottraps the population mean
    
    # Details:
    #   A very fast implementation of the basic nonparametric 
    #   bootstrap for obtaining confidence limits for the population 
    #   mean without assuming normality.       
    
    # Arguments:
    #   B - number of bootstrap resamples, by default 1000.
    #   ci - specifies the confidence level (0-1) for interval 
    #       estimation of the population mean. 
    #   na.rm - a logical flag, should NAs be removed?
    #   reps - set to TRUE to have bootMean return the vector 
    #       of bootstrapped means as the reps attribute of 
    #       the returned object .
    
    # Notes:
    #   The function calls "smean.cl.boot" from the "HMisc" package
    #   Requirements: require(Hmisc)       
 
    # FUNCTION:       
    
    # Requirements:
    # sink("@sink@") # Skip Loading Comments ...
    # library(Design, warn.conflicts = FALSE)
    # library(Hmisc, warn.conflicts = FALSE)
    # sink()
    # unlink("@sink@") 
           
    # Return Value:
    smean.cl.boot(x = x, conf.int = ci, B = B, na.rm = na.rm, reps = reps)
}


# ------------------------------------------------------------------------------
# stdev                 S-PLUS: Returns the standard deviation of a vector


stdev = 
function(x, na.rm = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the standard deviation of a vector
    
    # Notes:
    #   Under use sd, this function is for SPlus compatibility.

    # FUNCTION:
    
    # Standard Deviation:
    ans = sd(x = x, na.rm = na.rm)
    
    # Return Value: 
    ans
}


################################################################################
