
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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             SKEWNESS AND KURTOSIS:
#  skewness              Returns a number which is the skewness of the data
#   skewness.default      Default method
#   skewness.data.frame   Method for objects of class data.frame
#   skewness.POSIXct      Method for objects of class POSIXct 
#   skewness.POSIXlt      Method for objects of class POSIXlt 
#  kurtosis              Returns a number which is the kurtosis of the data
#   kurtosis.default      Default method
#   kurtosis.data.frame   Method for objects of class data.frame
#   kurtosis.POSIXct      Method for objects of class POSIXct
#   kurtosis.POSIXlt      Method for objects of class POSIXlt
#  basicStats            Returns a basic statistics summary
# FUNCTION:             DESCRIPTION:
#  .distCheck            Checks consistency of distributions
#  .bootMean             Boottraps the population mean
# FUNCTION:             SPLUS FUNCTIONALITY:
#  stdev                 S-PLUS: Returns the standard deviation of a vector
################################################################################


################################################################################
#  skewness              Returns a number which is the skewness of the data
#   skewness.default      Default method
#   skewness.data.frame   Method for objects of class data.frame
#   skewness.POSIXct      Method for objects of class POSIXct 
#   skewness.POSIXlt      Method for objects of class POSIXlt 
#  kurtosis              Returns a number which is the kurtosis of the data
#   kurtosis.default      Default method
#   kurtosis.data.frame   Method for objects of class data.frame
#   kurtosis.POSIXct      Method for objects of class POSIXct
#   kurtosis.POSIXlt      Method for objects of class POSIXlt
#  basicStats            Returns a basic statistics summary


kurtosis =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value:
    UseMethod("kurtosis")
}


# ------------------------------------------------------------------------------


kurtosis.default =
function (x, na.rm = FALSE, method = c("excess", "moment", "fisher"), ...) 
{   # A function implemented by Diethelm Wuertz
  
    # Description:
    #   Returns the value of the kurtosis of a distribution function. 
    
    # Details:
    #   Missing values can be handled.
    
    # FUNCTION:
    
    # Method:
    method = method[1]
    
    # Warnings:
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(as.numeric(NA))}
        
    # Remove NAs:
    if (na.rm) x = x[!is.na(x)]

    # Kurtosis:
    n = length(x)
    if (is.integer(x)) x = as.numeric(x) 
    if (method == "excess") {
        kurtosis = sum((x-mean(x))^4/var(x)^2)/length(x) - 3
    } 
    if (method == "moment") {
        kurtosis = sum((x-mean(x))^4/var(x)^2)/length(x)
    } 
    if (method == "fisher") {
        kurtosis = ((n+1)*(n-1)*((sum(x^4)/n)/(sum(x^2)/n)^2 - 
            (3*(n-1))/(n+1)))/((n-2)*(n-3))
    }

    # Add Control Attribute:
    attr(kurtosis, "method") <- method
    
    # Return Value:
    kurtosis  
}


# ------------------------------------------------------------------------------


kurtosis.data.frame = 
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value:
    sapply(x, kurtosis, ...)
}


# ------------------------------------------------------------------------------


kurtosis.POSIXct =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    structure(kortosis(unclass(x), ...), class = c("POSIXt", "POSIXct"))
}


# ------------------------------------------------------------------------------


kurtosis.POSIXlt =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz
 
    # FUNCTION:
    
    # Return Value:
    as.POSIXlt(kurtosis(as.POSIXct(x), ...))
}


# ******************************************************************************


skewness =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz
 
    # FUNCTION:
    
    # Return Value:
    UseMethod("skewness")
}


# ------------------------------------------------------------------------------


skewness.default =
function (x, na.rm = FALSE, method = c("moment", "fisher"), ...) 
{   # A function implemented by Diethelm Wuertz
  
    # Description:
    #   Returns the value of the skewness of a distribution function. 
    
    # Details:
    #   Missing values can be handled.
    
    # FUNCTION:
    
    # Method:
    method = match.arg(method)
    
    # Warnings:
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(as.numeric(NA))}
        
    # Remove NAs:
    if (na.rm) x = x[!is.na(x)]

    # Skewness:
    n = length(x)
    if (is.integer(x)) x = as.numeric(x) 
    
    # Selected Method:
    if (method == "moment") {
        skewness = sum((x-mean(x))^3/sqrt(var(x))^3)/length(x)
    } 
    if (method == "fisher") {
        if (n < 3)
            skewness = NA
        else 
            skewness = ((sqrt(n*(n-1))/(n-2))*(sum(x^3)/n))/((sum(x^2)/n)^(3/2))
    }   
    
    # Add Control Attribute:
    attr(skewness, "method") <- method
    
    # Return Value:
    skewness  
}


# ------------------------------------------------------------------------------


skewness.data.frame = 
function (x, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    sapply(x, skewness, ...)
}


# ------------------------------------------------------------------------------


skewness.POSIXct =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz
 
    # FUNCTION:
    
    # Return Value:
    structure(skewness(unclass(x), ...), class = c("POSIXt", "POSIXct"))
}


# ------------------------------------------------------------------------------


skewness.POSIXlt =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz
  
    # FUNCTION:
    
    # Return Value:
    as.POSIXlt(skewness(as.POSIXct(x), ...))
}


# ******************************************************************************


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


################################################################################
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


################################################################################
#  stdev                 S-PLUS: Returns the standard deviation of a vector


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
