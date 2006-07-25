
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
# FUNCTION:             ROW AND COLUMN STATISTICS:
#  rowStats              Computes sample statistics by row
#   rowAvgs               Computes sample mean by row
#   rowVars               Computes sample variance by row
#   rowStdevs             Computes sample variance by row
#   rowSkewness           Computes sample skewness by row
#   rowKurtosis           Computes sample kurtosis by row
#   rowCumsums            Computes sample cumulated sums by row
#  colStats              Computes sample statistics by column
#   colAvgs               Computes sample mean by column
#   colVars               Computes sample variance by column
#   colStdevs             Computes sample variance by column
#   colSkewness           Computes sample skewness by column
#   colKurtosis           Computes sample kurtosis by column
#   colCumsums            Computes sample cumulated sums by column
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

    # Changes:
    #
    
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
    
    # Changes:
    #
    
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

    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    sapply(x, kurtosis, ...)
}


# ------------------------------------------------------------------------------


kurtosis.POSIXct =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    structure(kortosis(unclass(x), ...), class = c("POSIXt", "POSIXct"))
}


# ------------------------------------------------------------------------------


kurtosis.POSIXlt =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    as.POSIXlt(kurtosis(as.POSIXct(x), ...))
}


# ******************************************************************************


skewness =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
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
    
    # Changes:
    #
    
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

    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    sapply(x, skewness, ...)
}


# ------------------------------------------------------------------------------


skewness.POSIXct =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    structure(skewness(unclass(x), ...), class = c("POSIXt", "POSIXct"))
}


# ------------------------------------------------------------------------------


skewness.POSIXlt =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
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
     
    # Changes:
    #
    
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
     
    # Changes:
    #
    
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
# FUNCTION:             ROW AND COLUMN STATISTICS:
#  rowStats              Computes sample statistics by row
#   rowAvgs               Computes sample mean by row
#   rowVars               Computes sample variance by row
#   rowStdevs             Computes sample variance by row
#   rowSkewness           Computes sample skewness by row
#   rowKurtosis           Computes sample kurtosis by row
#   rowCumsums            Computes sample cumulated sums by row
#  colStats              Computes sample statistics by column
#   colAvgs               Computes sample mean by column
#   colVars               Computes sample variance by column
#   colStdevs             Computes sample variance by column
#   colSkewness           Computes sample skewness by column
#   colKurtosis           Computes sample kurtosis by column
#   colCumsums            Computes sample cumulated sums by column


rowStats = 
function(x, FUN, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample statistics by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = FUN, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = FUN, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowAvgs = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample mean by column
    
    # Note:
    #   R's base package comes already with a colMeans!
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = mean, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = mean, ...) 
    }
    result = t(t(result))
    colnames(result) = "Mean"
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowVars = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample variance by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = var, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = var, ...) 
    }
    result = t(t(result))
    colnames(result) = "Variance"
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowStdevs = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample standard deviation by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = sqrt(apply(na.remove(x), MARGIN = 1, FUN = var, ...))
    } else {
        result = sqrt(apply(x, MARGIN = 1, FUN = var, ...))
    }
    result = t(t(result))
    colnames(result) = "StDev"
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowSkewness = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample skewness by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = skewness, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = skewness, ...) 
    }
    result = t(t(result))
    colnames(result) = "Skewness"
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowKurtosis = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample kurtosis by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = kurtosis, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = kurtosis, ...) 
    }
    result = t(t(result))
    colnames(result) = "Kurtosis"
        
    # Return Value:
    result 
}


# ------------------------------------------------------------------------------


rowCumsums = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 2, FUN = cumsum, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = cumsum, ...) 
    }
    colnames(result) = paste(1:ncol(x))
        
    # Return Value:
    result
}


################################################################################


colStats = 
function(x, FUN, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample statistics by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 2, FUN = FUN, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = FUN, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colAvgs = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample mean by column
    
    # Note:
    #   R's base package comes already with a colMeans!
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm){
        result = apply(na.remove(x), MARGIN = 2, FUN = mean, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = mean, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colVars = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample variance by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) { 
        result = apply(na.remove(x), MARGIN = 2, FUN = var, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = var, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colStdevs = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample standard deviation by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = sqrt(apply(na.remove(x), MARGIN = 2, FUN = var, ...))
    } else {
        result = sqrt(apply(x, MARGIN = 2, FUN = var, ...))
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colSkewness = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample skewness by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 2, FUN = skewness, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = skewness, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colKurtosis = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample kurtosis by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 2, FUN = kurtosis, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = kurtosis, ...) 
    }
        
    # Return Value:
    result 
}


# ------------------------------------------------------------------------------


colCumsums = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by column
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 2, FUN = cumsum, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = cumsum, ...) 
    }
        
    # Return Value:
    result 
}
  

################################################################################
#  .distCheck            Checks consistency of distributions
#  .bootMean             Boottraps the population mean


.distCheck = 
function(fun = "norm", s, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Checks consistency of distributions
    
    # Examples:
    #   .distCheck("norm", mean = 1, sd = 1)
    #   .distCheck("t", df = 4)
    #   .distCheck("exp", rate = 2)
    #   .distCheck("weibull", shape = 1)

    # FUNCTION:
    
    # Distribution Functions:
    cat("\nDistribution Check for:", fun, "\n")
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
    cat("\n1. Normalization Check:\nNORM ")
    print(NORM)
    
    # Check 2:
    cat("\n2. [p-pfun(qfun(p))]^2 Check:\n")
    p = c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999)
    P = pfun(qfun(p, ...), ...)
    cat("PROB = 0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999\n")
    RMSE = sd(p-P)
    print(c(RMSE = RMSE))
    
    # Check 3:
    cat("\n3. r(10'000) Check:\n")
    n = 10000
    r = rfun(n = 10000, ...)
    SAMPLE = data.frame(t(c(MEAN = mean(r), "VAR" = var(r))), 
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
    cat("\n")
    
    # Done:
    invisible()
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
    
    # Changes:
    #
    
    # FUNCTION:       
    
    # Requirements:
    sink("@sink@") # Skip Loading Comments ...
    library(Design, warn.conflicts = FALSE)
    library(Hmisc, warn.conflicts = FALSE)
    sink()
    unlink("@sink@") 
           
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
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value: 
    sd(x = x, na.rm = na.rm)
}


################################################################################
