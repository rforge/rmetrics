
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
# FUNCTION:             SPLUS FUNCTIONALITY:
#  stdev                 Returns the standard deviation of a vector
################################################################################


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

     # Return Value:
     sapply(x, kurtosis, ...)
}


# ------------------------------------------------------------------------------


kurtosis.POSIXct =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

     # Return Value:
     structure(kortosis(unclass(x), ...), class = c("POSIXt", "POSIXct"))
}


# ------------------------------------------------------------------------------


kurtosis.POSIXlt =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

     # Return Value:
     as.POSIXlt(kurtosis(as.POSIXct(x), ...))
}


# ******************************************************************************


skewness =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

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
    method = method[1]
    
    # Warnings:
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(as.numeric(NA))}
        
    # Remove NAs:
    if (na.rm) x = x[!is.na(x)]

    # Skewness:
    n = length(x)
    if (is.integer(x)) x = as.numeric(x) 
    
     
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

    # Return Value:
    sapply(x, skewness, ...)
}


# ------------------------------------------------------------------------------


skewness.POSIXct =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Return Value:
    structure(skewness(unclass(x), ...), class = c("POSIXt", "POSIXct"))
}


# ------------------------------------------------------------------------------


skewness.POSIXlt =
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

     # Return Value:
     as.POSIXlt(skewness(as.POSIXct(x), ...))
}


# ******************************************************************************


basicStats = 
function(x, ci = 0.95, column = 1) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Calculates Basic Statistics
    
    # Arguments:
    #   x - a numeric vector, or a rectangular data object. If 'x' 
    #       is a matrix, a data.frame, or a timeSeries object then 
    #       the first column is used by default, or alternatively
    #       the one specified by the argument 'column'.
    #   ci - a numeric value setting the confidence interval.
    
    # Value:
    #   a two-column data frame, where the first column takes the 
    #   value of the statistics, and the second its name, e.g.
    #   "nobs", "NAs",  "Minimum", "Maximum", "1. Quartile",  
    #   "3. Quartile",  "Mean", "Median", "Sum",  "SE Mean", 
    #   "LCL Mean", "UCL Mean", "Variance", "Stdev", "Skewness", 
    #   "Kurtosis")
     
    # FUNCTION:
    
    # DW 2005.05.02
    if (class(x) == "matrix") {
        x = x[, column]
        warning("Column ", column, " of matrix used")
    }
    if (class(x) == "data.frame") {
        x = x[, column]
        warning("Column ", column, " of data.frame used")
        if (!is.numeric(x)) stop("The selected column is not numeric")
    }
    if (class(x) == "timeSeries") {
        x = x@Data[, column]
        warning("Column ", column, " of timeSeries used")
    }
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
    ans = data.frame(Value = z, row.names = znames)
    
    # Return Value:
    ans
    
}


# ******************************************************************************


rowStats = 
function(x, FUN, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample statistics by column
    
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
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = mean, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = mean, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowVars = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample variance by column
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = var, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = var, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowStdevs = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample standard deviation by column
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = sqrt(apply(na.remove(x), MARGIN = 1, FUN = var, ...))
    } else {
        result = sqrt(apply(x, MARGIN = 1, FUN = var, ...))
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowSkewness = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample skewness by column
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = skewness, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = skewness, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowKurtosis = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample kurtosis by column
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = kurtosis, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = kurtosis, ...) 
    }
        
    # Return Value:
    result 
}


# ------------------------------------------------------------------------------


rowCumsums = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by column
    
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


# ******************************************************************************


colStats = 
function(x, FUN, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample statistics by column
    
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


# ******************************************************************************


stdev = 
function(x, na.rm = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the standard deviation of a vector
    
    # Notes:
    #   Under use sd, this function is for SPlus compatibility.
    
    # FUNCTION:
    
    # Return Value: 
    sd(x = x, na.rm = na.rm)
}
  

################################################################################

