
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
# METHODS:             MODIFICATION METHODS:
#  diff.timeSeries      S3: Differences a 'timeSeries' object
#  lag.timeSeries       S3: Lags a 'timeSeries' object
#  merge.timeSeries     S3: Merges two 'timeSeries' objects
#  scale.timeSeries     S3: Centers and/or scales a 'timeSeries' object
#  summary.timeSeries   S3: Summarizes a 'timeDate' object
#  var.timeSeries       S3: Returns variance for a 'timeSeries' object
# METHODS:             MATHEMATICAL OPERATIONS ON DATA:
#  Ops.timeSeries       S3: Arith method for a 'timeSeries' object
#  abs.timeSeries       S3: Returns abolute values of a 'timeSeries' object
#  sqrt.timeSeries      S3: Returns sqrt values of a 'timeSeries' object
#  exp.timeSeries       S3: Returns exponentials of a 'timeSeries' object
#  log.timeSeries       S3: Returns logarithms of a 'timeSeries' object
#  quantile.timeSeries  S3: produces sample quantiles of a 'timeSeries' object
# METHODS:             SUBSETTING METHODS ON DATA:
#  [.timeSeries         S3: subsets of a 'timeSeries' object
#  cut.timeSeries       S3: cuts a block from a 'timeSeries' object
#  head.timeSeries      S3: returns the head of a 'timeSeries' object
#  tail.timeSeries      S3: returns the tail of a 'timeSeries' object
#  outlier.timeSeries   S3: Removes outliers from a 'timeSeries' object  
# METHODS:             DIM OPERATIONS ON DATA: 
#  dim                  Returns the dimension of a 'timeSeries' object
#  dimnames             Returns the dimension names of a 'timeSeries' object
#  colnames<-.timeS*    Assigns column names to a 'timeSeries' object
#  rownames<-.timeS*    Assigns row names to a 'timeSeries' object
#  is.array.timeSeries  Allows that NCOL and NROW work properly
################################################################################


test.aaa = 
function()
{
    # Help File:
    par(ask = FALSE)
    helpFile = function() { 
        example(TimeSeriesData, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.diffTimeSeries = 
function()
{
    # diff.timeSeries - Differences a 'timeSeries' object

    # Univariate Series:
    # Multivariate Data Set:
    set.seed(4711)
    data = cbind(RNORM = round(rnorm(6), 2))
    charvec = timeCalendar()[1:6]
    recordIDs = data.frame(IDs = LETTERS[1:6])
    uTS = timeSeries(data, charvec, recordIDs = recordIDs)
    uTS
    uTS@recordIDs
   
    # Differencing over 1 lag
    X = diff(x = uTS, lag = 1, diff = 1, trim = FALSE, pad = NA)
    X
    X@recordIDs
    # X = diff(x = uTS, lag = 1, diff = 1, trim = TRUE, pad = NA)
    # X
    # X@recordIDs
    X = diff(x = uTS, lag = 1, diff = 1, trim = FALSE, pad = 0)
    X
    X@recordIDs
    
    # Differencing over 2 lags
    X = diff(x = uTS, lag = 2, diff = 1, trim = FALSE, pad = NA)
    X
    X@recordIDs
    # X = diff(x = uTS, lag = 2, diff = 1, trim = TRUE, pad = NA)
    # X
    # X@recordIDs
    X = diff(x = uTS, lag = 2, diff = 1, trim = FALSE, pad = 0)
    X
    X@recordIDs
   
    # Differencing twice:
    # X = diff(x = uTS, lag = 1, diff = 2, trim = FALSE, pad = NA) #ERROR
    # X
    # X@recordIDs
    # X = diff(x = uTS, lag = 2, diff = 2, trim = FALSE, pad = NA)  # ERROR
    # X
    # X@recordIDs
    # X = diff(x = uTS, lag = 1, diff = 2, trim = TRUE, pad = NA)
    # X
    # X@recordIDs
    # X = diff(x = uTS, lag = 2, diff = 2, trim = TRUE, pad = NA)
    # X
    # X@recordIDs
  
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.lagTimeSeries = 
function()
{
    # lag.timeSeries - Lags a 'timeSeries' object
         
    # Univariate Series:
    set.seed(4711)
    data = cbind(RNORM = round(rnorm(6), 2))
    charvec = timeCalendar()[1:6]
    recordIDs = data.frame(IDs = LETTERS[1:6])
    uTS = timeSeries(data, charvec, recordIDs = recordIDs)
    
    # Multivariate Data Set:
    set.seed(4711)
    data = cbind(round(rnorm(12), 2), round(rt(12, df = 4), 2) )
    charvec = format(timeCalendar(2006))
    mTS = timeSeries(data, charvec, units = c("RNORM", "RT"))
    mTS 
    
    # Time Series Lags:
    X = lag(x = uTS, k = 1, trim = FALSE, units = NULL)
    X 
    X@recordIDs
    X = lag(x = uTS, k = c(2,4), trim = FALSE, units = NULL)
    X 
    X@recordIDs
    # X = lag(x = uTS, k = c(2,4), trim = TRUE, units = NULL)   # ERROR
    # X 
    # X@recordIDs
    X = lag(x = uTS, k = -1:1, trim = FALSE, units = LETTERS[1:3])
    X 
    X@recordIDs 
    
    # Multivariaye Series:
    diff(mTS, 1, 1)
    lag(mTS, 1)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.mergeTimeSeries = 
function()
{
    # merge.timeSeries - Merges two 'timeSeries' objects
    # scale.timeSeries - Centers and/or scales a 'timeSeries' object
    # summary.timeSeries - Summarizes a 'timeDate' object
    # var.timeSeries - Returns variance for a 'timeSeries' object
         
    # Univariate Series:
    set.seed(4711)
    data = cbind(RNORM = round(rnorm(6), 2))
    charvec = timeCalendar()[1:6]
    recordIDs = data.frame(IDs = LETTERS[1:6])
    uTS = timeSeries(data, charvec, recordIDs = recordIDs)
    
    # Merge:
    X = uTS
    Y = log(abs(uTS))
    merge(x = X, y = Y, units = c("RN", "logAbsRN"))
    merge(x = X[-6], y = Y[-3], units = c("RN", "logAbsRN"))      
    merge(x = X[2:5], y = Y[4:6], units = c("RN", "logAbsRN"))     
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.scaleTimeSeries = 
function()
{
    # scale.timeSeries - Centers and/or scales a 'timeSeries' object
     
    # Univariate Series:
    set.seed(4711)
    data = cbind(RNORM = round(rnorm(6), 2))
    charvec = timeCalendar()[1:6]
    recordIDs = data.frame(IDs = LETTERS[1:6])
    uTS = timeSeries(data, charvec, recordIDs = recordIDs)
    
    # Multivariate Data Set:
    set.seed(4711)
    data = cbind(round(rnorm(12), 2), round(rt(12, df = 4), 2) )
    charvec = format(timeCalendar(2006))
    mTS = timeSeries(data, charvec, units = c("RNORM", "RT"))
    
    # Scale:
    scale(uTS)
    scale(mTS)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.summaryTimeSeries = 
function()
{
    # summary.timeSeries - Summarizes a 'timeDate' object
     
    # Univariate Series:
    set.seed(4711)
    data = cbind(RNORM = round(rnorm(6), 2))
    charvec = timeCalendar()[1:6]
    recordIDs = data.frame(IDs = LETTERS[1:6])
    uTS = timeSeries(data, charvec, recordIDs = recordIDs)
    
    # Multivariate Data Set:
    set.seed(4711)
    data = cbind(round(rnorm(12), 2), round(rt(12, df = 4), 2) )
    charvec = format(timeCalendar(2006))
    mTS = timeSeries(data, charvec, units = c("RNORM", "RT"))
    
    # Summary:
    summary(uTS)
    summary(mTS)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.varTimeSeries = 
function()
{
    # var.timeSeries - Returns variance for a 'timeSeries' object
     
    # Univariate Series:
    set.seed(4711)
    data = cbind(RNORM = round(rnorm(6), 2))
    charvec = timeCalendar()[1:6]
    recordIDs = data.frame(IDs = LETTERS[1:6])
    uTS = timeSeries(data, charvec, recordIDs = recordIDs)
    
    # Multivariate Data Set:
    set.seed(4711)
    data = cbind(round(rnorm(12), 2), round(rt(12, df = 4), 2) )
    charvec = format(timeCalendar(2006))
    mTS = timeSeries(data, charvec, units = c("RNORM", "RT"))
    
    # Covariance Matrix:
    var(x = uTS, y = NULL, na.rm = FALSE)
    var(x = mTS, y = NULL, na.rm = FALSE)
    
    # Note, using function cov() fails, since cov() requires an atomic
    #   object as input.

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.mathOpsTimeSeries = 
function()
{
    # Ops.timeSeries - Arith method for a 'timeSeries' object
    # abs.timeSeries - Returns abolute values of a 'timeSeries' object
    # sqrt.timeSeries - Returns sqrt values of a 'timeSeries' object
    # exp.timeSeries - Returns exponentials of a 'timeSeries' object
    # log.timeSeries - Returns logarithms of a 'timeSeries' object
    # quantile.timeSeries - produces sample quantiles of a 'timeSeries' object

    # Univariate Series:
    myFinCenter <<- "GMT"
    data = matrix(round(rnorm(12), 2))
    charvec = format(timeCalendar(2006))
    uTS = timeSeries(data, charvec, units = "RNORM")
    uTS
    
    # Multivariate Series:
    data = cbind(round(rnorm(12), 2), round(rt(12, df = 4), 2) )
    charvec = format(timeCalendar(2006))
    mTS = timeSeries(data, charvec, units = c("RNORM", "RT"))
    mTS 
    
    # Univariate Ops:
    uTS < 0
    uTS == abs(uTS)
    
    # Math Operations:
    uTS + 5
    uTS - 5
    100 * uTS
    uTS / 100
    uTS^2
    
    # mathematical Functions:
    log(abs(uTS))    
    sqrt(exp(uTS))
    
    # Quantiles:
    quantile(uTS)
    quantile(uTS, probs = c(0.9, 0.95))
    quantile(uTS, probs = c(0.9, 0.95), type = 5)

    # Logical Operations:
    mTS < 0
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.subsetTimeSeries = 
function()
{
    # [.timeSeries - subsets of a 'timeSeries' object
    # cut.timeSeries - cuts a block from a 'timeSeries' object
    # head.timeSeries - returns the head of a 'timeSeries' object
    # tail.timeSeries - returns the tail of a 'timeSeries' object
    # outlier.timeSeries - Removes outliers from a 'timeSeries' object  

    # Univariate Series:
    myFinCenter <<- "GMT"
    data = matrix(round(rnorm(12), 2))
    charvec = format(timeCalendar(2006))
    uTS = timeSeries(data, charvec, units = "RNORM")
    uTS
    
    # Multivariate Series:
    data = cbind(round(rnorm(12), 2), round(rt(12, df = 4), 2) )
    charvec = format(timeCalendar(2006))
    mTS = timeSeries(data, charvec, units = c("RNORM", "RT"))
    mTS 
    
    # Subsets:
    X = uTS[4:6]
    X
    X@recordIDs 
    X = uTS[4:6, ]
    X
    X@recordIDs
    
    # Head and Tail:
    head(uTS)
    tail(uTS)
    head(mTS)
    tail(mTS)
 
    # Data Subsetting:
    mTS[, 1]            # First Series
    mTS[4:6, 1]         # Second Quarter
  
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.dimOpsTimeSeries = 
function()
{
    # dim - Returns the dimension of a 'timeSeries' object
    # dimnames - Returns the dimension names of a 'timeSeries' object
    # colnames<-.timeS* - Assigns column names to a 'timeSeries' object
    # rownames<-.timeS* - Assigns row names to a 'timeSeries' object
    # is.array.timeSeries - Allows that NCOL and NROW work properly   

    # Univariate Series:
    myFinCenter <<- "GMT"
    data = matrix(round(rnorm(12), 2))
    charvec = format(timeCalendar(2006))
    uTS = timeSeries(data, charvec, units = "RNORM")
    uTS
    
    # Multivariate Series:
    data = cbind(round(rnorm(12), 2), round(rt(12, df = 4), 2) )
    charvec = format(timeCalendar(2006))
    mTS = timeSeries(data, charvec, units = c("RNORM", "RT"))
    mTS 
    
    # Dimension:
    dim(uTS) == c(12, 1)
    dimnames(uTS)
    
    # Column and Rownames:
    # X = uTS
    # colnames(X) = "X" 
    # rownames(X) = as.character(timeCalendar()+24*3600) 
    # X
    # X@Data
    
    # Array:
    is.array(uTS)
    
    # Number of Columns/Rows:
    NCOL(uTS)
    NROW(uTS)   
    ncol(uTS)
    nrow(uTS)
    
    # Return Value:
    return()    
} 
 

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/tests/runit4B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}


################################################################################
   
    