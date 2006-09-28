
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
# FUNCTION:             SPLUS FUNCTIONALITY:
#  stdev                 Returns the standard deviation of a vector
# FUNCTION:             DESCRIPTION:
#  .distCheck            Checks consistency of distributions
#  .bootMean             Boottraps the population mean
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(BasicStatistics); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

  
test.moments = 
function()
{
    # Data:
    require(fCalendar)
    msft = as.timeSeries(MSFT)[, 1]
    X = returnSeries(msft)
    
    # Mean, Variance:
    mean(X)
    var(X)
     
    # Skewness:
    skewness(X)
    skewness(X, method = "moment")
    skewness(X, method = "fisher")
    
    # Kurtosis:
    kurtosis(X)
    kurtosis(X, method = "excess")
    kurtosis(X, method = "moment")
    kurtosis(X, method = "fisher")
    
    # Return Value:
    return()
}
     

# ------------------------------------------------------------------------------

    
test.basicStats = 
function()
{
    # methods(as.matrix)
    # as.matrix.data.frame as.matrix.default    as.matrix.dist*     
    # as.matrix.noquote    as.matrix.POSIXlt  

    # Univariate Data:
    set.seed(1953)
    N = 10
    X = rnorm(N)
    tsX = as.ts(X)
    dfX = data.frame(X)
    
    # Multivariate Data:
    Y = rnorm(N)
    Z = cbind(X, Y)
    tsZ = as.ts(Z)
    dfZ = data.frame(Z)
    
    # Univariate Data:
    class(X)
    NUM = basicStats(X)
    class(tsX)
    TS = basicStats(tsX)
    class(dfX)
    DF = basicStats(dfX)
    checkEqualsNumeric(
        target = NUM,
        current = TS)
    checkEqualsNumeric(
        target = TS,
        current = DF)
    checkEqualsNumeric(
        target = DF,
        current = NUM)
    
    # Multivariate Data:
    class(Z)
    NUM = basicStats(Z)
    class(tsZ)
    TS = basicStats(tsZ)
    class(dfZ)
    DF = basicStats(dfZ)
    checkEqualsNumeric(
        target = NUM,
        current = TS)
    checkEqualsNumeric(
        target = TS,
        current = DF)
    checkEqualsNumeric(
        target = DF,
        current = NUM)
    
    # Return Value:
    return()
}
 

# ------------------------------------------------------------------------------

    
test.rowcolStats = 
function()
{
    # Data Frame:
    checkIdentical(target = class(MSFT), current = "data.frame")
    
    # Returns - Matrix:
    msft.mat = diff(log(as.matrix(MSFT)))[1:11, ]
    
    test = rowAvgs(msft.mat)
    print(test)
    target = round(sum(test), 4)
    target
    checkSum = -0.1012
    checkEqualsNumeric(target, checkSum)
    
    test = rowVars(msft.mat)
    print(test)
    target = round(sum(test), 4)
    target
    checkSum = 0.2811
    checkEqualsNumeric(target, checkSum)
    
    test = rowStdevs(msft.mat)
    print(test)
    target = round(sum(test), 4)
    target
    checkSum = 1.5185
    checkEqualsNumeric(target, checkSum)
    
    test = rowSkewness(msft.mat)
    print(test)
    target = round(sum(test), 4)
    target
    checkSum =  -0.7531
    checkEqualsNumeric(target, checkSum)
    
    test = rowKurtosis(msft.mat)
    print(test)
    target = round(sum(test), 4)
    target
    checkSum = -4.0053
    checkEqualsNumeric(target, checkSum)
    
    test = rowCumsums(msft.mat)
    print(test)
    target = round(sum(test), 4)
    target
    checkSum = -7.471
    checkEqualsNumeric(target, checkSum)
    
    # Columnwise:
    test = colAvgs(msft.mat)
    test = colVars(msft.mat)
    test = colStdevs(msft.mat)
    test = colSkewness(msft.mat)
    test = colKurtosis(msft.mat)
    test = colCumsums(msft.mat)
    
    
    # Time Series:
    msft.ret = returnSeries(as.timeSeries(MSFT))[1:11, ]
    
    rowAvgs(msft.ret)
    rowVars(msft.ret)
    rowStdevs(msft.ret)
    rowSkewness(msft.ret)
    rowKurtosis(msft.ret)
    rowCumsums(msft.ret)
    
    colAvgs(msft.ret)
    colVars(msft.ret)
    colStdevs(msft.ret)
    colSkewness(msft.ret)
    colKurtosis(msft.ret)
    colCumsums(msft.ret)
    
    # Return Value:
    return()
} 


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit3A.R")
    printTextProtocol(testResult)
}   
    

################################################################################