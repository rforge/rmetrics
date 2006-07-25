
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
    # mean -
    # var -
    # skewness -
    # kurtosis -
    data(DowJones30)
    IBM = DowJones30[, "IBM"]
    checkIdentical(
        target = class(IBM),
        current = "numeric")
    
    # Mean, Variance:
    mean(IBM)
    var(IBM)
     
    # Skewness, Kurtosis:
    skewness(IBM)
    kurtosis(IBM)
    
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
        
    # Time Series ...
    require(fCalendar)
    data(DowJones30)
    DowJones30 = as.timeSeries(DowJones30)
    DJ = DowJones30[, c("IBM", "JPM")]
    IBM = DowJones30[, "IBM"]
    basicStats(DJ)
    basicStats(IBM)  
    
    # Return Value:
    return()
}
 

# ------------------------------------------------------------------------------

    
test.rowcolStats = 
function()
{
    # Data Frame:
    data(DowJones30)
    DJ = DowJones30[10:20, c("IBM", "JPM")]
    checkIdentical(
        target = class(DJ),
        current = "data.frame")
    
    # Returns - Matrix:
    DJ = diff(log(as.matrix(DJ)))
    class(DJ)
    
    rowAvgs(DJ)
    rowVars(DJ)
    rowStdevs(DJ)
    rowSkewness(DJ)
    rowKurtosis(DJ)
    rowCumsums(DJ)
    
    colAvgs(DJ)
    colVars(DJ)
    colStdevs(DJ)
    colSkewness(DJ)
    colKurtosis(DJ)
    colCumsums(DJ)
    
    
    # Time Series:
    require(fCalendar)
    data(DowJones30)
    DJ = as.timeSeries(DowJones30)[10:20, c("IBM", "JPM")]
    # checkIdentical(
    #    target = class(DJ),
    #    current = "timeSeries")
    
    # Returns - Time Series:
    DJ = returnSeries(DJ)
    class(DJ)
    
    rowAvgs(DJ)
    rowVars(DJ)
    rowStdevs(DJ)
    rowSkewness(DJ)
    rowKurtosis(DJ)
    rowCumsums(DJ)
    
    colAvgs(DJ)
    colVars(DJ)
    colStdevs(DJ)
    colSkewness(DJ)
    colKurtosis(DJ)
    colCumsums(DJ)
    
    # Return Value:
    return()
} 


# ------------------------------------------------------------------------------


if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fBasics/test/runit015A.R")
    printTextProtocol(testResult)
}   
    

################################################################################