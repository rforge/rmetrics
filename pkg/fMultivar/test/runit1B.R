
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
# FUNCTION:                 BENCHMARK ANALYSIS FUNCTIONS:
#  getReturns                Computes return series given a price series
# FUNCTION:                 DRAWDOWNS:
#  maxDrawDown               Computes the maximum drawdown
# FUNCTION:                 PERFORMANCE RATIOS:
#  sharpeRatio               Calculates the Sharpe Ratio
#  sterlingRatio             Calculates the Sterling Ratio
# FUNCTION:                 OHLC PLOT:
#  ohlcPlot                  Creates a Open-High-Low-Close plot
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(BenchmarkAnalysis); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.getReturns =
function()
{
    X = as.timeSeries(MSFT)    
    
    R = getReturns(X)
}


# ------------------------------------------------------------------------------


test.maxDrawDown =
function()
{
    Close = as.timeSeries(MSFT)[,"Close"]
    
    maxDrawDown(Close)  
    
    plot(Close, type = "l")
    abline(v = as.POSIXct("2000-11-09"), lty = 3, col = "red")
    abline(v = as.POSIXct("2000-12-20"), lty = 3, col = "red")
}


# ------------------------------------------------------------------------------


test.Ratio =
function()
{
    X = as.timeSeries(MSFT)    
    R = getReturns(X)
    
    sharpeRatio(R[, "Close"])
    sterlingRatio(R[, "Close"])
}


# ------------------------------------------------------------------------------


test.ohlcPlot =
function()
{
    X = as.timeSeries(MSFT)    
    R = getReturns(X)[1:10,-5]
    
    ohlcPlot(as.ts(R))  # CHECK
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult = runTestFile("C:/Rmetrics/SVN/trunk/fMultivar/test/runit1B.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
