
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
# FUNCTION:               BASIC STATISTICS:
#  basicStats              Returns a basic statistics summary
# FUNCTION:               DESCRIPTION:
#  .distCheck              Checks consistency of distributions
# FUNCTION:               SPLUS FUNCTIONALITY:
#  stdev                   S-PLUS: Returns the standard deviation of a vector
################################################################################


################################################################################
# FUNCTION:               TAILORED PLOT FUNCTIONS:     
#  seriesPlot              Returns a tailored return series plot
#  histPlot                Returns a tailored histogram plot
#  densityPlot             Returns a tailored kernel density estimate plot
#  qqnormPlot              Returns a tailored normal quantile-quantile plot


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ReturnSeriesBasics, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.seriesPlot = 
function()
{
    # Time Series:
    tD = timeSequence(from = "2004-07-01", to = "2005-06-30")
    nD = length(tD)
    tS = timeSeries(cbind(N = rnorm(nD), T = rt(nD, 4)), tD)
    
    # Series Plot:
    par(mfrow = c(1, 1))
    seriesPlot(tS) 
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.histPlot = 
function()
{
    # Time Series:
    tD = timeSequence(from = "2004-07-01", to = "2005-06-30")
    nD = length(tD)
    tS = timeSeries(cbind(N = rnorm(nD), T = rt(nD, 4)), tD)
    
    # Histogram Plot:    
    par(mfrow = c(1, 1))
    histPlot(tS)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.densityPlot = 
function()
{    
    # Time Series:
    tD = timeSequence(from = "2004-07-01", to = "2005-06-30")
    nD = length(tD)
    tS = timeSeries(cbind(N = rnorm(nD), T = rt(nD, 4)), tD)
    
    # Density Plot:
    par(mfrow = c(1, 1))
    densityPlot(tS)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.quantilePlot = 
function()
{    
    # Time Series:
    tD = timeSequence(from = "2004-07-01", to = "2005-06-30")
    nD = length(tD)
    tS = timeSeries(cbind(N = rnorm(nD), T = rt(nD, 4)), tD)
      
    # Quantile Plot:
    par(mfrow = c(1, 1))
    quantilePlot(tS)

    # Return Value:
    return()    
}



################################################################################
# FUNCTION:               BASIC STATISTICS:
#  basicStats              Returns a basic statistics summary


test.basicStats = 
function()
{    
    # Time Series:
    tD = timeSequence(from = "2004-07-01", to = "2005-06-30")
    nD = length(tD)
    tS = timeSeries(cbind(N = rnorm(nD), T = rt(nD, 4)), tD)
      
    # timeSeries - basicStats(x, ci = 0.95) 
    basicStats(tS)
    basicStats(as.matrix(tS))
    basicStats(as.data.frame(tS))
    basicStats(as.ts(tS))
     
    # Return Value:
    return()    
}


################################################################################
# FUNCTION:               DESCRIPTION:
#  .distCheck              Checks consistency of distributions


################################################################################
# FUNCTION:               SPLUS FUNCTIONALITY:
#  stdev                   S-PLUS: Returns the standard deviation of a vector


test.basicStats = 
function()
{    
    # Time Series:
    tD = timeSequence(from = "2004-07-01", to = "2005-06-30")
    nD = length(tD)
    tS = timeSeries(cbind(N = rnorm(nD), T = rt(nD, 4)), tD)
      
    # timeSeries - basicStats(x, ci = 0.95) 
    tU = tS[, 1]
    stdev(tU)
    stdev(as.numeric(tU))
    stdev(as.vector(tU))
    stdev(as.ts(tU))
     
    # Return Value:
    return()    
}

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit1A.R")
    printTextProtocol(testResult)
}


################################################################################

