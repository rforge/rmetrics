
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
# FUNCTION:                 TAILORED PLOT FUNCTIONS:     
#  seriesPlot                Returns a time series plot
#  histPlot                  Returns a tailored histogram plot
#  densityPlot               Returns a tailored kernel density estimate plot
#  quantilePlot              Returns a tailored quantile-quantile plot
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(TailoredReturnPlots); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
# TAILORED PLOT FUNCTIONS:


test.tailoredPlots = 
function()
{
    # Series Plot:
    tD = timeSequence(from = "2004-07-01", to = "2005-06-30")
    nD = length(tD)
    tS = timeSeries(cbind(N = rnorm(nD), T = rt(nD, 4)), tD)
    
    par(mfrow = c(2,2), cex = 0.7)
    seriesPlot(tS)
    histPlot(tS)
    
    densityPlot(tS)
    quantilePlot(tS)

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit1B.R")
    printTextProtocol(testResult)
}


# ------------------------------------------------------------------------------

