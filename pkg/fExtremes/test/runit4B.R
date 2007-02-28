
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
# FUNCTION          EXPLORATIVE DATA ANALYSIS:
#  emdPlot           Creates an empirical distribution plot
#  qqPlot            Creates a normal quantile-quantile plot
#  qqbayesPlot       Creates a normal qq-Plot with confidence intervals
#  qPlot             Creates exploratory QQ plot for EV analysis
#  mePlot            Creates a sample mean excess plot
#   mxfPlot           Creates another view of a sample mean excess plot
#   mrlPlot           Returns a mean residual life plot with confidence levels
#  recordsPlot       Plots records development
#   ssrecordsPlot     Plots records development of data subsamples
#  msratioPlot       Plots ratio of maximums and sums
#  sllnPlot          Verifies Kolmogorov's Strong Law of large numbers
#  lilPlot           Verifies Hartman-Wintner's Law of the iterated logarithm
#  xacfPlot          Plots autocorrelations of exceedences
# FUNCTION:         PLOT UTILITIES:
#  interactivePlot   Plots several graphs interactively
#  gridVector        Creates from two vectors rectangular grid points
# FUNCTION          DATA PREPROCESSING:
#  blockMaxima       Returns block maxima from a time series
#  findThreshold     Upper threshold for a given number of extremes 
#  pointProcess      Returns peaks over a threshold from a time series
#  deCluster         Declusters a point process
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(DaylightSavingTime, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
# EXPLORATIVE DATA ANALYSIS:


test.eda = 
function()
{
    # Artificial Data Set:
    x = rt(1000, df = 3)
    
    # Empirical distribution plot:
    emdPlot(x)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit2A.R")
    printTextProtocol(testResult)
}


################################################################################

