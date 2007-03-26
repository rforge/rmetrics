
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
# FUNCTION:             TIME SERIES ASSETS PLOTS:
#  assetsSeriesPlot      Displays time series of individual assets
#  assetsHistPlot        Displays histograms of individual assets
#  assetsDensityPlot     Displays density plots of individual assets 
#  assetsQQNormPlot      Displays normal qq-plots of individual assets
# FUNCTION:             BIVARIATE ASSETS PLOTS:
#  assetsPairsPlot       Displays pairs of scatterplots of individual assets
#  assetsCorTestPlot     Displays and tests pairwise correlations of assets                    
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(AssetsPlots, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.assetsSeriesPlot =
function()
{ 
    X = as.timeSeries(data(berndtInvest))
    par(mfrow = c(2,2), cex = 0.7)
    assetsSeriesPlot(X, which = c(2, 4, 11, 13))
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsHistPlot =
function()
{ 
    X = as.timeSeries(data(berndtInvest))
    par(mfrow = c(2,2), cex = 0.7)
    assetsHistPlot(X, which = c(2, 4, 11, 13))                    # CHECK - main
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsDensityPlot =
function()
{ 
    X = as.timeSeries(data(berndtInvest))
    par(mfrow = c(2,2), cex = 0.7)
    # assetsDensityPlot(X, which = c(2, 4, 11, 13))                # CHECK fails    
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsQQNormPlot =
function()
{ 
    X = as.timeSeries(data(berndtInvest))
    par(mfrow = c(2,2), cex = 0.7)
    assetsQQNormPlot(X, which = c(2, 4, 11, 13))
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsPairsPlot =
function()
{ 
    X = as.timeSeries(data(berndtInvest))
    par(mfrow = c(2,2), cex = 0.7)
    assetsPairsPlot(X[, c(2, 4, 11, 13)])
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsCorTestPlot =
function()
{ 
    X = as.timeSeries(data(berndtInvest))
    par(mfrow = c(2,2), cex = 0.7)
    assetsCorTestPlot(X[, c(2, 4, 11, 13)], scale = 0.7)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit1B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

