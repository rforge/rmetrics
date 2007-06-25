
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
# FUNCTION:                 TIME SERIES ASSETS PLOTS:
#  assetsPlot                Displays an overview of single assets
#  assetsSeriesPlot          Displays time series of individual assets
#  assetsHistPlot            Displays histograms of individual assets
#  assetsDensityPlot         Displays density plots of individual assets 
#  assetsQQNormPlot          Displays normal qq-plots of individual assets
# FUNCTION:                 DENSITY BOX PLOTS:
#  assetsBoxPlot             Producess standard box plots
#  assetsBoxPercentilePlot   Producess side-by-side box-percentile plots
# FUNCTION:                 BIVARIATE ASSETS PLOTS:
#  assetsPairsPlot           Displays pairs of scatterplots of assets
#  assetsCorTestPlot         Displays and tests pairwise correlations
# FUNCTION:                 BIVARIATE CORRELATION PLOTS:
#  assetsCorgramPlot         Displays correlations between assets
#  assetsCorEigenPlot        Displays ratio of the largest two eigenvalues                 
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


test.assetsPlot =
function()
{ 
    # LPP 2005:
    LPP = 100* as.timeSeries(data(LPP2005REC))
    ans = assetsPlot(LPP[, 1:6], title = "LPP2005")
    ans = assetsPlot(LPP[, 7:9], title = "LPP2005")

    # Berndt Investmwnt Data:
    BI = as.timeSeries(data(berndtInvest))
    ans = assetsPlot(BI[,  1:5], title = "Berndt Invest")
    ans = assetsPlot(BI[, 6:10], title = "Berndt Invest") 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsSeriesPlot =
function()
{ 
    X = as.timeSeries(data(berndtInvest))
    par(mfrow = c(2,2), cex = 0.7)
    par(ask = FALSE)
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
    par(ask = FALSE)
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
    par(ask = FALSE)
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
    par(ask = FALSE)
    assetsQQNormPlot(X, which = c(2, 4, 11, 13))
    
    # Return Value:
    return()
}


################################################################################


test.assetsBoxPlot =
function()
{ 
    # Load Data:
    LPP = as.timeSeries(data(LPP2005REC))
    VAN = as.timeSeries(data(vanIndices))
    
    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    par(ask = FALSE)
    
    # Plot:
    assetsBoxPlot(LPP[, 1:6])
    assetsBoxPlot(LPP[, 1:6], main = "LPP", pch = 19)     
    
    # Plot:
    par(mfrow = c(1, 1), las = 2, oma = c(9, 0, 0, 0))
    assetsBoxPlot(VAN)
    title(main = "Van Hedge Fund Indices")
    par(las = 1)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsQQNormPlot =
function()
{ 
    # Load Data:
    LPP = as.timeSeries(data(LPP2005REC))
    
    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    par(ask = FALSE)
    
    # Plot:
    assetsBoxPercentilePlot(LPP[, 1:6])
    assetsBoxPercentilePlot(LPP[, 1:6], main = "LPP2005")
    
    # Return Value:
    return()
}


################################################################################


test.assetsPairsPlot =
function()
{ 
    X = as.timeSeries(data(berndtInvest))
    
    # Graph Frame:
    par(mfrow = c(2,2), cex = 0.7)
    par(ask = FALSE)
    
    # Plot:
    assetsPairsPlot(X[, c(2, 4, 11, 13)])
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsCorTestPlot =
function()
{ 
    # Load data:
    X = as.timeSeries(data(berndtInvest))
    
    # Graph Frame:
    par(mfrow = c(2,2), cex = 0.7)
    par(ask = FALSE)
    
    # Plot:
    assetsCorTestPlot(X[, c(2, 4, 11, 13)], scale = 0.7)
    
    # Return Value:
    return()
}


################################################################################


test.assetsCorgramPlot =
function()
{ 
    # Pictet Pension Fund Data Sets - Use Percentage Returns:
    LPP = 100 * as.timeSeries(data(LPP2005REC))[, 1:6]
    head(LPP)
    
    # Graph Frame:
    par(mfrow = c(1, 1)) 
    par(ask = FALSE)
    
    # Plot:
    assetsCorgramPlot(LPP) 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsCorEigenPlot =
function()
{ 
    # Pictet Pension Fund Data Sets - Use Percentage Returns:
    LPP = 100 * as.timeSeries(data(LPP2005REC))[, 1:6]
    DJ30 = as.timeSeries(data(DowJones30)))
    
    # Graph Frame:
    par(mfrow = c(1, 1))
    par(ask = FALSE)
    
    # Plot:
    assetsCorEigenPlot(LPP) 
    assetsCorEigenPlot(DJ30) 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsTreePlot =
function()
{ 
    
    # Load Data:
    DJ30 = as.timeSeries(data(DowJones30))[, sample(1:30)]
    LPP2005 = as.timeSeries(data(LPP2005REC))[, sample(1:6)]

    # Graph Frame:
    par(mfrow = c(1, 1))

    # Plot:
    assetsTreePlot(DJ30)
    assetsTreePlot(LPP2005)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


assetsDendogramPlot =
function()
{ 
    # Load Data:
    DJ30 = as.timeSeries(data(DowJones30))[, sample(1:30)]
    LPP2005 = as.timeSeries(data(LPP2005REC))[, sample(1:6)]

    # Graph Frame:
    par(mfrow = c(1, 1))

    # Plot:   
    assetsDendogramPlot(DJ30)
    assetsDendogramPlot(LPP2005)
    
    # Return Value:
    return()
}


################################################################################


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit1B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

