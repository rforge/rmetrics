
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
# FUNCTION:                    EFFICIENT FRONTIER PLOT AND ADDONS:  
#  frontierPlot                  Plots efficient Frontier
#   .sharpeRatioPlot             Adds Sharpe Ratio
#   .minvariancePlot             Adds Minimum Variance point
#   .cmlPlot                     Adds Market Portfolio and Capital Market Line
#   .tangencyPlot                Adds Tangency Portfolio point and line
#   .equalWeightsPlot            Adds point of equal weights portfolio
#   .singleAssetPlot             Adds points of single asset portfolios
#   .twoAssetsPlot               Adds EF for all combinations of two assets
#   .wheelPiePlot                Adds pie chart of weights on EF
#   .monteCarloPlot              Adds randomly produced feasible portfolios
#   .notStackedWeightsPlot       Plots the not stacked weights of potfolio
#   .addlegend                   Adds legend to sliders
# FUNCTION:                    DESCRIPTION:                  
#  weightsPlot                  Plots staggered weights
#  weightsPie                   Plots staggered weights
#  attributesPlot               Plots weighted means
# FUNCTION:                    DESCRIPTION:
#  covEllipsesPlot              Plots covariance ellipses                    
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PortfolioPlots, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.frontierPlot =
function()
{ 
    # Portfolio Frontier Plot:
    myPF = portfolioFrontier(Data)
    par(mfrow = c(1, 1))
    plot(myPF, which = c(1, 8, 2:6))
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.weightsPlot =
function()
{ 
    # Portfolio Frontier Plot:
    myPF = portfolioFrontier(Data)
    par(mfrow = c(1, 1))
    weightsPlot(myPF)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.weightsPie =
function()
{ 
    # Portfolio Frontier Plot:
    myPF = tangencyPortfolio(Data)
    par(mfrow = c(1, 1))
    weightsPie(myPF)
    title(main = "Weights Pie")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.attributesPlot =
function()
{ 
    # Portfolio Frontier Plot:
    myPF = portfolioFrontier(Data)
    par(mfrow = c(1, 1))
    attributes(myPF)                                # CHECK OUTPUT !!!
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.covEllipsesPlot =
function()
{ 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit2F.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

