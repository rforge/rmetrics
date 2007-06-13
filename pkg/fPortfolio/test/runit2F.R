
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
#  frontierPlot                 Plots efficient Frontier
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
#  attributesPie                Plots weighted means
#  riskBudgetsPlot              Plots weighted risks
#  riskBudgetsPie               Plots weighted risks
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


################################################################################


test.frontierPlot.ShortMV =
function()
{    
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications:
    spec = portfolioSpec()
    spec
    
    # Set Constraints:
    constraints = "Short"
    constraints
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Plot:
    par(mfrow = c(1, 1))
    frontierPlot(Frontier, pch = 19)
    .minvariancePlot(Frontier, col = "red", pch = 19, cex = 1.5)  
    .tangencyPlot(Frontier, col = "green") 
    .singleAssetPlot(Frontier, col = "red", cex = 1.5)
    .equalWeightsPlot(Frontier, col = "blue", pch = 19, cex = 1.5)
    .twoAssetsPlot(Frontier, col = "grey")
    .wheelPiePlot(Frontier)
    .monteCarloPlot(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)  
    .sharpeRatioPlot(Frontier, pch = 19, col = "blue") 

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.frontierPlot.ConstrainedMV =
function()
{    
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications:
    spec = portfolioSpec()
    spec
    
    # Set Constraints:
    constraints = NULL
    constraints
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Plot:
    par(mfrow = c(1, 1))
    frontierPlot(Frontier, pch = 19)
    .minvariancePlot(Frontier, col = "red", pch = 19, cex = 1.5)  
    .tangencyPlot(Frontier, col = "green") 
    .singleAssetPlot(Frontier, col = "red", cex = 1.5)
    .equalWeightsPlot(Frontier, col = "blue", pch = 19, cex = 1.5)
    .twoAssetsPlot(Frontier, col = "grey")
    .wheelPiePlot(Frontier)
    .monteCarloPlot(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)  
    .sharpeRatioPlot(Frontier, pch = 19, col = "blue") 

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.frontierPlot.ConstrainedCVaR =
function()
{    
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    spec
    
    # Set Constraints:
    constraints = NULL
    constraints
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Plot:
    par(mfrow = c(1, 1))
    frontierPlot(Frontier, pch = 19)
    .minvariancePlot(Frontier, col = "red", pch = 19, cex = 1.5)  
    .tangencyPlot(Frontier, col = "green") 
    .singleAssetPlot(Frontier, col = "red", cex = 1.5)
    .equalWeightsPlot(Frontier, col = "blue", pch = 19, cex = 1.5)
    .twoAssetsPlot(Frontier, col = "grey")
    .wheelPiePlot(Frontier)
    .monteCarloPlot(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)  
    .sharpeRatioPlot(Frontier, pch = 19, col = "blue") 

    # Return Value:
    return()
}


################################################################################


test.weightsPlot.ShortMV =
function()
{ 
    # Load Time Series Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # Constraints:
    constraints = "Short"
    constraints
    
    # Portfolio Weights Plot from Time Series Data:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Plot:
    par(mfrow = c(1, 1))
    weightsPlot(Frontier)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.weightsPlot.ConstrainedMV =
function()
{ 
    # Load Time Series Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Portfolio Weights Plot from Time Series Data:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Plot:
    par(mfrow = c(1, 1))
    weightsPlot(Frontier)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.weightsPlot.ConstrainedCVaR =
function()
{ 
    # Load Time Series Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setType = "CVaR"
    spec
    
    # Constraints:
    constraints = NULL
    constraints
    
    # Portfolio Weights Plot from Time Series Data:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Plot:
    par(mfrow = c(1, 1))
    weightsPlot(Frontier)
    
    # Return Value:
    return()
}


################################################################################


test.weightsPie =
function()
{ 
    # Load Time Series Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Portfolio Weights Pie from Time Series Data:
    myPF = tangencyPortfolio(Data)
    par(mfrow = c(1, 1))
    weightsPie(myPF)
    title(main = "Weights Pie")
    
    # Portfolio Weights Pie from Statistics Data:
    Statistics = portfolioData(Data)$statistics
    myPF = tangencyPortfolio(Statistics)
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
    # Load Time Series Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Portfolio Attributes Plot from Time Series Data:
    myPF = portfolioFrontier(Data)
    par(mfrow = c(1, 1))
    attributesPlot(myPF)  
    
    # Portfolio Attributes Plot from Statistics Data:
    Statistics = portfolioData(Data)$statistics
    myPF = portfolioFrontier(Statistics)
    par(mfrow = c(1, 1))
    attributesPlot(myPF)                             
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.covEllipsesPlot =
function()
{ 
    # Input must be a list of at least 2 covariance matrices!
    
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

