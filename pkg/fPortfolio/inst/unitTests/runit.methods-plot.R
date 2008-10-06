
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                    
#  test.plot.fPORTFOLIO  
#  test.frontierPlot.ShortMV
#  test.frontierPlot.ConstrainedMV
#  test.frontierPlot.ConstrainedCVaR
#  test.barPlots.ShortMV
#  test.barPlots.ConstrainedMV
#  test.barPlots.ConstrainedCVaR
#  test.piePlots.ShortMV
#  test.piePlots.ConstrainedMV
#  test.piePlots.ConstrainedCVaR
################################################################################


test.plot.fPORTFOLIO <- 
    function()
{    
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
   
    # Set Default Specifications:
    spec = portfolioSpec()
    setSolver(spec) = "solveRshortExact"
    print(spec)
    
    # Set Constraints:
    constraints = "Short"
    print(constraints)
   
    # Calculation of Long Only Minimum Variance Portfolio:
    frontier = portfolioFrontier(data, spec, constraints)
    print(frontier)
    
    # Plot:
    plot(frontier, which = "all")

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.frontierPlot.ShortMV <- 
    function()
{    
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications:
    spec = portfolioSpec()
    setSolver(spec) = "solveRshortExact"
    spec
    
    # Set Constraints:
    constraints = "Short"
    constraints
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Plot:
    par(mfrow = c(1, 1))
    object = Frontier
    frontierPlot(Frontier, pch = 19)
    minvariancePoints(Frontier, col = "red", pch = 19, cex = 1.5)
    tangencyPoints(Frontier, col = "green")
    tangencyLines(Frontier, col = "green")
    singleAssetPoints(Frontier, col = "red", cex = 1.5)
    equalWeightsPoints(Frontier, col = "blue", pch = 19, cex = 1.5)
    twoAssetsLines(Frontier, col = "grey")
    # .weightsWheel(Frontier)
    monteCarloPoints(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)
    sharpeRatioLines(Frontier, pch = 19, col = "blue")

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
    constraints = "LongOnly"
    constraints
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Plot:
    par(mfrow = c(1, 1))
    frontierPlot(Frontier, pch = 19)
    minvariancePoints(Frontier, col = "red", pch = 19, cex = 1.5)
    tangencyPoints(Frontier, col = "green")
    tangencyLines(Frontier, col = "green")
    singleAssetPoints(Frontier, col = "red", cex = 1.5)
    equalWeightsPoints(Frontier, col = "blue", pch = 19, cex = 1.5)
    twoAssetsLines(Frontier, col = "grey")
    # .weightsWheel(Frontier)
    monteCarloPoints(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)
    sharpeRatioLines(Frontier, pch = 19, col = "blue")

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
    constraints = "LongOnly"
    constraints
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    if (FALSE) {
    
    # Plot:
    par(mfrow = c(1, 1))
    frontierPlot(Frontier, risk = "CVaR", pch = 19)
    minvariancePoints(Frontier, risk = "CVaR", col = "red", pch = 19, cex = 1.5)
    tangencyPoints(Frontier, risk = "CVaR", col = "green")
    tangencyLines(Frontier, risk = "CVaR", col = "green")
    singleAssetPoints(Frontier, risk = "CVaR", col = "red", cex = 1.5)
    equalWeightsPoints(Frontier, risk = "CVaR", col = "blue", pch = 19, cex = 1.5)
    # twoAssetsLines(Frontier, risk = "CVaR", col = "grey")
    # .weightsWheel(Frontier)
    monteCarloPoints(Frontier, risk = "CVaR", mcSteps = 1000, cex = 0.25, pch = 19)
    sharpeRatioLines(Frontier, risk = "CVaR", pch = 19, col = "blue")
    
    } # Plot Problems:

    # Return Value:
    return()
}


################################################################################


test.barPlots.ShortMV =
function()
{ 
    # Load Time Series Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Specification:
    spec = portfolioSpec()
    setSolver(spec) = "solveRshortExact"
    spec
    
    # Constraints:
    constraints = "Short"
    constraints
    
    # Portfolio Weights Plot from Time Series Data:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPlot(Frontier)
    weightedReturnsPlot(Frontier)
    covRiskBudgetsPlot(Frontier)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.barPlots.ConstrainedMV =
function()
{ 
    # Load Time Series Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Specification:
    spec = portfolioSpec()
    
    # Constraints:
    constraints = "LongOnly"
    
    # Portfolio Weights Plot from Time Series Data:
    Frontier = portfolioFrontier(data, spec, constraints)
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPlot(Frontier)
    weightedReturnsPlot(Frontier)
    covRiskBudgetsPlot(Frontier)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.barPlots.ConstrainedCVaR =
function()
{ 
    # Load Time Series Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Portfolio Weights Plot from Time Series Data:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPlot(Frontier)
    weightedReturnsPlot(Frontier)
    covRiskBudgetsPlot(Frontier)
    
    # Return Value:
    return()
}


################################################################################


test.piePlots.ShortMV =
function()
{ 
    # Load Time Series Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Specification:
    spec = portfolioSpec()
    setSolver(spec) = "solveRshortExact"
    spec
    
    # Constraints:
    constraints = "Short"
    constraints
    
    # Portfolio Weights Plot from Time Series Data:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    Portfolio
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPie(Portfolio)
    weightedReturnsPie(Portfolio)
    covRiskBudgetsPie(Portfolio)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.piePlots.ConstrainedMV =
function()
{ 
    # Load Time Series Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Specification:
    spec = portfolioSpec()
    
    # Constraints:
    constraints = "LongOnly"
    
    # Portfolio Weights Plot from Time Series Data:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPie(Portfolio)
    weightedReturnsPie(Portfolio)
    covRiskBudgetsPie(Portfolio)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.piePlots.ConstrainedCVaR =
function()
{ 
    # Load Time Series Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    
    # Constraints:
    constraints = "LongOnly"
    
    # Portfolio Weights Plot from Time Series Data:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPie(Portfolio)
    weightedReturnsPie(Portfolio)
    covRiskBudgetsPie(Portfolio)
    
    # Return Value:
    return()
}


################################################################################

