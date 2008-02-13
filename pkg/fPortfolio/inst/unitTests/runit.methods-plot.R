
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
# FUNCTION:                    EFFICIENT FRONTIER PLOT AND ADDONS:  
#  frontierPlot                 Plots efficient Frontier
#   .minvariancePlot             Adds Minimum Variance point
#   .cmlPlot                     Adds Market Portfolio and Capital Market Line
#   .tangencyPlot                Adds Tangency Portfolio point and line
#   .equalWeightsPlot            Adds point of equal weights portfolio
#   .singleAssetPlot             Adds points of single asset portfolios
#   .twoAssetsPlot               Adds EF for all combinations of two assets
#   .wheelPiePlot                Adds pie chart of weights on EF
#   .monteCarloPlot              Adds randomly produced feasible portfolios
#   .sharpeRatioPlot             Adds Sharpe Ratio
#   .notStackedWeightsPlot       Plots the not stacked weights of potfolio
#   .addlegend                   Adds legend to sliders
# FUNCTION:                    FRONTIER BAR PLOTS:                  
#  weightsPlot                  Plots staggered weights
#  attributesPlot               Plots weighted means
#  covRiskBudgetsPlot           Plots weighted risks
# FUNCTION:                    PORTFOLIO PIE PLOTS:
#  weightsPie                   Plots staggered weights
#  attributesPie                Plots weighted means
#  covRiskBudgetsPie            Plots weighted risks
# FUNCTION:                    DESCRIPTION:
#  covEllipsesPlot              Plots covariance ellipses                
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
    object = Frontier
    frontierPlot(Frontier, pch = 19)
    .minvariancePlot(Frontier, col = "red", pch = 19, cex = 1.5)  
    .tangencyPlot(Frontier, col = "green") 
    .singleAssetPlot(Frontier, col = "red", cex = 1.5)
    .equalWeightsPlot(Frontier, col = "blue", pch = 19, cex = 1.5)
    .twoAssetsPlot(Frontier, col = "grey")
    .weightsWheel(Frontier)
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
    .weightsWheel(Frontier)
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
    .weightsWheel(Frontier)
    .monteCarloPlot(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)  
    .sharpeRatioPlot(Frontier, pch = 19, col = "blue") 

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
    
    # Constraints:
    constraints = "Short"
    
    # Portfolio Weights Plot from Time Series Data:
    Frontier = portfolioFrontier(data, spec, constraints)
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPlot(Frontier)
    attributesPlot(Frontier)
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
    attributesPlot(Frontier)
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
    
    # Constraints:
    constraints = NULL
    
    # Portfolio Weights Plot from Time Series Data:
    Frontier = portfolioFrontier(data, spec, constraints)
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPlot(Frontier)
    attributesPlot(Frontier)
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
    
    # Constraints:
    constraints = "Short"
    
    # Portfolio Weights Plot from Time Series Data:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPie(Portfolio)
    attributesPie(Portfolio)
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
    attributesPie(Portfolio)
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
    constraints = NULL
    
    # Portfolio Weights Plot from Time Series Data:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPie(Portfolio)
    attributesPie(Portfolio)
    covRiskBudgetsPie(Portfolio)
    
    # Return Value:
    return()
}


################################################################################

