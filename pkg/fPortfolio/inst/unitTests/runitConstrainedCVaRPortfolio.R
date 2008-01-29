
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


test.feasiblePortfolio =
function()
{   
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setWeights(spec) = rep(1/4, 4)
    setAlpha(spec) = 0.05
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio:
    Portfolio = feasiblePortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################


test.efficientPortfolio.SmallCaps = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setAlpha(spec) = 0.05
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio Optimization:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()    
}


################################################################################

   
test.efficientPortfolio.LPP = 
function()
{   
    # Second Example:
    data = 100*as.timeSeries(data(LPP2005REC))[, 1:6]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setAlpha(spec) = 0.05
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio Optimization:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.TwoAssets =
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setAlpha(spec) = 0.05
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}



################################################################################


test.cmlPortfolio = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setAlpha(spec) = 0.05
    setRiskFreeRate(spec) = mean(data@Data)/10
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio:
    Portfolio = cmlPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################


test.tangencyPortfolio = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setAlpha(spec) = 0.10
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Portfolio:
    Portfolio = tangencyPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################


test.minvariancePortfolio =
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setAlpha(spec) = 0.05
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio Optimization:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


################################################################################


test.portfolioFrontier = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setNFrontierPoints(spec) = 10
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio Optimization:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.portfolioFrontier.TwoAssets =
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Return Value:
    return()
}


################################################################################

    