
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
#  test.frontierPoints.feasiblePortfolio 
#  test.frontierPoints.portfolioFrontier               
################################################################################


test.frontierPoints.feasiblePortfolio <- 
    function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    # Specification:
    #   ... use defaults, Mean-Variance with Equal Weights
    spec = portfolioSpec()
    print(spec)
    
    # Constraints:
    #   ... use defaults, Long Only
    constraints = "LongOnly"
    print(constraints)
    
    # Feasible Portfolio:
    #   ... use defaults for spec and constraints 
    portfolio = feasiblePortfolio(data)
    print(portfolio)
    
    # Frontier Points:
    #   ... returns target risk and target return
    points = frontierPoints(portfolio)
    print(points)
    
    # Specify Return/Risk Measures, explicitely:
    print(frontierPoints(portfolio, auto = TRUE))
    print(frontierPoints(portfolio, 
        risk = "Cov", auto = FALSE))
    print(frontierPoints(portfolio, 
        risk = "Sigma", auto = FALSE))
    print(frontierPoints(portfolio, 
        risk = "CVaR", auto = FALSE))
    print(frontierPoints(portfolio, 
        risk = "VaR", auto = FALSE))
    print(frontierPoints(portfolio, 
        return = "mu", risk = "CVaR", auto = FALSE))
  
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.frontierPoints.portfolioFrontier <- 
    function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    # Specification:
    #   ... use defaults, Mean-Variance with Equal Weights
    spec = portfolioSpec()
    print(spec)
    
    # Constraints:
    #   ... use defaults, Long Only
    constraints = "LongOnly"
    print(constraints)
    
    # Feasible Portfolio:
    #   ... use defaults for spec and constraints 
    portfolio = portfolioFrontier(data)
    print(portfolio)
    
    # Frontier Points:
    #   ... returns target risk and target return
    points = frontierPoints(portfolio)
    print(points)
    
    # Specify Return/Risk Measures, explicitely:
    print(frontierPoints(portfolio, auto = TRUE))
    print(frontierPoints(portfolio, 
        return = "mean", risk = "Cov", auto = FALSE))
    print(frontierPoints(portfolio, 
        return = "mean", risk = "Sigma", auto = FALSE))
    print(frontierPoints(portfolio, 
        return = "mean", risk = "CVaR", auto = FALSE))
    print(frontierPoints(portfolio, 
        return = "mean", risk = "VaR", auto = FALSE))
    
    # Return Value:
    return()
}


################################################################################

