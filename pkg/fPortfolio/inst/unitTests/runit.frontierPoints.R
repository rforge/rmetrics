
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
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    # Specification:
    spec = portfolioSpec()
    setWeights(spec) = rep(1/ncol(data), ncol(data))
    print(spec)
    
    # Constraints:
    constraints = "LongOnly"
    print(constraints)
    
    # Feasible Portfolio:
    portfolio = feasiblePortfolio(data, spec, constraints)
    print(portfolio)
    
    # Frontier Points:
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
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    # Specification:
    spec = portfolioSpec()
    print(spec)
    
    # Constraints:
    constraints = "LongOnly"
    print(constraints)
    
    # Portfolio Frontier:
    frontier = portfolioFrontier(data)
    print(frontier)
    
    # Frontier Points:
    points = frontierPoints(frontier)
    print(points)
    
    # Specify Return/Risk Measures, explicitely:
    print(frontierPoints(frontier, auto = TRUE))
    print(frontierPoints(frontier, 
        return = "mean", risk = "Cov", auto = FALSE))
    print(frontierPoints(frontier, 
        return = "mean", risk = "Sigma", auto = FALSE))
    print(frontierPoints(frontier, 
        return = "mean", risk = "CVaR", auto = FALSE))
    print(frontierPoints(frontier, 
        return = "mean", risk = "VaR", auto = FALSE))
    
    # Return Value:
    return()
}


################################################################################

