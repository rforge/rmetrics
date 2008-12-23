
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
#  test.feasiblePortfolio.MV.Default
#  test.feasiblePortfolio.MV.RandomWeights.LongOnly
#  test.feasiblePortfolio.MV.Short
#  test.feasiblePortfolio.CVaR.LongOnly.Alpha
################################################################################


test.feasiblePortfolio.Default <- 
    function()
{  
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setWeights(spec) = rep(1/ncol(data), ncol(data))
    spec
    
    # Constraints:
    constraints = "LongOnly"
    
    # Feasible Portfolio:
    portfolio = feasiblePortfolio(data, spec)
    portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
   
 
test.feasiblePortfolio.MV.RandomWeights.LongOnly <- 
    function()
{   
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification: 
    spec = portfolioSpec()
    nAssets = ncol(data)
    Weights = runif(nAssets, 0, 1)
    Weights = round(Weights/sum(Weights), 3)
    setWeights(spec) <- Weights
    setTrace = TRUE
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Feasible Portfolio:
    portfolio = feasiblePortfolio(data, spec)
    portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasiblePortfolio.MV.Short <-  
    function()
{  
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setWeights(spec) = c(-0.2, 0.7, 0.6, -0.1)
    spec
    
    # Constraints:
    constraints = "Short"
    constraints
    
    # Portfolio:
    portfolio = feasiblePortfolio(data, spec, constraints)
    portfolio
    
    # Return Value:
    return()
}


################################################################################


test.feasiblePortfolio.CVaR.LongOnly.Alpha <- 
    function()
{   
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    #   ... Mean-CVaR Portfolio with equal weights
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setWeights(spec) = rep(1/4, 4)
    setAlpha(spec) = 0.05
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio:
    portfolio = feasiblePortfolio(data, spec, constraints)
    portfolio
    
    # Return Value:
    return()
}


################################################################################

