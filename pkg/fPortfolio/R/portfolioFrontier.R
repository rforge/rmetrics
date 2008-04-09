
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port: 
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                     DESCRIPTION:
#  portfolioFrontier             Returns the efficient frontier of a portfolio
################################################################################


portfolioFrontier <- 
    function(data, spec = portfolioSpec(), constraints = "LongOnly", 
    title = NULL, description = NULL)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Computes the efficient frontier of a portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL
    
    # FUNCTION:
    
    # Create Data Object:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)

    # Optimize portfolios along the frontier:
    nFrontierPoints = getNFrontierPoints(spec)
    mu = getMu(data)
    eps = 1e-8
    targetReturns <- 
        seq(min(mu)*(1+eps), max(mu)*(1-eps), length = nFrontierPoints)
    weights = targetReturn = targetRisk = covRiskBudgets = status = NULL
    for (i in 1:nFrontierPoints) {
        setTargetReturn(spec) = targetReturns[i]
        portfolio = efficientPortfolio(data, spec, constraints)
        if (getStatus(portfolio) == 0) {
            weights = rbind(weights, getWeights(portfolio))
            targetReturn = rbind(targetReturn, getTargetReturn(portfolio))
            targetRisk = rbind(targetRisk, getTargetRisk(portfolio))
            covRiskBudgets = rbind(covRiskBudgets, getCovRiskBudgets(portfolio))
        }
    }
    setTargetReturn(spec) <- NULL
  
    # Compose Frontier:
    portfolio@call = match.call()
    portfolio@portfolio$weights  = weights
    portfolio@portfolio$targetReturn = targetReturn
    portfolio@portfolio$targetRisk = targetRisk
    portfolio@portfolio$covRiskBudgets = covRiskBudgets
    portfolio@portfolio$status = 0
    portfolio@title = "Portfolio Frontier"    
    
    # Return Value:
    portfolio   
}


################################################################################

