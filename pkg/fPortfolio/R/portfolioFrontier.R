
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
    
    # Example:
    #   data = as.timeSeries(data(smallcap.ts))
    #   data = data[, c("BKE", "GG", "GYMB", "KRON")] 
    #   spec = portfolioSpec(); setNFrontierPoints(spec) = 20
    #   portfolioFrontier(data, spec)
    #   setSolver(spec) = "solveRdonlp2"; portfolioFrontier(data, spec)
    
    # FUNCTION:
    
    # Transform Data and Constraints:
    data = portfolioData(data, spec)
    constraints = portfolioConstraints(data, spec, constraints)
    
    # Check Solver:
    if (any(constraints@stringConstraints == "Short"))
        setSolver(spec) = "solveRshortExact"

    # Optimize portfolios along the frontier:
    nFrontierPoints = getNFrontierPoints(spec)
    mu = getMu(data)
    eps = 1e-6
    minMu = min(mu)*(1+eps)
    maxMu = max(mu)*(1-eps)
    targetReturns <- seq(minMu, maxMu, length = nFrontierPoints)
    
    # NEW Version:
    #   The Idea is to start from the minvariance portfolio and to explore
    #   the efficient frontier and the minimum variance locus starting from
    #   this point ...
    #   Then we stop when the status flag fails ...
    
    # Compute minvariancePortfolio:
    mvPortfolio = minvariancePortfolio(data, spec, constraints)
    mvReturn = getTargetReturn(mvPortfolio)[, "mean"]
    minIndex = which.min(abs(mvReturn-targetReturns))
    
    # Upper Frontier Part:
    Status = 0
    IDX = minIndex
    weights = targetReturn = targetRisk = covRiskBudgets = NULL
    while (Status == 0 & IDX <= nFrontierPoints) {
        setTargetReturn(spec) = targetReturns[IDX]
        portfolio = efficientPortfolio(data, spec, constraints)
        Status = getStatus(portfolio)
        if (Status == 0) {
            weights = rbind(weights, getWeights(portfolio))
            targetReturn = rbind(targetReturn, getTargetReturn(portfolio))
            targetRisk = rbind(targetRisk, getTargetRisk(portfolio))
            covRiskBudgets = rbind(covRiskBudgets, getCovRiskBudgets(portfolio))
        }
        IDX = IDX + 1
    }
    
    # Lower Min Variance Locus:
    if (minIndex > 1) {
        weights2 = targetReturn2 = targetRisk2 = covRiskBudgets2 = NULL
        Status = 0
        IDX = minIndex - 1
        while (Status == 0 & IDX > 0) {
            setTargetReturn(spec) = targetReturns[IDX]
            portfolio = efficientPortfolio(data, spec, constraints)
            Status = getStatus(portfolio)
            if (Status == 0) {
                weights2 = 
                    rbind(getWeights(portfolio), weights2)
                targetReturn2 = 
                    rbind(getTargetReturn(portfolio), targetReturn2)
                targetRisk2 = 
                    rbind(getTargetRisk(portfolio), targetRisk2)
                covRiskBudgets2 = 
                    rbind(getCovRiskBudgets(portfolio), covRiskBudgets2)
            }
            IDX = IDX - 1
        }
        weights = rbind(weights2, weights)
        targetReturn = rbind(targetReturn2, targetReturn)
        targetRisk = rbind(targetRisk2, targetRisk)
        covRiskBudgets = rbind(covRiskBudgets2, covRiskBudgets)
    }  
    
    # Reset Target Return:  
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

