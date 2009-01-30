
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


################################################################################
# FUNCTION:                     SINGLE PORTFOLIOS:
#  feasiblePortfolio             Returns a feasible portfolio
################################################################################


feasiblePortfolio <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented Diethelm Wuertz

    # Description:
    #   Computes Risk and Return for a feasible portfolio

    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL

    # FUNCTION:

    # Transform Data / spec / Constraints:
    Data = portfolioData(data, spec)
    Spec = spec
    Constraints = portfolioConstraints(data, spec, constraints)
    
    # Get Weights:
    if(is.null(getWeights(spec))) {
        stop("Missing weights")
    }
    weights = as.vector(getWeights(spec))
    names(weights) = colnames(getSeries(Data))

    # Compute Return:
    targetReturn = c(
        mean = (Data@statistics$mean %*% weights)[[1]],
        mu = (Data@statistics$mu %*% weights)[[1]])
    setTargetReturn(spec) = targetReturn
    
    # Compute Covariance Risk:
    Cov = Data@statistics$Cov
    cov = sqrt((weights %*% Cov %*% weights)[[1]])
 
    # Check Solver:
    # if (any(constraints@stringConstraints == "Short")) {
    #     setSolver(spec) = "solveRshortExact"
    #     warning("Short Constraints Specified: Solver forced to solveRshortExact")
    # }
    
    # Compute Alternative/Robust Covariance Risk:
    if (getType(spec) == "SPS") {
        funSigma = match.fun(getObjective(spec))
        rcov = funSigma(as.vector(weights))
    } else {
        Sigma = Data@statistics$Sigma
        rcov = sqrt((weights %*% Sigma %*% weights)[[1]])
    }

    # Compute VaR:
    alpha = getAlpha(spec)
    returns = as.matrix(getSeries(Data)) %*% weights
    VaR = quantile(returns, alpha, type = 1)

    # Compute CVaR:
    CVaR = VaR - 0.5*mean(((VaR-returns) + abs(VaR-returns))) / alpha

    # Compose Risks:
    targetRisk = c(cov, rcov, -CVaR, -VaR)
    names(targetRisk) = c("Cov", "Sigma", "CVaR", "VaR")

    # Compute Risk Budgets:
    covRiskBudgets = (weights * Cov %*% weights)[,1] / cov^2
    names(covRiskBudgets) = names(weights)

    # Compose Portfolio:
    Portfolio = new("fPFOLIOVAL",
        portfolio = list(
            weights = weights,
            covRiskBudgets = covRiskBudgets,
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetAlpha = alpha,
            status = getStatus(spec)))

    # Return Value:
    new("fPORTFOLIO",
        call = match.call(),
        data = Data,
        spec = Spec,
        constraints = Constraints,
        portfolio = Portfolio,
        title = "Feasible Portfolio",
        description = description() )
}


################################################################################

