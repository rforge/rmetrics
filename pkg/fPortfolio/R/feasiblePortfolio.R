
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

    # Transform Data and Constraints:
    data = portfolioData(data, spec)
    constraints = portfolioConstraints(data, spec, constraints)
    
    # Check Solver:
    if (any(constraints@stringConstraints == "Short"))
        setSolver(spec) = "solveRshortExact"

    # Get Weights:
    if(is.null(getWeights(spec))) {
        nAssets = getNAssets(data)
        setWeights(spec) = rep(1/nAssets, times = nAssets)
    }
    weights = as.vector(getWeights(spec))
    names(weights) = colnames(data@data$series)

    # Compute Return:
    targetReturn = c(
        mean = (data@statistics$mean %*% weights)[[1]],
        mu = (data@statistics$mu %*% weights)[[1]])

    # Compute Covariance Risk:
    Cov = data@statistics$Cov
    cov = sqrt((weights %*% Cov %*% weights)[[1]])

    # Compute Alternative/Robust Covariance Risk:
    if (getType(spec) == "SPS") {
        funSigma = match.fun(getObjective(spec))
        rcov = funSigma(as.vector(weights))
    } else {
        Sigma = data@statistics$Sigma
        rcov = sqrt((weights %*% Sigma %*% weights)[[1]])
    }

    # Compute VaR:
    alpha = getAlpha(spec)
    returns = as.matrix(data@data$series) %*% weights
    VaR = quantile(returns, alpha, type = 1)

    # Compute CVaR:
    CVaR = VaR - 0.5*mean(((VaR-returns) + abs(VaR-returns))) / alpha

    # Compose Risks:
    targetRisk = c(cov, rcov, -CVaR, -VaR)
    names(targetRisk) = c("Cov", "Sigma", "CVaR", "VaR")

    # Compute Risk Budgets:
    covRiskBudgets = (weights * Cov %*% weights)[,1] / cov^2

    # Compose Portfolio:
    portfolio = list(
        weights = t(weights),
        targetReturn = t(targetReturn),
        targetRisk = t(targetRisk),
        targetAlpha = alpha,
        covRiskBudgets = t(covRiskBudgets),
        status = getStatus(spec))

    # Return Value:
    new("fPORTFOLIO",
        call = match.call(),
        data = list(data = data),
        spec = list(spec = spec),
        constraints = constraints@stringConstraints,
        portfolio = portfolio,
        title = "Feasible Portfolio",
        description = description() )
}


################################################################################

