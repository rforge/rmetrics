
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
#  efficientPortfolio            Returns a frontier portfolio
#  maxratioPortfolio             Returns the max return/risk ratio portfolio
#  tangencyPortfolio             Returns the tangency portfolio
#  minriskPortfolio              Returns the minimum risk portfolio
#  minvariancePortfolio          Returns the minimum variance portfolio
#  maxreturnPortfolio            Returns the maximum return portfolio
################################################################################


efficientPortfolio <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes target risk and weights for an efficient portfolio

    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL

    # Example:
    #   data = as.timeSeries(data(smallcap.ts))[,c("BKE","GG","GYMB","KRON")]
    #   spec = portfolioSpec(); setTargetReturn(spec) <- mean(colMeans(data))
    #   efficientPortfolio(data, spec)


    # FUNCTION:

    # Check Arguments:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    if (is.null(constraints)) constraints = "LongOnly"
    if (any(constraints == "Short")) setSolver(spec) = "solveRshortExact"

    # Minimize Risk:
    if(is.null(getTargetReturn)) {
        stop("Missing target return for minimum risk optimization.")
    } else {
        # Optimize Portfolio:
        Solver = match.fun(getSolver(spec))
        portfolio = Solver(data, spec, constraints)
        setWeights(spec) = portfolio$weights
        setStatus(spec) = portfolio$status
        Title = "Risk Minimized Efficient Portfolio"
    }

    # Compose Portfolio:
    portfolio = feasiblePortfolio(data, spec, constraints)
    portfolio@call = match.call()
    portfolio@title = Title

    # Return Value:
    portfolio
}


# ------------------------------------------------------------------------------


maxratioPortfolio <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Capital Market Line

    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL

    # Example:
    #   data = as.timeSeries(data(smallcap.ts))[,c("BKE","GG","GYMB","KRON")]
    #   spec = portfolioSpec()
    #   maxratioPortfolio(data, spec)

    # FUNCTION:

    # Check Arguments:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    if (is.null(constraints)) constraints = "LongOnly"

    # Compute Sharpe ratio to be minimized:
    ratioFun = function(x, data, spec, constraints)
    {
        # x is the target return ...
        setTargetReturn(spec) = x
        Solver = match.fun(getSolver(spec))
        ans = Solver(data, spec, constraints)
        ratio = (x - getRiskFreeRate(spec)) / ans$objective
        attr(ratio, "weights") <- ans$weights
        attr(ratio, "status") <- ans$status
        return(ratio)
    }

    # Start Solution - Equal Weights Portfolio:
    nAssets = getNAssets(data)
    setWeights = rep(1/nAssets, times = nAssets)
    setTargetReturn(spec) =
        getTargetReturn(feasiblePortfolio(data, spec, constraints))

    # Minimize Sharp Ratio:
    # YC: scale data to avoid numerical errors in optimize
    scale <- 1000
    optData <- portfolioData(scale * getData(data)$series, spec)
    portfolio = optimize(ratioFun, interval = range(getMu(optData)),
        maximum = TRUE, data = optData, spec = spec, constraints = constraints)
    setWeights(spec) <- attr(portfolio$objective, "weights")
    setStatus(spec) <- attr(portfolio$objective, "status")

    # Compose Portfolio:
    portfolio = feasiblePortfolio(data, spec, constraints)
    portfolio@call = match.call()
    portfolio@title = "Max Return/Risk Ratio Portfolio"

    # Return Value:
    portfolio
}


# ------------------------------------------------------------------------------


tangencyPortfolio <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # Portfolio:
    portfolio = maxratioPortfolio(data, spec, constraints)
    portfolio@title = "Tangency Portfolio"

    # Return Value:
    portfolio
}


################################################################################


minriskPortfolio <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes minimum variance portfolio

    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL

    # Example:
    #   data = as.timeSeries(data(smallcap.ts))[,c("BKE","GG","GYMB","KRON")]
    #   minriskPortfolio(data)

    # FUNCTION:

    # Check Arguments:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    if (is.null(constraints)) constraints = "LongOnly"
    if (any(constraints == "Short")) setSolver(spec) = "solveRshortExact"

    # Compute target risk to be minimized:
    targetRiskFun = function(x, data, spec, constraints) {
        # x is the target return ...
        setTargetReturn(spec) = x
        Solver = match.fun(getSolver(spec))
        ans = Solver(data, spec, constraints)
        targetRisk = ans$objective
        attr(targetRisk, "weights") <- ans$weights
        attr(targetRisk, "status") <- ans$status
        return(targetRisk) }

    # Minimize target risk:
    # YC: scale data to avoid numerical errors in optimize
    scale <- 1000
    optData <- portfolioData(scale * getData(data)$series, spec)
    portfolio <- optimize(targetRiskFun, interval = range(getMu(optData)),
                          data = optData, spec = spec,
                          constraints = constraints)
    setWeights(spec) <- attr(portfolio$objective, "weights")
    setStatus(spec) <- attr(portfolio$objective, "status")

    # Compose Portfolio:
    portfolio = feasiblePortfolio(data, spec, constraints)
    portfolio@call = match.call()
    portfolio@title = "Minimum Risk Portfolio"

    # Return Value:
    portfolio
}


# ------------------------------------------------------------------------------


minvariancePortfolio <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # Portfolio:
    portfolio = minriskPortfolio(data, spec, constraints)
    portfolio@title = "Minimum Variance Portfolio"

    # Return Value:
    portfolio
}


################################################################################


maxreturnPortfolio <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes target risk and weights for an efficient portfolio

    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL

    # FUNCTION:

    # Check Arguments:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    if (is.null(constraints)) constraints = "LongOnly"
    if (any(constraints == "Short")) setSolver(spec) = "solveRshortExact"

    # Maximize Return:
    if(is.null(getTargetRisk)) {
        stop("Missing target risk for maximum return optimization.")
    } else {
        # Optimize Portfolio:
        Solver = match.fun(getSolver(spec))
        portfolio = Solver(data, spec, constraints)
        setWeights(spec) = portfolio$weights
        setStatus(spec) = portfolio$status
        Title = "Return Maximized Efficient Portfolio"
    }

    # Compose Portfolio:
    portfolio = feasiblePortfolio(data, spec, constraints)
    portfolio@call = match.call()
    portfolio@title = Title

    # Return Value:
    portfolio
}


################################################################################

