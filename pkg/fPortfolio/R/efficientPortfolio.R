
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
    #   data = as.timeSeries(data(LPP2005REC))[, 1:6]
    #   spec = portfolioSpec(); setTargetReturn(spec) <- mean(data)
    #   efficientPortfolio(data, spec)

    # FUNCTION:

    # Match Spec Versus Constraints:
    # .checkSpecVsConstraints(spec, constraints)

    # Optimize Portfolio:
    Solver = match.fun(getSolver(spec))
    portfolio = Solver(data, spec, constraints)
    
    # Set Parameters:
    # Do not use ...
    # setWeights(spec) = portfolio$weights
    # setTargetReturn(spec) = portfolio$targetReturn
    # setTargetRisk(spec) = portfolio$targetRisk
    # to provide overwriting use:
    spec@portfolio$weights = portfolio$weights
    spec@portfolio$targetReturn = portfolio$targetReturn
    spec@portfolio$targetRisk = portfolio$targetRisk
    
    # Add Status:
    setStatus(spec) = portfolio$status
    
    # Add Title:
    Title = "Efficient Portfolio"

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
    #   data = as.timeSeries(data(LPP2005REC))[, 1:6]
    #   maxratioPortfolio(data)

    # FUNCTION:

    # Match Spec Versus Constraints:
    # .checkSpecVsConstraints(spec, constraints)
    
    # Transform Data:
    Data = portfolioData(data, spec)
    
    # Compute Sharpe ratio to be minimized:
    ratioFun = function(x, data, spec, constraints)
    {
        # x is the target return ...
        setTargetReturn(spec) = x[1]
        Solver = match.fun(getSolver(spec))
        ans = Solver(data, spec, constraints)
        ratio = (x[1] - getRiskFreeRate(spec)) / ans$objective
        attr(ratio, "weights") <- ans$weights
        attr(ratio, "status") <- ans$status
        return(ratio)
    }

    # Start Solution - Equal Weights Portfolio:
    nAssets = getNAssets(Data)
    setWeights(spec) = rep(1/nAssets, times = nAssets)
    fp = feasiblePortfolio(data, spec, constraints)
    setTargetReturn(spec) <- getTargetReturn(fp)
    portfolio = optimize(f = ratioFun, interval = range(getMu(Data)),
        maximum = TRUE, data = data, spec = spec, constraints = constraints)  
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

    # Match Spec Versus Constraints:
    # .checkSpecVsConstraints(spec, constraints)
    
    # Transform Data:
    Data = portfolioData(data, spec)

    # Compute target risk to be minimized:
    targetRiskFun <- function(x, data, spec, constraints) {
        # x is the target return ...
        setTargetReturn(spec) = x[1]
        Solver = match.fun(getSolver(spec))
        ans = Solver(data, spec, constraints)
        targetRisk = ans$objective
        attr(targetRisk, "weights") <- ans$weights
        attr(targetRisk, "status") <- ans$status 
        return(targetRisk) 
    }

    # Minimal Risk:
    portfolio <- optimize(targetRiskFun, interval = range(getMu(Data)),
        data = data, spec = spec, constraints = constraints)
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

    # Match Spec Versus Constraints:
    # .checkSpecVsConstraints(spec, constraints)
    
    # Transform Data:
    data = portfolioData(data, spec)

    # Maximize Return:
    if(is.null(getTargetRisk(spec))) {
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

