
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
# FUNCTION:                     PORTFOLIO CLASS:
#  efficientPortfolio            Returns a frontier portfolio
#  tangencyPortfolio             Returns the tangency portfolio
#  minvariancePortfolio          Returns the minimum variance portfolio
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
    
    # FUNCTION:
    
    # Check Arguments:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    if (is.null(constraints)) constraints = "LongOnly"
    if (constraints[1] == "Short") setSolver(spec) = "solveRshortExact"
    
    optimize = NA
    if (getOptimize(spec) == "minRisk") {
        optimize = getOptimize(spec)
        if(is.null(getTargetReturn)) {
            stop("Missing target return for minimum risk optimization.")
        } else {
            # Optimize Portfolio:
            Solver = match.fun(getSolver(spec))         
            portfolio = Solver(data, spec, constraints)  
            setWeights(spec) = portfolio$weights
            setStatus(spec) = portfolio$status
            Title = paste(getType(spec), "Risk Minimized Efficient Portfolio")
        }
    } else if (getOptimize(spec) == "maxReturn") {
        optimize = getOptimize(spec)
        if(is.null(getTargetRisk)) {
            stop("Missing target risk for maximum return optimization.")
        } else {
            stop("SCOP solver not yet implemented.")
            # Optimize Portfolio:
            Solver = match.fun(getSolver(spec))         
            portfolio = Solver(data, spec, constraints)  
            setWeights(spec) = portfolio$weights
            setStatus(spec) = portfolio$status
            Title = paste(getType(spec), "Return Maximized Efficient Portfolio")
        }
    } else if (is.na(optimize)) {
        stop("Neither target return nor target risk specified.")
    }
      
    # Compose Portfolio:
    portfolio = feasiblePortfolio(data, spec, constraints)
    portfolio@call = match.call()
    portfolio@title = Title
    
    # Return Value:
    portfolio
}


# ------------------------------------------------------------------------------


tangencyPortfolio <-  
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Capital Market Line
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL
     
    # FUNCTION:
    
    # Check Arguments:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    if (is.null(constraints)) constraints = "LongOnly"
    
    # Compute Sharpe ratio to be minimized:
    sharpeRatio = function(x, data, spec, constraints) 
    {
        # x is the target return ...
        setTargetReturn(spec) = x
        ans = efficientPortfolio(data, spec, constraints)
        sharpeRatio = (x - getRiskFreeRate(spec)) / getTargetRisk(ans)[, "cov"]
        attr(sharpeRatio, "weights") <- getWeights(ans) 
        attr(sharpeRatio, "status") <- getStatus(ans) 
        return(sharpeRatio) 
    }

    # Start Solution - Equal Weights Portfolio:
    nAssets = getNAssets(data)
    setWeights = rep(1/nAssets, times = nAssets)
    setTargetReturn(spec) = 
        getTargetReturn(feasiblePortfolio(data, spec, constraints))
    
    # Minimize Sharp Ratio:
    portfolio = optimize(sharpeRatio, interval = range(getMu(data)), 
        maximum = TRUE, data = data, spec = spec, constraints = constraints)
    setWeights(spec) <- attr(portfolio$objective, "weights")
    setStatus(spec) <- attr(portfolio$objective, "status")
    
    # Compose Portfolio:
    portfolio = feasiblePortfolio(data, spec, constraints)
    portfolio@call = match.call()
    portfolio@title = "Capital Market Line Portfolio"
    
    # Return Value:
    portfolio
}


# ------------------------------------------------------------------------------


minvariancePortfolio <- 
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes minimum variance portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL
    
    # FUNCTION:
    
    # Check Arguments:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    if (is.null(constraints)) constraints = "LongOnly"
    if (constraints == "Short") setSolver(spec) = "solveRshortExact"
    
    # Compute target risk to be minimized:
    targetRisk = function(x, data, spec, constraints) {
        # x is the target return ...
        setTargetReturn(spec) = x
        ans = efficientPortfolio(data, spec, constraints)
        targetRisk = getTargetRisk(ans)[, "cov"]
        attr(targetRisk, "weights") <- getWeights(ans) 
        attr(targetRisk, "status") <- getStatus(ans) 
        return(targetRisk) }

    # Minimize target risk:
    portfolio = optimize(targetRisk, interval = range(getMu(data)),   
        data = data, spec = spec, constraints = constraints)
    setWeights(spec) <- attr(portfolio$objective, "weights")
    setStatus(spec) <- attr(portfolio$objective, "status")
    
    # Compose Portfolio:
    portfolio = feasiblePortfolio(data, spec, constraints)
    portfolio@call = match.call()
    portfolio@title = "Minimum Variance Portfolio"
    
    # Return Value:
    portfolio
}


################################################################################

