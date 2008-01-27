
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                     PORTFOLIO CLASS:
#  efficientPortfolio            Returns a frontier portfolio
#  cmlPortfolio                  Returns capital market line
#  tangencyPortfolio             Returns the tangency portfolio
#  minvariancePortfolio          Returns the minimum variance portfolio
################################################################################


efficientPortfolio <- 
    function(data, spec = portfolioSpec(), constraints = NULL)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Computes target risk and weights for an efficient portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL
    
    # FUNCTION:
    
    # Check Data:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    if (is.null(constraints)) constraints = "LongOnly"
    
    # Optimize Portfolio:
    Solver = match.fun(getSolver(spec))         
    portfolio = Solver(data, spec, constraints)  
    setWeights(spec) = portfolio$weights
    setStatus(spec) = portfolio$status
      
    # Compose Portfolio:
    portfolio = feasiblePortfolio(data, spec, constraints)
    portfolio@call = match.call()
    portfolio@title = "Efficient Portfolio"
    
    # Return Value:
    portfolio
}


# ------------------------------------------------------------------------------


cmlPortfolio <-  
    function(data, spec = portfolioSpec(), constraints = NULL)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Computes Capital Market Line
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL
     
    # FUNCTION:
    
    # Check Data:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    if (is.null(constraints)) constraints = "LongOnly"
    
    # Compute Sharpe ratio to be minimized:
    sharpeRatio = function(x, data, spec, constraints) {
        # x is the target return ...
        setTargetReturn(spec) = x
        ans = efficientPortfolio(data, spec, constraints)
        sharpeRatio = (x - getRiskFreeRate(spec)) / getTargetRisk(ans)[, "cov"]
        attr(sharpeRatio, "weights") <- getWeights(ans) 
        attr(sharpeRatio, "status") <- getStatus(ans) 
        return(sharpeRatio) }

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


tangencyPortfolio <- 
    function(data, spec = portfolioSpec(), constraints = NULL)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Computes target risk and weights for the tangency portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL
     
    # FUNCTION:
    
    # Check Data:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    if (is.null(constraints)) constraints = "LongOnly"
    
    # Set zero risk free rate:
    setRiskFreeRate(spec) = 0
   
    # Compose Portfolio:
    portfolio = cmlPortfolio(data, spec, constraints)
    portfolio@call = match.call()
    portfolio@title = "Tangency Portfolio"
  
    # Return Value:
    portfolio
}


# ------------------------------------------------------------------------------


minvariancePortfolio <- 
    function(data, spec = portfolioSpec(), constraints = NULL)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Computes minimum variance portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL
    
    # FUNCTION:
    
    # Check Data:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    if (is.null(constraints)) constraints = "LongOnly"
    
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

