
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
#  'fPORTFOLIO'                  S4 Portfolio Class
#  portfolioFrontier             Returns the efficient frontier of a portfolio
# FUNCTION:                     SINGLE PORTFOLIOS:
#  feasiblePortfolio             Returns a feasible portfolio
#  efficientPortfolio            Returns a frontier portfolio
#  cmlPortfolio                  Returns capital market line
#  tangencyPortfolio             Returns the tangency portfolio
#  minvariancePortfolio          Returns the minimum variance portfolio
################################################################################


setClass("fPORTFOLIO", 
    representation(
        call = "call",
        data = "list",
        spec = "list",
        constraints = "character",
        portfolio = "list",
        title = "character",
        description = "character")  
)


# ------------------------------------------------------------------------------


portfolioFrontier <- 
    function(data, spec = portfolioSpec(), constraints = NULL, 
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
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }         
    Type = getType(spec)
    fun = match.fun(paste(".portfolio", Model[1], Type[1], "Frontier", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
    # Reset Call:
    ans@call = match.call() 
    
    # Return Value:
    ans   
}


################################################################################


feasiblePortfolio <- 
    function(data, spec = portfolioSpec(), constraints = NULL)
{   
    # A function implemented by Rmetrics
    
    # Description:
    #   Computes Risk and Return for a feasible portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL
    
    # FUNCTION:
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Constraints:
    # .checkPortfolioConstraints
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }         
    Type = getType(spec)
    fun = match.fun(paste(".feasible", Model[1], Type[1], "Portfolio", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
    # Reset Call:
    ans@call = match.call()
    
    # Return Value:
    ans
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
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }         
    Type = getType(spec)
    fun = match.fun(paste(".cml", Model[1], Type[1], "Portfolio", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
    # Reset Call:
    ans@call = match.call()
    
    # Return Value:
    ans
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
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }        
    Type = getType(spec)
    fun = match.fun(paste(".tangency", Model[1], Type[1], "Portfolio", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
    # Reset Call:
    ans@call = match.call() 

    # Return Value:
    ans
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
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }       
    Type = getType(spec)
    fun = match.fun(paste(".minvariance", Model[1], Type[1], "Portfolio", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
    # Reset Call:
    ans@call = match.call() 
    
    # Return Value:
    ans   
}


# ------------------------------------------------------------------------------


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
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }         
    Type = getType(spec)
    fun = match.fun(paste(".efficient", Model[1], Type[1], "Portfolio", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
    # Reset Call:
    ans@call = match.call() 
    
    # Return Value:
    ans   
}


################################################################################

