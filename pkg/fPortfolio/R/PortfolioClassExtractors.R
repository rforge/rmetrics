
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
# FUNCTION:                     PORTFOLIO DATA EXTRACTORS:
#  getData                       Extracts data slot from a 'fPORTFOLIO' object
#   getSeries                    Extracts series from a 'fPORTFOLIO' object
#   getStatistics                Extracts statistics from a 'fPORTFOLIO' object
#   getNumberOfAssets            Extracts number of assets from a 'fPORTFOLIO' object
#  getSpec                      PORTFOLIO SPEC EXTRACTORS:
#   getType
#   getEstimator
#   getParams 
#   getSolver
#   getTrace
#  getConstraints               PORTFOLIO CONSTRAINTS EXTRACTORS:
#  getPortfolio
#   getWeights
#   getTargetReturn
#   getTargetRisk
#   getTargetAlpha
#   getRiskFreeRate
#   getNFrontierPoints
#   getStatus
# FUNCTION:                     GENERAL EXTRACTORS:
#  getFrontier
#  getCovRiskBudgets
#  getTailRiskBudgets
################################################################################


################################################################################
# fPORTFOLIO - S4

    #   call = "call",
    #   data = "list",
    #   specification = "list",
    #   constraints = "character",
    #   portfolio = "list",
    #   title = "character",
    #   description = "character")  


# ------------------------------------------------------------------------------


getData.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts data slot from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    ans = object@data$data
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getSeries.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts series from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    ans = getSeries(getData(object))
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getStatistics.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts statistics from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    ans = getStatistics(getData(object))
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getNumberOfAssets.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts number of assets from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    ans = getNumberOfAssets(getData(object))
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getSpec.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the specification slot from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    ans = object@spec$spec
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getType.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the specification-type slot from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    ans = getType(getSpec(object))
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getEstimator.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the specification-estimator slot from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    ans = getEstimator(getSpec(object))
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getParams.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the specification-params slot from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    ans = getParams(getSpec(object))
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getSolver.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the specification-solver slot from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    ans = getSolver(getSpec(object))
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTrace.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the specification-trace slot from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    ans = getTrace(getSpec(object))
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getConstraints.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the statistics from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@constraints
  
    # Return Value:
    ans  
}


################################################################################


getPortfolio.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the statistics from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@portfolio
  
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getWeights.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the weights from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@portfolio$weights
  
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTargetReturn.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target Return from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@portfolio$targetReturn
  
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTargetRisk.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target Risk from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@portfolio$targetRisk
  
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTargetAlpha.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target Alpha from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@portfolio$targetAlpha
  
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getRiskFreeRate.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the risk free rate from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@spec$riskFreeRate
  
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getNFrontierPoints.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the Number of Frontier Points from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@portfolio$nFrontierPoints
  
    # Return Value:
    ans  
}

# ------------------------------------------------------------------------------


getStatus.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the status from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@portfolio$status
  
    # Return Value:
    ans  
}


################################################################################


getFrontier.fPORTFOLIO =
function(object, frontier = c("both", "lower", "upper"), doplot = FALSE, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the efficient frontier from a 'fPORTFOLO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Settings:
    frontier = match.arg(frontier)
    
    # Get Efficient Frontier:
    Type = getType(object)
    targetRisk = getTargetRisk(object)[ ,"cov"] 
    targetReturn = getTargetReturn(object)[ , "mean"]
   
    ans = cbind(Risk = targetRisk, Return = targetReturn)

    # Extract upper part of frontier
    if(frontier == "upper"){
        index = 1:length(ans[, 1])
        test = c(-1, diff(ans[, 1]))
        index = index[test > 0]
        ans = ans[index, ]
    } else if(frontier == "lower"){
        index = 1:length(ans[, 1])
        test = c(-1, diff(ans[, 1]))
        index = index[test < 0]
        if (length(index) == 1) {
            ans = matrix(ans[index, ], ncol = 2)
        } else {
            ans = ans[index, ]
        }         
    }
    
    # Add colnames:
    colnames(ans) = c("targetRisk", "targetReturn")
  
    # Plot:
    if(doplot) plot(ans, ...)
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


.getCovRiskBudgets.fPORTFOLIO = 
function (object) 
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts risk budgets from a portfolio object
    
    # FUNCTION:
    
    # Covariance Risk Budgets:
    weights = object@portfolio$weights
    ans = NA
    Sigma = object@data$data@statistics$Sigma
    if (is.null(dim(weights))) {
        # Single Portfolio ...
        ans1 = as.vector(weights %*% Sigma %*% weights)
        ans2 = as.vector(weights * Sigma %*% weights)
        ans = round(ans2/ans1, digits = 4)
        names(ans) = names(weights)
    } else {
        # Frontier ...
        Names = colnames(weights)
        ans = NULL
        for (i in 1:(dim(weights)[1])) {
            ans1 = as.vector(weights[i, ] %*% Sigma %*% weights[i, ])
            ans2 = as.vector(weights[i, ] * Sigma %*% weights[i, ])
            ans = rbind(ans, ans2/ans1)
        }
        colnames(ans) = Names
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getCovRiskBudgets.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target Risk from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@portfolio$covRiskBudgets
  
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTailRiskBudgets.fPORTFOLIO = 
function (object) 
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts tail risk budgets from a portfolio object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Check if available:
    Lambda = object@spec$spec@model$tailRisk$lower
    if (is.null(Lambda)) return(NA)
    
    # Tail Risk Budgets:
    weights = getWeights(object)
    ans = NA
    if (is.null(dim(weights))) {
        ans1 = as.vector(weights %*% Lambda %*% weights)
        ans2 = as.vector(weights * Lambda %*% weights)
        ans1 = 1
        ans = round(ans2/ans1, digits = 4)
        names(ans) = names(weights)
    }
    else {
        Names = colnames(weights)
        ans = NULL
        for (i in 1:(dim(weights)[1])) {
            ans1 = as.vector(weights[i, ] %*% Lambda %*% weights[i, ])
            ans2 = as.vector(weights[i, ] * Lambda %*% weights[i, ])
            ans1 = 1
            ans = rbind(ans, ans2/ans1)
        }
        colnames(ans) = Names
    }
    
    # Return Value:
    ans
}


################################################################################

