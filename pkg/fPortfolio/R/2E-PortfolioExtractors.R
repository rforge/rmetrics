
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
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM DATA SLOT:
#  getAssets                     Extracts assets series data, if available
#  getStatistics                 Extracts assets statistics, mean and covariance
#  getNumberOfAssets             Extracts number of assets from statistics
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM SPECIFICATION SLOT:
#  getSpecification              Extracts @specification Slot
#  getType                       Extract portfolio type from specification
#  getSolver                     Extract solver from specification
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM PORTFOLIO SLOT:
#  getPortfolio                  Extracts @portfolio Slot
#  getFrontier                   Extracts the efficient frontier
#  getWeights                    Extracts weights from a portfolio object
#  getRiskBudgets                Extracts risk budgets from a portfolio object
#  getTailRiskBudgets            Extracts tail risk budgets from a portfolio
#  getTargetReturn               Extracts target return from a portfolio
#  getTargetRisk                 Extracts target riks from a portfolio
#  getTargetAlpha                Extracts target VaR-alpha from a portfolio
#  getTargetStdev                Extracts target std deviations from a portfolio
#  getNames                      Extracts assets names from a portfolio
################################################################################


getAssets =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the efficient frontier from a 'fPORTFOLO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
 
    # Get Series of Assets:
    ans = object@data$series
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getStatistics =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the efficient frontier from a 'fPORTFOLO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Series of Assets
    ans = object@data$statistics
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getNumberOfAssets =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the efficient frontier from a 'fPORTFOLO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Series of Assets
    ans = c(nAssets = length(object@data$statistics$mu))
    
    # Return Value:
    ans  
}


################################################################################


getSpecification =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the specification structure from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    ans = object@specification$spec
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getType =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the type from specification of a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Type:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@model$type[1]
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@specification$spec@model$type[1]
    }
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getSolver =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the solver from specification of a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Solver:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@solver$type[1]
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@specification$spec@solver$type[1]
    }
    
    # Return Value:
    ans  
}



################################################################################


getPortfolio =
function(object, doplot = FALSE, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the statistics from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@portfolio
    
    # Plot:
    if (doplot) plot(object, which = c(1, 3, 4, 5, 6), ...)
  
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getFrontier =
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
    targetRisk = getTargetRisk(object)
    targetReturn = getTargetReturn(object)
    
    if (Type == "MV") {
        ans = cbind(Risk = targetRisk, Return = targetReturn)
    } else if (Type == "CVaR") {
        if (is.matrix(targetRisk)) {
            Risk = targetRisk[, 1]
        } else {
            Risk = targetRisk[1]
        }
        ans = cbind(Risk = Risk, Return = targetReturn)
    }
    rownames(ans) = NULL

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


getWeights =
function(object, doplot = FALSE, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the weights from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Weights:
    ans = object@portfolio$weights
    
    # Plot:
    if (doplot) weightsPlot(object, ...)
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getRiskBudgets = 
function (object) 
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts risk budgets from a portfolio object
    
    # FUNCTION:
    
    # Covariance Risk Budgets:
    weights = getWeights(object)
    ans = NA
    Sigma = getStatistics(object)$Sigma
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


getTailRiskBudgets = 
function (object) 
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts tail risk budgets from a portfolio object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Check if available:
    Lambda = object@data$tailrisk
    if (is.na(Lambda)) return(NA)
    
    # Tail Risk Budgets:
    weights = getWeights(object)
    ans = NA
    if (is.null(dim(weights))) {
        ans1 = as.vector(weights %*% Lambda %*% weights)
        ans2 = as.vector(weights * Lambda %*% weights)
        ans = round(ans2/ans1, digits = 4)
        names(ans) = names(weights)
    }
    else {
        Names = colnames(weights)
        ans = NULL
        for (i in 1:(dim(weights)[1])) {
            ans1 = as.vector(weights[i, ] %*% Lambda %*% weights[i, ])
            ans2 = as.vector(weights[i, ] * Lambda %*% weights[i, ])
            ans = rbind(ans, ans2/ans1)
        }
        colnames(ans) = Names
    }
    
    # Return Value:
    ans
}



# ------------------------------------------------------------------------------


getTargetReturn =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target return from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # Example:
    #   targetReturn()
    
    # FUNCTION:
    
    # Target MV Return:
    ans = object@portfolio$targetReturn
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getTargetRisk =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target risk from a 'fPORTFOLIO' object
   
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Target MV Risk:
    ans = object@portfolio$targetRisk
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getTargetAlpha =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the VaR-alpha from a 'fPORTFOLIO' object
   
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Target Alpha:
    ans = object@portfolio$targetAlpha
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getTargetStdev =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target standard deviation from a 'fPORTFOLIO' object
   
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Target Standard Deviation:
    ans = object@portfolio$targetStdev
    if (length(ans) == 1) names(ans) = "targetStdev"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getNames =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the asset names from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Names of Assets:
    ans = names(object@data$statistics$mu)
    if(is.null(ans)){
        counter = seq(1, getNumberOfAssets(object), 1)
        ans = paste("A", counter, sep = "")
    } 
    
    # Return Value:
    ans  
}


################################################################################

