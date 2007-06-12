
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
#  getSeries                     Extracts assets series data 
#  getStatistics                 Extracts assets statistics, mean and covariance
#  getTailrisk                   Extracts tail risk
#  getNumberOfAssets             Extracts number of assets from statistics
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM SPECIFICATION SLOT:
#  getType                       Extract portfolio type from specification 
#  getEstimator                  Extract type of covariance estimator
#  getParams                     Extract parameters from specification
#  getWeights                    Extracts weights from a portfolio object
#  getTargetReturn               Extracts target return from specification
#  getTargetRisk                 Extracts target riks from specification
#  getTargetAlpha                Extracts target VaR-alpha specification
#  getRiskFreeRate               Extracts risk free rate from specification 
#  getNFrontierPoints            Extracts number of frontier points 
#  getSolver                     Extracts solver from specification
#  getTrace                      Extracts solver's trace flag
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM PORTFOLIO SLOT:
#  getData                       Extracts @data Slot
#  getSpecification              Extracts @specification Slot
#  getConstraints                Extracts @constraints Slot
#  getPortfolio                  Extracts @portfolio Slot
# FUNCTION:                     OTHER EXTRACTORS:
#  getFrontier                   Extracts the efficient frontier
#  getRiskBudgets                Extracts risk budgets from a portfolio object
#  getTailRiskBudgets            Extracts tail risk budgets from a portfolio
#  getNames                      Extracts assets names from a portfolio
################################################################################


################################################################################
# fPFOLIODATA - S4

    # Slots:
    # series = list(
    #   data)
    # statistics = list(
    #   mu,
    #   Sigma) 
    # tailrisk = list()
    
    
# ------------------------------------------------------------------------------
    
    
getSeries =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the series from data
    
    # Arguments:
    #   object - an object of S4 class fPFOLIODATA
    
    # FUNCTION:
    
    # Get Series:
    ans = object@series$series
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------
    
    
getNumberOfAssets =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the series from data
    
    # Arguments:
    #   object - an object of S4 class fPFOLIODATA
    
    # FUNCTION:
    
    # Get Series:
    ans = object@series$nAssets
    
    # Return Value:
    ans  
}



# ------------------------------------------------------------------------------


getStatistics =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the statistics from data 
    
    # Arguments:
    #   object - an object of S4 class fPFOLIODATA
    
    # FUNCTION:
    
    # Get Statistics 
    ans = object@statistics
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTailrisk =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the tailrisk from data 
    
    # Arguments:
    #   object - an object of S4 class fPFOLIODATA
    
    # FUNCTION:
    
    # Get Statistics 
    ans = object@tailrisk
    
    # Return Value:
    ans  
}
    

################################################################################
# fPFOLIOSPEC - S4

    # Slots:
    # model = list(
    #     type = c("MV", "CVaR"),
    #     estimator = c("mean", "cov"),
    #     params = list())
    # portfolio = list(
    #     weights = NULL, 
    #     targetReturn = NULL, 
    #     targetRisk = NULL, 
    #     targetAlpha = NULL,
    #     riskFreeRate = 0, 
    #     nFrontierPoints = 50),
    # solver = list(
    #     solver = c("quadprog", "Rdonlp2", "lpSolve"),
    #     trace = FALSE)


# ------------------------------------------------------------------------------


getType =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the type from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC or fPORTFOLIO
    
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


getEstimator =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the estimator from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC or fPORTFOLIO
    
    # FUNCTION:
    
    # Get Estimator:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@model$estimator
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@specification$spec@model$estimator
    }
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getParams =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the params from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC or fPORTFOLIO
    
    # FUNCTION:
    
    # Get Params:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@model$params
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@specification$spec@model$params
    }
    
    # Return Value:
    ans  
}


################################################################################



getWeights =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the weights from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC or fPORTFOLIO
    
    # FUNCTION:
    
    # Get Weights:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@portfolio$weights
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@portfolio$weights
    }
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTargetReturn =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target return from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC or fPORTFOLIO
    
    # Example:
    #   targetReturn()
    
    # FUNCTION:
    
    # Get Target Return:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@portfolio$targetReturn
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@specification$spec@portfolio$targetReturn
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getTargetRisk =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target risk from specification
   
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC or fPORTFOLIO
    
    # FUNCTION:
    
    # Get Target Risk:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@portfolio$targetRisk
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@specification$spec@portfolio$targetRisk
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getTargetAlpha =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the VaR-alpha from specification
   
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC or fPORTFOLIO
    
    # FUNCTION:
    
    # Get Target Alpha:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@portfolio$targetAlpha
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@specification$spec@portfolio$targetAlpha
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getRiskFreeRate =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the risk free rate from specification
   
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC or fPORTFOLIO
    
    # FUNCTION:
    
    # Get Risk Free Rate:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@portfolio$riskFreeRate
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@specification$spec@portfolio$riskFreeRate
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getNFrontierPoints =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the number of Frontier Points from specification
   
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC or fPORTFOLIO
    
    # FUNCTION:
    
    # Get Number of Frontier Points:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@portfolio$nFrontierPoints
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@specification$spec@portfolio$nFrontierPoints
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getSolver =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the solver from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC or fPORTFOLIO
    
    # FUNCTION:
    
    # Get Solver:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@solver$solver[1]
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@specification$spec@solver$solver[1]
    }
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTrace =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the trace from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC or fPORTFOLIO
    
    # FUNCTION:
    
    # Get Trace:
    if (class(object) == "fPFOLIOSPEC") {
        ans = object@solver$trace
    } else if (class(object) == "fPORTFOLIO") {
        ans = object@specification$spec@solver$trace
    }
    
    # Return Value:
    ans  
}



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


getData =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the data slot from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    stopifnot(class(object) == "fPortfolio")
    ans = object@Data
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getSpecification =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the specification slot from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Specification:
    stopifnot(class(object) == "fPortfolio")
    ans = object@specification
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getConstraints =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the statistics from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    stopifnot(class(object) == "fPortfolio")
    ans = object@constraints
  
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getPortfolio =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the statistics from a 'fPORTFOLIO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Get Portfolio:
    stopifnot(class(object) == "fPortfolio")
    ans = object@portfolio
  
    # Return Value:
    ans  
}


################################################################################


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


getRiskBudgets = 
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


################################################################################

