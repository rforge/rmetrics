
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
#   1999 - 2007, Rmetrics Foundation, GPL
#   Contact: Diethelm Wuertz <wuertz@phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                     PORTFOLIO SPECIFICATION CLASS:
#  'fPFOLIOSPEC'                 S4 Portfolio Specification Class
#  portfolioSpec                 Specifies a portfolio
#  show.fPFOLIOSPEC              Print method for 'fPFOLIOSPEC' objects
# FUNCTION:                     MODEL SLOT:
#  setType                       Sets type of portfolio Optimization
#  setType<-                      alternative function call
#  setEstimator                  Sets name of mean-covariance estimator
#  setEstimator<-                 alternative function call
#  setParams                     Sets optional model parameters
#  setParams<-                    alternative function call
# FUNCTION:                     PORTFOLIO SLOT:
#  setWeights                    Sets weights vector
#  setWeights<-                   alternative function call
#  setTargetReturn               Sets target return value
#  setTargetReturn<-              alternative function call
#  setTargetAlpha                Sets CVaR target alpha value
#  setTargetAlpha<-               alternative function call
#  setRiskFreeRate               Sets risk-free rate value
#  setRiskFreeRate<-              alternative function call
#  setNFrontierPoints            Sets number of frontier points
#  setNFrontierPoints<-           alternative function call
#  setReturnRange                Sets range of target returns
#  setReturnRange<-               alternative function call
#  setRiskRange                  Sets range of target risks
#  setRiskRange<-                 alternative function call
# FUNCTION:                     SOLVER SLOT:
#  setSolver                     Sets name of desired solver
#  setSolver<-                    alternative function call
# FUNCTION:                     Classical and Robust Estimators
#  portfolioStatistics           Estimates mu and Sigma statistics
#  portfolioData                 Creates portfolio data list
################################################################################


setClass("fPFOLIOSPEC", 
    representation(
        call = "call",
        model = "list",
        portfolio = "list",
        solver = "list",
        title = "character",
        description = "character")  
)


# ------------------------------------------------------------------------------


portfolioSpec = 
function(
model = list(
    type = "MV", 
    estimator = c("mean", "cov"), 
    params = list()),
portfolio = list(
    weights = NULL, 
    targetReturn = NULL, 
    targetAlpha = NULL,
    riskFreeRate = 0, 
    nFrontierPoints = 50, 
    returnRange = NULL, 
    riskRange = NULL),
solver = list(
    type = c("RQuadprog", "RDonlp2"),
    trace = FALSE),  
title = NULL, 
description = NULL)
{   # A function implemented by Rmetrics

    # Description:
    #   Specifies a portfolio to be optimized
    
    # Example:
    #   portfolioSpec(portfolio = list(targetReturn = 1.5))
    
    # FUNCTION:
    
    # Compose Checklists:
    model.type = "MV"
    model.estimator.mean = "mean"
    model.estimator.cov = c("cov", "mcd", "shrink")
    solver.type = c("RQuadprog", "RDonlp2")
    solver.trace = FALSE
    
    # Check Arguments:
    stopifnot(model$type %in% model.type)
    stopifnot(model$estimator[1] %in% model.estimator.mean)
    stopifnot(model$estimator[2] %in% model.estimator.cov)
    stopifnot(solver$type %in% solver.type)
    
    # Model:
    Model = list(type = "MV", estimator = c("mean", "cov"))
    Model[(Names <- names(model))] <- model
    
    # Portfolio:
    Portfolio = list(weights = NULL, targetReturn = NULL, targetRisk = NULL,
        targetAlpha = NULL, riskFreeRate = 0, nFrontierPoints = 100, 
        returnRange = NULL, riskRange = NULL)
    Portfolio[(Names <- names(portfolio))] <- portfolio
    # Check Portfolio - weights, targetReturn, targetRisk:
    # ... at least two of them must be set to NULL!
    checkPortfolio = 0
    if(!is.null(portfolio$weights)) checkPortfolio = checkPortfolio + 1
    if(!is.null(portfolio$targetReturn)) checkPortfolio = checkPortfolio + 1
    stopifnot(checkPortfolio <= 1)
  
    # Add Solver Solver:
    Solver = list(type = solver$type[1], trace = solver$trace)
    
    # Add Title and Description:
    if (is.null(title)) title = "Portfolio Specification"
    if (is.null(description)) description = .description()
    
    # Return Value:
    new("fPFOLIOSPEC", 
        call = match.call(),
        model = Model,
        portfolio = Portfolio,
        solver = Solver,
        title = title, 
        description = description)    
} 


# ------------------------------------------------------------------------------


show.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   S4 Print Method for an object of class "fPFOLIOSPEC"
    
    # Arguments:
    #   object - an object of class "fPFOLIOSPEC"
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n ")
    cat(getTitle(object), "\n")
    
    # Call:
    cat("\nCall:\n ")
    print.default(getCall(object))
    
    # Model:
    cat("\nPortfolio Type:\n ")
    cat(object@model$type, "\n")
    
    cat("\nCovariance Estimator:\n ")
    cat(object@model$estimator, "\n")
    
    cat("\nSolver:\n ")
    cat(object@solver$type[1], "\n")
    
    # Portfolio:
    if (!is.null(object@portfolio$weights)) {
        cat("\nPortfolio Weights:\n")
        print(object@portfolio$weights) 
    }
    if (!is.null(object@portfolio$targetReturn)) {
        cat("\nTarget Return:\n")
        print(object@portfolio$targetReturn)
    }
    if (!is.null(object@portfolio$targetAlpha)) {
        cat("\nTarget Alpha:\n")
        print(object@portfolio$targetAlpha)
    }
    if (!is.null(object@portfolio$riskFreeRate)) {
        cat("\nPortfolio Risk-Free Rate:\n ")
        cat(object@portfolio$riskFreeRate, "\n")
    }
    if (!is.null(object@portfolio$nFrontierPoints)) {
        cat("\nNumber of Frontier Points:\n ")
        cat(object@portfolio$nFrontierPoints, "\n")
    }
    if (!is.null(object@portfolio$returnRange)) {
        cat("\nTarget Return Range:\n")
        print(object@portfolio$returnRange) 
    }
    if (!is.null(object@portfolio$riskRange)) {
        cat("\nTarget Risk Range:\n")
        print(object@portfolio$riskRange)
    }
   
    # Description:
    cat("\nDescription:\n ")
    cat(getDescription(object), "\n")
        
    # Return Value: 
    invisible(object)
}


# ------------------------------------------------------------------------------


setMethod("show", "fPFOLIOSPEC", show.fPFOLIOSPEC)


################################################################################


setType =
function(spec = portfolioSpec(), type = c("MV", "LPM", "CVaR"))
{   # A function implemented by Rmetrics

    # Description:                
    #   Sets type of portfolio optimization
    
    # FUNCTION:
    
    # Check Validity:
    # ...
    
    # Type ?
    type = match.arg(type)
    spec@model$type = type
    if (type == "LPM") spec@model$estimator = c("lpm", "lpm")

    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setType<-" =
function(spec, value)
{   # A function implemented by Rmetrics

    # Description:                
    #   Sets type of portfolio optimization
    
    # FUNCTION:
    
    # Check Validity:
    # ...
    
    # Type ?
    spec@model$type = value
    if (value == "LPM") spec@model$estimator = c("lpm", "lpm")

    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


setEstimator = 
function(spec = portfolioSpec(), estimator = c("mean", "cov"))
{   # A function implemented by Rmetrics

    # Description:                  
    #   Sets name of mean-covariance estimator
    
    # FUNCTION:
    
    # Check Validity:
    # ...
    
    # Estimator ?
    spec@model$estimator = estimator 
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setEstimator<-" = 
function(spec, value)
{   # A function implemented by Rmetrics

    # Description:                  
    #   Sets name of mean-covariance estimator
    
    # FUNCTION:
    
    # Check Validity:
    # ...
    
    # Estimator ?
    spec@model$estimator = value 
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


setParams = 
function(spec = portfolioSpec(), params = list())
{   # A function implemented by Rmetrics

    # Description:                  
    #   Sets optional parameters
    
    # FUNCTION:
    
    # Check Validity:
    # ...
    
    # Estimator ?
    spec@model$params = params 
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setParams<-" = 
function(spec, value)
{   # A function implemented by Rmetrics

    # Description:                  
    #   Sets optional parameters
    
    # FUNCTION:
    
    # Check Validity:
    # ...
    
    # Estimator ?
    spec@model$params = value 
    
    # Return Value:
    spec
}


################################################################################


setWeights = 
function(spec = portfolioSpec(), weights = NULL)
{   # A function implemented by Rmetrics

    # Description:                    
    #   Sets weights vector
    
    # FUNCTION:
    
    # Weights ?
    spec@portfolio$weights = weights
    if(!is.null(weights)) {
        spec@portfolio$targetReturn = NULL
    }
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setWeights<-" = 
function(spec, value)
{   # A function implemented by Rmetrics

    # Description:                    
    #   Sets weights vector
    
    # FUNCTION:
    
    # Check Validity:
    # ...
    
    # Weights ?
    spec@portfolio$weights = value
    if(!is.null(value)) {
        spec@portfolio$targetReturn = NULL
    }
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


setTargetReturn = 
function(spec = portfolioSpec(), targetReturn = NULL)
{   # A function implemented by Rmetrics

    # Description:                                   
    #   Sets target return value
    
    # FUNCTION:
    
    # Check Validity:
    # ...
    
    # Target Return ?
    spec@portfolio$targetReturn = targetReturn 
    if(!is.null(targetReturn)) {
        spec@portfolio$weights = NULL
    }
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setTargetReturn<-" <- 
function(spec, value)
{   # A function implemented by Rmetrics

    # Description:                                   
    #   Sets target return value
    
    # FUNCTION:
    
    # Check Validity:
    # ...
    
    # Target Return ?
    spec@portfolio$targetReturn = value
    if(!is.null(value)) {
        spec@portfolio$weights = NULL
    }
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


setTargetAlpha = 
function(spec = portfolioSpec(), targetAlpha = NULL)
{   # A function implemented by Rmetrics

    # Description:                                   
    #   Sets target return value
    
    # FUNCTION:
    
    # Check Validity:
    # ...
    
    # Target Alpha ?
    spec@portfolio$targetAlpha = targetAlpha 
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setTargetAlpha<-" = 
function(spec, value)
{   # A function implemented by Rmetrics

    # Description:                  
    #   Sets target Alpha value
    
    # FUNCTION:
    
    # Check Validity:
    # ...
    
    # Estimator ?
    spec@portfolio$targetAlpha = value 
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


setRiskFreeRate = 
function(spec = portfolioSpec(), riskFreeRate = 0)
{   # A function implemented by Rmetrics

    # Description:                                   
    #   Sets risk-free rate value
    
    # FUNCTION:
    
    # Check Validity:
    stopifnot(is.numeric(riskFreeRate))
    
    # Risk-Free Rate ?
    spec@portfolio$riskFreeRate = riskFreeRate 
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setRiskFreeRate<-" <- 
function(spec, value)
{   # A function implemented by Rmetrics

    # Description:                                   
    #   Sets risk-free rate value
    
    # FUNCTION:
    
    # Check Validity:
    stopifnot(is.numeric(value))
    
    # Risk-Free Rate ?
    spec@portfolio$riskFreeRate = value
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


setNFrontierPoints = 
function(spec = portfolioSpec(), nFrontierPoints = 50)
{   # A function implemented by Rmetrics

    # Description:                                
    #   Sets number of frontier points
    
    # FUNCTION:
    
    # Check Validity:
    stopifnot(is.numeric(nFrontierPoints))
    stopifnot(nFrontierPoints > 0)
    
    # Number of Frontier Points ?
    spec@portfolio$nFrontierPoints = nFrontierPoints
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setNFrontierPoints<-" <- 
function(spec, value)
{   # A function implemented by Rmetrics

    # Description:                                   
    #   Sets number of frontier points
    
    # FUNCTION:
    
    # Check Validity:
    stopifnot(is.numeric(value))
    stopifnot(value > 0)
    
    # Risk-Free Rate ?
    spec@portfolio$nFrontierPoints = value
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


setReturnRange = 
function(spec = portfolioSpec(), returnRange = NULL)
{   # A function implemented by Rmetrics

    # Description:                                    
    #   Sets range of target returns
    
    # FUNCTION:
    
    # Check Validity:
    #
    
    # Return Range ?
    spec@portfolio$returnRange = returnRange
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setReturnRange<-" = 
function(spec = portfolioSpec(), value)
{   # A function implemented by Rmetrics

    # Description:                                    
    #   Sets range of target returns
    
    # FUNCTION:
    
    # Check Validity:
    #
    
    # Return Range ?
    spec@portfolio$returnRange = value
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


setRiskRange = 
function(spec = portfolioSpec(), riskRange = NULL)
{   # A function implemented by Rmetrics

    # Description:                                      
    #   Sets range of target risks
    
    # FUNCTION:
    
    # Check Validity:
    #
    
    # Risk Range ?
    spec@portfolio$riskRange = riskRange
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setRiskRange<-" = 
function(spec = portfolioSpec(), value)
{   # A function implemented by Rmetrics

    # Description:                                      
    #   Sets range of target risks
    
    # FUNCTION:
    
    # Check Validity:
    #
    
    # Risk Range ?
    spec@portfolio$riskRange = value
    
    # Return Value:
    spec
}


################################################################################
 

setSolver = 
function (spec = portfolioSpec(), solver = c("RQuadprog", "Rdonlp2")) 
{   # A function implemented by Rmetrics

    # Description:
    
    # FUNCTION:
    
    # Set Solver:
    solver = match.arg(solver)
    spec@solver$type = solver
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setSolver<-" <- function(spec, value)
{   # A function implemented by Rmetrics

    # Description:
    
    # FUNCTION:
    
    # Valid Solvers:
    validSolvers = c("RQuadprog", "RDonlp2", "RlpSolve")
    stopifnot(value %in% validSolvers)
    
    # Set Solver:
    spec@solver$type = value
    
    # Return Value:
    spec
}


################################################################################


portfolioStatistics = 
function(data, spec = portfolioSpec())
{   # A function implemented by Rmetrics

    # Description:
    #   Estimates mu and Sigma Statistics
    
    # FUNCTION:
    
    # Check Data: 
    stopifnot(!is.list(data))
    
    # Convert data to matrix object:
    series = as.matrix(data)
    
    # Select Estimator:
    meanEstimator = spec@model$estimator[1]
    covEstimator = spec@model$estimator[2]
    
    # Robust Estimates:
    if (meanEstimator == "mcd" | covEstimator == "mcd") {
        # require(MASS)
        estimate = MASS::cov.mcd(series)
        mu = estimate$center
        Sigma = estimate$cov
    } else if (meanEstimator == "mve" | covEstimator == "mve") {
        # require(MASS)
        estimate = MASS::cov.mve(series)
        mu = estimate$center
        Sigma = estimate$cov
    } else if(meanEstimator == "lpm" | covEstimator == "lpm") {
        stopifnot(!is.null(spec@model$params$tau))
        stopifnot(!is.null(spec@model$params$a))
        estimate = assetsLPM(x, 
            tau = spec@model$params$tau, a = spec@model$params$a)
        mu = estimate$mu
        Sigma = estimate$Sigma
    } else if(meanEstimator == "shrink" | covEstimator == "shrink") {
        estimate = assetsMeanCov(series, method = "shrink")
        mu = estimate$mu
        Sigma = estimate$Sigma
    } 
    # Classical Estimates:
    if (meanEstimator == "mean") {
        mu = apply(series, MARGIN = 2, FUN = mean)
    }
    if (meanEstimator == "median") {
        mu = apply(series, MARGIN = 2, FUN = median)
    }
    if (covEstimator == "cov") {
        Sigma = cov(series)
    }
    
    # Statistics:
    statistics = list(mu = mu, Sigma = Sigma)
    attr(statistics, "estimator") = spec@model$estimator
    
    # Return Value:
    statistics
}


# ------------------------------------------------------------------------------


portfolioData =
function(data, spec = portfolioSpec())
{   # A function implemented by Rmetrics

    # Description:
    #   Creates portfolio data list
    
    # FUNCTION:
    
    # Check Data: 
    if (is.list(data)) {
        # In this case no time series is given, only mean and covariance ...
        series = NA
        statistics = list(mu = data$mu, Sigma = data$Sigma)
    } else {
        # Take care of time ordering ...
        if (is.timeSeries(data)) data = sort(data)
        series = data
        statistics = portfolioStatistics(data, spec)
    }
    
    # Portfolio data list:
    data = list(series = series, statistics = statistics) 
    class(data) <- c("list", "fPFOLIODATA") 
    
    # Return Value:
    data
}


################################################################################

