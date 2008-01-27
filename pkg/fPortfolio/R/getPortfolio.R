
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
# FUNCTION:                     Description:
#  getData                       Extracts data slot
#   getSeries                     Extracts assets series data 
#   getNAssets                    Extracts number of assets from data
#   getNames                      Extracts assets names from data
#  getStatistics                 Extracts statistics slot
#   getMean                       Extracs mean from statistics
#   getCov                        Extracs covariance Sigma from statistics
#   getMu                         Extracs mu from statistics
#   getSigma                      Extracs Sigma from statistics
#   getEstimator                  Extracts estimator from 
#  getTailRisk                   Extracts tailRisk slot
# FUNCTION:                     Description:
#  getSpec                       Extracs specification Slot
#   getType                       Extracts type of portfolio
#   getEstimator                  Extracts mean-covariance estimator
#   getParams                     Extracts optional parameter list
# *getPortfolio                  Extract portfolio slot
#  *getWeights                    Extracts weights from a portfolio object
#  *getTargetReturn               Extracts target return from specification
#  *getTargetRisk                 Extracts target riks from specification
#  *getTargetAlpha                Extracts target VaR-alpha specification
#  *getRiskFreeRate               Extracts risk free rate from specification 
#  *getNFrontierPoints            Extracts number of frontier points 
#  *getStatus                     Extracts portfolio status information
#  getOptim                      Extract optim slot
#   getSolver                     Extracts solver from specification
#   getTrace                      Extracts solver's trace flag
# FUNCTION:                     Description:
#  getConstraints 
# FUNCTION:                     Description:               
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


        
getData.fPORTFOLIO = function(object) object@data$data
 getSeries.fPORTFOLIO = function(object) getSeries(getData(object))
 getNAssets.fPORTFOLIO = function(object) getNAssets(getData(object))
 getNames.fPORTFOLIO = function(object) getNames(getData(object))
 getStatistics.fPORTFOLIO = function(object) getStatistics(getData(object))
 getMean.fPORTFOLIO = function(object) getMean(getData(object))
 getCov.fPORTFOLIO = function(object) getCov(getData(object))
 getMu.fPORTFOLIO = function(object) getMu(getData(object))
 getSigma.fPORTFOLIO = function(object) getSigma(getData(object))

 
# ------------------------------------------------------------------------------


getSpec.fPORTFOLIO <- function(object) object@spec$spec
 getModel.fPORTFOLIO <- function(object) object@model  
  getType.fPORTFOLIO <- function(object) object@model$type[1]
  getEstimator.fPORTFOLIO <- function(object) object@model$estimator
  getTailRisk.fPORTFOLIO <- function(object) object@model$tailRisk
  getParams.fPORTFOLIO <- function(object) object@model$params
 getPortfolio.fPORTFOLIO <- function(object) object@model
  getWeights.fPORTFOLIO <- function(object) object@portfolio$weights
  getTargetReturn.fPORTFOLIO <- function(object) object@portfolio$targetReturn
  getTargetRisk.fPORTFOLIO <- function(object) object@portfolio$targetRisk
  getTargetAlpha.fPORTFOLIO <- function(object) object@portfolio$targetAlpha
  getRiskFreeRate.fPORTFOLIO <- function(object) object@portfolio$riskFreeRate
  getNFrontierPoints.fPORTFOLIO <- function(object) object@portfolio$nFrontierPoints
  getStatus.fPORTFOLIO <-  function(object) object@portfolio$status
 getOptim.fPORTFOLIO <- function(object) object@optim
  getSolver.fPORTFOLIO <- function(object) object@optim$solver 
  getTrace.fPORTFOLIO <- function(object) object@optim$trace


# ------------------------------------------------------------------------------


getConstraints.fPORTFOLIO <- function(object) object@constraints


# ------------------------------------------------------------------------------


getPortfolio.fPORTFOLIO <- function(object) object@portfolio
 getWeights.fPORTFOLIO <- function(object) object@portfolio$weights
 getTargetRisk.fPORTFOLIO <- function(object) object@portfolio$targetRisk
 getTargetAlpha.fPORTFOLIO <- function(object) object@portfolio$targetAlpha
 getRiskFreeRate.fPORTFOLIO <- function(object) object@spec$riskFreeRate
 getNFrontierPoints.fPORTFOLIO <- function(object) object@portfolio$nFrontierPoints
 getStatus.fPORTFOLIO <- function(object) object@portfolio$status



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

