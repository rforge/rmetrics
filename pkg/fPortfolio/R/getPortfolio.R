
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
# FUNCTION:                     DESCRIPTION:
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
# FUNCTION:                     DESCRIPTION:
#  getSpec                       Extracs specification Slot
#   getType                       Extracts type of portfolio
#   getEstimator                  Extracts mean-covariance estimator
#   getParams                     Extracts optional parameter list
# *getPortfolio                  Extract portfolio slot
#  *getWeights                    Extracts weights from a portfolio object
#  *getTargetReturn               Extracts target return from specification
#  *getTargetRisk                 Extracts target riks from specification
#  *getAlpha                Extracts target VaR-alpha specification
#  *getRiskFreeRate               Extracts risk free rate from specification 
#  *getNFrontierPoints            Extracts number of frontier points 
#  *getStatus                     Extracts portfolio status information
#  getOptim                      Extract optim slot
#   getSolver                     Extracts solver from specification
#   getTrace                      Extracts solver's trace flag
# FUNCTION:                     DESCRIPTION:
#  getConstraints                Extracts weight constraints
# FUNCTION:                     DESCRIPTION:               
#  getPortfolio                  Extracts portfolio slot
#   getWeights                   Extracts weights
#   getTargetReturn              Extracts target return
#   getTargetRisk                Extracts target return
#   getAlpha               Extracts significance level alpha
#   getRiskFreeRate              Extracts risk free rate
#   getNFrontierPoints           Extracts number of frontier points
#   getStatus                    Extracts status
# FUNCTION:                     GENERAL EXTRACTORS:
#  getCovRiskBudgets             Extracts covariance risk budgets
#  getTailRiskBudgets            Extracts tail risk budgets
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
 getModel.fPORTFOLIO <- function(object) getModel(getSpec(object))
  getType.fPORTFOLIO <- function(object) getType(getSpec(object))
  getEstimator.fPORTFOLIO <- function(object) getEstimator(getSpec(object))
  getTailRisk.fPORTFOLIO <- function(object) getTailRisk(getSpec(object))
  getParams.fPORTFOLIO <- function(object) getParams(getSpec(object))
 getPortfolio.fPORTFOLIO <- function(object) getPortfolio(getSpec(object))
  getWeights.fPORTFOLIO <- function(object) getWeights(getSpec(object))
  getTargetReturn.fPORTFOLIO <- function(object) getTargetReturn(getSpec(object))
  getTargetRisk.fPORTFOLIO <- function(object) getTargetRisk(getSpec(object))
  getAlpha.fPORTFOLIO <- function(object) getAlpha(getSpec(object))
  getRiskFreeRate.fPORTFOLIO <- function(object) getRiskFreeRate(getSpec(object))
  getNFrontierPoints.fPORTFOLIO <- function(object) get(getSpec(object))
  getStatus.fPORTFOLIO <-  function(object) get(getSpec(object))
 getOptim.fPORTFOLIO <- function(object) getOptim(getSpec(object))
  getSolver.fPORTFOLIO <- function(object) getSolver(getSpec(object)) 
  getTrace.fPORTFOLIO <- function(object) getTrace(getSpec(object))


# ------------------------------------------------------------------------------


getConstraints.fPORTFOLIO <- function(object) object@constraints


# ------------------------------------------------------------------------------


getPortfolio.fPORTFOLIO <- function(object) object@portfolio
 getWeights.fPORTFOLIO <- function(object) object@portfolio$weights
 getTargetReturn.fPORTFOLIO <- function(object) object@portfolio$targetReturn
 getTargetRisk.fPORTFOLIO <- function(object) object@portfolio$targetRisk
 getAlpha.fPORTFOLIO <- function(object) object@portfolio$targetAlpha
 getRiskFreeRate.fPORTFOLIO <- function(object) object@spec$riskFreeRate
 getNFrontierPoints.fPORTFOLIO <- function(object) object@portfolio$nFrontierPoints
 getStatus.fPORTFOLIO <- function(object) object@portfolio$status


################################################################################


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

