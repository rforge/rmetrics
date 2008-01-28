
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
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM SPECIFICATION:
#  getModel                      Extract model slot
#   getType                       Extract portfolio type from specification 
#   getEstimator                  Extract type of covariance estimator
#   getTailRisk                   Extract list of tail dependency risk matrixes
#   getParams                     Extract parameters from specification
#   getAlpha                      Extracts target VaR-alpha specification
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM SPECIFICATION:
#  getPortfolio                  Extract portfolio slot
#   getWeights                    Extracts weights from a portfolio object
#   getTargetReturn               Extracts target return from specification
#   getTargetRisk                 Extracts target riks from specification
#   getRiskFreeRate               Extracts risk free rate from specification 
#   getNFrontierPoints            Extracts number of frontier points 
#   getStatus                     Extracts portfolio status information
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM SPECIFICATION:
#  getOptim                       Extract optim slot
#   getSolver                     Extracts solver from specification
#   getTrace                      Extracts solver's trace flag
################################################################################


    # fPFOLIOSPEC
    # model = list(
    #   type = "MV",
    #   estimator = "covEstimator",
    #   tailRisk = NULL,
    #   params = list())
    # portfolio = list(
    #   weights = NULL, 
    #   targetReturn = NULL, 
    #   targetRisk = NULL, 
    #   targetAlpha = NULL,
    #   riskFreeRate = 0, 
    #   nFrontierPoints = 50,
    #   status = 0),
    # optim = list(
    #   solver = "solveRquadprog",
    #   trace = FALSE)


# ------------------------------------------------------------------------------


getModel.fPFOLIOSPEC <- function(object) object@model  
  getType.fPFOLIOSPEC <- function(object) object@model$type[1]
  getEstimator.fPFOLIOSPEC <- function(object) object@model$estimator
  getTailRisk.fPFOLIOSPEC <- function(object) object@model$tailRisk
  getParams.fPFOLIOSPEC <- function(object) object@model$params



# ------------------------------------------------------------------------------


getPortfolio.fPFOLIOSPEC <- function(object) object@model
  getWeights.fPFOLIOSPEC <- function(object) object@portfolio$weights
  getTargetReturn.fPFOLIOSPEC <- function(object) object@portfolio$targetReturn
  getTargetRisk.fPFOLIOSPEC <- function(object) object@portfolio$targetRisk
  getAlpha.fPFOLIOSPEC <- function(object) object@model$params$alpha
  getRiskFreeRate.fPFOLIOSPEC <- function(object) object@portfolio$riskFreeRate
  getNFrontierPoints.fPFOLIOSPEC <- function(object) object@portfolio$nFrontierPoints
  getStatus.fPFOLIOSPEC <-  function(object) object@portfolio$status
 
                    
# ------------------------------------------------------------------------------


getOptim.fPFOLIOSPEC <- function(object) object@optim
  getSolver.fPFOLIOSPEC <- function(object) object@optim$solver 
  getTrace.fPFOLIOSPEC <- function(object) object@optim$trace


################################################################################

