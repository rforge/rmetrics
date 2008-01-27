
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
#  getType                       Extract portfolio type from specification 
#  getEstimator                  Extract type of covariance estimator
#  getTailRisk                   Extract list of tail dependency risk matrixes
#  getParams                     Extract parameters from specification
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM SPECIFICATION:
#  getWeights                    Extracts weights from a portfolio object
#  getTargetReturn               Extracts target return from specification
#  getTargetRisk                 Extracts target riks from specification
#  getTargetAlpha                Extracts target VaR-alpha specification
#  getRiskFreeRate               Extracts risk free rate from specification 
#  getNFrontierPoints            Extracts number of frontier points 
#  getStatus                     Extracts portfolio status information
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM SPECIFICATION:
#  getSolver                     Extracts solver from specification
#  getTrace                      Extracts solver's trace flag
################################################################################


################################################################################
# fPFOLIOSPEC - S4

    # Slots:
    # model = list(
    #     type = c("MV", "CVaR"),
    #     estimator = c("mean", "cov"),
    #     tailRisk = NULL,
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


getType.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the type from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Type:
    ans = object@model$type[1]
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getEstimator.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the estimator from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Estimator:
    ans = object@model$estimator
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTailRisk.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extract list of tail dependency risk matrixes
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Estimator:
    ans = object@model$tailRisk
    
    # Return Value:
    ans  
}
                  

# ------------------------------------------------------------------------------


getParams.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the params from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Params:
    ans = object@model$params
    
    # Return Value:
    ans  
}


################################################################################


getWeights.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the weights from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Weights:
    ans = object@portfolio$weights
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTargetReturn.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target return from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # Example:
    #   targetReturn()
    
    # FUNCTION:
    
    # Get Target Return:
    ans = object@portfolio$targetReturn
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getTargetRisk.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target risk from specification
   
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Target Risk:
    ans = object@portfolio$targetRisk
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getTargetAlpha.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the VaR-alpha from specification
   
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Target Alpha:
    ans = object@portfolio$targetAlpha
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getRiskFreeRate.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the risk free rate from specification
   
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Risk Free Rate:
    ans = object@portfolio$riskFreeRate
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getNFrontierPoints.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the number of Frontier Points from specification
   
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Number of Frontier Points:
    ans = object@portfolio$nFrontierPoints
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getStatus.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts portfolio status information

   
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Number of Frontier Points:
    ans = object@portfolio$status
    names(ans) = "status"
    
    # Return Value:
    ans
}

                     
# ------------------------------------------------------------------------------


getSolver.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the solver from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Solver:
    ans = object@solver$solver[1]
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTrace.fPFOLIOSPEC =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the trace from specification
    
    # Arguments:
    #   object - an object of S4 class fPFOLIOSPEC
    
    # FUNCTION:
    
    # Get Trace:
    ans = object@solver$trace
    
    # Return Value:
    ans  
}


################################################################################

