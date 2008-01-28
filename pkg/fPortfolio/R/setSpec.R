
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
# FUNCTION:                     MODEL SLOT:
#  setType<-                     Sets type of portfolio Optimization
#  setEstimator<-                Sets name of mean-covariance estimator
#  setTailRisk<-                 Sets tail dependency matrix
#  setParams<-                   Sets optional model parameters
# FUNCTION:                     PORTFOLIO SLOT:
#  setWeights<-                  Sets weights vector
#  setTargetReturn<-             Sets target return value
#  setTargetAlpha<-              Sets CVaR target alpha value
#  setRiskFreeRate<-             Sets risk-free rate value
#  setNFrontierPoints<-          Sets number of frontier points
#  setStatus<-                   Sets portfolio status information
# FUNCTION:                     OPTIMIZATION SLOT:
#  setSolver<-                   Sets name of desired solver
#  setTrace<-                    Sets solver's trace flag
################################################################################


"setType<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:                
    #   Sets the portfolio type for a portfolio structure
    
    # FUNCTION:
    
    # Type ?
    spec@model$type = value
    if (value == "CVaR") setSolver(spec) <- "solveRlpSolve"
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setEstimator<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:                  
    #   Sets the type of mean-cov estimator for a portfolio structure
    
    # FUNCTION:
    
    # Estimator ?
    spec@model$estimator = value 
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setParams<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:                  
    #   Sets optional parameters for a portfolio structure
    
    # FUNCTION:

    # Estimator ?
    spec@model$params = value 
    
    # Return Value:
    spec
}


################################################################################


"setWeights<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:                    
    #   Sets the weights vector for a portfolio structure
    
    # FUNCTION:
    
    # Weights ?
    spec@portfolio$weights = value
    if(!is.null(value)) {
        spec@portfolio$targetReturn = NULL
        spec@portfolio$targetRisk = NULL
    }
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setTargetReturn<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:                                   
    #   Sets the target return value for a portfolio structure
    
    # FUNCTION:

    # Target Return ?
    spec@portfolio$targetReturn = value
    if(!is.null(value)) {
        spec@portfolio$weights = NULL
        spec@portfolio$targetRisk = NULL
    }
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setTargetRisk<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:                                   
    #   Sets the target return value for a portfolio structure
    
    # FUNCTION:
 
    # Target Return ?
    spec@portfolio$targetRisk = value
    if(!is.null(value)) {
        spec@portfolio$weights = NULL
        spec@portfolio$return = NULL
    }
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setAlpha<-" <-
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:                  
    #   Sets the CVaR alpha significance value for a portfolio structure
    
    # FUNCTION:
 
    # Estimator ?
    spec@model$params$alpha = value 
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setRiskFreeRate<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:                                   
    #   Sets the risk free rate for a portfolio structure
    
    # FUNCTION:
    
    # Check Validity:
    stopifnot(is.numeric(value))
    stopifnot(length(value) == 1)
    
    # Risk-Free Rate ?
    spec@portfolio$riskFreeRate = value
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setNFrontierPoints<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:                                   
    #   Sets the number of frontier points for a portfolio structure
    
    # FUNCTION:
    
    # Check Validity:
    stopifnot(is.numeric(value))
    stopifnot(length(value) == 1)
    stopifnot(value > 0)
    
    # Risk-Free Rate ?
    spec@portfolio$nFrontierPoints = value
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setStatus<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:                                   
    #   Sets portfolio status information
    
    # FUNCTION:
    
    # Check Validity:
    stopifnot(is.numeric(value))
    stopifnot(length(value) == 1)
    
    # Risk-Free Rate ?
    spec@portfolio$status = value
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setTailRisk<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:                    
    #   Sets the tail risk value for a portfolio structure
    
    # Arguments:
    #   value - a list with two matrix elements, $lower and $upper, 
    #       with the pairwise tail dependence coefficints.
            
    # Example:
    #   LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
    #   setTailRisk <- .nigDependencyFit(LPP)
    
    # FUNCTION:
    
    # Tail Risk ?
    spec@model$tailRisk = value  
    
    # Return Value:
    spec
}


################################################################################
 

"setSolver<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Sets the solver value for a portfolio structure
    
    # FUNCTION:
      
    # Set Solver:
    spec@optim$solver = value
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setTrace<-" <- 
    function(spec, value)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Sets the trace value for a portfolio structure
    
    # FUNCTION:
    
    # Set Trace:
    spec@solver$trace = value
    
    # Return Value:
    spec
}


################################################################################

