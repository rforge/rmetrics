
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA


################################################################################
# FUNCTION:            DESCRIPTION:
#  covarRisk            Computes covariance portfolio risk
#  mcr                  Computes marginal contribution to covariance risk
#  mcrBeta              Computes beta, the rescaled mcr to covariance risk
#  riskContributions    Computes covariance risk contributions
#  riskBudgets          Computes covariance risk budgets
################################################################################
   

covarRisk <-
function(data, weights = NULL, FUN = "cov", ...)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes covariance portfolio risk
    
    # Arguments:
    #   data - a multivariate timeSeries object of financial returns
    #   weights - numeric vector of portfolio weights
    #   FUN - a covariance estimator, which returns a matrix of 
    #       covariance estimates, by default the sample covariance
    #   ... - Optional arguments passed to the function FUN
    
    # Example:
    #   .covarRisk(data)
    
    # FUNCTION:
    
    # Covariance Risk:
    covFun = match.fun(FUN)
    COV = covFun(data)
    
    # Portfolio Weights:
    N = ncol(COV)
    if (is.null(weights)) weights = rep(1/N, N)
    names(weights) = colnames(COV)
    
    # Covariance Portfolio Risk:
    covarRisk <- sqrt( t(weights) %*% COV %*% weights )[[1, 1]]
    
    # Return Value:
    covarRisk
}


# ------------------------------------------------------------------------------


mcr <- 
function(data, weights = NULL, FUN = "cov", ...)
{
    # A function implemented by Diethelm Wuertz
    
    # Description
    #   Computes marginal contribution to covariance risk
    
    # Arguments:
    #   data - a multivariate timeSeries object of financial returns
    #   weights - numeric vector of portfolio weights
    #   FUN - a covariance estimator, which returns a matrix of 
    #       covariance estimates, by default the sample covariance
    #   ... - Optional arguments passed to the function FUN
    
    # Details
    #   The formula are implemented according to Goldberg et al., 
    #   see also R script assetsPfolio.R
    
    # References:
    #   Lisa Goldberg et al., Extreme Risk Management, 2009
    #   Scherer and Martin, Introduction to modern portfolio Optimimization
    
    # Example:
    #   data = assetsSim(100, 6); mcr(data)
    
    # FUNCTION:
    
    # Covariance Risk:
    covFun = match.fun(FUN)
    COV = covFun(data)
    N = ncol(data)
    if (is.null(weights)) weights = rep(1/N, N)
    
    # Marginal Contribution to Risk  
    mcr <- (COV %*% weights)[, 1] / covarRisk(data, weights, FUN, ...) 
    names(mcr) <- colnames(data) 
    
    # Return Value:
    mcr
}


# ------------------------------------------------------------------------------


mcrBeta <- 
function(data, weights = NULL, FUN = "cov", ...)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes beta, the rescaled marginal contribution to covariance risk
    
    # Arguments:
    #   data - a multivariate timeSeries object of financial returns
    #   weights - numeric vector of portfolio weights
    #   FUN - a covariance estimator, which returns a matrix of 
    #       covariance estimates, by default the sample covariance
    #   ... - Optional arguments passed to the function FUN
    
    # Example:
    #    mcrBeta(data)
   
    # FUNCTION:
    
    # Portfolio Beta:
    beta <- mcr(data, weights, FUN = FUN, ...) / 
        covarRisk(data, weights, FUN = FUN, ...)
   
    # Return Value:
    beta
}


# ------------------------------------------------------------------------------


riskContributions <-
function(data, weights = NULL, FUN = "cov", ...)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes covariance risk contributions
    
    # Arguments:
    #   data - a multivariate timeSeries object of financial returns
    #   weights - numeric vector of portfolio weights
    #   FUN - a covariance estimator, which returns a matrix of 
    #       covariance estimates, by default the sample covariance
    #   ... - Optional arguments passed to the function FUN
    
    # Example:
    #    riskContributions(data)
    
    # FUNCTION:
    
    # Risk Contributions:
    if (is.null(weights)) {
        N = ncol(data)
        weights = rep(1/N, times = N)
    }
    riskContributions <- weights * mcr(data, weights, FUN, ...)
    
    # Return Value:
    riskContributions
}
    
    
# ------------------------------------------------------------------------------


riskBudgets <-
function(data, weights = NULL, FUN = "cov", ...)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes covariance risk budgets
    
    # Arguments:
    #   data - a multivariate timeSeries object of financial returns
    #   weights - numeric vector of portfolio weights
    #   FUN - a covariance estimator, which returns a matrix of 
    #       covariance estimates, by default the sample covariance
    #   ... - Optional arguments passed to the function FUN
    
    # Example:
    #    data = 100*LPP2005.RET[, 1:6]; riskBudgets(data)
    
    # FUNCTION:
    
    # Risk Budgets:
    riskBudgets <- riskContributions(data, weights, FUN, ...) /
        covarRisk(data, weights, FUN, ...)
    
    # Return Value:
    riskBudgets
} 


################################################################################

