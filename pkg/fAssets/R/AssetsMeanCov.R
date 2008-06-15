
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

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
# FUNCTION:                 MEAN-COVARIANCE ESTIMATION:
#  assetsMeanCov             Estimates mean and variance for a set of assets
#  covEstimate               Sample mean/covariance Estimator
#  mcdEstimate               uses "cov.mve" from [MASS]
#  mveEstimate               uses "cov.mcd" from [MASS]
################################################################################


# COMMENTS On PREVIOUS VERSION:
# RENAMED:                  MEAN-COVARIANCE ESTIMATION:
#   method = "cov"           covEstimate: uses standard covariance estimation
#   method = "mve"           mveEstimate: uses "cov.mve" from [MASS]
#   method = "mcd"           mcdEstimate: uses "cov.mcd" from [MASS]
# RENAMED AND MOVED TO RMETRICS ADDON PACKAGE:
#   method = "Mcd"           mcdbaseEstimate: "covMcd" from [robustbase]  
#   method = "OGK"           ogkbaseEstimate: "covOGK" from [robustbase] 
#   method = "nnve"          nnveEstimate:    uses builtin from [covRobust]
#   method = "shrink"        shrinkEstimate:  uses builtin from [corpcor]
#   method = "bagged"        baggedEstimate:  uses builtin from [corpcor]


assetsMeanCov <- 
    function(x, FUN = "covEstimate", check = TRUE, force = TRUE, ...)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes mean and variance from multivariate time series
    
    # Arguments:
    #   x - a multivariate time series, a data frame, or any other
    #       rectangular object of assets which can be converted into
    #       a matrix by the function 'as.matrix'. Optional Dates are 
    #       rownames, instrument names are column names.
    #   FUN - Which estimator should be used to compute the covarinace?
    #   check - a logical. Should be checked if the covariance matrix
    #       is positive definite ?
    #   force - a logical. Should the covariance matrix be forced to be 
    #       positive definite ?
    #   alpha - MCD: numeric parameter controlling the size of the subsets 
    #       over which the determinant is minimized, i.e., alpha*n observations 
    #       are used for computing the determinant. Allowed values are between 
    #       0.5 and 1 and the default is 0.5.
    #   sigma.mu - OGK: a function that computes univariate robust location 
    #       and scale estimates. By default it should return a single numeric 
    #       value containing the robust scale (standard deviation) estimate. 
    #       When mu.too is true, sigmamu() should return a numeric vector of 
    #       length 2 containing robust location and scale estimates. See 
    #       scaleTau2, s_Qn, s_Sn, s_mad or s_IQR for examples to be used as 
    #       sigmamu argument.
       
    # Note:
    #   The output of this function can be used for portfolio optimization.
    
    # FUNCTION:
    
    # Settings:
    fun = match.fun(FUN)
      
    # Transform Input:
    x.mat = as.matrix(x)
    N = ncol(x)
    assetNames = colnames(x.mat)
    
    # Compute Mean / Covariance:  
    estimate = fun(x, ...)
    mu = estimate$mu
    Sigma = estimate$Sigma
    control = estimate$control
       
    # Add Size to Control List:
    control = c(control, size = as.character(N))
    
    # Add Names for Covariance Matrix to Control List:
    names(mu) = assetNames
    colnames(Sigma) = rownames(Sigma) = colNames = assetNames
    
    # Check Positive Definiteness:
    if (check) {
        result = isPositiveDefinite(Sigma)
        if(result) {
            control = c(control, posdef = "TRUE")
        } else {
            control = c(control, posdef = "FALSE")
        }
    }
    
    # Check Positive Definiteness:
    control = c(control, forced = "FALSE")
    if (force) {
        control = c(control, forced = "TRUE")
        if (!result) Sigma = makePositiveDefinite(Sigma)       
    }
    
    # Result:
    ans = list(center = mu, cov = Sigma, mu = mu, Sigma = Sigma)
    attr(ans, "control") = control
    
    # Return Value:
    ans
}


################################################################################


covEstimate <-
function(x)
{
    # Transform to matrix:
    x.mat = as.matrix(x)
    
    # Classical Covariance Estimation:
    mu = colMeans(x.mat)
    Sigma = cov(x.mat)
    
    # Return Value:
    list(mu = mu, Sigma = Sigma, control = "covEstimate")
}


# ------------------------------------------------------------------------------


mveEstimate <-
function(x)
{
    # Transform to matrix: 
    x.mat = as.matrix(x)
    
    # Require [MASS]: "mve"
    ans = MASS::cov.rob(x = x.mat, method = "mve")
    mu = ans$center
    Sigma = ans$cov
    
    # Return Value:
    list(mu = mu, Sigma = Sigma, control = "mveEstimate")
} 
       
    
# ------------------------------------------------------------------------------

        
mcdEstimate <-
function(x)
{
    # Transform to matrix: 
    x.mat = as.matrix(x)
    
    # Require(MASS): "mcd"
    ans = MASS::cov.rob(x = x.mat, method = "mcd") 
    mu = ans$center
    Sigma = ans$cov
    
    # Return Value:
    list(mu = mu, Sigma = Sigma, control = "mcdEstimate")
} 


################################################################################

