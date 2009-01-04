
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


################################################################################
# FUNCTION:                 DESCRIPTION:
#  assetsMeanCov             Estimates mean and variance for a set of assets
#   method = "cov"            uses standard covariance estimation
#   method = "mve"            uses "cov.mve" from [MASS]
#   method = "mcd"            uses "cov.mcd" from [MASS]
#   method = "MCD"            requires "covMcd" from [robustbase]  
#   method = "OGK"            requires "covOGK" from [robustbase] 
#   method = "nnve"           uses builtin from [covRobust]
#   method = "shrink"         uses builtin from [corpcor]
#   method = "bagged"         uses builtin from [corpcor]
#   method = "foo"            uses user defined estimator named foo
#  getCenterRob              Extracts the robust estimate for the center
#  getCovRob                 Extracts the robust estimate for the covariance
################################################################################


assetsMeanCov = 
    function(x, 
    method = c("cov", "mve", "mcd", "MCD", "OGK", "nnve", "shrink", "bagged"), 
    check = TRUE, force = TRUE, baggedR = 100, sigmamu = scaleTau2, alpha = 1/2,
    ...)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes robust mean and covariance from multivariate time series
    
    # Arguments:
    #   x - a multivariate time series, a data frame, or any other
    #       rectangular object of assets which can be converted into
    #       a matrix by the function 'as.matrix'. Optional Dates are 
    #       rownames, instrument names are column names.
    #   method - Which method should be used to compute the covarinace?
    #       method = "cov"        sample covariance computation
    #       method = "mve"        uses "mve" from [MASS]
    #       method = "mcd"        uses "mcd" from [MASS]
    #       method = "MCD"        uses "MCD" from [robustbase]
    #       method = "OGK"        uses "OGK" from [robustbase]
    #       method = "nnve"       uses "nnve" from [covRobust]
    #       method = "shrink"     uses "shrinkage" from [corpcor] 
    #       method = "bagged"     uses "bagging" [corpcor]
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
    #   The output of this function can be used for portfolio
    #   optimization.
    
    # Example:
    #   DJ = 100 * returns(as.timeSeries(data(DowJones30)))
    #   DJ = DJ[, c("CAT", "IBM", "GE", "JPM")]
    #   Sample Covariance:
    #       assetsMeanCov(DJ, "cov")
    #   MASS:
    #       assetsMeanCov(DJ, "mve")
    #       assetsMeanCov(DJ, "mcd")
    #   require(robustbase)
    #       assetsMeanCov(DJ, "MCD")
    #       assetsMeanCov(DJ, "OGK")
    #   require(covRobust)
    #       assetsMeanCov(DJ, "nnve")
    
    # FUNCTION:
    
    # Transform Input:
    x.mat = as.matrix(x)
    
    
    # Do not use: method = match.arg(method)
    method = method[1]
    N = ncol(x)
    assetNames = colnames(x.mat)
       
    # Attribute Control List:
    control = c(method = method[1])
    user = TRUE
    
    # Compute Classical Covariance:
    if (method == "cov") {
        # Classical Covariance Estimation:
        ans = list(center = colMeans(x.mat), cov = cov(x.mat))
        user = FALSE
    }
        
    # From R Package "robustbase":
    if (method == "MCD" | method == "Mcd") {
        ans = robustbase::covMcd(x.mat, alpha = alpha, ...)
        mu = ans$center
        Sigma = ans$cov
        user = FALSE
    }   
    if (method == "OGK" | method == "Ogk") {
        ans = robustbase::covOGK(x.mat, sigmamu = scaleTau2, ...)
        user = FALSE    
    }
    
    # [MASS] mve and mcd Routines:
    if (method == "mve") {
        # require(MASS)
        ans = MASS::cov.rob(x = x.mat, method = "mve")
        user = FALSE
    }
    if (method == "mcd") {
        # require(MASS)
        ans = MASS::cov.rob(x = x.mat, method = "mcd") 
        user = FALSE
    }    
        
    # [corpcor] Shrinkage and Bagging Routines 
    if (method == "shrink") {
        fit = .cov.shrink(x = x.mat, ...)
        ans = list(center = colMeans(x.mat), cov = fit)
        user = FALSE
    } 
    if (method == "bagged") {
        fit = .cov.bagged(x = x.mat, R = baggedR, ...)
        ans = list(center = colMeans(x.mat), cov = fit)
        control = c(control, R = as.character(baggedR))
        user = FALSE
    }
        
    # Nearest Neighbour Variance Estimation:
    if (method == "nnve") {
        fit = .cov.nnve(datamat = x.mat, ...)
        ans = list(center = colMeans(x.mat), cov = fit$cov)
        user = FALSE
    }
    
    # User specified estimator:
    if(user) {
        fun = match.fun(method[1])
        ans = fun(x.mat, ...)
    }
    
    # Result:
    mu = center = ans$center
    Sigma = cov = ans$cov
       
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


# ------------------------------------------------------------------------------


getCenterRob <-
function(object)
{
    object$center
}


# ------------------------------------------------------------------------------


getCovRob <-
function(object)
{
    object$cov
}


################################################################################

