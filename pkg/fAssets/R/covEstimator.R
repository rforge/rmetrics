
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
#  covEstimator              uses standard covariance estimation
#  mveEstimator              uses "cov.mve" from [MASS]
#  mcdEstimator              uses "cov.mcd" from [MASS]
#  covMcdEstimator           requires "covMcd" from [robustbase]  
#  covOGKEstimator           requires "covOGK" from [robustbase] 
#  nnveEstimator             uses builtin from [covRobust]
#  shrinkEstimator           uses builtin from [corpcor]
#  baggedEstimator           uses builtin from [corpcor]
################################################################################


covEstimator <- 
    function(x, spec = NULL, ...)
{
    # A function imolemented by Diethelm Wuertz
    
    # Description
    
    # FUNCTION:
    
    # Extract Matrix:
    x.mat = as.matrix(x)
    
    # Estimate:
    mu = colMeans(x.mat)
    Sigma = cov(x.mat)
    
    # Return Value:
    list(mu = mu, Sigma = Sigma)
}


# ------------------------------------------------------------------------------


covMcdEstimator <- 
    function(x, spec = NULL, ...)
{
    # A function imolemented by Diethelm Wuertz
    
    # Description
    
    # FUNCTION:
    
    # Extract Matrix:
    x.mat = as.matrix(x)
    
    # Estimate:
    estimate = robustbase::covMcd(x.mat, alpha = 1/2, ...)
    mu = estimate$center
    Sigma = estimate$cov
    
    # Return Value:
    list(mu = mu, Sigma = Sigma)
}


# ------------------------------------------------------------------------------


covOGKEstimator <- 
    function(x, spec = NULL, ...)
{
    # A function imolemented by Diethelm Wuertz
    
    # Description
    
    # FUNCTION:
    
    # Extract Matrix:
    x.mat = as.matrix(x)
    
    # Estimate:
    estimate = robustbase::covOGK(x.mat, sigmamu = scaleTau2, ...)
    mu = estimate$center
    Sigma = estimate$cov  
    
    # Return Value:
    list(mu = mu, Sigma = Sigma)
}


# ------------------------------------------------------------------------------


mveEstimator <- 
    function(x, spec = NULL, ...)
{
    # A function imolemented by Diethelm Wuertz
    
    # Description
    
    # FUNCTION:
    
    # Extract Matrix:
    x.mat = as.matrix(x)
    
    # Estimate: {
    estimate = MASS::cov.rob(x = x.mat, method = "mve")
    mu = estimate$center
    Sigma = estimate$cov
    
    # Return Value:
    list(mu = mu, Sigma = Sigma)
}


# ------------------------------------------------------------------------------


mcdEstimator <- 
    function(x, spec = NULL, ...)
{
    # A function imolemented by Diethelm Wuertz
    
    # Description
    
    # FUNCTION:
    
    # Extract Matrix:
    x.mat = as.matrix(x)
    
    # Estimate:
    ans = MASS::cov.rob(x = x.mat, method = "mcd") 
    mu = ans$center
    Sigma = ans$cov
    
    # Return Value:
    list(mu = mu, Sigma = Sigma)
}


# ------------------------------------------------------------------------------


shrinkEstimator <- 
    function(x, spec = NULL, ...)
{
    # A function imolemented by Diethelm Wuertz
    
    # Description
    
    # FUNCTION:
    
    # Extract Matrix:
    x.mat = as.matrix(x)
    
    # Estimate:
    estimate = .cov.shrink(x = x.mat, ...)
    mu = colMeans(x.mat)
    Sigma = estimate
    
    # Return Value:
    list(mu = mu, Sigma = Sigma)
}


# ------------------------------------------------------------------------------


baggedEstimator <- 
    function(x, spec = NULL, ...)
{
    # A function imolemented by Diethelm Wuertz
    
    # Description
    
    # FUNCTION:
    
    # Extract Matrix:
    x.mat = as.matrix(x)
    
    # Estimate:
    estimate = .cov.bagged(x = x.mat, R = 100, ...)
    mu = colMeans(x.mat)
    Sigma = estimate 
    
    # Return Value:
    list(mu = mu, Sigma = Sigma)
}


# ------------------------------------------------------------------------------


nnveEstimator <- 
    function(x, spec = NULL, ...)
{
    # A function imolemented by Diethelm Wuertz
    
    # Description
    
    # FUNCTION:
    
    # Extract Matrix:
    x.mat = as.matrix(x)
    
    # Estimate:
        estimate = .cov.nnve(datamat = x.mat, ...)
    mu = colMeans(x.mat)
    Sigma = estimate$cov 
    
    # Return Value:
    list(mu = mu, Sigma = Sigma)
}


################################################################################

