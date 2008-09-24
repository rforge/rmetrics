
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
#   method = "mcdbase"       mcdbaseEstimate:  "covMcd" from [robustbase]  
#   method = "ogkbase"       ogkbaseEstimator: "covOGK" from [robustbase] 
#   method = "nnve"          nnveEstimate:     uses builtin from [covRobust]
#   method = "shrink"        shrinkEstimate:   uses builtin from [corpcor]
#   method = "bagged"        baggedEstimate:   uses builtin from [corpcor]
################################################################################


### Additional mean/cov estimators for robust estimation ###


# COMMENTS ON PREVIOUS VERSION:
# RENAMED:                  MEAN-COVARIANCE ESTIMATION:
#   method = "cov"           covEstimate: uses standard covariance estimation
#   method = "mve"           mveEstimate: uses "cov.mve" from [MASS]
#   method = "mcd"           mcdEstimator: uses "cov.mcd" from [MASS]
# RENAMED AND MOVED TO RMETRICS ADDON PACKAGE:
#   method = "mcdbase"       mcdbaseEstimate:  "covMcd" from [robustbase]  
#   method = "ogkbase"       ogkbaseEstimator: "covOGK" from [robustbase] 
#   method = "nnve"          nnveEstimate:     uses builtin from [covRobust]
#   method = "shrink"        shrinkEstimate:   uses builtin from [corpcor]
#   method = "bagged"        baggedEstimate:   uses builtin from [corpcor]


# Usage:
#   assetsMeanCov(x, FUN = "covEstimate", check = TRUE, force = TRUE, ...)

# Description:
#   Computes mean and variance from multivariate time series

# Arguments:
#   x - a multivariate time series, a data frame, or any other
#       rectangular object of assets which can be converted into
#       a matrix by the function 'as.matrix'. Optional Dates are 
#       rownames, instrument names are column names.
#   FUN - Which estimator should be used to compute the covarinace?
#   check - 
#   force - 
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
    

# ------------------------------------------------------------------------------

       
mcdbaseEstimate <-
function(x, alpha = 0.5)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    
    # Arguments:
    #   x - timeSeries object or any other rectangular oobject which can
    #       be transformed by the function as.matrix into a numeric matrix
    
    # FUNCTION:
    
    # Transform to Matrix: 
    x.mat = as.matrix(x)
    
    # Require [robustbase]: MCD | mcd:
    estimate = robustbase::covMcd(x.mat, alpha = alpha)
    mu = estimate$center
    Sigma = estimate$cov 
    
    # Return Value:
    list(mu = mu, Sigma = Sigma, control = "mcdbaseEstimate")
} 
    

# ------------------------------------------------------------------------------

 
ogkbaseEstimate <-
function(x, sigmamu = scaleTau2)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    
    # Arguments:
    #   x - timeSeries object or any other rectangular oobject which can
    #       be transformed by the function as.matrix into a numeric matrix
    
    # FUNCTION:
    
    # Transform to Matrix: 
    x.mat = as.matrix(x)
    
    # Require [robustbase]: OGK | ogk
    estimate = robustbase::covOGK(x.mat, sigmamu = sigmamu)
    mu = estimate$center
    Sigma = estimate$cov     
    
    # Return Value:
    list(mu = mu, Sigma = Sigma, control = "ogkbaseEstimate")
} 
   
        
# ------------------------------------------------------------------------------


shrinkEstimate <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    
    # Arguments:
    #   x - timeSeries object or any other rectangular oobject which can
    #       be transformed by the function as.matrix into a numeric matrix
    
    # FUNCTION:
    
    # Transform to Matrix: 
    x.mat = as.matrix(x)
    
    # Require [corpcor] 
    # Shrinkage and Bagging Routines: "shrink"
    fit = .cov.shrink(x = x.mat)
    mu = colMeans(x.mat)
    Sigma = fit 

    # Return Value:
    list(mu = mu, Sigma = Sigma, control = "shrinkEstimate")
} 

        
# ------------------------------------------------------------------------------

 
baggedEstimate <-
function(x, R = 100)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    
    # Arguments:
    #   x - timeSeries object or any other rectangular oobject which can
    #       be transformed by the function as.matrix into a numeric matrix
    
    # FUNCTION:
    
    # Transform to Matrix: 
    x.mat = as.matrix(x)
    
    # Method: "bagged"
    fit = .cov.bagged(x = x.mat, R = R)
    mu = colMeans(x.mat)
    Sigma = fit 
    control = c("baggedEstimate", R = as.character(R))
    
    # Return Value:
    list(mu = mu, Sigma = Sigma, control = control)
} 
        

# ------------------------------------------------------------------------------

    
nnveEstimate <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    
    # Arguments:
    #   x - timeSeries object or any other rectangular oobject which can
    #       be transformed by the function as.matrix into a numeric matrix
    
    # FUNCTION:
    
    # Transform to Matrix: 
    x.mat = as.matrix(x)
    
    # Nearest Neighbour Variance Estimation: "nnve"
    fit = .cov.nnve(datamat = x.mat)
    mu = colMeans(x.mat)
    Sigma = fit$cov
    
    # Return Value:
    list(mu = mu, Sigma = Sigma, control = "nnveEstimate")
} 
       

################################################################################

