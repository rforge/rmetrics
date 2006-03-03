
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             MEAN AND VARIANCE ESTIMATION:
#  .meanVariance         Estimates mean and Variance for a set of assets
# REQUIREMENTS:
#  copcor                R contributed package copcor
#  covRobust             R contributed package covRobust
################################################################################


.meanVariance = 
function(x, method = c("cov", "shrink", "bagged", "nnve"), 
R.bagged = 100, lambda.shrink = 0.1)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Compute mean and variance from multivariate time series
    
    # Arguments:
    # Arguments:
    #   x - A multivariate time series, a data frame, or any other
    #       rectangular object of assets which can be converted into
    #       a matrix by the function as.matrix. Optional Dates are 
    #       rownames, instrument names are column names.
    #   method - Which method should be used to compute the covarinace?
    #       cov - standard covariance computation
    #       shrink - estimation with shrinkage method
    #       bagged - bagging
    #       nnve - nearest neighbour variance estimation
    
    # Note:
    #   The output of this function can be used for portfolio
    #   optimization.
    
    # FUNCTION:
    
    # Transform Input:
    x.mat = as.matrix(x)
    N = dim(x)[1]
       
    # Attribute Control List:
    control = c(method = method[1])
    
    # Compute Mean:
    mu = colMeans(x)
    
    # Compute Covariance:
    if (method[1] == "cov") {
        Sigma = cov(x.mat)
    } else if (method[1] == "shrink") {
        fit = cov.shrink(x = x.mat, lambda = lambda.shrink)
        Sigma = fit 
        attr(Sigma, "lambda") = NULL
        control = c(control, lambda = as.character(lambda.shrink))
    } else if (method[1] == "bagged") {
        fit = cov.bagged(x = x.mat, R = R.bagged)
        Sigma = fit 
        control = c(control, R = as.character(R.bagged))
    } else if (method[1] == "nnve") {
        # Nearest Neighbour Variance Estimation:
        fit = cov.nnve(datamat = x.mat)
        Sigma = fit$cov
    }
       
    # Add Control and add Names for Covariance Matrix:
    control = c(control, N = as.character(N))
    names(mu) = colnames(x)
    colnames(Sigma) = rownames(Sigma) = colNames = colnames(x)
    
    # Result:
    ans = list(mu = mu, Sigma = Sigma)
    attr(ans, "control") = control
    
    # Return Value:
    ans
}
        

################################################################################

