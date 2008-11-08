
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
# FUNCTION:             SIMULATION AND PARAMETER ESTIMATION:
#  assetsSim             Simulates a set of artificial assets
################################################################################
            

assetsSim <- 
    function(n, dim = 2, model = list(mu = rep(0, dim), Omega = diag(dim), 
    alpha = rep(0, dim), df = Inf), assetNames = NULL) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulates a multivariate set of asset log-returns distributed
    #   according to a Normal, skew-Normal, or skew Student-t Distribution 
    #   Function.
    
    # Arguments:
    #   n - the number of data records to be simulated
    #   dim - the dimension number, i.e. the number of assets to be simulated
    #   model - a list with the model parameters:
    #       mu - the numeric vector of mean values of each asset time series
    #       Omega - the covariance matrix of assets
    #       alpha - the skewness vector
    #       df - the degrees of freedom, a measures for the kurtosis
    #   assetNames - a string vector of asset names, by default NULL
    #       which creates asset names as "V1", "V2", ..., "Vd", where
    #       d denotes the dimension
    
    # Notes: 
    #   Requires function "msn.quantities" from R's GPL licensed 
    #     contributed package "sn", (C) 1998-2004 A. Azzalini.
    #   The model can also be the value returned by model slot from
    #     function assetsFit().
    
    # Example: 
    #   assetsSim(n=25)
    #   assetsSim(n=25, assetNames = c("RETURN-1", "RETURN-2")
    #   assetsSim(n=25, list(mu=c(0,0), Omega=diag(2), alpha=c(0,0), df=4)) 
    
    # FUNCTION:
    
    # Dimensions:
    d = length(model$alpha)
    if ( length(model$mu) != d | any(dim(model$Omega) != c(d, d))) 
        stop("dimensions of arguments do not match")
    
    # Adapted from contributed R package "sn:rmsn"
    Z = msn.quantities(model$mu, model$Omega, model$alpha)
    y = matrix(rnorm(n * d), n, d) %*% chol(Z$Psi)
    abs.y0 = matrix(rep(abs(rnorm(n)), d), ncol = d)
    z = Z$delta * t(abs.y0) + sqrt(1 - Z$delta^2) * t(y)
    
    # Select:
    if (model$df == Inf) {      
        ans = t(model$mu + Z$omega * z) 
    } else {
        x = rchisq(n, model$df)/model$df
        z = t(model$mu + Z$omega * z)
        ans = t(model$mu + t(sqrt(x) * z)) 
    }
        
    # Dimnames:
    dimnames(ans)[[2]] = assetNames 
    
    # Return Value:
    as.data.frame(ans)
}


################################################################################

