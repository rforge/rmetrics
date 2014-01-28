
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
# FUNCTION:                   DESCRIPTION:
#  assetsFit                   Fits the parameters of a set of assets
#  assetsSim                   Simulates a set of artificial assets
################################################################################


assetsFit <-
    function(x, method = c("st", "snorm", "norm"), title = NULL,
    description = NULL, fixed.df = NA)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits the parameters of a multivariate data set of assets
    #   and returns a list with the values for the mean, the covariance,
    #   the skewness, and the fatness of tails.

    # Arguments:
    #   x - A multivariate time series, a data frame, or any other
    #       rectangular object of assets which can be converted into
    #       a matrix by the function as.matrix. Optional Dates are
    #       rownames, instrument names are column names.
    #   type - Which type of distribution should be fitted?
    #       a) norm - multivariate Normal
    #       b) snorm - multivariate skew-Normal
    #       c) st - multivariate skew-Student-t

    # Value:
    #   The function returns a list with the following entries:
    #   mu (beta) - Mean values of each asset time series
    #   Omega - Covariance matrix of assets
    #   alpha - Skewness vector
    #   df (nu) - Degrees of freedom, measures kurtosis

    # Notes:
    #   Requires function "msn.mle" ans "mst.mle" from R's GPL licensed
    #     contributed package "sn", (C) 1998-2004 A. Azzalini.
    #   The list returned by this function can serve as input for the
    #     function assetsSim().

    # FUNCTION:

    # Settings:
    assets <- as.matrix(x)
    method <- method[1]
    colNames <- colnames(x)

    # Fit Normal Distribution:
    if (method == "norm") {
        # Fit Normal:
        fit <- list()
        fit$dp <- list(
            beta = as.vector(apply(assets, 2, mean)),
            Omega = cov(assets),
            alpha = rep(0, times = ncol(assets)),
            nu = Inf)
    }

    # Fit Skew-Normal Distribution:
    if (method == "snorm") {
        fit <- sn::msn.mle(y=assets)
        fit$dp$nu = Inf
    }

    # Fit Skew-Student-t Distribution:
    if (method == "st") {
        # Fit skew-Student:
        if (is.na(fixed.df)) fixed.df = NULL
        fit <- sn::mst.mple(y=assets, penalty=NULL, fixed.nu = fixed.df)
    }

    # Add Names:
    colNames <- colnames(assets)
    mu <- as.vector(fit$dp$beta)
    names(mu) <- colNames
    Omega <- fit$dp$Omega
    rownames(Omega) <- colNames
    colnames(Omega) <- colNames
    alpha <- as.vector(fit$dp$alpha)
    names(alpha) <- colNames
    df <- c(df=fit$dp$nu)
    
    # Used Method:
    fit$method <- as.character(method)
    
    # Return Value:
    list(
        param = list(mu = mu, Omega = Omega, alpha = alpha, df = df),
        fit = fit)
}


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
        #   Requires function ".msn.quantities" from R's GPL licensed 
        #     contributed package "sn", (C) 1998-2004 A. Azzalini.
        #   The model can also be the value returned by model slot from
        #     function assetsFit().
        
        # FUNCTION:
        
        # Dimensions:
        d <- length(model$alpha)
        if ( length(model$mu) != d | any(dim(model$Omega) != c(d, d))) 
            stop("dimensions of arguments do not match")
        
        # Simulate Series - calls from package sn:
        x <- rmnorm(n=n, mean=model$mu, varcov=model$Omega)
        x <- rmsn(n=n, xi=model$mu, Omega=model$Omega, alpha=model$alpha)
        x <- rmst(n=n, xi=model$mu, Omega=model$Omega, alpha=model$alpha, 
             nu=model$df)

        # Return Value:
        as.timeSeries(x)
    }


################################################################################

