
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
#  .mvnormFit                  Fits a multivariate Normal distribution
#  .mvsnormFit                 Fits a multivariate skew-Normal distribution
#  .mvstFit                    Fits a multivariate skew-Student-t distribution
################################################################################


assetsFit =
    function(x, method = c("st", "snorm", "norm"), title = NULL,
    description = NULL, fixed.df = NA, ...)
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
    #   mu - Mean values of each asset time series
    #   Omega - Covariance matrix of assets
    #   alpha - Skewness vector
    #   df - Degrees of freedom, measures kurtosis

    # Notes:
    #   Requires function "msn.mle" ans "mst.mle" from R's GPL licensed
    #     contributed package "sn", (C) 1998-2004 A. Azzalini.
    #   The list returned by this function can serve as input for the
    #     function assetsSim().

    # FUNCTION:

    # Settings:
    assets = as.matrix(x)
    method = method[1]
    colNames = colnames(x)

    # Normal Distribution:
    if (method == "norm") {
        # Fit Normal:
        fit = list()
        mu = apply(assets, 2, mean)
        Omega = cov(assets)
        alpha = rep(0, times = length(mu))
        df = Inf
    }

    # Skew-Normal Distribution:
    if (method == "snorm") {
        # Fit skew-Normal:
        fit = mvFit(assets, method = "snorm", ...)
        mu = as.vector(fit@fit$dp$beta)
        Omega = fit@fit$dp$Omega
        alpha = as.vector(fit@fit$dp$alpha)
        df = Inf
        fit = fit@fit
    }

    # Skew-Student-t Distribution:
    if (method == "st") {
        # Fit skew-Student:
        fit = mvFit(assets, method = "st", fixed.df = fixed.df, ...)
        mu = as.vector(fit@fit$beta)
        Omega = fit@fit$dp$Omega
        alpha = as.vector(fit@fit$dp$alpha)
        df = fit@fit$dp$df
        fit = fit@fit
    }

    # Add Names:
    names(mu) = colNames
    names(alpha) = colNames
    rownames(Omega) = colNames
    colnames(Omega) = colNames

    # Add Title:
    if (is.null(title))
        title = paste("Fitted Asset Data Model: ", method)

    # Add Description:
    if (is.null(description))
        description = .description()

    # Return Value:
    new("fASSETS",
        call = as.call(match.call()),
        method = as.character(method),
        model = list(mu = mu, Omega = Omega, alpha = alpha, df = df),
        data = as.data.frame(x),
        fit = as.list(fit),
        title = as.character(title),
        description = as.character(description)
        )
}


################################################################################


.mvnormFit =
    function(x, title = NULL, description = NULL, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits a multivariate Normal distribution

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
    #   mu - Mean values of each asset time series
    #   Omega - Covariance matrix of assets

    # Notes:
    #   Requires function "msn.mle" ans "mst.mle" from R's GPL licensed
    #     contributed package "sn", (C) 1998-2004 A. Azzalini.
    #   The list returned by this function can serve as input for the
    #     function assetsSim().

    # FUNCTION:

    # Settings:
    assets = as.matrix(x)
    method = method[1]
    colNames = colnames(x)

    # Fit mvNormal:
    fit = list()
    mu = apply(assets, 2, mean)
    Omega = cov(assets)
    alpha = rep(0, times = length(mu))
    df = Inf

    # Add Names:
    names(mu) = colNames
    names(alpha) = colNames
    rownames(Omega) = colNames
    colnames(Omega) = colNames

    # Add Title:
    if (is.null(title))
        title = paste("Fitted Asset Data Model: ", method)

    # Add Description:
    if (is.null(description))
        description = .description()

    # Return Value:
    new("fASSETS",
        call = as.call(match.call()),
        method = as.character(method),
        model = list(mu = mu, Omega = Omega, alpha = alpha, df = df),
        data = as.data.frame(x),
        fit = as.list(fit),
        title = as.character(title),
        description = as.character(description)
        )
}


# ------------------------------------------------------------------------------


mvsnormFit =
    function(x, title = NULL, description = NULL, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits a multivariate skew-Normal distribution

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
    #   mu - Mean values of each asset time series
    #   Omega - Covariance matrix of assets
    #   alpha - Skewness vector

    # Notes:
    #   Requires function "msn.mle" ans "mst.mle" from R's GPL licensed
    #     contributed package "sn", (C) 1998-2004 A. Azzalini.
    #   The list returned by this function can serve as input for the
    #     function assetsSim().

    # FUNCTION:

    # Settings:
    assets = as.matrix(x)
    method = method[1]
    colNames = colnames(x)

    # Fit skew-Normal:
    fit = mvFit(assets, method = "snorm", ...)
    mu = as.vector(fit@fit$dp$beta)
    Omega = fit@fit$dp$Omega
    alpha = as.vector(fit@fit$dp$alpha)
    df = Inf
    fit = fit@fit
    
    
    # Add Names:
    names(mu) = colNames
    names(alpha) = colNames
    rownames(Omega) = colNames
    colnames(Omega) = colNames

    # Add Title:
    if (is.null(title))
        title = paste("Fitted Asset Data Model: ", method)

    # Add Description:
    if (is.null(description))
        description = .description()

    # Return Value:
    new("fASSETS",
        call = as.call(match.call()),
        method = as.character(method),
        model = list(mu = mu, Omega = Omega, alpha = alpha, df = df),
        data = as.data.frame(x),
        fit = as.list(fit),
        title = as.character(title),
        description = as.character(description)
        )
}


# ------------------------------------------------------------------------------


.mvstFit =
    function(x, title = NULL, description = NULL, fixed.df = NA, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits a multivariate skew-Student-t distribution

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
    #   mu - Mean values of each asset time series
    #   Omega - Covariance matrix of assets
    #   alpha - Skewness vector
    #   df - Degrees of freedom, measures kurtosis

    # Notes:
    #   Requires function "msn.mle" ans "mst.mle" from R's GPL licensed
    #     contributed package "sn", (C) 1998-2004 A. Azzalini.
    #   The list returned by this function can serve as input for the
    #     function assetsSim().

    # FUNCTION:

    # Settings:
    assets = as.matrix(x)
    method = method[1]
    colNames = colnames(x)

    # Fit skew-Student:
    fit = mvFit(assets, method = "st", fixed.df = fixed.df, ...)
    mu = as.vector(fit@fit$beta)
    Omega = fit@fit$dp$Omega
    alpha = as.vector(fit@fit$dp$alpha)
    df = fit@fit$dp$df
    fit = fit@fit

    # Add Names:
    names(mu) = colNames
    names(alpha) = colNames
    rownames(Omega) = colNames
    colnames(Omega) = colNames

    # Add Title:
    if (is.null(title))
        title = paste("Fitted Asset Data Model: ", method)

    # Add Description:
    if (is.null(description))
        description = .description()

    # Return Value:
    new("fASSETS",
        call = as.call(match.call()),
        method = as.character(method),
        model = list(mu = mu, Omega = Omega, alpha = alpha, df = df),
        data = as.data.frame(x),
        fit = as.list(fit),
        title = as.character(title),
        description = as.character(description)
        )
}


################################################################################











