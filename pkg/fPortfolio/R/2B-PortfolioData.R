
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
# FUNCTION:                     Classical and Robust Estimators
#  portfolioStatistics           Estimates mu and Sigma statistics
#  portfolioData                 Creates portfolio data list
################################################################################


portfolioStatistics = 
function(data, spec = portfolioSpec())
{   # A function implemented by Rmetrics

    # Description:
    #   Estimates mu and Sigma Statistics
    
    # Arguments:
    #   data - a multivariate timeSeries object
    #   spec -  a portfolio specification structure, from which
    #       the mean and covariance type of estimator will be extracted
    
    # FUNCTION:
    
    # Check Data - Should be a multivariate time series: 
    stopifnot(!is.list(data))
    
    # Convert data to matrix object:
    series = as.matrix(data)
    
    # Select Estimator:
    meanEstimator = spec@model$estimator[1]
    covEstimator = spec@model$estimator[2]
    
    # Robust Estimates:
    if (meanEstimator == "mcd" | covEstimator == "mcd") {
        # require(MASS)
        estimate = MASS::cov.mcd(series)
        mu = estimate$center
        Sigma = estimate$cov
    } else if (meanEstimator == "mve" | covEstimator == "mve") {
        # require(MASS)
        estimate = MASS::cov.mve(series)
        mu = estimate$center
        Sigma = estimate$cov
    } else if(meanEstimator == "lpm" | covEstimator == "lpm") {
        stopifnot(!is.null(spec@model$params$tau))
        stopifnot(!is.null(spec@model$params$a))
        estimate = assetsLPM(x, 
            tau = spec@model$params$tau, a = spec@model$params$a)
        mu = estimate$mu
        Sigma = estimate$Sigma
    } else if(meanEstimator == "shrink" | covEstimator == "shrink") {
        estimate = assetsMeanCov(series, method = "shrink")
        mu = estimate$mu
        Sigma = estimate$Sigma
    } 
    # Classical Estimates:
    if (meanEstimator == "mean") {
        mu = apply(series, MARGIN = 2, FUN = mean)
    }
    if (meanEstimator == "median") {
        mu = apply(series, MARGIN = 2, FUN = median)
    }
    if (covEstimator == "cov") {
        Sigma = cov(series)
    }
    
    # Statistics:
    statistics = list(mu = mu, Sigma = Sigma)
    attr(statistics, "estimator") = spec@model$estimator
    
    # Return Value:
    statistics
}


# ------------------------------------------------------------------------------


portfolioData =
function(data, spec = portfolioSpec())
{   # A function implemented by Rmetrics

    # Description:
    #   Creates portfolio data list
    
    # Arguments:
    #   data - a multivariate timeSeries object
    #   spec -  a portfolio specification structure, from which
    #       the mean and covariance type of estimator will be extracted
    
    # FUNCTION:
    
    # Check Data: 
    if (is.list(data)) {
        # In this case no time series is given, only mean and covariance ...
        series = NA
        statistics = list(mu = data$mu, Sigma = data$Sigma)
    } else {
        # Take care of time ordering ...
        if (is.timeSeries(data)) data = sort(data)
        series = data
        statistics = portfolioStatistics(data, spec)
    }
    
    # Explore Tail Dependency:
    if (!is.na(series)) {
        tailrisk = .lambdaTailRisk(series)
    } else {
        tailrisk = NA
    }
    
    # Portfolio Data List:
    data = list(
        series = series, 
        statistics = statistics, 
        tailrisk = tailrisk) 
    class(data) <- c("list", "fPFOLIODATA") 
    
    # Return Value:
    data
}


################################################################################



