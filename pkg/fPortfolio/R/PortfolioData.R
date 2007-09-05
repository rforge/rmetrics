
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
# FUNCTION:                     PORTFOLIO DATA CLASS:
#  'fPFOLIODATA'                 S4 Portfolio Data Class
#  portfolioData                 Creates portfolio data list
#  show.fPFOLIODATA              Print method for 'fPFOLIODATA' objects
# FUNCTION:                     PORTFOLIO STATISTICS:
#  portfolioStatistics           Estimates mu and Sigma statistics
################################################################################


setClass("fPFOLIODATA", 
    representation(
        data = "list",
        statistics = "list",
        tailRisk = "list")  
)


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
    
    # Check and Sort Data: 
    stopifnot(class(data) == "timeSeries") 
    data = sort(data)
    nAssets = dim(data)[2]
        
    # Statistics:
    statistics = portfolioStatistics(data, spec)
    
    # Explore Tail Dependency:
    tailRisk = spec@model$tailRisk
     
    # Return Value:
    new("fPFOLIODATA", 
        data = list(series = data, nAssets = nAssets),
        statistics = statistics,
        tailRisk = tailRisk)  
}


# ------------------------------------------------------------------------------


show.fPFOLIODATA =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   S4 Print Method for an object of class "fPFOLIODATA"
    
    # Arguments:
    #   object - an object of class "fPFOLIOSPEC"
    
    # FUNCTION:
    
    # Series:
    cat("\nSeries Data:\n\n")
    print(object@data$series)
    
    # Statistics:
    cat("\nStatistics:\n\n")
    print(object@statistics)
    
    # Tailrisk:
    # NYI
        
    # Return Value: 
    invisible(object)
}


# ------------------------------------------------------------------------------


setMethod("show", "fPFOLIODATA", show.fPFOLIODATA)


# ------------------------------------------------------------------------------


portfolioStatistics = 
function(data, spec = portfolioSpec())
{   # A function implemented by Rmetrics

    # Description:
    #   Estimates mu and Sigma Statistics
    
    # Arguments:
    #   data - a multivariate timeSeries object
    #   spec - a portfolio specification structure, from which
    #       the mean and covariance type of estimator will be extracted
    
    # FUNCTION:
    
    # Check Data - Should be a multivariate time series: 
    stopifnot(!is.list(data))
    
    # Convert data to matrix object:
    series = as.matrix(data)
    
    # Select Estimator:
    meanEstimator = spec@model$estimator[1]
    covEstimator = spec@model$estimator[2]
    
    # LPM:
    if(meanEstimator == "lpm" | covEstimator == "lpm") {
        stopifnot(!is.null(spec@model$params$tau))
        stopifnot(!is.null(spec@model$params$a))
        estimate = assetsLPM(x = data, 
            tau = spec@model$params$tau, a = spec@model$params$a)
        mu = estimate$mu
        Sigma = estimate$Sigma
    }
        
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
    } else if (meanEstimator == "Mcd" | covEstimator == "Mcd") {
        # require(robustbase)
        estimate = robustbase::covMcd(series)
        mu = estimate$center
        Sigma = estimate$cov
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


################################################################################

