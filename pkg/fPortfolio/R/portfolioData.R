
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


portfolioData <- 
    function(data, spec = portfolioSpec())
{   
    # A function implemented by Rmetrics

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
    nAssets = NCOL(data)
        
    # Explore Tail Dependency:
    tailRisk = spec@model$tailRisk
    
    # Convert data to matrix object:
    series = as.matrix(data)
    
    # Select Estimator:
    estimator = match.fun(spec@model$estimator)
    muSigma = estimator(series, spec)
    
    # Estimates:
    mean = apply(series, MARGIN = 2, FUN = mean)
    Cov = cov(series) 
    mu = muSigma$mu
    Sigma = muSigma$Sigma
      
    # Statistics:
    statistics = list(mu = mu, Sigma = Sigma, mean = mean, Cov = Cov)
    attr(statistics, "estimator") = spec@model$estimator

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


################################################################################

