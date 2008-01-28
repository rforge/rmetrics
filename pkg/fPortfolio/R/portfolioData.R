
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
# FUNCTION:                     DESCRIPTION:
#  portfolioData                 Creates portfolio data list
################################################################################


portfolioData <- 
    function(data, spec = portfolioSpec())
{   
    # A function implemented by Rmetrics

    # Description:
    #   Creates portfolio data list
    
    # Arguments:
    #   data - a multivariate 'timeSeries' object
    #   spec -  a portfolio specification structure, from which
    #       the mean and covariance type of estimator will be extracted
    
    # FUNCTION:
    
    # Check and Sort Data: 
    stopifnot(class(data) == "timeSeries") 
    
    # Data:
    data = sort(data)
    nAssets = NCOL(data)
    names = colnames(data)
    if(is.null(names)) names = paste("A", 1:nAssets, sep = "")
    .data = list(
        series = data,
        nAssets = nAssets,
        names = names)
        
    # Statistics:
    estimator = getEstimator(spec)
    estimatorFun = match.fun(estimator)
    muSigma = estimatorFun(data, spec)
    .statistics = list(
        mean = colMeans(data), 
        Cov = cov(data),
        estimator = estimator,
        mu = muSigma$mu, 
        Sigma = muSigma$Sigma)
  
    # Tail Risk:
    .tailRisk = spec@model$tailRisk
        
    # Return Value:
    new("fPFOLIODATA", 
        data = .data,
        statistics = .statistics,
        tailRisk = .tailRisk)  
}


################################################################################

