
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
# FUNCTION:                      SINGLE PORTFOLIOS:
#  .feasibleShortMVPortfolio      Returns a feasible MV portfolio
#  .cmlShortMVPortfolio           Returns a capital market line
#  .tangencyShortMVPortfolio      Returns the tangency MV portfolio
#  .minvarianceShortMVPortfolio   Returns the minimum variance portfolio
#  .frontierShortMVPortfolio      Returns a frontier MV portfolio
# FUNCTION:                      PORTFOLIO FRONTIER:
#  .portfolioShortMVFrontier      Returns the EF of a short selling MV portfolio
################################################################################


.feasibleShortMVPortfolio =
function(data, spec = portfolioSpec(), constraintsStrings = NULL)
{
    # Description:
    #   Computes Risk and Return for a feasible portfolio
    
    # Arguments:
    #   data - a list with two named elements. 
    #       $series holding the time series which may be any rectangular,
    #       object or if not specified holding NA;
    #       $statistics holding a named two element list by itself, 
    #        $mu the location of the asset returns by default the mean and 
    #        $Sigma the scale of the asset returns by default the covariance
    #        matrix.
    
    # Note:
    #   In contrast to the functions *Portfolio(), which only require either the
    #   statistics or the series the functions .*Portfolio() require both as
    #   input
    
    # Example:
    #   .feasibleShortMVPortfolio(engelsPortfolioData())
    
    # FUNCTION:
       
    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
   
    # Targets:
    N = length(mu)
    weights = spec@portfolio$weights
    if (is.null(weights)) weights =  rep(1/N, times = N)
    targetReturn = as.numeric(mu %*% weights)
    targetRisk = sqrt( as.numeric( t(weights) %*% Sigma %*% (weights) ) )
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = data,
        specification = spec, 
        constraints = as.character(constraintsStrings),
        portfolio = list(
            weights = weights,
            targetReturn = Return,
            targetRisk = targetRisk,
            targetStdev = targetRisk),
        title = "Feasible Portfolio", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.cmlShortMVPortfolio =
function(data, spec = portfolioSpec(), constraintsStrings = NULL)
{
    # Description:
    #   Computes capital market line
    
    # Example:
    #   .tangencyShortMVPortfolio(engelsPortfolioData())
    
    # FUNCTION:
    
    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Risk-Free Rate:
    riskFreeRate = spec@portfolio$riskFreeRate
    
    # Parameter Settings:
    C0 = 1
    one = rep(1, times = length(mu))
    invSigma = solve(Sigma)
    a = as.numeric(mu %*% invSigma %*% mu)
    b = as.numeric(mu %*% invSigma %*% one)
    c = as.numeric(one %*% invSigma %*% one)
    d = as.numeric(a*c - b^2)
       
    # Capital Market Line:
    A = (a - b*riskFreeRate)
    B = (b - c*riskFreeRate)/C0
    weights = C0 * as.vector(invSigma %*% (mu - riskFreeRate) ) / B
    targetReturn = A / B
    targetRisk = sqrt(c*riskFreeRate^2 - 2*b*riskFreeRate + a) / B
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = data,
        specification = spec, 
        constraints = as.character(constraintsStrings),
        portfolio = list(
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetStdev = targetRisk),
        title = "Capital Market Line", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.tangencyShortMVPortfolio =
function(data, spec = portfolioSpec(), constraintsStrings = NULL)
{
    # Description:
    #   Computes target risk and weights for the tangency portfolio
    
    # Example:
    #   .tangencyShortMVPortfolio(engelsPortfolioData())
    
    # FUNCTION:
    
    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Parameter Settings:
    C0 = 1
    one = rep(1, times = length(mu))
    invSigma = solve(Sigma)
    a = as.numeric(mu %*% invSigma %*% mu)
    b = as.numeric(mu %*% invSigma %*% one)
    c = as.numeric(one %*% invSigma %*% one)
    d = as.numeric(a*c - b^2)
    
    # Tangency Portfolio:
    weights = C0 * as.vector(invSigma %*% mu ) / b 
    targetReturn = (a/b)*C0
    targetRisk = (sqrt(a)/b)*C0
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = data,
        specification = spec, 
        constraints = as.character(constraintsStrings),
        portfolio = list(
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetStdev = targetRisk),
        title = "Tangency MV Portfolio", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.minvarianceShortMVPortfolio =
function(data, spec = portfolioSpec(), constraintsStrings = NULL)
{
    # Description:
    #   Computes target risk and weights for the minimum variance portfolio
    
    # Example:
    #   .minvarianceShortMVPortfolio(engelsPortfolioData())
    
    # FUNCTION:
    
    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Parameter Settings:
    C0 = 1
    one = rep(1, times = length(mu))
    invSigma = solve(Sigma)
    a = as.numeric(mu %*% invSigma %*% mu)
    b = as.numeric(mu %*% invSigma %*% one)
    c = as.numeric(one %*% invSigma %*% one)
    d = as.numeric(a*c - b^2)
    
    # Minimum Variance Portfolio:
    targetReturn = (b/c)*C0
    targetRisk = C0/sqrt(c)
    weights = as.vector(invSigma %*% ((a-b*mu)*C0 + (c*mu-b)*targetReturn )/d)

    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        specification = spec, 
        constraints = as.character(constraintsStrings),
        data = data, 
        portfolio = list(
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetStdev = targetRisk),
        title = "Minimum Variance MV Portfolio", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.frontierShortMVPortfolio =
function(data, spec = portfolioSpec(), constraintsStrings = NULL)
{
    # Description:
    #   Computes target risk and weights for an efficient portfolio
    
    # Example:
    #   .frontierShortMVPortfolio(engelsPortfolioData())
    
    # FUNCTION:
    
    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Specification:
    targetReturn = spec@portfolio$targetReturn
    
    # Compute Default Target Return:   
    if (is.null(targetReturn)) {
        targetReturn = getTargetReturn(.tangencyShortMVPortfolio(data, spec))
    }
    
    # Parameter Settings:
    C0 = 1
    one = rep(1, times = length(mu))
    invSigma = solve(Sigma)
    a = as.numeric(mu %*% invSigma %*% mu)
    b = as.numeric(mu %*% invSigma %*% one)
    c = as.numeric(one %*% invSigma %*% one)
    d = as.numeric(a*c - b^2)
    
    # A Portfolio on the EF with given Target Return:
    targetRisk = sqrt((c*targetReturn^2 - 2*b*C0*targetReturn + a*C0^2) / d)
    weights = as.vector(invSigma %*% ((a-b*mu)*C0 + (c*mu-b)*targetReturn )/d)
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        specification = spec,
        constraints = as.character(constraintsStrings),
        data = data, 
        portfolio = list(
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetStdev = targetRisk),
        title = "Frontier MV Portfolio", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.portfolioShortMVFrontier = 
function(data, spec = portfolioSpec(), constraintsStrings = NULL,
title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the efficient frontier, short selling allowed
    
    # Details  from a matrix
    #   Calculates the efficient frontier (short selling allowed) from a
    #   a matrix of either market or simulated assets given in matrix "x". 
    #   Each time series represents a column in this matrix.
    
    # Arguments:     
    #   data - either a rectangular time series object which can be
    #       transformed into a matrix or a list with two named entries,
    #       mu - the returns of the multivariate series, and
    #       Sigma - the Covariance matrix of the multivariate series
    #   riskFreeRate - rate in percent of risk free asset
    #   nFrontierPoints - Number of equidistant return points on the Efficient 
    #       frontier
    #   muRange - Plot range of returns, if NULL, the range will be created
    #       automatically
    #   sigmaRange - Plot range of standard deviations, if NULL, the range 
    #       will be created automatically
    
    # Example:
    #   .shortMVFrontier(engelsPortfolioData())
    
    # FUNCTION:
    
    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Specification:
    riskFreeRate = spec@portfolio$riskFreeRate 
    nFrontierPoints = spec@portfolio$nFrontierPoints
    muRange = spec@portfolio$returnRange
    sigmaRange = spec@portfolio$riskRange
    
    # Parameter Settings:
    C0 = 1
    one = rep(1, times = length(mu))
    invSigma = solve(Sigma)
    a = as.numeric(mu %*% invSigma %*% mu)
    b = as.numeric(mu %*% invSigma %*% one)
    c = as.numeric(one %*% invSigma %*% one)
    d = as.numeric(a*c - b^2)
    
    # Ranges for mean and Standard Deviation:
    if (is.null(muRange)){
        muRange = range(mu)+ .25*c(-diff(range(mu)), diff(range(mu)))
    }
    sqrtSig = sqrt(diag(Sigma))
    if (is.null(sigmaRange)){
        sigmaRange = c(min(sqrtSig), max(sqrtSig))+
        .25*c(-diff(range(sqrtSig)), diff(range(sqrtSig)))
    }
               
    # Efficient Frontier Portfolios:
    targetReturn = seq(muRange[1], muRange[2], length = nFrontierPoints)
    targetReturn = as.vector(targetReturn)
    targetRisk = sqrt((c*targetReturn^2 - 2*b*C0*targetReturn + a*C0^2)/d)
    targetRisk = as.vector(targetRisk)
    weights = NULL
    Spec = spec
    for (i in 1:nFrontierPoints) {
        Spec@portfolio$targetReturn = targetReturn[i]
        nextWeight = getWeights(.frontierShortMVPortfolio(data, Spec))
        weights = rbind(weights, t(nextWeight))
    }
         
    # Adding title and description:
    if(is.null(title)) title = "Short Selling Portfolio Frontier"
    if(is.null(description)) description = .description()
    
    # Return Value:
    new("fPORTFOLIO",
        call = match.call(),
        specification = spec, 
        constraints = as.character(constraintsStrings),
        data = data, 
        portfolio = list(
            weights = weights, 
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetStdev = targetRisk), 
        title = title, 
        description = description)
}
        
    
################################################################################

