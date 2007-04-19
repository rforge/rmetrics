
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
#  .efficientShortMVPortfolio     Returns a frontier MV portfolio
# FUNCTION:                      PORTFOLIO FRONTIER:
#  .portfolioShortMVFrontier      Returns the EF of a short selling MV portfolio
################################################################################


.feasibleShortMVPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{
    # Description:
    #   Computes Risk and Return for a feasible portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Note:
    #   In contrast to the functions *Portfolio(), which only require either the
    #   statistics or the series the functions .*Portfolio() require both as
    #   input
    
    # Example:
    #   .feasibleShortMVPortfolio(engelsPortfolioData())
    
    # FUNCTION:
       
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
   
    # Get Weights:
    weights = spec@portfolio$weights
    N = length(mu)
    if (is.null(weights)) weights =  rep(1/N, times = N)
    names(weights) = names(mu)
    
    # Get Target Return:
    targetReturn = as.numeric(mu %*% weights)
    attr(targetReturn, "return") = spec@model$estimator[1]
    
    # Get Target Risk:
    targetRisk = sqrt( as.numeric( t(weights) %*% Sigma %*% (weights) ) )
    attr(targetRisk, "risk") <- spec@ model$estimator[2]
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = data,
        specification = list(spec = spec), 
        constraints = as.character(constraints),
        portfolio = list(
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetMean = targetReturn,
            targetStdev = targetRisk),
        title = "Feasible Portfolio", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.cmlShortMVPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{
    # Description:
    #   Computes capital market line
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .tangencyShortMVPortfolio(engelsPortfolioData())
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
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
    
    # Get Weights:
    weights = C0 * as.vector(invSigma %*% (mu - riskFreeRate) ) / B
    names(weights) = names(mu)
    
    # Get Target Return:
    targetReturn = A / B
    attr(targetReturn, "return") = spec@model$estimator[1]
    
    # Get Target Risk:
    targetRisk = sqrt(c*riskFreeRate^2 - 2*b*riskFreeRate + a) / B
    attr(targetRisk, "risk") <- spec@ model$estimator[2]
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = data,
        specification = list(spec = spec),
        constraints = as.character(constraints),
        portfolio = list(
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetMean = targetReturn,
            targetStdev = targetRisk),
        title = "Capital Market Line", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.tangencyShortMVPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{
    # Description:
    #   Computes target risk and weights for the tangency portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .tangencyShortMVPortfolio(engelsPortfolioData())
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
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
    
    # Get Weights:
    weights = C0 * as.vector(invSigma %*% mu ) / b 
    names(weights) = names(mu)
    
    # Get Target Return:
    targetReturn = (a/b)*C0
    attr(targetReturn, "return") = spec@model$estimator[1]
    
    # Get Target Risk:
    targetRisk = (sqrt(a)/b)*C0
    attr(targetRisk, "risk") <- spec@ model$estimator[2]
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = data,
        specification = list(spec = spec),
        constraints = as.character(constraints),
        portfolio = list(
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetMean = targetReturn,
            targetStdev = targetRisk),
        title = "Tangency MV Portfolio", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.minvarianceShortMVPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{
    # Description:
    #   Computes target risk and weights for the minimum variance portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .minvarianceShortMVPortfolio(engelsPortfolioData())
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
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
    
    # Get Weights:
    weights = as.vector(invSigma %*% ((a-b*mu)*C0 + (c*mu-b)*(b/c)*C0 )/d)
    names(weights) = names(mu)
    
    # Get Target Return:
    targetReturn = (b/c)*C0
    attr(targetReturn, "return") = spec@model$estimator[1]
    
    # Get Target Risk:
    targetRisk = C0/sqrt(c)
    attr(targetRisk, "risk") = spec@model$estimator[2]

    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        specification = list(spec = spec),
        constraints = as.character(constraints),
        data = data, 
        portfolio = list(
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetMean = targetReturn,
            targetStdev = targetRisk),
        title = "Minimum Variance MV Portfolio", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.efficientShortMVPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{
    # Description:
    #   Computes target risk and weights for an efficient portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .efficientShortMVPortfolio(engelsPortfolioData())
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
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
    
    # Get Target Return:
    targetReturn = spec@portfolio$targetReturn 
    if (is.null(targetReturn))  
        targetReturn = getTargetReturn(.tangencyShortMVPortfolio(data, spec))
    attr(targetReturn, "return") = spec@model$estimator[1]
    
    # Get Target Risk:
    targetRisk = sqrt((c*targetReturn^2 - 2*b*C0*targetReturn + a*C0^2) / d)
    attr(targetRisk, "risk") = spec@model$estimator[2]
    
    # Get Weights:
    weights = as.vector(invSigma %*% ((a-b*mu)*C0 + (c*mu-b)*targetReturn )/d)
    names(weights) = names(mu)
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        specification = list(spec = spec),
        constraints = as.character(constraints),
        data = data, 
        portfolio = list(
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetMean = targetReturn,
            targetStdev = targetRisk),
        title = "Frontier MV Portfolio", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.portfolioShortMVFrontier = 
function(data, spec = portfolioSpec(), constraints = NULL,
title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the efficient frontier, short selling allowed
    
    # Details  from a matrix
    #   Calculates the efficient frontier (short selling allowed) from a
    #   a matrix of either market or simulated assets given in matrix "x". 
    #   Each time series represents a column in this matrix.
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .shortMVFrontier(engelsPortfolioData())
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
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
            0.25*c(-diff(range(sqrtSig)), diff(range(sqrtSig)))
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
        nextWeight = getWeights(.efficientShortMVPortfolio(data, Spec))
        weights = rbind(weights, t(nextWeight))
    }
         
    # Adding title and description:
    if(is.null(title)) title = "Short Selling Portfolio Frontier"
    if(is.null(description)) description = .description()
    
    # Return Value:
    new("fPORTFOLIO",
        call = match.call(),
        specification = list(spec = spec),
        constraints = as.character(constraints),
        data = data, 
        portfolio = list(
            weights = weights, 
            targetReturn = targetReturn,
            targetRisk = targetRisk,
            targetMean = targetReturn,
            targetStdev = targetRisk), 
        title = title, 
        description = description)
}
        
    
################################################################################

