
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
#  .efficientShortMVPortfolio     Returns a frontier MV portfolio
#  .cmlShortMVPortfolio           Returns a capital market line
#  .tangencyShortMVPortfolio      Returns the tangency MV portfolio
#  .minvarianceShortMVPortfolio   Returns the minimum variance portfolio
# FUNCTION:                      PORTFOLIO FRONTIER:
#  .portfolioShortMVFrontier      Returns the EF of a short selling MV portfolio
################################################################################


.feasibleShortMVPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics

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
    mu = getStatistics(data)$mu
    Sigma = getStatistics(data)$Sigma
    nAssets = getNumberOfAssets(data)
    
    # Get Alpha:
    targetAlpha = getTargetAlpha(spec)
    
    # Get Weights:
    weights = getWeights(spec)
    if(is.null(weights)) weights = rep(1/nAssets, times = nAssets)  
    names(weights) = names(mu)
    
    # Target Return:
    targetReturn = matrix(as.numeric(mu %*% weights), nrow = 1)
    colnames(targetReturn) <- getEstimator(spec)[1]
    
    # Compute Target Risks:
    covTargetRisk = sqrt( as.numeric( weights %*% Sigma %*% weights ) )
    x = getSeries(data)@Data %*% weights
    VaR = quantile(x, targetAlpha, type = 1)
    CVaR = VaR - 0.5*mean(((VaR-x) + abs(VaR-x))) / targetAlpha
    targetRisk = matrix(c(covTargetRisk, CVaR, VaR), nrow = 1)
    colnames(targetRisk) <- 
        c("cov", paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""))
   
    # Status:
    status = 0
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = list(data = data), 
        spec = list(spec = spec),
        constraints = as.character(constraints),
        portfolio = list(
            weights = weights,  
            targetReturn = targetReturn, 
            targetRisk = targetRisk,
            targetAlpha = targetAlpha,
            status = status),
        title = "Feasible Portfolio", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.efficientShortMVPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics

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
    mu = getStatistics(data)$mu
    Sigma = getStatistics(data)$Sigma
    nAssets = getNumberOfAssets(data)
    
    # Get or Set Target Alpha:
    targetAlpha = getTargetAlpha(spec)
    
    # Parameter Settings:
    C0 = 1
    one = rep(1, times = length(mu))
    invSigma = solve(Sigma)
    a = as.numeric(mu %*% invSigma %*% mu)
    b = as.numeric(mu %*% invSigma %*% one)
    c = as.numeric(one %*% invSigma %*% one)
    d = as.numeric(a*c - b^2)
    
    # Compute Target Return:
    targetReturn = getTargetReturn(spec) 
    if (is.null(targetReturn))  
        targetReturn = getTargetReturn(.tangencyShortMVPortfolio(data, spec))
    targetReturn = matrix(targetReturn, nrow = 1)
    colnames(targetReturn) = getEstimator(spec)[1]
    
    # Get Weights:
    weights = as.vector(invSigma %*% ((a-b*mu)*C0 + (c*mu-b)*targetReturn )/d)
    names(weights) = names(mu)
    
    # Compute Target Risk:
    covTargetRisk = sqrt((c*targetReturn^2 - 2*b*C0*targetReturn + a*C0^2) / d)
    x = getSeries(data)@Data %*% weights
    VaR = quantile(x, targetAlpha, type = 1)
    CVaR = VaR - 0.5*mean(((VaR-x) + abs(VaR-x))) / targetAlpha
    targetRisk = matrix(c(covTargetRisk, CVaR, VaR), nrow = 1)
    colnames(targetRisk) <- 
        c("cov", paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""))
    
    # Status:
    status = 0
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = list(data = data), 
        spec = list(spec = spec),
        constraints = as.character(constraints),
        portfolio = list(
            weights = weights,  
            targetReturn = targetReturn, 
            targetRisk = targetRisk,
            targetAlpha = targetAlpha,
            status = status),
        title = "Frontier MV Portfolio", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.cmlShortMVPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes capital market line
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .cmlShortMVPortfolio(engelsPortfolioData())
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = getStatistics(data)$mu
    Sigma = getStatistics(data)$Sigma
    nAssets = getNumberOfAssets(data)
    
    # Get or Set Target Alpha:
    targetAlpha = getTargetAlpha(spec)
    
    # Risk-Free Rate:
    riskFreeRate = getRiskFreeRate(spec)
    
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
    targetReturn = matrix(targetReturn, nrow = 1)
    colnames(targetReturn) = getEstimator(spec)[1]
    
    # Get Target Risk:
    covTargetRisk = sqrt(c*riskFreeRate^2 - 2*b*riskFreeRate + a) / B
    x = getSeries(data)@Data %*% weights
    VaR = quantile(x, targetAlpha, type = 1)
    CVaR = VaR - 0.5*mean(((VaR-x) + abs(VaR-x))) / targetAlpha
    targetRisk = matrix(c(covTargetRisk, CVaR, VaR), nrow = 1)
    colnames(targetRisk) <- 
        c("cov", paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""))
    
    # Status:
    status = 0
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = list(data = data), 
        spec = list(spec = spec),
        constraints = as.character(constraints),
        portfolio = list(
            weights = weights,  
            targetReturn = targetReturn, 
            targetRisk = targetRisk,
            targetAlpha = targetAlpha,
            status = status),
        title = "Capital Market Line", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.tangencyShortMVPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics

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
    mu = getStatistics(data)$mu
    Sigma = getStatistics(data)$Sigma
    nAssets = getNumberOfAssets(data)
    
    # Get or Set Target Alpha:
    targetAlpha = getTargetAlpha(spec)
    
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
    targetReturn = matrix(targetReturn, nrow = 1)
    colnames(targetReturn) = spec@model$estimator[1]
    
    # Get Target Risk:
    covTargetRisk = (sqrt(a)/b)*C0
    x = getSeries(data)@Data %*% weights
    VaR = quantile(x, targetAlpha, type = 1)
    CVaR = VaR - 0.5*mean(((VaR-x) + abs(VaR-x))) / targetAlpha
    targetRisk = matrix(c(covTargetRisk, CVaR, VaR), nrow = 1)
    colnames(targetRisk) <- 
        c("cov", paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""))
        
    # Status:
    status = 0
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = list(data = data), 
        spec = list(spec = spec),
        constraints = as.character(constraints),
        portfolio = list(
            weights = weights,  
            targetReturn = targetReturn, 
            targetRisk = targetRisk,
            targetAlpha = targetAlpha,
            status = status),
        title = "Tangency MV Portfolio", 
        description = .description())  
}


# ------------------------------------------------------------------------------


.minvarianceShortMVPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics

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
    mu = getStatistics(data)$mu
    Sigma = getStatistics(data)$Sigma
    nAssets = getNumberOfAssets(data)
    
    # Get or Set Target Alpha:
    targetAlpha = getTargetAlpha(spec)
    
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
    targetReturn = matrix(targetReturn, nrow = 1)
    colnames(targetReturn) = getEstimator(spec)[1]
    
    # Get Target Risk:
    covTargetRisk = C0/sqrt(c)
    x = getSeries(data)@Data %*% weights
    VaR = quantile(x, targetAlpha, type = 1)
    CVaR = VaR - 0.5*mean(((VaR-x) + abs(VaR-x))) / targetAlpha
    targetRisk = matrix(c(covTargetRisk, CVaR, VaR), nrow = 1)
    colnames(targetRisk) <- 
        c("cov", paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""))

    # Status:
    status = 0
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = list(data = data), 
        spec = list(spec = spec),
        constraints = as.character(constraints),
        portfolio = list(
            weights = weights,  
            targetReturn = targetReturn, 
            targetRisk = targetRisk,
            targetAlpha = targetAlpha,
            status = status),
        title = "Minimum Variance MV Portfolio", 
        description = .description())  
}


################################################################################


.portfolioShortMVFrontier = 
function(data, spec = portfolioSpec(), constraints = NULL)
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
    mu = getStatistics(data)$mu
    Sigma = getStatistics(data)$Sigma
    nAssets = getNumberOfAssets(data)
    
    # Get or Set Target Alpha:
    targetAlpha = getTargetAlpha(spec)
    
    # Specification:
    riskFreeRate = getRiskFreeRate(spec)
    nFrontierPoints = getNFrontierPoints(spec)
    
    # Parameter Settings:
    C0 = 1
    one = rep(1, times = length(mu))
    invSigma = solve(Sigma)
    a = as.numeric(mu %*% invSigma %*% mu)
    b = as.numeric(mu %*% invSigma %*% one)
    c = as.numeric(one %*% invSigma %*% one)
    d = as.numeric(a*c - b^2)
    
    # Ranges for mean and Standard Deviation:
    muRange = range(mu)+ 0.25*c(-diff(range(mu)), diff(range(mu)))
    sqrtSig = sqrt(diag(Sigma))
    sigmaRange = c(min(sqrtSig), max(sqrtSig))+
        0.25*c(-diff(range(sqrtSig)), diff(range(sqrtSig)))
               
    # Efficient Frontier Portfolios:
    targetReturn = seq(muRange[1], muRange[2], length = nFrontierPoints)
    targetReturn = as.vector(targetReturn)
    targetRisk = sqrt((c*targetReturn^2 - 2*b*C0*targetReturn + a*C0^2)/d)
    covTargetRisk = as.vector(targetRisk)
    weights = NULL
    Spec = spec
    series = getSeries(data)@Data
    targetRisk = NULL
    for (i in 1:nFrontierPoints) {
        Spec@portfolio$targetReturn = targetReturn[i]
        nextWeight = getWeights(.efficientShortMVPortfolio(data, Spec))
        weights = rbind(weights, t(nextWeight))    
        # Get Target Risk:
        x = series %*% nextWeight
        nextVaR = quantile(x, targetAlpha, type = 1)
        nextCVaR = nextVaR-0.5*mean(((nextVaR-x)+abs(nextVaR-x))) / targetAlpha
        nextTargetRisk = matrix(c(covTargetRisk[i], nextCVaR, nextVaR), nrow = 1)
        targetRisk = rbind(targetRisk, nextTargetRisk)        
    }
     colnames(targetRisk) <- 
        c("cov", paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""))

    # Get TargetReturn:
    targetReturn = matrix(targetReturn, ncol = 1)
    colnames(targetReturn) = getEstimator(spec)[1]
    
    # Get Target Risk:
    colnames(targetRisk) <- 
        c("cov", paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""))
    
    # Status:
    status = 0
   
    # Return Value:
    new("fPORTFOLIO",
        call = match.call(),
        data = list(data = data), 
        spec = list(spec = spec),
        constraints = as.character(constraints),
        portfolio = list(
            weights = weights,  
            targetReturn = targetReturn, 
            targetRisk = targetRisk,
            targetAlpha = targetAlpha,
            status = status),
        title = "Short Selling Portfolio Frontier", 
        description = .description())
}
        
    
################################################################################

