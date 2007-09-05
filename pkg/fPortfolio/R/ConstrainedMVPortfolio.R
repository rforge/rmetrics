
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
# FUNCTION:                          SINGLE PORTFOLIOS:
#  .feasibleConstrainedMVPortfolio    Returns a constrained feasible MV-PF
#  .efficientConstrainedMVPortfolio   Returns a constrained frontier MV-PF
#  .cmlConstrainedMVPortfolio         Returns constrained CML-Portfolio
#  .tangencyConstrainedMVPortfolio    Returns constrained tangency MV-PF
#  .minvarianceConstrainedMVPortfolio Returns constrained min-Variance-PF
# FUNCTION:                          PORTFOLIO FRONTIER:
#  .portfolioConstrainedMVFrontier    Returns the EF of a constrained MV-PF
################################################################################


.feasibleConstrainedMVPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes Risk and Return for a feasible portfolio
    
    # Arguments:
    #   data - a list with two named elements. 
    #       $series holding the time series which may be any rectangular,
    #           object or if not specified holding NA;
    #       $statistics holding a named two element list by itself, 
    #           $mu the location of the asset returns by default the mean  
    #           and $Sigma the scale of the asset returns by default the 
    #           covariance matrix.
    #   spec - specification object of the portfolio
    #   constraints - string value or vector of constraints
    
    # Note:
    #   In contrast to the functions *Portfolio(), which only require either 
    #   the statistics or the series the functions .*Portfolio() require both 
    #   as input.

    # Example:
    #   .feasibleConstrainedMVPortfolio()
    
    # FUNCTION:

    # Create Data Object:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
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
            status = 0),
        title = "Feasible Portfolio", 
        description = .description()) 
}


#-------------------------------------------------------------------------------   


.efficientConstrainedMVPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes Risk and Return for a feasible portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints

    # Example:
    #   .efficientConstrainedMVPortfolio()
    
    # FUNCTION:
     
    # Create Data Object:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
    nAssets = getNumberOfAssets(data)
    
    # Get Alpha:
    targetAlpha = getTargetAlpha(spec)
    
    # Calling Solver:
    solver = getSolver(spec)
    stopifnot(solver == "quadprog" | solver == "Rdonlp2")
    if (solver == "quadprog") {
        portfolio = solveRQuadprog(data, spec, constraints) 
    } else if (solver == "Rdonlp2") {
        portfolio = solveRDonlp2(data, spec, constraints)
    }
    
    # Get Weights:
    weights = portfolio$weights
    attr(weights, "status") <- portfolio$status
    names(weights) = names(mu)
    
    # Get Target Risk:
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
            status = portfolio$status),
        title = paste("Constrained MV Portfolio - Solver:", solver),
        description = .description())    
}


#-------------------------------------------------------------------------------  

 
.cmlConstrainedMVPortfolio = 
function(data, spec, constraints)
{
    # Description:
    #   Computes Computes Risk, Return and Weight for CML portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Note:
    #   Calls .efficientConstrainedMVPortfolio()
    #   Calls efficientPortfolio()
    
    # Example:
    #   .cmlConstrainedMVPortfolio()
    
    # FUNCTION:
    
    # Create Data Object:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
    nAssets = getNumberOfAssets(data)

    # Get or Set Target Alpha:
    targetAlpha = getTargetAlpha(spec)
    
    # Compose function to be minimized:
    .sharpeRatioFun =
    function(x, data, spec, constraints) {
        spec@portfolio$targetReturn = x
        ans = .efficientConstrainedMVPortfolio(data = data, spec = spec,
            constraints = constraints)
        f = (x - spec@portfolio$riskFreeRate) / getTargetRisk(ans)[1]  
        attr(f, "targetRisk") <- getTargetRisk(ans)[1]  
        attr(f, "weights") <- getWeights(ans) 
        attr(f, "status") <- ans@portfolio$status
        f 
    }
    
    # Optimize Sharpe Ratio:
    cml = optimize(.sharpeRatioFun, interval = range(mu), maximum = TRUE,
        data = data, spec = spec, constraints = constraints,
        tol = .Machine$double.eps^0.5)
      
    # Get Weights:
    weights = attr(cml$objective, "weights")
    names(weights) = names(mu)
    
    # Get Status:
    status = attr(cml$objective, "status")

    # Get Target Return:     
    targetReturn = spec@portfolio$targetReturn = as.numeric(cml$maximum)  
    targetReturn = matrix(targetReturn, nrow = 1) 
    colnames(targetReturn) <- getEstimator(spec)[1]
    
    # Compute Target Risks:
    covTargetRisk = as.numeric(attr(cml$objective, "targetRisk"))
    x = getSeries(data)@Data %*% weights
    VaR = quantile(x, targetAlpha, type = 1)
    CVaR = VaR - 0.5*mean(((VaR-x) + abs(VaR-x))) / targetAlpha
    targetRisk = matrix(c(covTargetRisk, CVaR, VaR), nrow = 1)
    colnames(targetRisk) <- 
        c("cov", paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""))
        
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
        title = "CML Portfolio", 
        description = .description()) 
}


#-------------------------------------------------------------------------------


.tangencyConstrainedMVPortfolio = 
function(data, spec, constraints)
{
    # Description:
    #   Computes Risk, Return and Weight for the tangency portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Note:
    #   Calls .cmlConstrainedMVPortfolio()
    #       Calls .efficientConstrainedMVPortfolio()
    
    # Example:
    #   .tangencyConstrainedMVPortfolio()
    
    # FUNCTION:
 
    # Set Risk Free Rate:
    spec@portfolio$riskFreeRate = 0
    
    # Call cmlPorfolio unction:
    ans = .cmlConstrainedMVPortfolio(data, spec, constraints)
    ans@call = match.call()
    ans@title = "Tangency Portfolio"
    
    # Return Value:
    ans
}


#-------------------------------------------------------------------------------

   
.minvarianceConstrainedMVPortfolio = 
function(data, spec, constraints)
{
    # Description:
    #   Computes Risk, Return and Weight for minimum variance portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Note:
    #   Calls .efficientConstrainedMVPortfolio()
    #   Calls efficientPortfolio()
    
    # Example:
    #   .minvarianceConstrainedMVPortfolio()
    
    # FUNCTION:

    # Create Data Object:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
    nAssets = getNumberOfAssets(data)

    # Get or Set Target Alpha:
    targetAlpha = getTargetAlpha(spec)
    
    # Compose Function to be Minimized:
    .minVariancePortfolioFun = 
    function(x, data, spec, constraints) {
        spec@portfolio$targetReturn = x
        ans = .efficientConstrainedMVPortfolio(data = data, spec = spec,
            constraints = constraints)
        f = getTargetRisk(ans)[1]
        attr(f, "targetReturn") <- getTargetReturn(ans)   
        attr(f, "targetRisk") <- getTargetRisk(ans)[1]  
        attr(f, "weights") <- getWeights(ans) 
        f
    }

    # Optimize Minimum Risk Function:
    minVar = optimize(.minVariancePortfolioFun, interval = range(mu),
        data = data, spec = spec, constraints = constraints,
        tol = .Machine$double.eps^0.5)
        
    # Get Weights:  
    weights = attr(minVar$objective, "weights")
    names(weights) = names(mu)
    
    # Get Target Return:
    targetReturn = spec@portfolio$targetReturn = 
        as.numeric(attr(minVar$objective, "targetReturn"))
    targetReturn = matrix(targetReturn, nrow = 1) 
    colnames(targetReturn) <- spec@model$estimator[1]
    
    # Get Target Risk:
    # targetRisk = as.numeric(minVar$objective)      
    # names(targetRisk) <- spec@model$estimator[2]
    
    # Compute Target Risks:
    covTargetRisk = as.numeric(attr(minVar$objective, "targetRisk"))
    x = getSeries(data)@Data %*% weights
    VaR = quantile(x, targetAlpha, type = 1)
    CVaR = VaR - 0.5*mean(((VaR-x) + abs(VaR-x))) / targetAlpha
    targetRisk = matrix(c(covTargetRisk, CVaR, VaR), nrow = 1)
    colnames(targetRisk) <- 
        c("cov", paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""))
    
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
            status = 0),
        title = "Minimum Variance Portfolio", 
        description = .description()) 
}


################################################################################


.portfolioConstrainedMVFrontier = 
function(data, spec, constraints)
{
    # Description:
    #   Evaluates the EF for a given set of box and or sector constraints
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints

    # FUNCTION:

    # Create Data Object:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
    nAssets = getNumberOfAssets(data)

    # Get or Set Target Alpha:
    targetAlpha = getTargetAlpha(spec)
    
    # Settings:
    nFrontierPoints = getNFrontierPoints(spec)
    
    # Calculate Efficient Frontier:
    # targetMu = targetSigma = nextWeights = rep(0, times = nFrontierPoints)
    # targetWeights = error = NULL
    targetReturn = targetRisk = targetWeights = error = NULL

    # Loop over .efficientConstrainedMVPortfolio
    Spec = spec
    solver = spec@solver$solver
    # Start Weights:
    Spec@portfolio$weights = rep(1/nAssets, nAssets)
    k = 0 
    solverType = spec@solver$solver
    status = NULL
    for (nTargetReturn in seq(min(mu), max(mu), length = nFrontierPoints)) {
        
        k = k + 1  
        setTargetReturn(Spec) <- nTargetReturn     
        nextPortfolio = .efficientConstrainedMVPortfolio(
            data = data, spec = Spec, constraints = constraints)
        
        # Start Weights for Donlp2:
        Spec@portfolio$weights = nextPortfolio@portfolio$weights
        
        # Target Return and Risk:
        targetReturn = rbind(targetReturn, nextPortfolio@portfolio$targetReturn)
        
        # Target Return and Risk:
        targetRisk = rbind(targetRisk, nextPortfolio@portfolio$targetRisk)
        
        nextWeights = nextPortfolio@portfolio$weights
        names(nextWeights) = names(mu)
        
        # Status:
        status = c(status, nextPortfolio@portfolio$status)
        targetWeights = rbind(targetWeights, t(nextWeights))
    }
    
    Index = (1:length(status))[status == 0]
    
    # Get Weights:
    weights = targetWeights
    colnames(weights) = names(mu)
    weights = weights[Index, ]
    
    # Get TargetReturn:
    DIM = dim(targetReturn)[2]
    targetReturn = targetReturn[Index, ]
    targetReturn = matrix(targetReturn, ncol = DIM)
    colnames(targetReturn) = getEstimator(spec)[1]
    
    # Get Target Risk:
    targetRisk = targetRisk[Index, ]
  
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
        title = "Constrained MV Frontier", 
        description = .description())       
}


################################################################################

