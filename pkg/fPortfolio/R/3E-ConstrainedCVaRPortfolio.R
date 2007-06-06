
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
# FUNCTION:                            SINGLE PORTFOLIOS:
#  .feasibleConstrainedCVaRPortfolio    Returns constrained feasible M-CVaR PF
#  .efficientConstrainedCVaRPortfolio   Returns constrained frontier M-CVaR PF
#  .cmlConstrainedCVaRPortfolio         Returns constrained CML M-CVaR PF
#  .tangencyConstrainedCVaRPortfolio    Returns constrained tangency M-CVaR PF
#  .minvarianceConstraineCVaRPortfolio  Returns constrained min-Var M-CVaR PF
# FUNCTION:                            PORTFOLIO FRONTIER:
#  .portfolioConstrainedCVaRFrontier    Returns EF of a constrained M-CVaR PF
################################################################################


.feasibleConstrainedCVaRPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes Risk and Return for a feasible portfolio
   
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .feasibleConstrainedCVaRPortfolio()
    
    # FUNCTION:

    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    nAssets = length(mu)
    
    # Get or Set Target Alpha:
    targetAlpha = spec@portfolio$targetAlpha
    if (is.null(targetAlpha)) targetAlpha = 0.05
    
    # Get or Set Weights:
    weights = spec@portfolio$weights
    if(is.null(weights)) weights = rep(1/nAssets, times = nAssets)  
    names(weights) = names(mu)
    
    # Compute Mean Target Return:
    targetReturn = as.numeric(mu %*% weights)
    names(targetReturn) = "mean"
    
    # Compute Covariance and CVaR Target Risk:
    covTargetRisk = sqrt( as.numeric( weights %*% Sigma %*% weights ) )
    x = data$series@Data %*% weights
    VaR = quantile(x, targetAlpha, type = 1)
    CVaR = VaR - 0.5*mean(((VaR-x) + abs(VaR-x))) / targetAlpha
    targetRisk = c(CVaR, VaR, covTargetRisk)
    names(targetRisk) <- 
        c(paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""), "cov")
     
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
            targetAlpha = targetAlpha,
            status = 0),
        title = "Feasible CVaR Portfolio", 
        description = .description()) 
}


#-------------------------------------------------------------------------------   


.efficientConstrainedCVaRPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes CVaR-Risk and Mean-Return for an efficient portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .efficientConstrainedCVaRPortfolio()
    
    # FUNCTION:
     
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    nAssets = length(mu)
    
    # Get or Set Target Alpha:
    targetAlpha = spec@portfolio$targetAlpha
    if (is.null(targetAlpha)) targetAlpha = 0.05
    
    # Optimize Portfolio - Requires R Package lpSolve:
    stopifnot(spec@solver$type == "RlpSolve")
    solver = spec@solver$type
    ans = solveRlpSolve(data, spec, constraints) 
    
    # Extract weights - force small weights to be zero:
    weights = ans$weights
    attr(weights, "error") <- ans$ierr
    names(weights) = names(mu)
    
    # Extract Target Return from Specification:
    targetReturn = spec@portfolio$targetReturn  
    stopifnot(is.numeric(targetReturn)) 
    names(targetReturn) <- spec@model$estimator[1]
   
    # Compute Covariance and CVaR Target Risk:
    covTargetRisk = sqrt(as.numeric(weights %*% Sigma %*% weights))
    VaRTargetRisk = ans$VaR
    CVaRTargetRisk = ans$CVaR
    targetRisk = c(CVaRTargetRisk, VaRTargetRisk, covTargetRisk)
    names(targetRisk) <- 
        c(paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""), "cov")

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
            targetAlpha = targetAlpha,
            status = ans$status),
        title = paste("Constrained CVaR Portfolio - Solver:", solver),
        description = .description())    
}


#-------------------------------------------------------------------------------  

 
.cmlConstrainedCVaRPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes Computes Risk, Return and Weight for CML portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .cmlConstrainedCVaRPortfolio()
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma

    # Get or Set Target Alpha:
    targetAlpha = spec@portfolio$targetAlpha
    if (is.null(targetAlpha)) targetAlpha = 0.05
    
    # Function to be minimized:
    .sharpeRatioFun =
    function(x, data, spec, constraints) {
        spec@portfolio$targetReturn = x
        ans = .efficientConstrainedCVaRPortfolio(data = data, spec = spec,
            constraints = constraints)
        f = (x - spec@portfolio$riskFreeRate) / getTargetRisk(ans)[3]  
        attr(f, "targetRisk") <- getTargetRisk(ans)   
        attr(f, "weights") <- getWeights(ans) 
        f   
    }
    
    # Calling optimize() function:
    cml = optimize(.sharpeRatioFun, interval = range(mu), maximum = TRUE,
        data = data, spec = spec, constraints = constraints,
        tol = .Machine$double.eps^0.5)
        
    # Weights:
    weights = attr(cml$objective, "weights")
    names(weights) = names(mu)
    
    # Target Return:     
    targetReturn = spec@portfolio$targetReturn = cml$maximum  
    names(targetReturn) <- spec@model$estimator[1]
     
    # Compute Covariance and CVaR Target Risk:
    covTargetRisk = sqrt(as.numeric(weights %*% Sigma %*% weights))
    VaRTargetRisk = attr(cml$objective, "targetRisk")[2]
    CVaRTargetRisk = attr(cml$objective, "targetRisk")[1]
    targetRisk = c(CVaRTargetRisk, VaRTargetRisk, covTargetRisk)
    names(targetRisk) <- 
        c(paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""), "cov")

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
            targetAlpha = targetAlpha,
            status = NA),
        title = "CML CVaR Portfolio", 
        description = .description()) 
}

       
#-------------------------------------------------------------------------------


.tangencyConstrainedCVaRPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes Risk, Return and Weight for the tangency portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Note:
    #   Calls function .cmlConstrainedCVaRPortfolio() with zero 
    #   risk free rate.
    
    # Example:
    #   .tangencyConstrainedCVaRPortfolio()
    
    # FUNCTION:
 
    # Set Risk Free Rate:
    setRiskFreeRate(Spec) <- 0
    
    # Call cmlPorfolio unction:
    ans = .cmlConstrainedCVaRPortfolio(data, spec, constraints)
    ans@call = match.call()
    ans@title = "Tangency Portfolio"
    
    # Return value:
    ans
}        
        

#-------------------------------------------------------------------------------

   
.minvarianceConstrainedCVaRPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes Risk, Return and Weight for minimum variance portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .minvarianceConstrainedCVaRPortfolio()
    
    # FUNCTION:

    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
        
    # Get or Set Target Alpha:
    targetAlpha = spec@portfolio$targetAlpha
    if (is.null(targetAlpha)) targetAlpha = 0.05
    
    # Function to be Minimized:
    .minVariancePortfolioFun = 
    function(x, data, spec, constraints) {
        spec@portfolio$targetReturn = x
        ans = .efficientConstrainedCVaRPortfolio(
            data = data, spec = spec, constraints = constraints)
        f = getTargetRisk(ans)[1]
        attr(f, "targetReturn") <- getTargetReturn(ans)  
        attr(f, "targetRisk") <- getTargetRisk(ans)  
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
        attr(minVar$objective, "targetReturn")
    names(targetReturn) <- spec@model$estimator[1]
    
    # Get Target Risk:
    targetRisk = attr(minVar$objective, "targetRisk")  
    names(targetRisk) <-      
        c(paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""), "cov")

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
            targetAlpha = targetAlpha,
            status = 0),
        title = "Minimum CVaR Portfolio", 
        description = .description()) 
}


################################################################################


.portfolioConstrainedCVaRFrontier = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Evaluates the EF for a given set of box and or sector constraints
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints

    # FUNCTION:

    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    nAssets = length(mu)
       
    # Get or Set Target Alpha:
    targetAlpha = spec@portfolio$targetAlpha
    if (is.null(targetAlpha)) targetAlpha = 0.05
    
    # Settings:
    nFrontierPoints = spec@portfolio$nFrontierPoints
    
    # Calculate Efficient Frontier:
    muMin = min(mu) 
    muMax = max(mu)  
    targetMu = nextWeights = rep(0, times = nFrontierPoints)
    targetSigma1 = targetSigma2 = targetSigma3 = targetMu
    weights = error = status = NULL
    
    # Loop over .efficientConstrainedCVaRPortfolio():
    Spec = spec
    stopifnot(spec@solver$type == "RlpSolve")
    solver = spec@solver$type
    k = 0 
    for (nTargetReturn in seq(muMin, muMax, length = nFrontierPoints)) {
        k = k + 1  
        setTargetReturn(Spec) <- nTargetReturn    
        tmpObject = .efficientConstrainedCVaRPortfolio(
            data = data, spec = Spec, constraints = constraints)
        targetMu[k] = tmpObject@portfolio$targetReturn
        targetSigma1[k] = tmpObject@portfolio$targetRisk[1]
        targetSigma2[k] = tmpObject@portfolio$targetRisk[2]
        targetSigma3[k] = tmpObject@portfolio$targetRisk[3]
        nextWeights = tmpObject@portfolio$weights
        names(nextWeights) = names(mu)
        weights = rbind(weights, t(nextWeights))
        status[k] = tmpObject@portfolio$status
    }  
    
    # Get Weights:
    weights = weights 
    colnames(weights) = names(mu)
    
    # Get Target Risk:
    targetReturn = targetMu 
    names(targetReturn) <- NULL # spec@model$estimator[1]
    
    # Get Target Risk:
    targetRisk1 = targetSigma1 
    names(targetRisk1) <- "CVaR"
    targetRisk2 = targetSigma2 
    names(targetRisk2) <- "VaR"
    targetRisk3 = targetSigma3 
    names(targetRisk3) <- "cov"
    targetRisk = cbind(targetRisk1, targetRisk2, targetRisk3) 
    rownames(targetRisk) <- NULL
    colnames(targetRisk) <- 
        c(paste(c("CVaR.", "VaR."), targetAlpha*100, "%", sep = ""), "cov")
    
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
            targetAlpha = targetAlpha,
            status = status),
        title = "Constrained CVaR Frontier", 
        description = .description())       
}

  
################################################################################

