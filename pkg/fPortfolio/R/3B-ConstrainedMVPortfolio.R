
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
#  .tangencyConstrainedMVPortfolio    Returns constrained tangency MV-PF
#  .cmlConstrainedMVPortfolio         Returns constrained CML-Portfolio
#  .minvarianceConstrainedMVPortfolio Returns constrained min-Variance-PF
#  .efficientConstrainedMVPortfolio   Returns a constrained frontier MV-PF
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
    #       object or if not specified holding NA;
    #       $statistics holding a named two element list by itself, 
    #         $mu the location of the asset returns by default the mean and 
    #         $Sigma the scale of the asset returns by default the covariance
    #         matrix.
    
    # Note:
    #   In contrast to the functions *Portfolio(), which only require either the
    #   statistics or the series the functions .*Portfolio() require both as
    #   input

    # Example:
    #   .feasibleConstrainedMVPortfolio()
    
    # FUNCTION:

    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Setting the constraints matrix and vector:
    tmpConstraints = .setConstraints(data = data, spec = spec,
        constraints = constraints)
    dim = length(mu)
    A = tmpConstraints[, -(dim+1)]
    b0 = tmpConstraints[, (dim+1)]
    
    # Weights:
    weights = spec@portfolio$weights
    if(is.null(weights)) weights = rep(1/dim, times = dim)  
    names(weights) = names(mu)
    
    # Target Return and Risk:
    targetReturn = as.numeric(mu %*% weights)
    targetRisk = sqrt( as.numeric( t(weights) %*% Sigma %*% (weights) ) )
    
    # STILL TO DO:
    # Check constraints, if they are feasible:
    b0Test = A %*% weights
    b0Test = sum(b0Test)
    b0 = sum(b0)
    # if(b0 != b0Test) warning("Inconsistent Weights respectively Constraints")
    
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


#-------------------------------------------------------------------------------  

 
.cmlConstrainedMVPortfolio = 
function(data, spec, constraints)
{
    # Description:
    #   Computes Computes Risk, Return and Weight for CML portfolio
    
    # Note:
    #   Calls .efficientConstrainedMVPortfolio()
    #   Calls efficientPortfolio()
    
    # Example:
    #   .cmlConstrainedMVPortfolio()
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Function to be minimized:
    .sharpeRatioFun =
    function(x, data, spec, constraints) {
        spec@portfolio$targetReturn = x
        ans = .efficientConstrainedMVPortfolio(data = data, spec = spec,
            constraints = constraints)
        (x - spec@portfolio$riskFreeRate) / getTargetRisk(ans)      
    }
    
    # Calling optimize() function:
    cml = optimize(.sharpeRatioFun, interval = range(mu), maximum = TRUE,
        data = data, spec = spec, constraints = constraints,
        tol = .Machine$double.eps^0.5)
      
    # Compute Target Return and Risk:     
    targetReturn = spec@portfolio$targetReturn = cml$maximum  
    targetRisk = getTargetRisk(
        efficientPortfolio(data = data$statistics, spec, constraints)) 
        
    # Get Weights:
    weights = getWeights(efficientPortfolio(data = data$statistics,
        spec, constraints))
    names(weights) = names(mu)

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
        title = "CML Portfolio", 
        description = .description()) 
}


#-------------------------------------------------------------------------------


.tangencyConstrainedMVPortfolio = 
function(data, spec, constraints)
{
    # Description:
    #   Computes Risk, Return and Weight for the tangency portfolio
    
    # Note:
    #   Calls .cmlConstrainedMVPortfolio()
    #       Calls .efficientConstrainedMVPortfolio()
    
    # Example:
    #   .tangencyConstrainedMVPortfolio()
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
 
    # Set Risk Free Rate:
    spec@portfolio$riskFreeRate = 0
    
    # Calling cml function
    ans = .cmlConstrainedMVPortfolio(data, spec, constraints)
    ans@call = match.call()
    ans@title = "Tangency Portfolio"
    
    # Return value:
    ans
}


#-------------------------------------------------------------------------------

   
.minvarianceConstrainedMVPortfolio = 
function(data, spec, constraints)
{
    # Description:
    #   Computes Risk, Return and Weight for minimum variance portfolio
    
    # Note:
    #   Calls .efficientConstrainedMVPortfolio()
    #   Calls efficientPortfolio()
    
    # Example:
    #   .minvarianceConstrainedMVPortfolio()
    
    # FUNCTION:

    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Function to be minimized:
    .minVariancePortfolioFun = 
    function(x, data, spec, constraints) {
        spec@portfolio$targetReturn = x
        ans = .efficientConstrainedMVPortfolio(data = data, spec = spec,
            constraints = constraints)
        getTargetRisk(ans)
    }

    # Calling optimize function
    minVar = optimize(.minVariancePortfolioFun, interval = range(mu),
        data = data, spec = spec, constraints = constraints,
        tol = .Machine$double.eps^0.5)
    targetReturn = spec@portfolio$targetReturn = minVar$minimum
    targetRisk = getTargetRisk(efficientPortfolio(data = data$statistics, spec,
        constraints)) 
    weights = getWeights(efficientPortfolio(data = data$statistics, spec,
        constraints))
    names(weights) = names(mu)

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
        title = "Minimum Variance Portfolio", 
        description = .description()) 
}


#-------------------------------------------------------------------------------   


.efficientConstrainedMVPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

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
    #   Calls   solveRQuadprog()
    #   Calls   solveRDonlp2()

    # Example:
    #   .feasibleConstrainedMVPortfolio()
    
    # FUNCTION:
     
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Number of Assets:
    dim = length(mu)
    
    # Extracting data from spec:
    targetReturn = spec@portfolio$targetReturn  
    stopifnot(is.numeric(targetReturn)) 
    
    # Calling Solver:
    solver = spec@solver$type 
    if (solver == "RQuadprog") {
        ans = solveRQuadprog(data, spec, constraints) 
    } else if (solver == "RDonlp2") {
        ans = solveRDonlp2(data, spec, constraints)
    }
    
    # Setting all weights zero being smaler than the machine precision:
    for(i in 1:dim){
        if(abs(ans$solution[i]) < .Machine$double.eps){
            ans$solution[i] = 0
        }
    }
    weights = ans$solution

    # Converged ?
    attr(weights, "error") <- ans$ierr
    targetRisk = sqrt(weights %*% Sigma %*% weights)

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
        title = paste("Constrained MV Portfolio - Solver:", solver),
        description = .description())    
}


# ------------------------------------------------------------------------------


.portfolioConstrainedMVFrontier = 
function(data, spec, constraints)
{
    # Description:
    #   Evaluates the EF for a given set of box and or sector constraints
    
    # Arguments:
    #   data - portfolio of assets, a matrix or an object which can be 
    #       transformed to a matrix
    #   spec - specification of the portfolio
    #   constraints - string of constraints

    # FUNCTION:

    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma

    # Settings:
    returnRange = spec@portfolio$returnRange
    riskRange = spec@portfolio$riskRange
    nFrontierPoints = spec@portfolio$nFrontierPoints
    
    # Ranges for Mean and Standard Deviation:
    if (is.null(returnRange)){
        returnRange = range(mu) + 0.25*c(-diff(range(mu)), diff(range(mu)))
    }
    sqrtSig = sqrt(diag(Sigma))
    if (is.null(riskRange)){
        riskRange = c(min(sqrtSig), max(sqrtSig))+
        0.25*c(-diff(range(sqrtSig)), diff(range(sqrtSig)))
    }
    
    # Calculate Efficient Frontier:
    muMin = min(mu) # returnRange[1]
    muMax = max(mu) # returnRange[2]
    targetMu = targetSigma = nextWeights = rep(0, times = nFrontierPoints)
    weights = error = NULL

    # Loop over .efficientConstrainedMVPortfolio
    Spec = spec
    solver = spec@solver$type
    k = 0 
    for (nTargetReturn in seq(muMin, muMax, length = nFrontierPoints)) {
        k = k + 1  
        setTargetReturn(Spec)<-nTargetReturn    
        tmpObject = .efficientConstrainedMVPortfolio(
            data = data, spec = Spec, constraints = constraints)
        targetMu[k] = tmpObject@portfolio$targetReturn
        targetSigma[k] = tmpObject@portfolio$targetRisk
        nextWeights = tmpObject@portfolio$weights
        names(nextWeights) = names(mu)
        weights = rbind(weights, t(nextWeights))
        if (solver == "RQuadprog") {
            error = c(error, as.logical(attr(nextWeights, "error")))
        } else {
            error = c(error, FALSE)
        }
    }
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = data,
        specification = list(spec = spec),
        constraints = as.character(constraints),
        portfolio = list(
            weights = weights[!error, ],
            targetReturn = targetMu[!error],
            targetRisk = targetSigma[!error],
            targetMean = targetMu[!error],
            targetStdev = targetSigma[!error],
            error = error),
        title = "Constrained MV Frontier", 
        description = .description())       
}


################################################################################

