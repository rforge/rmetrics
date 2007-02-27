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
#  .frontierConstrainedMVPortfolio    Returns a constrained frontier MV-PF
# FUNCTION:                          PORTFOLIO FRONTIER:
#  .portfolioConstrainedMVFrontier    Returns the EF of a constrained MV-PF
################################################################################


.feasibleConstrainedMVPortfolio = 
function(data, spec, constraintsStrings)
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
    #   .feasibleConstrainedMVPortfolio()
    
    # FUNCTION:

    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Setting the constraints matrix and vector:
    tmp.ans = setConstraints(data = data, spec = spec,
        constraintsStrings =  constraintsStrings)
    dim = length(mu)
    A = tmp.ans[, -(dim+1)]
    b0 = tmp.ans[, (dim+1)]
    
    weights = spec@portfolio$weights
    if(is.null(weights)) weights = rep(1/dim, times = dim)  

    b0Test = A %*% weights
    b0Test = sum(b0Test)
    b0 = sum(b0)
    targetReturn = as.numeric(mu %*% weights)
    targetRisk = sqrt( as.numeric( t(weights) %*% Sigma %*% (weights) ) )

    
    # Check constraints, if it is feasible:
    if(b0 != b0Test) warning("Inconsistent Weights respectively Constraints")
   
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
            targetMean = targetReturn,
            targetStdev = targetRisk),
        title = "Feasible Portfolio", 
       description = .description()) 
}


#-------------------------------------------------------------------------------  

 
.cmlConstrainedMVPortfolio = 
function(data, spec, constraintsStrings)
{
    # Description:
    #   Computes Computes Risk, Return and Weight for CML portfolio
    
    # Example:
    #   .cmlConstrainedMVPortfolio()
    
    # FUNCTION:
    
    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Function to be minimized:
    .sharpeRatioFun =
    function(x, data, spec, constraintsStrings) {
        spec@portfolio$targetReturn = x
        ans = .frontierConstrainedMVPortfolio(data = data, spec = spec,
            constraintsStrings = constraintsStrings)
        (x - spec@portfolio$riskFreeRate) / getTargetRisk(ans)      
    }
    
    # Calling optimize function
    cml = optimize(.sharpeRatioFun, interval = range(mu), maximum = TRUE,
        data = data, spec = spec, constraintsStrings = constraintsStrings,
        tol = .Machine$double.eps^0.5)
           
    targetReturn = spec@portfolio$targetReturn = cml$maximum  
    targetRisk = getTargetRisk(frontierPortfolio(data = data$statistics, spec,
        constraintsStrings)) 
    weights = getWeights(frontierPortfolio(data = data$statistics,
        spec, constraintsStrings))

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
            targetMean = targetReturn,
            targetStdev = targetRisk),
        title = "CML Portfolio", 
        description = .description()) 
}


#-------------------------------------------------------------------------------


.tangencyConstrainedMVPortfolio = 
function(data, spec, constraintsStrings)
{
    # Description:
    #   Computes Risk, Return and Weight for the tangency portfolio
    
    # Example:
    #   .tangencyConstrainedMVPortfolio()
    
    # FUNCTION:
    
    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
 
    # Set Risk Free Rate:
    spec@portfolio$riskFreeRate = 0
    
    # Calling cml function
    ans = .cmlConstrainedMVPortfolio(data, spec, constraintsStrings)
    ans@call = match.call()
    ans@title = "Tangency Portfolio"
    
    # Return value:
    ans
}


#-------------------------------------------------------------------------------

   
.minvarianceConstrainedMVPortfolio = 
function(data, spec, constraintsStrings)
{
    # Description:
    #   Computes Risk, Return and Weight for minimum variance portfolio
    
    # Example:
    #   .minvarianceConstrainedMVPortfolio()
    
    # FUNCTION:

    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Function to be minimized:
    .minVariancePortfolioFun = 
    function(x, data, spec, constraintsStrings) {
        spec@portfolio$targetReturn = x
        ans = .frontierConstrainedMVPortfolio(data = data, spec = spec,
            constraintsStrings = constraintsStrings)
        getTargetRisk(ans)
    }

    # Calling optimize function
    minVar = optimize(.minVariancePortfolioFun, interval = range(mu),
        data = data, spec = spec, constraintsStrings = constraintsStrings,
        tol = .Machine$double.eps^0.5)
    targetReturn = spec@portfolio$targetReturn = minVar$minimum
    targetRisk = getTargetRisk(frontierPortfolio(data = data$statistics, spec,
        constraintsStrings)) 
    weights = getWeights(frontierPortfolio(data = data$statistics, spec,
        constraintsStrings))

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
            targetMean = targetReturn,
            targetStdev = targetRisk),
        title = "Minimum Variance Portfolio", 
        description = .description()) 
}


#-------------------------------------------------------------------------------   


.frontierConstrainedMVPortfolio = 
function(data, spec, constraintsStrings)
{
    # Description:
    #   Optimizes a mean-var portfolio for a given desired return and a set of
    #   box and or sector constraints
    
    # Arguments:
    #   data - portfolio of assets, a matrix or an object which can be 
    #       transformed to a matrix
    #   spec - specification of the portfolio

    # FUNCTION:
       
    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Number of Assets:
    dim = length(mu)

    # Extracting data from spec:
    targetReturn = spec@portfolio$targetReturn  
    stopifnot(is.numeric(targetReturn)) 

    # Setting the constraints matrix and vector:   
    tmp.ans = setConstraints(data = data, spec = spec,
        constraintsStrings = constraintsStrings)
    A = tmp.ans[, -(dim+1)]
    b0 = tmp.ans[, (dim+1)]
    dvec = rep(0, dim)

    # Calling QP solver:
    ans = .solve.QP(Dmat = Sigma, dvec = dvec, Amat = t(A), bvec = t(b0),
        meq = 2) 
    ierr = ans$ierr
    
    # Setting all weights zero being smaler than the machine precision:
    for(i in 1:dim){
        if(abs(ans$solution[i]) < .Machine$double.eps){
            ans$solution[i] = 0
        }
    }
    weights = ans$solution

    # Attributing no solutions
    attr(weights, "error") <- ierr
    targetRisk = sqrt(weights %*% Sigma %*% weights)

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
            targetMean = targetReturn,
            targetStdev = targetRisk),
        title = "Constrained MV Portfolio", 
        description = .description())    
}


# ------------------------------------------------------------------------------


.portfolioConstrainedMVFrontier = 
function(data, spec, constraintsStrings)
{
    # Description:
    #   Evaluates the EF for a given set of box and or sector constraints
    
    # Arguments:
    #   data - portfolio of assets, a matrix or an object which can be 
    #       transformed to a matrix
    #   spec - specification of the portfolio
    #   constraintsStrings - string of constraints

    # FUNCTION:

    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma

    # Settings:
    returnRange = spec@portfolio$returnRange
    riskRange = spec@portfolio$riskRange
    nFrontierPoints = spec@portfolio$nFrontierPoints
    
    # Ranges for mean and Standard Deviation:
    if (is.null(returnRange)){
        returnRange = range(mu) + 0.25*c(-diff(range(mu)), diff(range(mu)))
    }
    sqrtSig = sqrt(diag(Sigma))
    if (is.null(riskRange)){
        riskRange = c(min(sqrtSig), max(sqrtSig))+
        0.25*c(-diff(range(sqrtSig)), diff(range(sqrtSig)))
    }
    
    # Calculate Efficient Frontier:
    muMin = returnRange[1]
    muMax = returnRange[2]
    eps = 1.0e-6
    k = 0
    targetMu = targetSigma = nextWeights = rep(0, times = nFrontierPoints)
    weights = error = NULL

    # Loop over .frontierConstrainedMVPortfolio
    for (nTargetReturn in seq(muMin+eps, muMax-eps, length = nFrontierPoints)) {
        k = k+1
        Spec = portfolioSpec(portfolio = list(targetReturn = nTargetReturn))       
        # spec@portfolio$targetReturn = nTargetReturn
        tmp.object = .frontierConstrainedMVPortfolio(data = data, spec = Spec,
            constraintsStrings = constraintsStrings)
        targetMu[k] = tmp.object@portfolio$targetReturn
        targetSigma[k] = tmp.object@portfolio$targetRisk
        nextWeight = tmp.object@portfolio$weights
        weights = rbind(weights, t(nextWeight))
        error = c(error, as.logical(attr(nextWeight, "error")))
    }
    
    # Return Value:
    new("fPORTFOLIO", 
        call = match.call(),
        data = data,
        specification = spec,
        constraints = as.character(constraintsStrings),
        portfolio = list(
            weights = weights[!error, ],
            targetReturn = targetMu[!error],
            targetRisk = targetSigma[!error],
            targetMean = targetMu[!error],
            targetStdev = targetSigma[!error]),
        title = "Constrained MV Frontier", 
        description = .description())       
}


################################################################################
# BUILTIN: quadprog

    
.solve.QP = 
function(Dmat, dvec, Amat, bvec, meq)
{   # A Builtin function modified by Diethelm Wuertz

    # Description:
    #   Solves the quadratic programming problem
    
    # Note:   
    #   Package: quadprog
    #   Version: 1.4-7
    #   Date: 2004-01-31
    #   Title: Functions to solve Quadratic Programming Problems.
    #   Author: S original by Berwin A. Turlach <berwin.turlach@anu.edu.au>
    #       R port by Andreas Weingessel <Andreas.Weingessel@ci.tuwien.ac.at>
    #   Maintainer: Andreas Weingessel <Andreas.Weingessel@ci.tuwien.ac.at>
    #   Description: This package contains routines and documentation for
    #       solving quadratic programming problems.
    #   License: GPL version 2 or later
    #   Packaged: Sat Jan 31 13:32:53 2004; hornik
    #   Built: R 1.9.0; i386-pc-mingw32; 2004-03-28 15:03:03
    
    # FUNCTION:
    
    # Solve QP:
    n = nrow(Dmat)
    q = ncol(Amat)  
    r = min(n,q)
    work = rep(0, 2*n+r*(r+5)/2+2*q+1)
    res1 = .Fortran("qpgen2",
        as.double(Dmat), dvec = as.double(dvec), as.integer(n), 
        as.integer(n), sol = as.double(rep(0,n)), crval = as.double(0),
        as.double(Amat), as.double(bvec), as.integer(n), as.integer(q), 
        as.integer(meq), iact = as.integer(rep(0,q)), nact = as.integer(0),
        iter = as.integer(rep(0,2)), work = as.double(work),
        ierr = as.integer(0), PACKAGE = "fPortfolio")
    
    # Return Value:
    list(solution = res1$sol, value = res1$crval,
       unconstrainted.solution = res1$dvec, ierr = res1$ierr,
       iterations = res1$iter, iact = res1$iact[1:res1$nact]) 
}


################################################################################

