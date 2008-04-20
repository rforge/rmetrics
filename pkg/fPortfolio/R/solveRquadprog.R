
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                    DESCRIPTION: 
#  solveRquadprog               Calls Goldfarb and Idnani's QP solver
################################################################################


solveRquadprog <- 
    function(data, spec, constraints)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Calls Goldfarb and Idnani's QP solver for Mean-Variance Problems
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Note:
    #   This function is thought to minimize MV risk for a fixed return
    #   and additional box and group constraints.
    #   The function can in principle handle any case of linear constraints.
    
    # Details:
    #   The fortran function "qpgen2" is builtin from the contributed 
    #   R package quadprog:
    #   quadprog:   Functions to solve Quadratic Programming Problems.
    #   Version:    1.4-11
    #   Date:       2007-07-12
    #   Author:     S original by Berwin A. Turlach R port by A. Weingessel
    #   Maintainer: Andreas Weingessel
    #   License:    GPL-2
    
    # Value of slove.QP():
    #   solution - vector containing the solution of the quadratic 
    #       programming problem.
    #   value - scalar, the value of the quadratic function at the 
    #       solution
    #   unconstrained.solution - vector containing the unconstrained 
    #       minimizer of the quadratic function.
    #   iterations - vector of length 2, the first component contains 
    #       the number of iterations the algorithm needed, the second 
    #       indicates how often constraints became inactive after 
    #       becoming active first. vector with the indices of the 
    #       active constraints at the solution.
    
    # FUNCTION:
    
    # Get Statistics:
    if(!inherits(data, "fPFOLIODATA")) 
        data = portfolioData(data, spec)
    
    # Trace:
    trace = getTrace(spec)
    if(trace) 
        cat("\nPortfolio Optimiziation:\n Using Rquadprog ...\n\n")
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
    
    # Number of Assets:
    nAssets = getNAssets(data)

    # Extracting data from Specification:
    targetReturn = getTargetReturn(spec)  
    stopifnot(is.numeric(targetReturn)) 
    
    # Optimize Portfolio:
    if (nAssets == 2) {
        # Two Assets Portfolio:
        stopifnot(targetReturn >= min(mu))
        stopifnot(targetReturn <= max(mu))  
        ## names(targetReturn) <- spec@model$estimator[1]
        weights = (targetReturn-mu[2]) / (mu[1]-mu[2])
        weights = c(weights, 1- weights)
        optim = list(
            value = NA, 
            unconstrainted.solution = c(NA, NA), 
            iterations = c(NA, NA), 
            iact = c(NA, NA) )
        ans = list(
            solver = "twoAssetsMV",
            optim = optim,
            weights = weights, 
            targetReturn = targetReturn,
            targetRisk = NA,
            objective = NA,
            status = 0)
    } else { 
        # Dmat, dvec, Amat, bvec, meq = 0
        # Setting the constraints matrix and vector:   
        tmpConstraints = .setConstraints(data, spec, constraints)
        tmpConstraints[1, ] = tmpConstraints[1, ]
        tmpConstraints = tmpConstraints[c(2,1,3:NROW(tmpConstraints)), ]
        Dmat = Sigma
        dvec = rep(0, nAssets)
        Amat = t(tmpConstraints[, -(nAssets+1)])
        bvec = t(tmpConstraints[, (nAssets+1)])
        meq = 1
        res1 = .rquadprog(Dmat, dvec, Amat, bvec, meq)   
        weights = res1$sol 
        for(i in 1:nAssets) {
            if(abs(weights[i]) < sqrt(.Machine$double.eps)) weights[i] = 0
        } 
        # Added:
        res1$value = res1$crval
        res1$unconstrainted.solution = res1$dvec
        res1$iterations = res1$iter
        res1$iact = res1$iact[1:res1$nact]
        
        # Prepare Output List:
        ans = list(
            solver = "solveRquadprog",
            optim = res1,
            weights = weights, 
            targetReturn = targetReturn,
            targetRisk = NA,
            status = res1$ierr, 
            objective = res1$crval)
    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.rquadprog = 
function(Dmat, dvec, Amat, bvec, meq)
{
    # Arguments
    #   Dmat - matrix appearing in the quadratic function to be minimized.
    #   dvec - vector appearing in the quadratic function to be minimized.
    #   Amat - matrix defining the constraints under which we want to 
    #       minimize the quadratic function.
    #   bvec - vector holding the values of b_0 (defaults to zero).
    #   meq - the first meq constraints are treated as equality constraints, 
    #       all further as inequality constraints (defaults to 0).
    #   factorized - logical flag: if TRUE, then we are passing R^(-1) 
    #       (where D = R^T R) instead of the matrix D in the argument Dmat.   


    factorized = FALSE
    n <- nrow(Dmat)
    q <- ncol(Amat)
    if (missing(bvec)) 
        bvec <- rep(0, q)
    if (n != ncol(Dmat)) 
        stop("Dmat is not symmetric!")
    if (n != length(dvec)) 
        stop("Dmat and dvec are incompatible!")
    if (n != nrow(Amat)) 
        stop("Amat and dvec are incompatible!")
    if (q != length(bvec)) 
        stop("Amat and bvec are incompatible!")
    if ((meq > q) || (meq < 0)) 
        stop("Value of meq is invalid!")
    iact <- rep(0, q)
    nact <- 0
    r <- min(n, q)
    sol <- rep(0, n)
    crval <- 0
    work <- rep(0, 2 * n + r * (r + 5)/2 + 2 * q + 1)
    iter <- rep(0, 2)
    res1 <- .Fortran("qpgen2", 
        as.double(Dmat), 
        dvec = as.double(dvec), 
        as.integer(n), 
        as.integer(n), 
        sol = as.double(sol), 
        crval = as.double(crval), 
        as.double(Amat), 
        as.double(bvec), 
        as.integer(n), as.integer(q), 
        as.integer(meq), 
        iact = as.integer(iact), 
        nact = as.integer(nact), 
        iter = as.integer(iter), 
        work = as.double(work), 
        ierr = as.integer(factorized), 
        PACKAGE = "quadprog")
    if (res1$ierr == 1){
        stop("constraints are inconsistent, no solution!")
    } else if (res1$ierr == 2) {
        stop("matrix D in quadratic function is not positive definite!") 
    }
    
    # Return Value:  
    res1
}


################################################################################


.solveRquadprog <- 
    function(data, spec, constraints)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Calls Goldfarb and Idnani's QP solver for Mean-Variance Problems
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Note:
    #   This function is thought to minimize MV risk for a fixed return
    #   and additional box and group constraints.
    #   The function can in principle handle any case of linear constraints.
    
    # Details:
    #   The fortran function "qpgen2" is builtin from the contributed 
    #   R package quadprog:
    #   quadprog:   Functions to solve Quadratic Programming Problems.
    #   Version:    1.4-11
    #   Date:       2007-07-12
    #   Author:     S original by Berwin A. Turlach R port by A. Weingessel
    #   Maintainer: Andreas Weingessel
    #   License:    GPL-2
    
    # Value of slove.QP():
    #   solution - vector containing the solution of the quadratic 
    #       programming problem.
    #   value - scalar, the value of the quadratic function at the 
    #       solution
    #   unconstrained.solution - vector containing the unconstrained 
    #       minimizer of the quadratic function.
    #   iterations - vector of length 2, the first component contains 
    #       the number of iterations the algorithm needed, the second 
    #       indicates how often constraints became inactive after 
    #       becoming active first. vector with the indices of the 
    #       active constraints at the solution.
    
    # FUNCTION:
    
    # Get Statistics:
    if(!inherits(data, "fPFOLIODATA")) 
        data = portfolioData(data, spec)
    
    # Trace:
    trace = getTrace(spec)
    if(trace) 
        cat("\nPortfolio Optimiziation:\n Using Rquadprog ...\n\n")
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
    
    # Number of Assets:
    nAssets = getNAssets(data)

    # Extracting data from Specification:
    targetReturn = getTargetReturn(spec)  
    stopifnot(is.numeric(targetReturn)) 
    
    # Optimize Portfolio:
    if (nAssets == 2) {
        # Two Assets Portfolio:
        stopifnot(targetReturn >= min(mu))
        stopifnot(targetReturn <= max(mu))  
        ## names(targetReturn) <- spec@model$estimator[1]
        weights = (targetReturn-mu[2]) / (mu[1]-mu[2])
        weights = c(weights, 1- weights)
        ans = list(
            solver = "twoAssetsMV",
            weights = weights, 
            targetReturn = targetReturn,
            targetRisk = NA,
            objective = NA,
            status = 0, 
            # Further list elements ...
            value = NA, 
            unconstrainted.solution = c(NA, NA), 
            iterations = c(NA, NA), 
            iact = c(NA, NA))
    } else { 
        # Setting the constraints matrix and vector:   
        tmpConstraints = .setConstraints(
            data = data, spec = spec, constraints = constraints)
        Dmat = Sigma
        dvec = rep(0, nAssets)
        A = tmpConstraints[, -(nAssets+1)]
        Amat = t(A)
        b0 = tmpConstraints[, (nAssets+1)] 
        bvec = t(b0)
        meq = 2
        n = nrow(Dmat)
        q = ncol(Amat)
        r = min(n, q)
        work = rep(0, 2 * n + r * (r + 5)/2 + 2 * q + 1)
        res1 = .Fortran("qpgen2", 
            as.double(Dmat), 
            dvec = as.double(dvec), 
            as.integer(n), 
            as.integer(n), 
            sol = as.double(rep(0, n)), 
            crval = as.double(0), 
            as.double(Amat), 
            as.double(bvec), 
            as.integer(n), 
            as.integer(q), 
            as.integer(meq), 
            iact = as.integer(rep(0, q)), 
            nact = as.integer(0), 
            iter = as.integer(rep(0, 2)), 
            work = as.double(work), 
            ierr = as.integer(0), 
            PACKAGE = "quadprog")  
                 
        # Handle when failed ...
        weights = res1$sol 
        for(i in 1:nAssets) {
            if(abs(weights[i]) < sqrt(.Machine$double.eps)) weights[i] = 0
        }  
        
        # Prepare Output List:
        ans = list(
            solver = "quadprog",
            weights = weights, 
            targetReturn = targetReturn,
            targetRisk = NA,
            status = res1$ierr, 
            objective = res1$crval,
            # Further list elements ...
            value = res1$crval, 
            unconstrainted.solution = res1$dvec, 
            iterations = res1$iter, 
            iact = res1$iact[1:res1$nact])
    }

    # Return Value:
    ans
}


################################################################################

