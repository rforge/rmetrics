
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
# FUNCTION:                DESCRIPTION:
#  solveRdonlp2             Calls Spelucci's donlp2 solver via Rdonlp2
#  .solveRdonlp2alt         Alternative Implementation
#  .rdonlp2Status           Extracts status from solveRdonlp2
#  .rdonlp2StringsToS4      Converts String to S4 Constraints
# FUNCTION:                DESCRIPTION:
#  rdonlp2                  Portfolio interface to rdonlp2 solver
#  rdonlp2Control           Control list for "rdnlp2" Solver
################################################################################


## DW, todo and remarks:

## solveRdonlp2(), does not yet support properly the "rdonlp2Control" list!
## control = rdonlp2Control(iterma = 400, silent = !solver.trace) is fix!

## The solveRdonlp2alt() function is designed for general non-linear 
## constraints, e.g. for Value-at-Risk or Drawdon constraints.


# ------------------------------------------------------------------------------


solveRdonlp2 <-
    function(data, spec, constraints)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Calls Spelucci's donlp2 solver for MV Portfolio Optimization

    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints

    # Note:
    #   This function is thought to minimize MV risk for a fixed return
    #   and additional quadratic covariance risk budget constraints.
    #   So the function can in principle also handle the case of
    #   quadratic tail risk budget constraints.

    # Details:
    #   Code comes from R package Rdonlop2, this package is required.
    #   Rdonlp2 can be downloaded from "http://arumat.net/Rdonlp2/"
    #   Author: Ryuichi Tamura, ry.tamura@gmail.com
    
    # Example:
    #   Data = 100*as.timeSeries(data(LPP2005REC))[,1:6]
    #   Spec = portfolioSpec(); setSolver(Spec) = "solveRdonlp2"
    #   tangencyPortfolio(Data, Spec)

    # FUNCTION:
      
    # Load Rdonlp2:
    if (!require(Cdonlp2)) {
        cat("\n\nRdonlp2 Package missing")
        cat("\nPlease download package from Rmetrics Server\n")
    } 
    
    # Transform Data and Constraints:
    data = portfolioData(data, spec)
    if (class(constraints) == "fPFOLIOCON") 
        constraints = constraints@stringConstraints

    # Trace:
    trace = getTrace(spec)
    if(trace) cat("\nPortfolio Optimiziation:\n Using Rdonlp2 ...\n\n")

    # Get Mean-Variance Specifications:
    mu = getMu(data)
    Sigma = getSigma(data)

    # Extract Target Return from Specification:
    targetReturn = getTargetReturn(spec)
    stopifnot(is.numeric(targetReturn))  

    # Start Values for Weights:
    nAssets = getNAssets(data)
    if (is.null(getWeights(spec))) {
        par = rep(1/nAssets, nAssets)
    } else {
        par = getWeights(spec)
    }
    
    # Now Optimize ...

    # Donlp2 Settings - Function to be optimized:
    optimize = getOptimize(spec)
    if (optimize == "minRisk") {
        fn = function(x) { 
            x %*% Sigma %*% x 
        }
    } else if (optimize == "maxReturn") {
        fn = function(x) { 
            x %*% mu 
        }
    } else if (optimize == "objRisk") {
        fn = match.fun(getObjective(spec))
    } else {
        stop("Check spec@model$optimize Slot!")
    }

    # Add Box/Group Constraints:
    A.mat = .setConstraints(data, spec, constraints, type = "BoxGroup")
    upperNames = paste("maxW", 1:nAssets, sep = "")
    par.upper = -A.mat[upperNames, "Exposure"]
    lowerNames = paste("minW", 1:nAssets, sep = "")
    par.lower = A.mat[lowerNames, "Exposure"]

    # Linear Constraints Donlp2 Settings - Group Constraints:
    Rows = (1:nrow(A.mat))
    names(Rows) = rownames(A.mat)
    ### Rows[c(lowerNames, upperNames)]
    A = A.mat[-Rows[c(lowerNames, upperNames)], ]
    M = nrow(A)
    mNames = rownames(A)
    lin.upper = lin.lower = rep(NA, M)
    # All weights must sum up to one ...
    lin.upper[1] = lin.lower[1] = A[1, nAssets+1]
    # All assets must sum up to the target return ...
    lin.upper[2] = lin.lower[2] = A[2, nAssets+1]

    # Further Group Constraints:
    if (M > 2) {
        for (i in 3:M) {
            if (mNames[i] == "minsumW") {
                lin.lower[i] = A[i, nAssets+1]
                lin.upper[i] = Inf
            } else if (mNames[i] == "maxsumW") {
                lin.lower[i] = -Inf
                lin.upper[i] = -A[i, nAssets+1]
                A[i, 1:nAssets] = -A[i, 1:nAssets]
            }
        }
    }
    A = A[, -(nAssets+1)]

    # Trace Optimization Path ?
    solver.trace = getTrace(spec)

    # Check Constraint Strings for Risk Budgets:
    validStrings = c("minB", "maxB")
    usedStrings = unique(sort(sub("\\[.*", "", constraints)))
    checkStrings = sum(usedStrings %in% validStrings)
    if (checkStrings > 0) {
        includeRiskBudgeting = TRUE
    } else {
        includeRiskBudgeting = FALSE
    }

    if (solver.trace) cat("Include Risk Budgeting:",
        includeRiskBudgeting, "\n")
        
    # Control:
    CONTROL = rdonlp2Control(            
        # Setup:
        iterma = 4000, nstep = 200, fnscale = 1, 
        report = FALSE, rep.freq = 1,
        # Perfomance and Tunings:
        tau0 = 1.0, tau = 0.1, del0 = 1.0,
        # Termination Criteria:
        epsx = 1e-8, delmin = 0.01 * 1.0, # del0, 
        epsdif = 1e-12, nreset.multiplier = 1,
        # Numerical Differentiation:
        difftype = 3, epsfcn = 1e-16, taubnd = 1.0, hessian = FALSE,
        # Information:
        te0 = TRUE, te1 = FALSE, te2 = FALSE, te3 = FALSE,
        silent = !solver.trace, intakt = TRUE )

    # Covariance Risk Budgets:
    if (includeRiskBudgeting) {
        # Non-Linear Constraints Functions:
        nlcon <- function(x) {
            B1 = as.vector(x %*% Sigma %*% x)
            B2 = as.vector(x * Sigma %*% x)
            B = B2/B1
            B
        }

        # Compose non-linear functions:
        for (I in 1:nAssets)
        eval( parse(text = paste(
            "nlcon", I, " = function(x) { nlcon(x)[", I, "] }", sep = "")) )
        nlinFunctions = paste("nlcon", 1:nAssets, sep = "", collapse = ",")
        nlinFunctions = paste("list(", nlinFunctions, ")")
        nlin = eval( parse(text = nlinFunctions) )

        # Constraints Vectors:
        B = .setConstraints(data, spec, constraints, type = "RiskBudget")
        nlin.lower = B[1, ]
        nlin.upper = B[2, ]

        # Optimize - Call rdonlp2:
        ans = rdonlp2(
            par, fn,
            par.l = par.lower, par.u = par.upper,
            A = A, lin.l = lin.lower, lin.u = lin.upper,
            nlin = nlin, nlin.l = nlin.lower, nlin.u = nlin.upper,
            control = CONTROL,
            name = "portfolio")
    } else {
        # Optimize - Call rdonlp2:
        ans = rdonlp2(
            par, fn,
            par.l = par.lower, par.u = par.upper,
            A = A, lin.l = lin.lower, lin.u = lin.upper,
            control = CONTROL,
            name = "portfolio")
    }

    # Add to List:
    if (solver.trace) cat("Rdonlp2 Message:", ans$message, "\n")
    ans$solver = "RDonlp2"
    ans$weights = ans$par
    message = "KT-conditions satisfied, no further correction computed"
    if (ans$message == message) ans$status = 0 else ans$status = 1
    
    # Target Return and Risk:
    ans$targetReturn = targetReturn
    ans$targetRisk = sqrt((ans$weights %*% Sigma %*% ans$weights)[[1]])
    ans$objective = ans$fx
    
    # Return Value:
    ans
}


################################################################################


.solveRdonlp2alt <-
    function(data, spec, constraints)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Calls Spelucci's donlp2 solver for MV Portfolio Optimization

    # Arguments:
    #   data - a timeSeries object or an object of class fPFOLIODATA
    #   spec - a specification object of class fPFOLIOSPEC
    #   constraints - a constraints string or an object of class fPFOLIOCON
    
    # Note:
    #   This function is thought to minimize MV risk for a fixed return
    #   and additional quadratic covariance risk budget constraints.
    #   So the function can in principle also handle the case of
    #   quadratic tail risk budget constraints.

    # Details:
    #   Code comes from R package Rdonlop2, this package is required.
    #   Rdonlp2 can be downloaded from "http://arumat.net/Rdonlp2/"
    #   Author: Ryuichi Tamura, ry.tamura@gmail.com

    # FUNCTION:
    
    # Load Rdonlp2:
    if(!require(Cdonlp2)) stop("Rdonlp2 is not installed")
    
    # Trace:
    trace = getTrace(spec)
    if(trace) cat("\nPortfolio Optimiziation:\n Using Rdonlp2 ...\n\n")

    # Convert Data Input to fPFOLIODATA:
    data = portfolioData(data, spec) 

    # par: Start Values for Weights:
    nAssets = getNAssets(data)
    weights = getWeights(spec)
    par = if (is.null(weights)) rep(1/nAssets, nAssets) else weights
 
    # fn: Function to be optimized:
    mu = getMu(data)
    Sigma = getSigma(data)
    optimize = getOptimize(spec)
    if (optimize == "minRisk") fn = function(x) { x %*% Sigma %*% x }
    else if (optimize == "maxReturn") fn = function(x) { x %*% mu }
    else if (optimize == "objRisk") fn = match.fun(getObjective(spec))
    else stop("Check spec@model$optimize Slot!")
    
    # constraints: Convert them into a S3 list Object:
    if (is.character(constraints)) { 
        constraints = .rdonlp2StringsToS4(data, spec, constraints)
    } else if (class(constraints) == "fPFOLIOCON") {
        constraints = constraints@altConstraints
    }
    # Rdonlp2 Constraints:
    par.lower = constraints$par.lower
    par.upper = constraints$par.upper
    A = constraints$A
    lin.lower = constraints$lin.lower
    lin.upper = constraints$lin.upper
    nlin = constraints$nlin
    nlin.lower = constraints$nlin.lower
    nlin.upper = constraints$nlin.upper
    
    # Control Parameters:
    control = rdonlp2Control(            
        # Setup:
        iterma = 4000, nstep = 200, fnscale = 1, 
        report = FALSE, rep.freq = 1,
        # Perfomance and Tunings:
        tau0 = 1.0, tau = 0.1, del0 = 1.0,
        # Termination Criteria:
        epsx = 1e-8, delmin = 0.01 * 1.0, # del0, 
        epsdif = 1e-12, nreset.multiplier = 1,
        # Numerical Differentiation:
        difftype = 3, epsfcn = 1e-16, taubnd = 1.0, hessian = FALSE,
        # Information:
        te0 = TRUE, te1 = FALSE, te2 = FALSE, te3 = FALSE,
        silent = !trace, intakt = TRUE )
    
    # Optimize:
    if (length(A) == 0 & length(nlin) == 0) {
        ans = rdonlp2(
            par, fn,
            par.lower = par.lower, par.upper = par.upper,
            control = control)            
    } else if (length(nlin) == 0) {
        ans = rdonlp2(
            par, fn,
            par.lower = par.lower, par.upper = par.upper,
            A = A, lin.lower = lin.lower, lin.upper = lin.upper,
            control = control)
    } else {
        ans = rdonlp2(
            par, fn,
            par.lower = par.lower, par.upper = par.upper,
            A = A, lin.lower = lin.lower, lin.upper = lin.upper,
            nlin = nlin, nlin.lower = nlin.lower, nlin.upper = nlin.upper,
            control = control)
    }
      
    # Add to List:
    ans$solver = "Rdonlp2"
    ans$constraints = constraints
    ans$weights = ans$par
    ans$targetReturn = getTargetReturn(spec)
    ans$targetRisk = sqrt((ans$weights %*% Sigma %*% ans$weights)[[1]])
    ans$objective = ans$fx
    ans$status <- .rdonlp2Status(ans)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.rdonlp2Status <-
    function(ans)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Extracts the status from a list returned by solveRdonlp2
    
    # Arguments:
    #   ans - a list as returned by the function solveRdonlp2
    
    # FUNCTION:
    
    # Check and Add Messages:
    ans$status = 1
    
    # Message = "1234567890123456789012345"
    message11 = "KT-conditions satisfied, " # no further correction computed"
    message12 = "computed correction small" # , regular case"
    message13 = "stepsizeselection: x almo" # st feasible, dir. deriv. very small"
    if (substr(ans$message, 1, 25) == message11) ans$status = 0 
    if (substr(ans$message, 1, 25) == message12) ans$status = 0 
    if (substr(ans$message, 1, 25) == message13) ans$status = 0 
    
    # Force Status: 
    # ans$status = 0 
    
    # Return Value:
    ans$status
}


# ------------------------------------------------------------------------------


.rdonlp2StringsToS4 <-
    function(data, spec, constraints)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Converts String Constraints into a fPFOLIOCON object
    
    # Arguments:
    #   data - a timeSeries object or an object of class fPFOLIODATA
    #   spec - a specification object of class fPFOLIOSPEC
    #   constraints - a constraints string
    
    # Note:
    #   The returned list has the same named entries as the original
    #   donlp2 function from R's package Rdonlp2
    
    # FUNCTION:
    
    # Add Box/Group Constraints:
    data = portfolioData(data)
    nAssets = getNAssets(data)
    A.mat = .setConstraints(data, spec, constraints, type = "BoxGroup")
    upperNames = paste("maxW", 1:nAssets, sep = "")
    par.upper = -A.mat[upperNames, "Exposure"]
    lowerNames = paste("minW", 1:nAssets, sep = "")
    par.lower = A.mat[lowerNames, "Exposure"]

    # Linear Constraints Donlp2 Settings - Group Constraints:
    Rows = (1:nrow(A.mat))
    names(Rows) = rownames(A.mat)
    
    # Rows[c(lowerNames, upperNames)]
    A = A.mat[-Rows[c(lowerNames, upperNames)], ]
    M = nrow(A)
    mNames = rownames(A)
    lin.upper = lin.lower = rep(NA, M)
    
    # All weights must sum up to one ...
    lin.upper[1] = lin.lower[1] = A[1, nAssets+1]
    
    # All assets must sum up to the target return ...
    lin.upper[2] = lin.lower[2] = A[2, nAssets+1]

    # Further Group Constraints:
    if (M > 2) {
        for (i in 3:M) {
            if (mNames[i] == "minsumW") {
                lin.lower[i] = A[i, nAssets+1]
                lin.upper[i] = Inf
            } else if (mNames[i] == "maxsumW") {
                lin.lower[i] = -Inf
                lin.upper[i] = -A[i, nAssets+1]
                A[i, 1:nAssets] = -A[i, 1:nAssets]
            }
        }
    }
    A = A[, -(nAssets+1)]

    # Trace Optimization Path ?
    solver.trace = getTrace(spec)

    # Check Constraint Strings for Risk Budgets:
    validStrings = c("minB", "maxB")
    usedStrings = unique(sort(sub("\\[.*", "", constraints)))
    checkStrings = sum(usedStrings %in% validStrings)
    if (checkStrings > 0) {
        includeRiskBudgeting = TRUE
    } else {
        includeRiskBudgeting = FALSE
    }
        
    # Covariance Risk Budgets:
    if (includeRiskBudgeting) {
        
        if (getSpec(trace)) 
            cat("Include Risk Budgeting:", includeRiskBudgeting, "\n")
        
        # Non-Linear Constraints Functions:
        Sigma = getSigma(data)
        nlcon <- function(x) {
            B1 = as.vector(x %*% Sigma %*% x)
            B2 = as.vector(x * Sigma %*% x)
            B = B2/B1
            B
        }
        
        # Compose Non-Linear Functions:
        for (I in 1:nAssets)
        eval( parse(text = paste(
            "nlcon", I, " = function(x) { nlcon(x)[", I, "] }", sep = "")) )
        nlinFunctions = paste("nlcon", 1:nAssets, sep = "", collapse = ",")
        nlinFunctions = paste("list(", nlinFunctions, ")")
        nlin = eval( parse(text = nlinFunctions) )

        # Set Constraints Vectors:
        B = .setConstraints(data, spec, constraints, type = "RiskBudget")
        nlin.lower = B[1, ]
        nlin.upper = B[2, ]
    } else {
        
        # Compose Non-Linear Functions:
        nlin = list()
        
        # Set Constraints Vectors:
        nlin.lower = NULL
        nlin.upper = NULL
    }
    
    # Return Value:
    list(
        name = "Strings2S4",
        par.lower = par.lower, par.upper = par.upper,
        A = A, lin.lower = lin.lower, lin.upper = lin.upper,
        nlin = nlin, nlin.lower = nlin.lower, nlin.upper = nlin.upper)
}
    

################################################################################
# FUNCTION:                    DESCRIPTION:
#  rdonlp2                      Portfolio interface to rdonlp2 solver
#  rdonlp2Control               Control list for "rdnlp2" Solver
################################################################################


# Package: Rdonlp2
# Version: 0.3-1
# Date: 2007-03-18
# Title: C library to use Peter Spelluci's DONLP2 from R.
# Author: Ryuichi Tamura(ry.tamura@gmail.com)
# Depends: R (>= 2.4.0)
# License: Free for research purpose.
# URL: http://arumat.net/Rdonlp2/
# Description: DONLP2(http://plato.la.asu.edu/donlp2.html) is a general 
#   purpose nonlinear constrained programming problem solver written 
#   by Peter Spelluci. Rdonlp2 is a wrapper library to use it from R.


# Builtin: Requires Rmetrics Package: Cdonlp2


rdonlp2 <- 
    function(
    par, fn,
    par.upper = rep(+Inf, length(par)),
    par.lower = rep(-Inf, length(par)),
    A = NULL,
    lin.upper = rep(+Inf, length(par)),
    lin.lower = rep(-Inf, length(par)),
    nlin = list(),
    nlin.upper = rep(+Inf, length(nlin)),
    nlin.lower = rep(-Inf, length(nlin)), 
    control = rdonlp2Control(),
    control.fun = function(lst){return(TRUE)},
    env = .GlobalEnv, name = NULL)
{    
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Portfolio interface to rdonlp2 Solver
    
    # FUNCTION:
    
    # Running ...
    # print("Running rdonlp2 ...")
    
    # Use analytical gradients?
    if (is.function(attr(fn, "gr")) &
        # DW:
        # all(lapply(nlin, function(e)is.function(attr(e,"gr"))))){
        all(unlist(lapply(nlin, function(e)is.function(attr(e,"gr")))))) {
        control["analyt"] = TRUE
    } else {
        control["analyt"] = FALSE
    }
  
    # Check parameter and its box constraints:
    if (length(par) != length(par.upper) | length(par) != length(par.lower) ){
        stop("# of elements for box constraints != # of parameters")
    }

    # Check linear constraints matrix A:
    if (is.null(A)){
        num.lin <- 0
        conmat <- c()
        lin.upper <- lin.lower <- c()
    } else {
        num.lin <- nrow(A)
        if (ncol(A) != length(par))
            stop("# of ncol(A) should be equal to # of par")
        if (length(lin.upper) != num.lin | length(lin.lower) != num.lin)
            stop("# of bounds for linear constraints should be equal to nrow(A)")
        conmat <- t(A)
    }
  
    # Nonlinear constraints:
    num.nlin <- length(nlin)
    if (length(nlin.upper)!=num.nlin | length(nlin.lower)!=num.nlin)
    stop("# of bounds for nonlinear constraints should be equal to length(nlin)")
    # Concatenate bounds for internal use:
    lbd <- c(par.lower, lin.lower, nlin.lower)
    ubd <- c(par.upper, lin.upper, nlin.upper)
    
    #
    # the wrapper for objective and constraint function 
    # (called from eval_extern())
    # mode == 0: EVAL_FN(evaluate objective and constraint function)
    # mode == 1: EVAL_GR(evaluate gr of objective and constraint function)
    #
    # fun.id == 0: evaluate objective function 'fn'
    # fun.id >= 1: evaluate constraint function 'nlin[[fun.id]]'
    #
    
    confun <- function(arg) {
        mode = arg[1]
        fun.id = arg[2]
        p = arg[c(-1,-2)]
        if (mode == 0){      # evaluate function values
            if (fun.id == 0){
                return(as.double(eval(fn(p), env)))
            }
            return(as.double(eval(nlin[[fun.id]](p), env)))
        } else if (mode == 1) { # evaluate gradient values
            if (fun.id == 0){
                # Modified by DW:
                # return(as.double(eval( fn@gr(p),               env)))
                  return(as.double(eval( (attributes(fn)$gr)(p), env)))
            }
            # Modified by DW:
            # return(as.double(eval( (nlin[[fun.id]]@gr)            (p), env)))
              return(as.double(eval( (attributes(nlin[[fun.id]])$gr)(p), env)))
        } else {
            stop("unknown evaluation mode: %d", mode)
        }
    }        

    # accfun
    accfun <- function(lst){
        return(as.logical(control.fun(lst)))
    }
    
    fsilent <- FALSE
    if (is.null(name)){
        fsilent <- TRUE
        name = "dummy"
    }
    
    # Start donlp2:
    tryCatch(
        # start donlp2
        ans <- .Call("call_donlp2",
            as.double(par),
            as.integer(num.lin),
            as.integer(num.nlin),
            fsilent,
            name,
            nchar(name),
            as.double(lbd), 
            as.double(ubd),
            as.double(conmat),
            control,
            accfun,
            confun, environment(confun), 
            PACKAGE = "Cdonlp2"),
            # ensure to free memory and close .mes .pro files if opened
            finally = .Call("teardown", 0, 
            PACKAGE = "Cdonlp2")
            )
    ans$nr.update <- matrix(ans$nr.update, nr = length(par))
    if (control$hessian) {
        ans$hessian = matrix(ans$hessian, nr = length(par))
    } else {
        ans$hessian = NULL
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rdonlp2Control <- 
    function(            
    iterma = 4000, nstep = 20,fnscale = 1, report = FALSE, rep.freq = 1,
    tau0 = 1.0, tau = 0.1, del0 = 1.0, epsx = 1e-5, delmin = 0.1*del0,
    epsdif = 1e-8, nreset.multiplier = 1, difftype = 3, epsfcn = 1e-16, 
    taubnd = 1.0, hessian = FALSE, te0 = TRUE, te1 = FALSE, te2 = FALSE, 
    te3 = FALSE, silent = FALSE, intakt = TRUE )
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Control list for "rdnlp2" Solver
    
    # FUNCTION:
    
    # Return Value:
    list(
        iterma = as.integer(iterma), 
        nstep = as.integer(nstep),
        fnscale = fnscale,
        report = report,
        rep.freq = as.integer(ifelse(rep.freq<1, 1, rep.freq)),
        tau0 = tau0, 
        tau = tau, 
        del0 = del0,
        epsx = epsx, 
        delmin = delmin, 
        epsdif = epsdif,
        nreset.multiplier = nreset.multiplier,
        difftype = as.integer(ifelse(!difftype%in%c(1,2,3), 3, difftype)),
        epsfcn = epsfcn, 
        taubnd = taubnd, 
        hessian = hessian,
        te0 = te0, 
        te1 = te1, 
        te2 = te2, 
        te3 = te3,
        silent = silent, 
        intakt = intakt)
}


################################################################################

