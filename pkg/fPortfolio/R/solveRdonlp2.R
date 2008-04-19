
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
#  solveRdonlp2                 Calls Spelucci's donlp2 solver via Rdonp2
################################################################################


## DW, todo:
## solveRdonlp2(), does not yet support properly the "rdonlp2Control" list!
## control = rdonlp2Control(iterma = 400, silent = !solver.trace) is fix!


# ------------------------------------------------------------------------------


solveRdonlp2 <-
    function(data, spec, constraints)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Calls Spelucci's donlp2 solver for MV Portfolio Optimization

    # Note:
    #   This function is thought to minimize MV risk for a fixed return
    #   and additional quadratic covariance risk budget constraints.
    #   So the function can in principle also handle the case of
    #   quadratic tail risk budget constraints.

    # Details:
    #   Code comes from R package Rdonlop2, this package is required.
    #   Rdonlp2 can be downloaded from "http://arumat.net/Rdonlp2/"
    #   Author: Ryuichi Tamura, ry.tamura@gmail.com

    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints

    # FUNCTION:
      
    # Load Rdonlp2:
    if(!require(Rdonlp2))
        stop("Rdonlp2 is not installed")

    # Get Statistics:
    if(!inherits(data, "fPFOLIODATA")) 
        data = portfolioData(data, spec)

    # Trace:
    trace = getTrace(spec)
    if(trace) 
        cat("\nPortfolio Optimiziation:\n Using Rdonlp2 ...\n\n")

    # Get Mean-Variance Specifications:
    #   ... modify for Mean-LPM Portfolios 
    mu = getMu(data)
    Sigma = getSigma(data)
    
    # Number of Assets:
    nAssets = getNAssets(data)

    # Extract Target Return from Specification:
    targetReturn = getTargetReturn(spec)
    stopifnot(is.numeric(targetReturn))  

    # Start Values for Weights:
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
    
    # minRisk - Constraints:
    #   Target Return:              mu*x = b
    
    # maxReturn - Constraints:    
    #   Target Risk:                x*Sigma*x = s

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

        # Optimize:
        ans = rdonlp2(
            par, fn,
            par.l = par.lower, par.u = par.upper,
            A = A, lin.l = lin.lower, lin.u = lin.upper,
            nlin = nlin, nlin.l = nlin.lower, nlin.u = nlin.upper,
            control = CONTROL,
            name = "portfolio")
    } else {
        # Optimize:
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

    # Return Value:
    ans
}


################################################################################

