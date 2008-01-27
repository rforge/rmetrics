
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
# FUNCTION:                    DESCRIPTION: 
#  solveRdonlp2                 Calls Spelucci's donlp2 solver
################################################################################


.DEBUG = NA


# ------------------------------------------------------------------------------


solveRdonlp2 <- 
    function(data, spec, constraints)
{   
    # A function implemented by Rmetrics
    
    # Description:
    #   Calls Spelucci's donlp2 solver  
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
    nAssets = getNAssets(data)

    # Extracting data from spec:
    targetReturn = getTargetReturn(spec)  
    stopifnot(is.numeric(targetReturn))
    
    # Optimize:
    if (nAssets == 2) {
        
        # Two Assets Portfolio:
        stopifnot(targetReturn >= min(mu))
        stopifnot(targetReturn <= max(mu))  
        names(targetReturn) <- spec@model$estimator[1]
        weights = (targetReturn-mu[2]) / (mu[1]-mu[2])
        weights = c(weights, 1- weights)
        ans = list(
            weights = weights, 
            status = 0, 
            value = NA, 
            unconstrainted.solution = c(NA, NA), 
            iterations = c(NA, NA), 
            iact = c(NA, NA),
            solver = "twoAssetsMV")
            
    } else { 
        
        # Donlp2 Settings - Start Weights:
        if (is.null(spec@portfolio$weights)) {
            par = rep(1/nAssets, nAssets)
        } else {
            par = spec@portfolio$weights
        } 
        
        # Donlp2 Settings - Function to be optimized:
        fn = function(x) { x %*% Sigma %*% x } 
        
        # Donlp2 Settings - Box/Group Constraints:
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
        
        # Trace Solver:
        solver.trace = spec@solver$trace 
         
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
                control = rdonlp2Control(
                    iterma = 400, 
                    silent = !solver.trace),
                name = "portfolio")
        } else {
            # Optimize:
            ans = rdonlp2(
                par, fn, 
                par.l = par.lower, par.u = par.upper,
                A = A, lin.l = lin.lower, lin.u = lin.upper,  
                control = rdonlp2Control(
                    iterma = 400, 
                    silent = !solver.trace),
                name = "portfolio")
        }
        # Add:
        ans$weights = ans$par
        message = "KT-conditions satisfied, no further correction computed"
        if (ans$message == message) ans$status = 0 else ans$status = 1
        ans$solver = "RDonlp2"
        # if (solver.trace) 
            cat("Rdonlp2 Message:", ans$message, "\n")
   }
   
   # For Debugging ...
   .DEBUG <<- ans
   
   # Return Value:
   ans
}


################################################################################

