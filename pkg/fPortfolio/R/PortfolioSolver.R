
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
#  solveShortExact              Solves Analytically Unlimited Short Portfolio 
#  solveRquadprog               Calls Goldfarb and Idnani's QP solver
#  solveRdonlp2                 Calls Spelucci's donlp2 solver
#  solveRlpSolve                Calls linear programming solver
################################################################################


.DEBUG = NA


.solveShortExact =
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Solves Analyticallu Unlimited Short Portfolio 
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
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

    # Get Target Return - if NULL use Tangency Portfolio:
    targetReturn = spec@portfolio$targetReturn 
    if (is.null(targetReturn)) targetreturn = (a/b)*C0     
    
    # Get Target Risk:
    targetRisk = sqrt((c*targetReturn^2 - 2*b*C0*targetReturn + a*C0^2) / d)
    
    # Get Weights:
    weights = as.vector(invSigma %*% ((a-b*mu)*C0 + (c*mu-b)*targetReturn )/d)
    
    # Prepare Output List:
    ans = list(
        weights = weights, 
        targetReturn = targetReturn, targetRisk = targetRisk)
        
    # For Debugging ...
    .DEBUG <<- ans

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


solveRquadprog =
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Calls Goldfarb and Idnani's QP solver for Mean-Variance Problems
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
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
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
    nAssets = getNumberOfAssets(data)

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
            weights = weights, 
            status = res1$ierr, 
            value = res1$crval, 
            unconstrainted.solution = res1$dvec, 
            iterations = res1$iter, 
            iact = res1$iact[1:res1$nact],
            solver = "quadprog")
    }
    
    # For Debugging ...
    .DEBUG <<- ans

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


solveRdonlp2 =
function(data, spec, constraints)
{   # A function implemented by Rmetrics
    
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
    nAssets = getNumberOfAssets(data)

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


# ------------------------------------------------------------------------------


solveRlpSolve =
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Linear Solver from R package lpSolve for Mean-CVaR Problems
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Value of lp():
    #   direction - optimization direction, as entered 
    #   x.count - number of variables in objective function 
    #   objective - vector of objective function coefficients, 
    #       as entered 
    #   const.count - number of constraints entered 
    #   constraints - constraint matrix, as entered (not 
    #       returned by lp.assign or lp.transport) 
    #   int.count - number of integer variables 
    #   int.vec - vector of integer variables' indices, as entered 
    #   objval - value of objective function at optimum} 
    #   solution - vector of optimal coefficients 
    #   status - numeric indicator: 0 = success, 2 = no feasible 
    #       solution 
   
    # Note:
    #   This function requires to load the contributed R package
    #   lpSolve explicitely!
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
    nAssets = getNumberOfAssets(data)

    # Extracting data from spec:
    targetReturn = getTargetReturn(spec)  
    stopifnot(is.numeric(targetReturn))
    
    # Get quantile measure alpha:
    targetAlpha = spec@portfolio$targetAlpha
    
    # Scenarios:
    Data = getSeries(data)
    colNames = colnames(Data)
    rowNames = rownames(Data)
    assets = dim(Data)
    m = assets[1]
    w = assets[2]
    
    if (nAssets == 2) {
        # Two Assets Portfolio:
        stopifnot(targetReturn >= min(mu))
        stopifnot(targetReturn <= max(mu))  
        names(targetReturn) <- spec@model$estimator[1]
        weights = (targetReturn-mu[2]) / (mu[1]-mu[2])
        weights = c(weights, 1- weights)
        ans = list(
            weights = weights, 
            VaR = .varRisk(Data, weights, targetAlpha),
            solution = .varRisk(Data, weights, targetAlpha),
            CVaR = -.cvarRisk(Data, weights, targetAlpha),
            objval = .cvarRisk(Data, weights, targetAlpha),
            ierr = 0,
            solver = "twoAssetsCVaR",
            targetAlpha = targetAlpha)
    } else {
        # Compose objective function:
        Names = c("VaR", paste("e", 1:m, sep = ""), colNames)      
        f.obj = c(-1, rep(-1/(targetAlpha*m), m), rep(0, w))
        names(f.obj) = Names
        
        # Info on constraints - Constraint matrix:
        #   Example m=8 Data Records, and w=4 Assets
        #   
        #   VaR  es            weights          exposure
        #   x1   x2  ...  x9   x10 ... x13
        #       
        #    0    0       0    mu1     mu4      >= Mu
        #    0    0       0    1       1        == 1
        #               
        #   -1    1       0    r1.1    r4.1     >= 0
        #   -1    0  1    0    r1.2    r4.2     >= 0
        #   
        #   -1    0    1  0    r1.8    r4.8     >= 0
        #   -1    0       1    r1.9    r4.9     >= 0
        #   
        #   x2   >= 0    ...   x9   >= 0  
        #   x10  >= 0    ...   x13  >= 0
        
        # Compose Constraint Matrix:
        nX = 1 + m + w
        nY = 2 + m
        f.con = matrix(rep(0, nX*nY), ncol = nX)
        rownames(f.con) = c("Budget", "Return", rowNames)
        colnames(f.con) = c("VaR", paste("e", 1:m, sep = ""), colNames) 
        f.con[1, (2+m):(2+m+w-1)] = as.numeric(mu)
        f.con[2, (2+m):(2+m+w-1)] = 1 
        f.con[3:(m+2), 1] = 1 
        f.con[3:(m+2), 2:(m+1)] = diag(m)
        f.con[3:(m+2), (2+m):(2+m+w-1)] = seriesData(Data)
        
        # Box and Group Constraints:
        tmpConstraints = .setConstraints(data, spec, constraints)
        nConstraints = dim(tmpConstraints)
        append = cbind(matrix(0, ncol = 1+m, nrow = (nConstraints[1]-2)),
            tmpConstraints[3:nConstraints[1], 1:(nConstraints[2]-1)])
        f.con = rbind(f.con, append)
       
        # Set Directions:
        f.dir = c("==", "==", rep(">=", m))
        f.dir = c(f.dir, rep(">=", (nConstraints[1]-2)))
        names(f.dir) = rownames(f.con)
      
        # Compose Right Hand Side Vector:
        f.rhs = c(targetReturn, 1, rep(0, m))
        f.rhs = c(f.rhs, tmpConstraints[3:(nConstraints[1]), nConstraints[2]])
        names(f.rhs) = rownames(f.con)
        
        # Optimize Portfolio:
        ans = lp("max", f.obj, f.con, f.dir, f.rhs)
        class(ans) <- "list"
        
        # Prepare Output List:
        ans$weights = ans$solution[(m+2):(m+1+w)] 
        for(i in 1:w) {
            if(abs(ans$weights[i]) < sqrt(.Machine$double.eps)) 
                ans$weights[i] = 0
        }
        attr(ans$weights, "error") <- ans$ierr
        ans$weights[as.logical(ans$ierr)] = NA
        ans$VaR = ans$solution[1]
        ans$CVaR = -ans$objval 
        ans$ierr = ans$status
        ans$solver = "lpSolve"
        ans$targetAlpha = targetAlpha
    }
    
    # For Debugging ...
    .DEBUG <<- ans

    # Return Value:
    ans
}


################################################################################

