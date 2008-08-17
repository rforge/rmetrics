
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


################################################################################
# FUNCTION:                DESCRIPTION:
#  solveRdonlp2             Portfolio interface to solver Rdonlp2
#  .rdonlp2Arguments        Returns arguments for solver
#  .rdonlp2                 Wrapper to solver function
#  .rdonlp2Control          Returns default controls for solver
################################################################################


solveRdonlp2 <-
    function(data, spec, constraints)
{
    # Description:
    #   Portfolio interface to solver Rdonlp2

    # Example:
    #   data = .lppData; spec = .mvSpec; constraints = "LongOnly"
    #       minRisk <- function(x) { x %*% Sigma %*% x }
    #       Sigma = cov(data)
    #       fn = match.fun(getOptimize(spec))
    #   solveRdonlp2(.lppData, .mvSpec, "LongOnly")[-3]
    #   solveRdonlp2(.lppData, .mvSpec, .BoxGroups)[-3]
    #   solveRdonlp2(.lppData, .mvSpec, .CovBudgets)[-3] 
    #   portfolioTest("MV", "minRisk", "solveRdonlp2", "LongOnly")
    #   portfolioTest("MV", "minRisk", "solveRdonlp2", "BoxGroup") 
    #   portfolioTest("MV", "minRisk", "solveRdonlp2", "CovBudget")
    
    # FUNCTION:   

    # Settings:
    Data = portfolioData(data, spec)
    nAssets = getNAssets(Data)
    
    # Solve:
    if(nAssets == 2) {

        # Solve two Assets Portfolio Analytically:
        # ... this is only thhought for 'unlimited' LongOnly Constraints
        # box and group constraints are discarded here.
        # ans = .mvSolveTwoAssets(data, spec, constraints)
            
    } else {
        
        # Compile Arguments for Solver:
        args = .rdonlp2Arguments(data, spec, constraints)
        
        # Solve Multiassets Portfolio:
        ans = .rdonlp2(
            par = args$par,
            fn = args$fun, 
            par.lower = args$par.lower, 
            par.upper = args$par.upper, 
            A = args$A, 
            lin.lower = args$lin.lower, 
            lin.upper = args$lin.upper,
            nlin = args$nlin, 
            nlin.lower = args$nlin.lower, 
            nlin.upper = args$nlin.upper,
            targetReturn = args$targetReturn)
            
    }

    # Return Value:
    ans
}


################################################################################
# Here we solve the quadprog problem with box/group and optional
# risk budget constraints ...
  
    
.rdonlp2Arguments <-
function(data, spec, constraints)
{
    # Description:
    #   Create Arguments for Rdonlp2
    
    # Details:
    #       min:                    fn(x)
    #       subject to: 
    #                     par.lower <= x <= par.upper
    #                  lin.lower <= A %*% x <= lin.upper
    #                   in.lower <= nlin(x) <= lin.upper  
    
    # Example:
    #   .rdonlp2Arguments(.lppData, .mvSpec, "LongOnly") 
    #   .rdonlp2Arguments(.lppData, .mvSpec, .BoxGroup)
    #   .rdonlp2Arguments(.lppData, .mvSpec, .CovBudgets)
    #   .rdonlp2Arguments(.lppData, .mvSpec, c("minB[2:3]=0.1", "maxB[3:5]=0.9"))  
    #   portfolioTest("MV", "minRisk", "solveRdonlp2", "LongOnly")
    #   portfolioTest("MV", "minRisk", "solveRdonlp2", "BoxGroup") 
    #   portfolioTest("MV", "minRisk", "solveRdonlp2", "CovBudget")   

    DEBUG = FALSE
    
    # Settings:
    Data = portfolioData(data)
    nAssets = getNAssets(Data)
    mu <- getMu(Data)
    Sigma <- getSigma(Data)
    targetReturn = getTargetReturn(spec)
    
    # Objective Function 'fn' to be optimized:
    maxReturn <- function(x, mu) { x %*% mu }
    minRisk <- function(x, Sigma) { x %*% Sigma %*% x }
    fn = match.fun(getOptimize(spec))

    # Box Constrains:
    par.lower = minWConstraints(data, spec, constraints)
    par.upper = maxWConstraints(data, spec, constraints)
    if(DEBUG) print(rbind(par.lower, par.upper))
    
    # Linear / Group Constraints:
    eqsumW = eqsumWConstraints(data, spec, constraints)
    Aeqsum = eqsumW[, -1]
    aeqsum = eqsumW[, 1]
    minsumW = minsumWConstraints(data, spec, constraints)
    if (is.null(minsumW)) {
        Aminsum = aminsum = NULL
    } else {
        Aminsum = minsumW[, -1]
        aminsum = minsumW[, 1]
    }      
    maxsumW = maxsumWConstraints(data, spec, constraints)
    if (is.null(maxsumW)) {
        Amaxsum = amaxsum = NULL
    } else {
        Amaxsum = maxsumW[, -1]
        amaxsum = maxsumW[, 1]
    }      
    A = rbind(Aeqsum, Aminsum, Amaxsum)
    lin.lower = c(aeqsum, aminsum, rep(-Inf, length(amaxsum)))
    lin.upper = c(aeqsum, rep(Inf, length(aminsum)), amaxsum)
    if(DEBUG) print(cbind(lin.lower, A, lin.upper))
   
    # Nonlinear Constraints - Here Covariance Risk Budgets:
    nlin = list()
    nlin.lower = NULL
    nlin.upper = NULL
    
    # Check Constraints Strings for Risk Budgets:
    # Example: constraints = c("minB[2:3]=0.1", "maxB[3:5]=0.9")
    validStrings = c("minB", "maxB")
    usedStrings = unique(sort(sub("\\[.*", "", constraints)))
    checkStrings = sum(usedStrings %in% validStrings)
    includeRiskBudgeting = as.logical(checkStrings)
    if (DEBUG) print(includeRiskBudgeting)
    
    if (includeRiskBudgeting) {
        # Compose Non-Linear (Cov Risk Budget) Constraints Functions:
        nlcon <- function(x) {
            B1 = as.vector(x %*% Sigma %*% x)
            B2 = as.vector(x * Sigma %*% x)
            B = B2/B1
            B
        }
        if(DEBUG) print(nlcon)
        
        # Compose non-linear functions now for each asset ...
        for (I in 1:nAssets)
            eval( parse(text = paste(
                "nlcon", I, " = function(x) { nlcon(x)[", I, "] }", sep = "")) )
        nlinFunctions = paste("nlcon", 1:nAssets, sep = "", collapse = ",")
        nlinFunctions = paste("list(", nlinFunctions, ")")
        nlin = eval( parse(text = nlinFunctions) )
        if(DEBUG) print(nlin)

        # ... and finally Compose Constraints Vectors:
        nlin.lower = minBConstraints(data, spec, constraints)
        nlin.upper = maxBConstraints(data, spec, constraints)
        if(DEBUG) print(rbind(nlin.lower, nlin.upper))
    }
 
    # Return Value:
    list(
        par = rep(1/nAssets, nAssets), fn = fn,
        par.lower = par.lower, par.upper = par.upper, 
        A = A, lin.lower = lin.lower, lin.upper = lin.upper,
        nlin = nlin, nlin.lower = nlin.lower, nlin.upper = nlin.upper,
        targetReturn = targetReturn)
}
    

################################################################################


.rdonlp2 <-
    function(par, fn, par.lower, par.upper, A, lin.lower, lin.upper,
    nlin, nlin.lower, nlin.upper, targetReturn = targetReturn)
{
    # Description:
    #   Rdonlp2 Wrapper    
	
	# FUNCTION:
	
	# Solve:
    optim <- R.donlp2::donlp2(
        par = par,
        fn = fn, 
        par.lower = par.lower, 
        par.upper = par.upper, 
        A = A, 
        lin.lower = lin.lower, 
        lin.upper = lin.upper,
        nlin = nlin, 
        nlin.lower = nlin.lower, 
        nlin.upper = nlin.upper)   
    
    # Extract Weights:
    weights = .checkWeights(optim$par)
    attr(weights, "invest") = sum(weights)
    
    # Check Messages and Get Status:
    Status = 1
    # Message = "1234567890123456789012345"
    message11 = "KT-conditions satisfied, " # no further correction computed"
    message12 = "computed correction small" # , regular case"
    message13 = "stepsizeselection: x almo" # st feasible, dir. deriv. very small"
    if (substr(optim$message, 1, 25) == message11) Status = 0 
    if (substr(optim$message, 1, 25) == message12) Status = 0 
    if (substr(optim$message, 1, 25) == message13) Status = 0 
    
    # Result:
    ans <- list(
        type = "MV",
        solver = "solveRdonlp2",
        optim = optim,
        weights = weights, 
        targetReturn = targetReturn,
        targetRisk = sqrt(optim$fx), 
        objective = sqrt(optim$fx), 
        status = Status, 
        message = optim$message)    
        
    # Return Value:
    ans
}


################################################################################


.rdonlp2Control <- 
    function()         
{
    list(
        iterma = 4000,
        nstep = 20, 
        fnscale = 1,
        report = FALSE, 
        rep.freq = 1,
        tau0 = 1.0, 
        tau = 0.1, 
        del0 = 1.0,
        epsx = 1.0e-5, 
        delmin = 0.1, # suggested 0.1*del0
        epsdif = 1e-8, 
        nreset.multiplier = 1,
        difftype = 3, 
        epsfcn = 1.0e-16, 
        taubnd = 1.0,
        hessian = FALSE,
        te0 = TRUE, 
        te1 = FALSE, 
        te2 = FALSE, 
        te3 = FALSE,
        silent = TRUE,
        intakt = TRUE)
}


################################################################################

