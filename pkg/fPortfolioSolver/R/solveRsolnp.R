
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
#  solveRsolnp              Portfolio interface to solver Rsolnp
#  .rsolnpArguments         Returns arguments for solver
#  .rsolnp                  Wrapper to solver function
#  .rsolnpControl           Returns default controls for solver
################################################################################


# DW: DO NOT USE !!!


solveRsolnp <-
    function(data, spec, constraints)
{
    # Description:
    #   Portfolio interface to solver Rsolnp
    
    # Arguments;
    #   data - an object of class timeSeries
    #   spec - an object of class fPFOLIOSPEC
    #   constraints - an object of class character
    
    # FUNCTION:   

    # Settings:
    Data = portfolioData(data, spec)
    nAssets = getNAssets(Data)
    mu = getMu(Data)
    Sigma = getSigma(Data)
        
    # Compile Arguments for Solver:
    args = .rsolnpArguments(data, spec, constraints)
    
    # Solve Multiassets Portfolio:
    ans = .rsolnp(
        pars, 
        fun, 
        grad = NULL, 
        eqfun = NULL, 
        eqB = NULL, 
        eqgrad = NULL, 
        ineqfun = NULL, 
        ineqLB = NULL, 
        ineqUB = NULL, 
        ineqgrad = NULL, 
        LB = NULL, 
        UB = NULL, 
        control = list()) 
        
    returnFun = match.fun(getObjective(spec)[2])
    ans$targetReturn = returnFun(ans$weights)
    riskFun = match.fun(getObjective(spec)[3])
    ans$targetRisk = riskFun(ans$weights)

    # Return Value:
    class(ans) = c("solveRfoo", "list")
    ans
}
 
  
# ------------------------------------------------------------------------------


.rsolnpArguments <-
function(data, spec, constraints)
{
    # Description:
    #   Create Arguments for Rsolnp
    
    # Details:
    #       min:                    fun(x)
    #       subject to: 
    #                        g_i(x) = eqB
    #                  ineqLB <= h_i(x) <= ineqUB
    #                       LB <= x <= UB  
 
    # FUNCTION:  

    DEBUG = FALSE
    
    # Settings:
    Data = portfolioData(data)
    nAssets = getNAssets(Data)  
    mu = getMu(Data)
    Sigma = getSigma(Data)
    fn = match.fun(getObjective(spec)[1])
     
    # Box Constrains:
    LB = minWConstraints(data, spec, constraints)
    UB = maxWConstraints(data, spec, constraints)
    if(DEBUG) print(rbind(par.lower, par.upper))
    
    # Linear / Group Constraints:
    # ... targetReturn may be not defined,then set it to NA
    if (is.null(getTargetReturn(spec))) setTargetReturn(spec) <- NA
    # ... has in the first line the return constraint, if NA then ignore  it
    eqsumW = eqsumWConstraints(data, spec, constraints)
    if (is.na(eqsumW[1, 1])) eqsumW = eqsumW[-1, , drop= FALSE]
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
    
    # General non-lin Portfolio Constraints:
    # ... todo: currently overwrites previous selection
    nlin = listFConstraints(data, spec, constraints)
    if(DEBUG) print(nlin)
    nlin.lower = minFConstraints(data, spec, constraints)
    nlin.upper = maxFConstraints(data, spec, constraints)
    if(DEBUG) print(cbind(nlin.lower, nlin.upper))
 
    # Return Value:
    list(
        par = rep(1/nAssets, nAssets), fn = fn,
        par.lower = par.lower, par.upper = par.upper, 
        A = A, lin.lower = lin.lower, lin.upper = lin.upper,
        nlin = nlin, nlin.lower = nlin.lower, nlin.upper = nlin.upper)
}
    

################################################################################


.rsolnp <-
function(
    pars, 
    fun, 
    grad = NULL, 
    eqfun = NULL, 
    eqB = NULL, 
    eqgrad = NULL, 
    ineqfun = NULL, 
    ineqLB = NULL, 
    ineqUB = NULL, 
    ineqgrad = NULL, 
    LB = NULL, 
    UB = NULL, 
    control = list())
{
    # Description:
    #   Rsolnp Wrapper    
    
    # FUNCTION:
    
    # Solve:
    optim <- Rsolnp::solnp(
        pars = pars,
        fun = fun, 
        grad = grad, 
        eqfun = eqfun, 
        eqB = eqB, 
        eqgrad = eqgrad, 
        ineqfun = ineqfun, 
        ineqLB = ineqLB, 
        ineqUB = ineqUB, 
        ineqgrad = ineqgrad, 
        LB = LB, 
        UB = UB, 
        control = control)   
    
    # Extract Weights:
    weights = .checkWeights(optim$par)
    attr(weights, "invest") = sum(weights)
    
    # Check Messages and Get Status:
    #   ... unfortunately solnp has no status vaqriable, 
    #       so we have to analyze the messages
    Status = 1
    
    # Result:
    ans <- list(
        type = "MV",
        solver = "solveRsolnp",
        optim = optim,
        weights = weights, 
        objective = optim$fx, 
        status = Status, 
        message = optim$message)    
        
    # Return Value:
    ans
}


################################################################################


.rsolnpControl <- 
    function()         
{
    list(
        NA)
}


################################################################################

