
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
# FUNCTION:                    DESCRIPTION:
#  solveRsymphony               Portfolio interface to solver Rsymphony
#  .rsymphonyArguments          Returns arguments for solver
#  .rsymphony                   Wrapper to solver function
#  .rsymphonyControl            Returns default controls for solver
################################################################################


solveRsymphony <-
    function(data, spec, constraints)
{
    # Description:
    #   Portfolio interface to solver Rsymphony

    # Example:
    #   solveRsymphony(.lppData, .cvarSpec, "LongOnly")[-3]
    #   solveRsymphony(.lppData, .cvarSpec, .BoxGroups)[-3]
    
    # FUNCTION:   

    # Settings:
    Data = portfolioData(data, spec)
    nAssets = getNAssets(Data)
    
    # Solve:
    if(nAssets == 2) {

        # Solve two Assets Portfolio Analytically:
        # ... this is only thhought for 'unlimited' LongOnly Constraints
        # box and group constraints are discarded here.
        ans = .cvarSolveTwoAssets(data, spec, constraints)
            
    } else {
        
        # Compile Arguments for Solver:
        args = .rsymphonyArguments(data, spec, constraints)
        
        # Solve Multiassets Portfolio:
        ans = .rsymphony(
            obj = args$obj, 
            mat = args$mat, 
            dir = args$dir, 
            rhs = args$rhs, 
            bounds = args$bounds, 
            types = args$types, 
            max = args$max,
            nScenarios = args$nScenarios, 
            nAssets = args$nAssets,
            targetReturn = args$targetReturn, 
            Alpha = args$Alpha)
            
    }

    # Return Value:
    ans
}


################################################################################


.rsymphonyArguments <-
    function(data, spec, constraints)
{
    # Description:
    #   Returns symphony conform arguments for the solver
  
    # Details:
    #       max/min:                   obj %*% x
    #       subject to: 
    #                            mat %*%x  ?=  rhs                           
    #                                  dir  =  "?=" 
    #
    #   Rsymphony_solve_LP(obj, mat, dir, rhs, bounds = NULL, 
    #       types = NULL, max = FALSE)
    
    # Example:
    #   .rsymphonyArguments(.lppData, .cvarSpec, "LongOnly")[-2]
    #   .rsymphonyArguments(.lppData, .cvarSpec, .BoxGroups)[-2]
    
    # FUNCTION:
    
    # Settings:
    Data = portfolioData(data, spec)
    nAssets = getNAssets(Data)
    nScenarios = nrow(getSeries(Data))
    targetReturn = getTargetReturn(spec)
    Alpha = getAlpha(spec)
    
    # Objective Function:
    objNames = c("VaR", paste("e", 1:nScenarios, sep = ""), colnames(data))
    obj = c(1, -rep(1/(Alpha*nScenarios), nScenarios), rep(0, nAssets))
    names(obj) = objNames
    
    # The A_equal Equation Constraints: A_eq %*% x == a_eq
    eqsumW = eqsumWConstraints(data, spec, constraints)
    Aeq = cbind(matrix(0, ncol = 1+nScenarios, nrow = nrow(eqsumW)), eqsumW[, -1])
    aeq = eqsumW[, 1]
    deq = rep("==", nrow(eqsumW))
    
    # The VaR Equation Constraints:  (1 + diag + Returns) %*% (VaR,es,W)  >= 0
    Avar = cbind(
        matrix(rep(-1, nScenarios), ncol = 1),
        diag(nScenarios),
        as.matrix(getSeries(Data)) )
    avar = rep(0, nrow(Avar))
    dvar = rep(">=", nrow(Avar))
  
    # The e_s > = 0 Equation Constraints:
    Aes = cbind(
        matrix(rep(0, nScenarios), ncol = 1),
        diag(nScenarios),
        matrix(0, nrow = nScenarios, ncol = nAssets) )
    aes = rep(0, nrow(Aes))
    des = rep(">=", nrow(Aes))
    
    # Group Constraints: A W >= a
    minsumW = minsumWConstraints(data, spec, constraints)
    if (is.null(minsumW)){    
        Aminsum = NULL
        aminsum = NULL
        dminsum = NULL
    } else {
        Aminsum = cbind(
            matrix(0, nrow = nrow(minsumW), ncol = 1+nScenarios), 
            minsumW[, -1, drop = FALSE] )
        aminsum = minsumW[, 1]
        dminsum  = rep(">=", nrow(minsumW))
    }
    
    # Group Constraints: A W <= b
    maxsumW = maxsumWConstraints(data, spec, constraints)
    if (is.null(maxsumW)){    
        Amaxsum = NULL
        amaxsum = NULL
        dmaxsum = NULL
    } else {
        Amaxsum = cbind(
            matrix(0, nrow = nrow(maxsumW), ncol = 1+nScenarios), 
            maxsumW[, -1, drop = FALSE] )
        amaxsum = maxsumW[, 1]
        dmaxsum  = rep("<=", nrow(maxsumW))
    }
       
    # Putting all together:
    mat = rbind(Aeq, Avar, Aes, Aminsum, Amaxsum)
    rhs = c(aeq, avar, aes, aminsum, amaxsum)
    dir = c(deq, dvar, des, dminsum, dmaxsum)
    
    # Box Constraints: Upper and Lower Bounds as listn required ...
    minW = minWConstraints(data, spec, constraints)
    maxW = maxWConstraints(data, spec, constraints)
    nInd = 1:(1+nScenarios+nAssets)
    bounds = list(
        lower = list(ind = nInd, val = c(rep(-Inf, 1+nScenarios), minW)),
        upper = list(ind = nInd, val = c(rep( Inf, 1+nScenarios), maxW)) )
    
    # What variable Types, All Continuous:
    types = NULL
    
    # Should I minimize or maximize ?
    max = TRUE
  
    # Return Value:
    list(
        obj = obj, mat = mat, dir = dir, rhs = rhs,
        bounds = bounds, types = types, max = max,
        nScenarios = nScenarios, nAssets = nAssets,
        targetReturn = targetReturn, Alpha = Alpha) 
}


################################################################################


.rsymphony <-
    function(obj, mat, dir, rhs, bounds, types, max,
    nScenarios, nAssets, targetReturn, Alpha)
{
    # Description:
    #   Rsymphony
	
	# FUNCTION:
	
	# Solve - use Rsymphony_solve_LP:
    optim <- Rsymphony::Rsymphony_solve_LP(
        obj = obj, 
        mat = mat, 
        dir = dir, 
        rhs = rhs, 
        bounds = bounds, 
        types = types,         
        max = max)   
        
    # Extract Weights:
    weights = .checkWeights(optim$solution[-(1:(nScenarios+1))])
    attr(weights, "invest") = sum(weights)
    
    # Result:
    ans <- list(
        solver = "Rsymphony",
        optim = optim,
        weights = weights, 
        targetReturn = targetReturn,
        targetRisk = -optim$objval, 
        objective = -optim$objval, 
        status = optim$status[[1]], 
        message = gsub("_", " ", tolower(names(optim$status))))    
        
    # Return Value:
    ans
}


################################################################################


.rsymphonyControl <-
    function()
{
    # Description:
    #   Returns default symphony control settings
    
    # Arguments:
    #   none
    
    # FUNCTION:
    
    # This algorithm comes with no control parameter list
    
    NA
}


################################################################################

