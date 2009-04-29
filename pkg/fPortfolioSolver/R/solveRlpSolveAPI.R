
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
#  solveRlpSolveAPI             Portfolio interface to solver RlpSolveAPI
#  .rlpSolveAPIArguments        Returns arguments for solver
#  .rlpSolveAPI                 lpSolve linear solver function
#  .rlpSolveAPIControl          Returns default controls for solver
################################################################################


solveRlpSolveAPI <-
    function(data, spec, constraints)
{
    # Description:
    #   Portfolio interface to solver RlpSolveAPI

    # Example:
    #   solveRlpSolveAPI(.lppData, .cvarSpec, "LongOnly")[-3]
    #   solveRlpSolveAPI(.lppData, .cvarSpec, .BoxGroups)[-3]
    #   portfolioTest("CVaR", "minRisk", "solveRlpSolveAPI", "LongOnly")
    #   portfolioTest("CVaR", "minRisk", "solveRlpSolveAPI", "BoxGroup")

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
    
        # Get conform arguments:
        args = .RlpSolveAPIArguments(data, spec, constraints)
        
        # Solve Portfolio:
        ans = .rlpSolveAPI(
            obj = args$obj, 
            A = args$A, 
            b = args$b, 
            Aeq = args$Aeq, 
            beq = args$beq, 
            lb = args$lb, 
            ub = args$ub,
            nScenarios = args$nScenarios, 
            nAssets = args$nAssets,
            targetReturn = args$targetReturn, 
            Alpha = args$Alpha)
            
    }
    
    # Return Value:
    class(ans) = c("solveRfoo", "list")
    ans
}


################################################################################


.RlpSolveAPIArguments <-
    function(data, spec, constraints)
{
    # Description:
    #   Create Argument List for Linear Solver lpSolveAPI
  
    # Details:
    #       min:                    obj %*% x
    #       subject to: 
    #                               A %*%x  <= b
    #                            A_eq %*% x == beq 
    #                         lb[j] <= x[j] <= ub[j] 
    #
    #   lpSolve(
    #       obj, A, b, Aeq = NULL, beq = NULL, 
    #       lb = 0, ub = Inf,
    #       intvec = integer(0), control = list())
    
    # Example:
    #   solveRlpSolveAPI(.lppData, .cvarSpec, "LongOnly")[-2]
    #   solveRlpSolveAPI(.lppData, .cvarSpec, .BoxGroup)[-2]
    
    # FUNCTION:
    
    # Transform Data and Constraints:
    Data = portfolioData(data, spec)
    nScenarios = nrow(getSeries(Data))
    nAssets = getNAssets(Data)

    # Get Target Return and Quantile Alpha:
    targetReturn = getTargetReturn(spec)[1]
    Alpha = getAlpha(spec)
    
    # Objective Function:
    objNames = c("VaR", paste("e", 1:nScenarios, sep = ""), colnames(data))
    obj = -c(1, -rep(1/(Alpha*nScenarios), nScenarios), rep(0, nAssets))
    names(obj) = objNames
    
    # The VaR Equation Constraints:  (1 + diag + Returns) %*% (VaR,es,W)  >= 0
    Avar = -cbind(
        matrix(rep(-1, nScenarios), ncol = 1),
        diag(nScenarios),
        as.matrix(getSeries(Data)) )
    avar = rep(0, nrow(Avar))
  
    # The e_s > = 0 Equation Constraints:
    Aes = -cbind(
        matrix(rep(0, nScenarios), ncol = 1),
        diag(nScenarios),
        matrix(0, nrow = nScenarios, ncol = nAssets) )
    aes = rep(0, nrow(Aes))
    
    # Group Constraints: A W >= a
    minsumW = minsumWConstraints(data, spec, constraints)
    if (is.null(minsumW)){    
        Aminsum = aminsum = NULL
    } else {
        Aminsum = -cbind(
            matrix(0, nrow = nrow(minsumW), ncol = 1+nScenarios), 
            minsumW[, -1, drop = FALSE] )
        aminsum = -minsumW[, 1]
    }
    
    # Group Constraints: A W <= b
    maxsumW = maxsumWConstraints(data, spec, constraints)
    if (is.null(maxsumW)){    
        Amaxsum = amaxsum = NULL
    } else {
        Amaxsum = cbind(
            matrix(0, nrow = nrow(maxsumW), ncol = 1+nScenarios), 
            maxsumW[, -1, drop = FALSE] )
        amaxsum = maxsumW[, 1]
    }
    
    # Compose:
    A = rbind(Avar, Aes, Aminsum, Amaxsum)
    b = c(avar, aes, aminsum, amaxsum)   
    
    # The A_equal Equation Constraints: A_eq %*% x == a_eq
    eqsumW = eqsumWConstraints(data, spec, constraints)
    Aeq = cbind(matrix(0, ncol = 1+nScenarios, nrow = nrow(eqsumW)), eqsumW[, -1])
    beq = eqsumW[, 1]
       
    # Box Constraints: Upper and Lower Bounds ...
    minW = minWConstraints(data, spec, constraints)
    maxW = maxWConstraints(data, spec, constraints)
    lb = c(-Inf, rep(0, nScenarios), minW)
    ub = c(rep( Inf, 1+nScenarios), maxW)
    
    # Return Value:
    list(
        obj = obj, 
        A = A, b = b, Aeq = Aeq, beq = beq, lb = lb, ub = ub,
        nScenarios = nScenarios, nAssets = nAssets,
        targetReturn = targetReturn, Alpha = Alpha) 
}


################################################################################   


.rlpSolveAPI =
    function(obj, A, b, Aeq, beq, lb, ub, 
    nScenarios, nAssets, targetReturn, Alpha)
{
    # Description:
	#   Linear programming solver function for lpSolveAPI
	
	# FUNCTION:
	
	# Solve - use lpSolve from KK:
    optim <- RlpSolveAPI::lpSolveAPI(
        obj = obj,  
        A = A,  
        b = b,  
        Aeq = Aeq, 
        beq = beq,  
        lb = lb,  
        ub = ub)    
        
    # Extract Weights:
    weights = .checkWeights(optim$x[-(1:(nScenarios+1))])
    attr(weights, "invest") = sum(weights)
    
    # Result:
    ans <- list(
        type = "CVaR",
        solver = "RlpSolveAPI",
        optim = optim,
        weights = weights, 
        targetReturn = targetReturn[1],
        targetRisk = optim$objective,
        objective = optim$objective, 
        status = optim$status, 
        message = optim$message)  
    class(ans) = "list"  
        
    # Return Value:
    ans
}


################################################################################


.rlpSolveControl <-
    function()
{
    # Description:
    #   Returns default quadprog control settings
    
    # Arguments:
    #   none
    
    # FUNCTION:
    
    # Not yet implemented ...
    
    NA
}


################################################################################

