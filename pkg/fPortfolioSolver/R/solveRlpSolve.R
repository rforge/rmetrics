
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
#  solveRlpSolve                Portfolio interface to solver RlpSolve
#  .rlpSolveArguments           Returns arguments for LP solver
#  .rlpSolve                    Wrapper to solver function
#  .rlpSolveControl             Returns default controls for solver
################################################################################


solveRlpSolve <-
    function(data, spec, constraints)
{
    # Description:
    #   Portfolio interface to solver RlpSolve

    # Example:
    #   solveRlpSolve(.lppData, .cvarSpec, "LongOnly")[-3]
    #   solveRlpSolve(.lppData, .cvarSpec, .BoxGroups)[-3]
    #   portfolioTest("CVaR", "minRisk", "solveRlpSolve", "LongOnly")
    #   portfolioTest("CVaR", "minRisk", "solveRlpSolve", "BoxGroup")
    
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
    
        # Compute the Arguments from data, spec and constraints
        #   which are required by the linear solver function 'lpSolve':
        args = .rlpSolveArguments(data, spec, constraints)
        
        # Now Optimize the Portfolio:
        ans = .rlpSolve( 
            direction = args$direction, 
            objective.in = args$objective.in, 
            const.mat = args$const.mat, 
            const.dir = args$const.dir, 
            const.rhs = args$const.rhs,
            nScenarios = args$nScenarios, 
            nAssets = args$nAssets,
            targetReturn = args$targetReturn,
            Alpha = args$Alpha)
                
    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.rlpSolveArguments <-
function(data, spec, constraints)
{
    # Description:
    #   Returns lpSolve conform arguments for the solver
    
    # Details:
    #       max/min:             objective.in %*% x
    #       subject to: 
    #                        const.mast %*%x  ?=  const.rhs                           
    #                               const.dir  =  "?=" 
    #
    #   lpSolve(direction = "min", objective.in, const.mat, const.dir, 
    #       const.rhs, transpose.constraints = TRUE, int.vec, presolve = 0, 
    #       compute.sens = 0, binary.vec, all.int = FALSE, all.bin = FALSE, 
    #       scale = 196, dense.const, num.bin.solns = 1, use.rw = FALSE)
   
    # Example:
    #   data = 100*as.timeSeries(data(LPP2005REC))[1:8, 1:3]
    #   spec = portfolioSpec(); setTargetReturn(spec) = mean(data)
    #       setType(spec) = "CVaR"; setSolver(spec) = "solveRlpSolve"
    #   constraints = c("minW[1:2]=0.1", "maxW[2:3]=0.9","minsumW[1:2]=0.2","maxsumW[c(2,3)]=0.8")
    #
    #   constraints = "LongOnly" 
    #   .rlpSolveArguments(data, spec, constraints)
    
    # FUNCTION:
    
    # Settings:
    Data = portfolioData(data, spec)
    nScenarios = nrow(getSeries(Data))
    nAssets = getNAssets(Data)
    targetReturn = getTargetReturn(spec)[1]
    Alpha = getAlpha(spec)
    
    # Objective Function:
    objNames = c("VaR", paste("e", 1:nScenarios, sep = ""), colnames(data))
    objective.in = 
        -c(1, -rep(1/(Alpha*nScenarios), nScenarios), rep(0, nAssets))
    names(objective.in) = objNames

    # Info on CVaR constraints - Constraint matrix:
    #   Example m=8 Data Records, and w=4 Assets
    #
    #   VaR  es            weights          exposure
    #   x1   x2  ...  x9   x10 ... x13
    #   
    #    0    0       0    mu1     mu4      == Mu       target Return
    #    0    0       0    1       1        == 1        full Investment
    #
    #   -1    1       0    r1.1    r4.1     >= 0        -Var + e_s -Wr >= 0
    #   -1    0  1    0    r1.2    r4.2     >= 0
    #   -1    0    1  0    r1.8    r4.8     >= 0
    #   -1    0       1    r1.9    r4.9     >= 0
    #
    #    0    1       0     0       0       >= 0        e_s  >= 0
    #    0    0  1    0     0       0       >= 0
    #    0    0    1  0     0       0       >= 0
    #    0    0       1     0       0       >= 0
    #                                    
    #                                       >= 0        A W >= 0
    #                                       >= 0        W >= 0
   
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
        Aminsum = aminsum = dminsum = NULL
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
        Amaxsum = amaxsum = dmaxsum = NULL
    } else {
        Amaxsum = cbind(
            matrix(0, nrow = nrow(maxsumW), ncol = 1+nScenarios), 
            maxsumW[, -1, drop = FALSE] )
        amaxsum = maxsumW[, 1]
        dmaxsum  = rep("<=", nrow(maxsumW))
    }
       
    # The W > a Equation Constraints:
    minW = minWConstraints(data, spec, constraints)
    if (is.null(minW)){    
        Amin = amin = dmin = NULL
    } else {
        Amin = cbind(
            matrix(0, nrow = nAssets, ncol = 1+nScenarios), 
            diag(nAssets))
        amin = minW
        dmin  = rep(">=", nAssets)
    }
    
    # The W < a Equation Constraints:
    maxW = maxWConstraints(data, spec, constraints)
    if (is.null(maxW)){    
        Amax = amax = dmax = NULL
    } else {
        Amax = cbind(
            matrix(0, nrow = nAssets, ncol = 1+nScenarios), 
            diag(nAssets))
        amax = maxW
        dmax  = rep("<=", nAssets)
    }
    
    const.mat = rbind(Aeq, Avar, Aes, Aminsum, Amaxsum, Amin, Amax)
    const.rhs =     c(aeq, avar, aes, aminsum, amaxsum, amin, amax)
    const.dir =     c(deq, dvar, des, dminsum, dmaxsum, dmin, dmax)
    
    # Return Value:
    list(
        direction = "min", 
        objective.in = objective.in, 
        const.mat = const.mat, 
        const.dir = const.dir, 
        const.rhs = const.rhs, 
        nScenarios = nScenarios, 
        nAssets = nAssets,
        targetReturn = targetReturn,
        Alpha = Alpha)      
}


# ------------------------------------------------------------------------------


.rlpSolve <- 
    function(direction, 
    objective.in, const.mat, const.dir, const.rhs,
    nScenarios, nAssets, targetReturn, Alpha)
{
	# Description:
	#   Linear programming solver function for solver lpSolve
	
	# FUNCTION:
	
	# Optimize:
	optim = RlpSolve::lpSolve(
	    direction, objective.in, const.mat, const.dir, const.rhs,
	    presolve = 0, scale = 0)
	
	# Extract Weights:
    weights = .checkWeights(optim$solution[-(1:(nScenarios+1))])
    attr(weights, "invest") = sum(weights)
    
    # Prepare Output List:
    ans = list(
        type = "CVaR",
        solver = "RlpSolve",
        optim = optim,
        weights = weights,
        targetReturn = targetReturn,
        targetRisk = optim$objval,
        objective = optim$objval, 
        status = optim$status, 
        message = "")  
    
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

  