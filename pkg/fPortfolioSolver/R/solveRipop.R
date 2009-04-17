
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
#  solveRipop                   Portfolio interface to solver Ripop
#  .ripopArguments              Returns arguments for solver
#  .ripop                       Wrapper to solver function
#  .ripopControl                Returns default controls for solver
################################################################################


solveRipop <-
    function(data, spec, constraints)
{
    # Description:
    #   Portfolio interface to solver Ripop

    # Example:
    #   solveRipop(.lppData, .mvSpec, "LongOnly")[-3]
    #   solveRipop(.lppData, .mvSpec, .BoxGroups)[-3]
    
    # FUNCTION:   

    # Transform Data:
    Data = portfolioData(data, spec)
    nAssets = getNAssets(Data)
    
    # Solve:
    if(nAssets == 2) {

        # Solve two Assets Portfolio Analytically:
        ans = .mvSolveTwoAssets(data, spec, constraints)
        # ... this is only  for 'unlimited' LongOnly constraints,
        # box and group constraints are discarded here.
            
    } else {
        
        # Compile Arguments for Solver:
        args = .ripopArguments(data, spec, constraints)
        
        # Solve Multiassets Portfolio:
        ans = .ripop(
            c = args$c,
            H = args$H,
            A = args$A,
            b = args$b,
            l = args$l,
            u = args$u,
            r = args$r,
            targetReturn = args$targetReturn)
            
    }

    # Return Value:
    ans
}


################################################################################


.ripopArguments <-
    function(data, spec, constraints)
{
    # Description:
    #   Returns ipop conform arguments for the solver
    
    # Example:
    #   .ripopArguments(.lppData, .mvSpec, "LongOnly")
    #   .ripopArguments(.lppData, .mvSpec, .BoxGroups)
    
    # FUNCTION:
    
    # Settings:
    Data = portfolioData(data, spec)
    Sigma = getSigma(Data)
    nAssets = getNAssets(Data)
    targetReturn = getTargetReturn(spec)
    
    # Set up A_mat of Constraints:
    eqsumW = eqsumWConstraints(data, spec, constraints)
    minsumW = minsumWConstraints(data, spec, constraints)
    maxsumW = maxsumWConstraints(data, spec, constraints)
    Amat = eqsumW[, -1]
    if(!is.null(minsumW)) Amat = rbind(Amat, minsumW[, -1])
    if(!is.null(maxsumW)) Amat = rbind(Amat, maxsumW[, -1])

    # Set up Vector A_mat >= bvec of Constraints:
    BIG = 999
    Lower = Upper = eqsumW[, 1]
    L = minsumW[, 1]
    U = maxsumW[, 1]
    if(!is.null(minsumW)) Lower = c(Lower, L, rep(-BIG, length(U)))
    if(!is.null(maxsumW)) Upper = c(Upper, rep(BIG, length(L)), U)
    
    # Boxes:
    minW = minWConstraints(data, spec, constraints)
    maxW = maxWConstraints(data, spec, constraints)
    
    # Return Value:
    list(
        c = rep(0, nAssets), 
        H = Sigma, 
        A = Amat, 
        b = Lower, 
        l = minW, 
        u = maxW,
        r = Upper - Lower,
        targetReturn = targetReturn)   
}


################################################################################


.ripop <- 
function(c, H, A, b, l, u, r, targetReturn,
    sigf = 12, maxiter = 100, margin = 1e-8, bound = 10, verb = 0)
{
    # Description:
    #   "ipop" solves a quadratic programming problem
    
    # Details:
    #   minimize     c' * primal + 1/2 primal' * H * primal
    #   subject to   b <= A*primal <= b + r
    #                l <= primal <= u
    #                d is the optimizer itself
    #   returns primal and dual variables (i.e. x and the Lagrange
    #   multipliers for b <= A * primal <= b + r)
    #   for additional documentation see
    #       R. Vanderbei
    #       LOQO: An Interior Point Code for Quadratic Programming, 1992
    #   Author:      R version Alexandros Karatzoglou, 
    #                orig. matlab Alex J. Smola
    #   Created:     12/12/97
    #   R Version:   12/08/03
    #   Updated:     13/10/05
    #   Code:        A modified copy from contributed R package kernlab
    #                released under the GNU Public License
    #   Note:        A QP Solver entirely written in R from contributed 
    #                Package kernlab ...
    
    # Require:
    #   Package Ripop
    
    # FUNCTION:
    
    optim = Ripop::ipop(c, H, A, b, l, u, r, targetReturn,
        sigf, maxiter, margin, bound, verb)
        
    # Set Tiny Weights to Zero:
    weights = .checkWeights(optim$weights)
    attr(weights, "invest") = sum(weights)  
        
    # Compose Output List:
    ans = list(
        type = "MV",
        solver = "solveRipop",
        optim = optim,
        weights = weights,
        targetReturn = targetReturn,
        targetRisk = sqrt(weights %*% H %*% weights)[[1,1]],
        objective = sqrt(weights %*% H %*% weights)[[1,1]],
        status = status,
        message = message)
            
    # Return Value:
    ans
}


################################################################################


.ripopControl <-
    function()
{
    # Description:
    #   Returns default ipop control settings
    
    # Arguments:
    #   none
    
    # FUNCTION:
    
    # This algorithm comes with no control parameter list
    
    NA
}


################################################################################

