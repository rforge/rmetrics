
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
#  solveRquadprogCLA            Portfolio interface to solver Rquadprog
#  .rquadprogCLAArguments       Returns arguments for solver
#  .rquadprogCLA                Wrapper to solver function
#  .rquadprogCLAControl         Returns default controls for solver
################################################################################


solveRquadprogCLA <-
    function(data, spec, constraints)
{
    # Description:
    #   Portfolio interface to solver Rquadprog

    # Example:
    #   solveRquadprogCLA(data, spec, constraints)[-3]
   
    # FUNCTION:   

    # Transform Data:
    Data = portfolioData(data, spec)
    nAssets = getNAssets(Data)
    
    # Solve:
    ##if(nAssets == 2) {

        # Solve two Assets Portfolio Analytically:
        ## ans = .mvSolveTwoAssets(data, spec, constraints)
        # ... this is only  for 'unlimited' LongOnly constraints,
        # box and group constraints are discarded here.
            
    ##} else {
        
        # Compile Arguments for Solver:
        args = .rquadprogCLAArguments(data, spec, constraints)
        
        # Solve Multiassets Portfolio:
        ans = .rquadprogCLA(
            Dmat = args$Dmat, 
            dvec = args$dvec, 
            Amat = args$Amat, 
            bvec = args$bvec, 
            meq = args$meq,
            lambda = args$lambda)
         
        # Save Arguments:
        ans$optim$args = args
            
    ##}

    # Return Value:
    ans
}


################################################################################


.rquadprogCLAArguments <-
    function(data, spec, constraints)
{
    # Description:
    #   Returns quadprog conform arguments for the solver
    
    # Example:
    
    # FUNCTION:
    
    # Set up the default quadprog QP
    ans = .rquadprogArguments(data, spec, constraints)
    
    # min(-d^T x + 1/2 x^T D x) 
    
    # Start from it and modify for CLA:
    lambda = spec@model$param$lambda
    ans$Dmat = lambda * getSigma(portfolioData(data)) / 2
    ans$dvec = getMu(portfolioData(data)) 
    ans$Amat = ans$Amat[, -1]
    ans$bvec = ans$bvec[-1]
    ans$meq = ans$meq - 1
    ans$dir = ans$dir[-1]
    ans$lambda = lambda

    # Return Value:
    ans
}


################################################################################


.rquadprogCLA <-
    function(Dmat, dvec, Amat, bvec, meq, lambda)
{
    # Description:
    #   Goldfarb and Idnani's quadprog solver function
    
    # Note:
    #   Requires to load contributed R package quadprog from which we use
    #   the Fortran subroutine of the quadratic solver.
    
    # Package: quadprog
    #   Title: Functions to solve Quadratic Programming Problems.
    #   Author: S original by Berwin A. Turlach <berwin.turlach@anu.edu.au>
    #       R port by Andreas Weingessel <Andreas.Weingessel@ci.tuwien.ac.at>
    #   Maintainer: Andreas Weingessel <Andreas.Weingessel@ci.tuwien.ac.at>
    #   Description: This package contains routines and documentation for
    #       solving quadratic programming problems.
    #   License: GPL-2
    
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

    # FUNCION:
    
    # Settings:
    n = nrow(Dmat)
    q = ncol(Amat)
    r = min(n, q)
    work = rep(0, 2 * n + r * (r + 5)/2 + 2 * q + 1)

    # Optimize:
    optim = .Fortran("qpgen2",
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
          
    # Set Tiny Weights to Zero:
    weights = .checkWeights(optim$sol)
    attr(weights, "invest") = sum(weights) 
    
    # Compose Output List:
    Sigma = Dmat * 2 / lambda
    mu = dvec
    ans = list(
        type = "MV",
        solver = "solveRquadprogCLA",
        optim = optim,
        weights = weights,
        targetReturn = (optim$sol %*% mu)[[1,1]],
        targetRisk = sqrt(optim$sol %*% Sigma %*% optim$sol)[[1,1]],
        objective = optim$crval,
        status = optim$ierr,
        message = NA)
            
    # Return Value:
    ans
}


################################################################################


.rquadprogCLAControl <-
    function()
{
    # Description:
    #   Returns default quadprog control settings
    
    # Arguments:
    #   none
    
    # FUNCTION:
    
    # This algorithm comes with no control parameter list
    
    NA
}


################################################################################

