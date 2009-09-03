
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
#  solveRcplex               Portfolio interface to solver Rcplex
#  .rcplexArguments          Returns arguments for solver
#  .rcplex                   Wrapper to solver function
#  .rcplexControl            Returns default controls for solver
################################################################################


solveRcplex <-
    function(data, spec, constraints)
{
    # Description:
    #   Portfolio interface to solver Rcplex

    # Example:
    #   solveRcplex(data, spec, constraints)[-3]
    #   solveRcplex(.lppData, .mvSpec, "LongOnly")[-3]
    #   solveRcplex(.lppData, .mvSpec, "LongOnly")$optim$args
    #   solveRcplex(.lppData, .mvSpec, c("LongOnly", "partial"))$optim$args
    #   solveRcplex(.lppData, .mvSpec, .BoxGroups)[-3]
    #   portfolioTest("MV", "minRisk", "solveRcplex", "LongOnly")
    #   portfolioTest("MV", "minRisk", "solveRcplex", "BoxGroup")

    # FUNCTION:

    # Transform Data:
    Data = portfolioData(data, spec)
    data <- getSeries(Data)
    nAssets = getNAssets(Data)

    # Solve:
    if(nAssets == 2) {

        # Solve two Assets Portfolio Analytically:
        ans = .mvSolveTwoAssets(Data, spec, constraints)
        # ... this is only  for 'unlimited' LongOnly constraints,
        # box and group constraints are discarded here.

    } else {

        # Compile Arguments for Solver:
        args = .rcplexArguments(Data, spec, constraints)

        # Solve Multiassets Portfolio:
        ans = .rcplex(
            Dmat = args$Dmat,
            dvec = args$dvec,
            Amat = args$Amat,
            bvec = args$bvec,
            dir = args$dir)

        # Save Arguments:
        ans$optim$args = args
        
        # class:
        class(ans) = c("solveRfoo", "list")

        ##if(ans$status != 0)
        ##print(ans)
    }

    # Return Value:
    ans
}


################################################################################


.rcplexArguments <-
    function(data, spec, constraints)
{
    # Description:
    #   Returns cplex conform arguments for the solver

    # Example:
    #   .rcplexArguments(.lppData, .mvSpec, "LongOnly")
    #   .rcplexArguments(.lppData, .mvSpec, .BoxGroups)

    # FUNCTION:

    # Data and Constraints as S4 Objects:
    Data = portfolioData(data, spec)
    data <- getSeries(Data)
    Sigma = getSigma(Data)
    nAssets = getNAssets(Data)

    # Set up A_mat of Constraints:
    eqsumW = eqsumWConstraints(Data, spec, constraints)
    minsumW = minsumWConstraints(Data, spec, constraints)
    maxsumW = maxsumWConstraints(Data, spec, constraints)
    Amat = rbind(eqsumW[, -1], diag(nAssets), -diag(nAssets))
    if(!is.null(minsumW)) Amat = rbind(Amat, minsumW[, -1])
    if(!is.null(maxsumW)) Amat = rbind(Amat, -maxsumW[, -1])

    # Set up Vector A_mat >= bvec of Constraints:
    minW = minWConstraints(Data, spec, constraints)
    maxW = maxWConstraints(Data, spec, constraints)
    bvec = c(eqsumW[, 1], minW, -maxW)
    if(!is.null(minsumW)) bvec = c(bvec, minsumW[, 1])
    if(!is.null(maxsumW)) bvec = c(bvec, -maxsumW[, 1])

    # Part (meq=1) or Full (meq=2) Investment, the Default ?
    meq = nrow(eqsumW)

    # Directions:
    dir = c(
        rep("E", times = meq),
        rep("G", times = length(bvec) - meq))

    # Return Value:
    list(
        Dmat = Sigma, dvec = rep(0, nAssets),
        Amat = Amat, bvec = bvec, dir = dir)
}


################################################################################


.rcplex <-
    function(Dmat, dvec, Amat, bvec, dir)
{
    # Description:
    #   CPLEX solver function

    # Note:
    #   Requires to load contributed R package Rcplex. We use CPLEX's
    #   facilities to solve quadratic problems.

    # Package: Rcplex
    # Package: Rcplex
    # Version: 0.2-3
    # Date: 2009-09-02
    # Title: R interface to CPLEX
    # Author: Hector Corrada Bravo, with contributions from Stefan Theussl
    # Maintainer: Hector Corrada Bravo <hcorrada@gmail.com>
    # Description: R interface to CPLEX solvers for linear, quadratic, and (linear and quadratic) mixed integer programs. A working installation of CPLEX is required for usage of the Rcplex package. See the file "INSTALL" for details on how to install the Rcplex package in Linux/Unix-like systems and Windows systems. Support for sparse matrices is provided by an S3-style class "simple_triplet_matrix" from package slam and by objects from the Matrix package class hierarchy.
    # LazyLoad: yes
    # Depends: R (>= 2.6.0), slam
    # Enhances: Matrix
    # License: LGPL (>= 2.0)
    # URL: http://R-Forge.R-project.org/projects/rcplex

    # Value of Rcplex():
    # 
    #      Returns a list with the following components, or, if 'n > 1' a
    #      list of length equal to the number of optimal solutions containing
    #      the following components for each solution: 
    # 
    #     xopt: Values of problem variables at optimum.
    # 
    #     obj : Value of objective function at optimum.
    # 
    #   status: Solution status. See CPLEX documentation for meaning of
    #           status codes.
    # 
    #    extra: List with extra information about solution with components
    # 
    #           slack: Values of slack variables for inequality constraints.
    # 
    #           nodecnt: (IF MIP PROBLEM) Number of nodes in the search tree
    #                evaluated
    # 
    #           lambda: (IF NOT MIP PROBLEM) Values of dual variables at
    #                optimum

    # Optimize:
    optim <- Rcplex::Rcplex(cvec = dvec,
                            Amat = Amat,
                            bvec = bvec,
                            Qmat = Dmat,
                            sense = dir,
                            control = list(trace = 0, round = 1)
                            )

    # Set Tiny Weights to Zero:
    weights = .checkWeights(optim$xopt)
    attr(weights, "invest") = sum(weights)

    ## Simple db for "ok" status results:
    ok_status_db <-
        c("CPX_STAT_OPTIMAL" = 1L,      # (Simplex or barrier): optimal
                                        # solution is available
          "CPXMIP_OPTIMAL" = 101L,      # (MIP): optimal integer solution
                                        # has been found
          "CPXMIP_OPTIMAL_TOL" = 102L,  # (MIP): Optimal soluton with
                                        # the tolerance defined by epgap
                                        # or epagap has been found
          "CPXMIP_POPULATESOL_LIM" = 128L, # (MIP-MultSols): The limit on
                                        # mixed integer solutions
                                        # generated by populate has been
                                        # reached
          "CPXMIP_OPTIMAL_POPULATED" = 129L, # (MIP-MultSols): Populate
                                        # has completed the enumeration of
                                        # all solutions it could enumerate
          "CPXMIP_OPTIMAL_POPULATED_TOL" = 130L # (MIP-MultSols): similar
                                        # to 129L but additionally
                                        # objective value fits the
                                        # tolerance specified by paramaters
          )
    status <- ifelse(optim$status %in% ok_status_db, 0, status)
    
    # Compose Output List:
    ans = list(
        type = "MV",
        solver = "solveRcplex",
        optim = optim,
        weights = weights,
        targetReturn = bvec[1],
        targetRisk = sqrt(weights %*% Dmat %*% weights)[[1,1]],
        objective = sqrt(weights %*% Dmat %*% weights)[[1,1]],
        status = status,
        message = NA)

    # Return Value:
    ans
}


################################################################################


.rcplexControl <-
    function()
{
    # Description:
    #   Returns default cplex control settings

    # Arguments:
    #   none

    # FUNCTION:

    # This algorithm comes with no control parameter list

    NA
}


################################################################################

