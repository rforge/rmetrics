
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
    # implemented by Stefan Theussl <stefan.theussl AT wu.ac.at>
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
       if(getType(spec) == "MV"){
         # Solve two Assets Portfolio Analytically:
         ans = .mvSolveTwoAssets(Data, spec, constraints)
         # ... this is only  for 'unlimited' LongOnly constraints,
         # box and group constraints are discarded here.
       }
       else {
         # Solve two Assets Portfolio Analytically:
         # ... this is only thhought for 'unlimited' LongOnly Constraints
         # box and group constraints are discarded here.
         ans = .cvarSolveTwoAssets(Data, spec, constraints)
       }
    } else {

        # Compile Arguments for Solver:
        args = .rcplexArguments(Data, spec, constraints)

        # Solve Multiassets Portfolio:
        ans = .rcplex(
            Dmat = args$Dmat,
            dvec = args$dvec,
            Amat = args$Amat,
            bvec = args$bvec,
            dir = args$dir,
            lb = args$lb,
            ub = args$ub,
            types = args$types,
            objsense = args$objsense,
            nScenarios = args$nScenarios,
            nAssets = args$nAssets,
            targetReturn = args$targetReturn,
            Alpha = args$Alpha,
            spec = spec
          )

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
    # Almost as usual
    Data = portfolioData(data, spec)
    data <- getSeries(Data)
    nAssets = getNAssets(Data)

    # What variable Types, All Continuous:
    types = NULL
    
    # Additionally for LP problems
    nScenarios = nrow(getSeries(Data))
    targetReturn = getTargetReturn(spec)
    Alpha = getAlpha(spec)

    # get optimization type
    optType <- getType(spec)

    if(optType == "MV"){

      # for QP problems we also need the covariance matrix
      Sigma = getSigma(Data)
    
      # Set up A_mat of Constraints:
      eqsumW = eqsumWConstraints(Data, spec, constraints)
      minsumW = minsumWConstraints(Data, spec, constraints)
      maxsumW = maxsumWConstraints(Data, spec, constraints)
      mat = rbind(eqsumW[, -1], diag(nAssets), -diag(nAssets))
      if(!is.null(minsumW)) mat = rbind(mat, minsumW[, -1])
      if(!is.null(maxsumW)) mat = rbind(mat, -maxsumW[, -1])

      # Set up Vector mat >= rhs of Constraints:
      minW = minWConstraints(Data, spec, constraints)
      maxW = maxWConstraints(Data, spec, constraints)
      rhs = c(eqsumW[, 1], minW, -maxW)
      if(!is.null(minsumW)) rhs = c(rhs, minsumW[, 1])
      if(!is.null(maxsumW)) rhs = c(rhs, -maxsumW[, 1])

      # Part (meq=1) or Full (meq=2) Investment, the Default ?
      meq = nrow(eqsumW)

      # Directions:
      dir = c(
        rep("==", times = meq),
        rep(">=", times = length(rhs) - meq))

      # additional in objective function set linear objectives to 0
      obj <- rep(0, nAssets)

      # Should I minimize or maximize ?
      objsense = "min"

      # use standard bounds
      bounds <- NULL

    } else {
      # CVaR
      # FIXME: if(optType == "xxxx") { ?

      # Objective Function:
      objNames = c("VaR", paste("e", 1:nScenarios, sep = ""), colnames(data))
      obj = c(1, -rep(1/(Alpha*nScenarios), nScenarios), rep(0, nAssets))
      names(obj) = objNames
  
      # The A_equal Equation Constraints: A_eq %*% x == a_eq
      eqsumW = eqsumWConstraints(Data, spec, constraints)
      Aeq = cbind(matrix(0, ncol = 1+nScenarios, nrow = nrow(eqsumW)), eqsumW[, -1])
      aeq = eqsumW[, 1]
      deq = rep("==", nrow(eqsumW))
  
      # The VaR Equation Constraints:  (1 + diag + Returns) %*% (VaR,es,W)  >= 0
      Avar = cbind(
          matrix(rep(-1, nScenarios), ncol = 1),
          diag(nScenarios),
          getDataPart(getSeries(Data)) )
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
      minsumW = minsumWConstraints(Data, spec, constraints)
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
      maxsumW = maxsumWConstraints(Data, spec, constraints)
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
      minW = minWConstraints(Data, spec, constraints)
      maxW = maxWConstraints(Data, spec, constraints)
      nInd = 1:(1+nScenarios+nAssets)
      bounds = list(
          lower = list(ind = nInd, val = c(rep(-Inf, 1+nScenarios), minW)),
          upper = list(ind = nInd, val = c(rep( Inf, 1+nScenarios), maxW)) )
    
      # Should I minimize or maximize ?
      objsense = "max"

      # Further arguments
      Sigma <- NULL
    }

    .as_Rcplex_sense <- function(x) {
      TABLE <- c("L", "L", "G", "G", "E")
      names(TABLE) <- c("<", "<=", ">", ">=", "==")
      TABLE[x]
    }
    
    # Return Value:
    ## FIXME: bound length is equal to the number of declared objective variables
    list(
         dvec = obj, Amat = mat, bvec = rhs, Dmat = Sigma,
         lb = Rglpk:::as.glp_bounds.list(bounds, length(obj))$lower,
         ub = Rglpk:::as.glp_bounds.list(bounds, length(obj))$upper,
         dir = .as_Rcplex_sense(dir), objsense = objsense, types = types,
         nScenarios = nScenarios, nAssets = nAssets,
         targetReturn = targetReturn, Alpha = Alpha)
}


################################################################################

.rcplex <-
    function(Dmat, dvec, Amat, bvec, dir, lb, ub, types, objsense, nScenarios, nAssets, targetReturn, Alpha, spec)
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
                            objsense = objsense,
                            vtype = types,
                            lb = lb,
                            ub = ub,
                            control = list(trace = 0, round = 1)
                            )

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

    ## return depends on what we want to optimize (for the time being this is an 'if' clause)
    optType <- getType(spec)

    if(optType == "MV"){

      # Set Tiny Weights to Zero:
      weights = .checkWeights(optim$xopt)
      attr(weights, "invest") = sum(weights)

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
      
    } else {
     # Result:
     # Extract Weights:
      weights = .checkWeights(optim$xopt[-(1:(nScenarios+1))])
      attr(weights, "invest") = sum(weights)

      ans <- list(
        solver = "solveRcplex",
        optim = optim,
        weights = weights,
        targetReturn = targetReturn,
        targetRisk = -optim$obj,
        objective = -optim$obj,
        status = status,
        message = NA)

    }
    
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

