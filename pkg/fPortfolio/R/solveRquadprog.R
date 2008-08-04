
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                    DESCRIPTION:
#  solveRquadprog               Calls Goldfarb and Idnani's QP solver
# FUNCTION:                    DESCRIPTION:
#  rquadprog                    Interface to quadprog solver
################################################################################


solveRquadprog <-
    function(data, spec, constraints)
{
    # A function implemented by Rmetrics

    # Description:
    #   Calls Goldfarb and Idnani's QP solver for Mean-Variance Problems

    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints

    # Note:
    #   This function is thought to minimize MV risk for a fixed return
    #   and additional box and group constraints.
    #   The function can in principle handle any case of linear constraints.

    # Details:
    #   The fortran function "qpgen2" is builtin from the contributed
    #   R package quadprog:
    #   quadprog:   Functions to solve Quadratic Programming Problems.
    #   Version:    1.4-11
    #   Date:       2007-07-12
    #   Author:     S original by Berwin A. Turlach R port by A. Weingessel
    #   Maintainer: Andreas Weingessel
    #   License:    GPL-2

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

    # Example:
    #   Data = 100*as.timeSeries(data(LPP2005REC))[,1:6]
    #   tangencyPortfolio(Data)
    #   minvariancePortfolio(Data)
    
    # FUNCTION:
    
    # Load quadprog:
    if (!require(quadprog)) {
        cat("\n\nquadprog Package missing")
        cat("\nPlease install quadprog from CRAN Server\n")
    }    

    # Transform Data and Constraints:
    data = portfolioData(data, spec)
    if (class(constraints) == "fPFOLIOCON")
        constraints = constraints@stringConstraints 

    # Get Specifications:
    mu = getMu(data)
    Sigma = getSigma(data)
    nAssets = getNAssets(data)
    targetReturn = getTargetReturn(spec)
    stopifnot(is.numeric(targetReturn))
    trace = getTrace(spec)

    # Optimize Portfolio:
    if(trace) cat("\nPortfolio Optimiziation:\n Using Rquadprog ...\n\n")
    if (nAssets == 2) {

        ### # Two Assets Portfolio:
        ### # YC: test might failed because of numerical errors, hence 'round'
        ### stopifnot(round(targetReturn, 6) >= round(min(mu), 6))
        ### stopifnot(round(targetReturn, 6) <= round(max(mu), 6))

        # Solve:
        stopifnot(targetReturn >= min(mu))
        stopifnot(targetReturn <= max(mu))
        weights = (targetReturn-mu[2]) / (mu[1]-mu[2])
        weights = c(weights, 1 - weights)
        
        # Output List:
        ans = list(
            solver = "twoAssetsMV",
            optim = optim,
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = NA,
            objective = sqrt(weights %*% Sigma %*% weights),
            status = 0,
            message = NA)
    } else {
        # Add Rquadprog conform constraints [portfolioConstraints.R]
        args = .setRquadprogConstraints(data, spec, constraints)
        # Solve:
        optim = rquadprog(args$Dmat, args$dvec, args$Amat, args$bvec, args$meq)
        weights = .checkWeights(optim$sol)

        # Output List:
        ans = list(
            solver = "quadprog",
            optim = optim,
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = NA,
            objective = sqrt(weights %*% Sigma %*% weights),
            status = optim$ierr,
            message = NA)
    }

    # Return Value:
    ans
}


################################################################################
# FUNCTION:                    DESCRIPTION:
#  rquadprog                    Interface to quadprog solver
################################################################################


# Package: quadprog
# Version: <CRAN>
# Date: <CRAN>
# Title: Functions to solve Quadratic Programming Problems.
# Author: S original by Berwin A. Turlach <berwin.turlach@anu.edu.au>
#   R port by Andreas Weingessel <Andreas.Weingessel@ci.tuwien.ac.at>
# Maintainer: Andreas Weingessel <Andreas.Weingessel@ci.tuwien.ac.at>
# Description: This package contains routines and documentation for
#   solving quadratic programming problems.
# License: GPL-2


# Requires to load contributed R package quadprog


rquadprog <-
    function(Dmat, dvec, Amat, bvec, meq)
{
    # Running ...
    # print("Running rquadprog ...")
    
    # Settings:
    n = nrow(Dmat)
    q = ncol(Amat)
    r = min(n, q)
    work = rep(0, 2 * n + r * (r + 5)/2 + 2 * q + 1)

    # Optimize:
    ans = .Fortran("qpgen2",
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

    # Return Value:
    ans
}


################################################################################


# rquadprogControl


################################################################################

