
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
#  .solveRquadprog              Calls Goldfarb and Idnani's QP solver
#  .rquadprog

################################################################################


################################################################################
# Original Version:


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

    # FUNCTION:

    # Transform Data and Constraints:
    data = portfolioData(data, spec)
    if (class(constraints) == "fPFOLIOCON")
        constraints = constraints@stringConstraints

    # Trace:
    trace = getTrace(spec)
    if(trace) cat("\nPortfolio Optimiziation:\n Using Rquadprog ...\n\n")

    # Get Specifications:
    mu = getMu(data)
    Sigma = getSigma(data)

    # Number of Assets:
    nAssets = getNAssets(data)

    # Extracting data from Specification:
    targetReturn = getTargetReturn(spec)
    stopifnot(is.numeric(targetReturn))

    # Optimize Portfolio:
    if (nAssets == 2) {

###         # Two Assets Portfolio:
###         # YC: test might failed because of numerical errors, hence 'round'
###         stopifnot(round(targetReturn, 6) >= round(min(mu), 6))
###         stopifnot(round(targetReturn, 6) <= round(max(mu), 6))

        stopifnot(targetReturn >= min(mu))
        stopifnot(targetReturn <= max(mu))
        weights = (targetReturn-mu[2]) / (mu[1]-mu[2])
        weights = c(weights, 1- weights)
        optim = list(
            # Further list elements ...
            value = NA,
            unconstrainted.solution = c(NA, NA),
            iterations = c(NA, NA),
            iact = c(NA, NA))
        ans = list(
            solver = "twoAssetsMV",
            optim = optim,
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = NA,
            objective = sqrt(weights %*% Sigma %*% weights),
            status = 0)
    } else {
        # Setting the constraints matrix and vector:
        tmpConstraints = .setConstraints(data, spec, constraints)
        Dmat = Sigma
        dvec = rep(0, nAssets)
        A = tmpConstraints[, -(nAssets+1)]
        Amat = t(A)
        b0 = tmpConstraints[, (nAssets+1)]
        bvec = t(b0)
        meq = 2

        res1 = .rquadprog(Dmat, dvec, Amat, bvec, meq)
        if (FALSE) {
            # .rquadprog:
            n = nrow(Dmat)
            q = ncol(Amat)
            r = min(n, q)
            work = rep(0, 2 * n + r * (r + 5)/2 + 2 * q + 1)
            res1 = .Fortran("qpgen2",
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
            # ... end of .rquadprog
        }

        # Handle when failed ...
        weights = res1$sol
        for(i in 1:nAssets) {
            if(abs(weights[i]) < sqrt(.Machine$double.eps)) weights[i] = 0
        }

        optim = list(
            # Further list elements ...
            value = res1$crval,
            unconstrainted.solution = res1$dvec,
            iterations = res1$iter,
            iact = res1$iact[1:res1$nact])

        #print(res1$crval)
        #print(weights %*% Sigma %*% weights)
        #print(sqrt(weights %*% Sigma %*% weights))

        # Prepare Output List:
        ans = list(
            solver = "quadprog",
            optim = optim,
            weights = weights,
            targetReturn = targetReturn,
            targetRisk = NA,
            objective = sqrt(weights %*% Sigma %*% weights),
            status = res1$ierr)
    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.rquadprog <-
function(Dmat, dvec, Amat, bvec, meq)
{
    # Settings:
    n = nrow(Dmat)
    q = ncol(Amat)
    r = min(n, q)
    work = rep(0, 2 * n + r * (r + 5)/2 + 2 * q + 1)

    # Optimize:
    res1 = .Fortran("qpgen2",
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
    res1
}


################################################################################

