
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
#  solveRglpk                   Portfolio interface to solver Rglpk
#  .rglpkArguments              Returns arguments for solver
#    .cvarRglpkArguments         Returns CVaR arguments for solver
#    .madRglpkArguments          Returns MAD arguments for solver
#  .rglpk                       Wrapper to solver function
#  .rglpkControl                Returns default controls for solver
################################################################################


# IMPORTANT NOTE:
#   MAD works, but frontier plot has to be adjusted, since at the moment it
#   can not yet plot against MAD!


solveRglpk <-
    function(data, spec, constraints)
{
    # Description:
    #   Portfolio interface to solver Rglpk

    # Example:
    #
    #   solveRglpk(.lppData, .cvarSpec, "LongOnly")[-3]
    #   solveRglpk(.lppData, .cvarSpec, .BoxGroups)[-3]
    #   portfolioTest("CVaR", "minRisk", "solveRglpk", "LongOnly")
    #   portfolioTest("CVaR", "minRisk", "solveRglpk", "BoxGroup")
    #
    #   solveRglpk(.lppData, .madSpec, "LongOnly")[-3]
    #   solveRglpk(.lppData, .madSpec, .BoxGroups)[-3]
    #   portfolioTest("MAD", "minRisk", "solveRglpk", "LongOnly")
    #   portfolioTest("MAD", "minRisk", "solveRglpk", "BoxGroup")

    # FUNCTION:

    # Settings:
    Data = portfolioData(data, spec)
    data <- getSeries(Data)
    nAssets = getNAssets(Data)
    type = getType(spec)

    # Solve:
    if(nAssets == 2) {

        # Solve two Assets Portfolio Analytically:
        # ... this is only thhought for 'unlimited' LongOnly Constraints
        # box and group constraints are discarded here.

        fun = match.fun(paste(".", tolower(type), "SolveTwoAssets", sep = ""))
        ans = fun(Data, spec, constraints)

    } else {

        # Compile Arguments for Solver:
        args = .rglpkArguments(Data, spec, constraints)

        # Solve Multiassets Portfolio:
        ans = .rglpk(
            obj = args$obj,
            mat = args$mat,
            dir = args$dir,
            rhs = args$rhs,
            types = args$types,
            max = args$max,
            bounds = args$bounds,
            verbose = args$verbose,
            nScenarios = args$nScenarios,
            nAssets = args$nAssets,
            targetReturn = args$targetReturn,
            Alpha = args$Alpha,
            Type = args$Type)

    }

    # Return Value:
    ans
}


################################################################################


.rglpkArguments <-
    function(data, spec, constraints)
{
    # Example:
    #   .rglpkArguments(.lppData[1:8,1:3], .cvarSpec, "LongOnly")
    #   .rglpkArguments(.lppData[1:8,1:3], .madSpec, "LongOnly")

    # Settings:
    type = getType(spec)

    # Execute appropriate .rfooArguments function:
    fun = match.fun(paste(".", tolower(type), "RglpkArguments", sep = ""))

    # Return Value:
    fun(data, spec, constraints)
}


# ------------------------------------------------------------------------------


.cvarRglpkArguments <-
    function(data, spec, constraints)
{
    # Description:
    #   Returns glpk conform CVaR arguments for the solver

    # Details:
    #       max/min:                   obj %*% x
    #       subject to:
    #                            mat %*% x ?=  rhs
    #                                  dir  =  "?="
    #                               upper/lower bounds
    #
    #   Rglpk_solve_LP(obj, mat, dir, rhs, types = NULL, max = FALSE,
    #       bounds = NULL, verbose = FALSE)

    # Example:
    #   .cvarRglpkArguments(.lppData[1:8,1:3], .cvarSpec, "LongOnly")
    #   .cvarRglpkArguments(.lppData[1:8,1:3], .cvarSpec, .BoxGroups)

    # FUNCTION:

    # Settings:
    Data = portfolioData(data, spec)
    data <- getSeries(Data)
    nAssets = getNAssets(Data)
    nScenarios = nrow(getSeries(Data))
    targetReturn = getTargetReturn(spec)
    Alpha = getAlpha(spec)
    Type = getType(spec)

    # Objective Function:
    objNames = c("VaR", paste("e", 1:nScenarios, sep = ""), colnames(data))
    obj = c(1, -rep(1/(Alpha*nScenarios), nScenarios), rep(0, nAssets))
    names(obj) = objNames

    # The A_equal Equation Constraints: A_eq %*% x == a_eq
    eqsumW = eqsumWConstraints(Data, spec, constraints)
    Aeq = cbind(matrix(0, ncol = 1+nScenarios, nrow = nrow(eqsumW)), eqsumW[, -1])
    aeq = eqsumW[, 1]
    deq = rep("==", nrow(eqsumW))

    # The VaR Equation Constraints:
    #   (1 + diag + Returns) %*% (VaR,es,W)  >= 0
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
        Aminsum = aminsum = dminsum = NULL
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
        Amaxsum = amaxsum = dmaxsum = NULL
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

    # What variable Types, All Continuous:
    types = NULL

    # Should I minimize or maximize ?
    max = TRUE

    # Return Value:
    list(
        obj = obj, mat = mat, dir = dir, rhs = rhs,
        types = types, max = max, bounds = bounds, verbose = FALSE,
        nScenarios = nScenarios, nAssets = nAssets,
        targetReturn = targetReturn, Alpha = Alpha, Type = Type)
}


# ------------------------------------------------------------------------------


.madRglpkArguments <-
    function(data, spec, constraints)
{
    # Description:
    #   Returns glpk conform MAD arguments for the solver

    # Details:
    #       max/min:                   obj %*% x
    #       subject to:
    #                           mat %*% x  ?=  rhs
    #                                  dir  =  "?="
    #                              upper/lower bounds
    #
    #   Rglpk_solve_LP(obj, mat, dir, rhs, types = NULL, max = FALSE,
    #       bounds = NULL, verbose = FALSE)

    # Example:
    #   .madRglpkArguments(.lppData[1:8,1:3], .madSpec, "LongOnly")
    #   .madRglpkArguments(.lppData[1:8,1:3], .madSpec, .BoxGroups)

    # FUNCTION:

    # Settings:
    Data = portfolioData(data, spec)
    data <- getSeries(Data)
    nAssets = getNAssets(Data)
    Series = getSeries(Data)
    Series = Series - colMeans(Series)
    nScenarios = nrow(Series)
    targetReturn = getTargetReturn(spec)
    Type = getType(spec)

    # Objective Function to be Maximized:
    objNames = c(paste("e", 1:nScenarios, sep = ""), colnames(data))
    obj = c(rep(1/nScenarios, nScenarios), rep(0, nAssets))
    names(obj) = objNames

    # The A_equal Equation Constraints: A_eq %*% x == a_eq
    eqsumW = eqsumWConstraints(Data, spec, constraints)
    Aeq = cbind(matrix(0, ncol = nScenarios, nrow = nrow(eqsumW)), eqsumW[, -1])
    aeq = eqsumW[, 1]
    deq = rep("==", nrow(eqsumW))

    # The negative MAD Equation Constraints:
    #   (-diag + [Returns-mu]) %*% (es,W) <= 0
    Aneg = cbind(-diag(nScenarios), getDataPart(Series) )
    aneg = rep(0, nrow(Aneg))
    dneg = rep("<=", nrow(Aneg))

    # The positive MAD Equation Constraints:
    #   (+diag + [Returns-mu]) %*% (es,W) >= 0
    Apos = cbind( diag(nScenarios), getDataPart(Series) )
    apos = rep(0, nrow(Apos))
    dpos = rep(">=", nrow(Apos))

    # The Konno MAD Equation Constraints:
    #   (-diag - [Returns-mu]) %*% (es,W) <= 0
    Akonno = cbind( -diag(nScenarios), - getDataPart(Series) )
    akonno = rep(0, nrow(Apos))
    dkonno = rep("<=", nrow(Apos))

    # The e_s > = 0 Equation Constraints:
    Aes = cbind(diag(nScenarios), matrix(0, nrow = nScenarios, ncol = nAssets))
    aes = rep(0, nrow(Aes))
    des = rep(">=", nrow(Aes))

    # Group Constraints: A W >= a
    minsumW = minsumWConstraints(Data, spec, constraints)
    if (is.null(minsumW)){
        Aminsum = aminsum = dminsum = NULL
    } else {
        Aminsum = cbind(
            matrix(0, nrow = nrow(minsumW), ncol = nScenarios),
            minsumW[, -1, drop = FALSE] )
        aminsum = minsumW[, 1]
        dminsum  = rep(">=", nrow(minsumW))
    }

    # Group Constraints: A W <= b
    maxsumW = maxsumWConstraints(Data, spec, constraints)
    if (is.null(maxsumW)){
        Amaxsum = amaxsum = dmaxsum = NULL
    } else {
        Amaxsum = cbind(
            matrix(0, nrow = nrow(maxsumW), ncol = nScenarios),
            maxsumW[, -1, drop = FALSE] )
        amaxsum = maxsumW[, 1]
        dmaxsum  = rep("<=", nrow(maxsumW))
    }

    # Putting all together:
    what = "scherer"
    if (what == "konno") {
        mat = rbind(Aeq, Akonno, Aes, Aminsum, Amaxsum)
        rhs =     c(aeq, akonno, aes, aminsum, amaxsum)
        dir =     c(deq, dkonno, des, dminsum, dmaxsum)
    } else if (what == "scherer") {
        mat = rbind(Aeq, Apos, Aneg, Aes, Aminsum, Amaxsum)
        rhs =     c(aeq, apos, aneg, aes, aminsum, amaxsum)
        dir =     c(deq, dpos, dneg, des, dminsum, dmaxsum)
    }

    # Box Constraints: Upper and Lower Bounds as listn required ...
    minW = minWConstraints(Data, spec, constraints)
    maxW = maxWConstraints(Data, spec, constraints)
    nInd = 1:(nScenarios+nAssets)
    bounds = list(
        lower = list(ind = nInd, val = c(rep(  0, nScenarios), minW)),
        upper = list(ind = nInd, val = c(rep(Inf, nScenarios), maxW)) )

    # What variable Types, All Continuous:
    types = NULL

    # Should I minimize or maximize ?
    max = FALSE

    # Return Value:
    list(
        obj = obj, mat = mat, dir = dir, rhs = rhs,
        types = types, max = max, bounds = bounds, verbose = FALSE,
        nScenarios = nScenarios, nAssets = nAssets,
        targetReturn = targetReturn, Alpha = NA, Type = Type)
}


################################################################################


.rglpk <-
    function(obj, mat, dir, rhs, types, max, bounds, verbose,
    nScenarios, nAssets, targetReturn, Alpha, Type)
{
    # Description:
    #   Rglpk Solver

	# FUNCTION:

	# Solve - use Rglpk_solve_LP:
    optim <- Rglpk::Rglpk_solve_LP(
        obj = obj,
        mat = mat,
        dir = dir,
        rhs = rhs,
        types = types,
        max = max,
        bounds = bounds,
        verbose = verbose)

    # Extract Weights:
    weights = .checkWeights(rev(rev(optim$solution)[1:nAssets]))
    attr(weights, "invest") = sum(weights)

    # Result:
    ans <- list(
        type = Type,
        solver = "Rglpk",
        optim = optim,
        weights = weights,
        targetReturn = targetReturn,
        targetRisk = -optim$optimum,
        objective = -optim$optimum,
        status = optim$status[[1]],
        message = "")

    # Return Value:
    ans
}


################################################################################


.rglpkControl <-
    function()
{
    # Description:
    #   Returns default glpk control settings

    # Arguments:
    #   none

    # FUNCTION:

    # This algorithm comes with no control parameter list

    NA
}


################################################################################

