
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:               SOLVER:
#  .garchRnlminb           R coded solver nlmin
#  .garchRlbgfsb           R coded solver optim using method lbgfsb
#  .garchRnm               R coded solver nm as hybrid addon
#  .garchFsqp              Fortran coded solver sqp
#  .garchRdonlp2           R coded solver donlp2
#  .garchFmnfb             Fortran coded solver mnfb
################################################################################


.garchRnlminb <-
    function(.params, .series, .garchLLH, trace)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Port3 nlminb R-code Solver

    # Arguments:

    # FUNCTION:

    # Port3 nlminb R-code Solver:
    if(trace) cat("\n\n\nR coded nlminb Solver: \n\n\n")

    # Scale Function and Parameters:
    INDEX = .params$index
    parscale = rep(1, length = length(INDEX))
    names(parscale) = names(.params$params[INDEX])
    parscale["omega"] = var(.series$x)^(.params$delta/2)
    parscale["mu"] = abs(mean(.series$x))

    # Control:
    TOL1 = .params$control$tol1

    # Fit Parameters - par | objective:
    fit = nlminb(
        start = .params$params[INDEX],
        objective = .garchLLH,
        lower = .params$U[INDEX],
        upper = .params$V[INDEX],
        scale = 1/parscale,
        control = list(
            eval.max = 2000,
            iter.max = 1500,
            rel.tol = 1.0e-14 * TOL1,
            x.tol = 1.0e-14 * TOL1),
        trace = trace)
    fit$value = fit.llh = fit$objective
    names(fit$par) = names(.params$params[INDEX])

    # Result:
    fit$coef = fit$par
    fit$llh = fit$objective

    # Return Value:
    fit
}


# ------------------------------------------------------------------------------


.garchRlbfgsb <-
    function(.params, .series, .garchLLH, trace)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   optim[L-BFGS-B] Solver

    # Arguments:

    # FUNCTION:

    # optim[L-BFGS-B] Solver:
    if(trace) cat("\n\n\nR coded optim[L-BFGS-B] Solver: \n\n")

    # Scale Function and Parameters:
    INDEX = .params$index
    parscale = rep(1, length = length(INDEX))
    names(parscale) = names(.params$params[INDEX])
    parscale["omega"] = var(.series$x)^((.params$params["delta"])/2)

    # Control:
    TOL1 = .params$control$tol1

    # Fit Parameters - par, value:
    fit = optim(
        par = .params$params[INDEX],
        fn = .garchLLH,
        lower = .params$U[INDEX],
        upper = .params$V[INDEX],
        method = "L-BFGS-B",
        control = list(
            parscale = parscale,
            lmm = 20,
            pgtol = 1.0e-11 * TOL1,
            factr = 1.0 * TOL1),
        trace = trace)
    names(fit$par) = names(.params$params[INDEX])

    print(fit$hessian)

    # Result:
    fit$coef = fit$par
    fit$llh = fit$value

    # Return Value:
    fit
}


# ------------------------------------------------------------------------------


.garchRnm <-
    function(.params, .series, .garchLLH, trace)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Nelder-Mead as Hybrid Solver

    # Arguments:

    # FUNCTION:

    # Nelder-Mead as Hybrid Solver:
    if(trace) cat("\n\n\nR coded Nelder-Mead Hybrid Solver: \n\n")

    # Scale Function and Parameters:
    INDEX = .params$index
    fnscale = abs(.params$llh)
    parscale = abs(.params$params[INDEX])

    # Control:
    TOL2 = .params$control$tol2

    # Fit Parameters:
    fit = optim(
        par = .params$params[INDEX],
        fn = .garchLLH,
        method = "Nelder-Mead",
        control = list(
            ndeps = rep(1e-14*TOL2, length = length(INDEX)),
            maxit = 10000,
            reltol = 1.0e-11 * TOL2,
            fnscale = fnscale,
            parscale = c(1, abs((.params$params[INDEX])[-1]))),
        hessian = TRUE,
        trace = trace)
    names(fit$par) = names(.params$params[INDEX])

    # Result:
    fit$coef = fit$par
    fit$llh = fit$value

    # Return Value:
    fit
}


# ------------------------------------------------------------------------------


.garchFsqp <-
    function(.params, .series, .garchLLH, trace)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   SQP Full Fortran Solver

    # Arguments:

    # FUNCTION:

    # SQP Full Fortran Solver:
    if(trace) cat("\n\n\nFortran coded SQP Solver: \n\n")

    # Control:
    INDEX = .params$index
    IPAR = c(
        MIT = .params$control$MIT,
                    # maximum number of iterations (200)
        MFV = .params$control$MFV,
                    # maximum number of function evaluations (500)
        MET = .params$control$MET,
                    # specifies scaling strategy:
                    #  MET=1 - no scaling
                    #  MET=2 - preliminary scaling in 1st iteration (default)
                    #  MET=3 - controlled scaling
                    #  MET=4 - interval scaling
                    #  MET=5 - permanent scaling in all iterations
        MEC = .params$control$MEC,
                    # correction for negative curvature:
                    #  MEC=1 - no correction
                    #  MEC=2 - Powell correction (default)
        MER = .params$control$MER,
                    # restarts after unsuccessful variable metric updates:
                    #  MER=0 - no restarts
                    #  MER=1 - standard restart
        MES = .params$control$MES)
                    # interpolation method selection in a line search:
                    #  MES=1 - bisection
                    #  MES=2 - two point quadratic interpolation
                    #  MES=3 - three point quadratic interpolation
                    #  MES=4 - three point cubic interpolation (default)
    RPAR = c(
        XMAX = .params$control$XMAX,
        TOLX = .params$control$TOLX,
        TOLC = .params$control$TOLC,
        TOLG = .params$control$TOLG,
        TOLD = .params$control$TOLD,
        TOLS = .params$control$TOLS,
        RPF  = .params$control$RPF)
    MDIST = c(norm = 10, snorm = 11, std = 20, sstd = 21, ged = 30,
        sged = 31)[.params$cond.dist]
    if(.params$control$fscale) NORM = length(.series$x) else NORM = 1
    REC = 1
    if(.series$init.rec == "uev") REC = 2
    MYPAR = c(
        REC   = REC,                                  # How to initialize
        LEV   = as.integer(.params$leverage),         # Include Leverage 0|1
        MEAN  = as.integer(.params$includes["mu"]),   # Include Mean 0|1
        DELTA = as.integer(.params$includes["delta"]),# Include Delta 0|1
        SKEW  = as.integer(.params$includes["skew"]), # Include Skew 0|1
        SHAPE = as.integer(.params$includes["shape"]),# Include Shape 0|1
        ORDER = .series$order,                        # Order of ARMA-GARCH
        NORM  = as.integer(NORM))

    # Now Estimate Parameters:
    MAX = max(.series$order)
    NF = length(INDEX)
    N = length(.series$x)
    DPARM = c(.params$delta, .params$skew, .params$shape)
    if(IPAR[1] == 0) sink("@sink@")
    fit <- .Fortran("garchfit",
                    N = as.integer(N),
                    Y = as.double(.series$x),
                    Z = as.double(rep(2, times = N)),
                    H = as.double(rep(0, times = N)),
                    NF = as.integer(NF),
                    X = as.double(.params$params[INDEX]),
                    XL = as.double(.params$U[INDEX]),
                    XU = as.double(.params$V[INDEX]),
                    DPARM = as.double(DPARM),
                    MDIST = as.integer(MDIST),
                    IPAR = as.integer(IPAR),
                    RPAR = as.double(RPAR),
                    IPRNT = as.integer(trace),
                    MYPAR = as.integer(MYPAR),
                    F = as.double(FALSE),
                    PACKAGE = "fGarch")
    if(IPAR[1] == 0) {
        sink()
        unlink("@sink@")
    }

    # Result:
    if(trace) {
        cat("\nControl Parameter:\n")
        print(IPAR)
        print(RPAR)
    }
    fit$par = fit[[6]]
    fit$value = fit[[15]]

    # Update .series
    names(fit$par) = names(.params$params[INDEX])
    fit$coef = fit$par
    updateLLH = .garchLLH(fit$par, trace)

    # Return Value:
    fit
}


# ------------------------------------------------------------------------------


.garchRdonlp2 <-
    function(.params, .series, .garchLLH, trace)
{
    # A function implemented by Diethelm Wuertz

    # Description:

    # Arguments:

    # FUNCTION:

    # donlp2 R-code Solver:
    if(!require(Rdonlp2)) stop("Package Rdonlp2 cannot be loaded")
    if(trace) cat("\n\n\nNow DONLP2 \n\n\n")

    # Scale Function and Parameters:
    INDEX = .params$index
    parscale = rep(1, length = length(INDEX))
    names(parscale) = names(.params$params[INDEX])
    parscale["omega"] = var(.series$x)^(.params$delta/2)
    parscale["mu"] = abs(mean(.series$x))

    fit = donlp2(
        par = .params$params[INDEX],
        fn = .garchLLH,
        par.lower = .params$U[INDEX],
        par.upper = .params$V[INDEX])
    fit$value = fit$fx
    names(fit$par) = names(.params$params[INDEX])
    fit$coef = fit$par

    # Return Value:
    fit
}

# ------------------------------------------------------------------------------


.garchFmnfb <-
    function(.params, .series, .garchLLH, trace)
{
    # A function implemented by Diethelm Wuertz

    # Description:

    # Arguments:

    # FUNCTION:

    # nlminb Full Fortran Solver:
    if(trace) cat(" NLMINB FULL FORTRAN Algorithm\n\n")

    # The following comes from SQP ...
    #   should be adapted:
    INDEX = .params$index
    IPAR = c(
        IPRNT = as.integer(trace),
        MIT = .params$control$MIT,
        MFV = .params$control$MFV,
        MET = .params$control$MET,
        MEC = .params$control$MEC,
        MER = .params$control$MER,
        MES = .params$control$MES)
    RPAR = c(
        XMAX = .params$control$XMAX,
        TOLX = .params$control$TOLX,
        TOLC = .params$control$TOLC,
        TOLG = .params$control$TOLG,
        TOLD = .params$control$TOLD,
        TOLS = .params$control$TOLS,
        RPF  = .params$control$RPF)
    MDIST = c(norm = 10, snorm = 11, std = 20, sstd = 21, ged = 30,
        sged = 31)[.params$cond.dist]
    if(.params$control$fscale) NORM = length(.series$x) else NORM = 1
    REC = 1
    if(.series$init.rec == "uev") REC = 2
    MYPAR = c(
        REC   = REC,                                  # How to initialize
        LEV   = as.integer(.params$leverage),         # Include Leverage 0|1
        MEAN  = as.integer(.params$includes["mu"]),   # Include Mean 0|1
        DELTA = as.integer(.params$includes["delta"]),# Include Delta 0|1
        SKEW  = as.integer(.params$includes["skew"]), # Include Skew 0|1
        SHAPE = as.integer(.params$includes["shape"]),# Include Shape 0|1
        ORDER = .series$order,                        # Order of ARMA-GARCH
        NORM  = as.integer(NORM))

    # Now Estimate Parameters:
    MAX = max(.series$order)
    NF = length(INDEX)
    N = length(.series$x)
    DPARM = c(.params$delta, .params$skew, .params$shape)
    if(IPAR[1] == 0) sink("@sink@")
    fit = .Fortran("garchfit2",
        NN = as.integer(N),
        YY = as.double(.series$x),
        ZZ = as.double(rep(2, times = N)),
        HH = as.double(rep(0, times = N)),
        NF = as.integer(NF),
        X = as.double(.params$params[INDEX]),
        XL = as.double(.params$U[INDEX]),
        XU = as.double(.params$V[INDEX]),
        DPARM = as.double(DPARM),
        MDIST = as.integer(MDIST),
        IPAR = as.integer(IPAR),
        RPAR = as.double(RPAR),
        MYPAR = as.integer(MYPAR),
        F = as.double(0),
        PACKAGE = "fGarch")
    if(IPAR[1] == 0) {
        sink()
        unlink("@sink@")
    }

    # Result:
    fit$par = fit[[6]]
    fit$llh = fit$value = fit[[14]]

    # Update .series
    names(fit$par) = names(.params$params[INDEX])
    fit$coef = fit$par
    updateLLH = .garchLLH(fit$par, trace)

    # Return Value:
    fit
}

################################################################################

