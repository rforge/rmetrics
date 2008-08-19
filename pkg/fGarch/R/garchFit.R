
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
# FUNCTION:               PARAMETER ESTIMATION:
#  garchFit                Fits the parameters of GARCH process
#  .garchArgsParser        Parses formula and data for garchFit
#  .garchOptimizerControl  Sets default values for Garch Optimizer
#  .garchFit               ... old Version, still in use by garchFit()
#  .garchInitSeries        Initializes Series
#  .garchInitParameters    Initializes Parameters
#  .garchSetCondDist       Selects conditional density function
#   .garchDist              Defines conditional density function
#  .garchOptimizeLLH       Opimizes log-likelihood function
#   .garchLLH               Computes log-likelihood function
#  .garchNames              Slot names, @fit slot, parameters and controls
################################################################################


# ------------------------------------------------------------------------------
# To do:


    #   .garchFit() should be integrated into garchFit()
    #   .garchFilter() should be added for filter, internal and testing mode
    #   Still undocumented: algorithm="donlp2, and algorithm = "NLMINB"


# ------------------------------------------------------------------------------
# Globally needed Variables:


setfGarchEnv(.llh = 1e99)
setfGarchEnv(.garchDist = NA)
setfGarchEnv(.params = NA)
setfGarchEnv(.series = NA)
setfGarchEnv(.trace = NA) # to be added for donlp2 which has no "..." argument


# ------------------------------------------------------------------------------
# History:


    # Fast Forward difference approximated Hessian added
    #   ... this is now the default, alternatively can be used the
    #       central forward approximated Hessian, (NYI)

    # .garchHessian has been moved garchHessian.R

    # algorithm "NLMINB" can be used unofficicially ...

    # algorithm "donlp2" can be used unofficicially ...
    #   ... we need to call: require("Rdonlp2")
    #   ... "internal" has to be checked, we don't support it currently
    #       inspect .garchOptimizerControl() used fix coded "fixed"


# ------------------------------------------------------------------------------


garchFit <-
    function(formula, data,
    init.rec = c("mci", "uev"),
    delta = 2, skew = 1, shape = 4,
    cond.dist = c("norm", "snorm", "ged", "sged", "std", "sstd"),
    include.mean = TRUE, include.delta = NULL, include.skew = NULL,
    include.shape = NULL, leverage = NULL,
    trace = TRUE,
    algorithm = c("nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), ## "mnfb", "sqp",
    hessian = "rcd", ## "fcd", "ffd"
    control = list(),
    title = NULL, description = NULL, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Fit parameters to a ARMA-GARCH model

    # Arguments:
    #   formula - ARMA(m,n) + GARCH/APARCH(p,q) mean and variance
    #       specification
    #   data - any univariate time series which can be converted
    #       into a timeSeries using the generic function as.timeSeries
    #   init.rec - names type of initialization of recurrence
    #       mci = mu-current-iteration, or
    #       uev = unconditional-expected-variances
    #   delta - numeric value of the exponent delta
    #   skew - optional skewness parameter
    #   shape - optional shape parameter
    #   cond.dist - name of the conditional distribution
    #   include.mean - should the mean value be estimated ?
    #   include.delta - should the exponent be estimated ?
    #   leverage - should the leverage factors be estimated ?
    #   trace - should the optimization be traced ?
    #   control - list of additional control parameters for solver
    #   title - an optional title string
    #   description - an optional project description string

    # FUNCTION:

    # Match arguments:
    init.rec = match.arg(init.rec)
    cond.dist = match.arg(cond.dist)
    hessian = match.arg(hessian)
    algorithm = match.arg(algorithm)

    # Call:
    CALL = match.call()

    # Parse formula and data for garchFit ...
    #   Note in the new version we are working with timeSeries ...
    Name = capture.output(substitute(data))
    if(is.character(data)) {
        eval(parse(text = paste("data(", data, ")")))
        data = eval(parse(text = data))
    }
    # data <- if (inherits(data, "timeSeries") data else as.timeSeries(data)
    data <- as.data.frame(data)

    # Column Names:
    if (isUnivariate(data)) {
        colnames(data) <- "data"
    } else {
        # Check unique column Names:
        uniqueNames = unique(sort(colnames(data)))
        if (is.null(colnames(data))) {
            stop("Column names of data are missing.")
        }
        if (length(colnames(data)) != length(uniqueNames)) {
            stop("Column names of data are not unique.")
        }
    }

    # Handle if we have no left-hand-side for the formula ...
    #   Note in this case the length of the formula is 2 (else 3):
    if (length(formula) == 3 && isUnivariate(data) ) formula[2] <- NULL
    if (length(formula) == 2) {
        if (isUnivariate(data)) {
            # Missing lhs -- we substitute the data file name as lhs ...
            formula = as.formula(paste("data", paste(formula, collapse = " ")))
        } else {
            stop("Multivariate data inputs require lhs for the formula.")
        }
    }

    args = .garchArgsParser(formula = formula, data = data, trace = FALSE)

    # Fit:
    ans = .garchFit(
        formula.mean = args$formula.mean,
        formula.var = args$formula.var,
        series = args$series,
        init.rec, delta, skew, shape, cond.dist, include.mean,
            include.delta, include.skew, include.shape, leverage,
        trace,
        algorithm,
        hessian,
        control,
        title, description, ...)
    ans@call = CALL
    attr(formula, "data") <- paste("data = ", Name, sep = "")
    ans@formula = formula

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.garchArgsParser <-
    function(formula, data, trace = FALSE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Parses formula and data for garchFit

    # Arguments:
    #   formula - ARMA(m,n) + GARCH/APARCH(p,q) mean and variance
    #       specification
    #   data - time series input as a timeSeries

    # Note:
    #   This function returns the input formula and input data in
    #   proper formats. Two cases are deistinguished

    # FUNCTION:

    # Get Data:
    allVars = unique(sort(all.vars(formula)))
    allVarsTest =  mean(allVars %in% colnames(data))
    if (allVarsTest != 1) {
        print(allVars)
        print(colnames(data))
        stop ("Formula and data units do not match.")
    }
    formula.lhs = as.character(formula)[2]

    # Model frame:
    mf = match.call(expand.dots = FALSE)
    if(trace) {
        cat("\nMatched Function Call:\n ")
        print(mf)
    }
    m = match(c("formula", "data"), names(mf), 0)
    mf = mf[c(1, m)]

    # Model the timeSeries - Have a look on the function .garchModelSeries() ...
    #   here we cant use "model/frame" !
    mf[[1]] = as.name(".garchModelSeries")
    mf$fake = FALSE
    mf$lhs = TRUE
    if(trace) {
        cat("\nModelSeries Call:\n ")
        print(mf)
    }
    x = eval(mf, parent.frame())
    if(trace) print(x)

    # Now extract the modelled series ...
    x = as.vector(x[, 1])
    names(x) = rownames(data)
    if(trace) print(x)

    # Compose Mean and Variance Formula:
    allLabels = attr(terms(formula), "term.labels")
    if(trace) {
        cat("\nAll Term Labels:\n ")
        print(allLabels)
    }
    if(length(allLabels) == 2) {
        formula.mean = as.formula(paste("~", allLabels[1]))
        formula.var = as.formula(paste("~", allLabels[2]))
    } else if(length(allLabels) == 1) {
        formula.mean = as.formula("~ arma(0, 0)")
        formula.var = as.formula(paste("~", allLabels[1]))
    }
    if(trace) {
        cat("\nMean Formula:\n ")
        print(formula.mean)
        cat("\nVariance Formula:\n ")
        print(formula.var)
    }

    # Result:
    ans <- list(formula.mean = formula.mean,
                formula.var = formula.var,
                formula.lhs = formula.lhs,
                series = x)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.garchModelSeries <-
    function (formula, data, fake = FALSE, lhs = FALSE)
{
    # A function implemented by Diethelm Wuertz

    # Note:
    #   ... is the same funtion as Rmetrics' .modelSeries()
    #   ... have also a look on model.frame()

    # FUNCTION:

    if (length(formula) == 2) {
        formula = as.formula(paste("x", formula[1], formula[2],
            collapse = ""))
        stopifnot(!missing(data))
    }
    if (missing(data)) {
        data = eval(parse(text = search()[2]), parent.frame())
    }
    if (is.numeric(data)) {
        data = data.frame(data)
        colnames(data) = all.vars(formula)[1]
        lhs = TRUE
    }
    if (fake) {
        response = as.character(formula)[2]
        Call = as.character(match.call()[[2]][[3]])
        method = Call[1]
        predictors = Call[2]
        formula = as.formula(paste(response, "~", predictors))
    }
    if (lhs) {
        response = as.character(formula)[2]
        formula = as.formula(paste(response, "~", 1))
    }

    x = model.frame(formula, data)

    if (class(data) == "timeSeries")
        x = timeSeries(x)
    if (fake)
        attr(x, "control") <- method

    # Return Value:
    x
}


# ------------------------------------------------------------------------------


.garchOptimizerControl <-
    function(algorithm)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Sets default values for Garch Optimizer

    # Arguments:
    #   none

    # FUNCTION:

    # Generate Control List with Default Settings:
    con <- list(

        # In General:
        fscale = TRUE,
        xscale = FALSE,
        algorithm = algorithm,
        llh = c("filter", "internal", "testing")[1],

        # BFGS - NLMINB Algorithm:
        tol1 = 1,
        tol2 = 1,

        # SQP Algorithm:
        MIT = 2000,    # maximum number of iterations (200)
        MFV = 5000,    # maximum number of function evaluations (500)
        MET = 5,       # specifies scaling strategy:
                       #  MET=1 - no scaling
                       #  MET=2 - preliminary scaling in 1st iteration (default)
                       #  MET=3 - controlled scaling
                       #  MET=4 - interval scaling
                       #  MET=5 - permanent scaling in all iterations
        MEC = 2,       # correction for negative curvature:
                       #  MEC=1 - no correction
                       #  MEC=2 - Powell correction (default)
        MER = 1,       # restarts after unsuccessful variable metric updates:
                       #  MER=0 - no restarts
                       #  MER=1 - standard restart
        MES = 4,       # interpolation method selection in a line search:
                       #  MES=1 - bisection
                       #  MES=2 - two point quadratic interpolation
                       #  MES=3 - three point quadratic interpolation
                       #  MES=4 - three point cubic interpolation (default)
        XMAX = 1.0e3,
        TOLX = 1.0e-10,
        TOLC = 1.0e-6,
        TOLG = 1.0e-6,
        TOLD = 1.0e-6,
        TOLS = 1.0e-4,
        RPF  = 1.0e-2) # 1.0e-4)
    # Return Value:
    con
}


# ------------------------------------------------------------------------------


.garchFit <-
    function(
    formula.mean = ~arma(0, 0), formula.var = ~garch(1, 1),
    series,
    init.rec = c("mci", "uev"),
    delta = 2, skew = 1, shape = 4,
    cond.dist = c("norm", "snorm", "ged", "sged", "std", "sstd"),
    include.mean = TRUE, include.delta = NULL, include.skew = NULL,
        include.shape = NULL, leverage = NULL,
    trace = TRUE,
    algorithm = c("sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"),
    hessian = c("fda", "cda"),
    control = list(),
    title = NULL, description = NULL, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description
    #   Fit parameters to a ARMA-GARCH model

    # Arguments:
    #   formula.mean - ARMA(m,n) mean specification
    #   formula.var - GARCH/APARCH(p,q) variance specification
    #   series - time series
    #   init.rec - names type of initialization of recurrence
    #       mci = mu-current-iteration, or
    #       uev = unconditional-expected-variances
    #   delta - numeric value of the exponent delta
    #   skew - optional skewness parameter
    #   shape - optional shape parameter
    #   cond.dist - name of the conditional distribution
    #   include.mean - should the mean value be estimated ?
    #   include.delta - should the exponent be estimated ?
    #   leverage - should the leverage factors be estimated ?
    #   trace - should the optimization be traced ?
    #   control - list of additional control parameters for solver
    #   title - an optional title string
    #   description - an optional project description string

    # Note:
    #   This is the old version of garchFit, we keep it for backward
    #   compatibility.

    # FUNCTION:

    # Allow only full formula specification:
    fcheck = rev(all.names(formula.mean))[1]
    if (fcheck == "ma") {
        stop("Use full formula: arma(0,q) for ma(q)")
    } else if (fcheck == "ar") {
        stop("Use full formula expression: arma(p,0) for ar(p)")
    }

    # Check for Recursion Initialization:
    if(init.rec[1] != "mci" & algorithm[1] != "sqp") {
        stop("Algorithm only supported for mci Recursion")
    }

    # Start Time:
    .StartFit <- Sys.time()

    # Generate Control List - Define Default Settings:
    con <- .garchOptimizerControl(algorithm)
    con[(namc <- names(control))] <- control

    # Initialize Time Series Information - Save Globally:
    .series <- .garchInitSeries(formula.mean = formula.mean,
        formula.var = formula.var, series = series, scale = sd(series),
        init.rec = init.rec[1], h.start = NULL, llh.start = NULL,
        trace = trace)
    setfGarchEnv(.series = .series)

    # Initialize Model Parameters - Save Globally:
    .params <- .garchInitParameters(formula.mean = formula.mean,
        formula.var = formula.var, delta = delta, skew = skew,
        shape = shape, cond.dist = cond.dist[1],
        include.mean = include.mean, include.delta = include.delta,
        include.skew = include.skew, include.shape = include.shape,
        leverage = leverage, algorithm = algorithm[1], control = con,
        trace = trace)
    setfGarchEnv(.params = .params)

    # Select Conditional Distribution Function:
    setfGarchEnv(.garchDist = .garchSetCondDist(cond.dist[1]))

    # Estimate Model Parameters - Minimize llh, start from big value:
    setfGarchEnv(.llh = 1.0e99)
    .llh <- getfGarchEnv(".llh")
    fit = .garchOptimizeLLH(hessian, trace)
    fit$llh = .llh

    # Add to Fit:
    .series <- getfGarchEnv(".series")
    .params <- getfGarchEnv(".params")
    names(.series$h) <- NULL
    fit$series = .series
    fit$params = .params

    # Retrieve Residuals and Fitted Values:
    residuals = .series$z
    fitted.values = .series$x - residuals
    h.t = .series$h
    if (.params$includes["delta"])
        deltainv = 1/fit$par["delta"]
    else
        deltainv = 1/fit$params$delta
    sigma.t = (.series$h)^deltainv

    # Standard Errors and t-Values:
    fit$cvar = - solve(fit$hessian)
    fit$se.coef = sqrt(diag(fit$cvar))
    fit$tval = fit$coef/fit$se.coef
    fit$matcoef = cbind(fit$coef, fit$se.coef,
        fit$tval, 2*(1-pnorm(abs(fit$tval))))
    dimnames(fit$matcoef) = list(names(fit$tval), c(" Estimate",
        " Std. Error", " t value", "Pr(>|t|)"))

    # Add Title and Description:
    if(is.null(title)) title = "GARCH Modelling"
    if(is.null(description)) description = .description()

    # Total Execution Time:
    Time =  Sys.time() - .StartFit
    if(trace) {
        cat("\nTime to Estimate Parameters:\n ")
        print(Time)
    }

    # Return Value:
    new("fGARCH",
        call = as.call(match.call()),
        formula = as.formula(paste("~", formula.mean, "+", formula.var)),
        method = "Max Log-Likelihood Estimation",
        data = series,
        fit = fit,
        residuals = residuals,
        fitted = fitted.values,
        h.t = h.t,
        sigma.t = as.vector(sigma.t),
        title = as.character(title),
        description = as.character(description)
    )
}


# ------------------------------------------------------------------------------


.garchInitSeries <-
    function(formula.mean, formula.var, series, scale, init.rec,
    h.start, llh.start, trace)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Initialize time series

    # Arguments:
    #   see function garchFit()

    # FUNCTION:

    # Check Mean Formula ARMA - Is it Valid ?
    mm = length(formula.mean)
    if(mm != 2) stop("Mean Formula misspecified")
    end = regexpr("\\(", as.character(formula.mean[mm])) - 1
    model.mean = substr(as.character(formula.mean[mm]), 1, end)
    if(!any( c("ar", "ma", "arma") == model.mean))
        stop("formula.mean must be one of: ar, ma, arma")

    # Check Variance Formula GARCH - Is it Valid ?
    mv = length(formula.var)
    if(mv != 2) stop("Variance Formula misspecified")
    end = regexpr("\\(", as.character(formula.var[mv])) - 1
    model.var = substr(as.character(formula.var[mv]), 1, end)
    if(!any( c("garch", "aparch") == model.var))
        stop("formula.var must be one of: garch, aparch")

    # Determine Mean Order from ARMA Formula:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.mean), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    u = model.order[1]
    v = 0
    if(length(model.order) == 2) v = model.order[2]
    maxuv = max(u, v)
    if(u < 0 | v < 0) stop("*** ARMA orders must be positive.")

    # Determine Variance Order from GARCH Formula:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.var), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    p = model.order[1]
    q = 0
    if(length(model.order) == 2) q = model.order[2]
    if(p+q == 0)
        stop("Misspecified GARCH Model: Both Orders are zero!")
    maxpq = max(p, q)
    if(p < 0 | q < 0) stop("*** GARCH orders must be positive.")

    # Fix Start Position of Series "h" and for Likelihood Calculation:
    max.order = max(maxuv, maxpq)
    if(is.null(h.start)) h.start = max.order + 1
    if(is.null(llh.start)) llh.start = 1

    # Check for Recursion Initialization:
    if(init.rec != "mci" & model.var != "garch") {
        stop("Algorithm only supported for mci Recursion")
    }

    # Trace the Result:
    if(trace) {
        cat("\nSeries Initialization:")
        cat("\n ARMA model:               ", model.mean)
        cat("\n Formula mean:             ", as.character(formula.mean))
        cat("\n GARCH model:              ", model.var)
        cat("\n Formula var:              ", as.character(formula.var))
        cat("\n ARMA Order:               ", u, v)
        cat("\n Max ARMA Order:           ", maxuv)
        cat("\n GARCH Order:              ", p, q)
        cat("\n Max GARCH Order:          ", maxpq)
        cat("\n Maximum Order:            ", max.order)
        cat("\n h.start:                  ", h.start)
        cat("\n llh.start:                ", llh.start)
        cat("\n Length of Series:         ", length(series))
        cat("\n Recursion Init:           ", init.rec)
        cat("\n Series Scale:             ", scale)
        cat("\n\n")
    }

    # Result:
    ans  = list(
        model = c(model.mean, model.var),
        order = c(u = u, v = v, p = p, q = q),
        max.order = max.order,
        z = rep(0, times = length(series)),
        h = rep(var(series), times = length(series)),
        x = series,
        scale = scale,
        init.rec = init.rec,
        h.start = h.start,
        llh.start = llh.start)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.garchInitParameters <-
    function(formula.mean, formula.var, delta, skew, shape, cond.dist,
    include.mean, include.delta, include.skew, include.shape, leverage,
    algorithm, control, trace)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Initialize model parameters

    # Arguments:
    #   see function garchFit()

    # FUNCTION:

    # DEBUG:
    .DEBUG = FALSE

    # global variables
    .series <- getfGarchEnv(".series")

    # Determine Mean Order from ARMA Formula:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.mean), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    u = model.order[1]
    v = 0
    if(length(model.order) == 2) v = model.order[2]

    # Determine Variance Order from GARCH Formula:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.var), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    p = model.order[1]
    if (p == 0) stop("The order p must be > 0 in GARCH/APARCH(p,q)")
    q = 0
    if(length(model.order) == 2) q = model.order[2]

    # Includes:
    model.var = .series$model[2]
    if(is.null(include.delta)) {
        if(model.var == "garch") {
            include.delta = FALSE
        } else {
            include.delta = TRUE
        }
    }
    if(is.null(leverage)) {
        if(model.var == "garch") {
            leverage = FALSE
        } else {
            leverage = TRUE
        }
    }

    # Distributional Includes:
    if(cond.dist == "t") cond.dist = "std"
    skewed.dists = c("snorm", "sged", "sstd")
    if(is.null(include.skew)) {
        if(any(skewed.dists == cond.dist)) {
            include.skew = TRUE
        } else {
            include.skew = FALSE
        }
    }
    shaped.dists = c("ged", "sged", "std", "sstd")
    if(is.null(include.shape)) {
        if(any(shaped.dists == cond.dist)) {
            include.shape = TRUE
        } else {
            include.shape = FALSE
        }
    }

    # Set Names for Parameters:
    Names = c(
        "mu",
        if(u > 0) paste("ar", 1:u, sep = ""),
        if(v > 0) paste("ma", 1:v, sep = ""),
        "omega",
        if(p > 0) paste("alpha", 1:p, sep = ""),
        if(p > 0) paste("gamma", 1:p, sep = ""),
        if(q > 0) paste("beta",  1:q, sep = ""),
        "delta",
        "skew",
        "shape")
    if(.DEBUG) { cat("\nDEBUG - Names: \n"); print(Names) }

    # Initialize Model Parameters to be Estimated:
    fit.mean = arima(.series$x, order = c(u, 0, v),
        include.mean = include.mean)$coef
    alpha.start = 0.1
    beta.start = 0.8
    ## if(include.delta) delta = 1.5
    params = c(
        if(include.mean) fit.mean[length(fit.mean)] else 0,
        if(u > 0) fit.mean[1:u],
        if(v > 0) fit.mean[(u+1):(length(fit.mean)-as.integer(include.mean))],
        var(.series$x, na.rm = TRUE)*(1-alpha.start-beta.start),
        if(p > 0) rep(alpha.start/p, times = p),
        if(p > 0) rep(0.1, times = p),
        if(q > 0) rep(beta.start/q, times = q),
        delta,
        skew,
        shape)
    names(params) = Names
    if(.DEBUG) { cat("\nDEBUG - params: \n"); print(params) }

    # Set Lower Limits of Parameters to be Estimated:
    TINY = 1.0e-8
    U = c(
        -10*abs(mean(.series$x)),
        if(u > 0) rep(-1+TINY, times = u),
        if(v > 0) rep(-1+TINY, times = v),
        1.0e-6*var(.series$x),
        if(p > 0) rep( 0+TINY, times = p),
        if(p > 0) rep(-1+TINY, times = p),
        if(q > 0) rep( 0+TINY, times = q),
        0,
        1/10,
        1)
    names(U) = Names
    if(.DEBUG) { cat("\nDEBUG - U: \n"); print(U) }

    # Set Upper Limits of Parameters to be Estimated:
    V = c(
        10*abs(mean(.series$x)),
        if(u > 0) rep(1-TINY, times = u),
        if(v > 0) rep(1-TINY, times = v),
        100*var(.series$x),
        if(p > 0) rep(1-TINY, times = p),
        if(p > 0) rep(1-TINY, times = p),
        if(q > 0) rep(1-TINY, times = q),
        2,
        10,
        20)
    names(V) = Names
    if(.DEBUG) { cat("\nDEBUG - V: \n"); print(V) }

    # Includes:
    includes = c(
        include.mean,
        if(u > 0) rep(TRUE, times = u),
        if(v > 0) rep(TRUE, times = v),
        TRUE,
        if(p > 0) rep(TRUE, times = p),
        if(p > 0) rep(leverage, times = p),
        if(q > 0) rep(TRUE, times = q),
        include.delta,
        include.skew,
        include.shape)
    names(includes) = Names
    if(.DEBUG) { cat("\nDEBUG - V: \n"); print(includes) }

    # Index List of Parameters to be Optimized:
    index = (1:length(params))[includes == TRUE]
    names(index) = names(params)[includes == TRUE]
    if(.DEBUG) { cat("\nDEBUG - fixed: \n"); print(index) }

    # Persistence:
    alpha <- beta <- NULL
    if(p > 0) alpha = params[substr(Names, 1, 5) == "alpha"]
    if(p > 0 & leverage) gamma = params[substr(Names, 1, 5) == "gamma"]
    if(p > 0 & !leverage) gamma = rep(0, times = p)
    if(q > 0) beta  = params[substr(Names, 1, 4) == "beta"]
    if(.series$model[2] == "garch") {
        persistence = sum(alpha) + sum(beta)
    } else if(.series$model[2] == "aparch") {
        persistence = sum(beta)
        for (i in 1:p)
            persistence = persistence + alpha[i]*garchKappa(cond.dist,
                gamma[i], params["delta"], params["skew"], params["shape"])
    }
    names(persistence) = "persistence"

    # Trace the Result:
    if(trace) {
        cat("Parameter Initialization:")
        cat("\n Initial Parameters:          $params")
        cat("\n Limits of Transformations:   $U, $V")
        cat("\n Which Parameters are Fixed?  $includes")
        cat("\n Parameter Matrix:\n")
        ans = data.frame(U, V, params, includes)
        rownames(ans) = paste("   ", names(params))
        print(ans)
        cat(" Index List of Parameters to be Optimized:\n")
        print(index)
        cat(" Persistence:                 ", persistence, "\n")
    }

    # Return Value:
    list(params = params, U = U, V = V, includes = includes,
        index = index, mu = params[1], delta = delta, skew = skew,
        shape = shape, cond.dist = cond.dist, leverage = leverage,
        persistence = persistence, control = control)
}


# ------------------------------------------------------------------------------


.garchSetCondDist <-
    function(cond.dist = "norm")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Select Conditional Density Function

    # Arguments:
    #   cond.dist - a character string with the name of the
    #       conditional distribution function. Valid strings are:
    #       "norm", "snorm", "std", "sstd", "ged", "sged".

    # Value:
    #   Returns the selection conditional distribution function
    #   named uniquely '.garchDist'.

    # Details:
    #   Implemented Distributions:
    #    norm - Normal Distribution: nothing to estimate
    #    snorm - Skew Normal Distribution: xi may be estimated
    #    std - Student-t Distribution: nu may be estimated
    #    sstd - Skew Student-t Distribution: nu and xi may be estimated
    #    ged - Generalized Error Distribution: nu may be estimated
    #    sged - Skew Generalized Error Distribution: nu and xi may be estimated

    # FUNCTION:

    # Normal Distribution:
    if(cond.dist == "norm") {
         .garchDist = function(z, hh, skew, shape) {
            dnorm(x = z/hh, mean = 0, sd = 1) / hh
        }
    }
    if(cond.dist == "snorm") {
        .garchDist = function(z, hh, skew, shape) {
            dsnorm(x = z/hh, mean = 0, sd = 1, xi = skew) / hh
        }
    }

    # Standardized Student-t:
    if(cond.dist == "std") {
        .garchDist = function(z, hh, skew, shape) {
            dstd(x = z/hh, mean = 0, sd = 1, nu = shape) / hh
        }
    }
    if(cond.dist == "sstd") {
        .garchDist = function(z, hh, skew, shape) {
            dsstd(x = z/hh, mean = 0, sd = 1, nu = shape, xi = skew) / hh
        }
    }

    # Generalized Error Distribution:
    if(cond.dist == "ged") {
        .garchDist = function(z, hh, skew, shape) {
            dged(x = z/hh, mean = 0, sd = 1, nu = shape) / hh
        }
    }
    if(cond.dist == "sged") {
        .garchDist = function(z, hh, skew, shape) {
            dsged(x = z/hh, mean = 0, sd = 1, nu = shape, xi = skew) / hh
        }
    }

    # Trace the Result:
    if(FALSE) {
        cat("\n Distribution:     ", cond.dist, "\n    .garchDist = ")
        print(.garchDist)
    }

    # Return Value:
    .garchDist
}


# ------------------------------------------------------------------------------


setfGarchEnv(.garchDist = .garchSetCondDist("norm"))


# ------------------------------------------------------------------------------


.garchLLH <-
    function(params, trace = TRUE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute Log-Likelihood Function

    # Arguments:
    #   params - a named numeric vector with the model parameters
    #       to be optimized

    # Value:
    #   Returns the value of the max log-likelihood function.

    # Note:
    #   The variables '.series' and '.params' must be global available

    # FUNCTION:

    # DEBUG:
    .DEBUG = FALSE

    # get global variables
    .series <- getfGarchEnv(".series")
    .params <- getfGarchEnv(".params")
    .garchDist <- getfGarchEnv(".garchDist")
    .llh <- getfGarchEnv(".llh")

    # Retrieve From Initialized Series:
    x = .series$x

    # Get Order:
    u = .series$order[1]
    v = .series$order[2]
    p = .series$order[3]
    q = .series$order[4]
    max.order = max(u, v, p, q)

    # Get Start Conditions:
    h.start = .series$h.start
    llh.start = .series$llh.start

    # Get the Index Values and Add Names - Just to be Sure:
    index = .params$index
    names(params) = names(.params$params[index])
    Names = names(params)

    # Retrieve From Initialized Parameters:
    cond.dist = .params$cond.dist

    # Extracting the parameters by name ...
    alpha <- beta <- NULL
    mu = c(mu = .params$mu)
    delta = c(delta = .params$delta)
    skew = c(skew = .params$skew)
    shape = c(shape = .params$shape)
    leverage = c(leverage = .params$leverage)
    if(.params$includes["mu"]) mu = params["mu"]
    if(u > 0) ar = params[substr(Names, 1, 2) == "ar"]
    if(v > 0) ma = params[substr(Names, 1, 2) == "ma"]
    omega = params[substr(Names, 1, 5) == "omega"]
    if(p > 0) alpha = params[substr(Names, 1, 5) == "alpha"]
    if(p > 0 & leverage) gamma = params[substr(Names, 1, 5) == "gamma"]
    if(p > 0 & !leverage) gamma = rep(0, times = p)
    if(q > 0) beta  = params[substr(Names, 1, 4) == "beta"]
    if(.params$includes["delta"]) delta = params["delta"]
    if(.params$includes["skew"])  skew  = params["skew"]
    if(.params$includes["shape"]) shape = params["shape"]

    # Iterate z:
    N = length(x)
    z = rep(0, N)
    if(u > 0 & v > 0)
        for (i in (h.start):N)
            z[i] = x[i] - mu - sum(ar*x[i-(1:u)]) - sum(ma*z[i-(1:v)])
    if(u > 0 & v == 0)
        for (i in (h.start):N)
            z[i] = x[i] - mu - sum(ar*x[i-(1:u)])
    if(u == 0 & v > 0)
        for (i in (h.start):N)
            z[i] = x[i] - mu - sum(ma*z[i-(1:v)])
    if(u == 0 & v == 0)
        z = x - mu

    # Initialize Variance Equation:
    deltainv = 1/delta
    if(.series$model[2] == "garch") {
        persistence = sum(alpha) + sum(beta)
    } else if(.series$model[2] == "aparch") {
        persistence = sum(beta)
        for (i in 1:p)
            persistence = persistence + alpha[i]*garchKappa(cond.dist,
                gamma[i], delta, skew, shape)
    }
    names(persistence) = "persistence"
    attr(persistence, "control") = NULL
    attr(persistence, "cond.dist") = NULL
    .params$persistence <- persistence
    setfGarchEnv(.params = .params)
    mvar = mean(z^2)
    h = rep(omega + persistence*mvar, N)

    # Iterate Conditional Variances h:
    if(p == 0) {
        alpha = 0
        p = 1
    }
    if(q == 0) {
        beta = 0
        q = 1
    }

    # How to compute the LLH recursion?
    USE = .params$control$llh

    # Test Version Just a Simple Double 'for' Loop:
    if(USE == "testing") {
        # As You Can Imagine, Slow Version But Very Useful for Testing:
        if(!.params$leverage) {
            for (i in (h.start):N) {
                h[i] = omega +
                    sum(alpha * ( abs(z[i-(1:p)])) ^ delta ) +
                    sum(beta*h[i-(1:q)])
            }
        } else {
            for (i in (h.start):N) {
                h[i] = omega +
                    sum(alpha * ( abs(z[i-(1:p)]) -
                    gamma * z[i-(1:p)])^delta ) + sum(beta*h[i-(1:q)])
            }
        }
    }

    # R Filter Representation:
    # Entirely written in S, and very effective ...
    if(USE == "filter") {
        # Note, sometimes one of the beta's can become undefined
        # during optimization.
        if(!.params$leverage) gamma = rep(0, p)
        pq = max(p, q)
        edeltat = 0
        for (j in 1:p) {
            Filter = rep(0, length = p+1)
            Filter[j+1] = alpha[j]
            edelta = (abs(z) - gamma[j]*z)^delta
            edelta = filter(edelta, filter = Filter, sides = 1)
            edeltat = edeltat + edelta
        }
        c.init = omega/(1-sum(beta))
        h = c( h[1:pq], c.init + filter(edeltat[-(1:pq)], filter = beta,
             method = "recursive", init = h[q:1]-c.init))
        ### ? remove ?
        if( sum(is.na(h)) > 0 ) {
            # We use the testing Version ...
            warning("Problems in Filter Representation")
            if(!.params$leverage) {
                for (i in (h.start):N) {
                    h[i] = omega +
                        sum(alpha * ( abs(z[i-(1:p)])) ^ delta ) +
                        sum(beta*h[i-(1:q)])
                }
            } else {
                for (i in (h.start):N) {
                    h[i] = omega +
                        sum(alpha * ( abs(z[i-(1:p)]) -
                        gamma * z[i-(1:p)])^delta ) + sum(beta*h[i-(1:q)])
                }
            }
        }
    }

    # Fortran Implementation:
    # May be Even Faster Compared with R's Filter Representation ...
    if(USE == "internal") {
        if(!.params$leverage) gamma = rep(0, p)
        # For asymmetric APARCH Models Only !!! ---- Check it ----
        h = .Fortran("garchllh", as.double(z), as.double(h), as.integer(N),
            as.double(omega), as.double(alpha), as.double(gamma),
            as.integer(p), as.double(beta), as.integer(q), as.double(delta),
            as.integer(h.start), PACKAGE = "fGarch2")[[2]]
    }

    # Save h and z:
    .series$h <- h
    .series$z <- z
    setfGarchEnv(.series = .series)

    # Calculate Log Likelihood:
    hh = (abs(h[(llh.start):N]))^deltainv
    zz = z[(llh.start):N]
    llh = -sum(log(.garchDist(z = zz, hh = hh, skew = skew, shape = shape)))
    if(.DEBUG) cat("DEBUG - LLH:   ", llh, "\n")
    names(params) = names(.params$params[.params$index])
    if(is.na(llh)) llh = .llh + 0.1*(abs(.llh))
    if(!is.finite(llh)) llh = .llh + 0.1*(abs(.llh))

    # Print if LLH has Improved:
    if(llh < .llh) {
        diff = (.llh - llh)/llh
        if(trace & diff > 1e-2) {
            # cat(" LLH: ", llh, "   norm LLH: ", llh/N, "\n")
            # print(params)
            if(persistence > 1)
                cat("Warning - Persistence:", persistence, "\n")
        }
        setfGarchEnv(.llh = llh)
    }

    # Return Value:
    c(LogLikelihood = llh)
}


# ------------------------------------------------------------------------------


.garchOptimizeLLH <-
    function(hessian = hessian, trace)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Opimize Log-Likelihood Function

    # Arguments:
    #   none

    # FUNCTION:

    # DEBUG:
    .DEBUG = FALSE

    # get global variables
    .series <- getfGarchEnv(".series")
    .params <- getfGarchEnv(".params")

    # Initialization:
    INDEX = .params$index

    # Algorithm:
    algorithm = .params$control$algorithm[1]
    TOL1 = .params$control$tol1
    TOL2 = .params$control$tol2

    # Optimize:
    if(trace) cat("\n\n--- START OF TRACE ---\n")


    # First Method: Two Step Apparoach > Trust Region + Nelder-Mead Simplex
    if(algorithm == "nlminb" | algorithm == "nlminb+nm") {
        fit <- .garchRnlminb(.params, .series, .garchLLH, trace)
        .params$llh = fit$llh
        .params$params[INDEX] = fit$par
        setfGarchEnv(.params = .params)
    }
    if(algorithm == "nlminb+nm") {
        fit <- .garchRnm(.params, .series, .garchLLH, trace)
        .params$params[INDEX] = fit$par
        setfGarchEnv(.params = .params)
    }


    # Second Method - Two Step Approach > BFGS + Nelder-Mead Simplex
    if(algorithm == "lbfgsb" | algorithm == "lbfgsb+nm") {
        fit <- .garchRlbfgsb(.params, .series, .garchLLH, trace)
        .params$llh = fit$llh
        .params$params[INDEX] = fit$par
        setfGarchEnv(.params = .params)
    }
    if(algorithm == "lbfgsb+nm") {
        fit <- .garchRnm(.params, .series, .garchLLH, trace)
        .params$params[INDEX] = fit$par
        setfGarchEnv(.params = .params)
    }


    # Third Method: Sequential Programming Algorithm ...
    if(algorithm == "sqp") {
        fit <- .garchFsqp(.params, .series, .garchLLH, trace)
    }


    # Fourth Method: Rdonlp2 ...
    if(algorithm == "donlp2") {
        fit <- .garchRdonlp2(.params, .series, .garchLLH, trace)
    }


    # Fivth Method: NLMINB full Fortran implementation ...
    if(algorithm == "mnfb") {
        fit = .garchFmnfb(.params, .series, .garchLLH, trace)
    }


    # Compute the Hessian:
    if (hessian == "fcd") {
        fit$hessian = - .garchFCDAHessian(
            par = fit$par, .params = .params, .series = .series)
            titleHessian = "Central"
    } else if (hessian == "rcd") {
        fit$hessian = - .garchRCDAHessian(
            par = fit$par, .params = .params, .series = .series)
            titleHessian = "Central"
    } else if (hessian == "ffd") {
        fit$hessian = - .garchFFDAHessian(
            par = fit$par, .params = .params, .series = .series)
            titleHessian = "Forward"
    }

    # Compute Information Criterion Statistics:
    N = length(.series$x)
    NPAR = length(fit$par)
    fit$ics = c(
        AIC  = (2*fit$value)/N + 2 * NPAR/N,
        BIC  = (2*fit$value)/N + NPAR * log(N)/N,
        SIC  = (2*fit$value)/N + log((N+2*NPAR)/N),
        HQIC = (2*fit$value)/N + (2*NPAR*log(log(N)))/N )

    # Print LLH if we trace:
    if(trace) {
        setfGarchEnv(.llh = 1.0e99)
        cat("\nFinal Estimate of the Negative LLH:\n")
        .llh =  .garchLLH(fit$par, trace = FALSE)
        cat(" LLH: ", .llh, "   norm LLH: ", .llh/N, "\n")
        print(fit$par)
        setfGarchEnv(.llh = .llh)
    }

    # Print Hessian Matrix if we trace:
    if(trace) {
        cat("\n", titleHessian, " Difference Approximated Hessian Matrix:\n",
            sep = "")
        print(fit$hessian)
        cat("\n--- END OF TRACE ---\n\n")
    }

    # Return Value:
    fit
}


# ------------------------------------------------------------------------------


.garchNames <-
    function(object)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Print slot names, @fit slot, parameters and controls

    # Arguments:
    #   object - an object of class 'fGARCH'

    # FUNCTION:

    # Slot Names:
    cat("\nNames - @ \n")
    print(slotNames(object))

    # @fit Slot:
    cat("\nNames - @fit \n")
    print(names(object@fit))

    # Parameters:
    cat("\nNames - @fit$params \n")
    print(names(object@fit$params))

    # Control:
    cat("\nNames - @fit$params$control \n")
    print(names(object@fit$params$control))

    # Return Value:
    invisible()
}


################################################################################

