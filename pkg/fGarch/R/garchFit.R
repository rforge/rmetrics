
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


##############################################################################
# FUNCTION:               PARAMETER ESTIMATION:    
#  garchFit                Fits GARCH and APARCH processes
#  .garchArgsParser        Parses formula and data for garchFit
#  .garchOptimizerControl  Sets default values for Garch Optimizer
#  .garchFit               ... old Version
#  .garchInitSeries        Initializes Series
#  .garchInitParameters    Initializes Parameters
#  .garchSetCondDist       Selects conditional density function
#   .garchDist              Defines conditional density function
#  .garchOptimizeLLH       Opimizes log-likelihood function  
#   .garchLLH               Computes log-likelihood function
#   .garchHessian           Computes Hessian matrix numerically
#  .garchNames              Slot names, @fit slot, parameters and controls
##############################################################################
  


.llh = 1e99
.garchDist = NA
.params = NA
.series = NA


# ------------------------------------------------------------------------------
    

garchFit <-  
    function(formula, data, init.rec = c("mci", "uev"), delta = 2, skew = 1, 
    shape = 4, cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"), 
    include.mean = TRUE, include.delta = NULL, include.skew = NULL, 
    include.shape = NULL, leverage = NULL, trace = TRUE, 
    algorithm = c("nlminb", "sqp", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
    control = list(), title = NULL, description = NULL, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Fit parameters to a ARMA-GARCH model
    
    # Arguments:
    #   formula - ARMA(m,n) + GARCH/APARCH(p,q) mean and variance 
    #       specification
    #   data - any univariate time series which can be converted
    #       into anumeric vector using the generic function asVector   
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
    
    # Call:
    CALL = match.call()
    
    # Parse formula and data for garchFit
    args = .garchArgsParser(formula, data, trace = FALSE)
          
    # Fit:
    ans = .garchFit(
        formula.mean = args$formula.mean, 
        formula.var = args$formula.var, 
        series = args$series, 
        init.rec, delta, skew, shape, cond.dist, include.mean, 
        include.delta, include.skew, include.shape, leverage, 
        trace, algorithm, control, title, description, ...)
    ans@call = CALL
    
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
    #   data - any univariate time series which can be converted
    
    # FUNCTION:
    
    # Check formula expression dependent on data class:
    if(class(data) == "timeSeries") {
        formulaLength = length(formula)
        if(formulaLength < 3) {
            stop1 = "For timeSeries objects you must specifiy the full formula:"
            stop2 = "e.g. formula = X ~ arma(2,1) + garch(1,1)"
            stop(paste(stop1, stop2)) 
        }
    }
    if(class(data) == "data.frame") {
        formulaLength = length(formula)
        if(formulaLength < 3) {
            stop1 = "For data.frame objects you must specifiy the full formula:"
            stop2 = "e.g. formula = X ~ arma(2,1) + garch(1,1)"
            stop(paste(stop1, stop2)) 
        }
    }
    
    # Get Data:
    mf = match.call(expand.dots = FALSE)
    if(trace) {
        cat("\nMatched Function Call:\n ")
        print(mf)
    }
    m = match(c("formula", "data"), names(mf), 0)
    mf = mf[c(1, m)]
    mf[[1]] = as.name(".modelSeries")
    mf$fake = FALSE
    mf$lhs = TRUE
    if(trace) {
        cat("\nModelSeries Call:\n ") 
        print(mf)
    }
    x = eval(mf, parent.frame())
    x = asVector(x[, 1])
    if(class(mf$data) == "timeSeries") names(x) = rownames(data)
    if(trace) {
        cat("\nThe first few data points:\n ")
        print(head(x))
        cat("\nData Class:\n ")
        print(class(x))
    }
    
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
    ans = list(
        formula.mean = formula.mean, 
        formula.var = formula.var, 
        series = x)
    
    # Return Value:
    invisible(ans)  
}


# ------------------------------------------------------------------------------


.garchOptimizerControl =
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
        fscale = FALSE,
        xscale = FALSE,
        algorithm = algorithm,
        llh = c("filter", "internal", "testing")[1],
        
        # BFGS - NLMINB Algorithm:
        tol1 = 1, 
        tol2 = 1, 
        
        # SQP Algorithm:
        MIT = 200,     # maximum number of iterations (200)
        MFV = 500,     # maximum number of function evaluations (500)
        MET = 2,       # specifies scaling strategy:
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
        TOLX = 1.0e-16, 
        TOLC = 1.0e-6, 
        TOLG = 1.0e-6, 
        TOLD = 1.0e-6, 
        TOLS = 1.0e-4, 
        RPF  = 1.0e-4) 
        
    # Return Value:
    con
}


# ------------------------------------------------------------------------------


.garchFit <-
    function(formula.mean = ~arma(0, 0), formula.var = ~garch(1, 1), 
    series, init.rec = c("mci", "uev"), delta = 2, skew = 1, shape = 4,
    cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"), 
    include.mean = TRUE, include.delta = NULL, include.skew = NULL,
    include.shape = NULL, leverage = NULL, trace = TRUE,  
    algorithm = c("sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
    control = list(), title = NULL, description = NULL, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description
    #   Fit parameters to a ARMA-GARCH model
    
    # Arguments:
    #   formula.mean - ARMA(m,n) mean specification
    #   formula.var - GARCH/APARCH(p,q) variance specification
    #   series - time series 
    #       by default the data are taken from the series x
    #           if x is a data.frame then the first column is selected
    #       if series is a character string then the data are
    #           retrieved from data(series)
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
  
    # Check for Recursion Initialization:
    if(init.rec[1] != "mci" & algorithm[1] != "sqp") {
        stop("Algorithm only supported for mci Recursion")
    }
    
    # Series:
    if(is.character(series)) {
        eval(parse(text = paste("data(", series, ")")))
        series = eval(parse(text = series))
    }
    if(is.data.frame(series)) {
        series = series[, 1]
    }
    series = as.vector(series)
    
    # Start Time:
    .StartFit <- Sys.time()
    
    # Generate Control List - Define Default Settings:
    con <- .garchOptimizerControl(algorithm)
    con[(namc <- names(control))] <- control
    
    # Initialize Time Series Information - Save Globally:            
    .series <<- .garchInitSeries(formula.mean = formula.mean, 
        formula.var = formula.var, series = series, scale = sd(series),
        init.rec = init.rec[1], h.start = NULL, llh.start = NULL, 
        trace = trace)
        
    # Initialize Model Parameters - Save Globally:
    .params <<- .garchInitParameters(formula.mean = formula.mean, 
        formula.var = formula.var, delta = delta, skew = skew, 
        shape = shape, cond.dist = cond.dist[1], 
        include.mean = include.mean, include.delta = include.delta, 
        include.skew = include.skew, include.shape = include.shape, 
        leverage = leverage, algorithm = algorithm[1], control = con,
        trace = trace)

    # Select Conditional Distribution Function:
    .garchDist <<- .garchSetCondDist(cond.dist[1]) 
    
    # Estimate Model Parameters - Minimize llh, start from big value: 
    .llh <<- 1.0e99   
    fit = .garchOptimizeLLH(trace) 
    fit$llh = .llh
     
    # Add to Fit: 
    names(.series$h) <- NULL
    fit$series = .series
    fit$params = .params
        
    # Retrieve Residuals and Fitted Values: 
    residuals = .series$z 
    fitted.values = .series$x - residuals
    h.t = .series$h
    deltainv = 1/fit$params$delta
    sigma.t = (.series$h)^deltainv
    
    # Standard Errors and t-Values:
    fit$cvar = solve(fit$hessian)
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
        formula = list(formula.mean = formula.mean, formula.var = formula.var),
        method = "Max Log-Likelihood Estimation", 
        data = list(x = series),
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
        stop("GARCH model only supported for mci Recutrsion")
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
      

.garchInitParameters = 
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
    if(cond.dist == "t") cond.dist = "dstd"
    skewed.dists = c("dsnorm", "dsged", "dsstd")
    if(is.null(include.skew)) {
        if(any(skewed.dists == cond.dist)) {
            include.skew = TRUE
        } else {
            include.skew = FALSE
        }
    }
    shaped.dists = c("dged", "dsged", "dstd", "dsstd")
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
    function(cond.dist = "dnorm") 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Select Conditional Density Function
    
    # Arguments:
    #   cond.dist - a character string with the name of the 
    #       conditional distribution function. Valid strings are:
    #       "dnorm", "dsnorm", "dstd", "dsstd", "dged", "dsged".
    
    # Value:
    #   Returns the selection conditional distribution function
    #   named uniquely '.garchDist'.
    
    # Details:
    #   Implemented Distributions: 
    #    dnorm - Normal Distribution: nothing to estimate
    #    dsnorm - Skew Normal Distribution: xi may be estimated 
    #    dstd - Student-t Distribution: nu may be estimated
    #    dsstd - Skew Student-t Distribution: nu and xi may be estimated
    #    dged - Generalized Error Distribution: nu may be estimated
    #    dsged - Skew Generalized Error Distribution: nu and xi may be estimated
    
    # FUNCTION:
    
    # Normal Distribution:
    if(cond.dist == "dnorm") {
         .garchDist = function(z, hh, skew, shape) {
            dnorm(x = z/hh, mean = 0, sd = 1) / hh 
        }
    }
    if(cond.dist == "dsnorm") { 
        .garchDist = function(z, hh, skew, shape) {
            dsnorm(x = z/hh, mean = 0, sd = 1, xi = skew) / hh 
        }
    }
    
    # Standardized Student-t:
    if(cond.dist == "dstd") { 
        .garchDist = function(z, hh, skew, shape) {
            dstd(x = z/hh, mean = 0, sd = 1, nu = shape) / hh
        }
    }
    if(cond.dist == "dsstd") { 
        .garchDist = function(z, hh, skew, shape) {
            dsstd(x = z/hh, mean = 0, sd = 1, nu = shape, xi = skew) / hh
        }
    }
      
    # Generalized Error Distribution:
    if(cond.dist == "dged") {
        .garchDist = function(z, hh, skew, shape) {
            dged(x = z/hh, mean = 0, sd = 1, nu = shape) / hh
        }
    }
    if(cond.dist == "dsged") {
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


.garchDist = .garchSetCondDist("dnorm")


# ------------------------------------------------------------------------------
 

.garchLLH =
    function(params, trace) 
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
    .params$persistence <<- persistence
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
        # For asymmetric APARCH Models Only !!! 
        h = .Fortran("aparchllh", as.double(z), as.double(h), as.integer(N),
            as.double(omega), as.double(alpha), as.double(gamma), 
            as.integer(p), as.double(beta), as.integer(q), as.double(delta), 
            as.integer(h.start), PACKAGE = "fGarch")[[2]]  
    }
    
    # Save h and z:
    .series$h <<- h
    .series$z <<- z
    
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
        .llh <<- llh
        if(trace) {   
            cat(" LLH: ", llh, "   norm LLH: ", llh/N, "\n")
            print(params)
            if(persistence > 1) 
                cat("Warning - Persistence:", persistence, "\n")
        }
    }
    
    # Return Value:
    c(LogLikelihood = llh) 
}


# ------------------------------------------------------------------------------


.garchHessian =
    function(par, trace)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the Hessian Matrix
    
    # Details:
    #   This function computes the Hessian dependent on the 
    #   implementation. For the pure S implementations "nlminb"
    #   and "lbfgsb" the Hessian is also computed from a pure S
    #   function. In the case of the Fortran version we also 
    #   compute the Hessian from a much more effective Fortran
    #   implementation.

    # FUNCTION:
 
    # Compute Hessian:
    algorithm = .params$control$algorithm[1]
    EPS0 = 1.0e-4
    if(algorithm == "nlminb" | algorithm == "lbfgsb" | 
        algorithm == "nlminb+nm" | algorithm == "lbfgsb+nm") {
        # CASE I: NLMINB and BFGS
        keep.trace = trace
        keep.control = .params$control
        eps = EPS0 * par
        n = length(par)
        H = matrix(0, ncol = n, nrow = n)
        trace <- FALSE
        for (i in 1:n) {
            for (j in 1:n) {
                x1 = x2 = x3 = x4 = par
                x1[i] = x1[i] + eps[i]
                x1[j] = x1[j] + eps[j]
                x2[i] = x2[i] + eps[i]
                x2[j] = x2[j] - eps[j]
                x3[i] = x3[i] - eps[i]
                x3[j] = x3[j] + eps[j]
                x4[i] = x4[i] - eps[i]
                x4[j] = x4[j] - eps[j]
                H[i, j] = ( 
                    .garchLLH(x1, trace) - 
                    .garchLLH(x2, trace) -
                    .garchLLH(x3, trace) + 
                    .garchLLH(x4, trace) ) / 
                        (4*eps[i]*eps[j])
            }
        }
        trace <- keep.trace  
        .params$control <<- keep.control 
    } else {
        # Case II: SQP
        N = length(.series$x)
        NF = length(par)
        if(.params$includes["delta"]) {
            XDELTA = par["delta"] 
        } else {
            XDELTA = .params$delta
        } 
        if(.params$includes["skew"]) {
            XSKEW = par["skew"] 
        } else {
            XSKEW = .params$skew
        }   
        if(.params$includes["shape"]) {
            XSHAPE = par["shape"] 
        } else {
            XSHAPE = .params$shape
        } 
        DPARM = c(XDELTA, XSKEW, XSHAPE)    
        MDIST = c(dnorm = 10, dsnorm = 11, dstd = 20, dsstd = 21, dged = 30, 
            dsged = 31)[.params$cond.dist]                # Which Distribution
        REC = 1
        if(.series$init.rec == "uev") REC = 2
        MYPAR = c(
            REC   = REC,                                  # How to initialize
            LEV   = as.integer(.params$leverage),         # Include Leverage 0|1 
            MEAN  = as.integer(.params$includes["mu"]),   # Include Mean 0|1 
            DELTA = as.integer(.params$includes["delta"]),# Include Delta 0|1                          
            SKEW  = as.integer(.params$includes["skew"]), # Include Skew 0|1 
            SHAPE = as.integer(.params$includes["shape"]),# Include Shape 0|1 
            ORDER = .series$order)                        # Order of ARMA-GARCH
        # Compute Hessian:
        ans = .Fortran("garchhess",
            N = as.integer(N), 
            Y = as.double(.series$x), 
            Z = as.double(rep(0, times = N)), 
            H = as.double(rep(0, times = N)), 
            NF = as.integer(NF), 
            X = as.double(par), 
            DPARM = as.double(DPARM), 
            MDIST = as.integer(MDIST), 
            MYPAR = as.integer(MYPAR), 
            E0 = as.double(EPS0),
            HESS = as.double(rep(0, times = NF*NF)),
            PACKAGE = "fGarch")
        # The Matrix:
        H = matrix(ans[["HESS"]], ncol = NF)  
        colnames(H) = rownames(H) = names(par)
    }
    
    # Return Value:
    H   
}


# ------------------------------------------------------------------------------


.garchOptimizeLLH =
    function(trace) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Opimize Log-Likelihood Function
    
    # Arguments:
    #   none
    
    # FUNCTION:
       
    # DEBUG:
    .DEBUG = FALSE
    
    # Initialization:
    INDEX = .params$index
      
    # Algorithm:
    algorithm = .params$control$algorithm[1]
    TOL1 = .params$control$tol1
    TOL2 = .params$control$tol2
    
    # Optimize:
    if(trace) cat("\nIteration Path:\n") 
    
    # First Method:
    # Two Step Apparoach - Trust Region + Nelder-Mead Simplex
    if(algorithm == "nlminb" | algorithm == "nlminb+nm") {
        if(trace) cat("\n\n\nNow NLMINB \n\n\n")
        parscale = rep(1, length = length(INDEX))
        names(parscale) = names(.params$params[INDEX])
        parscale["omega"] = var(.series$x)^(.params$delta/2)
        parscale["mu"] = abs(mean(.series$x))
        fit = nlminb(
            start = .params$params[INDEX],
            objective = .garchLLH, 
            lower = .params$U[INDEX],
            upper = .params$V[INDEX],
            scale = 1/parscale,
            control = list(eval.max = 2000, iter.max = 1500, 
                rel.tol = 1e-14*TOL1, x.tol = 1e-14*TOL1),
            trace = trace)
        fit$value = fit$objective 
        if(algorithm == "nlminb+nm") {          
            if(trace) cat("\n\n\nNow Nelder-Mead \n\n\n")
            fnscale = abs(.garchLLH(.params$params[INDEX], trace))
            fit = optim(
                par = fit$par,
                fn = .garchLLH,
                method = "Nelder-Mead",
                control = list(
                    ndeps = rep(1e-14*TOL2, length = length(INDEX)), 
                    maxit = 10000, 
                    reltol = 1e-14*TOL2, 
                    fnscale = fnscale, 
                    parscale = c(1, abs(fit$par[-1]))),
                hessian = TRUE,
                trace = trace)
        }
    }
    
    # Second Method:
    # Two Step Approach - BFGS + Nelder-Mead Simplex
    if(algorithm == "lbfgsb" | algorithm == "lbfgsb+nm") {
        if(trace) cat("\n\n\nNow L-BFGS-B \n\n\n")
        parscale = rep(1, length = length(INDEX))
        names(parscale) = names(.params$params[INDEX])
        parscale["omega"] = var(.series$x)^((.params$params["delta"])/2)
        fit = optim(
            par = .params$params[INDEX], 
            fn = .garchLLH, 
            lower = .params$U[INDEX], 
            upper = .params$V[INDEX], 
            method = "L-BFGS-B", 
            control = list(
                parscale = parscale, 
                lmm = 20, 
                pgtol = 1e-14 * TOL1, 
                factr = 1 * TOL1),
            trace = trace)       
        if(algorithm == "lbfgsb+nm") {
            if(trace) cat("\n\n\nNow Nelder-Mead \n\n\n")
            fnscale = abs(fit$value)
            parscale = abs(fit$par)
            fit = optim(
                par = fit$par, 
                fn = .garchLLH, 
                method = "Nelder-Mead", 
                control = list(
                    ndeps = rep(1e-14 * TOL2, length = length(INDEX)), 
                    maxit = 10000, 
                    reltol = 1e-14 * TOL2, 
                    fnscale = fnscale, 
                    parscale = parscale), 
                hessian = TRUE,
                trace = trace)
        }
    } # End of Second Method
    
    # Third Method:
    # Sequential Programming Algorithm
    # IPAR, RPAR and MYPAR Parameter Setting:
    if(algorithm == "sqp") {
        if(trace) cat(" SQP Algorithm\n\n")
        IPAR = c(
            IPRNT = as.integer(trace),    #  [1, 200, 500, 2, 2, 1, 4]
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
        MDIST = c(dnorm = 10, dsnorm = 11, dstd = 20, dsstd = 21, dged = 30, 
            dsged = 31)[.params$cond.dist]
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
        ans = .Fortran("garchfit",
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
        fit = list()
        fit$par = ans[[6]]
        fit$value = ans[[14]] 
        
        # Update .series
        names(fit$par) = names(.params$params[INDEX]) 
        updateLLH = .garchLLH(fit$par, trace)
    } # End of Third Method
    
    # Add Names:
    names(fit$par) = names(.params$params[INDEX]) 
    fit$coef = fit$par
    
    # Compute Hessian:
    .StartHessian <- Sys.time()
    H = .garchHessian(fit$par, trace)
    Time =  Sys.time() - .StartHessian
    if(trace) {
        cat("\nTime to Compute Hessian:\n ")
        print(Time)  
    }  
    
    # Information Criterion Statistics:
    N = length(.series$x)
    NPAR = length(fit$par)
    fit$ics = c(
        AIC  = (-2*fit$value)/N + 2 * NPAR/N,
        BIC  = (-2*fit$value)/N + NPAR * log(N)/N,
        SIC  = (-2*fit$value)/N + log((N+2*NPAR)/N),
        HQIC = (-2*fit$value)/N + (2*NPAR*log(log(N)))/N )
        
    # Final Function Evaluation: 
    if(trace) {
        # Note, that .garchLLH() will print the final estimate ...
        .llh <<- 1.0e99
        cat("\nFinal Estimate:\n")
        .llh <<- .garchLLH(fit$par, trace)
    }  
    
    # Hessian:
    colnames(H) = rownames(H) = names(.params$params[INDEX])
    fit$hessian = H
    
    # Print Hessian Matrix:
    if(trace) {
        cat("\nHessian Matrix:\n")
        print(fit$hessian)
        cat("\n--- END OF TRACE ---\n\n") 
    }
    
    # Alternative Variable of Coefficients:
    fit$coef = fit$par
 
    # Return Value:
    fit 
}


# ------------------------------------------------------------------------------


.garchNames =
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

