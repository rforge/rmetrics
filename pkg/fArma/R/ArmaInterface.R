
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
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


################################################################################
# FUNCTION:               SIMULATION AND FITTING:
#  'fARMA'                 S4 Class representation for "fARMA" objects
#  armaSim                 Simulates an ARIMA time series process
#  armaFit                 Fits parameters for ARMA Time Series process
#  .arFit                   Internal function called by armaFit
#  .arimaFit                Internal function called by armaFit
#  .arfimaFit               Internal function called by armaFit
# S3 METHOD:              PREDICTION:
#  predict.fARMA           S3: Predicts from an ARMA time series prrocess 
#  .arPpredict              Internal function called by predict.fARMA
#  .arimaPpredict           Internal function called by predict.fARMA
#  .arfimaPredict           Internal function - Not yet implemented
# GENERIC METHODS:        PRINT - PLOT - SUMMARY METHODS:
#  show.fARMA              S4: Prints a fitted ARMA time series object
#  plot.fARMA              S3: Plots stylized facts of a fitted ARMA object
#  summary.fARMA           S3: Summarizes a fitted ARMA time series object
# S3 METHOD:              ADDON METHODS:
#  coef.fARMA              S3: Returns coefficidents from a fitted ARMA object
#  coefficients.fARMA      S3: Synonyme for coef.fARMA
#  fitted.fARMA            S3: Returns fitted values from a fitted ARMA object
#  residuals.fARMA         S3: Returns residuals from a fitted ARMA object
# FUNCTION:
#  .modelSeries         Models a timeSeries object to use formulas
################################################################################

       
################################################################################
# BUILTIN - PACKAGE DESCRIPTION:
#  Package: fracdiff
#  Version: 1.1-1
#  Title: Fractionally differenced ARIMA (p,d,q) models
#  Date: 2004-01-12
#  Author: S original by Chris Fraley <fraley@stat.washington.edu>.
#    R port by Fritz Leisch <leisch@ci.tu-wien.ac.at>;
#    since 2003-12: Martin Maechler
#  Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>
#  Description: Maximum likelihood estimation of the parameters of a 
#    fractionally differenced ARIMA(p,d,q) model (Haslett and Raftery, 
#    Appl.Statistics, 1989).
#  License: GPL version 2 or later
#  Packaged: Mon Jan 12 11:22:27 2004; maechler
################################################################################


setClass("fARMA", 
    representation(
        call = "call",
        formula = "formula",
        method = "character",
        parameter = "list",
        data = "list",
        fit = "list",
        residuals = "list",
        fitted = "list",
        title = "character",
        description = "character"
    )  
)


# ------------------------------------------------------------------------------


armaSim = 
function(model = list(ar = c(0.5, -0.5), d = 0, ma = 0.1), n = 100,
positions = NULL, innov = NULL, n.start = 100, start.innov = NULL, 
rand.gen = rnorm, rseed = NULL, addControl = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulates an ARIMA Time Series Process
    
    # Details:
    #   Splus-Like argument list ...
    #   Rmetrics Notation:
    #     armaSim(model = list(ar = c(0.5, -0.5), d = 0, ma = 0.1), n = 100,
    #       innov = NULL, n.start = 100, start.innov = NULL, 
    #       rand.gen = rnorm, rseed = NULL, ...) 
    # SPlus Notation:
    #     arima.sim (model, n = 100, 
    #       innov = rand.gen(n, ...), n.start = 100, start.innov = NULL, 
    #       rand.gen = rnorm, xreg = NULL, reg.coef = NULL, ...)
    
    # Example:
    #   armaSim(model = list(ar = c(0.5, -0.5), d = 0, ma = 0.1))
    #   armaSim(model = list(ar = c(0.5, -0.5), d = 0.2, ma = 0.1))
    #   armaSim(model = list(ar = 0, d = 0.2, ma = 0))
    #   armaSim(model = list(d = 0.2))
   
    # FUNCTION:
    
    # Checks:
    if (!is.list(model)) 
        stop("model must be a list")
        
    # Simulate:
    if (!is.null(rseed))  
        set.seed(rseed)
    if (is.null(innov)) 
        innov = rand.gen(n, ...)
    n = length(innov) 
    if (is.null(start.innov)) 
        start.innov = rand.gen(n, ...) 
    n.start = length(start.innov)

    # AR PART:
    p = length(model$ar)
    if (p == 1 && model$ar == 0) 
        p = 0
    if (p) { 
        minroots = min(Mod(polyroot(c(1, -model$ar))))
        if (minroots <= 1) warning(" AR part of model is not stationary") 
    }
    
    # MA PART:
    q = length(model$ma)
    if (q == 1 && model$ma == 0) 
        q = 0
    if (n.start < p + q) 
        stop("burn-in must be as long as ar + ma")
    
    # DIFFERENCING:
    ## if (model$d < 0) stop("d must be positive ") 
    dd = length(model$d)    
    if (dd) { 
        # ARFIMA|FRACDIFF if "dd" is a non-integer value:
        d = model$d
        if (d != round(d) ) { 
            TSMODEL = "ARFIMA" 
        } else { 
            TSMODEL = "ARIMA" 
        } 
    } else {
        d = 0 
        TSMODEL = "ARIMA" 
    } 
    
    # ARMA:
    if (TSMODEL == "ARIMA") {
        x = ts(c(start.innov, innov), start = 1 - n.start) 
        if (length(model$ma)) x = filter(x, c(1, model$ma), sides = 1)
        if (length(model$ar)) x = filter(x, model$ar, method = "recursive")
        x = x[-(1:n.start)]
        if (d > 0) x = diffinv(x, differences = d) 
    }
     
    # ARFIMA [FRACDIFF]:   
    if (TSMODEL == "ARFIMA") {
        if (p == 0) model$ar = 0
        if (q == 0) model$ma = 0
        mu = 0
        # Use Fortran Routine from R's contributed fracdiff package:
        # This is a BUILTIN function ...
        if (!is.null(rseed)) set.seed(rseed)
        eps = rnorm(n + q)
        x = .Fortran("fdsim", as.integer(n), as.integer(p), as.integer(q), 
            as.double(model$ar), as.double(model$ma), as.double(model$d), 
            as.double(mu), as.double(eps), x = double(n + q), 
            as.double(.Machine$double.xmin), as.double(.Machine$double.xmax), 
            as.double(.Machine$double.neg.eps), as.double(.Machine$double.eps), 
            PACKAGE = "fArma")$x[1:n] 
    }
               
    # Time Series:
    if (is.null(positions)) {
        ans = as.ts(x)
    } else {
        stopifnot(class(positions) == "timeDate")
        stopifnot(n == length(positions))
        ans = timeSeries(data = matrix(x, ncol = 1), charvec = positions, ...)
    }
    
    # Add Control:
    if (addControl) {
        control = c(ar = model$ar, d = model$d, ma = model$ma)
        Names = names(control)
        control = as.character(c(control, substitute(rand.gen)))
        names(control) = c(Names, "rand.gen")
        if (!is.null(rseed)) control = c(control, rseed = rseed)
        attr(ans, "control") = control
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------
 

armaFit = 
function(
formula, data, method = c("mle", "ols"), 
include.mean = TRUE, fixed = NULL, title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits Model Parameters for an ARMA Time Series Process
    
    # Arguments:
    #   method - "mle", "ols"
    
    # Notes:
    #   Valid formulas are:
    #       "ar" "ma" "arma" "arima" "arfima" (not documented "fracdiff")
    #       Note, "arma(p,q)" uses arima(p,0,q)
    
    # Details:
    #   R-base:
    #       arima(
    #           x, 
    #           order = c(0, 0, 0), 
    #           seasonal = list(order = c(0, 0, 0), period = NA), 
    #           xreg = NULL, 
    #           include.mean = TRUE, 
    #           transform.pars = TRUE, 
    #           fixed = NULL, 
    #           init = NULL, 
    #           method = c("CSS-ML", "ML", "CSS"), 
    #           n.cond, 
    #           optim.control = list(), 
    #           kappa = 1e+06) 
    #   Compare with SPlus:
    #       arima.mle(
    #           x, 
    #           model, 
    #           n.cond, 
    #           xreg=NULL,  
    #           ...) 
   
    # FUNCTION:
    
    # Check for Method:
    # ar.method       = c("yw", "burg1", "burg2", "ols", "mle")
    # arma.method     = c("CSS")
    # arima.method    = c("CSS-ML", "ML", "CSS")    
    # fracdiff.method = NA
    method = method[1]  # Don't use match.arg(methods)
    
    # Call:
    fit = NULL
    call = match.call()
    
    # Add to Fracdiff: h and M default Settings
    mf = match.call(expand.dots = TRUE)
    m = match("h", names(mf), 0)
    if (m == 0) h = -1 else h = eval(mf[[m]])
    fracdiff.h = h
    m = match("M", names(mf), 0)
    if (m == 0) M = 100 else M = eval(mf[[m]])
    fracdiff.M = M
    
    # Get Series:
    # DW 2005-09-03
    mf = match.call(expand.dots = FALSE)
    m = match(c("formula", "data"), names(mf), 0)
    mf = mf[c(1, m)]
    mf[[1]] = as.name(".modelSeries")
    mf$fake = FALSE
    mf$lhs = TRUE
    if (missing(data)) data = eval(parse(text = search()[2]), parent.frame())
    mf$data = data
    x = eval(mf, parent.frame())
    x = ts = as.vector(x[, 1])

    # Allow for univariate 'timeSeries' Objects:
    # Added 2004-09-04 DW
    if (class(ts) == "timeSeries") ts = as.vector(ts)
    
    # Which Model?
    # DW 2006-02-21
    K = length(formula)
    end = regexpr("\\(", as.character(formula[K]))-1
    tsmodel =  substr(as.character(formula[K]), 1, end)
    
    # Valid Model?
    valid = FALSE
    if (tsmodel == "ar" ) valid = TRUE
    if (tsmodel == "ma" ) valid = TRUE
    if (tsmodel == "arma") valid = TRUE
    if (tsmodel == "arima") valid = TRUE
    if (tsmodel == "arfima") valid = TRUE
    if (tsmodel == "fracdiff") valid = TRUE
    if (!valid) stop("Invalid Formula Specification")
    
    # Which Order?
    start = regexpr("\\(", as.character(formula[K]))+1
    end   = regexpr("\\)", as.character(formula[K]))-1
    order = substr(as.character(formula[K]), start, end)
    
    if (tsmodel == "arfima" || tsmodel == "fracdiff") {
        # method will be ignored ...
        pos = regexpr(",", order)
        p = as.integer(substr(order, 1, pos-1))
        q = as.integer(substr(order, pos+1, nchar(order)))
        order = c(p, q)
        tsmodel = "arfima"
    } 
    
    if (tsmodel == "arima") {
        if (method == "mle") method = "CSS-ML"
        pos = regexpr(",", order)   
        p = as.integer(substr(order, 1, pos-1))
        order = substr(order, pos+2, nchar(order))
        d = as.integer(substr(order, 1, pos-1))
        q = as.integer(substr(order, pos+1, nchar(order)))
        order = c(p = p, d = d, q = q)
    }
    
    if (tsmodel == "arma") {
        if (method == "mle") method = "CSS-ML"
        # "arma" uses "arima"
        pos = regexpr(",", order)
        p = as.integer(substr(order, 1, pos-1))
        q = as.integer(substr(order, pos+1, nchar(order)))
        order = c(p = p, d = 0, q = q)
        tsmodel = "arima"
    }
    
    if (tsmodel == "ar") {
        # if method is CSS-ML, CSS, or ML, then "ar" uses "arima":
        order = as.integer(order) 
        if (method == "mle") method = "CSS-ML"
        if (method == "CSS-ML" | method == "CSS" | method == "ML") {
            p = order
            order = c(p = p , d = 0, q = 0)
            tsmodel = "arima"
        }
    }
    
    if (tsmodel == "ma") {
        # if method is CSS-ML, CSS, or ML, then "ma" uses "arima":
        if (method == "mle") method = "CSS-ML"
        order = as.integer(order) 
        order = c(p = 0 , d = 0, q = order)
        tsmodel = "arima"
    }
    
    # Which Function?
    fun = match.fun(paste(".", tsmodel, "Fit", sep = ""))
   
    # Fit:
    fit = fun(x = ts, order = order, include.mean = include.mean, 
        method = method[1], fixed = fixed, ...)  
     
    # "ols" specific:
    if (method == "ols") {
        se.coef = unlist(fit$se.coef)
        if (include.mean){
            ols.mean = se.coef[1]
            fit$se.coef = c(se.coef[-1], ols.mean) } 
    } 
    fit$call = call
    fit$tsmodel = tsmodel
    fit$class = "fARMA"
    class(fit) = "list"
       
    # Add title and desription:
    if (is.null(title)) title = "ARIMA Modelling"
    if (is.null(description)) description = .description()
    
    
    # Parameters:
    parameter = list(include.mean = include.mean, fixed = fixed)
    if (tsmodel == "arfima" || tsmodel == "fracdiff") { 
        parameter$M = fracdiff.M
        parameter$h = fracdiff.h
    }
       
    # Return Value:
    new("fARMA",     
        call = as.call(match.call()),
        formula = as.formula(formula), 
        method = as.character(method),
        parameter = parameter,
        data = list(x = x),
        fit = fit,
        residuals = list(residuals = fit$residuals),
        fitted = list(fitted = fit$fitted.values),
        title = as.character(title), 
        description = as.character(description) )
}


# ------------------------------------------------------------------------------


.arFit =
function(x, order, include.mean, fixed = NULL,
method = c("yw", "burg1", "burg2", "ols", "mle"), M = NULL, h = NULL, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits an AR time series model
    
    # Note:
    #   Calls ar() from R-stats.
    
    # FUNCTION:
    
    # Fit:
    call = match.call()
    var.method = as.integer(substring(method[1], 5, 5))
    method = substr(method[1], 1, 4)  
    fit = ar(x = x, aic = FALSE, order.max = order, method = method, 
        var.method = var.method, demean = include.mean, 
        intercept = include.mean, ...)  
    
    # Add and Modify:
    fit$call = call
    fit$tstitle = paste("AR(", 
        as.character(order), ") with method: ", method, sep = "")
    fit$order = order
    
    # Residuals:
    fit$residuals = fit$resid
    fit$fitted.values = x - fit$resid
    fit$sigma2 = fit$var.pred 
    
    # Coefficients:
    if (method == "ols") {
        fit$coef = fit$ar[,,1]
    } else {
        fit$coef = fit$ar
    }
    names(fit$coef) = c(paste("ar", 1:order, sep=""))
    
    # Mean:
    if (include.mean) {
        coeff = c(fit$coef, fit$x.mean)
        names(coeff) = c(names(fit$coef), "intercept") 
        fit$coef = coeff
    } 
    
    # Standard Errors:
    if (method == "ols") { 
        fit$se.coef = fit$asy.se.coef
        n = sqrt(length(as.vector(fit$se.coef)))
        fit$var.coef = matrix(rep(NA, times = n*n), ncol = n) 
    } else { 
        fit$var.coef = fit$asy.var.coef
        fit$se.coef = sqrt(diag(fit$asy.var.coef))  
        if (include.mean) {        
            m = dim(fit$asy.var.coef)[1] + 1
            var.coef = matrix(rep(NA, times = m*m), m, m)
            for ( i in 1:(m-1) ) { 
                for ( j in 1:(m-1) ) {
                    var.coef[i,j] = fit$var.coef[i,j] 
                } 
            }
            fit$var.coef = var.coef
            fit$se.coef = c(fit$se.coef, NA) 
        } 
    }
    
    # Add Data:
    fit$x = x
    
    # Return Value:
    fit 
} 


# ------------------------------------------------------------------------------


.arimaFit =
function (x, order, include.mean, fixed,  
method = c("CSS-ML", "ML", "CSS"), M = NULL, h = NULL, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits an ARIMA time series model
    
    # Note:
    #   Calls arima() from R-stats.
    
    # FUNCTION:
    
    # Fit:
    call = match.call()
    fit = arima(x = x, order = order, method =  method[1], 
        include.mean = include.mean, fixed = fixed, ...) 
    
    # Add Title:
    fit$tstitle = paste("ARIMA(", 
        as.character(order[1]), ",", as.character(order[2]), ",",
        as.character(order[3]), ") with method: ", method[1], sep = "")
        
    # Add Data:
    fit$x = x  
    
    # Add Fitted Values: 
    fit$fitted.values = fit$x - fit$residuals
    
    # Add Standard Errors:
    fit$se.coef = sqrt(diag(fit$var.coef))  
    
    # Add Call:
    fit$call = call
    
    # Return Value:
    fit 
} 


# ------------------------------------------------------------------------------
  

.arfimaFit =
function (x, order, include.mean, fixed, method = "arfima", 
M = 100, h = -1, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits an ARFIMA (FRACDIFF) time series model
    
    # Arguments:
    #   x - time series for the ARIMA model
    #   nar - number of autoregressive parameters
    #   nma - number of moving average parameters
    #   ar - initial autoregressive parameters
    #   ma - initial moving average parameters
    #   dtol - desired accurcay for d, by default (and if 
    #       negative), (4th root of machine precision)
    #       is used.  dtol will be changed internally if 
    #       necessary
    #   drange - interval over which the likelihood function is 
    #       to be maximized as a function of d
    #   h - finite difference interval
    #   M - number of terms in the likelihood approximation
    #       (see Haslett and Raftery 1989) 
    
    # Note:
    #   A Builtin Copy from R's fracdiff Package 
    #   Calls fracdiff() from R-fracdiff
    
    # FUNCTION:
    
    # Settings:
    call = match.call()
    nar = order[1]
    nma = order[2]
    ar = rep(NA, max(order[1], 1))
    ma = rep(NA, max(order[2], 1))
    dtol = .Machine$double.eps^0.25 # ~ 1.22e-4
    drange = c(0, 0.5)  

    # fracdiff:
    if (any(is.na(x)))
        stop("missing values not allowed in time series")
    if (is.matrix(x) && ncol(x) > 2)
        stop("multivariate time series not allowed")
    n = length(x)
    npq = nar + nma
    npq1 = npq + 1
    lwork = max(npq+2*(n+M), 3*n+(n+6)*npq+npq%/%2+1, (3+2*npq1)*npq1+1)
    ar[is.na(ar)] = 0
    ma[is.na(ma)] = 0
    
    # if dtol < 0: the fortran code will choose defaults
    result = .Fortran("fracdf", as.double(x), as.integer(n), 
        as.integer(M), as.integer(nar), as.integer(nma), 
        dtol = as.double(dtol), drange = as.double(drange),
        hood = double(1), d = double(1), ar = as.double(ar), 
        ma = as.double(ma), w = double(lwork), as.integer(lwork), 
        info = integer(1), .Machine$double.xmin, 
        .Machine$double.xmax, .Machine$double.neg.eps,
        .Machine$double.eps, PACKAGE = "fArma")
    if (result$info) switch(result$info,
        stop("insufficient workspace"),
        stop("error in gamma function"),
        stop("invalid MINPACK input"),
        warning(" Warning in gamma function"),
        warning(" Optimization failure"),
        warning(" Optimization limit reached"))
    hess = .Fortran("fdhpq",
         as.double(x), hess = double(npq1 * npq1), as.integer(npq1),
         result$w, PACKAGE = "fArma")$hess
    temp = .Fortran("fdcov", as.double(x), as.double(result$d),
         h = as.double(if (missing(h)) -1 else h), hd = double(npq1),
         cov = hess, as.integer(npq1), cor = hess, as.integer(npq1), 
         se = double(npq1), result$w, info = integer(1), 
         PACKAGE = "fArma")
    if (temp$info) switch(temp$info,
         warning(" Warning in gamma function"),
         warning(" Singular Hessian matrix"),
         warning(" Unable to compute correlation matrix"),
         stop("error in gamma function"))
    if (npq == 0) {
        result$ar = NULL
        result$ma = NULL }
    nam = "d"
    if (nar) nam = c(nam, paste("ar", 1:nar, sep = ""))
    if (nma) nam = c(nam, paste("ma", 1:nma, sep = ""))
    hess = matrix(hess, nrow = npq1, ncol = npq1, dimnames = list(nam, nam))
    hess[1, ] = temp$hd
    hess[row(hess) > col(hess)] = hess[row(hess) < col(hess)]
    se.ok = temp$info != 0 || temp$info < 3
    
    # Fitting Result:
    fit = list(
        log.likelihood = result$hood,
        d = result$d, 
        ar = result$ar, ma = result$ma,
        covariance.dpq = array(temp$cov, c(npq1, npq1), list(nam, nam)), 
        stderror.dpq = if (se.ok) temp$se, # else NULL
        correlation.dpq = if (se.ok) array(temp$cor, c(npq1, npq1)), # else NULL
        h = temp$h, d.tol = result$dtol, M = M, hessian.dpq = hess)
       
    # Add ts Title:
    fit$tstitle = paste("FRACDIFF(", as.character(order[1]), ",", 
        as.character(order[2]), ") with method: ", method[1], sep = "")
    
    # Add Series:
    fit$x = x  
    
    # Add Coefficients: 
    fit$coef = c(fit$d, fit$ar, fit$ma)
    namesCoef = "d"
    if (order[1] > 0) {
        names.ar = c(paste("ar", 1:order[1], sep=""))
        namesCoef = c(namesCoef, names.ar) }
    if (order[2] > 0) {
        names.ma = c(paste("ma", 1:order[2], sep=""))
        namesCoef = c(namesCoef, names.ma) }
    names(fit$coef) = namesCoef
    fit$var.coef = fit$correlation.dpq  
    
    # Add Fitted Values:
    n = 0:fit$M
    w = lgamma(-fit$d+n) - (lgamma(-fit$d)+lgamma(n+1)) 
    w = exp(w)
    fit$fitted.values = filter(fit$x, w, sides = 1) 
    
    # Add Residuals:
    fit$residuals = x - fit$fitted.values
    
    # Add Standard Errors:
    fit$se.coef = fit$stderror.dpq    
    
    # Add fracdiff Parameters:
    fit$fracdiff = c(M, h) 
    
    # Add Call:
    fit$call = call 
    
    # Return Value:
    fit 
}


################################################################################
# PREDICT


predict.fARMA = 
function (object, n.ahead = 10, n.back = 50, conf = c(80, 95), 
doplot = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Predicts from an ARMA Time Series Process
    
    # Example:
    #   x = armaSim(n = 500)
    #   object = armaFit(formula = x ~ arima(2, 0, 1))
    #   predict(object)

    # FUNCTION:
    
    # OX Arfima:
    if (object@call[[1]] == "arfimaOxFit") {
        # .arfimaOxPredict(object, n.ahead = 10, n.back = 50, trace = FALSE)
        ans = .arfimaOxPredict(object, n.ahead, n.back, ...)
        return(ans)
    }
    
    # Predict "ar":
    if (object@fit$tsmodel == "ar") {
        pred = .arPredict(object, n.ahead, se.fit = TRUE, ...)
    }
    
    # Predict "arima":
    if (object@fit$tsmodel == "arima") {
        pred = .arimaPredict(object, n.ahead, se.fit = TRUE, ...)
    }
        
    # Predict "arfima":
    if (object@fit$tsmodel == "arfima") {
        warning(" Prediction for ARFIMA not yet implemented")
        return()
    }

    # Predict "arfima" from Ox:
    if (object@fit$tsmodel == "arfimaOX") {
        pred = .arfimaOxPredict(object, n.ahead, ...)
    }
    
    # Prediction:
    names(pred$pred) = names(pred$se) = NULL
    ans = list(pred = pred$pred, se = pred$se)
    
    # Plot:
    if (doplot) {
         
        # Data:
        data = as.ts(object@data$x) 
        freq = frequency(data)
        start = start(data)
        n = length(data)   
        
        # Fit Slot:
        options(warn = -1)
        fit = object@fit
        class(fit) = fit$class
    
        # Upper and Lower Bands:
        nint = length(conf)
        upper = lower = matrix(NA, ncol = nint, nrow = length(pred$pred))
        for (i in 1:nint) {
            qq = qnorm(0.5 * (1 + conf[i]/100))
            lower[, i] = pred$pred - qq * pred$se
            upper[, i] = pred$pred + qq * pred$se}    
        colnames(lower) = colnames(upper) = paste(conf, "%", sep = "")
            
        # Colors:
        shadecols = switch(1 + (length(conf) > 1), 7, length(conf):1)
        shadepalette = heat.colors(length(conf))
        col = 1
        
        # Plot History:  
        npred = length(pred$pred) 
        ylim = range(c(data[(n-n.back+1):n], pred$pred), na.rm = TRUE)
        ylim = range(ylim, lower, upper, na.rm = TRUE)   
        ylab = paste("Series: ", fit$series)
        vTS = ts(c(data[(n-n.back+1):n], pred$pred[1], rep(NA, npred-1)), 
            end = tsp(data)[2] + npred/freq, f = freq)
        plot(vTS, type = "o", pch = 19, ylim = ylim, ylab = ylab)
        title(main = paste(fit$tstitle)) 
             
        # Confidence Intervals:
        xx = tsp(data)[2] + (1:npred)/freq
        idx = rev(order(conf))
        if (nint > 1) palette(shadepalette)     
        for (i in 1:nint) { polygon(c(xx, rev(xx)), c(lower[, idx[i]], 
            rev(upper[, idx[i]])), col = shadecols[i], border = FALSE) }
        palette("default")
        
        # Mean:
        vTS = ts(pred$pred, start = tsp(data)[2]+1/freq, f = freq)
        lines(vTS, lty = 1, col = 4)
        points(vTS, pch = 19)
       
        # Printout:
        nconf = length(conf)
        out = pred$pred
        upper = as.matrix(upper)
        lower = as.matrix(lower)
        names = "Forecast"
        for (i in nconf:1) {
            out = cbind(out, lower[, i])
            names = c(names, paste("Low", conf[i])) }
        out = cbind(out, pred$pred)
        names = c(names, "Forecast")
        for (i in 1:nconf) {
            out = cbind(out, upper[, i])
            names = c(names, paste("High", conf[i])) }
        out = round(out, digits = 4)[,2:(2*nconf+2)]
        colnames(out) = names[2:(2*nconf+2)]
        
        # Grid:
        grid()
        options(warn = 0)  
        
        # Add to Output:
        ans$out = out
    }
 
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.arPredict = 
function (object, n.ahead = 10, se.fit = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Predict - object@fit$tsmodel = "ar":
    fit = object@fit
    class(fit) = "ar"
    ans = predict(object = fit, newdata = fit$x, 
        n.ahead = n.ahead, se.fit = se.fit)  
        
    # Return Value:
    ans 
}
  

# ------------------------------------------------------------------------------


.arimaPredict = 
function (object, n.ahead = 10, se.fit = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Predict - object@fit$tsmodel = "arima":
    fit = object@fit
    class(fit) = "Arima"
    if (!exists("xreg")) xreg = NULL
    if (!exists("newxreg")) newxreg = NULL
    class(object) = "Arima"
    ans = predict(object = fit, n.ahead = n.ahead, 
        newxreg = newxreg, se.fit = se.fit, xreg = xreg, ...) 
            
    # Return Value:
    ans 
}

    
################################################################################
# PRINT - SUMMARY - PLOT:


show.fARMA = 
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Old S3 print method for a fitted ARMA timeSeries object

    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n ")
    cat(object@title, "\n")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", collapse = "\n"), 
        "\n", sep = "")
       
    # Model: 
    cat("\nModel:\n ", object@fit$tstitle, "\n", sep = "")
    
    # Coefficients:
    cat("\nCoefficient(s):\n")
    digits = max(4, getOption("digits") - 4) 
    print.default(format(object@fit$coef, digits = digits), print.gap = 2, 
        quote = FALSE)
        
    # Description:
    cat("\nDescription:\n ")
    cat(object@description, "\n\n")
        
    # Return Value:
    invisible()
}


setMethod("show", "fARMA", show.fARMA)


# ------------------------------------------------------------------------------


summary.fARMA = 
function (object, doplot = TRUE, which = "all", ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Analyzes a Fitted ARMA timeSeries Object

    # FUNCTION:
        
    # Initialize:
    if (object@fit$tsmodel == "arfima" & doplot) {
        warning(" Plot Method for arfima Models not yet implemented")
        doplot = FALSE
    }
    ans = NULL
    
    # Fit Call and Model:
    x = object
    object = x@fit
    ans$call = object$call
    ans$tsmodel = object$tstitle
    
    # Calculate Residuals and Variance:
    # ans$residuals = na.remove(object$residuals)
    ans$residuals = as.vector(na.omit(object$residuals))
    if (length(ans$residuals) == 0) { 
        ans$var = 0 }
    if (length(ans$residuals) > 0) { 
        ans$var = var(ans$residuals) }
    ans$sigma2 = object$sigma2
    
    # Generate Coef Matrix:
    tval = object$coef/object$se.coef
    prob = 2 * (1 - pnorm(abs(tval)))
    ans$coefmat = cbind(object$coef, object$se.coef, tval, prob)
    dimnames(ans$coefmat) = list(names(object$coef), 
        c(" Estimate", " Std. Error", " t value", "Pr(>|t|)"))
   
    # More Parameters: aic, etc ...
    if (object$tsmodel == "ar") {
        ans$aic = (object$n.used * (1 + log(2 * pi)) + object$n.used * 
            log(ans$var) + 2 * length(object$coef)) }
    if (object$tsmodel == "arma") {
        ans$aic = (object$n.used * (1 + log(2 * pi)) + object$n.used * 
            log(ans$var) + 2 * length(object$coef))
        ans$css = object$css }
    if (object$tsmodel == "arima") {
        ans$aic = object$aic
        ans$loglik = object$loglik }
    if (object$tsmodel == "fracdiff") {
        doplot = FALSE }
    
    # Print Part:
    
    # Title:
    cat("\nTitle:\n ")
    cat(x@title, "\n")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object$call), sep = "\n", collapse = "\n"), 
        "\n", sep = "")
        
    # Model: 
    cat("\nModel:\n ", object$tstitle, "\n", sep = "")
    
    # Coefficients:
    cat("\nCoefficient(s):\n")
    digits = max(4, getOption("digits") - 4) 
    print.default(format(object$coef, digits = digits), print.gap = 2, 
        quote = FALSE)
     
    # Residuals:
    digits = max(4, getOption("digits") - 4)
    if (length(object$residuals) > 2) {
        cat("\nResiduals:\n")
        rq = structure(quantile(ans$residuals), 
            names = c("Min", "1Q", "Median", "3Q", "Max"))
        print(rq, digits = digits)
        # Moments:
        cat("\nMoments: \n")
        skewness = sum((ans$residuals - mean(ans$residuals))^3 /
            sqrt(var(ans$residuals))^3)/length(ans$residuals)
        kurtosis = sum((ans$residuals - mean(ans$residuals))^4 /
            var(ans$residuals)^2)/length(ans$residuals) - 3 
        stats = structure(c(skewness, kurtosis), 
            names = c("Skewness", "Kurtosis"))
        print(stats, digits = digits) }
    
    # Coef Matrix:
    cat("\nCoefficient(s):\n")
    signif.stars = getOption("show.signif.stars")
    printCoefmat(ans$coefmat, digits = digits, 
        signif.stars = signif.stars, ...)
    
    # Fit:
    cat("\n")
    if (x@fit$tsmodel == "ar") {
        cat("sigma^2 estimated as:       ", 
            format(object$var, digits = digits), "\n")
        cat("AIC Criterion:              ", 
            format(round(object$aic, 2)), "\n") }
    if (x@fit$tsmodel == "arma") {
        cat("sigma^2 estimated as:       ", 
            format(object$sigma2, digits = digits), "\n")
        cat("Conditional Sum-of-Squares: ", 
            format(round(object$css, digits=2)), "\n")
        ## cat("AIC Criterion:              ", 
        ##    format(round(object$aic, digits=2)), "\n") 
        }  
    if (x@fit$tsmodel == "arima") {
        cm = object$call$method
        if (is.null(cm) || cm != "CSS")
            cat(
              "sigma^2 estimated as: ", format(object$sigma2, digits = digits),
            "\nlog likelihood:       ", format(round(object$loglik, 2)),
            "\nAIC Criterion:        ", format(round(object$aic, 2)), 
            "\n", sep = "")
        else
            cat(
              "sigma^2 estimated as: ", format(object$sigma2, digits = digits),
            "\npart log likelihood:  ", format(round(object$loglik,2)),
            "\n", sep = "") }
       
    # Doplot:
    if (doplot) plot.fARMA(x, which = which, ...)
    
    # Description:
    cat("\nDescription:\n ")
    cat(x@description, "\n\n")
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


plot.fARMA =
function(x, which = "ask", gof.lag = 10, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class 'fARMA'

    # FUNCTION:

    # Check:
    if (x@fit$tsmodel == "arfima") {
        warning(" Plot method for ARFIMA models not yet implemented")
        return()
    }
    
    # Store Lag:
    x@fit$gof.lag = gof.lag
    
    # Plot:
    .interactiveArmaPlot(
        x,
        choices = c(
            "Standardized Residuals",
            "ACF of Residuals",
            "QQ Plot of Residuals",
            "Ljung-Box p Values"),
        plotFUN = c(
            ".plot.arma.1",  ".plot.arma.2",  ".plot.arma.3", ".plot.arma.4"),
        which = which) 
            
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


.plot.arma.1 <- 
function(x, ...) 
{
    # 1. Standardized Residuals Plot:
    object = x@fit
    rs = as.vector(na.omit(object$residuals))
    stdres = rs/sqrt(object$sigma2)
    plot(stdres, type = "h", 
        main = "Standardized Residuals", 
        ylab = "Residuals", col = "steelblue", ...)
    grid()
    abline(h = 0, col = "grey")
}   
    

# ------------------------------------------------------------------------------


.plot.arma.2 <- 
function(x, ...) 
{
    # 2. ACF of Residuals:
    object = x@fit
    acf(object$residuals, plot = TRUE, main = "ACF of Residuals", 
        na.action = na.pass, ...)
    grid()    
} 


# ------------------------------------------------------------------------------ 


.plot.arma.3 <- 
function(x, ...) 
{           
    # 3. QQ Plot of Residuals:
    object = x@fit
    rs = as.vector(na.omit(object$residuals))
    stdres = rs/sqrt(object$sigma2)
    qqnorm(stdres, 
        xlab = "Normal Quantiles", 
        ylab = "Residual Quantiles", 
        main = "QQ-Plot of Residuals", 
        pch = 19, col = "steelblue", ...)
    qqline(stdres, col = "grey")
    grid()
}  


# ------------------------------------------------------------------------------

         
.plot.arma.4 <- 
function(x, ...) 
{        
    # 4. Ljung-Box p Values:
    object = x@fit
    rs = as.vector(na.omit(object$residuals))
    nlag = x@fit$gof.lag
    pval = numeric(nlag)
    for (i in 1:nlag) 
        pval[i] = Box.test(rs, i, type = "Ljung-Box")$p.value
    plot(1:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0, 1), 
        pch = 19, col = "steelblue", main = "Ljung-Box p-values", ...)
    abline(h = 0.05, lty = 2, col = "grey")
    grid()
}   


# ------------------------------------------------------------------------------

.interactiveArmaPlot = 
function(x, choices = paste("Plot", 1:19), 
plotFUN = paste("plot.", 1:19, sep = ""), which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "template".
    
    # Arguments:
    #   x - an object to be plotted
    #   choices - the character string for the choice menu
    #   plotFUN - the names of the plot functions
    #   which - plot selection, which graph should be 
    #     displayed. If a character string named "ask" the 
    #     user is interactively asked which to plot, if
    #     a logical vector of length N, those plots which
    #     are set "TRUE" are displayed, if a character string
    #     named "all" all plots are displayed.
    
    # Note:
    #   At maximum 19 plots are supported.

    # FUNCTION:
    
    # Some cecks:
    if (length(choices) != length(plotFUN)) 
        stop("Arguments choices and plotFUN must be of same length.")
    if (length(which) > length(choices)) 
        stop("Arguments which has incorrect length.")
    if (length(which) > length(plotFUN)) 
        stop("Arguments which has incorrect length.")
    if (length(choices) > 19)
        stop("Sorry, only 19 plots at max are supported.")
                              
    # Plot:
    if (is.numeric(which)) {
        Which = rep(FALSE, times = length(choices))
        Which[which] = TRUE
        which = Which
    }
    if (which[1] == "all") {
        which = rep(TRUE, times = length(choices))
    }
    if (which[1] == "ask") {
        .multArmaPlot(x, choices, ...) 
    } else {
        for ( i in 1:length(which) ) {
            FUN = match.fun(plotFUN[i])
            if (which[i]) FUN(x) 
        } 
    }
            
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


.multArmaPlot = function (x, choices, ...) 
{    
    # Internal "askPlot" Function:                

    # Match Functions, up to nine ...
    if (length(plotFUN) < 19) plotFUN = 
        c(plotFUN, rep(plotFUN[1], times = 19 - length(plotFUN)))
    plot.1  = match.fun(plotFUN[1]);  plot.2  = match.fun(plotFUN[2]) 
    plot.3  = match.fun(plotFUN[3]);  plot.4  = match.fun(plotFUN[4]) 
    plot.5  = match.fun(plotFUN[5]);  plot.6  = match.fun(plotFUN[6]) 
    plot.7  = match.fun(plotFUN[7]);  plot.8  = match.fun(plotFUN[8]) 
    plot.9  = match.fun(plotFUN[9]);  plot.10 = match.fun(plotFUN[10])
    plot.11 = match.fun(plotFUN[11]); plot.12 = match.fun(plotFUN[12]) 
    plot.13 = match.fun(plotFUN[13]); plot.14 = match.fun(plotFUN[14]) 
    plot.15 = match.fun(plotFUN[15]); plot.16 = match.fun(plotFUN[16]) 
    plot.17 = match.fun(plotFUN[17]); plot.18 = match.fun(plotFUN[18]) 
    plot.19 = match.fun(plotFUN[19])        
    pick = 1
    while (pick > 0) { pick = menu (
        ### choices = paste("plot:", choices),
        choices = paste(" ", choices), 
        title = "\nMake a plot selection (or 0 to exit):")
        # up to 19 plot functions ...
        switch (pick, 
            plot.1(x),  plot.2(x),  plot.3(x),  plot.4(x),  plot.5(x), 
            plot.6(x),  plot.7(x),  plot.8(x),  plot.9(x),  plot.10(x),
            plot.11(x), plot.12(x), plot.13(x), plot.14(x), plot.15(x), 
            plot.16(x), plot.17(x), plot.18(x), plot.19(x)) 
    } 
}


################################################################################


coef.fARMA =
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns coefficients from a fitted ARMA object
    
    # Note:
    #   Alternatively you can use coefficient().

    # FUNCTION:
    
    # Coefficients:
    ans = object@fit$coef
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


fitted.fARMA = 
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns fitted values from a fitted ARMA object

    # FUNCTION:
        
    # Fitted Values:
    ans = object@fitted$fitted
    classAns = class(object@data$x)
    if (classAns == "ts") ans = as.ts(ans)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


residuals.fARMA = 
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns Residuals from a Fitted ARMA Object
       
    # FUNCTION:
    
    # Check:
       
    # Residual Values:
    ans = object@residuals$residuals
    classAns = class(object@data$x)
    if (classAns == "ts") ans = as.ts(ans)
    
    # Return Value:
    ans
}


################################################################################
# FUNCTION:
#  .modelSeries         Models a timeSeries object to use formulas


.modelSeries = 
function(formula, data, fake = FALSE, lhs = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Arguments:
    #   data - a timeSeries, a data.frame or a numeric vector
    
    # Details:
    #   Time Series Modelling
    #   Regression Modelling
    #   Data Management
    
    
    # If no respnonse is pecified:
    if (length(formula) == 2) {
        formula = as.formula(paste("x", formula[1], formula[2], collapse = ""))
        stopifnot(!missing(data))
    }

    # Isf data is missing, take the first data set from the search path:
    if (missing(data)) {
        data = eval(parse(text = search()[2]), parent.frame())
    }
    
    if (is.numeric(data)) {
        data = data.frame(data)
        colnames(data) = all.vars(formula)[1]
        lhs = TRUE
    }
    
    # If we consider a faked formula:
    if (fake) {
        response = as.character(formula)[2]
        Call = as.character(match.call()[[2]][[3]])
        method = Call[1]
        predictors = Call[2]
        formula = as.formula(paste(response, "~", predictors))
    }
    
    # If only left hand side is required:
    if (lhs) {
        response = as.character(formula)[2]
        formula = as.formula(paste(response, "~", 1))
    } 
    
    # Create Model Data:
    x = model.frame(formula, data)
    
    # Convert:
    if (class(data) == "timeSeries") x = timeSeries(x)
    if (fake) attr(x, "control") <- method
    
    # Return value:
    x
    
}


################################################################################

