
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
#   1999 - 2006, Diethelm Wuertz, GPL
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
#  .arPpredict             Internal function called by predict.fARMA
#  .arimaPpredict          Internal function called by predict.fARMA
#  .arfimaPredict          Not yet implemented
# S3 METHOD:              PRINT - PLOT - SUMMARY METHODS:
#  print.fARMA             S3: Prints a fitted ARMA time series object
#  plot.fARMA              S3: Plots stylized facts of a fitted ARMA object
#  summary.fARMA           S3: Summarizes a fitted ARMA time series object
# S3 METHOD:              ADDON METHODS:
#  coef.fARMA              S3: Returns coefficidents from a fitted ARMA object
#  coefficients.fARMA      S3: Synonyme for coef.fARMA
#  fitted.fARMA            S3: Returns fitted values from a fitted ARMA object
#  residuals.fARMA         S3: Returns residuals from a fitted ARMA object
################################################################################

################################################################################
# FUNCTION:               TRUE STATISTICS:
#  armaTrueacf             Returns True ARMA autocorrelation function
#  armaRoots               Returns Roots of the ARMA characteristic polynomial
#  .armaToeplitz           Returns Toeplitz matrix from an ARMA process
#  .armaFischer            Returns ARMA Fischer information matrix
#  .schurTest               Tests invertibility
#  .toeplitzARMA            Computes Toeplitz matrix
#  .iARMA                   Information matrix ARMA
#  .iFARMA                  Information matrix FARMA
#  .psiwtsARMA              MA expansion coefficients
#  .tacvfARMA               True acvf ARMA
#  .tccfAR                  Cross covariances between ARs
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
        residuals = "numeric",
        fitted.values = "numeric",
        predicted.values = "list",
        title = "character",
        description = "character"
    )  
)


# ------------------------------------------------------------------------------


armaSim = 
function(model = list(ar = c(0.5, -0.5), d = 0, ma = 0.1), n = 100,
innov = NULL, n.start = 100, start.innov = NULL, rand.gen = rnorm, 
rseed = NULL, ...) 
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
    
    # Changes:
    #
    
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
            PACKAGE = "fSeries")$x[1:n] 
    }
               
    # Time Series:
    ans = as.ts(x)
    control = c(ar = model$ar, d = model$d, ma = model$ma)
    Names = names(control)
    control = as.character(c(control, substitute(rand.gen)))
    names(control) = c(Names, "rand.gen")
    if (!is.null(rseed)) control = c(control, rseed = rseed)
    attr(ans, "control") = control
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


armaFit = 
function(
formula = x ~ arima(2, 0, 1), method = c("mle", "ols"), 
include.mean = TRUE, fixed = NULL, title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits Model Parameters for an ARMA Time Series Process
    
    # Arguments:
    #   method - "CSS-ML", "ML", "CSS", "yw", "burg1", "burg2", "ols", "mle"
    
    # Notes:
    #   Valid formulas are:
    #       "ar" 
    #       "arma"
    #       "arima"
    #       "fracdiff"
    #   "arma(p,q)" uses arima(p,0,q)
    
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
    #   SPlus:
    #       arima.mle(
    #           x, 
    #           model, 
    #           n.cond, 
    #           xreg=NULL,  
    #           ...) 

    # Example:
    #   x = armaSim(); fit = armaFit(x ~ arima(2, 0, 1)); fit
    
    # Changes:
    #
    
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
    mf <- match.call(expand.dots = TRUE)
    m <- match("h", names(mf), 0)
    if (m == 0) h = -1 else y = eval(mf[[m]])
    m <- match("M", names(mf), 0)
    if (m == 0) M = 100 else M = eval(mf[[m]])
    
    # Check for Formula length:
    formula = as.formula(formula)
    m = length(formula)
    if (m != 3) stop("Formula misspecified")
    
    # Get Series:
    # ts = eval(formula[[2]], + sys.parent())
    # DW 2005-02-04
    x = ts = eval(formula[[2]], + sys.parent())
    
    # Allow for univariate 'timeSeries' Objects:
    # Added 2004-09-04 DW
    if (class(ts) == "timeSeries") ts = as.vector(ts)
    
    # Which Model?
    # DW 2006-02-21
    # regexpr("\\(", as.character(formula[3]))
    end = regexpr("\\(", as.character(formula[3]))-1
    tsmodel =  substr(as.character(formula[3]), 1, end)
    
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
    start = regexpr("\\(", as.character(formula[3]))+1
    end   = regexpr("\\)", as.character(formula[3]))-1
    order = substr(as.character(formula[3]), start, end)
    
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
        method = method[1], fixed = fixed, M = M, h = h, ...)  
     
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
       
    # Return Value:
    new("fARMA",     
        call = as.call(match.call()),
        formula = as.formula(formula), 
        method = as.character(method),
        parameter = list(include.mean = include.mean, fixed = fixed, 
            fracdiff.M = fracdiff.M, fracdiff.h = fracdiff.h),
        data = list(x = x),
        fit = fit,
        residuals = as.vector(fit$residuals),
        fitted.values = as.vector(fit$fitted.values),
        predicted.values = list(),
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
    
    # Changes:
    #
    
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

    # Changes:
    #
    
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
function (x, order, include.mean, fixed, method = "arfima", M = 100, h = -1) 
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
    
    # Changes:
    #
    
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
        .Machine$double.eps, PACKAGE = "fSeries")
    if (result$info) switch(result$info,
        stop("insufficient workspace"),
        stop("error in gamma function"),
        stop("invalid MINPACK input"),
        warning(" Warning in gamma function"),
        warning(" Optimization failure"),
        warning(" Optimization limit reached"))
    hess = .Fortran("fdhpq",
         as.double(x), hess = double(npq1 * npq1), as.integer(npq1),
         result$w, PACKAGE = "fSeries")$hess
    temp = .Fortran("fdcov", as.double(x), as.double(result$d),
         h = as.double(if (missing(h)) -1 else h), hd = double(npq1),
         cov = hess, as.integer(npq1), cor = hess, as.integer(npq1), 
         se = double(npq1), result$w, info = integer(1), 
         PACKAGE = "fSeries")
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
    w = gamma(-fit$d+n)/(gamma(-fit$d)*gamma(n+1)) 
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
  
    # Changes:
    #
    
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

    # Changes:
    #
    
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

    # Changes:
    #
    
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


print.fARMA = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Prints a Fitted ARMA Time Series Object
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Fit:
    object = x@fit
    
    # Title:
    cat("\nTitle:\n ")
    cat(x@title, "\n")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(x@call), sep = "\n", collapse = "\n"), 
        "\n", sep = "")
       
    # Model: 
    cat("\nModel:\n ", object$tstitle, "\n", sep = "")
    
    # Coefficients:
    cat("\nCoefficient(s):\n")
    digits = max(4, getOption("digits") - 4) 
    print.default(format(object$coef, digits = digits), print.gap = 2, 
        quote = FALSE)
        
    # Description:
    cat("\nDescription:\n ")
    cat(x@description, "\n\n")
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


summary.fARMA = 
function (object, doplot = TRUE, which = "all", ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Analyzes a Fitted ARMA Time Series Object
    
    # Changes:
    #
    
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
    cat("Description:\n ")
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
    
    # Changes:
    #
    
    # FUNCTION:

    # Check:
    if (x@fit$tsmodel == "arfima") {
        warning(" Plot method for ARFIMA models not yet implemented")
        return()
    }
    
    # 1. Standardized Residuals Plot:
    plot.1 <<- function(x, ...) {
        object = x@fit
        rs = as.vector(na.omit(object$residuals))
        stdres = rs/sqrt(object$sigma2)
        plot(stdres, type = "h", 
            main = "Standardized Residuals", 
            ylab = "Residuals", col = "steelblue", ...)
        grid()
        abline(h = 0, col = "grey")
    }   
        
    # 2. ACF of Residuals:
    plot.2 <<- function(x, ...) {
        object = x@fit
        acf(object$residuals, plot = TRUE, main = "ACF of Residuals", 
            na.action = na.pass, ...)
        grid()    
    }   
    
    # 3. QQ Plot of Residuals:
    plot.3 <<- function(x, ...) {           
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
             
    # 4. Ljung-Box p Values:
    plot.4 <<- function(x, ...) {        
        object = x@fit
        rs = as.vector(na.omit(object$residuals))
        nlag = gof.lag
        pval = numeric(nlag)
        for (i in 1:nlag) 
            pval[i] = Box.test(rs, i, type = "Ljung-Box")$p.value
        plot(1:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0, 1), 
            pch = 19, col = "steelblue", main = "Ljung-Box p-values", ...)
        abline(h = 0.05, lty = 2, col = "grey")
        grid()
    }   
    
    # Plot:
    .interactiveGarchPlot(
        x,
        choices = c(
            "Standardized Residuals",
            "ACF of Residuals",
            "QQ Plot of Residuals",
            "Ljung-Box p Values"),
        plotFUN = c(
            "plot.1",  "plot.2",  "plot.3", "plot.4"),
        which = which) 
            
    # Return Value:
    invisible(x)
}


################################################################################


coef.fARMA =
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns coefficients from a fitted ARMA object
    
    # Note:
    #   Alternatively you can use coefficient().
    
    # Changes:
    #
    
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
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Check:
    if (object@fit$tsmodel == "arfima") {
        warning (" Fitted method for ARFIMA models not yet implemented")
        return()
    }
        
    # Fitted Values:
    ans = object@fitted.values
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
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Check:
    if (object@fit$tsmodel == "arfima") {
        warning (" Residuals method for ARFIMA models not yet implemented")
        return()
    }
       
    # Residual Values:
    ans = object@fit$residuals
    classAns = class(object@data$x)
    if (classAns == "ts") ans = as.ts(ans)
    
    # Return Value:
    ans
}


################################################################################
# FUNCTION:                 DESCRIPTION:
#  armaTrueacf               True ARMA Autocorrelation Function
#  armaRoots                 Roots of the ARMA Characteristic Polynomial
################################################################################


armaTrueacf = 
function(model, lag.max = 20, type = c("correlation", "partial", "both"), 
doplot = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   A synonyme to ARMAacf

    # Notes:
    #   A synonyme for arma.tacf under R. See R's .First.lib.
    #   Implemented from ARMAacf
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Settings:
    lag = 0:lag.max
    result = NA
    
    # Partial:
    if (type == "partial" || type == "both") {
        main = ylab = "True PACF"
        lag = 1:lag.max
        pacf = ARMAacf(model$ar, model$ma, lag.max = lag.max, pacf = TRUE)
        result = data.frame(cbind(lag, pacf))
        if (doplot) {
            plot(x = lag, y = pacf, type = "n", xlab = "Lag", 
                ylab = ylab, main = main, 
                ylim = c(min(c(pacf, 0)), 1) )
            lines(x = lag, y = pacf, type = "h")
            abline(h = 0)
        }
    }
            
    # Correlation
    if (type == "correlation" || type == "both") {
        main = ylab = "True ACF"
        lag = 0:lag.max
        acf = ARMAacf(model$ar, model$ma, lag.max = lag.max, pacf = FALSE)
        result = data.frame(cbind(lag, acf))
        if (doplot) {
            plot(x=lag, y = acf, type = "n", xlab = "Lag", 
                ylab = ylab, main = main, 
                ylim = c(min(c(acf, 0)), 1) )
            lines(x = lag, y = acf, type = "h")
            abline(h = 0) 
        } 
    }   
            
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


armaRoots = 
function(coefficients, n.plot = 400, digits = 4, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the roots of a characteristc polynomial

    # Changes:
    #
    
    # FUNCTION:
    
    # Algorithm:
    root = polyroot(c(1, -coefficients))
    real.root = Re(root)
    im.root = Im(root)
    xrange = range(real.root)
    xrange = c(xrange[1] - 1.2*abs(xrange[1]), 
        xrange[2]+1.2 * abs(xrange[2]))
    xplot = seq(xrange[1], xrange[2], length = n.plot)
    fpoly = 1
    for(i in 1:length(coefficients)) {
        fpoly = fpoly - xplot^i * coefficients[i] 
    }
    plot(xplot, fpoly, type = "l", xlab = "B", ylab = "Function", 
        col = "steelblue", pch = 19, ...)
    title(main = "Polynomial Function vs. B")
    abline(h = 0)
    distance = sqrt(real.root^2 + im.root^2)
    root.mat = cbind(round(real.root, digits = digits),
        round(im.root, digits = digits), 
        round(distance, digits = digits))
    dimnames(root.mat) = list(1:nrow(root.mat), c("re", "im", "dist"))
    size.limit = max(abs(real.root), 1.5, abs(im.root))
    plot(root, xlim = c( - size.limit, size.limit),
        ylim = c( - size.limit, size.limit), xlab = "", ylab = "", 
        col = "steelblue", pch = 19, ...)
    x = (2*pi/360)*(0:360)
    # symbols(0, 0, circles = 1, add = TRUE, inches = FALSE, col = 6)
    lines(sin(x), cos(x))
    abline(h = 0)
    abline(v = 0)
    title("Roots and Unit Circle", 
        xlab = "Real Part", ylab = "Imaginary Part")
    result = root.mat
        
    # Return Value:
    data.frame(result)
}


################################################################################
# FUNCTION:          DESCRIPTION:
#  .armaToeplitz      Toeplitz Matrix
#  .armaFischer       ARMA Fischer Information matrix
# INTERNAL FUNCTION: DESCRIPTION:
#  .schurTest          Test for invertibility
#  .toeplitzARMA       Toeplitz matrix
#  .iARMA              Information matrix ARMA
#  .iFARMA             Information matrix FARMA
#  .psiwtsARMA         MA expansion coefficients
#  .tacvfARMA          True acvf ARMA
#  .tccfAR             Cross covariances between ARs
################################################################################


.armaToeplitz =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Arguments:
    #   x - a vector of autocovariances. The returned Toeplitz matrix is  
    #       the corresponding covariance matrix of the observatons.
        
    # Changes:
    #
    
    # FUNCTION:
    
    # Wraps:
    .toeplitzARMA(x)
}


# ------------------------------------------------------------------------------


.armaFischer = 
function(model = list(ar = c(0.5, -0.5), ma = 0.1))
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Wraps:
    .iARMA(phi = model$ar, theta = model$ma)
}



# ******************************************************************************


.schurTest = 
function(phi)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tests for invertibility
    
    # Details:
    #   Tests if all roots of 
    #   1 - phi[1] B - ... - phi[length(phi)] B^length(phi) = 0 
    #   are outside the unit circle.
    
    # References:
    #   Pagano M., (1973);
    #       When is an autoregressive process stationary? 
    #       Commun. in Statist. 1, pp. 533-544.
    #   McLeod I., (1974);
    #       Derivation of the Theoretical Autocovariance Function of
    #       Autoregressive-Moving Average Time Series
    #       Appl. Statist. 24, pp. 255--256.
    
    # Author:
    #   Original Version from "iarma" R library: A.I. McLeod, July 1998
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Case 1 - Return Value:
    if (length(phi) == 0) { 
        return(TRUE) 
    }
        
    # Case 2 - Return Value:
    p = length(phi)
    phi = c(-1, phi)
    A = matrix(numeric(p^2), nrow = p, ncol = p)
    for (j in 1:p) {
        for (i in 1:p) {
            if (j > i) {
                A[i, j] = A[j, i] 
            } else {
                k = 1:min(i, j)
                A[i, j] = sum(phi[1 + i - k] * phi[1 + j - k] - 
                    phi[1 + p + k - i] * phi[1 + p + k - j]) 
            } 
        } 
    }
    
    if (dim(A)[1] == attr(chol(A, pivot = TRUE), "rank")) { 
        return(TRUE) 
    }
    
        
    # Case 3 - Return Value:    
    return(FALSE)
}


# ------------------------------------------------------------------------------


.toeplitzARMA =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the Toeplitz Matrix
    
    # Details:
    #   Given a vector x, the Toeplitz matrix is a square matrix of
    #   order length(x) and with [i,j] entries given by x[abs(i-j)].
    #   If x is a vector of autocovariances, the Toeplitz matrix is  
    #   the corresponding covariance matrix of the observatons.
    
    # Author:
    #   Original Version from "iarma" R library: A.I. McLeod, July 1998
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Compute:
    ans = matrix(x[1 + abs(outer(seq(along = x), seq(along = x), 
        FUN = "-"))], byrow = TRUE, ncol = length(x))
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.iARMA = 
function(phi = numeric(0), theta = numeric(0))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the Information Matrix of an ARMA Process
    
    # Author:
    #   Original Version from "iarma" R library: A.I. McLeod, July 1998
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Check:
    if (!(.schurTest(phi) & .schurTest(theta))) {
        cat("Model is non-causal or non-invertible\n")
        return(NULL) 
    }
        
    # Compute:
    p = length(phi)
    q = length(theta)
    unames = vnames = character(0)
    if (p > 0) {
        if (p > 1) {
            vvmatrix = (.tccfAR(phi, phi)[ - (1:(p - 1))])[ - (p + 1)] }
        else if (p == 1) {
            vvmatrix = .tccfAR(phi, phi)[ - (p + 1)] }
        vvmatrix = .toeplitzARMA(vvmatrix)
        imatrix = vvmatrix
        vnames = paste("phi(", 1:p, ")", sep = "") 
    }
    if (q > 0) {
        if (q > 1) {
            uumatrix = (.tccfAR(theta, theta)[ - (1:(q - 1))])[ - ( q + 1)] 
        } else if (q == 1) {
            uumatrix = .tccfAR(theta, theta)[ - (q + 1)] 
        }
        uumatrix = .toeplitzARMA(uumatrix)
        imatrix = uumatrix
        unames = paste("theta(", 1:q, ")", sep = "") 
    }
    if (p > 0 && q > 0) {
        uvmatrix = matrix(numeric(1), nrow = p, ncol = q)
        tuv =  -.tccfAR(phi, theta)
        for (i in 1:p) {
            for (j in 1:q) {
                uvmatrix[i, j] = tuv[q + i - j] 
            } 
        }
        imatrix = cbind(rbind(vvmatrix, t(uvmatrix)), rbind(uvmatrix, 
            uumatrix)) 
    }
    inames = c(vnames, unames)
    dimnames(imatrix) = list(inames, inames)
    
    # Return Value:
    imatrix
}


# ------------------------------------------------------------------------------


.iFARMA = 
function(phi = numeric(0), theta = numeric(0), maxlag = 128)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the Information Matrix of a Fractional ARMA Process
    
    # Author:
    #   Original Version from "iarma" R library: A.I. McLeod, July 1998
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Internal Functions:
    .psiwtsAR = function(phi, maxlag){
        p = length(phi)
        x = numeric(maxlag + 1)
        x = 1
        for (i in 1:p) { 
            x[i + 1] = crossprod(phi[1:i], (rev(x))[1:i]) 
        }
        if (maxlag > p) {
            for (i in (p + 1):maxlag) { 
                x[i + 1] = crossprod(phi, (rev(x))[1:p]) 
            } 
        }
        x 
    }
    
    jFARMA = function(theta, maxlag) {
        psis = psiwtsAR(theta, maxlag = maxlag)
        q = length(theta)
        J = numeric(q)
        for (k in 1:q) { 
            J[k] = sum(psis/(k + 0:maxlag)) 
        }
        J 
    }

    # Check
    if (!(.schurTest(phi) & .schurTest(theta))) {
        cat("Model is non-causal or non-invertible\n")
        return(NULL) 
    }
        
    # Compute:  
    I22 = (pi^2)/6
    if ((length(phi) == 0) && (length(theta) == 0)) return(I22)
    I11 = .iARMA(phi = phi, theta = theta)
    J11 = numeric(0)
    if (length(phi) > 0) J11 = -jFARMA(phi, maxlag)
    J12 = numeric(0)
    if (length(theta) > 0) J12 = jFARMA(theta, maxlag)
    J = c(J11, J12)
    I = rbind(I11, J)
    J = c(J, I22)
    I = cbind(I, J)
    inames = c(dimnames(I11)[[1]], "d")
    dimnames(I) = list(inames, inames)
    
    # Return Value:
    I
}


# ------------------------------------------------------------------------------


.psiwtsARMA = 
function(phi = numeric(0), theta = numeric(0), maxlag = 20)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes MA expansion coefficients
    
    # Author:
    #   Original Version from "iarma" R library: A.I. McLeod, July 1998
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Compute:
    r = max((p = length(phi)), (q = length(theta)))
    phi2 = theta2 = numeric(r)
    maxlagp1 = 1 + maxlag
    if (q > 0) theta2[1:q] = theta
    if (p > 0) phi2[1:p] = phi
    x = numeric(maxlagp1)
    x[1] = 1
    if (r == 0) return(x[1:maxlagp1])
    for (i in 1:r) {
        x[i + 1] = crossprod(phi2[1:i], rev(x[1:i])) - theta2[i] 
    }
    if (p == 0) return(x[1:maxlagp1])
    if (maxlag > r) {
        for (i in (r + 1):maxlag) {
            x[i + 1] = crossprod(phi, x[i - (0:(p - 1))]) 
        } 
    }
    ans = x[1:maxlagp1]
            
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.tacvfARMA = 
function(phi = numeric(0), theta = numeric(0), maxlag = 20)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes theoretical autocovariance function of an ARMA 
    #   Process
    
    # Reference: 
    #   McLeod A.I. (1975);
    #   Derivation of the theoretical autocovariance function of 
    #   autoregressive-moving average models, 
    #   Applied Statistics 24, pp. 255-256. 
    
    # Author:
    #   Original Version from "iarma" R library: A.I. McLeod, July 1998
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Check:
    if (!(.schurTest(phi) & .schurTest(theta))) {
        cat("Model is non-causal or non-invertible\n")
        return(NULL) 
    }
        
    # Compute:
    p = length(phi)
    q = length(theta)
    maxlagp1 = maxlag + 1
    if (max(p, q) == 0) { 
        return(c(1, numeric(maxlagp1))) 
    }
    r = max(p, q) + 1
    b = numeric(r)
    C = numeric(q + 1)
    C[1] = 1
    theta2 = c(-1, theta)
    phi2 = numeric(3 * r)
    phi2[r] = -1
    if (p > 0) { 
        phi2[r + 1:p] = phi 
    }
    if (q > 0) {
        for (k in 1:q) {
            C[k + 1] =  - theta[k]
            if (p > 0) {
                for (i in 1:min(p, k)) {
                  C[k + 1] = C[k + 1] + phi[i] * C[k + 1 - i] 
                } 
            } 
        } 
    }
    for (k in 0:q) {
        for (i in k:q) {
            b[k + 1] = b[k + 1] - theta2[i + 1] * C[i - k + 1] 
        } 
    }
    if (p == 0) {
        g = c(b, numeric(maxlagp1))[1:maxlagp1]
        return(g) 
    } else if (p > 0) {
        a = matrix(numeric(r^2), ncol = r)
        for (i in 1:r) {
            for (j in 1:r) {
                if (j == 1) {
                  a[i, j] = phi2[r + i - 1] 
                } else if (j != 1) {
                  a[i, j] = phi2[r + i - j] + phi2[r + i + j - 2] 
                } 
            } 
        }
        g = solve(a, -b)
        if (length(g) <= maxlag) {
            g = c(g, numeric(maxlag - r))
            for (i in (r + 1):maxlagp1) {
                g[i] = phi %*% g[i - 1:p] 
            }
            return(g) 
        } else if (length(g) >= maxlagp1) {
            return(g[1:maxlagp1]) 
        } 
    }
}


# ------------------------------------------------------------------------------


.tccfAR = 
function(phi, theta)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the theoretical cross-covariance function of two 
    #   autoregressions
    #   z[t] - phi[1] z_[t-1] --- phi[p] z[t-p]     = a[t]
    #   z[t] - theta[1] z_[t-1] --- theta[q] z[t-q] = a[t]
    #   where p, q are length(phi), length(theta)
    
    # Notes:
    #   Auxilary function used with iarma
    
    # Author:
    #   Original Version from "iarma" R library: A.I. McLeod, July 1998
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Compute:
    p = length(phi)
    q = length(theta)
    if (p == 0 || q == 0) {
        return(numeric(0))
    }
    k = p + q
    rhs = c(-1, rep(0, k - 1))
    A = matrix(numeric(k^2), nrow = k, ncol = k)
    for (i in 1:k) {
        for (j in 1:k) {
            imj = i - j
            ijq = i + j - q - 1
            if (i > q) {
                if (i > j && imj <= q) {
                    A[i, j] = theta[imj]
                } else if (i > q && imj == 0) A[i, j] = -1 
            } else {
                if (ijq > 0 && ijq <= p) {
                    A[i, j] = phi[ijq]
                } else if (ijq == 0) 
                    A[i, j] = -1 
            }
        } 
    } 
    ans = solve(A, rhs)
                
    # Return Value:
    ans
}


################################################################################

