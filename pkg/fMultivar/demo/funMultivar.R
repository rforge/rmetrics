
#
# Example:
# 	fSeries Functions Addons
#
# Description:
#	This is a collection of frunctions which support the following
#	tasks:
#    1 Aparch Time Series Simulation
#    2 Chaotic Time Series Maps
#    3 Absolute Moment Statistics
#    4 Garch Ox Interface
#    ** 5 Missing Data Manipulations **
#    6 OLS Functions
#    ** 7 Moving Averages **
#    8 Time Series Filter
#    9 Additional Trading Indicators
#   10 Lagged or leading vector/matrix of selected order(s)
#   11 Regressor matrix for polynomial distributed lags
#   12 Time series disaggregation from low to high frequency
#
# Author:
#   (C) 2002, Diethelm Wuertz, GPL
#


################################################################################
# 1 APARCH TIME SERIES SIMULATION


################################################################################
# FUNCTION:       DESCRIPTION:
#  .aparchSim      Simulate an APARCH time series process 
################################################################################


.aparchSim = 
function(model = list(omega = 1e-6, alpha = 0.1, gamma = 0, alpha.lags = 1,
beta = 0.8, beta.lags = 1, delta = 1), n = 1000, innov = NULL, n.start = 100, 
start.innov = NULL, rand.gen = rnorm, ...)
{   # A function implemented by D. Wuertz   

    # Description:
    #   Simulate an APARCH time series process
    
    # Notes:
    #   This function is written completely in R without the needs of 
    #   a compiled external function.
    
    # FUNCTION:
    
    # Innovations:      
    if (is.null(innov)) innov = rand.gen(n, ...)
    if (is.null(start.innov)) start.innov = rand.gen(n.start, ...)  
    x = z = c(start.innov, innov)
    h = 0 * x 
     
    # Order maxpq:
    deltainv = 1/model$delta    
    maxpq = max( 
        model$alpha.lags[length(model$alpha.lags)], 
        model$beta.lags[length(model$beta.lags)] )
    
    # Simulate:
    for (n in (maxpq+1):length(z)) {     
        h[n] = model$omega +    
            sum(model$alpha*(abs(abs(x[n-model$alpha.lags]) - 
                model$gamma*x[n-model$alpha.lags])^model$delta)) +
            sum(model$beta*h[n-model$beta.lags]) 
        x[n] = h[n]**deltainv * z[n] }
        
    # Return Value:
    as.ts(x[(length(start.innov)+1):length(z)])
}


################################################################################
# 2 CHAOTIC TIME SERIES MAPS


################################################################################
# FUNCTION:       DESCRIPTION:
#  henonSim        Simulate data from Henon Map 
#  ikedaSim        Simulate data from Ikeda Map
#  logisticSim     Simulate data from Logistic Map
#  lorentzSim      Simulate data from Lorentz Map
#  roesslerSim     Simulate data from Roessler Map
################################################################################


henonSim = 
function(n = 1000, n.skip = 100, a = 1.4, b = 0.3, x.start = runif(1), 
y.start = runif(1), doplot = FALSE)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Simulate Data from Henon Map
    
    # Details:
    #   Creates iterates of the Henon map:
    #   *   x(n+1)  =  1 - a*x(n)^2 + b*y(n)
    #   *   y(n+1)  =  x(n)

    # Argument List:
    #   Name:   Description:
    #   n       number of points x, y
    #   n.skip  number of transients discarded
    #   a       parameter a
    #   b       parameter b
    #   x       initial x
    #   y       initial y
    
    # Copyright and License:
    #   Diethelm Wuertz, GPL
    
    # FUNCTION:
    
    # Simulate Map: 
    x = rep(0, times = (n+n.skip))
    y = rep(0, times = (n+n.skip))
    x[1] = x.start
    y[1] = y.start
    for ( i in 2:(n+n.skip) ) {
        x[i]  =  1 - a*x[i-1]^2 + b*y[i-1]
        y[i]  =  x[i-1] }
    x = x[(n.skip+1):(n.skip+n)] 
    y = y[(n.skip+1):(n.skip+n)] 

    # Plot Map:
    if (doplot) {
        plot(x = x, y = y, type = "n", xlab = "x[n]", ylab = "y[n]", 
        main = "Henon Map")
        points(x = x, y = y, col = "red", cex = 0.25) }
    
    # Return Value:
    data.frame(x, y)    
}


# ------------------------------------------------------------------------------


ikedaSim = 
function(n = 1000, n.skip = 100, a = 0.4, b = 6.0, c = 0.9, x = runif(1), 
y = runif(1), doplot = FALSE)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Simulate Ikeda Map Data
    
    # Details:
    #   Prints iterates of the Ikeda map (Re(z) and Im(z)): 
    #                                        i*b
    #   z(n+1)  =  1 + c*z(n)* exp( i*a - ------------ )
    #                                     1 + |z(n)|^2

    # Argument List:
    #   Name:   Description:
    #   n       number of points z
    #   n.skip  number of transients discarded
    #   a       parameter a
    #   b       parameter b; 6.0 
    #   c       parameter c; 0.9 
    #   x       initial Re(z); runif(1)
    #   y       initial Im(z); runif(1)
    
    # Copyright and License:
    #   Diethelm Wuertz, GPL
    
    # FUNCTION:
    
    # Simulate Map: 
    a = complex(real = 0, imag = a)
    b = complex(real = 0, imag = b)
    z = rep(complex(real = x, imag = y), times = (n+n.skip))
    for ( i in 2:(n+n.skip) ) {
        z[i] = 1 + c*z[i-1] * exp(a-b/(1+abs(z[i-1])^2)) }
    z = z[(n.skip+1):(n.skip+n)] 
    
    # Plot Map:
    if (doplot) {
        x = Re(z); y = Im(z)
        plot(x, y, type = "n", xlab = "x[n]", ylab = "y[n]", 
            main = "Ikeda Map")
        points(x, y, col = "red", cex = 0.25)
        x = Re(z)[1:(length(z)-1)]; y = Re(z)[2:length(z)]
        plot(x, y, type = "n", xlab = "x[n]", ylab = "x[n+1]", 
            main = "Ikeda Map")
        points(x, y, col = "red", cex = 0.25) }
    
    # Return Value:
    data.frame(Re(z), Im(z), col.names = c("x", "y"))   
}
 

# ------------------------------------------------------------------------------


logisticSim = 
function(n = 1000, n.skip = 100, r = 4, x.start = runif(1), doplot = FALSE)
{   # A function written by Diethelm Wuertz
    
    # Description:
    #   Simulate Data from Logistic Map
    
    # Details:
    #   Creates iterates of the Logistic Map:
    #   *   x(n+1)  =  r * x[n] * ( 1 - x[n] )

    # Argument List:
    #   Name:   Description:
    #   n       number of points x, y
    #   n.skip  number of transients discarded
    #   r       parameter r
    #   x.start initial x
    
    # Copyright and License:
    #   Diethelm Wuertz, GPL
    
    # FUNCTION:
    
    # Simulate Map: 
    x = rep(0, times = (n+n.skip))
    x[1] = x.start
    for ( i in 2:(n+n.skip) ) {
        x[i]  =  r * x[i-1] * ( 1 - x[i-1] ) }
    x = x[(n.skip+1):(n.skip+n)] 

    # Plot Map:
    if (doplot) {
        plot(x = x[1:(n-1)], y = x[2:n], type = "n", xlab = "x[n-1]", 
            ylab = "x[n]", main = "Logistic Map")
        points(x = x[1:(n-1)], y = x[2:n], col = "red", cex = 0.25) }
    
    # Return Value:
    data.frame(x)   
}
    
                       
# ------------------------------------------------------------------------------


lorentzSim = 
function(times = seq(0, 40, by = 0.01), parms = c(sigma = 16, r = 45.92, b = 4),
start = c(-14, -13, 47), doplot = TRUE, ...)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Simulates a Lorentz Map
    
    # Notes:
    #   Requires contributed R package "odesolve"
    
    # Copyright and License:
    #   Diethelm Wuertz, GPL
   
    # FUNCTION:
    
    # Requirements:
    require(odesolve)
    
    # Attractor:
    lorentz = function(t, x, parms) {
        X = x[1]; Y = x[2]; Z = x[3] 
        with(as.list(parms), {
            dX = sigma * ( Y - X )
            dY = -X*Z + r*X - Y
            dZ = X*Y - b*Z
            list(c(dX, dY, dZ))}) }

    # Classical RK4 with fixed time step:
    s = rk4(start, times, lorentz, parms)

    # Display:
    if (doplot) {
        xylab = c("x", "y", "z", "x")
        for (i in 2:4) plot(s[, 1], s[, i], type = "l", 
            xlab = "t", ylab = xylab[i-1], main =  "Roessler", ...)
        k = c(3, 4, 2)
        for (i in 2:4) plot(s[, i], s[, k[i-1]], type = "l", 
            xlab = xylab[i-1], ylab = xylab[i], main =  "Roessler", ...) }
        
    # Return Value:
    invisible(s)
}


# ------------------------------------------------------------------------------


roesslerSim = 
function(times = seq(0, 100, by = 0.01), parms = c(a = 0.2, b = 0.2, c = 8.0),
start = c(-1.894, -9.920, 0.0250), doplot = TRUE, ...)
{   # A function written by Diethelm Wuertz
    
    # Description:
    #   Simulates a Lorentz Map
    
    # Notes:
    #   Requires contributed R package "odesolve"
    
    # Copyright and License:
    #   Diethelm Wuertz, GPL
   
    # FUNCTION:
    
    # Attractor:
    roessler = function(t, x, parms) {
        X = x[1]; Y = x[2]; Z = x[3] 
        with(as.list(parms), {
            dX = -(Y+Z)
            dY = X + a*Y
            dZ = b + X*Z -c*Z
            list(c(dX, dY, dZ))}) }

    # Classical RK4 with fixed time step:
    s = rk4(start, times, roessler, parms)

    # Display:
    if (doplot) {
        xylab = c("x", "y", "z", "x")
        for (i in 2:4) plot(s[, 1], s[, i], type = "l", 
            xlab = "t", ylab = xylab[i-1], main =  "Roessler", ...)
        k = c(3, 4, 2)
        for (i in 2:4) plot(s[, i], s[, k[i-1]], type = "l", 
            xlab = xylab[i-1], ylab = xylab[i], main =  "Roessler", ...) }
        
    # Return Value:
    invisible(s)
}


################################################################################
# 3 ABSOLUTE MOMENT STATISTICS


################################################################################
# FUNCTION:       DESCRIPTION:
#  absMoments      Absolute moments of a standardized symmetric distribution
################################################################################


absMoments =
function(n, density, ...)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the absolute moments of a standardized
    #   symmetric distribution function.
    
    # Arguments:
    #   n - a vector of integers i, to compute M_i
    #   density  - a character string denoting the density
    #       "norm", "ged", "std" or any other
    #   ... - optional parameters to be passed 
    
    # Value:
    #   Returns a numeric vector of moments M_i.
    #   Stores globally errors in the variable absMoment.error
    #     if the moments were comuted numerically.
    
    # FUNCTION:
              
    # norm - Normal Distribution:
    if (density == "dnorm") {
        return (sqrt(2)^n * gamma((n+1)/2) / sqrt(pi)) }

    # ged - Generalized Error Distribution:
    if (density == "dged") {
        parm = function(n, nu) {
            lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
            mu2r = (2^(1/nu)*lambda)^n * gamma((n+1)/nu) / gamma(1/nu)
            return(mu2r) }
        return(parm(n, ...)) }

    # std - Student-t Distribution:
    # Note: nu > 2*n
    if (density == "dstd") {
        parm = function(n, nu) {
            r = n/2
            mu2r = ((nu-2)^r/2) * beta((r+1)/2, (nu-r)/2) / beta(1/2, nu/2)
            return(mu2r) }
        return(parm(n, ...)) }

    # Any other standardized symmetric Distribution ...
    fun = match.fun(density)
    moments = function(x, n, ...) { 2 * x^n * fun(x, ...) }
    M = absMoments.error <<- NULL
    for (i in n) {
        I = integrate(moments, 0, Inf, n=i, ...)
        M = c(M, I$value)
        absMoments.error <<- c(absMoments.error, I$abs.error) }
        return(M)

    # Return Value:
    invisible()
}


################################################################################
# 4 GARCH OX INTERFACE


################################################################################
# FUNCTION:            DESCRIPTION:
#  garchOxFit           Fits parameters of a garch model           
#  print.garchOx        S3 Print Method
#  plot.garchOx         S3 Plot Method
################################################################################


garchOxFit = 
function(formula.mean = ~ arma(0, 0), formula.var = ~ garch(1, 1), 
series = x, cond.dist = c("gaussian", "t", "ged", "skewed-t"), 
include.mean = TRUE, truncation = 100, trace = TRUE)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fits a time series by a ARMA-GARCH model interfacing Ox.
    
    # Arguments:
    #   formula.mean -
    #
    #   formula.var - 
    #
    #   series -
    #
    #   cond.dist - a character string describing the distribution of 
    #       innovations. By default the optimization is based on gaussian 
    #       log likelihood parameter optimization denoted by "gaussian". 
    #       Alternatively, a Student "t", "ged", or "skewed-t" can be chosen.    
    #   include mean -
    #   
    #   truncation -
    #
    #   trace - a logical. Trace optimizer output? 
    #       By default TRUE.
   
    # Value:
    #   returns a list of class fGARCH with at least 
    #   the following components:
    #
    #   order- the order of the fitted model.
    #   coef - estimated GARCH coefficients for the fitted model.
    #   n.likeli - the negative log-likelihood function evaluated 
    #       at the coefficient estimates (apart from some constant).
    #   n.used - the number of observations of "x".
    #   residuals - the series of residuals.
    #   fitted.values - the bivariate series of conditional standard
    #       deviation predictions for "x".
    #   series - the name of the series "x".
    #   frequency - the frequency of the series "x".
    #   call - the call of the \code{garch} function.}
    #   asy.se.coef - the asymptotic-theory standard errors of the
    #       coefficient estimates.
    
    # FUNCTION:
    
    # Fit:
    fit = list()
    fit$x = series
    
    # Include Constants:
    include.var = TRUE
    fit$csts = c(include.mean, include.var) 
    
    # Select Distribution:
    # 0 : Gaussian
    # 1 : Student-t
    # 2 : GED
    # 3 : Skewed-Student-t
    distris = 0:3
    names(distris) = c("gaussian", "t", "ged", "skewed-t")
    distri = distris[cond.dist[1]]
    fit$cond.dist = cond.dist[1]
    
    # Determine ARMA Order:
    if (missing(formula.mean)) {
        # if missing use ARMA(0, 0) ...
        fit$formula.mean = ~ arma(0, 0)
        fit$arma.orders = c(0, 0) }
    else {
        # otherwise determine orders "u" and "v" ...
        fit$arma.orders = as.numeric(strsplit(strsplit(strsplit(as.character(
            formula.mean), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]]) }    
    
    # ARFIMA wanted?
    arfima = FALSE
    fit$arfima = as.integer(arfima)
    
    # Determine GARCH Order:        
    if (missing(formula.var)) {
        # if missing use GARCH(1, 1) ...
        fit$formula.var = ~ garch(1, 1)
        fit$garch.orders = c(1, 1) }
    else {
        # otherwise determine orders "p" and "q" ...
        fit$garch.orders = as.numeric(strsplit(strsplit(strsplit(as.character(
            formula.var), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]]) } 
    
    # ARCH-IN-MEAN?
    arch.in.mean = 0
    fit$arch.in.mean = arch.in.mean
    
    # Selected Model:
    models = 1:11
    names(models) = c("garch", "egarch", "gjr", "aparch", "igarch", 
        "figarch.bbm", "figarch.chung", "fiegarch", "fiaparch.bbm", 
        "fiaparch.chung", "hygarch")
    selected = strsplit(as.character(formula.var), "\\(")[[2]][1]
    fit$model = models[selected]    
    
    # Length of Time Series:
    nt = length(series)
    
    # Temporary File:
    ident = paste(selected, as.character(floor(runif(1)*10000)), sep="")
                                            
    # Write parameters to file - OxParameter.txt:
    parameters = c(fit$csts, distri, fit$arma.orders, fit$arfima, 
        fit$garch.orders, fit$model, fit$arch.in.mean, truncation, nt)  
    write(x=parameters, file="OxParameter.txt") 
    
    # Write data to file - OxSeries:
    write(x="X", file="OxSeries.csv", ncolumns=1)
    write(x, file="OxSeries.csv", ncolumns=1, append=TRUE)                      
    
    # Calculate:    
    fit$ox = system(OXCMD, show.output.on.console = trace, invisible = TRUE)
    fit$model = selected
    fit$call = match.call()
    fit$residuals = scan("OxResiduals.csv", skip = 1, quiet = TRUE)
    fit$condvars = scan("OxCondVars.csv", skip = 1, quiet = TRUE)
    fit$coef = matrix(scan("OxParameters.csv", skip = 1, quiet = TRUE), 
        byrow = TRUE, ncol = 3)
    
    # Return Value:
    class(fit) = "garchOx"
    invisible(fit)
}


# ------------------------------------------------------------------------------


print.garchOx = 
function(object, digits = max(3, getOption("digits") - 3), ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Print method for an object of class "garchOx".
    
    # FUNCTION:
    
    # Check object:
    if (!inherits(object, "garchOx")) 
        stop("method is only for garchOx objects")
    
    # Function Call:
    cat("\nCall:\n")
    cat(paste(deparse(object$call), sep = "\n", collapse = "\n"), 
        "\n", sep = "")
    
    # Mean Equation:
    cat("\nMean Equation: ~arma(", object$arma.orders[1], ", ",
        object$arma.orders[2], ")\n", sep = "")
    
    # Conditional Variance Equation:
    cat("\nConditional Variance Equation: ~", object$model, "(", 
        object$garch.orders[1], ", ", object$garch.orders[2], ")\n", 
        sep = "")
        
    # Conditional Distribution:
    cat("\nConditional Distribution: ", object$cond.dist, "\n", 
    sep = "")
        
    # Coefficients:
    cat("\nCoefficient(s):\n")
    Value = object$coef[,1]
    Std.Error = object$coef[,2]
    t.value = object$coef[,3]
    coef.names = "Cst(M)"
    if(object$arfima == 1) coef.names = c(coef.names, "d-arfima")
    
    if (object$arma.orders[1] > 0) {
        for (i in 1:object$arma.orders[1])
        coef.names = c(coef.names, paste("AR(", as.character(i), ")", 
            sep=""))}
    if (object$arma.orders[2] > 0) {
        for (i in 1:object$arma.orders[2])
        coef.names = c(coef.names, paste("MA(", as.character(i), ")", 
            sep=""))}
    coef.names = c(coef.names, "Cst(V)")
    if (object$garch.orders[1] > 0) {
        for (i in 1:object$garch.orders[1])
        coef.names = c(coef.names, paste("ARCH(", as.character(i), ")", 
            sep=""))}
    if (object$garch.orders[2] > 0) {
        for (i in 1:object$garch.orders[2])
        coef.names = c(coef.names, paste("GARCH(", as.character(i), ")", 
            sep=""))}            
    if(object$cond.dist == "t") coef.names = c(coef.names, "Student(DF)")
    if(object$cond.dist == "ged") coef.names = c(coef.names, "GED(DF)")
    if(object$cond.dist == "skewed-t") {
        coef.names = c(coef.names, "Asymmetry", "Tail")  }
    if (object$arch.in.mean == 1) {
        coef.names = c(coef.names, "ARCH-in-mean(var)") }   
    coef = data.frame(cbind(Value, Std.Error, t.value), row.names=coef.names)
    print(coef)     
    cat("\n")
    
    # Return Value:
    invisible(object)
}


# ------------------------------------------------------------------------------


plot.garchOx = 
function(object) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "garchOx".
    
    # FUNCTION:
    
    # Check Object:
    if (!inherits(object, "garchOx")) 
        stop("method is only for garchOx objects")
        
    # Plot Time Series"
    plot(object$x, type = "l", main = "Time Series")
    
    # Conditional Variances:
    plot(object$condvars, type = "l", main = "Conditional Variances")
    
    # Autocorrelation Functions: 
    acf(object$x)
    acf(object$x^2)
  
}


################################################################################
# 6 OLS FUNCTIONS


################################################################################
# FUNCTION:       DESCRIPTION:
#  OLS            Fit an OLS regression model
#   print.OLS      S3 Print method for an OLS regression model
#   plot.OLS       S3 Plot method for an OLS regression model
#   summary.OLS    S3 Summary method for an OLS regression model
################################################################################


OLS = 
function(formula, data, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   OLS Estimator
    
    # FUNCTION:
    
    # Estimate:
    fit = lm(formula = formula, data = data, ...)
    fit$call = match.call()
    fit$formula = formula
    fit$data<- data
    
    # Return Value:
    class(fit) = "OLS"
    fit 
}

# ------------------------------------------------------------------------------


print.OLS = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Print Method
    
    # FUNCTION:
    
    # Print:
    class(object) = "lm"
    print.lm(object, ...) 
}
    

# ------------------------------------------------------------------------------


plot.OLS = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Plot Method
    
    # FUNCTION:
    
    # Plot:
    class(object) = "lm"
    plot.lm(object, ...)
}


# ------------------------------------------------------------------------------

    
summary.OLS = function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Summary Method
    
    # FUNCTION:
    
    # Summary:
    class(object) = "lm"
    summary.lm(object, ...) 
}


################################################################################
# 8 TIME SERIES FILTER


################################################################################
# FUNCTION:       DESCRIPTION:
#  hpFilter        Decompose a series using the Hodrick-Prescott filter
################################################################################


hpFilter = 
function(y, lambda = 1600) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the Hodrick-Prescott decomposition of a macroeconomic time 
    #   series into a smooth trend component and a cyclical component. 
    
    # Arguments:
    #   x - a vector, or a "timeSeries" object of the original data. 
    #       Usually this is represented on a logarithmic scale.
    #   lambda - a positive number representing the smoothness 
    #       parameter. 
    
    # Details:
    #   A larger number for the smoothness parameter "lambda" results 
    #   in more smoothing. A rule of thumb is to use 100 for annual, 
    #   1600 for quarterly, and 14400 for monthly data. However, Ravn  
    #   and Uhlig (2002) recently proposed a different smoothing 
    #   parameter of 6.25 for yearly data.
    #   This function is implemented along the recommendations given
    #   in Posch [2002].
        
    # Value: 
    #   a vector, or a "timeSeries" with the same length as x, representing 
    #   the smooth trend component. The cyclical (business cycle) component 
    #   is the difference between the original data and the trend component. 

    # References:
    #   Hodrick R.J., Prescott E.C. (1997); 
    #       Postwar U.S. business cycles: an empirical investigation. 
    #       Journal of Money, Credit and Banking, 29:1--16. 
    #   Kydland F.E., Prescott, E.C. (1990); 
    #       Business cycles: real facts and a monetary myth. 
    #       Federal Reserve Bank of Minneapolis Quarterly Review, 
    #       Spring(1990):3--18.
    #   Posch O. (2002);
    #       The HP Filter and its R implementation
    #       University of Dresden, October 02, 2002.
    #   Ravn M.O., Uhlig, H. (2002);
    #       On Adjusting the Hodrick-Prescott Filter
    #       for the Frequency of Observations,
    #       Review of Economics and Statistics, 84, 371--76.
    
    # FUNCTION:
    
    # Number of observations:
    n = length(y) 
    # Creates an identity matrix:
    I = diag(n) 
    # Second order differences:
    D = diff(I, lag=1, d=2) 
    # Solve focs:
    result = solve(I + lambda * crossprod(D) , y) 
    
    # Return Value:
    result 
}


################################################################################
# PASTECS FILTER:
#  decaverage
#  deccensus
#  decdiff
#  decevf
#  decloess
#  decmedian
#  decreg



################################################################################
# 9 ADDITINAL TRADING INDICATORS


################################################################################
# FUNCTION:            DESCRIPTION:
#  accelTA              Acceleration
#  adiTA                AD Indicator      
#  adoscillatorTA       AD Oscillator
#  bollingerTA          Bollinger Bands
#  chaikinoTA           Chaikin Oscillator
#  chaikinvTA           Chaikin Volatility
#  garmanKlassTA        Garman-Klass Volatility
#  macdTA               MACD
#  medpriceTA           Median Price
#  momentumTA           Momentum
#  nviTA                Negative Volume Index
#  obvTA                On Balance Volume
#  pviTA                Positive Volume Index
#  pvtrendTA            Price-Volume Trend
#  rocTA                RateOfChange
#  rsiTA                Relative Strength Index
#  stochasticTA         Stochastic Oscillator
#  typicalPriceTA       Typical Price
#  wcloseTA             Weighted Close
#  williamsadTA         Williams AD
#  williamsrTA          Williams R%
################################################################################


accelTA = 
function(x, n = 12, trim = TRUE) 
{
    # FUNCTION:
    
    # Indicator:
    accel = diff( x[(n+1):length(x)]-x[1:(length(x)-n)] )  
    if (!trim) accel = c(rep(NA, n+1), accel)
    
    # Return Value:
    accel 
}   


# ------------------------------------------------------------------------------
    
    
adiTA = 
function(high, low, close, volume) 
{
    # FUNCTION:
    
    # Indicator:
    adi = cumsum((2 * close - high - low) / (high - low) * volume) 
    
    # Return Value:
    adi 
}
    

# ------------------------------------------------------------------------------

    
adoscillatorTA = 
function(open, high, low, close) 
{
    # FUNCTION:
    
    # Indicator:
    adoscillator = (high - open + close - low) / (high - low) * 50 
    
    # Return Value:
    adoscillator 
}
    

# ------------------------------------------------------------------------------

    
bollingerTA = 
function(x, n = 20, n.sd = 2, trim = TRUE, na.rm = FALSE) 
{
    # FUNCTION:
    
    # Indicator:
    mean = c(rep(NA, n-1), SMA(x = x, n = n))
    std = c(rep(NA, n-1), n.sd*sqrt(rollVar(x = x, n = n)))
    bollinger = as.matrix(cbind(upper = mean+std, price = x, 
        lower = mean-std))
    if (trim){ 
        bollinger = bollinger[n:length(x),] }
    else { 
        rownames(bollinger) = as.character(1:length(x)) }
    
    # Return Value:
    bollinger 
}
    

# ------------------------------------------------------------------------------


chaikinoTA = 
function(high, low, close, volume, n.long = 10, n.short = 3, 
start = "average", na.rm = NULL) 
{
    # FUNCTION:
    
    # Indicator:
    adi = TA.adi(high, low, close, volume)
    chaikino = EWMA(adi, n.short, start = start, na.rm = na.rm) - 
        EWMA(adi, n.long, start=start, na.rm = na.rm) 
    
    # Return Value:
    chaikino 
}
    

# ------------------------------------------------------------------------------

    
chaikinvTA = 
function(high, low, n.range = 10, n.change = 10, trim = TRUE, 
start = "average", na.rm = FALSE) 
{
    # FUNCTION:
    
    # Indicator:
    rt = EWMA(high-low, n.range, start = start, na.rm = na.rm)
    chaikinv = (rt[-(1:n.change)]/rt[1:(length(rt)-n.change)]-1)*100
    if (!trim) chaikinv = c(rep(NA, n), chaikinv)
    
    # Return Value:
    chaikinv 
}   
    

# ------------------------------------------------------------------------------

        
garmanKlassTA = 
function(open, high, low, close, trim = TRUE) 
{
    # FUNCTION:
    
    # Indicator:
    prices = log(cbind(open, high, low, close))
    n = nrow(prices); alpha = 0.12; f = 0.192
    u = high-open; d = low-open; cc = close - open
    oc = (prices[2:n, 1] - prices[1:(n - 1), 4])^2
    garmanKlass = 0.511*(u-d)^2 - 0.019*(cc*(u+d) - 2*u*d) - 0.383*cc^2
    garmanKlass = sqrt(((1 - alpha)*garmanKlass[2:n])/(1-f) + (alpha*oc)/f)
    if (!trim) garmanKlass = c(NA, garmanKlass)
    
    # Return Value:
    garmanKlass 
}
    

# ------------------------------------------------------------------------------

    
macdTA = 
function(x, n.short = 12, n.long = 26, n.signal = 9, start = "average", 
na.rm = NULL) 
{
    # FUNCTION:
    
    # Indicator:
    MACD = EWMA(x, n.short, start=start, na.rm=na.rm) - 
        EWMA(x, n.long, start=start, na.rm=na.rm)
    signal = EWMA(MACD, n.signal, start=start, na.rm = na.rm)
    macd = cbind(macd = MACD, signal = signal) 
    rownames(macd) = as.character(1:length(x))
    
    # Return Value:
    macd 
}
    

# ------------------------------------------------------------------------------

    
medpriceTA = 
function(high, low) 
{
    # FUNCTION:
    
    # Indicator:
    medprice = (high + low) / 2 
    
    # Return Value:
    medprice 
}
    


# ------------------------------------------------------------------------------

    
momentumTA = 
function(x, n = 12, trim = TRUE) 
{
    # FUNCTION:
    
    # Indicator:
    m = length(x)
    momentum = x[(n+1):m] - x[1:(m-n)] 
    if (!trim) momentum = c(rep(NA, n), momentum)
    
    # Return Value:
    momentum 
}
    
    
# ------------------------------------------------------------------------------


nviTA = 
function(close, volume) 
{
    # FUNCTION:
    
    # Indicator:
    ind = rep(0, length(close)-1)
    ind[diff(volume) < 0] = 1
    ch = c(0, TA.roc(close, n = 1, trim = TRUE)/100) 
    nvi = cumsum(ch * c(0, ind)) 
    
    # Return Value:
    nvi 
}
    
    

# ------------------------------------------------------------------------------


obvTA = 
function(close, volume) 
{
    # FUNCTION:
    
    # Indicator:
    obv = cumsum(volume * c(0, sign(diff(close))))
    
    # Return Value:
    obv 
}
    


# ------------------------------------------------------------------------------

    
pviTA = 
function(close, volume) 
{
    # FUNCTION:
    
    # Indicator:
    ind = rep(0, length(close)-1)
    ind[diff(volume) > 0] = 1
    ch = c(0, TA.roc(close, n = 1, trim = TRUE)/100)
    pvi = cumsum(ch * c(0, ind))
    
    # Return Value:
    pvi 
}
    

# ------------------------------------------------------------------------------


pvtrendTA = 
function(close, volume) 
{
    # FUNCTION:
    
    # Indicator:
    m = length(close)   
    ch = cumsum( volume * c(0, (close[2:m]/close[1:(m-1)]-1)*100)) 
    
    # Return Value:
    ch 
}
    


# ------------------------------------------------------------------------------

    
rocTA = 
function(x, n = 12, trim = TRUE) 
{
    # FUNCTION:
    
    # Indicator:
    m = length(x)
    roc = (x[(n+1):m]/x[1:(m-n)]-1)*100 
    if (!trim) roc = c(rep(NA, n), roc)
    
    # Return Value:
    roc 
}
    


# ------------------------------------------------------------------------------

    
rsiTA = 
function(x, n = 14, simple = TRUE, trim = TRUE, start = "average", 
na.rm = NULL) 
{
    # FUNCTION:
    
    # Indicator:
    dx = diff(x)
    if (simple) r = apply(cbind(pmax(dx, 0), -pmin(dx, 0)), 2, SMA, n=n)
    else r = apply(cbind(pmax(dx, 0), -pmin(dx, 0)), 2, EWMA, n=n)
    rsi = as.vector(100*(1-1/(1 + r[,1]/r[,2])))
    if (simple) { names(rsi) = as.character((n+1):length(x)) }
    else { names(rsi) = as.character(2:length(x)) }
    if (!trim) {
        if (simple) rsi = c(rep(NA,n),rsi) else rsi = c(NA,rsi)
        names(rsi) = as.character(1:length(x))}
    
    # Return Value:
    rsi 
}   
    
   

# ------------------------------------------------------------------------------

 
stochasticTA = 
function(high, low, close, n.k = 10, n.d = 3, type = "slow", 
trim = TRUE, na.rm = FALSE) 
{
    # FUNCTION:
    
    # Indicator:
    hh = rollMax(high, n.k, trim = TRUE, na.rm = na.rm)
    ll = rollMin(low, n.k, trim = TRUE, na.rm = na.rm)
    K = (close[ - (1:(n.k - 1))] - ll)/(hh - ll) * 100
    D = SMA(K, n.d, trim = TRUE, na.rm = na.rm)
    type = casefold(type)
    if (type == "slow") {
        K = SMA(K, n.d, trim = TRUE, na.rm = na.rm)
        D = SMA(D, n.d, trim = TRUE, na.rm = na.rm)}
    if (trim) {
        K = K[ - (1:(n.d - 1))]
        stochastic = cbind(K = K, D = D) }
    else {
        K = c(rep(NA, n.k - 1), K)
        D = c(rep(NA, n.k + n.d - 2), D)
        if (type == "slow") {
            K = c(rep(NA, n.d - 1), K)
            D = c(rep(NA, n.d - 1), D)}
        stochastic = cbind(K = K, D = D) }
    
    # Return Value:
    stochastic 
}
    

# ------------------------------------------------------------------------------

    
typicalPriceTA = 
function(high, low, close) 
{
    # FUNCTION:
    
    # Indicator:
    typicalPrice = (high + low + close)/3 
    names(tyicalPrice) = as.character(1:length(x))
    
    # Return Value:
    typicalPrice 
}
    

# ------------------------------------------------------------------------------

    
wcloseTA = 
function(high, low, close) 
{
    # FUNCTION:
    
    # Indicator:
    wclose = (high + low + 2 * close)/4 
    names(wclose) = as.character(1:length(x))
    
    # Return Value:
    wclose 
}
    

# ------------------------------------------------------------------------------

    
williamsadTA = 
function(high, low, close) 
{
    # FUNCTION:
    
    # Indicator:
    ind = c(0, sign(diff(close)))
    williamsad = vector("numeric", length(close))
    ind.pos = (ind == 1)
    ind.neg = (ind == -1)
    williamsad[ind.pos] = (close - low)[ind.pos]
    williamsad[ind.neg] =  - (high - close)[ind.neg]
    williamsad = cumsum(williamsad) 
    names(williamsad) = as.character(1:length(x))
    
    # Return Value:
    williamsad 
}
    


# ------------------------------------------------------------------------------

    
williamsrTA = 
function(high, low, close, n = 20, trim = TRUE, na.rm = FALSE) 
{
    # FUNCTION:
    
    # Indicator:
    hh = rollMax(high, n, trim = FALSE, na.rm = na.rm)
    ll = rollMin(low, n, trim = FALSE, na.rm = na.rm)
    williamsr = (hh-close)/(hh-ll)*100 
    names(williamsr) = as.character(1:length(x))
    if (trim) williamsr = williamsr[n:length(high)]
    
    # Return Value:
    williamsr 
}           
        
    
################################################################################    
# 10 TSLAG


################################################################################
# FUNCTION:     DESCRIPTION:
#  tslag         Creates a lagged or leading vector/matrix of selected order(s)
################################################################################


tslag = 
function(x, k = 1, trim = FALSE)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Creates a lagged or leading vector/matrix of selected order(s).
	
	# Arguments:
	#	x - a vector of data, missing values (NA) are allowed. 
	#	k - the number of positions the new series is to lag 
	#		or to lead the input series. 
	#	trim - a logical flag, if TRUE, the missing values at the 
	#		beginning or end of the returned series will be 
	#		trimmed. The default value is FALSE. 
	
	# Details:
	#	With a positive value of "k" we get a lagged series and with
	#	a negative value we get a leading series. 
	
	# Examples:
	#	tslag(rnorm(10), 2)
	#	tslag(rnorm(10), -2:2)

	# FUNCTION:
	
	# Internal Function:
	tslag1 = function(x, k) {
		y = x
		if (k > 0) y = c(rep(NA, times = k), x[1:(length(x)-k)])
		if (k < 0) y = c(x[(-k+1):length(x)], rep(NA, times = -k))
		y }
		
	# Bind:
	ans = NULL
	for ( i in k) {
		ans = cbind(ans, tslag1(x, i)) }
		
	# Trim:
	if (trim) {
		indexes = (1:length(ans[,1]))[!is.na(apply(ans, 1, sum))]
		ans = ans[indexes, ] }
		
	# As Vector:
	if (length(k) == 1) ans = as.vector(ans)
	
	# Return Value:
	ans
}



################################################################################    
# 11 PDL


################################################################################
# FUNCTION:     DESCRIPTION:
#  pdl           Creates regressor matrix for polynomial distributed lags
################################################################################


pdl = 
function(x, d = 2, q = 3, trim = FALSE)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Creates regressor matrix for polynomial distributed lags.
	
	# Arguments:
	#	x - a numeric vector.
	#	d - an integer specifying the order of the polynomial. 
	# 	q - an integer specifying the number of lags to use in 
	#		creating polynomial distributed lags. This must be 
	#		greater than d. 
	#	trim - a logical flag; if TRUE, the missing values at 
	#		the beginning of the returned matrix will be trimmed. 

	# Value:
	#	Returns a matrix representing the regressor matrix. 

	
	# FUNCTION:

	# Polynomial distributed lags:
	M = tslag(x, 1:q, FALSE)
	C = NULL
	for (i in 0:d) { C = rbind(C, (1:q)^i) }
	Z = NULL
	for (i in 1:(d+1)) { Z = cbind(Z, apply(t(C[i,]*t(M)), 1, sum)) }
	Z[, 1] = Z[, 1] + x
	
	# Trim:
	if (trim) {
		indexes = (1:length(Z[,1]))[!is.na(apply(Z, 1, sum))]
		Z = Z[indexes, ] }

	# Return Value:
	Z
}


################################################################################
# 12 DISSAGGREGATE


################################################################################
# FUNCTION:      DESCRIPTION:
#  disaggregate   Disaggregates time series from low to high frequency
################################################################################


disaggregate = 
function(data, k, 
method = c("linear", "constant", "fmm", "spline", "natural", "periodic"), 
how = NA, x = NA, out.positions = NA, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Time series disaggregation/distribution from low frequency
	# 	to high frequency.
	
	# Arguments:
	#	data - a vector, or a matrix, or a "timeSeries" object 
	#	 	that represents the low frequency time series to 
	#		be disaggregated. 
	# 	k - a positive integer specifying the number of time 
	#		periods to distribute data into. For example, to 
	#	   	disaggregate an annual series into a quarterly 
	#     	series, you set k to 4. 

	# Value:
	#	Returns a vector, or a matrix, or a "timeSeries" object that 
	#	represents the disaggregated high frequency time series. 

	# FUNCTION:
	
	# Method:
	method = method[1]
	
	# Data
	x = 1:length(data)
	y = as.vector(data)
	xout = seq(1, length(data), length=k*length(data))
	
	# "linear" / "constant" Interpolation:	
	if (method == "linear" || method == "constant") 
		ans = approx(x = x, y = y, xout, method = "linear")
	
	# "fmm" / "natural" / "periodic" Spline interpolation:
	if (method == "spline") method = "fmm"
	if (method == "fmm" | method == "natural" | method == "periodic") 
		ans = spline(x = x, y = y, n = k*length(x), method = "fmm",
            xmin = min(x), xmax = max(x))	
	
	# Return Value:
	ans
}


################################################################################

