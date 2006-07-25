
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
# FUNCTION:            DESCRIPTION:    
#  'fDISTFIT'           S4 Class representation
#  print.fDISTFIT       Prints Results from a Fitted Distribution
# FUNCTION:            NORMAL DISTRIBUTION:
#  .normFit             Fits parameters of a Normal density
# FUNCTION:            STUDENT DISTRIBUTION:
#  tFit                 Fits parameters of a Student-t density
# FUNCTION:            STABLE DISTRIBUTION:
#  stableFit            Fits parameters of a stable density
#  .phiStable            Creates contour table for McCulloch estimators
#  .qStableFit           Estimates stable parameters by McCulloch approach
#  .mleStableFit         Estimates stable parameters by MLE approach
# FUNCTION:            GENERALIZED DISTRIBUTION:
#  ghFit                Fits parameters of a generalized hyperbolic density
#  hypFit               Fits parameters of a hyperbolic density
#  nigFit               Fits parameters of a normal inverse Gaussian density
# FUNCTION:            SMOOTHING SPLINE ANOVA:
#  ssdFit               Fits probability densities using smoothing spline ANOVA
#   print.ssd           S3 Print Method        
#  .ssden               Internal function required by ssdFit
#  .ssden.mkterm.cubic1 Internal function required by ssdFit
#  .ssden.mkphi.cubic   Internal function required by ssdFit
#  .ssden.mkrk.cubic    Internal function required by ssdFit
#  .ssden.gauss.quad    Internal function required by ssdFit 
#  .ssden.sspdsty       Internal function required by ssdFit
#  .ssden.nlm0          Internal function required by ssdFit
# FUNCTION:            UTILITY FUNCTIONS FOR SPLUS VERSION:
#  much.fun             S-PLUS: R substitute for 'much.fun'
#  which.min            S-PLUS: R substitute for 'which.min'
#  which.max            S-PLUS: R substitute for 'which.max'
################################################################################


################################################################################   
#  'fDISTFIT'           S4 Class Representation
#  print.fDISTFIT       Prints Results from a Fitted Distribution


setClass("fDISTFIT", 
    representation(
        call = "call",
        model = "character",
        data = "data.frame",
        fit = "list",
        title = "character",
        description = "character"
    )  
)


# ------------------------------------------------------------------------------


print.fDISTFIT =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Prints Results from a Fitted Distribution
    
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
    cat("\nModel:\n ", x@model, "\n", sep = "")
    
    # Estimate:
    cat("\nEstimated Parameter(s):\n")
    print(x@fit$estimate)
        
    # Description:
    cat("\nDescription:\n ")
    cat(x@description, "\n\n")
        
    # Return Value:
    invisible()
}




################################################################################
#  .normFit             Fits parameters of a Normal density


.normFit = 
function(x, doplot = TRUE, span = "auto", title = NULL, 
description = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Return Maximum log-likelihood estimated
    #   Paramters for Normal Distribution
      
    # Notes:
    #   Function Calls: nlminb(), density() 
    #   The function normFit() can be found in the Rmetrics
    #       chapter GarchDistributions.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x.orig = x
    x = as.vector(x)
    
    # Settings:
    CALL = match.call()
    
    # MLE:
    N = length(x)
    mean = sum(x)/N
    sd = sqrt(sum((x-mean)^2)/N) 

    # Optional Plot:
    if (doplot) {
        if (span == "auto") {
            span.min = qnorm(0.001, mean, sd)
            span.max = qnorm(0.999, mean, sd)
            span = seq(span.min, span.max, length = 100)  
        }
        par(err = -1)
        z = density(x, n = 100, ...)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dnorm(span, mean, sd)
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]),  
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)", ...)
        title("NORMAL: Parameter Estimation")
        lines(x = span, y = log(y.points), col = "steelblue4") 
        if (exists("grid")) grid()  
    }
    
    # Add Title and Description:
    if (is.null(title)) title = "Student-t Parameter Estimation"
    if (is.null(description)) description = as.character(date())
        
    # Fit:
    fit = list(estimate = c(mean = mean, sd = sd)) 
        
    # Return Value:
    new("fDISTFIT",     
        call = as.call(CALL),
        model = "Student-t Distribution",
        data = as.data.frame(x.orig),
        fit = fit,
        title = as.character(title), 
        description = as.character(description) )
}


################################################################################
#  tFit                 Fits parameters of a Student-t density


tFit = 
function(x, df = 4, doplot = TRUE, span = "auto", trace = FALSE, 
title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Return Maximum log-likelihood estimated
    #   Paramters for Student-t Distribution:
      
    # Note:
    #   Function Calls: nlminb(), density() 
    
    # Example:
    #   tFit(rt(1000, df=4))
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x.orig = x
    x = as.vector(x)
    
    # Settings:
    CALL = match.call()
    .trace <<- trace
    steps <<- 0
    
    # Log-likelihood Function:
    etmle = function(x, y = x) { 
        # Prevent from negative df's
        if (x[1] <= 0) x[1] = x.save
        f = -sum(log(dt(y, x[1])))
        # Print Iteration Path:
        steps <<- steps + 1
        if (.trace) {
            cat("\n Optimization Step:         ", steps)
            cat("\n Objective Function Value:  ", -f)
            cat("\n Students df Estimate:      ", x[1], "\n") 
        }
        x.save <<- x[1]
        f 
    }
        
    # Minimization:
    r = nlm(f = etmle, p = c(df), y = x)
    
    # Optional Plot:
    if (doplot) {
        if (span == "auto") {
            df = r$estimate[1]
            span.min = qt(0.001, df)
            span.max = qt(0.999, df)
            span = seq(span.min, span.max, length = 100)  
        }
        par(err = -1)
        z = density(x, n = 100, ...)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dt(span, df = r$estimate[1])
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]), 
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)", ...)
        title("STUDENT-T: Parameter Estimation")
        lines(x = span, y = log(y.points), col = "steelblue4") 
        if (exists("grid")) grid()  
    }
    
    # Add Title and Description:
    if (is.null(title)) title = "Student-t Parameter Estimation"
    if (is.null(description)) description = as.character(date())
        
    # Fit:
    fit = list(estimate = c(df = r$estimate), minimum = -r$minimum, 
        code = r$code, gradient = r$gradient, steps = steps) 
        
    # Return Value:
    new("fDISTFIT",     
        call = as.call(CALL),
        model = "Student-t Distribution",
        data = as.data.frame(x.orig),
        fit = fit,
        title = as.character(title), 
        description = as.character(description) )
}


################################################################################
#  stableFit            Fits parameters of a stable density
#  .phiStable            Creates contour table for McCulloch estimators
#  .qStableFit           Estimates stable parameters by McCulloch approach
#  .mleStableFit         Estimates stable parameters by MLE approach


stableFit = 
function(x, alpha = 1.75, beta = 0, gamma = 1, delta = 0, 
type = c("q", "mle"), doplot = TRUE, trace = FALSE, 
title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz 

    # Description 
    #   Stable Parameter Estimation
    
    # Changes:
    #
    
    # FUNCTION:

    # Start Values: Use Quantile Method:
    ans = .qStableFit(x, doplot, title, description)
    
    # Continue with MLE Approach:
    if (type[1] == "mle") {
        Alpha = ans@fit$estimate[1]
        Beta  = ans@fit$estimate[2]
        Gamma = ans@fit$estimate[3]
        Delta = ans@fit$estimate[4]
        if (is.na(Alpha)) Alpha = alpha
        if (is.na(Beta)) Beta = beta
        if (is.na(Gamma)) Gamma = gamma
        if (is.na(Delta)) Delta = delta
        ans = .mleStableFit(x, Alpha, Beta, Gamma, Delta, doplot, 
            trace, title, description)
    }
            
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.phiStable =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates contour table for McCulloch estimators
    
    # Note:
    #   Stable Distribution - delta=1 and gamma=0 fixed!
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Settings:
    alpha = c(seq(0.50, 1.95, by = 0.1), 1.95, 1.99)
    beta = c(-0.95, seq(-0.90, 0.90, by = 0.10), 0.95)
    m = length(alpha)
    n = length(beta)

    # phi1:
    phi1 = function(alpha, beta)
    {
        ( qstable(0.95, alpha, beta) - qstable(0.05, alpha, beta) ) / 
        ( qstable(0.75, alpha, beta) - qstable(0.25, alpha, beta) )
    }

    # phi2:
    phi2 = function(alpha, beta)
    {
        ( ( qstable(0.95, alpha, beta) - qstable(0.50, alpha, beta) ) -
        ( qstable(0.50, alpha, beta) - qstable(0.05, alpha, beta) ) ) /   
        ( qstable(0.95, alpha, beta) - qstable(0.05, alpha, beta) )
    }

    # Phi:
    Phi1 = Phi2 = matrix(rep(0, n*m), ncol = n)
    for ( i in 1:m ) {
        for ( j in 1:n ) {
            Phi1[i,j] = phi1(alpha[i], beta[j])
            Phi2[i,j] = phi2(alpha[i], beta[j])
            print( c(alpha[i], beta[j], Phi1[i,j], Phi2[i,j]) ) 
        } 
    }
        
    #Plot:
    contour(alpha, beta, Phi1, levels = c(2.5, 3, 5, 10, 20, 40), 
        xlab = "alpha", ylab = "beta", labcex = 1.5, xlim = c(0.5, 2.0))
    contour(alpha, beta, Phi2, levels = c(-0.8, -0.6, -0.4, -0.2, 0,
        0.2, 0.4, 0.6, 0.8), col = "red",  labcex = 1.5, add = TRUE)
    
    # Result:
    .PhiStable = list(Phi1 = Phi1, Phi2 = Phi2, alpha = alpha, beta = beta)
    
    # Dump:
    if (FALSE) dump(".PhiStable", "PhiStable.R")

    # Return Value:
    .PhiStable
}   


# ------------------------------------------------------------------------------


.qStableFit =
function(x, doplot = TRUE, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Estimates stable parameters by McCulloch approach
    
    # Note:
    #   This implementation assumes delta=1 and gamma=0
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Settings:
    CALL = match.call()
    
    # Load Contour Table:
    data(PhiStable)
    Phi1 = .PhiStable$Phi1
    Phi2 = .PhiStable$Phi2
    alpha = .PhiStable$alpha
    beta = .PhiStable$beta
    
    # Estimate phi:
    r = sort(x)
    N = length(r)
    q95 = r[round(0.95*N)]
    q75 = r[round(0.75*N)]
    q50 = r[round(0.50*N)]
    q25 = r[round(0.25*N)]
    q05 = r[round(0.05*N)]
    phi1 = max( (q95-q05) / (q75-q25), 2.4389 )
    phi2 = ((q95-q50)-(q50-q05)) / (q95-q05)
    # print(c(min(Phi1), phi1, max(Phi1)))
    # print(c(min(Phi2), phi2, max(Phi2)))
    
    # Plot:
    if (doplot) {
        contour(alpha, beta, Phi1, levels = c(2.5, 3, 5, 10, 20, 40), 
            xlab = "alpha", ylab = "beta", xlim = c(0.5, 2.0))
        contour(alpha, beta, Phi2, levels = c(-0.8, -0.6, -0.4, -0.2,
            0.2, 0.4, 0.6, 0.8), col = "red", add = TRUE)
        lines(c(0.5, 2), c(0, 0), col = "red")
        contour(alpha, beta, Phi1, levels = phi1, add = TRUE, lty = 3,
            col = "blue")
        contour(alpha, beta, Phi2, levels = phi2, add = TRUE, lty = 3,
            col = "blue")
        title(main = "Stable Quantiles")
    }
    
    # Extract Estimate from Contours, if possible:
    u = contourLines(alpha, beta, Phi1, levels = phi1)
    Len = length(u)
    if( Len > 0) {
        u = u[[1]][-1]
        v = contourLines(alpha, beta, Phi2, levels = phi2)
        # print("v")
        # print(v)
        v = v[[1]][-1]  
        xout = seq(min(v$y), max(v$y), length = 200)
        z = approx(v$y, v$x, xout = xout)$y - approx(u$y, u$x, xout = xout)$y
        index = which.min(abs(z))       
        V = round(xout[index], 3)
        U = round(approx(v$y, v$x, xout = xout[index])$y, 3)    
        if (doplot) points(U, V, pch = 19, cex = 1)
    } else {
        U = V = NA
    }
    
    # Add Title and Description:
    if (is.null(title)) title = "Stable Parameter Estimation"
    if (is.null(description)) description = as.character(date())
    
    if (is.na(U) | is.na(V)) {
        GAM = NA
    } else {
        phi3 = qstable(0.75, U, V) - qstable(0.25, U, V)
        GAM = (q75-q25) / phi3  
    }
    
    if (is.na(U) | is.na(V)) {
        DELTA = NA
    } else {
        phi4 = -qstable(0.50, U, V) + V*tan(pi*U/2)
        DELTA = phi4*GAM - V*GAM*tan(pi*U/2) + q50
    }
        
    # Fit:
    fit = list(estimate = c(alpha = U, beta = V, gamma = GAM, delta = DELTA)) 
    
    # Return Value:
    new("fDISTFIT",     
        call = as.call(CALL),
        model = "Student-t Distribution",
        data = as.data.frame(x),
        fit = fit,
        title = as.character(title), 
        description = as.character(description) )
}


# ------------------------------------------------------------------------------


.mleStableFit = 
function(x, alpha = 1.75, beta = 0, gamma = 1, delta = 0, doplot = TRUE, 
trace = FALSE, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Estimates stable parameters by MLE approach
    
    # Note:
    #   This implementation assumes delta=1 and gamma=0
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x.orig = x
    x = as.vector(x)
    
    # Settings:
    CALL = match.call()
    steps <<- 0
    
    # Log-likelihood Function:
    establemle = function(x, y = x) { 
        alpha = 2/(1+exp(-x[1]))
        beta = tanh(x[2])
        gamma = x[3]
        delta = x[4]
        f = -sum(log(dstable(y, alpha = alpha, beta = beta,
            gamma = gamma, delta = delta)))
        # Print Iteration Path:
        steps <<- steps + 1
        if (.trace) {
            cat("\n Optimization Step:         ", steps)
            cat("\n Objective Function Value:  ", -f)
            cat("\n Stable Estimate:           ", alpha, beta, gamma, delta)
            cat("\n") 
        }
        f 
    }
        
    # Minimization:
    r = nlm(f = establemle, p = c(log(alpha/(2-alpha)), atanh(beta),
        gamma, delta), y = x)
    alpha = 2/(1+exp(-r$estimate[1]))
    beta = tanh(r$estimate[2])
    gamma = r$estimate[3]
    delta = r$estimate[4]
            
    # Optional Plot:
    if (doplot) {
        span.min = qstable(0.01, alpha, beta)
        span.max = qstable(0.99, alpha, beta)
        span = seq(span.min, span.max, length = 100)  
        par(err = -1)
        z = density(x, n = 100)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dstable(span, alpha, beta)
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]), 
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)")
        title("STUDENT-T: Parameter Estimation")
        lines(x = span, y = log(y.points), col = "steelblue4") 
        if (exists("grid")) grid()  
    }
    
    # Add Title and Description:
    if (is.null(title)) title = "Stable Parameter Estimation"
    if (is.null(description)) description = as.character(date())
        
    # Fit:
    fit = list(estimate = c(alpha = alpha, beta = beta, gamma = gamma, 
        delta = delta), minimum = -r$minimum, code = r$code, gradient = 
        r$gradient, steps = steps) 
    
    # Return Value:
    new("fDISTFIT",     
        call = as.call(CALL),
        model = "Student-t Distribution",
        data = as.data.frame(x.orig),
        fit = fit,
        title = as.character(title), 
        description = as.character(description) )
}


################################################################################
#  ghFit                Fits parameters of a generalized hyperbolic density
#  hypFit               Fits parameters of a hyperbolic density
#  nigFit               Fits parameters of a normal inverse Gaussian density


ghFit = 
function(x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1, doplot = TRUE, 
span = "auto", trace = FALSE, title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fits parameters of a generalized hyperbolic density

    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x.orig = x
    x = as.vector(x)
    
    # Settings:
    CALL = match.call()
    .trace <<- trace
    steps <<- 0

    # Log-likelihood Function:
    eghmle = function(x, y = x){ 
        # alpha and delta must be positive ...
        alpha = exp(-x[1])            # alpha >= 0
        beta = alpha * tanh(x[2])     # abs(beta) <= alpha
        delta = exp(-x[3])            # delta >= 0
        mu = x[4]
        lambda = x[5]
        if (alpha <= 0) return(Inf)  
        if (delta <= 0) return(Inf)
        if (abs(beta) >= alpha) return(Inf)
        f = -sum(log(dgh(y, alpha, beta, delta, mu, lambda)))
        # Print Iteration Path:
        steps <<- steps + 1
        if (.trace) {
            cat("\n Optimization Step:         ", steps)
            cat("\n Objective Function Value:  ", -f)
            cat("\n Parameter Estimates:       ", 
            alpha, beta, delta, mu, lambda, "\n") 
        }
        f 
    }
        
    # Variable Transformation and Minimization:
    r = nlm(f = eghmle, 
        p = c(-log(alpha), atanh(beta/alpha), -log(delta), mu, lambda), y = x)  
    r$estimate[1] = exp(-r$estimate[1])
    r$estimate[2] = r$estimate[1] * tanh(r$estimate[2])     
    r$estimate[3] = exp(-r$estimate[3])

    # Optional Plot:
    if (doplot) {
        if (span == "auto") {
            alpha = r$estimate[1]
            beta = r$estimate[2]
            delta = r$estimate[3]
            mu = r$estimate[4]
            lambda = r$estimate[5]
            span.min = qgh(0.001, alpha, beta, delta, mu, lambda)
            span.max = qgh(0.999, alpha, beta, delta, mu, lambda)
            span = seq(span.min, span.max, length = 100)  
        }
        par(err = -1)
        z = density(x, n = 100, ...)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dgh(span, 
            alpha = r$estimate[1], 
            beta = r$estimate[2], 
            delta = r$estimate[3], 
            mu = r$estimate[4],
            lambda = r$estimate[5])
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]), 
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)", ...)
        title("HYP: Parameter Estimation")    
        lines(x = span, y = log(y.points), col = "steelblue4")
        if (exists("grid")) grid()  
    }
    
    # Add Title and Description:
    if (is.null(title)) title = "Generalized Hyperbolic Parameter Estimation"
    if (is.null(description)) description = as.character(date())
        
    # Fit:
    fit = list(estimate = c(alpha = r$estimate[1], beta = r$estimate[2],
        delta = r$estimate[3], mu = r$estimate[4], lambda = r$estimate[5]), 
        minimum = -r$minimum, code = r$code, gradient = r$gradient, 
        steps = steps)
        
    # Return Value:
    new("fDISTFIT",     
        call = as.call(CALL),
        model = "Generalized Hyperbolic Distribution",
        data = as.data.frame(x.orig),
        fit = fit,
        title = as.character(title), 
        description = as.character(description) )
}


# ------------------------------------------------------------------------------


hypFit = 
function(x, alpha = 1, beta = 0, delta = 1, mu = 0, doplot = TRUE, 
span = "auto", trace = FALSE, title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fits parameters of a hyperbolic density 

    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x.orig = x
    x = as.vector(x)
    
    # Settings:
    CALL = match.call()
    .trace <<- TRUE
    steps <<- 0
    
    # Log-likelihood Function:
    ehypmle = function(x, y = x){ 
        # alpha and delta must be positive ...
        alpha = exp(-x[1])            # alpha >= 0
        beta = alpha * tanh(x[2])     # abs(beta) <= alpha
        delta = exp(-x[3])            # delta >= 0
        mu = x[4]
        lambda = x[5]
        # if (alpha <= 0) return(Inf)  
        # if (delta <= 0) return(Inf)
        # if (abs(beta) >= alpha) return(Inf)
        f = -sum(log(dhyp(y, alpha, beta, delta, mu)))
        # Print Iteration Path:
        steps <<- steps + 1
        if (.trace) {
            cat("\n Optimization Step:         ", steps)
            cat("\n Objective Function Value:  ", -f)
            cat("\n Parameter Estimates:       ", alpha, beta, delta, mu, "\n") 
        }
        f 
    }
        
    # Variable Transformation and Minimization:
    r = nlm(f = ehypmle, 
        p = c(-log(alpha), atanh(beta/alpha), -log(delta), mu), y = x)  
    r$estimate[1] = exp(-r$estimate[1])
    r$estimate[2] = r$estimate[1] * tanh(r$estimate[2])
    r$estimate[3] = exp(-r$estimate[3])
        
    # Optional Plot:
    if (doplot) {
        if (span == "auto") {
            alpha = r$estimate[1]
            beta = r$estimate[2]
            delta = r$estimate[3]
            mu = r$estimate[4]
            span.min = qhyp(0.01, alpha, beta, delta, mu)
            span.max = qhyp(0.99, alpha, beta, delta, mu)
            span = seq(span.min, span.max, length = 100)  
        }
        par(err = -1)
        z = density(x, n = 100, ...)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dhyp(span, 
            alpha = r$estimate[1], 
            beta = r$estimate[2], 
            delta = r$estimate[3], 
            mu = r$estimate[4])
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]), 
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)", ...)
        title("HYP: Parameter Estimation")    
        lines(x = span, y = log(y.points), col = "steelblue4")
        if (exists("grid")) grid()  
    }
    
    # Add Title and Description:
    if (is.null(title)) title = "Hyperbolic Parameter Estimation"
    if (is.null(description)) description = as.character(date())
        
    # Fit:
    fit = list(estimate = c(alpha = r$estimate[1], beta = r$estimate[2],
        delta = r$estimate[3], mu = r$estimate[4]), minimum = -r$minimum, 
        code = r$code, gradient = r$gradient, steps = steps)
        
    # Return Value:
    new("fDISTFIT",     
        call = as.call(CALL),
        model = "Hyperbolic Distribution",
        data = as.data.frame(x.orig),
        fit = fit,
        title = as.character(title), 
        description = as.character(description) )
}


# ------------------------------------------------------------------------------


nigFit = 
function(x, alpha = 1, beta = 0, delta = 1, mu = 0, doplot = TRUE, 
span = "auto", trace = FALSE, title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fits parameters of a normal inverse Gaussian density
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Transform:
    x.orig = x
    x = as.vector(x)
    
    # Settings:
    CALL = match.call()
    .trace <<- trace
    steps <<- 0
    
    # Log-likelihood Function:
    enigmle = function(x, y = x) { 
        if (x[1] <= 0) return(Inf)  
        if (x[3] <= 0) return(Inf)
        if (abs(x[2]) >= x[1]) return(Inf)
        f = -sum(log(dnig(y, x[1], x[2], x[3], x[4])))
        # Print Iteration Path:
        steps <<- steps + 1
        if (.trace) {
            cat("\n Optimization Step:         ", steps)
            cat("\n Objective Function Value:  ", -f)
            cat("\n Parameter Estimates:       ", x[1], x[2], x[3], x[4], "\n")
        } 
        f 
    }
        
    # Minimization:
    r = nlm(f = enigmle, p = c(alpha, beta, delta, mu), y = x, ...)
        
    # Optional Plot:
    if (doplot) {
        if (span == "auto") {
            alpha = r$estimate[1]
            beta = r$estimate[2]
            delta = r$estimate[3]
            mu = r$estimate[4]
            span.min = qnig(0.001, alpha, beta, delta, mu)
            span.max = qnig(0.999, alpha, beta, delta, mu)
            span = seq(span.min, span.max, length = 100)   
        }
        par(err=-1)
        z = density(x, n = 100)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dnig(span, 
            alpha = r$estimate[1], 
            beta = r$estimate[2], 
            delta = r$estimate[3], 
            mu = r$estimate[4])
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]), 
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)")
        title("NIG: Parameter Estimation")
        lines(x = span, y = log(y.points), col = "steelblue4") 
        if (exists("grid")) grid() 
    }
    
    # Add Title and Description:
    if (is.null(title)) title = "Normal Inverse Gaussian Parameter Estimation"
    if (is.null(description)) description = as.character(date())
    
    # Fit:
    fit = list(estimate = r$estimate, minimum = -r$minimum, code = r$code,        
        gradient = r$gradient, steps = steps)
        
    # Return Value:
    new("fDISTFIT",     
        call = as.call(CALL),
        model = "Normal Inverse Gaussian Distribution",
        data = as.data.frame(x.orig),
        fit = fit,
        title = as.character(title), 
        description = as.character(description) )
}


################################################################################
#  ssdFit               Fits probability densities using smoothing spline ANOVA
#   print.ssd           S3 Print Method        
#  .ssden               Internal function required by ssdFit
#  .ssden.mkterm.cubic1 Internal function required by ssdFit
#  .ssden.mkphi.cubic   Internal function required by ssdFit
#  .ssden.mkrk.cubic    Internal function required by ssdFit
#  .ssden.gauss.quad    Internal function required by ssdFit 
#  .ssden.sspdsty       Internal function required by ssdFit
#  .ssden.nlm0          Internal function required by ssdFit
# ------------------------------------------------------------------------------
# Code Copied from:
#   Package: gss
#   Version: 0.9-3
#   Depends: R (>= 1.7.0)
#   Title: General Smoothing Splines
#   Author: Chong Gu <chong@stat.purdue.edu>
#   Maintainer: Chong Gu <chong@stat.purdue.edu>
#   Description: A comprehensive package for structural multivariate
#           function estimation using smoothing splines.
#   License: GPL
#   Packaged: Thu Sep 23 16:28:03 2004


ssdFit = 
function (x, alpha = 1.4, seed = NULL, title = NULL, description = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Estimate probability densities using smoothing spline ANOVA 
    #   models with cubic spline, linear spline, or thin-plate spline 
    #   marginals for numerical variables.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Fit: 
    x.orig = x
    CALL = match.call()
    
    ans = .ssden(~x, alpha = alpha, seed = seed)
    ans$call = CALL
    class(ans) = "ssd"
    
    # Add Title and Description:
    if (is.null(title)) title = "Smooth Spline Distribution Fit"
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


print.ssd = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # call
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    
    if (FALSE) {
        # terms
        cat("Terms:\n")
        print.default(x$terms$labels)
        cat("\n")
        
        # terms overview
        cat("Number of unpenalized and penalized terms:\n\n")
        print.default(x$desc)
        cat("\n")
    }
    
    # Smoothing Parameters:
    cat("Smoothing parameters are selected by CV with alpha=", x$alpha, ".", 
        sep = "")
    cat("\n")
    
    # the rest are suppressed
    invisible()
}


# ------------------------------------------------------------------------------


.ssden = 
function (formula, data = list(), alpha = 1.4, weights = NULL, subset, 
nbasis = NULL, seed = NULL, domain = as.list(NULL), ext = 0.05, 
prec = 1.0e-07, maxiter = 30) 
{   # An adapted copy from contributed R package gss

    # Description:
    #   Fit density model
     
    # Changes:
    #
    
    # FUNCTION:
    
    # Obtain model frame and model terms:
    id.basis = NULL
    mf = match.call()
    mf$type = mf$alpha = NULL
    mf$id.basis = mf$nbasis = mf$seed = NULL
    mf$domain = mf$quadrature = mf$ext  = NULL
    mf$prec = mf$maxiter = mf$order = NULL
    mf[[1]] = as.name("model.frame")
    mf = eval(mf, sys.frame(sys.parent()))
    cnt = model.weights(mf)
    mf$"(weights)" = NULL
    
    # Set Domain:
    Name = names(mf)
    mn = min(mf[[Name]])
    mx = max(mf[[Name]])
    range = mx - mn
    mn = mn - ext*range
    mx = mx + ext*range
    domain[[Name]] = c(mn, mx)
    domain = as.data.frame(domain)
    
    # Generate sub-basis:
    nobs = dim(mf)[1]
    if (is.null(nbasis)) nbasis = max(30, ceiling(10*nobs^(2/9)))
    if (nbasis >= nobs) nbasis = nobs
    if (!is.null(seed)) set.seed(seed)
    if (is.null(cnt)) cnt2 = rep(1, times = nobs)
    id.basis = sample(nobs, nbasis, prob = cnt2)
    
    # Generate terms:
    term = .ssden.mkterm.cubic1(mf, domain)
    term$labels = term$labels[term$labels != "1"]
    
    # Generate default quadrature:
    domain = domain[, 1, drop = FALSE]
    mn = apply(domain, 2, min)
    mx = apply(domain, 2, max)
    
    # Gauss-Legendre quadrature:
    quad = .ssden.gauss.quad(200, c(mn, mx))
    quad$pt = data.frame(quad$pt)
    # colIds(quad$pt) = Name # colIds(mf)
    colnames(quad$pt) = Name
    quadrature = list(pt = quad$pt, wt = quad$wt)
    
    # Generate s, r, and q:
    qd.pt = quadrature$pt
    qd.wt = quadrature$wt
    nmesh = length(qd.wt)
    s = qd.s = r = qd.r = q = NULL
    nq = 0
    x = mf[, term[[Name]]$vlist]
    x.basis = mf[id.basis, term[[Name]]$vlist]
    qd.x = qd.pt[, term[[Name]]$vlist]
    nphi = term[[Name]]$nphi
    nrk = term[[Name]]$nrk
    phi = term[[Name]]$phi
    s = cbind(s, phi$fun(x, nu = 1, env = phi$env))
    qd.s = cbind(qd.s, phi$fun(qd.x, nu = 1, env = phi$env))
    rk = term[[Name]]$rk
    nq = nq+1
    r = array(c(r, rk$fun(x.basis, x, nu = 1, env = rk$env, 
        out = TRUE)), c(nbasis, nobs, nq))
    qd.r = array(c(qd.r, rk$fun(x.basis, qd.x, nu = 1, env = 
        rk$env,out = TRUE)), c(nbasis,nmesh,nq))
    q = array(c(q, rk$fun(x.basis, x.basis, nu = 1, env = 
        rk$env, out = TRUE)), c(nbasis, nbasis, nq))

    # Check s rank:
    nnull = dim(s)[2]
    if (qr(s)$rank < nnull)
        stop("error in .ssden: fixed effect MLE is not unique")
    s = t(s)
    qd.s = t(qd.s)
    
    # Fit the model:
    r = r[, , 1]
    qd.r = qd.r[, , 1]
    q = q[, , 1]
    z = .ssden.sspdsty(s, r, q, cnt, qd.s, qd.r, qd.wt, prec, maxiter, alpha)
    
    # Result:
    obj = c(list(call = match.call(), mf = mf, cnt = cnt, terms = term,
        desc = NULL, alpha = alpha, domain = domain, quad = quadrature,
        id.basis = id.basis), z)
    class(obj) = "ssden"
    
    # Return Value:
    obj
}


# ------------------------------------------------------------------------------


.ssden.mkterm.cubic1 = 
function(mf, range)
{   # An adapted copy from contributed R package gss

    # Description:
    #   Make phi and rk for cubic spline model terms

    # Changes:
    #
    
    # FUNCTION:
    
    # Obtain model terms:
    mt = attr(mf, "terms")
    
    # xvars = as.character(attr(mt, "variables"))
    # For SPlus compatibility:
    xvars = as.character(attr(mt, "variables"))
    if (class(version) != "Sversion") xvars = xvars[-1]
    
    # Continue:
    xfacs = attr(mt, "factors")
    term.labels = labels(mt)
    if (attr(attr(mf, "terms"), "intercept")) {
        term.labels = c("1", term.labels)
    }
    
    # Create the phi and rk functions:
    term = list(labels = term.labels)
    iphi.wk = 1
    irk.wk = 1
    for (label in term.labels) {
        iphi = irk = phi = rk = NULL
        if (label == "1") {
            # The constant term:
            iphi = iphi.wk
            iphi.wk = iphi.wk + 1
            term[[label]] = list(iphi = iphi, nphi = 1, nrk = 0)
            next
        }
        vlist = xvars[as.logical(xfacs[, label])]
        x = mf[, vlist]
        lmt = range[, vlist]
        dm = length(vlist)
        if (dm == 1) {
            if (!is.factor(x)) {
                # Numeric variable:
                mn = min(lmt)
                mx = max(lmt)
                # phi:
                phi.env = .ssden.mkphi.cubic(c(mn, mx))
                phi.fun <<- function(x, nu = 1, env) env$fun(x, env$env)
                nphi = 1
                iphi = iphi.wk
                iphi.wk = iphi.wk + nphi
                phi = list(fun = phi.fun, env = phi.env)
                # rk:
                rk.env = .ssden.mkrk.cubic(c(mn,mx))
                rk.fun <<- function(x, y, nu = 1, env, outer.prod = FALSE) {
                    env$fun(x, y, env$env, outer.prod)
                }
                nrk = 1
                irk = irk.wk
                irk.wk = irk.wk + nrk
                rk = list(fun = rk.fun, env = rk.env)
            } else {
                # Factor variable:
                if (!is.ordered(x)) fun.env = mkrk.nominal(levels(x))
                else fun.env = mkrk.ordinal(levels(x))
                if (nlevels(x)>2) {
                    # phi:
                    nphi = 0
                    # rk:
                    rk.fun <<- function(x, y, nu = 1, env, outer.prod = FALSE) {
                        env$fun(x, y, env$env, outer.prod)
                    }
                    nrk = 1
                    irk = irk.wk
                    irk.wk = irk.wk + nrk
                    rk = list(fun = rk.fun, env = fun.env)
                } else {
                    # phi:
                    phi.fun <<- function(x, nu = 1, env) {
                        wk = as.factor(names(env$env$code)[1])
                        env$fun(x, wk, env$env)
                    }
                    nphi = 1
                    iphi = iphi.wk
                    iphi.wk = iphi.wk + nphi
                    phi = list(fun = phi.fun, env = fun.env)
                    # rk:
                    nrk = 0
                }
            }
        } else {
            bin.fac = n.phi = phi.list = rk.list = NULL
            for (i in 1:dm) {
                if (!is.factor(x[[i]])) {
                    # Numeric variable:
                    mn = min(lmt[[i]])
                    mx = max(lmt[[i]])
                    phi.wk = .ssden.mkphi.cubic(c(mn, mx))
                    rk.wk = .ssden.mkrk.cubic(c(mn, mx))
                    n.phi = c(n.phi, 1)
                    bin.fac = c(bin.fac, 0)
                } else {
                    ## actor variable:
                    if (!is.ordered(x[[i]])) {
                        rk.wk = mkrk.nominal(levels(x[[i]]))
                    } else {
                        rk.wk = mkrk.ordinal(levels(x[[i]]))
                    }
                    phi.wk = rk.wk
                    n.phi = c(n.phi,0)
                    bin.fac = c(bin.fac, !(nlevels(x[[i]]) > 2))
                }
                phi.list = c(phi.list, list(phi.wk))
                rk.list = c(rk.list, list(rk.wk))
            }
            # phi:
            if (sum(n.phi+bin.fac) < dm) {
                nphi = 0
            } else {
                phi.env = list(dim = dm, n.phi = n.phi, phi = phi.list)
                phi.fun <<- function(x, nu = 1, env) {
                    z = 1
                    for (i in 1:env$dim) {
                        if (env$n.phi[i]) {
                            z = z * env$phi[[i]]$fun(x[[i]], env$phi[[i]]$env)
                        } else {
                            wk = as.factor(names(env$phi[[i]]$env$code)[1])
                            z = z * env$phi[[i]]$fun(x[[i]], wk, 
                                env$phi[[i]]$env)
                        }
                    }
                    z
                }
                nphi = 1
                iphi = iphi.wk
                iphi.wk = iphi.wk + nphi
                phi = list(fun = phi.fun, env = phi.env)
            }
            # rk:
            rk.env = list(dim = dm, n.phi = n.phi, nphi = nphi, 
                phi = phi.list, rk = rk.list)
            rk.fun <<- function(x, y, nu, env,outer.prod = FALSE) {
                div = env$n.phi + 1
                ind = nu - 1 + env$nphi
                z = 1
                for (i in 1:env$dim) {
                    code = ind%%div[i] + 1
                    ind = ind%/%div[i]
                    if (code == div[i]) {
                        z = z * env$rk[[i]]$fun(x[[i]], y[[i]],
                            env$rk[[i]]$env,outer.prod)
                    } else {
                        phix = env$phi[[i]]$fun(x[[i]], env$phi[[i]]$env)
                        phiy = env$phi[[i]]$fun(y[[i]], env$phi[[i]]$env)
                        if (outer.prod) {
                            z = z * outer(phix, phiy)
                        } else {
                            z = z * phix * phiy
                        }
                    }
                }
                z
            }
            nrk = prod(n.phi+1) - nphi
            irk = irk.wk
            irk.wk = irk.wk + nrk
            rk = list(fun = rk.fun, env = rk.env)
        }
        term[[label]] = list(vlist = vlist, iphi = iphi, nphi = nphi,
            phi=phi, irk = irk, nrk = nrk, rk = rk)
    }
    # Return Value:
    term
}


# ------------------------------------------------------------------------------


.ssden.mkphi.cubic = 
function(range)
{   # An adapted copy from contributed R package gss

    # Description:
    #   Make phi function for cubic splines
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Create the environment
    env = list(min=min(range), max=max(range))
    
    # Create the phi function
    fun <<- function(x, env) {
        # Check the input:
        if (!is.vector(x)) {
            stop("error in phi: inputs are of wrong types")
        }
        if ((min(x) < env$min) | (max(x) > env$max)) {
            stop("error in phi: inputs are out of range")
        }
        # Return the result:
        (x-env$min) / (env$max-env$min) - 0.5
    }
    
    # Return Value:
    list(fun = fun, env = env)
}


# ------------------------------------------------------------------------------


.ssden.mkrk.cubic = 
function(range)
{   # An adapted copy from contributed R package gss

    # Description:
    #   Make RK for cubic splines
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Create the environment
    env = list(min = min(range), max = max(range))
    
    # Create the rk function
    fun <<- function(x, y, env, outer.prod = FALSE) {
        # Check the inputs
        if (!(is.vector(x)&is.vector(y))) {
            stop("error in rk: inputs are of wrong types")
        }
        if ((min(x,y) < env$min) | (max(x,y) > env$max)) {
            stop("error in rk: inputs are out of range")
        }
        
        # Scale the inputs:
        x = (x-env$min) / (env$max-env$min)
        y = (y-env$min) / (env$max-env$min)
        
        # Result:
        rk <<- function(x,y) {
            k2 <<- function(x) ((x-.5)^2-1/12)/2
            k4 <<- function(x) ((x-.5)^4-(x-.5)^2/2+7/240)/24
            k2(x)*k2(y)-k4(abs(x-y))
        }
        if (outer.prod) {
            outer(x, y, rk)
        } else {
            rk(x, y)
        }
    }
    
    # Return Value:
    list(fun = fun, env = env)
}


# ------------------------------------------------------------------------------

.ssden.gauss.quad = 
function(size, interval) 
{   # An adapted copy from contributed R package gss

    # Description:
    #   Generate Gauss-Legendre quadrature
    
    # Changes:
    #
    
    # FUNCTION:
    
    if (interval[1] >= interval[2])
        warning("warning in .ssden.gauss.quad: interval limits swapped")
    z = .Fortran("gaussq",
          as.integer(1),
          as.integer(size),
          as.double(0), 
          as.double(0),
          as.integer(0),
          as.double(c(-1,1)), 
          double(size),
          t=double(size), 
          w=double(size),
          PACKAGE = "fBasics")
          
    mn = min(interval[1:2])
    range = abs(interval[1]-interval[2])
    pt = mn+range*(z$t+1)/2
    wt = range*z$w/2
    
    # Return Value:
    list(pt = pt, wt = wt)
}


# ------------------------------------------------------------------------------


.ssden.sspdsty = 
function(s, r, q, cnt, qd.s, qd.r, qd.wt, prec, maxiter, alpha)
{   # An adapted copy from contributed R package gss

    # Description:
    #   Fit single smoothing parameter density
    
    # Note:
    #   Global variables introduced for SPlus compatibility
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Settings:
    nxi <<- dim(r)[1]
    nobs <<- dim(r)[2]
    nqd <<- length(qd.wt)
    if (!is.null(s)) {
        nnull <<- dim(s)[1]
    } else {
        nnull <<- 0
    }
    nxis <<- nxi + nnull
    if (is.null(cnt)) cnt <<- 0
    
    prec <<- prec
    maxiter <<- maxiter
    alpha <<- alpha
    
    s <<- s
    r <<- r
    q <<- q
    
    qd.s <<- qd.s
    qd.r <<- qd.r
    qd.wt <<- qd.wt
    
    # Internal Function:
    cv <<- function(lambda) {
        fit <<- .Fortran("dnewton",
            cd = as.double(cd), 
            as.integer(nxis),
            as.double(10^(lambda+theta)*q), 
            as.integer(nxi),
            as.double(rbind(10^theta*r, s)), 
            as.integer(nobs),
            as.integer(sum(cnt)), 
            as.integer(cnt),
            as.double(t(rbind(10^theta*qd.r, qd.s))), 
            as.integer(nqd),
            as.double(qd.wt),
            as.double(prec), 
            as.integer(maxiter),
            as.double(.Machine$double.eps),
            wk = double(2*(nqd+nobs)+nxis*(nxis+4)+max(nxis, 3)),
            info = integer(1),
            PACKAGE = "fBasics")
        if (fit$info == 1) 
            stop("error in .ssden: Newton iteration diverges")
        if (fit$info == 2) 
            warning("gss warning in .ssden: Newton iteration fails to converge")
        if (class(version) != "Sversion") {
            assign("cd", fit$cd, inherit = TRUE)
            assign("int", fit$wk[3], inherit = TRUE)
        } else {
            assign("cd", fit$cd)
            assign("int", fit$wk[3])
            cd <<- cd
            int <<- int
        }
        cv = alpha*fit$wk[2]-fit$wk[1]
        alpha.wk = max(0,log.la0-lambda-5)*(3-alpha) + alpha
        alpha.wk = min(alpha.wk, 3)
        adj = ifelse (alpha.wk > alpha, (alpha.wk-alpha)*fit$wk[2], 0)
        cv+adj
    }
    
    # Initialization:
    mu.r = apply(qd.wt*t(qd.r), 2, sum) / sum(qd.wt)
    v.r = apply(qd.wt*t(qd.r^2), 2, sum) / sum(qd.wt)
    mu.s = apply(qd.wt*t(qd.s), 2, sum) / sum(qd.wt)
    v.s = apply(qd.wt*t(qd.s^2), 2, sum) / sum(qd.wt)
    if (is.null(s)) {
        theta <<- 0
    } else {
        theta <<- log10(sum(v.s-mu.s^2)/nnull/sum(v.r-mu.r^2)*nxi) / 2
    }
    log.la0 <<- log10(sum(v.r-mu.r^2)/sum(diag(q))) + theta
        
    # lambda Search:
    cd <<- rep(0, nxi+nnull)
    int <<- NULL
    la = log.la0
    repeat {
        mn = la-1
        mx = la+1
        zz = .ssden.nlm0(cv, c(mn, mx))
        if (min(zz$est-mn, mx-zz$est) >= 1.0e-3) break
        else la = zz$est
    }
    
    # Result:
    jk1 = cv(zz$est)
    c = cd[1:nxi]
    if (nnull) {
        d = cd[nxi+(1:nnull)]
    } else {
        d = NULL
    }
    
    # Return Value:
    list(lambda = zz$est, theta = theta, c = c, d = d, int = int, cv = zz$min)
}


# ------------------------------------------------------------------------------


.ssden.nlm0 = 
function(fun, range, prec = 1.0e-7)
{   # An adapted copy from contributed R package gss

    # Description:
    #   minimization of univariate function on finite intervals
    #   using 3-point quadratic fit with golden-section safe-guard

    # Changes:
    #
    
    # FUNCTION:
    
    ratio = 2 / (sqrt(5)+1)
    ll.x = min(range)
    uu.x = max(range)
    if (uu.x-ll.x < prec) {
        sol = (ll.x+uu.x) / 2
        fit = fun(sol)
        return(list(estimate = sol, minimum = fit, evaluations = 1))
    }
    
    ml.x = uu.x - ratio*(uu.x-ll.x)
    mu.x = ll.x + ratio*(uu.x-ll.x)
    
    # Initialization
    uu.fit = fun(uu.x)
    mu.fit = fun(mu.x)
    ml.fit = fun(ml.x)
    ll.fit = fun(ll.x)
    neval = 4
    
    # Iteration:
    repeat {
        # Fit a parabola to the 3 best points and find its minimum
        if (ll.fit<uu.fit) {
            delta.l = ml.x-ll.x
            sigma.l = ml.x+ll.x
            d.l = (ml.fit-ll.fit) / delta.l
            delta.u = mu.x-ml.x
            d.u = (mu.fit-ml.fit) / delta.u
        } else {
            delta.l = mu.x-ml.x
            sigma.l = mu.x+ml.x
            d.l = (mu.fit-ml.fit) / delta.l
            delta.u = uu.x-mu.x
            d.u = (uu.fit-mu.fit) / delta.u
        }
        a = (d.u-d.l) / (delta.l+delta.u)
        b = d.l-a*sigma.l
        if (a <= 0) {
            nn.x = max(range)
        } else {
            nn.x = -b/2/a
        }
        
        # New bracket:
        if (ml.fit<mu.fit) {
            uu.x = mu.x
            uu.fit = mu.fit
            mm.x = ml.x
            mm.fit = ml.fit
        } else {
            ll.x = ml.x
            ll.fit = ml.fit
            mm.x = mu.x
            mm.fit = mu.fit
        }
        range.l = mm.x-ll.x
        range.u = uu.x-mm.x
        delta = min(abs(nn.x-c(ll.x, mm.x, uu.x)))
        
        # Safeguard:
        if ((nn.x < ll.x) | (nn.x > uu.x) | (delta < prec)) {
            if (range.u > range.l) {
                nn.x = uu.x - ratio*range.u
            } else {
                nn.x = ll.x + ratio*range.l
            }
        }
        
        # Update middle points:
        nn.fit = fun(nn.x)
        neval = neval + 1
        if (nn.x<mm.x) {
            ml.x = nn.x
            ml.fit = nn.fit
            mu.x = mm.x
            mu.fit = mm.fit
        }
        else {
            ml.x = mm.x
            ml.fit = mm.fit
            mu.x = nn.x
            mu.fit = nn.fit
        }
        
        # Result:
        if ((range.l+range.u < 0.5) & (abs(mm.x-nn.x) < sqrt(prec))) {
            if (nn.fit < mm.fit) {
                solution = nn.x
                fit = nn.fit
            } else {
                solution = mm.x
                fit = mm.fit
            }
            break
        }
    }
    
    # Return Value:
    list(estimate = solution, minimum = fit, evaluations = neval)
}


################################################################################
# PART V: FUNCTIONS FOR SPLUS VERSION:
  

if (!exists("match.fun")) {
match.fun =
function(FUN)
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # For S-Plus Compatibility:
    if (is.function(FUN)) {
        ans = FUN
    } else {
        ans = getFunction(as.character(FUN))
    }
    
    # Return Value:
    ans
}}


# ------------------------------------------------------------------------------


if (!exists("which.min")) 
{   
which.min = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # For S-Plus Compatibility:
    ans = order(x)[1]
    
    # Return Value:
    ans
}}

 
# ------------------------------------------------------------------------------


if (!exists("which.max")) 
{   
which.max = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # For S-Plus Compatibility:
    ans = rev(order(x))[1]
    
    # Return Value:
    ans
}}


################################################################################

