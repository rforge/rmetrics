
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

