
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:           DESCRIPTION:
#  dmvsnorm            Multivariate Skew Normal Density Function
#  pmvsnorm            Multivariate Skew Normal Probability Function
#  rmvsnorm            Multivariate Skew Normal Random Deviates
# FUNCTION:           DESCRIPTION:
#  dmvst               Multivariate Skew Sudent-t Density Function
#  pmvst               Multivariate Skew Sudent-t Probability Function
#  rmvst               Multivariate Skew Sudent-t Random Deviates
# FUNCTION:           PARAMETER ESTIMATION:
#  fMV                 S4 Object of class 'fMV'
#  mvFit               Fits a MV Normal or Student-t Distribution
#  print.fMV           S3: Print method for objects of class 'fMV'
#  plot.fMV            S3: Plot method for objects of class 'fMV'
#  summary.fMV         S3: Summary method for objects of class 'fMV'
#  .mvsnormFit         Fits a Multivariate Normal Distribution
#  .mvstFit            Fits a Multivariate Student-t Distribution
#  .mvsnormPlot        Plots for Multivariate Normal Distribution
#  .mvstPlot           Plots for Multivariate Student-t Distribution
# REQUIREMENTS:       FOR:
#  fExtremes           InteractivePlot Function
# MVTNORM BUILTIN:    DESCRIPTION:
#  .checkmvArgs        Internal Function
#  .pmvnorm            Internal Function
#  .pmvt               Internal Function
#  .mvt                Internal Function
#  .rmvt               Internal Function
#  .qmvnorm            Internal Function
#  .qmvt               Internal Function
# SN BUILTIN:         DESCRIPTION:
#  .rsn                Internal Function
#  .rst                Internal Function
#  .rmst               Internal Function
#  .rmsn               Internal Function
#  .msn.quantities     Internal Function
#  .msn.mle            Internal Function
#  .msn.dev            Internal Function
#  .msn.dev.grad       Internal Function
#  .msn.moment.fit     Internal Function
#  .zeta               Internal Function
#  .num.deriv          Internal Function
#  .mst.mle            Internal Function
#  .mst.dev            Internal Function
#  .mst.dev.grad       Internal Function
################################################################################


################################################################################
# Multivariate Skew Normal Distribution


dmvsnorm = 
function(x, dim = 2, mu = rep(0, dim), Omega = diag(dim), 
alpha = rep(0, dim))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Multivariate Skew Normal Density Function
    
    # FUNCTION:
        
    # Settings:
    xi = mu 
    ans = NA
    
    # Univariate Case:
    if (is.vector(x) & dim == 1) {
        ans = .dsn(x, location = xi[1], scale = as.vector(Omega)[1], 
            shape = alpha[1])
    }
    
    # Multivariate Case:
    if (is.matrix(x)) {
        if (dim == ncol(x)) {
            ans = .dmsn(x = x, xi = xi, Omega = Omega, alpha = alpha)
        } 
    }
    
    # Check for conflicting Dimensions:
    if (is.na(ans[1])) {
        stop("conflicting x and dim")
    }
        
    # Return Value:
    as.vector(ans)
}


# ------------------------------------------------------------------------------


pmvsnorm = 
function(q, dim = 2, mu = rep(0, dim), Omega = diag(dim), 
alpha = rep(0, dim))
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Settings:
    x = q
    xi = mu
    ans = NA
    
    # Univariate Case:
    if (is.vector(x) & dim == 1) {
        ans = .psn(x, location = xi[1], scale = as.vector(Omega)[1], 
            shape = alpha[1])
    }
    
    # Multivariate Case:
    if (is.matrix(x)) {
        if (dim == ncol(x)) {
            ans = NULL
            for (i in 1:nrow(x) ) {
                ans = c(ans, .pmsn(x = x[i,], xi = xi, Omega = Omega, 
                    alpha = alpha))
            }
        } 
    }
    
    # Check for conflicting Dimensions:
    if (is.na(ans[1])) {
        stop("conflicting x and dim")
    }
        
    # Return Value:
    as.vector(ans)
}


# ------------------------------------------------------------------------------


rmvsnorm = 
function(n, dim = 2, mu = rep(0, dim), Omega = diag(dim), 
alpha = rep(0, dim))
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Settings:
    ans = NA
    xi = mu
    
    # Univariate Case:
    if (dim == 1) {
        ans = as.matrix(.rsn(n, location = xi[1], 
            scale = as.vector(Omega)[1], shape = alpha[1]))
    }
    
    # Multivariate Case:
    if (dim > 1) {
        ans = .rmsn(n, xi = xi, Omega = Omega, alpha = alpha)
    }
    
    # Check for conflicting Dimensions:
    if (is.na(ans[1])) {
        stop("dim must be greater 1")
    }
        
    # Return Value:
    rownames(ans) = as.character(1:n)
    colnames(ans) = as.character(1:dim)
    ans
}


################################################################################
# MULTIVARIATE SKEW STUDENT-T


dmvst = 
function(x, dim = 2, mu = rep(0, dim), Omega = diag(dim), 
alpha = rep(0, dim), df = 4)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Multivariate Skew Sudent-t Density Function
    
    # FUNCTION:
    
    # Settings:
    xi = mu
    ans = NA
    
    # Univariate Case:
    if (is.vector(x) & dim == 1) {
        ans = .dst(x, location = xi[1], scale = as.vector(Omega)[1], 
            shape = alpha[1], df = Inf)
    }
    
    # Multivariate Case:
    if (is.matrix(x)) {
        if (dim == ncol(x)) {
            ans = .dmst(x = x, xi = xi, Omega = Omega, alpha = alpha, df = df)
        } 
    }
    
    # Check for conflicting Dimensions:
    if (is.na(ans[1])) {
        stop("conflicting x and dim")
    }
        
    # Return Value:
    as.vector(ans)
}


# ------------------------------------------------------------------------------


pmvst = 
function(q, dim = 2, mu = rep(0, dim), Omega = diag(dim), 
alpha = rep(0, dim), df = 4)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Settings:
    x = q
    xi = mu
    ans = NA
    
    # Univariate Case:
    if (is.vector(x) & dim == 1) {
        ans = .pst(x, location = xi[1], scale = as.vector(Omega)[1], 
            shape = alpha[1], df = df)
    }
    
    # Multivariate Case:
    if (is.matrix(x)) {
        if (dim == ncol(x)) {
            ans = NULL
            for (i in 1:nrow(x) ) {
                ans = c(ans, .pmst(x = x[i,], xi = xi, Omega = Omega, 
                    alpha = alpha, df = df))
            }
        } 
    }
    
    # Check for conflicting Dimensions:
    if (is.na(ans[1])) {
        stop("conflicting x and dim")
    }
        
    # Return Value:
    as.vector(ans)
}


# ------------------------------------------------------------------------------


rmvst = 
function(n, dim = 2, mu = rep(0, dim), Omega = diag(dim), 
alpha = rep(0, dim), df = 4)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Settings:
    ans = NA
    xi = mu
    
    # Univariate Case:
    if (dim == 1) {
        ans = as.matrix(.rst(n, location = xi[1], 
            scale = as.vector(Omega)[1], shape = alpha[1], df = df))
    }
    
    # Multivariate Case:
    if (dim > 1) {
        ans = .rmst(n, xi = xi, Omega = Omega, alpha = alpha, df = df)
    }
    
    # Check for conflicting Dimensions:
    if (is.na(ans[1])) {
        stop("dim must be greater 1")
    }
        
    # Return Value:
    rownames(ans) = as.character(1:n)
    colnames(ans) = as.character(1:dim)
    ans
}


################################################################################
# PARAMETER FIT:


setClass("fMV", 
    representation(
        call = "call",
        method = "character",
        model = "list",
        data = "data.frame",
        fit = "list",
        title = "character",
        description = "character")  
)


# ------------------------------------------------------------------------------


mvFit = 
function(x, method = c("snorm", "st"), fixed.df = NA, title = NULL,
description = NULL, trace = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Fit:
    if (method[1] == "snorm") {
        # Normal Fit:
        fit = .mvsnormFit(x = x, trace = trace, ...)
        fit$df = Inf
    }
    if (method[1] == "st") {
       # Student-t Fit:
       fit = .mvstFit(x = x, fixed.df = fixed.df, trace = trace, ...)
    }
        
    # Add to fit:
    fit$method = method[1]
    class(fit) = "list"
       
    # Model Slot: 
    model = list(beta = fit$beta, Omega = fit$Omega, 
        alpha = fit$alpha, df = fit$df)
    
    # Title Slot:
    if (is.null(title)) {
        if (method[1] == "snorm") 
            title = "Multivariate Normal Distribution"
        if (method[1] == "st") 
            title = "Multivariate Student-t Distribution" 
    }
    
    # Description Slot:
    if (is.null(description)) descripotion = as.character(date())
    
    # Return Value:
    new("fMV",     
        call = as.call(match.call()),
        method = as.character(method[1]),
        model = model,
        data = as.data.frame(x), 
        fit = fit,
        title = as.character(title), 
        description = as.character(description) )
}


# ------------------------------------------------------------------------------


print.fMV =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Extract fit:
    fit = x@fit
    
    # Print:
    cat("\nCall:\n")
    print.default(fit$call)
    
    cat("\nParameter Sstimates:\n")
    print.default(fit$dp)
    
    cat("\nParameter Errors:\n")
    print.default(fit$se) 
    
    cat("\nOptimization:\n")
    print.default(fit$optim)   
}


# ------------------------------------------------------------------------------


plot.fMV =
function(x, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Plot:
    if (x@fit$method == "snorm") {
        # Multivariate Skew Normal Distribution:
        return(.mvsnormPlot(x = x@fit, which = which, ...))
    }
    if (x@fit$method == "st") {
         # Multivariate Skew Student-t Distribution:
         return(.mvstPlot(x = x@fit, which = which, ...))
    }
}


# ------------------------------------------------------------------------------


summary.fMV =
function(object, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Print:
    print(x = object, ...)
    
    # Plot:
    plot(x = object, which = which, ...)
    
    # Return Value:
    invisible(object)
}


################################################################################
# INERNAL FUNCTIONS:


.mvsnormFit = 
function(x, trace = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function

    # FUNCTION:
    
    # Settings:
    y = x
    y.name = deparse(substitute(y))
    y.names = dimnames(y)[[2]]
    y = as.matrix(y)
    colnames(y) = y.names
    k = ncol(y)
    freq = rep(1, nrow(y))
    n = sum(freq)
    X = rep(1, nrow(y))
    X = as.matrix(X)
    m = ncol(X)
    dimnames(y) = list(NULL, outer("V", as.character(1:k), paste, sep = ""))
    y.names = as.vector(dimnames(y)[[2]])
    qrX = qr(X)
    
    # Fit:
    mle = .msn.mle(X = X, y = y, freq = freq, trace = trace, ...) 
    mle$call = match.call()
    mle$y = y
    mle$y.names = y.names
    
    # Parameters:
    mle$beta = beta = mle$dp$beta
    mle$xi = xi = X %*% beta
    mle$Omega = Omega = mle$dp$Omega
    mle$alpha = alpha = mle$dp$alpha

    # Test:
    dev.norm = .msn.dev(c(qr.coef(qrX, y), rep(0, k)), X, y, freq)
    test = dev.norm + 2 * mle$logL
    p.value = 1 - pchisq(test, k)    
    mle$test.normality = list(LRT = test, p.value = p.value)
    
    # Save for Plot:
    Xb = qr.fitted(qrX, y)
    res = qr.resid(qrX, y)
    mle$k = k
    mle$n = n
    mle$pp = qchisq((1:n)/(n + 1), k)
    mle$rad.n = apply((y - Xb) * ((y - Xb) %*% solve(var(res))), 1, sum)
    mle$rad.sn = apply((y - xi) * ((y - xi) %*% solve(Omega)), 1, sum)
    
    # Return Value:
    class(mle) = "snFit"
    mle
}


# ------------------------------------------------------------------------------


.mvstFit =
function(x, fixed.df = NA, trace = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Settings:
    y = as.matrix(x)
    k = ncol(y)
    y.name = deparse(substitute(y))
    dimnames(y) = list(NULL, outer("V", as.character(1:k), paste, sep = ""))
    y.names = dimnames(y)[[2]]
    
    freq = rep(1, nrow(y))
    n = sum(freq)
    
    X = as.matrix(rep(1, nrow(y)))
    qrX = qr(X)
    m = ncol(X)
    
    # Fit:
    mle = .mst.mle(X = X, y = y, freq = freq, fixed.df = fixed.df, 
        start = NA, trace = trace, ...)
    mle$call = match.call()
    mle$y = y
    mle$y.names = y.names
    
    # Parameters:
    mle$beta = beta = mle$dp$beta
    mle$xi = xi = X %*% beta
    mle$Omega = Omega = mle$dp$Omega
    mle$alpha = alpha = mle$dp$alpha 
    mle$df = df = mle$dp$df
    
    
    # Save for Plot:
    Xb = qr.fitted(qrX, y)
    res = qr.resid(qrX, y)
    mle$k = k
    mle$n = n
    mle$pp = k * qf((1:n)/(n + 1), k, df)
    mle$rad.n = as.vector(apply(res * (res %*% solve(var(res))), 1, sum))
    mle$rad.sn = as.vector(apply((y - xi)*((y - xi) %*% solve(Omega)), 1, sum))

    # Return Value:
    class(mle) = "stFit"
    mle 
}


# ------------------------------------------------------------------------------


.mvsnormPlot =
function(x, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # FUNCTION: 
    
    # Internal Plot Functions:
    dim = x$k
    if (dim == 1 ) {
        plot.1 <<- function(x) {
            z = x
            y0 <- z$y
            xi0 <- apply(z$xi, 2, mean)
            y0 <- as.vector(y0)
            x <- seq(min(pretty(y0, 10)), max(pretty(y0, 10)), length = 100)
            omega <- sqrt(diag(z$Omega))
            dp0 <- c(xi0, omega, z$alpha)
            xlab <- z$y.name
            hist(y0, prob = TRUE, breaks = "FD", xlab = xlab, 
                ylab = "density", border = "white", col = "steelblue4", 
                main = z$y.name)
            lines(x, .dsn(x, dp0[1], dp0[2], dp0[3]))
            if (length(y0) < 201) 
                points(y0, rep(0, z$n), pch = 1) 
        }
    } else {
        plot.1 <<- function(x) {
            opt = options()
            options(warn = -1)
            pairs(
                x$y, 
                labels = x$y.names, 
                panel = function(x, y, Y, y.names, xi, Omega, alpha) {
                    for (i in 1:length(alpha)) {
                        if (all(Y[, i] == x)) 
                            Ix = i
                        if (all(Y[, i] == y)) 
                            Iy = i }
                    points(x, y)
                    marg = .msn.marginal(xi, Omega, alpha, c(Ix, Iy))
                    xi.marg = marg$xi
                    Omega.marg = marg$Omega
                    alpha.marg = marg$alpha
                    x1 = seq(min(x), max(x), length = 30)
                    x2 = seq(min(y), max(y), length = 30)
                    .dsn2.plot(x1, x2, xi.marg, Omega.marg, alpha.marg, 
                        add = TRUE, col = "steelblue4")}, 
                Y = x$y, 
                y.names = dimnames(x$y)[[2]], 
                xi = apply(x$xi, 2, mean),  
                Omega = x$Omega, 
                alpha = x$alpha) 
                options(opt) } 
    }
    plot.2 <<- function(x) {
        plot(x$pp, sort(x$rad.n), pch = 1, ylim = c(0, max(x$rad.n, x$rad.sn)), 
            xlab = "Chi-square Percentiles", 
            ylab = "Mahalanobis Distances")
        abline(0, 1, lty = 3)
        title(main = "Normal QQ-Plot", sub = x$y.name) }
    plot.3 <<- function(x) {            
        plot(x$pp, sort(x$rad.sn), pch = 1, ylim = c(0, max(x$rad.n, x$rad.sn)), 
            xlab = "Percentiles of chi-square distribution", 
            ylab = "Mahalanobis distances")
        abline(0, 1, lty = 3)
        title(main = "Skew-Normal QQ-Plot", sub = x$y.name) }
    plot.4 <<- function(x) {
        plot((1:x$n)/(x$n + 1), sort(pchisq(x$rad.n, x$k)), 
            xlab = "",  ylab = "")
        abline(0, 1, lty = 3)
        title(main = "Normal PP-Plot", sub = x$y.name) }
    plot.5 <<- function(x) {            
        plot((1:x$n)/(x$n + 1), sort(pchisq(x$rad.sn, x$k)), 
            xlab = "", ylab = "")
        abline(0, 1, lty = 3)
        title(main = "Skew-Normal PP-Plot", sub = x$y.name) }
            
    # Plot:
    plot1Title = "Scatterplots"
    if (dim == 1) plot1Title = "Histogram Plot" 
    interactivePlot(
        x = x,
        choices = c(
            plot1Title, 
            "Normal QQ-Plot", 
            "Skew-Normal QQ-Plot",
            "Normal PP-Plot",
            "Skew-Normal PP-Plot"),
        plotFUN = c(
            "plot.1", 
            "plot.2", 
            "plot.3",
            "plot.4",
            "plot.5"),
        which = which)
                    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


.mvstPlot =
function(x, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # FUNCTION: 
    
    # Internal Plot Functions:
    dim = x$k
    if (dim == 1 ) {
        plot.1 <<- function(x) {
            z = x
            y0 <- z$y
            xi0 <- apply(z$xi, 2, mean)
            y0 <- as.vector(y0)
            x <- seq(min(pretty(y0, 10)), max(pretty(y0, 10)), length = 100)
            omega <- sqrt(diag(z$Omega))
            dp0 <- c(xi0, omega, z$alpha, z$df)
            xlab <- z$y.name
            hist(y0, prob = TRUE, breaks = "FD", xlab = xlab, 
                ylab = "density", border = "white", col = "steelblue4", 
                main = z$y.name)
            lines(x, .dst(x, dp0[1], dp0[2], dp0[3], dp0[4]))
            if (length(y0) < 201) 
                points(y0, rep(0, z$n), pch = 1) 
        }
    } else {
        plot.1 <<- function(x) {
            opt = options()
            options(warn = -1)
            pairs(
                x$y, 
                labels = x$y.names, 
                panel = function(x, y, Y, y.names, xi, Omega, alpha, df) {
                    for (i in 1:length(alpha)) {
                        if (all(Y[, i] == x)) 
                            Ix = i
                        if (all(Y[, i] == y)) 
                            Iy = i }
                    points(x, y)
                    marg = .msn.marginal(xi, Omega, alpha, c(Ix, Iy))
                    xi.marg = marg$xi
                    Omega.marg = marg$Omega
                    alpha.marg = marg$alpha
                    x1 = seq(min(x), max(x), length = 30)
                    x2 = seq(min(y), max(y), length = 30)
                    .dst2.plot(x1, x2, xi.marg, Omega.marg, alpha.marg, 
                        df, add = TRUE, col = "steelblue4")} , 
                Y = x$y, 
                y.names = dimnames(x$y)[[2]], 
                xi = apply(x$xi, 2, mean),  
                Omega = x$Omega, 
                alpha = x$alpha,
                df = x$df) 
                options(opt) } 
    }
    plot.2 <<- function(x) {
        plot(x$pp, sort(x$rad.n), pch = 1, ylim = c(0, max(x$rad.n, x$rad.sn)), 
            xlab = "Chi-square Percentiles", 
            ylab = "Mahalanobis Distances")
        abline(0, 1, lty = 3)
        title(main = "Normal QQ-Plot", sub = x$y.name) }
    plot.3 <<- function(x) {            
        plot(x$pp, sort(x$rad.sn), pch = 1, ylim = c(0, max(x$rad.n, x$rad.sn)), 
            xlab = "Percentiles of chi-square distribution", 
            ylab = "Mahalanobis distances")
        abline(0, 1, lty = 3)
        title(main = "Skew-Normal QQ-Plot", sub = x$y.name) }
    plot.4 <<- function(x) {
        plot((1:x$n)/(x$n + 1), sort(pchisq(x$rad.n, x$k)), 
            xlab = "",  ylab = "")
        abline(0, 1, lty = 3)
        title(main = "Normal PP-Plot", sub = x$y.name) }
    plot.5 <<- function(x) {            
        plot((1:x$n)/(x$n + 1), sort(pchisq(x$rad.sn, x$k)), 
            xlab = "", ylab = "")
        abline(0, 1, lty = 3)
        title(main = "Skew-Normal PP-Plot", sub = x$y.name) }
            
    # Plot:
    plot1Title = "Scatterplots"
    if (dim == 1) plot1Title = "Histogram Plot" 
    interactivePlot(
        x = x,
        choices = c(
            plot1Title, 
            "Normal QQ-Plot", 
            "Skew-Normal QQ-Plot",
            "Normal PP-Plot",
            "Skew-Normal PP-Plot"),
        plotFUN = c(
            "plot.1", 
            "plot.2", 
            "plot.3",
            "plot.4",
            "plot.5"),
        which = which)
                    
    # Return Value:
    invisible(x)
}


################################################################################
# BUILTIN: mvtnorm


.checkmvArgs = 
function(lower, upper, mean, corr, sigma) 
{   # A modified copy from contributed R package mvtnorm Version 0.6-8

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    UNI = FALSE
    if (is.null(lower) || any(is.na(lower)))
        stop("lower not specified or contains NA")
    if (is.null(upper) || any(is.na(upper)))
        stop("upper not specified or contains NA")
    rec = cbind(lower, upper, mean)
    lower = rec[,"lower"]
    upper = rec[,"upper"]
    mean = rec[,"mean"]
    
    if (any(is.na(mean)))
        stop("mean contains NA")
        
    if (is.null(corr) && is.null(sigma)) {
        corr = diag(length(lower))
        # warning("both corr and sigma not specified: 
        # using sigma=diag(length(lower))")
    }
    
    if (!is.null(corr) && !is.null(sigma)) {
        sigma = NULL
        warning("both corr and sigma specified: ignoring sigma")
    }
    
    if (!is.null(corr)) {
         if (!is.matrix(corr)) {
             if (length(corr) == 1)
                UNI = TRUE
             if (length(corr) != length(lower))
               stop("diag(corr) and lower are of different length")
         } else {
             if (length(corr) == 1) {
                 UNI = TRUE
                 corr = corr[1,1]
                 if (length(lower) != 1)
                   stop("corr and lower are of different length")
             } else {
                 if (length(diag(corr)) != length(lower))        
                     stop("diag(corr) and lower are of different length")
             }
         }
    }
    
    if (!is.null(sigma)) {
         if (!is.matrix(sigma)) {
            if (length(sigma) == 1)
                UNI = TRUE
            if (length(sigma) != length(lower))        
               stop("diag(sigma) and lower are of different length")
         } else {
            if (length(sigma) == 1) {
                UNI = TRUE       
                sigma = sigma[1,1]
                if (length(lower) != 1) 
                  stop("sigma and lower are of different length")
            } else {
              if (length(diag(sigma)) != length(lower))                     
                 stop("diag(sigma) and lower are of different length")
            }
         }
    }
    
    # Return Value:
    list(lower = lower, upper = upper, mean = mean, corr = corr, 
        sigma = sigma, uni = UNI)
}


# ------------------------------------------------------------------------------


.pmvnorm = 
function(lower = -Inf, upper = Inf, mean = rep(0, length(lower)), 
corr = NULL, sigma = NULL, maxpts = 25000, abseps = 0.001, releps = 0)
{   # A modified copy from contributed R package mvtnorm Version 0.6-8

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    carg = .checkmvArgs(lower = lower, upper = upper, mean = mean, 
        corr = corr, sigma = sigma)
        
    if (!is.null(carg$corr)) {
        corr = carg$corr
        if (carg$uni) {
            stop("sigma not specified: cannot compute pnorm")
        } else {
            lower = carg$lower - carg$mean
            upper = carg$upper - carg$mean
            mean = rep(0, length(lower))
            RET = .mvt(lower = lower, upper = upper, df=0, corr = corr, 
                delta = mean, maxpts = maxpts, abseps=abseps,releps=releps)
        }
    } else {
        if (carg$uni) {
            RET = list(value = pnorm(carg$upper, mean = carg$mean, 
                sd = sqrt(carg$sigma)) - pnorm(carg$lower, mean = carg$mean, 
                sd = sqrt(carg$sigma)), error = 0, 
                msg = "univariate: using pnorm")
        } else {
            lower = (carg$lower - carg$mean)/sqrt(diag(carg$sigma))
            upper = (carg$upper - carg$mean)/sqrt(diag(carg$sigma))
            mean = rep(0, length(lower))
            corr = cov2cor(carg$sigma)
            RET = .mvt(lower = lower, upper = upper, df=0, corr=corr, delta=mean,
                maxpts=maxpts, abseps=abseps,releps=releps)
        }
    }
    
    # Return Value:
    attr(RET$value, "error") = RET$error
    attr(RET$value, "msg") = RET$msg
    return(RET$value)
}


# ------------------------------------------------------------------------------


.pmvt = 
function(lower = -Inf, upper = Inf, delta=rep(0, length(lower)), df = 1, 
corr = NULL, sigma = NULL, maxpts = 25000, abseps = 0.001, releps = 0)
{   # A modified copy from contributed R package mvtnorm Version 0.6-8

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    carg = .checkmvArgs(lower=lower, upper=upper, mean=delta, corr=corr,
                       sigma=sigma)
    if (is.null(df))
        stop("df not specified")
    if (any(df < 0))
        stop("cannot compute multivariate t distribution with df < 0")
    if (carg$uni) {
        if (df > 0)
            RET = list(value = pt(carg$upper, df = df, ncp = carg$mean) -
                pt(carg$lower, df = df, ncp = carg$mean),
                error = 0, msg = "univariate: using pt")
        else
            RET = list(value = pnorm(carg$upper, mean = carg$mean) -
                pnorm(carg$lower, mean = carg$mean),
                error = 0, msg = "univariate: using pnorm")
    } else {
        if (!is.null(carg$corr)) {
            RET = .mvt(lower = carg$lower, upper = carg$upper, df = df, 
                corr = carg$corr, delta = carg$mean, maxpts = maxpts,
                abseps = abseps,releps = releps)
        } else {
            lower = carg$lower/sqrt(diag(carg$sigma))
            upper = carg$upper/sqrt(diag(carg$sigma))
            corr = cov2cor(carg$sigma)
            RET = .mvt(lower = lower, upper = upper, df = df, corr = corr,
                delta = carg$mean, maxpts = maxpts, abseps = abseps,
                releps = releps)
        }
    }
    
    # Return Value:
    attr(RET$value, "error") = RET$error
    attr(RET$value, "msg") = RET$msg
    return(RET$value)
}


# ------------------------------------------------------------------------------


.mvt = 
function(lower, upper, df, corr, delta, maxpts = 25000, abseps = 0.001, 
releps = 0)
{   # A modified copy from contributed R package mvtnorm Version 0.6-8

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    n = ncol(corr)
    
    if (is.null(n) || n < 2) 
        stop("dimension less then n = 2")

    if (length(lower) != n) 
        stop("wrong dimensions")
    if (length(upper) != n) 
        stop("wrong dimensions")

    if (n > 1000) 
        stop("only dimensions 1 <= n <= 1000 allowed") 

    infin = rep(2, n)
    infin[upper == Inf] = 1
    infin[lower == -Inf] = 0
    infin[lower == -Inf & upper == Inf] = -1
    
    if (n > 1) {
        corrF = matrix(as.vector(corr), ncol = n, byrow = TRUE)
        corrF = corrF[upper.tri(corrF)]
    } else { 
        corrF = corr 
    }

    lower[lower == -Inf] = 0
    upper[upper == Inf] = 0

    error = 0
    value = 0
    inform = 0

    ### TOL argument re-added in version 0.6-3
    ### not yet exported

    tol = 1.0e-10

    ret = .Fortran("mvtdst", N = as.integer(n), 
        NU = as.integer(df), LOWER = as.double(lower), 
        UPPER = as.double(upper), INFIN = as.integer(infin),
        CORREL = as.double(corrF), DELTA = as.double(delta), 
        MAXPTS = as.integer(maxpts), ABSEPS = as.double(abseps), 
        RELEPS = as.double(releps), TOL = as.double(tol),
        error = as.double(error), value = as.double(value),
        inform = as.integer(inform), PACKAGE = "fMultivar")
    
    error = ret$error
    value = ret$value
    inform = ret$inform

    msg = NULL
    if (inform == 0) 
        msg = "Normal Completion"
    if (inform == 1) 
        msg = "Completion with error > abseps"
    if (inform == 2) 
        msg = "N greater 1000 or N < 1"
    if (inform == 3) 
        msg = "Covariance matrix not positive semidefinite"
    if (is.null(msg)) 
        msg = inform
    
    # Return Value:
    list(value = value, error = error, msg = msg)
}


# ------------------------------------------------------------------------------


.rmvt = 
function(n, sigma = diag(2), df = 1) 
{   # A modified copy from contributed R package mvtnorm Version 0.6-8

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Random Deviates:
    ans = .rmvnorm(n, sigma = sigma)/sqrt(rchisq(n, df) / df)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.qmvnorm = 
function(p, interval = c(-10, 10),  tail = c("lower.tail", "upper.tail", 
"both.tails"), mean = 0, corr = NULL, sigma = NULL, maxpts = 25000, 
abseps = 0.001, releps = 0, ...) 
{
    if (length(p) != 1 || (p <= 0 || p >= 1)) 
        stop(sQuote("p"), " is not a double between zero and one")

    tail = match.arg(tail)
    dim = length(mean)
    if (is.matrix(corr)) dim = nrow(corr)
    if (is.matrix(sigma)) dim = nrow(sigma)
    lower = rep(0, dim)
    upper = rep(0, dim)
    args = .checkmvArgs(lower, upper, mean, corr, sigma)
    dim = length(args$mean)

    pfct = function(q) {
        switch(tail, "both.tails" = {
                  low = rep(-abs(q), dim)
                  upp = rep( abs(q), dim)
           }, "upper.tail" = {
                  low = rep(      q, dim)
                  upp = rep(    Inf, dim)
           }, "lower.tail" = {
                  low = rep(   -Inf, dim)
                  upp = rep(      q, dim)
           },)
           .pmvnorm(lower = low, upper = upp, mean = args$mean,
                   corr = args$corr, sigma = args$sigma,
                   abseps = abseps, maxpts = maxpts, releps = releps) - p
    }

    if (tail == "both.tails") {
        interval[1] = 0
        interval = abs(interval)
    }

    qroot = uniroot(pfct, interval = interval, ...)
    names(qroot)[1:2] = c("quantile", "f.quantile")
    qroot
}


# ------------------------------------------------------------------------------


.qmvt = 
function(p, interval = c(-10, 10), tail = c("lower.tail", "upper.tail", 
"both.tails"), df = 1, delta = 0, corr = NULL, sigma = NULL, maxpts = 25000, 
abseps = 0.001, releps = 0, ...) 
{

    if (length(p) != 1 || (p <= 0 || p >= 1)) 
        stop(sQuote("p"), " is not a double between zero and one")

    tail = match.arg(tail)
    dim = length(mean)
    if (is.matrix(corr)) dim = nrow(corr)
    if (is.matrix(sigma)) dim = nrow(sigma)
    lower = rep(0, dim)
    upper = rep(0, dim)
    args = .checkmvArgs(lower, upper, delta, corr, sigma)
    dim = length(args$mean)

    pfct = function(q) {
        switch(tail, "both.tails" = {
                  low = rep(-abs(q), dim)
                  upp = rep( abs(q), dim)
           }, "upper.tail" = {
                  low = rep(      q, dim)
                  upp = rep(    Inf, dim)
           }, "lower.tail" = {
                  low = rep(   -Inf, dim)
                  upp = rep(      q, dim)
           },)
           .pmvt(lower = low, upper = upp, df = df, delta = args$mean,
                corr = args$corr, sigma = args$sigma,
                abseps = abseps, maxpts = maxpts, releps = releps) - p
    }

    if (tail == "both.tails") {
        interval[1] = 0
        interval = abs(interval)
    }

    qroot = uniroot(pfct, interval = interval, ...)
    names(qroot)[1:2] = c("quantile", "f.quantile")
    qroot
}


################################################################################
# BUILTIN: sn


.dsn = 
function(x, location = 0, scale = 1, shape = 0, log = FALSE)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    z = (x - location) / scale
    
    if(!log) {
        y = 2 * dnorm(z) * pnorm(z * shape) / scale
    } else {
        y = (-0.9189385332046727-logb(scale)-z^2/2+zeta(0,shape*z))
    }
    
    # Return Value:
    replace(y, scale <= 0, NaN)

}


# ------------------------------------------------------------------------------


.psn = 
function(x, location = 0, scale = 1, shape = 0, ...)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    z = (x-location)/scale
    p = pmin(1, pmax(0, pnorm(z) - 2 * .T.Owen(z, shape,...)))
   
    # Return Value:
    replace(p, scale <= 0, NaN)
}


# ------------------------------------------------------------------------------


.rsn = 
function(n = 1, location = 0, scale = 1, shape = 0)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    u1 = rnorm(n)
    u2 = rnorm(n)
    id = (u2 > shape * u1)
    u1[id] = (-u1[id])
    y = location + scale * u1
    attr(y,"parameters") = c(location, scale, shape)
    
    # Return Value:
    return(y)
}


# ------------------------------------------------------------------------------
 

.T.Owen = function(h, a, jmax = 50, cut.point = 6)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Internal Function:
    .T.int <-function(h,a,jmax,cut.point) {
        fui<- function(h,i) (h^(2*i))/((2^i)*gamma(i+1)) 
        seriesL = seriesH = NULL
        i = 0:jmax
        low = (h<=cut.point)
        hL = h[low]
        hH = h[!low]
        L = length(hL)
        if (L > 0) {
            b = outer(hL,i,fui)
            cumb = apply(b,1,cumsum)
            b1 = exp(-0.5*hL^2)*t(cumb)
            matr = matrix(1,jmax+1,L)-t(b1)
            jk = rep(c(1,-1),jmax)[1:(jmax+1)]/(2*i+1)
            matr = t(matr*jk) %*%  a^(2*i+1)
            seriesL = (atan(a)-as.vector(matr))/(2*pi)
        }
        if (length(hH) > 0) 
            seriesH = atan(a)*exp(-0.5*(hH^2)*a/atan(a)) *
            (1+0.00868*(hH^4)*a^4)/(2*pi)   
        series = c(seriesL, seriesH)
        id = c((1:length(h))[low],(1:length(h))[!low]) 
        # re-sets in original order
        series[id] = series  
        series
    }
      
    if (!is.vector(a) | length(a)>1) 
        stop("a must be a vector of length 1")
        
    if (!is.vector(h)) 
        stop("h must be a vector")
        
    aa = abs(a)    
    ah = abs(h)
    
    if (aa == Inf) 
        return(0.5 * pnorm(-ah))
    if (aa == 0)   
        return(rep(0, length(h)))
        
    na = is.na(h)
    inf = (ah == Inf)
    ah = replace(ah, (na | inf), 0)
    
    if (aa <= 1)
        owen = .T.int(ah, aa, jmax,cut.point)
    else
        owen = 0.5 * pnorm(ah) + pnorm(aa*ah) * (0.5 - pnorm(ah)) - 
            .T.int(aa*ah,(1/aa), jmax, cut.point)
            
    owen = replace(owen, na, NA)
    owen = replace(owen, inf, 0)
    
    # Return Value:
    return(owen*sign(a))
} 


# ------------------------------------------------------------------------------


.dst =  
function (x, location = 0, scale = 1, shape = 0, df = Inf)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    if (df == Inf) return(.dsn(x, location, scale, shape))
    z   = (x - location) / scale
    pdf = dt(z, df = df) / scale
    cdf = pt(shape*z*sqrt((df+1)/(z^2+df)), df = df+1)
    
    
    2 * pdf * cdf
}


# ------------------------------------------------------------------------------


.pst = 
function (x,  location = 0, scale = 1, shape = 0, df = Inf)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    if (df == Inf) {
        p =  .psn(x, location, scale, shape)
    } else {
        fp = function(v, shape, df, t)
            .psn(sqrt(v)*t, 0, 1, shape) * dchisq(v * df, df = df) * df
        z = (x - location) / scale
        p = numeric(length(z))
        for (i in 1:length(z))
            p[i] = integrate(fp, 0, Inf, shape = shape, df = df, 
                t = z[i])$value
    }
    
    # Return Value:
    p
}


# ------------------------------------------------------------------------------
  

.rst = 
function (n=1, location = 0, scale = 1, shape = 0, df = Inf)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    z = .rsn(n, location = 0, scale, shape)
    if (df == Inf) return(z + location)
    v = rchisq(n, df) / df
    y = z / sqrt(v) + location
    attr(y, "parameters") = c(location, scale, shape, df)
    
    # Return Value:
    return(y)
}


# ------------------------------------------------------------------------------


.dmsn = 
function(x, xi = rep(0, d), Omega, alpha)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Density of Multivariate SN rv with parameters (xi, Omega, alpha) 
    # evaluated at x, which is either a d-vector or (n x d) matrix
    
    scale = sqrt(diag(Omega))
    
    if (is.vector(x)) {
        n <-1     
        d = length(x)
    } else {
        n <-dim(x)[1]
        d = dim(x)[2]
    }
    
    X = t(matrix(x, nrow = n,ncol = d))- xi
    z = X/scale
    # diag of (x Omega^(-1) x^T)
    Q = apply((solve(Omega)%*% X)* X, 2, sum) 
    # d = diag(qr(Omega)[[1]])
    Det = prod(abs( diag(qr(Omega)[[1]])))
    pdf = 2*exp(-0.5*Q) * pnorm(t(z)%*%as.matrix(alpha)) / 
        sqrt((2*pi)^d * Det)
        
    # Return Value:
    pdf
}


# ------------------------------------------------------------------------------


.pmsn =  
function(x, xi = rep(0, d), Omega, alpha, ...)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
     
    d = length(alpha)
    p = .pmst(x, xi, Omega, alpha, df = Inf, ...)
    
    # Return Value:
    p
}



# ------------------------------------------------------------------------------


.rmsn = 
function(n=1, xi = rep(0, d), Omega, alpha)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # generates SN_d(xi,Omega,alpha) variates using transformation method
    
    d = ncol(Omega)
    Z = .msn.quantities(xi, Omega, alpha)
    y = matrix(rnorm(n*d), n, d) %*% chol(Z$Psi)
    # each row of y is N_d(0,Psi)
    abs.y0 = abs(rnorm(n))  
    abs.y0 = matrix(rep(abs.y0, d), ncol = d)
    delta = Z$delta
    z = delta * t(abs.y0) +  sqrt(1-delta^2) * t(y)
    y = t(xi + Z$omega*z)
    
    # Return Value:
    return(y)
}


# ------------------------------------------------------------------------------


.dmst = 
function(x, xi = rep(0, d), Omega, alpha, df = Inf)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Density of multivariate ST rv with parameters (xi, Omega, alpha, df) 
    # evaluated at x, which is either a d-vector or (n,d) matrix
    
    if (df == Inf) 
        return(dmsn(x, xi, Omega, alpha))
    if(is.vector(x)) {
        n = 1
        d = length(x)
    } else {
        n = dim(x)[1]
        d = dim(x)[2]
    }
    
    omega = sqrt(diag(Omega))
    X = t(matrix(x, nrow = n, ncol = d)) - xi
    z = X/omega
    
    # diag of (x Omega^(-1) x^T)
    Q = apply((solve(Omega) %*% X) * X, 2, sum) 
    # Det = as.numeric(det.Hermitian(as.Matrix(Omega), logarithm = F)$modulus)
    Det = abs(prod(diag(qr(Omega)$qr)))
    L = as.vector(t(z) %*% as.matrix(alpha))
    pdf.mt = (gamma((df+d)/2) / (sqrt((pi*df)^d*Det) * 
        gamma(df/2) * (1+Q/df)^((df+d)/2)))
    cdf.T = pt(L*sqrt((df+d) / (Q+df)), df + d)
    ans = 2 * pdf.mt * cdf.T
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.pmst = 
function(x, xi = rep(0, d), Omega = 1, alpha = rep(0, d), df = Inf, ...)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    Diag = function(x) diag(x,nrow=length(x),ncol=length(x))
    d = ifelse(is.matrix(Omega),nrow(Omega),1)
    Omega<- matrix(Omega,d,d) 
    omega<- sqrt(diag(Omega))
    Ocor = Diag(1/omega) %*% Omega %*% Diag(1/omega)
    #  t(Omega /omega)/omega
    O.alpha = as.vector(Ocor %*% alpha)
    delta = O.alpha/sqrt(1+sum(alpha*O.alpha))
    A = matrix(rbind(c(1,-delta),cbind(-delta,Ocor)), d+1,d+1)
    
    if (df == Inf) 
        2 * .pmvnorm(upper = c(0,(x - xi) / omega), corr = A, ...)
    else 
        2 * .pmvt(upper = c(0,(x - xi) / omega), corr = A, df = df, ...)
}


# ------------------------------------------------------------------------------


.rmst = 
function(n=1, xi=rep(0,d), Omega, alpha, df = Inf)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    d = ncol(Omega)
    if (df == Inf) x = 1 
    else x = rchisq(n,df)/df
    z = .rmsn(n, rep(0, d), Omega, alpha)
    y = t(xi+t(sqrt(x)*z))
    
    # Return Value:
    return(y)
}

# ------------------------------------------------------------------------------


.msn.quantities = 
function(xi, Omega, alpha)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # 21-12/1997; computes various quantities related to SN_k(xi,Omega,alpha)
    
    Diag = function(x) diag(x,nrow = length(x),ncol = length(x))
    k = length(alpha)
    if (length(xi)!=k | any(dim(Omega)!=c(k,k))) 
       stop("dimensions of arguments do not match")
    omega = sqrt(diag(Omega))
    O.cor = Diag(1/omega) %*% Omega %*% Diag(1/omega)
    tmp = as.vector(sqrt(1 + t(as.matrix(alpha))%*%O.cor%*%alpha)) 
    delta = as.vector(O.cor %*% alpha) / tmp
    lambda = delta/sqrt(1-delta^2)
    D = diag(sqrt(1+lambda^2))
    Psi = D %*% (O.cor-outer(delta,delta)) %*% D
    Psi = (Psi+t(Psi))/2
    O.inv = solve(Omega)
    oi = sqrt(diag(O.inv))
    O.pcor = Diag(1/oi)%*% (-O.inv) %*% Diag(1/oi)
    diag(O.pcor) = rep(1,k)
    muZ = delta*sqrt(2/pi)
    muY = xi+omega*muZ
    Sigma = Diag(omega) %*% (O.cor-outer(muZ,muZ)) %*% Diag(omega) 
    Sigma = (Sigma+t(Sigma))/2
    cv = muZ/sqrt(1-muZ^2)
    gamma1 = 0.5*(4-pi)*cv^3
    
    # Return Value:
    list(xi = xi, Omega = Omega, alpha = alpha, omega = omega,  mean = muY, 
        variance = Sigma, Omega.conc = O.inv, Omega.cor = O.cor, 
        Omega.pcor = O.pcor, lambda = lambda, Psi = Psi, delta = delta, 
        skewness = gamma1)
}


# ------------------------------------------------------------------------------


.msn.mle = 
function(X, y, freq, start, trace = FALSE, method = "BFGS", 
control = list(iter.max = 150) )
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    y = as.matrix(y)
    if (missing(X)) X = rep(1,nrow(y))
    if (missing(freq)) freq = rep(1,nrow(y))
    X = as.matrix(X) 
    k = ncol(y)  
    n = sum(freq)
    m = ncol(X)
    y.names = dimnames(y)[[2]] 
    x.names = dimnames(X)[[2]]
    if (missing(start)) {
        fit0 = lm.fit(X, y, method="qr")
        beta = as.matrix(coef(fit0))
        res = resid(fit0)
        a = .msn.moment.fit(res)
        Omega = a$Omega
        omega = a$omega
        alpha = a$alpha
        if (!a$admissible) alpha = alpha / (1+max(abs(alpha)))
        beta[1,] = beta[1,]-omega*a$delta*sqrt(2/pi)  
    } else {
        beta  = start$beta
        Omega = start$Omega
        alpha = start$alpha
        omega = sqrt(diag(Omega)) 
    }
    
    al.om = alpha / omega
    if (trace){ 
        cat("Initial parameters:\n")
        print(cbind(t(beta),al.om,Omega))
    }
    
    param = c(beta,al.om)
    dev = .msn.dev(param,X,y,freq) 
    opt = optim(param, fn = .msn.dev, gr = .msn.dev.grad, method = method,
        control = control, X = X, y = y, freq = freq, trace = trace) 
             
    if (trace) 
        cat(paste("Message from optimization routine:", opt$message,"\n"))
        
    logL = (-opt$value)/2
    beta = matrix(opt$par[1:(m*k)],m,k)
    dimnames(beta)[2] = list(y.names)
    dimnames(beta)[1] = list(x.names)
    al.om = opt$par[(m*k+1):(m*k+k)]
    xi = X %*% beta
    Omega = t(y-xi) %*% (freq*(y-xi))/n
    omega = sqrt(diag(Omega))
    alpha = al.om*omega
    param = cbind(omega,alpha)
    dimnames(Omega) = list(y.names,y.names)
    dimnames(param)[1] = list(y.names)
    info = .num.deriv(opt$par, FUN = ".msn.dev.grad", 
        X = X, y = y, freq = freq)/2
    se = sqrt(diag(solve(info)))
    se.beta = matrix(se[1:(m*k)],m,k)
    se.alpha = se[(m*k+1):(m*k+k)]*omega
    dimnames(se.beta)[2] = list(y.names)
    dimnames(se.beta)[1] = list(x.names)
    se = list(beta = se.beta, alpha = se.alpha, info = info)
    dp = list(beta = beta, Omega = Omega, alpha = alpha)
    
    # Return Value:
    list(call = match.call(), dp = dp, logL = logL, se = se, optim = opt)
}
 

# ------------------------------------------------------------------------------


.msn.dev = 
function(param, X, y, freq, trace = FALSE)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    k = ncol(y)
    # if (missing(freq)) freq = rep(1, nrow(y))
    n = sum(freq)
    m = ncol(X)
    beta = matrix(param[1:(m*k)],m,k)
    al.om = param[(m*k+1):(m*k+k)]
    y0 = y-X %*% beta
    Omega = (t(y0) %*% (y0*freq))/n  
    d = diag(qr(2*pi*Omega)[[1]])
    logDet = sum(log(abs(d)))
    dev = n*logDet-2*sum(.zeta(0,y0 %*% al.om)*freq)+n*k
    if (trace) { 
        cat("\nmsn.dev:", dev, "\n","parameters:"); 
        print(rbind(beta, al.om))
    }
    
    # Return Value:
    dev
}


# ------------------------------------------------------------------------------


.msn.dev.grad = 
function(param, X, y, freq, trace = FALSE)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    k = ncol(y)
    # if (missing(freq)) freq = rep(1, nrow(y))
    n = sum(freq)
    m = ncol(X)
    beta = matrix(param[1:(m*k)],m,k)
    al.om = param[(m*k+1):(m*k+k)]
    y0 = y-X %*% beta
    Omega = (t(y0) %*% (freq*y0))/n
    p1 = .zeta(1, as.vector(y0 %*% al.om))
    Dbeta = t(X)%*% (y0*freq) %*%solve(Omega) - 
        outer(as.vector(t(X*freq)%*%p1), al.om)
    Dal.om = as.vector(t(y0*freq) %*% p1)
    
    if (trace) {
        cat("gradient:\n")
        print(rbind(Dbeta, Dal.om))
    }
    ans = -2 * c(Dbeta,Dal.om)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.msn.moment.fit = 
function(y)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # 31-12-1997: simple fit of MSN distribution usign moments
    
    Diag = function(x) diag(x,nrow=length(x),ncol=length(x))
    y = as.matrix(y)
    k = ncol(y)
    m.y = apply(y, 2, mean)
    var.y = var(y)
    y0 = (t(y)-m.y) / sqrt(diag(var.y))
    gamma1 = apply(y0^3, 1, mean)
    out = (abs(gamma1) > 0.99527)
    gamma1[out] = sign(gamma1[out]) * 0.995
    a = sign(gamma1) * (2*abs(gamma1) / (4-pi))^0.33333
    delta = sqrt(pi/2)*a/sqrt(1+a^2)
    m.z = delta*sqrt(2/pi) 
    omega = sqrt(diag(var.y)/(1-m.z^2))
    Omega = var.y+outer(omega*m.z,omega*m.z) 
    xi = m.y-omega*m.z
    O.cor = Diag(1/omega) %*% Omega %*% Diag(1/omega)
    O.cor = (t(O.cor)+O.cor)/2
    O.inv = solve(O.cor)
    tmp = as.vector(1-t(delta) %*% O.inv %*% delta)
    if (tmp <= 0) {
        tmp = 0.0001
        admissible = FALSE
    } else {
        admissible = TRUE
    }
    alpha = as.vector(O.inv %*% delta)/sqrt(tmp)
    
    # Return Value:
    list(xi = xi, Omega = Omega, alpha = alpha, Omega.cor = O.cor, 
        omega = omega, delta = delta, skewness = gamma1, 
        admissible = admissible) 
}


# ------------------------------------------------------------------------------


.zeta = 
function(k, x)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # k integer in (0, 4)
    if (k < 0 | k > 4) 
        return(NULL)
    if (k != as.integer(k)) 
        warning("k must be an integer")
    k = as.integer(k)
    na = is.na(x)
    x = replace(x,na,0)
    z = switch(k+1, pnorm(x, log.p = TRUE) + log(2), ifelse(x>(-20), 
        dnorm(x)/pnorm(x), ifelse(x>(-200), exp(-x^2/2-0.5*log(2*pi) - 
        pnorm(x,log.p = TRUE)), - x*(1+1/x^2-2/x^4))), (-zeta(1,x) * 
        (x+zeta(1,x))), (-zeta(2,x)*(x+zeta(1,x))-zeta(1,x)*(1+zeta(2,x))),
        (-zeta(3,x)*(x+2*zeta(1,x)) - 2*zeta(2,x)*(1+zeta(2,x))), NULL)
    neg.inf = (x == -Inf)
    if (any(neg.inf))
    z = switch(k+1, z, replace(z, neg.inf, Inf), replace(z, neg.inf, 1),
        replace(z, neg.inf, 0), replace(z, neg.inf, 0), NULL)
    if (k > 1) z = replace(z, x == Inf, 0)
    ans = replace(z, na, NA)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.num.deriv = 
function(coefficients, FUN, ...)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # da rm.fit: derivate seconde numeriche, se FUN da` il gradiente
    
    FUN = get(FUN, inherit = TRUE)
    values = FUN(coefficients, ...)
    p = length(values)
    H = matrix(0, p, p)
    h = rep(0, p)
    delta = cbind((abs(coefficients) + 1.0e-10) * 1.0e-5, rep(1.0e-06, p))
    delta = apply(delta, 1, max)
    for(i in 1:p) {
        h[i] = delta[i]
        new.values = FUN(coefficients + h, ...)
        H[, i] = (new.values - values)/delta[i]
        h[i] = 0
    }
    ans = (H+t(H))/2
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.msn.marginal = 
function(xi = NULL, Omega = NULL, alpha = NULL, comp = 1:d, dp = NULL)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # calcola parametri della marginale associata a comp di un SN_d 
    
    Diag = function(x) diag(x, nrow = length(x), ncol = length(x))
    
    if (!is.null(xi) && !is.null(dp)) 
        stop("You cannot set both xi and dp")
    if (!is.null(xi) && is.list(xi) && is.null(dp)) 
        dp = xi
    if (!is.null(dp)) {
        xi = dp$xi
        Omega = dp$Omega
        alpha = dp$alpha
    }
    
    xi = as.vector(xi)
    comp = as.integer(comp)
    alpha = as.vector(alpha)
    d = length(alpha)
    
    if (length(comp) < d) {
        if (any(comp > d | comp < 1)) 
            stop("comp makes no sense")
        scale = sqrt(diag(Omega))
        O = Diag(1/scale) %*% Omega %*% Diag(1/scale)
        O11 = O[comp,comp, drop=FALSE]
        O12 = O[comp,-comp, drop=FALSE]
        O21 = O[-comp,comp, drop=FALSE]
        O22 = O[-comp,-comp, drop=FALSE]
        alpha1 = as.matrix(alpha[comp, drop=FALSE])
        alpha2 = as.matrix(alpha[-comp, drop=FALSE])
        O22.1 = O22 - O21 %*% solve(O11) %*% O12
        a.sum = as.vector(t(alpha2) %*% O22.1 %*% alpha2)
        a.new = as.vector(alpha1+solve(O11) %*% O12 %*% alpha2) /
            sqrt(1+a.sum)
        O.new = Diag(scale[comp]) %*% O11 %*% Diag(scale[comp])
        result = list(xi=xi[comp], Omega=O.new, alpha=a.new)
    } else {
        if (any(sort(comp) != (1:d))) 
            stop("comp makes no sense")
        result = list(xi = xi[comp], 
            Omega = as.matrix(Omega[comp, comp, drop = FALSE]), 
            alpha = alpha[comp]) 
    }
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


.mst.mle = 
function(X, y, freq, start = NA, fixed.df = NA, trace = FALSE, 
method = "BFGS", control = list(iter.max = 150))
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    Diag = function(x) diag(x, nrow=length(x), ncol=length(x))
    y.name  = deparse(substitute(y))
    y.names = dimnames(y)[[2]] 
    y = as.matrix(y)
    if (missing(X)) X = matrix(rep(1,nrow(y)),ncol=1) 
    if (missing(freq)) freq = rep(1,nrow(y))
    x.names = dimnames(X)[[2]]
    X = as.matrix(X) 
    d = ncol(y)  
    n = sum(freq)
    m = ncol(X)
    if (missing(start) | is.na(start)) {
        qrX   = qr(X)
        beta  = as.matrix(qr.coef(qrX,y))
        Omega = matrix(var(qr.resid(qrX,y)),d,d)
        omega = sqrt(diag(Omega))
        alpha = rep(0,d)
        if (is.na(fixed.df)) 
            df = 15 
        else 
            df = fixed.df
        if (trace) {
            cat("mst.mle: dp=","\n")
            print(c(beta,Omega,alpha))
            cat("df:", df,"\n")
        }
    } else {
        beta = start$beta
        Omega = start$Omega
        alpha = start$alpha
        df  = start$df
    } 
    
    eta = alpha / sqrt(diag(Omega))
    Oinv = solve(Omega)
    Oinv = (Oinv+t(Oinv)) / 2
    upper = chol(Oinv)
    D = diag(upper)
    A = upper / D
    D = D^2
    
    if (d > 1) 
        param = c(beta, -0.5*log(D), A[!lower.tri(A,diag = TRUE)], eta) 
    else 
        param = c(beta, -0.5*log(D), eta)
        
    if (is.na(fixed.df)) 
        param = c(param, log(df))
        
    opt = optim(param, fn = .mst.dev,  gr = .mst.dev.grad, 
        method = method,  control=control, hessian = TRUE, 
        X = X, y = y, freq = freq, trace = trace, fixed.df = fixed.df)
        
    dev = opt$value
    param = opt$par
    
    if (trace){
        cat("Message from optimization routine:", opt$message,"\n")
        cat("deviance:", dev,"\n")
    }
    
    beta = matrix(param[1:(m*d)],m,d)
    D = exp(-2*param[(m*d+1):(m*d+d)])
    
    if (d>1) {
        A = diag(d)
        A[!lower.tri(A,diag=TRUE)] = param[(m*d+d+1):(m*d+d+d*(d-1)/2)]
        i0 = m*d+d+d*(d-1)/2
    } else {
        i0 = m+1
        A = as.matrix(1)
    }
    
    eta = param[(i0+1):(i0+d)]
    
    if (is.na(fixed.df)) {
        df = exp(param[i0+d+1])
    } else {
        df = fixed.df
    }
    
    # Omega = solve(t(A) %*% diag(D) %*% A)
    Ainv = backsolve(A,diag(d))
    Omega = Ainv %*% Diag(1/D) %*% t(Ainv)
    omega = sqrt(diag(Omega))
    alpha = eta*omega
    dimnames(beta) = list(x.names,y.names)
    dimnames(Omega) = list(y.names, y.names)
    
    if (length(y.names)>0) 
        names(alpha) = y.names
        
    info = opt$hessian/2
    
    if (all(is.finite(info))) {
        qr.info = qr(info)
        info.ok = (qr.info$rank == length(param))
    } else {
        info.ok = FALSE 
    }
        
    if (info.ok) {
        se2 = diag(solve(qr.info)) 
        if (min(se2) < 0 ) {
            se = NA 
        } else {
            se = sqrt(se2)
            se.beta = matrix(se[1:(m*d)],m,d)
            se.alpha =  se[(i0+1):(i0+d)] * omega
            dimnames(se.beta)[2] = list(y.names)
            dimnames(se.beta)[1] = list(x.names)
            names(se.alpha) = y.names
            se.df = df*se[i0+d+1]
            se = list(beta = se.beta, alpha = se.alpha, df = se.df,
                internal = se, info = info)
        }
    } else {
        se = NA }
        
    dp = list(beta = beta, Omega = Omega,  alpha = alpha, df = df)
  
    # Return Value:
    list(call = match.call(), logL = -0.5*dev, deviance = dev, dp = dp, 
        se = se, optim = opt)
}


# ------------------------------------------------------------------------------


.mst.dev = 
function(param, X, y, freq = rep(1, nrow(X)), fixed.df = NA, trace = FALSE)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    Diag = function(x) diag(x, nrow = length(x), ncol = length(x))
    d = ncol(y)
    # if (missing(freq)) freq = rep(1, nrow(y))
    n = sum(freq)
    m = ncol(X)
    beta = matrix(param[1:(m*d)],m,d)
    D = exp(-2*param[(m*d+1):(m*d+d)])
    
    if (d > 1) {
        A = diag(d)
        A[!lower.tri(A,diag=TRUE)] = param[(m*d+d+1):(m*d+d+d*(d-1)/2)]
        i0 = m*d+d+d*(d-1)/2
    } else {
        i0 = m+1
        A = as.matrix(1)
    }
  
    eta = param[(i0+1):(i0+d)]
    
    if (is.na(fixed.df)) {
        df = exp(param[i0+d+1])
    } else { 
        df = fixed.df
    }
        
    Oinv = t(A) %*% Diag(D) %*% A
    
    # Omega = solve(Oinv)
    u =  y - X %*% beta
    Q = apply((u %*% Oinv)*u,1,sum)
    L = as.vector(u %*% eta) 
    logDet = sum(log(df*pi/D))
    dev = (n*(2*lgamma(df/2) + logDet - 2*lgamma((df+d)/2)) + (df+d) * 
        sum(freq * log(1+Q/df)) - 2*sum(freq * log(2*pt( L * 
        sqrt((df+d) / (Q+df)),df+d))))
        
    if (trace) 
        cat("mst.dev: ",dev, "\n")
        
    # Return Value:
    dev
}


# ------------------------------------------------------------------------------


.mst.dev.grad = 
function(param, X, y, freq = rep(1, nrow(X)), fixed.df = NA, trace = FALSE)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    Diag = function(x) diag(x, nrow=length(x), ncol=length(x))
    d = ncol(y)
    # if (missing(freq)) freq = rep(1, nrow(y))
    n = sum(freq)
    m = ncol(X)
    beta = matrix(param[1:(m*d)],m,d)
    D   = exp(-2*param[(m*d+1):(m*d+d)])
    
    if (d>1) {
        A  = diag(d)
        A[!lower.tri(A,diag=TRUE)] = param[(m*d+d+1):(m*d+d+d*(d-1)/2)]
        i0 = m*d+d+d*(d-1)/2
    } else {
        i0 = m*d+d
        A  = as.matrix(1)
    }
    
    eta   = param[(i0+1):(i0+d)]
    
    if (is.na(fixed.df)) {
        df = exp(param[i0 + d + 1])
    } else {
        df = fixed.df
    }
        
    tA = t(A)
    Oinv = tA %*% Diag(D) %*% A
    u = y-X %*% beta
    Q = as.vector(apply((u %*% Oinv)*u,1,sum))
    L = as.vector(u %*% eta) 
    t. = L*sqrt((df+d)/(Q+df))
    dlogft = -(df+d)/(2*df*(1+Q/df))
    dt.dL = sqrt((df+d)/(Q+df))
    dt.dQ = (-0.5)*L*sqrt(df+d)/(Q+df)^1.5
    T. = pt(t., df+d)
    dlogT. = dt(t., df+d)/T.
    u.freq = u*freq
    Dbeta = (-2* t(X) %*% (u.freq*dlogft) %*% Oinv - 
        outer(as.vector(t(X) %*% (dlogT. * dt.dL* freq)), eta) - 
        2 * t(X) %*% (dlogT.* dt.dQ * u.freq) %*% Oinv )
    Deta  = apply(dlogT.*sqrt((df+d)/(Q+df))*u.freq, 2, sum)
    
    if (d > 1) {
        M  = 2*( Diag(D) %*% A %*% t(u * dlogft + u * dlogT. * 
            dt.dQ) %*% u.freq)
        DA = M[!lower.tri(M,diag=TRUE)]
    } else {
        DA = NULL
    } 
    
    M = ( A %*% t(u*dlogft + u*dlogT.*dt.dQ) %*% u.freq %*% tA)
    
    if (d > 1) {
        DD = diag(M) + 0.5*n/D
    } else {
        DD = as.vector(M + 0.5*n/D) 
    }
    
    grad = (-2)*c(Dbeta,DD*(-2*D), DA, Deta)
    
    # browser()  
    if (is.na(fixed.df)) {  
        dlogft.ddf = 0.5 * (digamma((df+d)/2) - digamma(df/2) - 
            d/df + (df+d)*Q/((1+Q/df)*df^2) - log(1+Q/df))
        eps = 1.0e-4
        T.eps = pt(L*sqrt((df+eps+d) / (Q+df+eps)), df+eps+d)
        dlogT.ddf = (log(T.eps) - log(T.)) / eps
        Ddf = sum((dlogft.ddf + dlogT.ddf) * freq)
        grad = c(grad, -2 * Ddf * df)
    }
    
    if (trace)  
        cat("mst.dev.grad: norm is ",sqrt(sum(grad^2)),"\n")  
    
    # Return Value:
    return(grad)
}


# ------------------------------------------------------------------------------


.dsn2.plot = 
function(x, y, xi, Omega, alpha, ...)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # plot bivariate density SN_2(xi,Omega,alpha) computed at (x,y) grid
    if (any(dim(Omega)!=c(2,2))) 
        stop("dim(Omega) != c(2,2)")
        
    nx = length(x)
    ny = length(y)
    xoy = cbind(rep(x, ny), as.vector(matrix(y, nx, ny, byrow = TRUE)))
    X = matrix(xoy, nx*ny, 2, byrow = FALSE)
    pdf = .dmsn(X, xi, Omega, alpha)
    pdf = matrix(pdf, nx, ny)
    contour(x, y, pdf, ...)
    
    # Return Value:
    invisible(list(x = x, y = y, density = pdf, xi = xi, Omega = Omega, 
        alpha = alpha))
}


# ------------------------------------------------------------------------------


.dst2.plot = 
function(x, y, xi, Omega, alpha, df, ...)
{   # A modified copy from contributed R package mvtnorm Version 0.32-2

    # Description:
    #   Internal Function
    
    # FUNCTION:

    # plot bivariate density ST_2(xi,Omega,alpha,df) computed at (x,y) grid
    if(any(dim(Omega) != c(2, 2))) 
        stop("dim(Omega) != c(2,2)")
    nx = length(x)
    ny = length(y)
    xoy = cbind(rep(x, ny), as.vector(matrix(y, nx, ny, byrow = TRUE)))
    X = matrix(xoy, nx * ny, 2, byrow = FALSE)
    pdf = .dmst(X, xi, Omega, alpha, df)
    pdf = matrix(pdf, nx, ny)
    contour(x, y, pdf, ...)
    
    # Return Value:
    invisible(list(x = x, y = y, density = pdf, xi = xi, Omega = Omega,
        alpha = alpha, df = df))
}


# ##############################################################################

