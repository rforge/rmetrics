
################################################################################

# Package: ismev
# Version: 1.2
# Date: 2006-03-10
# Title: An Introduction to Statistical Modeling of Extreme Values  
# Author: Original S functions by Stuart Coles, R port and R 
#   documentation files by Alec Stephenson.
# Maintainer: Alec Stephenson <alec_stephenson@hotmail.com>
# Depends: R (>= 1.5.0)
# Description: Functions to support the computations carried out in
#   `An Introduction to Statistical Modeling of Extreme Values' by
#   Stuart Coles. The functions may be divided into the following 
#   groups; maxima/minima, order statistics, peaks over thresholds
#   and point processes.  
# License: GPL (Version 2 or above)
# URL: http://www.maths.lancs.ac.uk/~stephena/
# Packaged: Thu Mar  9 19:50:18 2006; stephena

################################################################################


# This file contains the following functions:
# gev.fit  gev.diag  gev.pp  gev.qq  gev.rl  gev.his
# gevf  gevq  gev.dens  gev.profxi  gev.prof

"gev.fit"<-
function(xdat, ydat = NULL, mul = NULL, sigl = NULL, shl = NULL, mulink = identity, siglink = identity, shlink = identity, show = TRUE, method = "Nelder-Mead", maxit = 10000, ...)
{
#
# obtains mles etc for gev distn
#
    z <- list()
        npmu <- length(mul) + 1
        npsc <- length(sigl) + 1
        npsh <- length(shl) + 1
    z$trans <- FALSE    # if maximization fails, could try
# changing in1 and in2 which are 
# initial values for minimization routine
    in2 <- sqrt(6 * var(xdat))/pi
    in1 <- mean(xdat) - 0.57722 * in2
    if(is.null(mul)) {
        mumat <- as.matrix(rep(1, length(xdat)))
        muinit <- in1
    }
    else {
        z$trans <- TRUE
        mumat <- cbind(rep(1, length(xdat)), ydat[, mul])
        muinit <- c(in1, rep(0, length(mul)))
    }
    if(is.null(sigl)) {
        sigmat <- as.matrix(rep(1, length(xdat)))
        siginit <- in2
    }
    else {
        z$trans <- TRUE
        sigmat <- cbind(rep(1, length(xdat)), ydat[, sigl])
        siginit <- c(in2, rep(0, length(sigl)))
    }
    if(is.null(shl)) {
        shmat <- as.matrix(rep(1, length(xdat)))
        shinit <- 0.1
    }
    else {
        z$trans <- TRUE
        shmat <- cbind(rep(1, length(xdat)), ydat[, shl])
        shinit <- c(0.1, rep(0, length(shl)))
    }
    z$model <- list(mul, sigl, shl)
    z$link <- deparse(substitute(c(mulink, siglink, shlink)))
    init <- c(muinit, siginit, shinit)
        gev.lik <- function(a) {
        # computes neg log lik of gev model
        mu <- mulink(mumat %*% (a[1:npmu]))
        sc <- siglink(sigmat %*% (a[seq(npmu + 1, length = npsc)]))
    xi <- shlink(shmat %*% (a[seq(npmu + npsc + 1, length = npsh)]))
    y <- (xdat - mu)/sc
    y <- 1 + xi * y
    if(any(y <= 0) || any(sc <= 0)) return(10^6)
    sum(log(sc)) + sum(y^(-1/xi)) + sum(log(y) * (1/xi + 1))
        }
    x <- optim(init, gev.lik, hessian = TRUE, method = method,
                   control = list(maxit = maxit, ...))
    z$conv <- x$convergence
        mu <- mulink(mumat %*% (x$par[1:npmu]))
    sc <- siglink(sigmat %*% (x$par[seq(npmu + 1, length = npsc)]))
    xi <- shlink(shmat %*% (x$par[seq(npmu + npsc + 1, length = npsh)]))
    z$nllh <- x$value
    z$data <- xdat
    if(z$trans) {
        z$data <-  - log(as.vector((1 + (xi * (xdat - mu))/sc)^(
            -1/xi)))
    }
    z$mle <- x$par
        z$cov <- solve(x$hessian)
    z$se <- sqrt(diag(z$cov))
    z$vals <- cbind(mu, sc, xi)
        if(show) {
        if(z$trans)
        print(z[c(2, 3, 4)])
        else print(z[4])
        if(!z$conv)
                print(z[c(5, 7, 9)])
    }
    invisible(z)
}

"gev.diag"<-
function(z)
{
#
# produces diagnostic plots for output of
# gev.fit stored in z
#
    n <- length(z$data)
    x <- (1:n)/(n + 1)
    if(z$trans) {
            oldpar <- par(mfrow = c(1, 2))
            plot(x, exp( - exp( - sort(z$data))), xlab = 
                "Empirical", ylab = "Model")
            abline(0, 1, col = 4)
            title("Residual Probability Plot")
            plot( - log( - log(x)), sort(z$data), ylab = 
                "Empirical", xlab = "Model")
            abline(0, 1, col = 4)
            title("Residual Quantile Plot (Gumbel Scale)")
        }
        else {
            oldpar <- par(mfrow = c(2, 2))
            gev.pp(z$mle, z$data)
            gev.qq(z$mle, z$data)
            gev.rl(z$mle, z$cov, z$data)
            gev.his(z$mle, z$data)
        }
        par(oldpar)
        invisible()
}

"gev.pp"<-
function(a, dat)
{
#
# sub-function for gev.diag
# produces probability plot
#
    plot((1:length(dat))/length(dat), gevf(a, sort(dat)), xlab = 
        "Empirical", ylab = "Model", main = "Probability Plot")
    abline(0, 1, col = 4)
}

"gev.qq"<-
function(a, dat)
{
#
# function called by gev.diag
# produces quantile plot
#
    plot(gevq(a, 1 - (1:length(dat)/(length(dat) + 1))), sort(dat), ylab = 
        "Empirical", xlab = "Model", main = "Quantile Plot")
    abline(0, 1, col = 4)
}

"gev.rl"<-
function(a, mat, dat)
{
#
# function called by gev.diag
# produces return level curve and 95 % confidence intervals
# on usual scale
#
    eps <- 1e-006
    a1 <- a
    a2 <- a
    a3 <- a
    a1[1] <- a[1] + eps
    a2[2] <- a[2] + eps
    a3[3] <- a[3] + eps
    f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 
        0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
    q <- gevq(a, 1 - f)
    d1 <- (gevq(a1, 1 - f) - q)/eps
    d2 <- (gevq(a2, 1 - f) - q)/eps
    d3 <- (gevq(a3, 1 - f) - q)/eps
    d <- cbind(d1, d2, d3)
    v <- apply(d, 1, q.form, m = mat)
    plot(-1/log(f), q, log = "x", type = "n", xlim = c(0.1, 1000), ylim = c(
        min(dat, q), max(dat, q)), xlab = "Return Period", ylab = 
        "Return Level")
    title("Return Level Plot")
    lines(-1/log(f), q)
    lines(-1/log(f), q + 1.96 * sqrt(v), col = 4)
    lines(-1/log(f), q - 1.96 * sqrt(v), col = 4)
    points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))
}

"gev.his"<-
function(a, dat)
{
#
# Plots histogram of data and fitted density
# for output of gev.fit stored in z
#
    h <- hist(dat, prob = TRUE, plot = FALSE)
    if(a[3] < 0) {
        x <- seq(min(h$breaks), min(max(h$breaks), (a[1] - a[2]/a[3] - 
            0.001)), length = 100)
    }
    else {
        x <- seq(max(min(h$breaks), (a[1] - a[2]/a[3] + 0.001)), max(h$
            breaks), length = 100)
    }
    y <- gev.dens(a, x)
    hist(dat, prob = TRUE, ylim = c(0, max(y)), xlab = "z", ylab = "f(z)", 
        main = "Density Plot")
    points(dat, rep(0, length(dat)))
    lines(x, y)
}

"gevf"<-
function(a, z)
{
#
# ancillary function calculates gev dist fnc
#
    if(a[3] != 0) exp( - (1 + (a[3] * (z - a[1]))/a[2])^(-1/a[3])) else 
            gum.df(z, a[1], a[2])
}

"gevq"<-
function(a, p)
{
    if(a[3] != 0)
        a[1] + (a[2] * (( - log(1 - p))^( - a[3]) - 1))/a[3]
    else gum.q(p, a[1], a[2])
}

"gev.dens"<-
function(a, z)
{
#
# evaluates gev density with parameters a at z
#
    if(a[3] != 0) (exp( - (1 + (a[3] * (z - a[1]))/a[2])^(-1/a[3])) * (1 + (
            a[3] * (z - a[1]))/a[2])^(-1/a[3] - 1))/a[2] else {
        gum.dens(c(a[1], a[2]), z)
    }
}

"gev.profxi"<-
function(z, xlow, xup, conf = 0.95, nint = 100)
{
#
# plots profile log-likelihood for shape parameter
# in gev model
#
    cat("If routine fails, try changing plotting interval", fill = TRUE)
    v <- numeric(nint)
    x <- seq(xup, xlow, length = nint)
    sol <- c(z$mle[1], z$mle[2])
        gev.plikxi <- function(a) {
        # computes profile neg log lik
        if (abs(xi) < 10^(-6)) {
                y <- (z$data - a[1])/a[2]
                if(a[2] <= 0) l <- 10^6
                else l <- length(y) * log(a[2]) + sum(exp(-y)) + sum(y)
        }
        else {
        y <- (z$data - a[1])/a[2]
        y <- 1 + xi * y
        if(a[2] <= 0 || any(y <= 0))
            l <- 10^6
        else l <- length(y) * log(a[2]) + sum(y^(-1/xi)) + sum(log(y
            )) * (1/xi + 1)
    }
    l
        }
    for(i in 1:nint) {
        xi <- x[i]
        opt <- optim(sol, gev.plikxi)
        sol <- opt$par ; v[i] <- opt$value
    }
    plot(x,  - v, type = "l", xlab = "Shape Parameter", ylab = 
        "Profile Log-likelihood")
    ma <-  - z$nllh
    abline(h = ma, col = 4)
    abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)
    invisible()
}

"gev.prof"<-
function(z, m, xlow, xup, conf = 0.95, nint = 100)
{
#
# plots profile log likelihood for m 'year' return level
# in gev model
#
        if(m <= 1) stop("`m' must be greater than one")
    cat("If routine fails, try changing plotting interval", fill = TRUE)
    p <- 1/m
    v <- numeric(nint)
    x <- seq(xlow, xup, length = nint)
    sol <- c(z$mle[2], z$mle[3])
        gev.plik <- function(a) {
        # computes profile neg log lik
        if (abs(a[2]) < 10^(-6)) {
                mu <- xp + a[1] * log(-log(1 - p))
                y <- (z$data - mu)/a[1]
                if(is.infinite(mu) || a[1] <= 0) l <- 10^6
                else l <- length(y) * log(a[1]) + sum(exp(-y)) + sum(y)
        }
    else {
                mu <- xp - a[1]/a[2] * (( - log(1 - p))^( - a[2]) - 1)
        y <- (z$data - mu)/a[1]
        y <- 1 + a[2] * y
                if(is.infinite(mu) || a[1] <= 0 || any(y <= 0))
            l <- 10^6
        else l <- length(y) * log(a[1]) + sum(y^(-1/a[2])) + sum(log(
                y)) * (1/a[2] + 1)
    }
    l
        }
        for(i in 1:nint) {
                xp <- x[i]
        opt <- optim(sol, gev.plik)
        sol <- opt$par ; v[i] <- opt$value 
    }
    plot(x,  - v, type = "l", xlab = "Return Level", ylab = 
        " Profile Log-likelihood")
    ma <-  - z$nllh
    abline(h = ma, col = 4)
    abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)
    invisible()
}







# This file contains the following functions:
# gpd.fitrange  gpd.fit  gpd.diag  gpd.pp  gpd.qq  gpd.rl
# gpd.his  gpdf  gpdq  gpdq2  gpd.dens  gpd.profxi  gpd.prof

"gpd.fitrange"<-
function(data, umin, umax, nint = 10, show = FALSE)
{
#
# computes mle's in gpd model, adjusted for threshold, 
# over range of threshold choices.
#
    m <- s <- up <- ul <- matrix(0, nrow = nint, ncol = 2)
    u <- seq(umin, umax, length = nint)
    for(i in 1:nint) {
        z <- gpd.fit(data, u[i], show = show)
        m[i,  ] <- z$mle
        m[i, 1] <- m[i, 1] - m[i, 2] * u[i]
        d <- matrix(c(1,  - u[i]), ncol = 1)
        v <- t(d) %*% z$cov %*% d
        s[i,  ] <- z$se
        s[i, 1] <- sqrt(v)
        up[i,  ] <- m[i,  ] + 1.96 * s[i,  ]
        ul[i,  ] <- m[i,  ] - 1.96 * s[i,  ]
    }
    names <- c("Modified Scale", "Shape")
    oldpar <- par(mfrow = c(2, 1))
    for(i in 1:2) {
        um <- max(up[, i])
        ud <- min(ul[, i])
        plot(u, m[, i], ylim = c(ud, um), xlab = "Threshold", ylab = 
            names[i], type = "b")
        for(j in 1:nint)
            lines(c(u[j], u[j]), c(ul[j, i], up[j, i]))
    }
        par(oldpar)
        invisible()
}

"gpd.fit"<-
function(xdat, threshold, npy = 365, ydat = NULL, sigl = NULL, shl = NULL, siglink = identity, shlink = identity, show = TRUE, method = "Nelder-Mead", maxit = 10000, ...)
{
# 
# obtains mles etc for gpd model
#
    z <- list()
        npsc <- length(sigl) + 1
    npsh <- length(shl) + 1
        n <- length(xdat)
    z$trans <- FALSE
    if(is.function(threshold))
            stop("`threshold' cannot be a function")
    u <- rep(threshold, length.out = n)
        if(length(unique(u)) > 1) z$trans <- TRUE
    xdatu <- xdat[xdat > u]
    xind <- (1:n)[xdat > u]
    u <- u[xind]
    in2 <- sqrt(6 * var(xdat))/pi
    in1 <- mean(xdat, na.rm = TRUE) - 0.57722 * in2
    if(is.null(sigl)) {
        sigmat <- as.matrix(rep(1, length(xdatu)))
        siginit <- in2
    }
    else {
        z$trans <- TRUE
        sigmat <- cbind(rep(1, length(xdatu)), ydat[xind, sigl])
        siginit <- c(in2, rep(0, length(sigl)))
    }
    if(is.null(shl)) {
        shmat <- as.matrix(rep(1, length(xdatu)))
        shinit <- 0.1
    }
    else {
        z$trans <- TRUE
        shmat <- cbind(rep(1, length(xdatu)), ydat[xind, shl])
        shinit <- c(0.1, rep(0, length(shl)))
    }
    init <- c(siginit, shinit)
    z$model <- list(sigl, shl)
    z$link <- deparse(substitute(c(siglink, shlink)))
        z$threshold <- threshold
    z$nexc <- length(xdatu)
    z$data <- xdatu 
        gpd.lik <- function(a) {
        # calculates gpd neg log lik
    sc <- siglink(sigmat %*% (a[seq(1, length = npsc)]))
    xi <- shlink(shmat %*% (a[seq(npsc + 1, length = npsh)]))
    y <- (xdatu - u)/sc
    y <- 1 + xi * y
    if(min(sc) <= 0)
        l <- 10^6
    else {
        if(min(y) <= 0)
            l <- 10^6
        else {
            l <- sum(log(sc)) + sum(log(y) * (1/xi + 1))
        }
    }
    l
        }
        x <- optim(init, gpd.lik, hessian = TRUE, method = method,
                   control = list(maxit = maxit, ...))
    sc <- siglink(sigmat %*% (x$par[seq(1, length = npsc)]))
    xi <- shlink(shmat %*% (x$par[seq(npsc + 1, length = npsh)]))
    z$conv <- x$convergence
    z$nllh <- x$value
    z$vals <- cbind(sc, xi, u)
    if(z$trans) {
        z$data <-  - log(as.vector((1 + (xi * (xdatu - u))/sc)^(-1/xi))
            )
    }
    z$mle <- x$par
    z$rate <- length(xdatu)/n
        z$cov <- solve(x$hessian)
    z$se <- sqrt(diag(z$cov))
    z$n <- n
    z$npy <- npy
    z$xdata <- xdat
        if(show) {
        if(z$trans)
        print(z[c(2, 3)])
        if(length(z[[4]]) == 1)
        print(z[4])
        print(z[c(5, 7)])
        if(!z$conv)
        print(z[c(8, 10, 11, 13)])
        }
    invisible(z)
}

"gpd.diag"<-
function(z)
{
#
# produces diagnostic plots for gpd model
# estimated using gpd.fit with output stored in z
#
    n <- length(z$data)
    x <- (1:n)/(n + 1)
        if(z$trans) {
            oldpar <- par(mfrow = c(1, 2))
            plot(x, 1 - exp( - sort(z$data)), xlab = "Empirical", 
                ylab = "Model")
            abline(0, 1, col = 4)
            title("Residual Probability Plot")
            plot( - log(1 - x), sort(z$data), ylab = "Empirical", 
                xlab = "Model")
            abline(0, 1, col = 4)
            title("Residual Quantile Plot (Exptl. Scale)")
        }
        else {
            oldpar <- par(mfrow = c(2, 2))
            gpd.pp(z$mle, z$threshold, z$data)
            gpd.qq(z$mle, z$threshold, z$data)
            gpd.rl(z$mle, z$threshold, z$rate, z$n, z$npy, z$cov, z$
                data, z$xdata)
            gpd.his(z$mle, z$threshold, z$data)
        }
        par(oldpar)
        invisible()
}

"gpd.pp"<-
function(a, u, dat)
{
# 
# function called by gpd.diag
# produces probability plot for gpd model
#
    plot((1:length(dat))/length(dat), gpdf(a, u, sort(dat)), xlab = 
        "Empirical", ylab = "Model", main = "Probability Plot")
    abline(0, 1, col = 4)
}

"gpd.qq"<-
function(a, u, dat)
{
#
# function called by gpd.diag
# produces quantile plot for gpd model
#
    plot(gpdq(a, u, 1 - (1:length(dat)/(length(dat) + 1))), sort(dat), ylab
         = "Empirical", xlab = "Model", main = "Quantile Plot")
    abline(0, 1, col = 4)
}

"gpd.rl"<-
function(a, u, la, n, npy, mat, dat, xdat)
{
#
# function called by gpd.diag
# produces return level curve and 95% confidence intervals
# for fitted gpd model
    a <- c(la, a)
    eps <- 1e-006
    a1 <- a
    a2 <- a
    a3 <- a
    a1[1] <- a[1] + eps
    a2[2] <- a[2] + eps
    a3[3] <- a[3] + eps
    jj <- seq(-1, 3.75 + log10(npy), by = 0.1)
    m <- c(1/la, 10^jj)
    q <- gpdq2(a[2:3], u, la, m)
    d1 <- (gpdq2(a1[2:3], u, la, m) - q)/eps
    d2 <- (gpdq2(a2[2:3], u, la, m) - q)/eps
    d3 <- (gpdq2(a3[2:3], u, la, m) - q)/eps
    d <- cbind(d1, d2, d3)
    mat <- matrix(c((la * (1 - la))/n, 0, 0, 0, mat[1, 1], mat[1, 2], 0, 
        mat[2, 1], mat[2, 2]), nc = 3)
    v <- apply(d, 1, q.form, m = mat)
    plot(m/npy, q, log = "x", type = "n", xlim = c(0.1, max(m)/npy), ylim
         = c(u, max(xdat, q[q > u - 1] + 1.96 * sqrt(v)[q > u - 1])), 
        xlab = "Return period (years)", ylab = "Return level", main = 
        "Return Level Plot")
    lines(m[q > u - 1]/npy, q[q > u - 1])
    lines(m[q > u - 1]/npy, q[q > u - 1] + 1.96 * sqrt(v)[q > u - 1], col
         = 4)
    lines(m[q > u - 1]/npy, q[q > u - 1] - 1.96 * sqrt(v)[q > u - 1], col
         = 4)
    nl <- n - length(dat) + 1
    sdat <- sort(xdat)
    points((1/(1 - (1:n)/(n + 1))/npy)[sdat > u], sdat[sdat > u])   
    #   points(1/(1 - (1:n)/(n + 1))/npy, 
#       sort(xdat))
#   abline(h = u, col = 3)
}

"gpd.his"<-
function(a, u, dat)
{
#
# function called by gpd.diag
# produces histogram and density plot
#
    h <- hist(dat, prob = TRUE, plot = FALSE)
    x <- seq(u, max(h$breaks), length = 100)
    y <- gpd.dens(a, u, x)
    hist(dat, prob = TRUE, ylim = c(0, max(y)), xlab = "x", ylab = "f(x)", 
        main = "Density Plot")
    lines(x, y, col = 4)
}

"gpdf"<-
function(a, u, z)
{
#
# ancillary function
# calculates gpd distribution function
#
    1 - (1 + (a[2] * (z - u))/a[1])^(-1/a[2])
}

"gpdq"<-
function(a, u, p)
u + (a[1] * (p^( - a[2])    #
# ancillary function
# computes gpd quantiles
#
 - 1))/a[2]

"gpdq2"<-
function(a, u, la, m)
{
#
# ancillary function
# calculates quantiles of gpd model
#
    u + (a[1] * ((m * la)^(a[2]) - 1))/a[2]
}

"gpd.dens"<-
function(a, u, z)
{
#
# ancillary function computes gpd density
#
    (1 + (a[2] * (z - u))/a[1])^(-1/a[2] - 1)/a[1]
}

"gpd.profxi"<-
function(z, xlow, xup, conf = 0.95, nint = 100)
{
#
# plots profile log likelihood for shape parameter
# in gpd model
#
    cat("If routine fails, try changing plotting interval", fill = TRUE)
    xdat <- z$data ; u <- z$threshold
    v <- numeric(nint)
    x <- seq(xup, xlow, length = nint)
    sol <- z$mle[1]
        gpd.plikxi <- function(a) {
        # calculates profile log lik
    if(abs(xi) < 10^(-4)) l <- length(xdat) * log(a) + sum(xdat - u)/a
         else {
        y <- (xdat - u)/a
        y <- 1 + xi * y
                if(any(y <= 0) || a <= 0)
            l <- 10^6
        else l <- length(xdat) * log(a) + sum(log(y)) * (1/xi + 1)
    }
    l
        }
    for(i in 1:nint) {
        xi <- x[i]
        opt <- optim(sol, gpd.plikxi, method = "BFGS")
        sol <- opt$par ; v[i] <- opt$value 
    }
    plot(x,  - v, type = "l", xlab = "Shape Parameter", ylab = 
        "Profile Log-likelihood")
    ma <-  - z$nllh
    abline(h = ma, lty = 1)
    abline(h = ma - 0.5 * qchisq(conf, 1), lty = 1)
    invisible()
}



"gpd.prof"<-
function(z, m, xlow, xup, npy = 365, conf = 0.95, nint = 100)
{
#
# plots profile log-likelihood for m-year return level
# in gpd model
#
    cat("If routine fails, try changing plotting interval", fill = TRUE)
        xdat <- z$data ; u <- z$threshold ; la <- z$rate
    v <- numeric(nint)
    x <- seq(xlow, xup, length = nint)
        m <- m * npy
    sol <- z$mle[2]
        gpd.plik <- function(a) {
        # calculates profile neg log lik
        if(m != Inf) sc <- (a * (xp - u))/((m * la)^a - 1) else sc <- (u - xp)/
            a
    if(abs(a) < 10^(-4))
        l <- length(xdat) * log(sc) + sum(xdat - u)/sc
    else {
        y <- (xdat - u)/sc
        y <- 1 + a * y
                if(any(y <= 0) || sc <= 0)
            l <- 10^6
        else l <- length(xdat) * log(sc) + sum(log(y)) * (1/a + 1)
    }
    l
        }
    for(i in 1:nint) {
        xp <- x[i]
        opt <- optim(sol, gpd.plik, method = "BFGS")
        sol <- opt$par ; v[i] <- opt$value
    }
    plot(x,  - v, type = "l", xlab = "Return Level", ylab = 
        "Profile Log-likelihood")
    ma <-  - z$nllh
    abline(h = ma)
    abline(h = ma - 0.5 * qchisq(conf, 1))
    invisible()
}





# This file contains the following functions:
# gum.fit  gum.diag  gum.rl  gum.df  gum.q  gum.dens

"gum.fit"<-
function(xdat, ydat = NULL, mul = NULL, sigl = NULL, mulink = identity, siglink = identity, show = TRUE, method = "Nelder-Mead", maxit = 10000, ...)
{
#
# finds mles etc for gumbel model
#
    z <- list()
        npmu <- length(mul) + 1
        npsc <- length(sigl) + 1
    z$trans <- FALSE
    in2 <- sqrt(6 * var(xdat))/pi
    in1 <- mean(xdat) - 0.57722 * in2
    if(is.null(mul)) {
        mumat <- as.matrix(rep(1, length(xdat)))
        muinit <- in1
    }
    else {
        z$trans <- TRUE
        mumat <- cbind(rep(1, length(xdat)), ydat[, mul])
        muinit <- c(in1, rep(0, length(mul)))
    }
    if(is.null(sigl)) {
        sigmat <- as.matrix(rep(1, length(xdat)))
        siginit <- in2
    }
    else {
        z$trans <- TRUE
        sigmat <- cbind(rep(1, length(xdat)), ydat[, sigl])
        siginit <- c(in2, rep(0, length(sigl)))
    }
    z$model <- list(mul, sigl)
    z$link <- c(deparse(substitute(mulink)), deparse(substitute(siglink)))
    init <- c(muinit, siginit)
        gum.lik <- function(a) {
        # calculates neg log lik of gumbel model
    mu <- mulink(mumat %*% (a[1:npmu]))
    sc <- siglink(sigmat %*% (a[seq(npmu + 1, length = npsc)]))
        if(any(sc <= 0)) return(10^6)
    y <- (xdat - mu)/sc
        sum(log(sc)) + sum(y) + sum(exp( - y))
        }
    x <- optim(init, gum.lik, hessian = TRUE, method = method,
                   control = list(maxit = maxit, ...))
    z$conv <- x$convergence
        if(!z$conv) {
                mu <- mulink(mumat %*% (x$par[1:npmu]))
            sc <- siglink(sigmat %*% (x$par[seq(npmu + 1, length = npsc)]))
            z$nllh <- x$value
            z$data <- xdat
            if(z$trans) {
                z$data <- as.vector((xdat - mu)/sc)
            }
            z$mle <- x$par
                z$cov <- solve(x$hessian)
            z$se <- sqrt(diag(z$cov))
            z$vals <- cbind(mu, sc)
        }
        if(show) {
        if(z$trans)
        print(z[c(2, 3, 4)])
        else print(z[4])
        if(!z$conv)
                print(z[c(5, 7, 9)])
        }
    invisible(z)
}

"gum.diag"<-
function(z)
{
#
# produces diagnostic plots for output of
# gum.fit stored in z
#
    z$mle <- c(z$mle, 0)
    n <- length(z$data)
    x <- (1:n)/(n + 1)
    if(z$trans) {
            oldpar <- par(mfrow = c(1, 2))
            plot(x, exp( - exp( - sort(z$data))), xlab = "empirical",
                     ylab = "model")
            abline(0, 1, col = 4)
            title("Residual Probability Plot")
            plot( - log( - log(x)), sort(z$data), xlab = 
                "empirical", ylab = "model")
            abline(0, 1, col = 4)
            title("Residual Quantile Plot (Gumbel Scale)")
    }
        else {
            oldpar <- par(mfrow = c(2, 2))
            gev.pp(z$mle, z$data)
            gev.qq(z$mle, z$data)
            gum.rl(z$mle, z$cov, z$data)
            gev.his(z$mle, z$data)
        }
        par(oldpar)
        invisible()
}

"gum.rl"<-
function(a, mat, dat)
{
#
# function called by gum.diag
# produces return level curve and 95 % confidence intervals
# on usual scale for gumbel model
#
    eps <- 1e-006
    a1 <- a
    a2 <- a
    a1[1] <- a[1] + eps
    a2[2] <- a[2] + eps
    f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 
        0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
    q <- gevq(a, 1 - f)
    d1 <- (gevq(a1, 1 - f) - q)/eps
    d2 <- (gevq(a2, 1 - f) - q)/eps
    d <- cbind(d1, d2)
    v <- apply(d, 1, q.form, m = mat)
    plot(-1/log(f), q, log = "x", type = "n", xlim = c(0.1, 1000), ylim = c(
        min(dat, q), max(dat, q)), xlab = "Return Period", ylab = 
        "Return Level")
    title("Return Level Plot")
    lines(-1/log(f), q)
    lines(-1/log(f), q + 1.96 * sqrt(v), col = 4)
    lines(-1/log(f), q - 1.96 * sqrt(v), col = 4)
    points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))
}

"gum.df"<-
function(x, a, b)
{
#
# ancillary function calculates dist fnc of gumbel model
#
    exp( - exp( - (x - a)/b))
}

"gum.q"<-
function(x, a, b)
{
#
# ancillary routine
# calculates quantiles of gumbel distn
#
    a - b * log( - log(1 - x))
}

"gum.dens"<-
function(a, x)
{
#
# ancillary function calculates density for gumbel model
#
    y <- (x - a[1])/a[2]
    (exp( - y) * exp( - exp( - y)))/a[2]
}






# This file contains the following functions:
# identity  q.form  mrl.plot

"identity"<-
function(x)
x

"q.form"<-
function(d, m)
{
#
# ancillary routine
# evaluates quadratic forms
#
    t(as.matrix(d)) %*% m %*% as.matrix(d)
}

"mrl.plot"<-
function(data, umin = min(data), umax = max(data) - 0.1, conf = 0.95, nint = 
    100)
{
#
# function to produce empirical mean residual life plot
# as function of threshold.
# confidence intervals included as well.
#
    x <- xu <- xl <- numeric(nint)
    u <- seq(umin, umax, length = nint)
    for(i in 1:nint) {
        data <- data[data > u[i]]
        x[i] <- mean(data - u[i])
        sdev <- sqrt(var(data))
        n <- length(data)
        xu[i] <- x[i] + (qnorm((1 + conf)/2) * sdev)/sqrt(n)
        xl[i] <- x[i] - (qnorm((1 + conf)/2) * sdev)/sqrt(n)
    }
    plot(u, x, type = "l", xlab = "u", ylab = "Mean Excess", ylim = c(min(
        xl[!is.na(xl)]), max(xu[!is.na(xu)])))
    lines(u[!is.na(xl)], xl[!is.na(xl)], lty = 2)
    lines(u[!is.na(xu)], xu[!is.na(xu)], lty = 2)
}

# This file contains the following functions:
# pp.fitrange  pp.fit  pp.diag  pp.pp  pp.qq
# ppf  ppq  ppp

"pp.fitrange"<-
function(data, umin, umax, npy = 365, nint = 10, show = FALSE)
{
#
# produces estimates and 95% confidence intervals
# for point process model across range of thresholds
#
        m <- s <- up <- ul <- matrix(0, nrow = nint, ncol = 3)
    u <- seq(umin, umax, length = nint)
    for(i in 1:nint) {
        z <- pp.fit(data, u[i], npy, show = show)
        m[i,  ] <- z$mle
        s[i,  ] <- z$se
        up[i,  ] <- z$mle + 1.96 * z$se
        ul[i,  ] <- z$mle - 1.96 * z$se
    }
    names <- c("Location", "Scale", "Shape")
    oldpar <- par(mfrow = c(1, 3))
    for(i in 1:3) {
        um <- max(up[, i])
        ud <- min(ul[, i])
        plot(u, m[, i], ylim = c(ud, um), xlab = "Threshold", ylab = 
            names[i], type = "b")
        for(j in 1:nint)
            lines(c(u[j], u[j]), c(ul[j, i], up[j, i]))
    }
        par(oldpar)
        invisible()
}

"pp.fit"<-
function(xdat, threshold, npy = 365, ydat = NULL, mul = NULL, sigl = NULL, shl = NULL, mulink = identity, siglink = identity, shlink = identity, show = TRUE, method = "Nelder-Mead", maxit = 10000, ...)
{
    z <- list()
        npmu <- length(mul) + 1
    npsc <- length(sigl) + 1
    npsh <- length(shl) + 1
        n <- length(xdat)
    z$trans <- FALSE
    if(is.function(threshold)) 
            stop("`threshold' cannot be a function")
    u <- rep(threshold, length.out = n)
    if(length(unique(u)) > 1) z$trans <- TRUE
    xdatu <- xdat[xdat > u]
    xind <- (1:n)[xdat > u]
    u <- u[xind]
    in2 <- sqrt(6 * var(xdat))/pi
    in1 <- mean(xdat) - 0.57722 * in2
    if(is.null(mul)) {
        mumat <- as.matrix(rep(1, length(xdatu)))
        muinit <- in1
    }
    else {
        z$trans <- TRUE
        mumat <- cbind(rep(1, length(xdatu)), ydat[xind, mul])
        muinit <- c(in1, rep(0, length(mul)))
    }
    if(is.null(sigl)) {
        sigmat <- as.matrix(rep(1, length(xdatu)))
        siginit <- in2
    }
    else {
        z$trans <- TRUE
        sigmat <- cbind(rep(1, length(xdatu)), ydat[xind, sigl])
        siginit <- c(in2, rep(0, length(sigl)))
    }
    if(is.null(shl)) {
        shmat <- as.matrix(rep(1, length(xdatu)))
        shinit <- 0.1
    }
    else {
        z$trans <- TRUE
        shmat <- cbind(rep(1, length(xdatu)), ydat[xind, shl])
        shinit <- c(0.1, rep(0, length(shl)))
    }
    init <- c(muinit, siginit, shinit)
    z$model <- list(mul, sigl, shl)
    z$link <- deparse(substitute(c(mulink, siglink, shlink)))
        z$threshold <- threshold
    z$npy <- npy
    z$nexc <- length(xdatu)
    z$data <- xdatu
        pp.lik <- function(a) {
    mu <- mulink(mumat %*% (a[1:npmu]))
    sc <- siglink(sigmat %*% (a[seq(npmu + 1, length = npsc)]))
    xi <- shlink(shmat %*% (a[seq(npmu + npsc + 1, length = npsh)]))
        if(any(sc <= 0)) return(10^6)
    if(min(1 + ((xi * (u - mu))/sc)) < 0) {
        l <- 10^6
    }
    else {
        y <- (xdatu - mu)/sc
        y <- 1 + xi * y
        if(min(y) <= 0)
            l <- 10^6
        else l <- sum(log(sc)) + sum(log(y) * (1/xi + 1)) + n/npy * 
                mean((1 + (xi * (u - mu))/sc)^(-1/xi))
    }
    l
        }
    x <- optim(init, pp.lik, hessian = TRUE, method = method,
                   control = list(maxit = maxit, ...))
        mu <- mulink(mumat %*% (x$par[1:npmu]))
    sc <- siglink(sigmat %*% (x$par[seq(npmu + 1, length = npsc)]))
    xi <- shlink(shmat %*% (x$par[seq(npmu + npsc + 1, length = npsh)]))
    z$conv <- x$convergence
    z$nllh <- x$value
    z$vals <- cbind(mu, sc, xi, u)
    z$gpd <- apply(z$vals, 1, ppp, npy)
    if(z$trans) {
        z$data <- as.vector((1 + (xi * (xdatu - u))/z$gpd[2,  ])^(-1/xi
            ))
    }
    z$mle <- x$par
        z$cov <- solve(x$hessian)
    z$se <- sqrt(diag(z$cov))
        if(show) {
        if(z$trans)
        print(z[c(2, 3)])
        if(length(z[[4]]) == 1)
        print(z[4])
        print(z[c(5, 6, 8)])
        if(!z$conv)
        print(z[c(9, 12, 14)])
        }
        invisible(z)
}

"pp.diag"<-
function(z)
{
    n <- length(z$data)
    x <- (1:n)/(n + 1)
    if(z$trans) {
        oldpar <- par(mfrow = c(1, 2))
        plot(x, sort(z$data), xlab = "empirical", ylab = "model")
        abline(0, 1, col = 3)
        title("Residual Probability Plot")
        plot( - log(1 - x),  - log(1 - sort(z$data)), ylab = 
            "empirical", xlab = "model")
        abline(0, 1, col = 3)
        title("Residual quantile Plot (Exptl. Scale)")
    }
    else {
        oldpar <- par(mfrow = c(1, 2), pty = "s")
        pp.pp(z$mle, z$threshold, z$npy, z$data)
        pp.qq(z$mle, z$threshold, z$npy, z$data)
    }
    par(oldpar)
    invisible()
}

"pp.pp"<-
function(a, u, npy, dat)
{
#
# function called by pp.diag
# produces probability plot
#
    y <- apply(as.matrix(sort(dat)), 1, ppf, a = a, u = u, npy = npy)
    plot((1:length(dat))/length(dat), y, xlab = "empirical", ylab = "model",
        main = "Probability plot")
    abline(0, 1, col = 4)
}

"pp.qq"<-
function(a, u, npy, dat)
{
#
# function called by pp.diag
# computes quantile plot
#
    y <- apply(as.matrix((length(dat):1/(length(dat) + 1))), 1, ppq, a = a, 
        u = u, npy = npy)
    plot(y, sort(dat), ylab = "empirical", xlab = "model", main = 
        "Quantile Plot")
    abline(0, 1, col = 4)
}

"ppf"<-
function(a, z, u, npy)
{
#
# ancillary function
# calculates distribution function in point process model
#
    b <- ppp(c(a, u), npy)
    1 - (1 + (b[3] * (z - u))/b[2])^(-1/b[3])
}

"ppq"<-
function(a, u, npy, p)
{
#
# ancillary routine
# finds quantiles in point process model
#
    b <- ppp(c(a, u), npy)
    u + (b[2] * (((p))^( - b[3]) - 1))/b[3]
}

"ppp"<-
function(a, npy)
{
    u <- a[4]
    la <- 1 - exp( - (1 + (a[3] * (u - a[1]))/a[2])^(-1/a[3])/npy)
    sc <- a[2] + a[3] * (u - a[1])
    xi <- a[3]
    c(la, sc, xi)
}









# This file contains the following functions:
# rlarg.fit  rlarg.diag  rlarg.pp  rlarg.qq
# rlargf  rlargq  rlargq2

"rlarg.fit"<-
function(xdat, r = dim(xdat)[2], ydat = NULL, mul = NULL, sigl = NULL, shl = NULL, mulink = identity, siglink = identity, shlink = identity, show = TRUE, method = "Nelder-Mead", maxit = 10000, ...)
{
#
# calculates mles etc for rlargest order statistic model
#
    z <- list()
        npmu <- length(mul) + 1
        npsc <- length(sigl) + 1
        npsh <- length(shl) + 1
        z$trans <- FALSE
    in2 <- sqrt(6 * var(xdat[, 1]))/pi
    in1 <- mean(xdat[, 1]) - 0.57722 * in2
    if(is.null(mul)) {
        mumat <- as.matrix(rep(1, dim(xdat)[1]))
        muinit <- in1
    }
    else {
        z$trans <- TRUE
        mumat <- cbind(rep(1, dim(xdat)[1]), ydat[, mul])
        muinit <- c(in1, rep(0, length(mul)))
    }
    if(is.null(sigl)) {
        sigmat <- as.matrix(rep(1, dim(xdat)[1]))
        siginit <- in2
    }
    else {
        z$trans <- TRUE
        sigmat <- cbind(rep(1, dim(xdat)[1]), ydat[, sigl])
        siginit <- c(in2, rep(0, length(sigl)))
    }
    if(is.null(shl)) {
        shmat <- as.matrix(rep(1, dim(xdat)[1]))
        shinit <- 0.1
    }
    else {
        z$trans <- TRUE
        shmat <- cbind(rep(1, dim(xdat)[1]), ydat[, shl])
        shinit <- c(0.1, rep(0, length(shl)))
    }
        xdatu <- xdat[, 1:r, drop = FALSE]
        init <- c(muinit, siginit, shinit)
    z$model <- list(mul, sigl, shl)
    z$link <- deparse(substitute(c(mulink, siglink, shlink)))
        u <- apply(xdatu, 1, min, na.rm = TRUE)
        rlarg.lik <- function(a) {
        # calculates neg log lik
    mu <- mulink(drop(mumat %*% (a[1:npmu])))
    sc <- siglink(drop(sigmat %*% (a[seq(npmu + 1, length = npsc)])))
    xi <- shlink(drop(shmat %*% (a[seq(npmu + npsc + 1, length = npsh)])))
        if(any(sc <= 0)) return(10^6)
        y <- 1 + xi * (xdatu - mu)/sc
    if(min(y, na.rm = TRUE) <= 0)
        l <- 10^6
    else {
                y <- (1/xi+1) * log(y) + log(sc)
                y <- rowSums(y, na.rm = TRUE)
                l <- sum((1 + xi * (u - mu)/sc)^(-1/xi) + y)
        }
    l
        }
    x <- optim(init, rlarg.lik, hessian = TRUE, method = method,
                   control = list(maxit = maxit, ...))
        mu <- mulink(drop(mumat %*% (x$par[1:npmu])))
    sc <- siglink(drop(sigmat %*% (x$par[seq(npmu + 1, length = npsc)])))
    xi <- shlink(drop(shmat %*% (x$par[seq(npmu + npsc + 1, length = npsh)])))
    z$conv <- x$convergence
    z$nllh <- x$value
    z$data <- xdat
    if(z$trans) {
        for(i in 1:r)
            z$data[, i] <-  - log((1 + (as.vector(xi) * (xdat[, i] - 
                as.vector(mu)))/as.vector(sc))^(-1/as.vector(xi
                )))
    }
    z$mle <- x$par
        z$cov <- solve(x$hessian)
    z$se <- sqrt(diag(z$cov))
    z$vals <- cbind(mu, sc, xi)
    z$r <- r
        if(show) {
        if(z$trans)
        print(z[c(2, 3)])
        print(z[4])
        if(!z$conv)
        print(z[c(5, 7, 9)])
        }
    invisible(z)
}

"rlarg.diag"<-
function(z, n = z$r)
{
#
# takes output from rlarg.fit
# produces probability and quantile plots for
# each order statistic
#
    z2 <- z
    z2$data <- z$data[, 1]
        oldpar <- par(ask = TRUE, mfcol = c(2, 2))
    if(z$trans) {
        for(i in 1:n) {
            rlarg.pp(c(0, 1, 0), z$data[, 1:z$r], i)
            rlarg.qq(c(0, 1, 0), z$data[, 1:z$r], i)
        }
    }
    else {
        gev.diag(z2)
        for(i in 1:n) {
            rlarg.pp(z$mle, z$data, i)
            rlarg.qq(z$mle, z$data, i)
        }
    }
    par(oldpar)
    invisible()
}

"rlarg.pp"<-
function(a, dat, k)
{
#
# ancillary function
# calculates probability plot in r largest model
#
    da <- dat[!is.na(dat[, k]), k]
    plot((1:length(da))/length(da), rlargf(a, sort(da), k), xlab = "", ylab
         = "")
    title(paste("k=", k, sep = ""), cex = 0.7)
    abline(0, 1, col = 4)
}

"rlarg.qq"<-
function(a, dat, k)
{
#
# ancillary function
# calculates quantile plot in r largest model
#
    da <- dat[!is.na(dat[, k]), k]
    plot(rlargq(a, 1 - (1:length(da)/(length(da) + 1)), k, da), sort(da), 
        xlab = "", ylab = "")
    title(paste("k=", k, sep = ""), cex = 0.7)
    abline(0, 1, col = 4)
}

"rlargf"<-
function(a, z, k)
{
#
# ancillary function
# calculates dist fnc in r largest model
#
    eps <- 10^(-6)
    res <- NULL
    if(abs(a[3]) < eps)
        tau <- exp( - (z - a[1])/a[2])
    else tau <- (1 + (a[3] * (z - a[1]))/a[2])^(-1/a[3])
    for(i in 1:length(tau)) {
        if(is.na(tau[i]))
            res[i] <- 1
        else res[i] <- exp( - tau[i]) * sum(tau[i]^(0:(k - 1))/gamma(1:(
                k)))
    }
    res
}

"rlargq"<-
function(a, p, k, dat)
{
#
# ancillary routine 
# for finding quantiles in r largest model
    res <- NULL
    for(i in 1:length(p)) {
        inter <- c(min(dat) - 1, max(dat) + 1)
        res[i] <- uniroot(rlargq2, inter, a = a, kk = k, p = p[i])$root
    }
    res
}

"rlargq2"<-
function(x, a, kk, p)
{
#
# ancillary routine
# for finding quantiles in r largest model
#
    res <- rlargf(a, x, kk) - (1 - p)
    res
}



"dowjones" <-
structure(list(Date = structure(c(810777600, 810864000, 810950400, 
811036800, 811123200, 811382400, 811468800, 811555200, 811641600, 
811728000, 811987200, 812073600, 812160000, 812246400, 812332800, 
812592000, 812678400, 812764800, 812851200, 812937600, 813196800, 
813283200, 813369600, 813456000, 813542400, 813801600, 813888000, 
813974400, 814060800, 814147200, 814406400, 814492800, 814579200, 
814665600, 814752000, 815011200, 815097600, 815184000, 815270400, 
815356800, 815616000, 815702400, 815788800, 815875200, 815961600, 
816220800, 816307200, 816393600, 816480000, 816566400, 816825600, 
816912000, 816998400, 817084800, 817171200, 817430400, 817516800, 
817603200, 817689600, 817776000, 818035200, 818121600, 818208000, 
818294400, 818380800, 818640000, 818726400, 818812800, 818899200, 
818985600, 819244800, 819331200, 819417600, 819504000, 819590400, 
819849600, 819936000, 820022400, 820108800, 820195200, 820454400, 
820540800, 820627200, 820713600, 820800000, 821059200, 821145600, 
821232000, 821318400, 821404800, 821664000, 821750400, 821836800, 
821923200, 822009600, 822268800, 822355200, 822441600, 822528000, 
822614400, 822873600, 822960000, 823046400, 823132800, 823219200, 
823478400, 823564800, 823651200, 823737600, 823824000, 824083200, 
824169600, 824256000, 824342400, 824428800, 824688000, 824774400, 
824860800, 824947200, 825033600, 825292800, 825379200, 825465600, 
825552000, 825638400, 825897600, 825984000, 826070400, 826156800, 
826243200, 826502400, 826588800, 826675200, 826761600, 826848000, 
827107200, 827193600, 827280000, 827366400, 827452800, 827712000, 
827798400, 827884800, 827971200, 828057600, 828316800, 828403200, 
828489600, 828576000, 828662400, 828921600, 829008000, 829094400, 
829180800, 829267200, 829526400, 829612800, 829699200, 829785600, 
829872000, 830131200, 830217600, 830304000, 830390400, 830476800, 
830736000, 830822400, 830908800, 830995200, 831081600, 831340800, 
831427200, 831513600, 831600000, 831686400, 831945600, 832032000, 
832118400, 832204800, 832291200, 832550400, 832636800, 832723200, 
832809600, 832896000, 833155200, 833241600, 833328000, 833414400, 
833500800, 833760000, 833846400, 833932800, 834019200, 834105600, 
834364800, 834451200, 834537600, 834624000, 834710400, 834969600, 
835056000, 835142400, 835228800, 835315200, 835574400, 835660800, 
835747200, 835833600, 835920000, 836179200, 836265600, 836352000, 
836438400, 836524800, 836784000, 836870400, 836956800, 837043200, 
837129600, 837388800, 837475200, 837561600, 837648000, 837734400, 
837993600, 838080000, 838166400, 838252800, 838339200, 838598400, 
838684800, 838771200, 838857600, 838944000, 839203200, 839289600, 
839376000, 839462400, 839548800, 839808000, 839894400, 839980800, 
840067200, 840153600, 840412800, 840499200, 840585600, 840672000, 
840758400, 841017600, 841104000, 841190400, 841276800, 841363200, 
841622400, 841708800, 841795200, 841881600, 841968000, 842227200, 
842313600, 842400000, 842486400, 842572800, 842832000, 842918400, 
843004800, 843091200, 843177600, 843436800, 843523200, 843609600, 
843696000, 843782400, 844041600, 844128000, 844214400, 844300800, 
844387200, 844646400, 844732800, 844819200, 844905600, 844992000, 
845251200, 845337600, 845424000, 845510400, 845596800, 845856000, 
845942400, 846028800, 846115200, 846201600, 846460800, 846547200, 
846633600, 846720000, 846806400, 847065600, 847152000, 847238400, 
847324800, 847411200, 847670400, 847756800, 847843200, 847929600, 
848016000, 848275200, 848361600, 848448000, 848534400, 848620800, 
848880000, 848966400, 849052800, 849139200, 849225600, 849484800, 
849571200, 849657600, 849744000, 849830400, 850089600, 850176000, 
850262400, 850348800, 850435200, 850694400, 850780800, 850867200, 
850953600, 851040000, 851299200, 851385600, 851472000, 851558400, 
851644800, 851904000, 851990400, 852076800, 852163200, 852249600, 
852508800, 852595200, 852681600, 852768000, 852854400, 853113600, 
853200000, 853286400, 853372800, 853459200, 853718400, 853804800, 
853891200, 853977600, 854064000, 854323200, 854409600, 854496000, 
854582400, 854668800, 854928000, 855014400, 855100800, 855187200, 
855273600, 855532800, 855619200, 855705600, 855792000, 855878400, 
856137600, 856224000, 856310400, 856396800, 856483200, 856742400, 
856828800, 856915200, 857001600, 857088000, 857347200, 857433600, 
857520000, 857606400, 857692800, 857952000, 858038400, 858124800, 
858211200, 858297600, 858556800, 858643200, 858729600, 858816000, 
858902400, 859161600, 859248000, 859334400, 859420800, 859507200, 
859766400, 859852800, 859939200, 860025600, 860112000, 860371200, 
860457600, 860544000, 860630400, 860716800, 860976000, 861062400, 
861148800, 861235200, 861321600, 861580800, 861667200, 861753600, 
861840000, 861926400, 862185600, 862272000, 862358400, 862444800, 
862531200, 862790400, 862876800, 862963200, 863049600, 863136000, 
863395200, 863481600, 863568000, 863654400, 863740800, 8.64e+08, 
864086400, 864172800, 864259200, 864345600, 864604800, 864691200, 
864777600, 864864000, 864950400, 865209600, 865296000, 865382400, 
865468800, 865555200, 865814400, 865900800, 865987200, 866073600, 
866160000, 866419200, 866505600, 866592000, 866678400, 866764800, 
867024000, 867110400, 867196800, 867283200, 867369600, 867628800, 
867715200, 867801600, 867888000, 867974400, 868233600, 868320000, 
868406400, 868492800, 868579200, 868838400, 868924800, 869011200, 
869097600, 869184000, 869443200, 869529600, 869616000, 869702400, 
869788800, 870048000, 870134400, 870220800, 870307200, 870393600, 
870652800, 870739200, 870825600, 870912000, 870998400, 871257600, 
871344000, 871430400, 871516800, 871603200, 871862400, 871948800, 
872035200, 872121600, 872208000, 872467200, 872553600, 872640000, 
872726400, 872812800, 873072000, 873158400, 873244800, 873331200, 
873417600, 873676800, 873763200, 873849600, 873936000, 874022400, 
874281600, 874368000, 874454400, 874540800, 874627200, 874886400, 
874972800, 875059200, 875145600, 875232000, 875491200, 875577600, 
875664000, 875750400, 875836800, 876096000, 876182400, 876268800, 
876355200, 876441600, 876700800, 876787200, 876873600, 876960000, 
877046400, 877305600, 877392000, 877478400, 877564800, 877651200, 
877910400, 877996800, 878083200, 878169600, 878256000, 878515200, 
878601600, 878688000, 878774400, 878860800, 879120000, 879206400, 
879292800, 879379200, 879465600, 879724800, 879811200, 879897600, 
879984000, 880070400, 880329600, 880416000, 880502400, 880588800, 
880675200, 880934400, 881020800, 881107200, 881193600, 881280000, 
881539200, 881625600, 881712000, 881798400, 881884800, 882144000, 
882230400, 882316800, 882403200, 882489600, 882748800, 882835200, 
882921600, 883008000, 883094400, 883353600, 883440000, 883526400, 
883612800, 883699200, 883958400, 884044800, 884131200, 884217600, 
884304000, 884563200, 884649600, 884736000, 884822400, 884908800, 
885168000, 885254400, 885340800, 885427200, 885513600, 885772800, 
885859200, 885945600, 886032000, 886118400, 886377600, 886464000, 
886550400, 886636800, 886723200, 886982400, 887068800, 887155200, 
887241600, 887328000, 887587200, 887673600, 887760000, 887846400, 
887932800, 888192000, 888278400, 888364800, 888451200, 888537600, 
888796800, 888883200, 888969600, 889056000, 889142400, 889401600, 
889488000, 889574400, 889660800, 889747200, 890006400, 890092800, 
890179200, 890265600, 890352000, 890611200, 890697600, 890784000, 
890870400, 890956800, 891216000, 891302400, 891388800, 891475200, 
891561600, 891820800, 891907200, 891993600, 892080000, 892166400, 
892425600, 892512000, 892598400, 892684800, 892771200, 893030400, 
893116800, 893203200, 893289600, 893376000, 893635200, 893721600, 
893808000, 893894400, 893980800, 894240000, 894326400, 894412800, 
894499200, 894585600, 894844800, 894931200, 895017600, 895104000, 
895190400, 895449600, 895536000, 895622400, 895708800, 895795200, 
896054400, 896140800, 896227200, 896313600, 896400000, 896659200, 
896745600, 896832000, 896918400, 897004800, 897264000, 897350400, 
897436800, 897523200, 897609600, 897868800, 897955200, 898041600, 
898128000, 898214400, 898473600, 898560000, 898646400, 898732800, 
898819200, 899078400, 899164800, 899251200, 899337600, 899424000, 
899683200, 899769600, 899856000, 899942400, 900028800, 900288000, 
900374400, 900460800, 900547200, 900633600, 900892800, 900979200, 
901065600, 901152000, 901238400, 901497600, 901584000, 901670400, 
901756800, 901843200, 902102400, 902188800, 902275200, 902361600, 
902448000, 902707200, 902793600, 902880000, 902966400, 903052800, 
903312000, 903398400, 903484800, 903571200, 903657600, 903916800, 
904003200, 904089600, 904176000, 904262400, 904521600, 904608000, 
904694400, 904780800, 904867200, 905126400, 905212800, 905299200, 
905385600, 905472000, 905731200, 905817600, 905904000, 905990400, 
906076800, 906336000, 906422400, 906508800, 906595200, 906681600, 
906940800, 907027200, 907113600, 907200000, 907286400, 907545600, 
907632000, 907718400, 907804800, 907891200, 908150400, 908236800, 
908323200, 908409600, 908496000, 908755200, 908841600, 908928000, 
909014400, 909100800, 909360000, 909446400, 909532800, 909619200, 
909705600, 909964800, 910051200, 910137600, 910224000, 910310400, 
910569600, 910656000, 910742400, 910828800, 910915200, 911174400, 
911260800, 911347200, 911433600, 911520000, 911779200, 911865600, 
911952000, 912038400, 912124800, 912384000, 912470400, 912556800, 
912643200, 912729600, 912988800, 913075200, 913161600, 913248000, 
913334400, 913593600, 913680000, 913766400, 913852800, 913939200, 
914198400, 914284800, 914371200, 914457600, 914544000, 914803200, 
914889600, 914976000, 915062400, 915148800, 915408000, 915494400, 
915580800, 915667200, 915753600, 916012800, 916099200, 916185600, 
916272000, 916358400, 916617600, 916704000, 916790400, 916876800, 
916963200, 917222400, 917308800, 917395200, 917481600, 917568000, 
917827200, 917913600, 9.18e+08, 918086400, 918172800, 918432000, 
918518400, 918604800, 918691200, 918777600, 919036800, 919123200, 
919209600, 919296000, 919382400, 919641600, 919728000, 919814400, 
919900800, 919987200, 920246400, 920332800, 920419200, 920505600, 
920592000, 920851200, 920937600, 921024000, 921110400, 921196800, 
921456000, 921542400, 921628800, 921715200, 921801600, 922060800, 
922147200, 922233600, 922320000, 922406400, 922665600, 922752000, 
922838400, 922924800, 923011200, 923270400, 923356800, 923443200, 
923529600, 923616000, 923875200, 923961600, 924048000, 924134400, 
924220800, 924480000, 924566400, 924652800, 924739200, 924825600, 
925084800, 925171200, 925257600, 925344000, 925430400, 925689600, 
925776000, 925862400, 925948800, 926035200, 926294400, 926380800, 
926467200, 926553600, 926640000, 926899200, 926985600, 927072000, 
927158400, 927244800, 927504000, 927590400, 927676800, 927763200, 
927849600, 928108800, 928195200, 928281600, 928368000, 928454400, 
928713600, 928800000, 928886400, 928972800, 929059200, 929318400, 
929404800, 929491200, 929577600, 929664000, 929923200, 930009600, 
930096000, 930182400, 930268800, 930528000, 930614400, 930700800, 
930787200, 930873600, 931132800, 931219200, 931305600, 931392000, 
931478400, 931737600, 931824000, 931910400, 931996800, 932083200, 
932342400, 932428800, 932515200, 932601600, 932688000, 932947200, 
933033600, 933120000, 933206400, 933292800, 933552000, 933638400, 
933724800, 933811200, 933897600, 934156800, 934243200, 934329600, 
934416000, 934502400, 934761600, 934848000, 934934400, 935020800, 
935107200, 935366400, 935452800, 935539200, 935625600, 935712000, 
935971200, 936057600, 936144000, 936230400, 936316800, 936576000, 
936662400, 936748800, 936835200, 936921600, 937180800, 937267200, 
937353600, 937440000, 937526400, 937785600, 937872000, 937958400, 
938044800, 938131200, 938390400, 938476800, 938563200, 938649600, 
938736000, 938995200, 939081600, 939168000, 939254400, 939340800, 
939600000, 939686400, 939772800, 939859200, 939945600, 940204800, 
940291200, 940377600, 940464000, 940550400, 940809600, 940896000, 
940982400, 941068800, 941155200, 941414400, 941500800, 941587200, 
941673600, 941760000, 942019200, 942105600, 942192000, 942278400, 
942364800, 942624000, 942710400, 942796800, 942883200, 942969600, 
943228800, 943315200, 943401600, 943488000, 943574400, 943833600, 
943920000, 944006400, 944092800, 944179200, 944438400, 944524800, 
944611200, 944697600, 944784000, 945043200, 945129600, 945216000, 
945302400, 945388800, 945648000, 945734400, 945820800, 945907200, 
945993600, 946252800, 946339200, 946425600, 946512000, 946598400, 
946857600, 946944000, 947030400, 947116800, 947203200, 947462400, 
947548800, 947635200, 947721600, 947808000, 948067200, 948153600, 
948240000, 948326400, 948412800, 948672000, 948758400, 948844800, 
948931200, 949017600, 949276800, 949363200, 949449600, 949536000, 
949622400, 949881600, 949968000, 950054400, 950140800, 950227200, 
950486400, 950572800, 950659200, 950745600, 950832000, 951091200, 
951177600, 951264000, 951350400, 951436800, 951696000, 951782400, 
951868800, 951955200, 952041600, 952300800, 952387200, 952473600, 
952560000, 952646400, 952905600, 952992000, 953078400, 953164800, 
953251200, 953510400, 953596800, 953683200, 953769600, 953856000, 
954115200, 954201600, 954288000, 954374400, 954460800, 954720000, 
954806400, 954892800, 954979200, 955065600, 955324800, 955411200, 
955497600, 955584000, 955670400, 955929600, 956016000, 956102400, 
956188800, 956275200, 956534400, 956620800, 956707200, 956793600, 
956880000, 957139200, 957225600, 957312000, 957398400, 957484800, 
957744000, 957830400, 957916800, 958003200, 958089600, 958348800, 
958435200, 958521600, 958608000, 958694400, 958953600, 959040000, 
959126400, 959212800, 959299200, 959558400, 959644800, 959731200, 
959817600, 959904000, 960163200, 960249600, 960336000, 960422400, 
960508800, 960768000, 960854400, 960940800, 961027200, 961113600, 
961372800, 961459200, 961545600, 961632000, 961718400, 961977600, 
962064000, 962150400, 962236800, 962323200, 962582400, 962668800, 
962755200, 962841600, 962928000, 963187200, 963273600, 963360000, 
963446400, 963532800, 963792000, 963878400, 963964800, 964051200, 
964137600, 964396800, 964483200, 964569600, 964656000, 964742400, 
965001600, 965088000, 965174400, 965260800, 965347200, 965606400, 
965692800, 965779200, 965865600, 965952000, 966211200, 966297600, 
966384000, 966470400, 966556800, 966816000, 966902400, 966988800, 
967075200, 967161600, 967420800, 967507200, 967593600, 967680000, 
967766400, 968025600, 968112000, 968198400, 968284800), class = c("POSIXt", 
"POSIXct")), Index = c(4704.94, 4747.21, 4765.52, 4801.8, 4797.57, 
4780.41, 4767.04, 4792.69, 4767.4, 4764.15, 4769.93, 4765.6, 
4762.35, 4787.64, 4789.08, 4761.26, 4749.7, 4740.67, 4762.71, 
4769.21, 4726.22, 4720.8, 4735.25, 4764.88, 4793.78, 4784.38, 
4795.94, 4777.52, 4802.45, 4794.86, 4755.48, 4783.66, 4753.68, 
4703.82, 4741.75, 4756.57, 4755.48, 4766.68, 4808.59, 4825.57, 
4814.01, 4797.03, 4852.67, 4864.23, 4870.37, 4872.9, 4871.81, 
4922.75, 4969.36, 4989.95, 4983.09, 5023.55, 5041.61, 5041.61, 
5048.84, 5070.88, 5078.1, 5105.56, 5074.49, 5087.13, 5139.52, 
5177.45, 5199.13, 5159.39, 5156.86, 5184.32, 5174.92, 5216.47, 
5182.15, 5176.73, 5075.21, 5109.89, 5059.32, 5096.53, 5097.97, 
5097.97, 5110.26, 5105.92, 5095.8, 5117.12, 5117.12, 5177.45, 
5194.07, 5173.84, 5181.43, 5197.68, 5130.13, 5032.94, 5065.1, 
5061.12, 5043.78, 5088.22, 5066.9, 5124.35, 5184.68, 5219.36, 
5192.27, 5242.84, 5216.83, 5271.75, 5304.98, 5381.21, 5395.3, 
5405.06, 5373.99, 5407.59, 5459.61, 5492.12, 5539.45, 5541.62, 
5600.15, 5601.23, 5579.55, 5551.37, 5503.32, 5503.32, 5458.53, 
5515.97, 5608.46, 5630.49, 5565.1, 5549.21, 5506.21, 5485.62, 
5536.56, 5600.15, 5642.42, 5629.77, 5641.69, 5470.45, 5581, 5583.89, 
5568.72, 5586.06, 5584.97, 5683.6, 5669.51, 5655.42, 5626.88, 
5636.64, 5643.86, 5670.6, 5626.88, 5630.85, 5587.14, 5637.72, 
5671.68, 5689.74, 5682.88, 5682.88, 5594.37, 5560.41, 5485.98, 
5487.07, 5532.59, 5592.92, 5620.02, 5549.93, 5551.74, 5535.48, 
5564.74, 5588.59, 5553.9, 5566.91, 5567.99, 5573.41, 5569.08, 
5575.22, 5498.27, 5478.03, 5464.31, 5420.95, 5474.06, 5475.14, 
5518.14, 5582.6, 5624.71, 5625.44, 5635.05, 5687.5, 5748.82, 
5736.26, 5778, 5762.12, 5762.86, 5762.86, 5709.67, 5673.83, 5693.41, 
5643.18, 5624.71, 5665.71, 5697.48, 5667.19, 5697.11, 5687.87, 
5668.66, 5668.29, 5657.95, 5649.45, 5652.78, 5628.03, 5648.35, 
5659.43, 5705.23, 5717.79, 5719.27, 5682.7, 5677.53, 5654.63, 
5729.98, 5720.38, 5703.02, 5703.02, 5588.14, 5550.83, 5581.86, 
5603.65, 5520.5, 5510.56, 5349.51, 5358.76, 5376.88, 5464.18, 
5426.82, 5390.94, 5346.55, 5354.69, 5422.01, 5473.06, 5434.59, 
5481.93, 5528.91, 5594.75, 5679.83, 5674.28, 5696.11, 5718.67, 
5713.49, 5681.31, 5704.98, 5647.28, 5666.88, 5665.78, 5689.45, 
5699.44, 5721.26, 5689.82, 5733.47, 5722.74, 5693.89, 5711.27, 
5712.38, 5647.65, 5616.21, 5616.21, 5648.39, 5656.9, 5606.96, 
5659.86, 5733.84, 5727.18, 5754.92, 5771.94, 5838.52, 5889.2, 
5888.83, 5877.36, 5867.74, 5888.46, 5894.74, 5874.03, 5877.36, 
5868.85, 5872.92, 5882.17, 5904.9, 5933.97, 5932.85, 5992.86, 
5979.81, 5966.77, 5930.62, 5921.67, 5969.38, 6010, 6004.78, 6020.81, 
6059.2, 6094.23, 6090.87, 6061.8, 6036.46, 5992.48, 6007.02, 
5972.73, 6007.02, 5993.23, 6029.38, 6021.93, 6041.68, 6081.18, 
6177.71, 6206.04, 6219.82, 6255.6, 6266.04, 6274.24, 6313, 6348.03, 
6346.91, 6397.6, 6430.02, 6418.47, 6471.76, 6547.79, 6528.41, 
6499.34, 6499.34, 6521.7, 6521.7, 6442.69, 6422.94, 6437.1, 6381.94, 
6463.94, 6473.25, 6402.52, 6303.71, 6304.87, 6268.35, 6308.33, 
6346.77, 6473.64, 6484.4, 6489.02, 6522.85, 6522.85, 6546.68, 
6560.91, 6549.37, 6448.27, 6448.27, 6442.49, 6544.09, 6567.18, 
6600.66, 6549.48, 6625.67, 6703.79, 6709.18, 6762.29, 6726.88, 
6765.37, 6833.1, 6843.87, 6883.9, 6850.03, 6755.75, 6696.48, 
6660.69, 6656.08, 6740.74, 6823.86, 6813.09, 6806.16, 6833.48, 
6746.9, 6773.06, 6855.8, 6806.54, 6858.11, 6961.63, 7022.44, 
6988.96, 6988.96, 7067.46, 7020.13, 6927.38, 6931.62, 7008.2, 
7037.83, 6983.18, 6925.07, 6877.74, 6918.92, 6852.72, 6945.85, 
6944.7, 7000.89, 7079.39, 7085.16, 7039.37, 6878.89, 6935.46, 
6955.48, 6896.56, 6877.68, 6820.28, 6804.79, 6905.25, 6876.17, 
6880.7, 6740.59, 6740.59, 6583.48, 6611.05, 6517.01, 6477.35, 
6526.07, 6555.91, 6609.16, 6563.84, 6540.05, 6391.69, 6451.9, 
6587.16, 6679.87, 6658.6, 6703.55, 6660.21, 6833.59, 6812.72, 
6792.25, 6738.87, 6783.02, 6962.03, 7008.99, 6976.48, 7071.2, 
7214.49, 7225.32, 7085.65, 7136.62, 7169.53, 7292.75, 7274.21, 
7286.16, 7333.55, 7194.67, 7228.88, 7303.46, 7290.69, 7258.13, 
7345.91, 7345.91, 7383.41, 7357.23, 7330.18, 7331.04, 7289.4, 
7312.15, 7269.66, 7305.29, 7435.78, 7478.5, 7539.27, 7575.83, 
7711.47, 7782.04, 7772.09, 7760.78, 7718.71, 7777.06, 7796.51, 
7604.26, 7758.06, 7689.98, 7654.25, 7687.72, 7672.79, 7722.33, 
7795.38, 7895.81, 7895.81, 7858.49, 7962.31, 7842.43, 7886.76, 
7921.82, 7922.98, 7975.71, 8038.88, 8020.77, 7890.46, 7906.72, 
8061.65, 8088.36, 8116.93, 8113.44, 8121.11, 8174.53, 8254.89, 
8222.61, 8194.04, 8198.45, 8187.54, 8259.31, 8188, 8031.22, 8062.11, 
7960.84, 7928.32, 7942.03, 7694.66, 7803.36, 7918.1, 8021.23, 
7893.95, 7887.91, 7859.57, 7782.22, 7787.33, 7694.43, 7622.42, 
7622.42, 7879.78, 7894.64, 7867.24, 7822.41, 7835.18, 7851.91, 
7719.28, 7660.98, 7742.97, 7721.14, 7895.92, 7886.44, 7922.72, 
7917.27, 7996.83, 7970.06, 7906.71, 7848.01, 7922.18, 7991.43, 
7945.26, 8015.5, 8027.53, 8038.58, 8100.22, 8178.31, 8095.06, 
8061.42, 8045.21, 8072.22, 8096.29, 8057.98, 7938.88, 7847.03, 
7921.44, 8060.44, 8034.65, 7847.77, 7715.41, 7161.15, 7498.32, 
7506.67, 7381.67, 7442.08, 7674.39, 7689.13, 7692.57, 7683.24, 
7581.32, 7552.59, 7558.73, 7401.32, 7487.76, 7572.48, 7698.22, 
7650.82, 7724.74, 7826.61, 7881.07, 7767.92, 7808.95, 7794.78, 
7794.78, 7823.13, 8013.11, 8018.83, 8032.01, 8050.16, 8149.13, 
8110.84, 8049.66, 7978.79, 7848.99, 7838.3, 7922.59, 7976.31, 
7957.41, 7846.5, 7756.29, 7819.31, 7691.77, 7660.13, 7660.13, 
7679.31, 7792.41, 7915.97, 7908.25, 7908.25, 7965.04, 7978.99, 
7906.25, 7902.27, 7802.62, 7580.42, 7647.18, 7732.13, 7784.69, 
7691.77, 7753.55, 7753.55, 7873.12, 7794.4, 7730.88, 7700.74, 
7712.94, 7815.08, 7915.47, 7973.02, 7906.5, 8107.78, 8160.35, 
8129.71, 8117.25, 8189.49, 8180.52, 8295.61, 8314.55, 8369.6, 
8370.1, 8370.1, 8398.5, 8451.06, 8375.58, 8413.94, 8410.2, 8370.1, 
8457.78, 8490.67, 8545.72, 8550.45, 8584.83, 8539.24, 8444.33, 
8569.39, 8567.14, 8643.12, 8675.75, 8659.56, 8602.52, 8718.85, 
8749.99, 8775.4, 8803.05, 8906.43, 8816.25, 8904.44, 8872.8, 
8846.89, 8796.08, 8782.12, 8799.81, 8868.32, 8986.64, 8983.41, 
9033.23, 8956.5, 8891.48, 8994.86, 8994.86, 9012.3, 9110.2, 9162.27, 
9076.57, 9167.5, 9141.84, 9184.94, 9176.72, 9143.33, 9064.62, 
8917.64, 8898.96, 8951.52, 9063.37, 9147.07, 9192.66, 9147.57, 
9054.65, 8976.68, 9055.15, 9091.52, 9161.77, 9211.84, 9172.23, 
9096, 9050.91, 9054.65, 9171.48, 9132.37, 9114.44, 9114.44, 8963.73, 
8936.57, 8970.2, 8899.95, 8922.37, 8891.24, 8803.8, 8870.56, 
9037.71, 9069.6, 9049.92, 8971.7, 8811.77, 8834.94, 8627.93, 
8665.29, 8829.46, 8813.01, 8712.87, 8711.13, 8828.46, 8923.87, 
8935.58, 8944.54, 8997.36, 8952.02, 9048.67, 9025.26, 9025.26, 
9091.77, 9085.04, 9174.97, 9089.78, 9105.74, 9096.21, 9245.54, 
9234.47, 9328.19, 9337.97, 9295.75, 9190.19, 9128.91, 8932.98, 
8937.36, 9028.24, 8934.78, 8914.96, 9026.95, 8883.29, 8786.74, 
8487.31, 8546.78, 8577.68, 8598.02, 8574.85, 8462.85, 8552.96, 
8459.5, 8425, 8574.85, 8714.65, 8693.28, 8611.41, 8533.65, 8566.61, 
8602.65, 8523.35, 8165.99, 8051.68, 7539.07, 7827.43, 7782.37, 
7682.22, 7640.25, 7640.25, 8020.78, 7865.02, 7615.54, 7795.5, 
7945.35, 8024.39, 8089.78, 7873.77, 7895.66, 7933.25, 7897.2, 
8154.41, 8001.99, 8028.77, 8108.84, 8080.52, 7842.62, 7632.53, 
7784.69, 7726.24, 7742.98, 7741.69, 7731.91, 7899.52, 8001.47, 
7938.14, 7968.78, 8299.36, 8416.76, 8466.45, 8505.85, 8519.23, 
8533.14, 8452.29, 8432.21, 8366.04, 8371.97, 8495.03, 8592.1, 
8706.15, 8706.15, 8783.14, 8915.47, 8975.46, 8897.96, 8863.98, 
8823.82, 8829.74, 8919.59, 9011.25, 8986.28, 9041.11, 9056.05, 
9159.55, 9374.27, 9301.15, 9314.28, 9314.28, 9333.08, 9116.55, 
9133.54, 9064.54, 8879.68, 9016.14, 9070.47, 9027.98, 9009.19, 
8841.58, 8821.76, 8695.6, 8823.3, 8790.6, 8875.82, 8903.63, 8988.85, 
9044.46, 9202.03, 9217.99, 9217.99, 9226.75, 9320.98, 9274.64, 
9181.43, 9181.43, 9184.27, 9311.19, 9544.97, 9537.76, 9643.32, 
9619.89, 9474.68, 9349.56, 9120.93, 9340.55, 9340.55, 9355.22, 
9335.91, 9264.08, 9120.67, 9203.32, 9324.58, 9200.23, 9281.33, 
9358.83, 9345.7, 9274.12, 9366.81, 9304.5, 9304.24, 9291.11, 
9133.03, 9177.31, 9363.46, 9274.89, 9274.89, 9297.03, 9195.47, 
9298.63, 9339.95, 9552.68, 9544.42, 9399.67, 9366.34, 9306.58, 
9324.78, 9297.61, 9275.88, 9467.4, 9736.08, 9727.61, 9693.76, 
9772.84, 9897.44, 9876.35, 9958.77, 9930.47, 9879.41, 9997.62, 
9903.55, 9890.51, 9671.83, 9666.84, 9836.39, 9822.24, 10006.78, 
9913.26, 9786.16, 9832.51, 9832.51, 10007.33, 9963.49, 10085.31, 
10197.7, 10173.84, 10339.51, 10395.01, 10411.66, 10462.72, 10493.89, 
10440.53, 10448.55, 10581.42, 10727.18, 10689.67, 10718.59, 10831.71, 
10845.45, 10878.38, 10789.04, 11014.69, 10886.11, 10955.41, 10946.82, 
11031.59, 11007.25, 11026.15, 11000.37, 11107.19, 10913.32, 10853.47, 
10836.95, 10887.39, 10866.74, 10829.28, 10654.67, 10531.09, 10702.1, 
10466.93, 10559.74, 10559.74, 10596.26, 10577.89, 10663.69, 10799.84, 
10909.38, 10765.64, 10690.29, 10621.27, 10490.51, 10563.33, 10594.99, 
10784.95, 10841.63, 10855.56, 10815.98, 10721.63, 10666.86, 10534.83, 
10552.56, 10655.15, 10815.35, 10970.8, 11066.42, 11139.24, 11139.24, 
11135.12, 11187.36, 11126.89, 11193.7, 11200.98, 11175.02, 11148.1, 
11186.41, 11209.84, 11187.68, 10996.13, 11002.78, 10969.22, 10910.96, 
10863.16, 10979.04, 10972.07, 10791.29, 10655.15, 10645.96, 10677.31, 
10674.77, 10793.82, 10714.03, 10707.7, 10655.15, 10787.8, 10789.39, 
10973.65, 11046.79, 11117.08, 10991.38, 10963.84, 11100.61, 11299.76, 
11283.3, 11326.04, 11198.45, 11090.17, 10914.13, 10829.28, 10937.88, 
10843.21, 11078.45, 11078.45, 11034.13, 11036.34, 11079.39, 11028.42, 
11030.33, 10910.33, 10801.42, 10737.45, 10803.63, 10823.89, 10598.47, 
10524.06, 10318.59, 10279.33, 10303.39, 10275.53, 10213.47, 10336.95, 
10273, 10401.22, 10400.59, 10588.34, 10537.05, 10649.76, 10648.17, 
10417.06, 10232.16, 10286.61, 10019.7, 10116.28, 10204.92, 10392.36, 
10297.69, 10470.25, 10349.92, 10302.13, 10394.89, 10622.53, 10729.86, 
10648.52, 10581.84, 10609.06, 10639.64, 10705.39, 10718.84, 10617.31, 
10597.73, 10595.3, 10769.31, 10760.75, 10932.33, 10883.09, 11035.7, 
11003.89, 11089.52, 10995.63, 11008.17, 11008.17, 10988.91, 10947.92, 
10877.81, 10998.39, 11039.06, 11286.17, 11225.02, 11106.64, 11068.13, 
11134.78, 11224.7, 11192.59, 11160.17, 11225.31, 11244.89, 11257.42, 
11144.27, 11200.53, 11203.59, 11405.77, 11405.77, 11391.08, 11476.7, 
11484.66, 11452.86, 11497.13, 11357.52, 10997.92, 11122.64, 11253.27, 
11522.56, 11572.2, 11511.08, 11551.09, 11582.42, 11722.98, 11722.98, 
11560.72, 11489.36, 11351.3, 11251.7, 11008.17, 11029.89, 11032.98, 
11028.02, 10738.88, 10940.53, 11041.05, 11003.2, 11013.44, 10963.8, 
10905.78, 10957.59, 10699.16, 10643.63, 10425.2, 10519.84, 10718.09, 
10561.41, 10514.56, 10219.52, 10219.52, 10304.84, 10225.72, 10092.63, 
9862.11, 10038.64, 10128.31, 10137.92, 10164.92, 10367.2, 10170.5, 
9796.03, 9856.53, 10010.72, 9928.81, 9947.13, 9811.23, 10131.41, 
10630.59, 10595.23, 10680.23, 10907.34, 10866.7, 11119.86, 11112.72, 
11025.84, 10936.11, 11018.72, 10980.25, 10921.92, 11221.92, 11164.84, 
11033.92, 11114.27, 11111.48, 11186.56, 11287.08, 11125.13, 10923.55, 
10305.77, 10582.52, 10767.42, 10674.95, 10844.05, 10844.05, 10906.09, 
11124.81, 10945.5, 10888.09, 10733.91, 10811.78, 10731.13, 10480.13, 
10412.48, 10577.86, 10603.63, 10536.75, 10367.78, 10545.97, 10609.38, 
10807.78, 10934.56, 10769.73, 10777.28, 10626.84, 10542.55, 10422.27, 
10535.34, 10323.92, 10299.23, 10299.23, 10527.13, 10522.33, 10652.2, 
10794.77, 10815.3, 10735.56, 10812.86, 10668.72, 10614.06, 10564.2, 
10621.84, 10687.95, 10714.81, 10449.3, 10557.84, 10435.16, 10497.74, 
10376.12, 10404.75, 10542.98, 10504.45, 10527.78, 10398.03, 10447.89, 
10560.67, 10560.67, 10483.59, 10481.47, 10635.98, 10646.58, 10727.19, 
10783.77, 10788.7, 10812.75, 10804.27, 10739.92, 10696.08, 10843.87, 
10733.56, 10685.12, 10699.97, 10516.48, 10586.13, 10511.17, 10521.98, 
10606.95, 10687.53, 10706.58, 10767.75, 10867.01, 10976.89, 10905.83, 
10908.76, 11027.8, 11176.14, 11067, 11008.39, 11055.64, 11046.48, 
11079.81, 11139.15, 11144.65, 11182.74, 11192.63, 11252.84, 11215.1, 
11103.01, 11215.1, 11238.78, 11238.78, 11260.61, 11310.64, 11259.87
)), .Names = c("Date", "Index"), row.names = c("1", "2", "3", 
"4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
"16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", 
"27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", 
"38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", 
"49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", 
"60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", 
"71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", 
"82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", 
"93", "94", "95", "96", "97", "98", "99", "100", "101", "102", 
"103", "104", "105", "106", "107", "108", "109", "110", "111", 
"112", "113", "114", "115", "116", "117", "118", "119", "120", 
"121", "122", "123", "124", "125", "126", "127", "128", "129", 
"130", "131", "132", "133", "134", "135", "136", "137", "138", 
"139", "140", "141", "142", "143", "144", "145", "146", "147", 
"148", "149", "150", "151", "152", "153", "154", "155", "156", 
"157", "158", "159", "160", "161", "162", "163", "164", "165", 
"166", "167", "168", "169", "170", "171", "172", "173", "174", 
"175", "176", "177", "178", "179", "180", "181", "182", "183", 
"184", "185", "186", "187", "188", "189", "190", "191", "192", 
"193", "194", "195", "196", "197", "198", "199", "200", "201", 
"202", "203", "204", "205", "206", "207", "208", "209", "210", 
"211", "212", "213", "214", "215", "216", "217", "218", "219", 
"220", "221", "222", "223", "224", "225", "226", "227", "228", 
"229", "230", "231", "232", "233", "234", "235", "236", "237", 
"238", "239", "240", "241", "242", "243", "244", "245", "246", 
"247", "248", "249", "250", "251", "252", "253", "254", "255", 
"256", "257", "258", "259", "260", "261", "262", "263", "264", 
"265", "266", "267", "268", "269", "270", "271", "272", "273", 
"274", "275", "276", "277", "278", "279", "280", "281", "282", 
"283", "284", "285", "286", "287", "288", "289", "290", "291", 
"292", "293", "294", "295", "296", "297", "298", "299", "300", 
"301", "302", "303", "304", "305", "306", "307", "308", "309", 
"310", "311", "312", "313", "314", "315", "316", "317", "318", 
"319", "320", "321", "322", "323", "324", "325", "326", "327", 
"328", "329", "330", "331", "332", "333", "334", "335", "336", 
"337", "338", "339", "340", "341", "342", "343", "344", "345", 
"346", "347", "348", "349", "350", "351", "352", "353", "354", 
"355", "356", "357", "358", "359", "360", "361", "362", "363", 
"364", "365", "366", "367", "368", "369", "370", "371", "372", 
"373", "374", "375", "376", "377", "378", "379", "380", "381", 
"382", "383", "384", "385", "386", "387", "388", "389", "390", 
"391", "392", "393", "394", "395", "396", "397", "398", "399", 
"400", "401", "402", "403", "404", "405", "406", "407", "408", 
"409", "410", "411", "412", "413", "414", "415", "416", "417", 
"418", "419", "420", "421", "422", "423", "424", "425", "426", 
"427", "428", "429", "430", "431", "432", "433", "434", "435", 
"436", "437", "438", "439", "440", "441", "442", "443", "444", 
"445", "446", "447", "448", "449", "450", "451", "452", "453", 
"454", "455", "456", "457", "458", "459", "460", "461", "462", 
"463", "464", "465", "466", "467", "468", "469", "470", "471", 
"472", "473", "474", "475", "476", "477", "478", "479", "480", 
"481", "482", "483", "484", "485", "486", "487", "488", "489", 
"490", "491", "492", "493", "494", "495", "496", "497", "498", 
"499", "500", "501", "502", "503", "504", "505", "506", "507", 
"508", "509", "510", "511", "512", "513", "514", "515", "516", 
"517", "518", "519", "520", "521", "522", "523", "524", "525", 
"526", "527", "528", "529", "530", "531", "532", "533", "534", 
"535", "536", "537", "538", "539", "540", "541", "542", "543", 
"544", "545", "546", "547", "548", "549", "550", "551", "552", 
"553", "554", "555", "556", "557", "558", "559", "560", "561", 
"562", "563", "564", "565", "566", "567", "568", "569", "570", 
"571", "572", "573", "574", "575", "576", "577", "578", "579", 
"580", "581", "582", "583", "584", "585", "586", "587", "588", 
"589", "590", "591", "592", "593", "594", "595", "596", "597", 
"598", "599", "600", "601", "602", "603", "604", "605", "606", 
"607", "608", "609", "610", "611", "612", "613", "614", "615", 
"616", "617", "618", "619", "620", "621", "622", "623", "624", 
"625", "626", "627", "628", "629", "630", "631", "632", "633", 
"634", "635", "636", "637", "638", "639", "640", "641", "642", 
"643", "644", "645", "646", "647", "648", "649", "650", "651", 
"652", "653", "654", "655", "656", "657", "658", "659", "660", 
"661", "662", "663", "664", "665", "666", "667", "668", "669", 
"670", "671", "672", "673", "674", "675", "676", "677", "678", 
"679", "680", "681", "682", "683", "684", "685", "686", "687", 
"688", "689", "690", "691", "692", "693", "694", "695", "696", 
"697", "698", "699", "700", "701", "702", "703", "704", "705", 
"706", "707", "708", "709", "710", "711", "712", "713", "714", 
"715", "716", "717", "718", "719", "720", "721", "722", "723", 
"724", "725", "726", "727", "728", "729", "730", "731", "732", 
"733", "734", "735", "736", "737", "738", "739", "740", "741", 
"742", "743", "744", "745", "746", "747", "748", "749", "750", 
"751", "752", "753", "754", "755", "756", "757", "758", "759", 
"760", "761", "762", "763", "764", "765", "766", "767", "768", 
"769", "770", "771", "772", "773", "774", "775", "776", "777", 
"778", "779", "780", "781", "782", "783", "784", "785", "786", 
"787", "788", "789", "790", "791", "792", "793", "794", "795", 
"796", "797", "798", "799", "800", "801", "802", "803", "804", 
"805", "806", "807", "808", "809", "810", "811", "812", "813", 
"814", "815", "816", "817", "818", "819", "820", "821", "822", 
"823", "824", "825", "826", "827", "828", "829", "830", "831", 
"832", "833", "834", "835", "836", "837", "838", "839", "840", 
"841", "842", "843", "844", "845", "846", "847", "848", "849", 
"850", "851", "852", "853", "854", "855", "856", "857", "858", 
"859", "860", "861", "862", "863", "864", "865", "866", "867", 
"868", "869", "870", "871", "872", "873", "874", "875", "876", 
"877", "878", "879", "880", "881", "882", "883", "884", "885", 
"886", "887", "888", "889", "890", "891", "892", "893", "894", 
"895", "896", "897", "898", "899", "900", "901", "902", "903", 
"904", "905", "906", "907", "908", "909", "910", "911", "912", 
"913", "914", "915", "916", "917", "918", "919", "920", "921", 
"922", "923", "924", "925", "926", "927", "928", "929", "930", 
"931", "932", "933", "934", "935", "936", "937", "938", "939", 
"940", "941", "942", "943", "944", "945", "946", "947", "948", 
"949", "950", "951", "952", "953", "954", "955", "956", "957", 
"958", "959", "960", "961", "962", "963", "964", "965", "966", 
"967", "968", "969", "970", "971", "972", "973", "974", "975", 
"976", "977", "978", "979", "980", "981", "982", "983", "984", 
"985", "986", "987", "988", "989", "990", "991", "992", "993", 
"994", "995", "996", "997", "998", "999", "1000", "1001", "1002", 
"1003", "1004", "1005", "1006", "1007", "1008", "1009", "1010", 
"1011", "1012", "1013", "1014", "1015", "1016", "1017", "1018", 
"1019", "1020", "1021", "1022", "1023", "1024", "1025", "1026", 
"1027", "1028", "1029", "1030", "1031", "1032", "1033", "1034", 
"1035", "1036", "1037", "1038", "1039", "1040", "1041", "1042", 
"1043", "1044", "1045", "1046", "1047", "1048", "1049", "1050", 
"1051", "1052", "1053", "1054", "1055", "1056", "1057", "1058", 
"1059", "1060", "1061", "1062", "1063", "1064", "1065", "1066", 
"1067", "1068", "1069", "1070", "1071", "1072", "1073", "1074", 
"1075", "1076", "1077", "1078", "1079", "1080", "1081", "1082", 
"1083", "1084", "1085", "1086", "1087", "1088", "1089", "1090", 
"1091", "1092", "1093", "1094", "1095", "1096", "1097", "1098", 
"1099", "1100", "1101", "1102", "1103", "1104", "1105", "1106", 
"1107", "1108", "1109", "1110", "1111", "1112", "1113", "1114", 
"1115", "1116", "1117", "1118", "1119", "1120", "1121", "1122", 
"1123", "1124", "1125", "1126", "1127", "1128", "1129", "1130", 
"1131", "1132", "1133", "1134", "1135", "1136", "1137", "1138", 
"1139", "1140", "1141", "1142", "1143", "1144", "1145", "1146", 
"1147", "1148", "1149", "1150", "1151", "1152", "1153", "1154", 
"1155", "1156", "1157", "1158", "1159", "1160", "1161", "1162", 
"1163", "1164", "1165", "1166", "1167", "1168", "1169", "1170", 
"1171", "1172", "1173", "1174", "1175", "1176", "1177", "1178", 
"1179", "1180", "1181", "1182", "1183", "1184", "1185", "1186", 
"1187", "1188", "1189", "1190", "1191", "1192", "1193", "1194", 
"1195", "1196", "1197", "1198", "1199", "1200", "1201", "1202", 
"1203", "1204", "1205", "1206", "1207", "1208", "1209", "1210", 
"1211", "1212", "1213", "1214", "1215", "1216", "1217", "1218", 
"1219", "1220", "1221", "1222", "1223", "1224", "1225", "1226", 
"1227", "1228", "1229", "1230", "1231", "1232", "1233", "1234", 
"1235", "1236", "1237", "1238", "1239", "1240", "1241", "1242", 
"1243", "1244", "1245", "1246", "1247", "1248", "1249", "1250", 
"1251", "1252", "1253", "1254", "1255", "1256", "1257", "1258", 
"1259", "1260", "1261", "1262", "1263", "1264", "1265", "1266", 
"1267", "1268", "1269", "1270", "1271", "1272", "1273", "1274", 
"1275", "1276", "1277", "1278", "1279", "1280", "1281", "1282", 
"1283", "1284", "1285", "1286", "1287", "1288", "1289", "1290", 
"1291", "1292", "1293", "1294", "1295", "1296", "1297", "1298", 
"1299", "1300", "1301", "1302", "1303", "1304"), class = "data.frame")
"engine"<-
structure(.Data = list(Time = c(5.231237562997292, 0.8837411621143357, 
    0.2458245189711115, 3.737508046199831, 1.193548682648953, 
    0.7444490091933975, 0.0003316719876203153, 2.212633058499362, 
    0.09988934086693642, 0.1570130757404862, 0.5938764874007935, 
    0.5450763124675745, 2.78271317329455, 0.955511841861073, 
    0.1205484810065402, 0.388568088103867, 0.1455613887992655, 
    0.3927463238338724, 0.2340125343524729, 0.6133401160995717, 
    0.3597261345221488, 0.02001332508547954, 0.08535049758900932, 
    0.8378777077059852, 1.491809687280274, 0.080670416816001, 
    1.210000995755949, 0.7985181166114014, 0.450192367490525, 
    0.6097920424330588, 0.3081687741629526, 0.08961976704041333), Corrosion
     = c(0.02856560982763767, 0.1164455339312554, 0.3255641227588058, 
    0.3618757030926645, 0.7728950004093349, 1.07671243371442, 
    1.408066027797758, 1.530194312799722, 1.568192027043551, 
    1.644205823540688, 1.644408642314374, 1.664612088352442, 
    1.69701453531161, 1.749573536217213, 1.788764433003962, 
    1.877758731134236, 1.888144421391189, 2.0260074082762, 
    2.051496629137546, 2.190115910489112, 2.365581477060914, 
    2.399481928441674, 2.561722400132566, 2.56528501631692, 
    2.620787434279918, 2.716439834330231, 2.92964335065335, 
    3.337955195456743, 3.406588821671903, 3.86109929298982, 
    4.168309979140758, 4.178956965915858)), class = "data.frame", row.names
     = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
    "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
    "25", "26", "27", "28", "29", "30", "31", "32"))


"euroex"<-
c(1.3451, 1.3645, 1.3607, 1.3668, 1.3717, 1.3739, 1.3705, 1.3677, 1.3685, 
    1.3735, 1.375, 1.3824, 1.3837, 1.3879, 1.3913, 1.3722, 1.3679, 1.3732, 
    1.3729, 1.3731, 1.3594, 1.3564, 1.3676, 1.3764, 1.3908, 1.3909, 1.3934, 
    1.3966, 1.4116, 1.4114, 1.4077, 1.4037, 1.4116, 1.3968, 1.4128, 1.4002, 
    1.4048, 1.4078, 1.4096, 1.42, 1.4142, 1.4179, 1.4115, 1.4237, 1.4184, 
    1.421, 1.4157, 1.4034, 1.4065, 1.3947, 1.4007, 1.3976, 1.3873, 1.3751, 
    1.3888, 1.3901, 1.3937, 1.4038, 1.4081, 1.416, 1.4037, 1.4104, 1.4148, 
    1.4113, 1.4096, 1.4106, 1.4225, 1.4206, 1.4293, 1.4276, 1.4332, 1.4307, 
    1.4413, 1.4359, 1.4367, 1.432, 1.4253, 1.438, 1.4253, 1.4308, 1.4389, 
    1.4378, 1.4406, 1.4367, 1.4343, 1.4348, 1.4377, 1.4453, 1.4436, 1.4199, 
    1.4043, 1.4202, 1.4204, 1.4204, 1.4256, 1.4173, 1.4271, 1.4249, 1.4131, 
    1.4169, 1.4234, 1.4231, 1.4228, 1.4308, 1.4365, 1.4476, 1.4485, 1.4311, 
    1.4483, 1.4432, 1.4344, 1.4471, 1.4437, 1.4489, 1.4555, 1.4497, 1.4523, 
    1.4506, 1.4527, 1.4603, 1.4638, 1.4659, 1.4643, 1.4677, 1.4722, 1.4782, 
    1.4843, 1.5003, 1.5011, 1.4993, 1.5081, 1.5062, 1.5031, 1.5219, 1.5309, 
    1.5292, 1.5238, 1.5197, 1.5178, 1.5247, 1.5381, 1.5445, 1.5458, 1.5432, 
    1.5229, 1.5134, 1.5164, 1.5304, 1.544, 1.5478, 1.5286, 1.5066, 1.4886, 
    1.4992, 1.4955, 1.4766, 1.4899, 1.4899, 1.4955, 1.499, 1.499, 1.4901, 
    1.4734, 1.4864, 1.4762, 1.4822, 1.4764, 1.4888, 1.4823, 1.4674, 1.4674, 
    1.459, 1.4609, 1.4665, 1.4559, 1.4513, 1.4528, 1.443, 1.4374, 1.4451, 
    1.4618, 1.4544, 1.4655, 1.472, 1.4544, 1.4683, 1.4459, 1.4491, 1.4509, 
    1.4608, 1.4594, 1.4472, 1.4507, 1.4507, 1.4469, 1.4461, 1.4488, 1.4474, 
    1.4453, 1.4408, 1.4558, 1.4654, 1.4818, 1.4768, 1.4657, 1.4711, 1.4786, 
    1.468, 1.47, 1.461, 1.4669, 1.4755, 1.4678, 1.4618, 1.4742, 1.4644, 
    1.4654, 1.485, 1.4781, 1.4796, 1.4814, 1.4793, 1.4793, 1.4807, 1.4857, 
    1.4821, 1.4792, 1.4895, 1.4901, 1.5031, 1.508, 1.5047, 1.5031, 1.4908, 
    1.4896, 1.4862, 1.4846, 1.4878, 1.4766, 1.4798, 1.4677, 1.4737, 1.4807, 
    1.4951, 1.4954, 1.4951, 1.4957, 1.497, 1.5085, 1.4988, 1.4938, 1.4977, 
    1.5056, 1.5095, 1.4976, 1.4866, 1.4875, 1.4945, 1.5039, 1.504, 1.5084, 
    1.513, 1.5176, 1.5158, 1.4886, 1.5105, 1.508, 1.5073, 1.4959, 1.5057, 
    1.5158, 1.5148, 1.5103, 1.5107, 1.5103, 1.502, 1.5058, 1.5064, 1.4922, 
    1.5028, 1.5006, 1.5089, 1.5049, 1.5075, 1.5095, 1.5078, 1.5091, 1.4933, 
    1.4992, 1.5095, 1.5067, 1.5101, 1.5092, 1.5092, 1.5116, 1.5178, 1.5145, 
    1.5116, 1.5163, 1.523, 1.5247, 1.5281, 1.53, 1.5344, 1.5372, 1.5356, 
    1.5357, 1.5422, 1.541, 1.5406, 1.5455, 1.5504, 1.5581, 1.5565, 1.558, 
    1.5512, 1.5447, 1.5426, 1.544, 1.5297, 1.5356, 1.5328, 1.5324, 1.528, 
    1.5411, 1.535, 1.5229, 1.5148, 1.5151, 1.5158, 1.5113, 1.513, 1.5132, 
    1.5152, 1.5172, 1.5012, 1.501, 1.4908, 1.4874, 1.4722, 1.4731, 1.4711, 
    1.4724, 1.4719, 1.4746, 1.4763, 1.47, 1.4677, 1.4578, 1.4587, 1.461, 
    1.4697, 1.4767, 1.4716, 1.4798, 1.4793, 1.4783, 1.4732, 1.4676, 1.4723, 
    1.4745, 1.4772, 1.4837, 1.4884, 1.4901, 1.4979, 1.5032, 1.5023, 1.514, 
    1.5129, 1.5157, 1.5149, 1.5166, 1.5149, 1.52, 1.5239, 1.5192, 1.5207, 
    1.5114, 1.5066, 1.5017, 1.5028, 1.5075, 1.5014, 1.4932, 1.4918, 1.4895, 
    1.4847, 1.484, 1.4849, 1.4878, 1.4868, 1.4946, 1.495, 1.4967, 1.4809, 
    1.4776, 1.4799, 1.4742, 1.4718, 1.4691, 1.4669, 1.4708, 1.4727, 1.4718, 
    1.4681, 1.4728, 1.4797, 1.4694, 1.4752, 1.4788, 1.482, 1.4877, 1.4907, 
    1.4929, 1.4964, 1.5039, 1.4981, 1.4952, 1.4864, 1.4823, 1.4705, 1.4698, 
    1.4559, 1.4555, 1.4422, 1.4463, 1.452, 1.4392, 1.4452, 1.445, 1.4485, 
    1.4421, 1.4422, 1.4425, 1.4432, 1.448, 1.452, 1.4514, 1.4423, 1.4283, 
    1.421, 1.399, 1.396, 1.392, 1.4011, 1.4039, 1.4152, 1.4204, 1.4077, 
    1.3963, 1.408, 1.4208, 1.4199, 1.4137, 1.414, 1.4093, 1.4039, 1.4084, 
    1.4085, 1.4005, 1.4039, 1.4074, 1.402, 1.4107, 1.4257, 1.4209, 1.4221, 
    1.4263, 1.4219, 1.4211, 1.4195, 1.4215, 1.4334, 1.434, 1.436, 1.4357, 
    1.4389, 1.4209, 1.4169, 1.4169, 1.4189, 1.4205, 1.4131, 1.4101, 1.4115, 
    1.4087, 1.4139, 1.4194, 1.4219, 1.4199, 1.4209, 1.4253, 1.4311, 1.4291, 
    1.4287, 1.4317, 1.4313, 1.4239, 1.4163, 1.3975, 1.4085, 1.4221, 1.4126, 
    1.42, 1.4199, 1.4113, 1.4101, 1.4143, 1.4242, 1.4246, 1.4265, 1.4233, 
    1.4264, 1.429, 1.4318, 1.4339, 1.4366, 1.4459, 1.447, 1.4519, 1.4505, 
    1.4444, 1.4497, 1.4509, 1.45, 1.4471, 1.4378, 1.4371, 1.4462, 1.4469, 
    1.4572, 1.4553, 1.4558, 1.4685, 1.4722, 1.468, 1.4554, 1.4511, 1.4582, 
    1.4754, 1.4767, 1.4821, 1.4843, 1.4827, 1.4757, 1.4874, 1.4844, 1.4956, 
    1.4919, 1.4843, 1.488, 1.4795, 1.4825, 1.4922, 1.4916, 1.4992, 1.5022, 
    1.5007, 1.5078, 1.5064, 1.5016, 1.4937, 1.4904, 1.4955, 1.4689, 1.4808, 
    1.4836, 1.49, 1.4879, 1.4972, 1.4963, 1.5023, 1.5078, 1.5149, 1.5187, 
    1.5147, 1.521, 1.5212, 1.5217, 1.5191, 1.5208, 1.5199, 1.5225, 1.5213, 
    1.5302, 1.5232, 1.5214, 1.5156, 1.5098, 1.5123, 1.5181, 1.5236, 1.5185, 
    1.5174, 1.5183, 1.5199, 1.5168, 1.5149, 1.5125, 1.5242, 1.5287, 1.5367, 
    1.5354, 1.5417, 1.5519, 1.5519, 1.5565, 1.5551, 1.5377, 1.5307, 1.5277, 
    1.5302, 1.5435, 1.5294, 1.5408, 1.5418, 1.5359, 1.5401, 1.5402, 1.5302, 
    1.5269, 1.5224, 1.5264, 1.5266, 1.5303, 1.5412, 1.5404, 1.5306, 1.5265, 
    1.527, 1.5236, 1.5347, 1.5295, 1.5306, 1.5332, 1.5315, 1.5319, 1.5073, 
    1.4978, 1.5077, 1.5037, 1.4932, 1.496, 1.4979, 1.5055, 1.5146, 1.5146, 
    1.5044, 1.5002, 1.4994, 1.499, 1.5048, 1.5104, 1.5128, 1.5188, 1.5169, 
    1.5232, 1.5237, 1.5207, 1.515, 1.5233, 1.5183, 1.5152, 1.5209, 1.5184, 
    1.5199, 1.5204, 1.5144, 1.5046, 1.5121, 1.5172, 1.5285, 1.5491, 1.5542, 
    1.5477, 1.5457, 1.5515, 1.5601, 1.56, 1.5593, 1.5589, 1.559, 1.5721, 
    1.5691, 1.5775, 1.5691, 1.5473, 1.547, 1.5441, 1.5469, 1.5458, 1.5471, 
    1.541, 1.5535, 1.5404, 1.5364, 1.5349, 1.5326, 1.5391, 1.5428, 1.5468, 
    1.5513, 1.5531, 1.559, 1.5624, 1.5645, 1.559, 1.5619, 1.5631, 1.5656, 
    1.5678, 1.5696, 1.5577, 1.5589, 1.5589, 1.5566, 1.5646, 1.5733, 1.5671, 
    1.5602, 1.5685, 1.5669, 1.5692, 1.5824, 1.581, 1.5833, 1.5833, 1.5866, 
    1.5835, 1.5854, 1.5936, 1.5989, 1.5833, 1.5883, 1.5838, 1.5973, 1.597, 
    1.6041, 1.601, 1.5983, 1.5859, 1.5929, 1.5933, 1.5947, 1.5936, 1.5904, 
    1.5942, 1.6096, 1.6079, 1.6051, 1.5877, 1.5883, 1.5964, 1.5919, 1.5973, 
    1.5964, 1.6011, 1.6052, 1.6149, 1.6162, 1.6183, 1.6258, 1.6331, 1.6354, 
    1.6491, 1.6415, 1.6378, 1.6541, 1.6604, 1.6592, 1.6602, 1.6435, 1.6208, 
    1.629, 1.6271, 1.6323, 1.6259, 1.6282, 1.6173, 1.6241, 1.6221, 1.6293, 
    1.6279, 1.6232, 1.6209, 1.6072, 1.6024, 1.6096, 1.6307, 1.6482, 1.6371, 
    1.6338, 1.6398, 1.6439, 1.638, 1.6491, 1.655, 1.6333, 1.6357, 1.6364, 
    1.6299, 1.6232, 1.6217, 1.6202, 1.6151, 1.6209, 1.6343, 1.6336, 1.6361, 
    1.6432, 1.651, 1.6664, 1.6594, 1.6635, 1.6722, 1.6672, 1.6485, 1.6485, 
    1.6505, 1.6506, 1.6553, 1.6623, 1.6679, 1.6582, 1.66, 1.6642, 1.6845, 
    1.6849, 1.6819, 1.7046, 1.7142, 1.7342, 1.7123, 1.7046, 1.7195, 1.7549, 
    1.7338, 1.7072, 1.7105, 1.6972, 1.6662, 1.6621, 1.6711, 1.6498, 1.6583, 
    1.6721, 1.654, 1.6605, 1.6308, 1.623, 1.6207, 1.5977, 1.6072, 1.6041, 
    1.6042, 1.6014, 1.5989, 1.5997, 1.592, 1.5865, 1.5773, 1.5832, 1.5842, 
    1.5722, 1.5662, 1.5853, 1.572, 1.5749, 1.5804, 1.5825, 1.6079, 1.6035, 
    1.5973, 1.5881, 1.6, 1.5968, 1.5847, 1.5926, 1.5836, 1.5866, 1.5965, 
    1.5898, 1.5944, 1.603, 1.6051, 1.5993, 1.5957, 1.6031, 1.618, 1.6242, 
    1.624, 1.6256, 1.6175, 1.6104, 1.6206, 1.625, 1.6171, 1.6256, 1.6332, 
    1.6535, 1.6566, 1.6667, 1.6685, 1.6507, 1.6635, 1.6666, 1.6488, 1.6399, 
    1.6375, 1.6425, 1.6517, 1.6502, 1.652, 1.6396, 1.6333, 1.635, 1.6296, 
    1.6322, 1.6341, 1.6234, 1.6354, 1.6596, 1.645, 1.639, 1.6368, 1.6304, 
    1.6345, 1.6308, 1.633, 1.6433, 1.6544, 1.6673, 1.6683, 1.6576, 1.6638, 
    1.6565, 1.6591, 1.6581, 1.6726, 1.6693, 1.6657, 1.6688, 1.6693, 1.6629, 
    1.6696, 1.6771, 1.7039, 1.7031, 1.703, 1.6951, 1.7243, 1.7157, 1.7194, 
    1.7401, 1.7331, 1.7333, 1.7312, 1.7263, 1.7236, 1.7099, 1.6835, 1.6836, 
    1.6806, 1.667, 1.667, 1.6638, 1.6612, 1.6593, 1.669, 1.6644, 1.6674, 
    1.6711, 1.6759, 1.675)
"exchange"<-
structure(.Data = list(USD.GBP = c(1.6865, 1.6905, 1.6855, 1.6951, 1.6884, 
    1.6947, 1.6817, 1.6725, 1.6701, 1.6783, 1.6767, 1.6707, 1.6603, 1.6637, 
    1.6537, 1.6295, 1.6279, 1.6235, 1.6161, 1.6212, 1.6094, 1.6023, 1.6146, 
    1.6207, 1.6377, 1.6351, 1.623, 1.6387, 1.6445, 1.6335, 1.6243, 1.6211, 
    1.6181, 1.6031, 1.6139, 1.6111, 1.6155, 1.6335, 1.6327, 1.6335, 1.6291, 
    1.6291, 1.6177, 1.6163, 1.611, 1.6133, 1.6035, 1.6009, 1.606, 1.5927, 
    1.597, 1.6025, 1.5901, 1.5895, 1.5981, 1.5942, 1.6017, 1.617, 1.6172, 
    1.628, 1.6315, 1.6452, 1.6488, 1.6425, 1.6455, 1.638, 1.6278, 1.6235, 
    1.6215, 1.6235, 1.6246, 1.6227, 1.6267, 1.6245, 1.6313, 1.632, 1.6352, 
    1.6366, 1.6221, 1.6255, 1.6238, 1.6235, 1.6326, 1.6205, 1.6242, 1.6223, 
    1.6203, 1.6375, 1.6371, 1.6201, 1.6212, 1.6237, 1.6321, 1.6321, 1.6387, 
    1.6377, 1.6444, 1.6465, 1.6265, 1.633, 1.6405, 1.6287, 1.6379, 1.6407, 
    1.6357, 1.6343, 1.6361, 1.6279, 1.6334, 1.6286, 1.6353, 1.6385, 1.6373, 
    1.636, 1.636, 1.6368, 1.6389, 1.6383, 1.6483, 1.6545, 1.6687, 1.6643, 
    1.6634, 1.6667, 1.6617, 1.6653, 1.6663, 1.6892, 1.6861, 1.687, 1.6885, 
    1.6841, 1.6887, 1.6943, 1.6901, 1.6766, 1.6745, 1.6753, 1.6783, 1.6777, 
    1.6739, 1.6775, 1.6733, 1.6643, 1.6285, 1.6285, 1.6314, 1.6409, 1.6351, 
    1.6253, 1.6013, 1.588, 1.5852, 1.59, 1.5774, 1.5822, 1.5868, 1.609, 
    1.6095, 1.6039, 1.5927, 1.5905, 1.6111, 1.6057, 1.6127, 1.6105, 1.6151, 
    1.6211, 1.5903, 1.5862, 1.5863, 1.5925, 1.5831, 1.5885, 1.589, 1.597, 
    1.61, 1.6045, 1.5941, 1.5991, 1.6134, 1.611, 1.6025, 1.6125, 1.6107, 
    1.6284, 1.6099, 1.6165, 1.6125, 1.6147, 1.6141, 1.6143, 1.6139, 1.6204, 
    1.6205, 1.6252, 1.6212, 1.6197, 1.6239, 1.6187, 1.6145, 1.63, 1.6337, 
    1.6341, 1.6324, 1.6349, 1.6555, 1.6751, 1.6739, 1.6654, 1.6776, 1.6768, 
    1.6835, 1.6777, 1.6931, 1.6907, 1.6935, 1.704, 1.7014, 1.6933, 1.694, 
    1.6966, 1.6916, 1.6933, 1.69, 1.6895, 1.6813, 1.6743, 1.6721, 1.689, 
    1.6825, 1.6824, 1.6845, 1.6719, 1.6575, 1.6475, 1.6531, 1.6499, 1.6561, 
    1.6501, 1.6343, 1.6327, 1.651, 1.6627, 1.6689, 1.6623, 1.6667, 1.6696, 
    1.6707, 1.6545, 1.6424, 1.6419, 1.6329, 1.6325, 1.6259, 1.6122, 1.6162, 
    1.6229, 1.6325, 1.6303, 1.631, 1.634, 1.6371, 1.6273, 1.6263, 1.6496, 
    1.6695, 1.6551, 1.6471, 1.6429, 1.6413, 1.6325, 1.6385, 1.6417, 1.6385, 
    1.6545, 1.6459, 1.6347, 1.6234, 1.6325, 1.6387, 1.6349, 1.6383, 1.6315, 
    1.64, 1.6357, 1.6372, 1.6474, 1.6465, 1.6471, 1.6437, 1.6475, 1.6477, 
    1.6523, 1.6477, 1.6452, 1.6343, 1.6363, 1.6383, 1.6471, 1.6567, 1.6665, 
    1.6685, 1.6745, 1.6704, 1.6682, 1.667, 1.6789, 1.6756, 1.6745, 1.6863, 
    1.6848, 1.6785, 1.674, 1.6727, 1.6677, 1.6582, 1.6643, 1.6663, 1.6685, 
    1.6729, 1.6665, 1.6849, 1.6842, 1.6914, 1.6847, 1.676, 1.6765, 1.6744, 
    1.6661, 1.6675, 1.673, 1.6671, 1.6703, 1.6727, 1.668, 1.6665, 1.6607, 
    1.6617, 1.6489, 1.6377, 1.63, 1.6351, 1.6309, 1.6309, 1.6271, 1.6245, 
    1.6335, 1.6315, 1.6325, 1.6275, 1.6369, 1.6325, 1.6277, 1.6339, 1.6395, 
    1.6387, 1.6386, 1.6393, 1.6357, 1.6327, 1.6352, 1.6277, 1.63, 1.6307, 
    1.6317, 1.6521, 1.6617, 1.6695, 1.674, 1.6723, 1.6641, 1.6686, 1.6714, 
    1.6608, 1.6643, 1.6697, 1.6559, 1.6476, 1.6445, 1.6379, 1.6367, 1.6313, 
    1.6326, 1.6417, 1.6375, 1.6337, 1.6381, 1.6435, 1.6477, 1.6429, 1.6423, 
    1.6478, 1.6604, 1.657, 1.6489, 1.6433, 1.6415, 1.6335, 1.6324, 1.635, 
    1.6338, 1.6295, 1.6329, 1.6299, 1.6297, 1.6267, 1.6187, 1.6168, 1.6167, 
    1.6231, 1.6277, 1.6373, 1.6404, 1.6399, 1.6377, 1.651, 1.6713, 1.6763, 
    1.6747, 1.6695, 1.6767, 1.6725, 1.6587, 1.6605, 1.6815, 1.6845, 1.6772, 
    1.6738, 1.6764, 1.6835, 1.6849, 1.6845, 1.6847, 1.6801, 1.6939, 1.7023, 
    1.7043, 1.707, 1.7001, 1.7047, 1.7041, 1.6896, 1.683, 1.6968, 1.7218, 
    1.7055, 1.7019, 1.7055, 1.7031, 1.7038, 1.6998, 1.7062, 1.7015, 1.6957, 
    1.6845, 1.6735, 1.6715, 1.6759, 1.6737, 1.6657, 1.6589, 1.6583, 1.6631, 
    1.6583, 1.6585, 1.6584, 1.658, 1.6633, 1.6779, 1.677, 1.6717, 1.6555, 
    1.6539, 1.6587, 1.6594, 1.6596, 1.651, 1.6478, 1.6541, 1.6621, 1.6647, 
    1.6634, 1.6528, 1.6546, 1.6584, 1.668, 1.6815, 1.6889, 1.6849, 1.6759, 
    1.6757, 1.6805, 1.6805, 1.6775, 1.6767, 1.6724, 1.6832, 1.6664, 1.6627, 
    1.6594, 1.6566, 1.6549, 1.6492, 1.6409, 1.6379, 1.6307, 1.6495, 1.6529, 
    1.6509, 1.653, 1.6553, 1.6473, 1.6515, 1.6552, 1.6563, 1.6597, 1.649, 
    1.6474, 1.6458, 1.6411, 1.6431, 1.6377, 1.6393, 1.6371, 1.6381, 1.6357, 
    1.629, 1.6244, 1.6317, 1.6274, 1.6308, 1.6345, 1.6339, 1.6259, 1.6248, 
    1.6139, 1.5973, 1.6063, 1.603, 1.6069, 1.6139, 1.6135, 1.6067, 1.6077, 
    1.6086, 1.6172, 1.627, 1.6327, 1.6334, 1.6224, 1.6243, 1.63, 1.6297, 
    1.6303, 1.6285, 1.637, 1.641, 1.6316, 1.6201, 1.6145, 1.6118, 1.6145, 
    1.6067, 1.6012, 1.5926, 1.5971, 1.6085, 1.6069, 1.6134, 1.614, 1.613, 
    1.6102, 1.6118, 1.6073, 1.6145, 1.6077, 1.6121, 1.6175, 1.6115, 1.6177, 
    1.6147, 1.6107, 1.6087, 1.6081, 1.6211, 1.6337, 1.6399, 1.6346, 1.6291, 
    1.6217, 1.6193, 1.6187, 1.6174, 1.6193, 1.6203, 1.6177, 1.6126, 1.602, 
    1.6016, 1.5957, 1.5958, 1.6022, 1.6023, 1.6105, 1.6078, 1.6057, 1.6079, 
    1.6014, 1.6057, 1.5997, 1.6008, 1.6096, 1.608, 1.5936, 1.5875, 1.5926, 
    1.5925, 1.59, 1.5897, 1.5791, 1.5836, 1.5899, 1.5832, 1.5783, 1.5763, 
    1.5794, 1.5749, 1.5692, 1.5603, 1.5576, 1.552, 1.5558, 1.5568, 1.5637, 
    1.5671, 1.5627, 1.5631, 1.5703, 1.5766, 1.5845, 1.579, 1.5907, 1.5899, 
    1.5945, 1.6133, 1.6199, 1.6176, 1.6201, 1.6194, 1.615, 1.6057, 1.6179, 
    1.6107, 1.6089, 1.6053, 1.6017, 1.603, 1.6011, 1.6187, 1.6142, 1.6075, 
    1.5945, 1.5861, 1.5878, 1.5888, 1.5881, 1.6088, 1.6027, 1.6085, 1.6039, 
    1.6055, 1.6188, 1.6329, 1.6165, 1.6097, 1.6081, 1.6117, 1.6251, 1.6225, 
    1.6231, 1.6309, 1.6375, 1.6375, 1.6427, 1.6457, 1.6496, 1.6427, 1.6463, 
    1.6549, 1.6559, 1.6527, 1.6558, 1.6523, 1.6519, 1.6539, 1.6533, 1.6582, 
    1.6687, 1.6721, 1.6713, 1.6651, 1.6767, 1.6585, 1.6632, 1.6527, 1.6479, 
    1.6399, 1.643, 1.6405, 1.645, 1.6441, 1.6385, 1.6203, 1.6153, 1.6215, 
    1.6257, 1.6139, 1.6231, 1.6171, 1.6231, 1.6165, 1.6163, 1.6208, 1.6239, 
    1.609, 1.6148, 1.6052, 1.6029, 1.5957, 1.5961, 1.5977, 1.6015, 1.6233, 
    1.6237, 1.6253, 1.6237, 1.6227, 1.6237, 1.6119, 1.6089, 1.6127, 1.6071, 
    1.6042, 1.6101, 1.6063, 1.6165, 1.6153, 1.6171, 1.6125, 1.6153, 1.6371, 
    1.641, 1.6481, 1.6387, 1.6375, 1.6483, 1.6465, 1.6485, 1.6362, 1.6305, 
    1.638, 1.6441, 1.6545, 1.6517, 1.6525, 1.6487, 1.6397, 1.6367, 1.6207, 
    1.6183, 1.6157, 1.6059, 1.6029, 1.5904, 1.592, 1.6101, 1.6121, 1.6062, 
    1.5925, 1.5895, 1.5955, 1.6037, 1.6055, 1.5988, 1.5994, 1.6169, 1.6052, 
    1.5987, 1.5917, 1.5943, 1.5787, 1.5852, 1.578, 1.5811, 1.5731, 1.5773, 
    1.5852, 1.5818, 1.5802, 1.5785, 1.5727, 1.574, 1.5749, 1.5713, 1.5686, 
    1.5729, 1.5705, 1.5839, 1.5911, 1.5853, 1.5873, 1.5877, 1.5923, 1.5921, 
    1.5981, 1.5981, 1.5905, 1.5807, 1.5831, 1.5829, 1.5875, 1.5878, 1.5885, 
    1.5862, 1.5854, 1.5771, 1.5779, 1.5797, 1.5803, 1.5795, 1.5801, 1.5753, 
    1.5567, 1.5553, 1.5593, 1.562, 1.5442, 1.5287, 1.5309, 1.5319, 1.5157, 
    1.4995, 1.5173, 1.5077, 1.5011, 1.4912, 1.4809, 1.4857, 1.4733, 1.4761, 
    1.4705, 1.4873, 1.4914, 1.4942, 1.4971, 1.4908, 1.508, 1.5143, 1.5239, 
    1.5233, 1.5077, 1.5083, 1.5123, 1.5126, 1.5022, 1.5107, 1.5169, 1.5155, 
    1.5103, 1.4971, 1.5114, 1.5009, 1.4993, 1.5002, 1.5109, 1.5193, 1.5131, 
    1.5147, 1.5119, 1.5118, 1.5143, 1.5138, 1.5143, 1.5071, 1.4999, 1.4992, 
    1.4924, 1.4945, 1.4944, 1.5089, 1.5177, 1.5141, 1.5192, 1.516, 1.5114, 
    1.5027, 1.4986, 1.5001, 1.4923, 1.4953, 1.5034, 1.503, 1.5002, 1.4983, 
    1.5045, 1.5063, 1.5062, 1.4992, 1.499, 1.4896, 1.4911, 1.4801, 1.4816, 
    1.4803, 1.474, 1.4719, 1.4615, 1.4566, 1.451, 1.4601, 1.4518, 1.4445, 
    1.438, 1.4203, 1.4117, 1.4017, 1.4128, 1.4057, 1.4001, 1.4013, 1.4085, 
    1.4111, 1.4279, 1.4577, 1.4538, 1.4598, 1.4611, 1.4641, 1.479, 1.467, 
    1.4567, 1.4563, 1.4507, 1.4443, 1.4495, 1.4617, 1.4728, 1.4591, 1.4459, 
    1.4423, 1.4468, 1.4432, 1.4453, 1.4555, 1.4497, 1.4341, 1.4323, 1.4515, 
    1.4535, 1.4509, 1.4457, 1.4443, 1.4485, 1.4311, 1.4312, 1.4232, 1.4243, 
    1.4309, 1.4309, 1.4265, 1.4229, 1.4233, 1.4223, 1.4169), CAD.GBP = c(
    2.3167, 2.3224, 2.3056, 2.2999, 2.285, 2.2906, 2.2666, 2.2557, 2.2551, 
    2.2521, 2.2449, 2.2379, 2.2195, 2.227, 2.2082, 2.1858, 2.1952, 2.1818, 
    2.1661, 2.181, 2.1694, 2.1588, 2.1687, 2.1768, 2.1996, 2.2044, 2.1898, 
    2.2152, 2.2293, 2.214, 2.1975, 2.1882, 2.187, 2.1762, 2.1941, 2.1905, 
    2.2006, 2.2247, 2.2198, 2.2312, 2.2276, 2.2262, 2.2117, 2.2142, 2.204, 
    2.2012, 2.1936, 2.1948, 2.1948, 2.1718, 2.1769, 2.1845, 2.1775, 2.1838, 
    2.203, 2.1981, 2.2102, 2.2282, 2.2277, 2.2372, 2.2467, 2.2775, 2.2856, 
    2.279, 2.2849, 2.2801, 2.2615, 2.2531, 2.25, 2.2589, 2.2705, 2.2693, 
    2.2718, 2.2702, 2.2794, 2.2837, 2.2854, 2.2839, 2.2612, 2.2638, 2.2676, 
    2.2721, 2.2817, 2.2638, 2.255, 2.2438, 2.2378, 2.2561, 2.2618, 2.2408, 
    2.2561, 2.2563, 2.2619, 2.2619, 2.2598, 2.2436, 2.2618, 2.2506, 2.2309, 
    2.2452, 2.2586, 2.2512, 2.2664, 2.2658, 2.2587, 2.2514, 2.2477, 2.2395, 
    2.2477, 2.2538, 2.2673, 2.2715, 2.2708, 2.2614, 2.2552, 2.2657, 2.271, 
    2.2722, 2.2882, 2.3041, 2.3182, 2.3147, 2.3201, 2.3022, 2.2951, 2.2999, 
    2.295, 2.3196, 2.317, 2.3215, 2.3205, 2.3119, 2.3193, 2.3219, 2.3129, 
    2.2977, 2.2999, 2.3087, 2.3065, 2.306, 2.3128, 2.3168, 2.3145, 2.3049, 
    2.2579, 2.256, 2.2533, 2.2667, 2.2555, 2.2449, 2.2205, 2.203, 2.2077, 
    2.2149, 2.1975, 2.2059, 2.2064, 2.237, 2.2343, 2.2365, 2.2199, 2.2192, 
    2.2472, 2.2343, 2.2492, 2.2415, 2.2411, 2.2509, 2.2013, 2.1939, 2.1975, 
    2.2012, 2.1893, 2.199, 2.2006, 2.2225, 2.2432, 2.2331, 2.2183, 2.2204, 
    2.245, 2.2393, 2.225, 2.2414, 2.2345, 2.2538, 2.2276, 2.2379, 2.2285, 
    2.2263, 2.2183, 2.214, 2.2133, 2.2243, 2.224, 2.2343, 2.236, 2.2384, 
    2.2511, 2.2444, 2.2375, 2.2572, 2.2689, 2.2732, 2.2682, 2.2751, 2.3141, 
    2.3599, 2.3476, 2.3432, 2.3641, 2.3589, 2.3631, 2.3491, 2.3734, 2.3829, 
    2.3833, 2.4006, 2.3953, 2.3918, 2.3963, 2.3993, 2.3936, 2.4058, 2.3942, 
    2.4006, 2.3837, 2.3855, 2.3817, 2.4055, 2.3969, 2.3918, 2.3883, 2.3781, 
    2.3581, 2.3426, 2.3512, 2.3495, 2.3642, 2.3442, 2.3222, 2.3243, 2.349, 
    2.3682, 2.3919, 2.3846, 2.393, 2.3962, 2.4045, 2.3767, 2.3472, 2.3397, 
    2.3298, 2.3363, 2.3254, 2.3074, 2.3053, 2.3277, 2.3443, 2.3387, 2.3433, 
    2.3469, 2.3545, 2.3418, 2.3481, 2.3932, 2.43, 2.3998, 2.3952, 2.3932, 
    2.4027, 2.3774, 2.3801, 2.3819, 2.3801, 2.381, 2.3512, 2.3466, 2.3198, 
    2.3451, 2.3569, 2.3592, 2.3682, 2.3497, 2.3531, 2.3276, 2.3253, 2.3466, 
    2.3417, 2.3448, 2.3364, 2.3452, 2.3424, 2.3436, 2.3429, 2.3368, 2.3178, 
    2.3142, 2.3166, 2.3183, 2.3376, 2.3491, 2.3584, 2.3719, 2.3696, 2.3652, 
    2.3638, 2.3869, 2.3778, 2.361, 2.3812, 2.3841, 2.3873, 2.3714, 2.3707, 
    2.3665, 2.3561, 2.3606, 2.3698, 2.3768, 2.3839, 2.3866, 2.4175, 2.4241, 
    2.43, 2.4083, 2.3932, 2.3989, 2.3986, 2.39, 2.3934, 2.4069, 2.3985, 
    2.4012, 2.3923, 2.3912, 2.3913, 2.3892, 2.3907, 2.3743, 2.353, 2.3364, 
    2.3415, 2.3557, 2.3612, 2.3601, 2.3555, 2.3682, 2.3613, 2.3665, 2.3587, 
    2.3805, 2.3777, 2.3688, 2.3808, 2.3879, 2.3827, 2.3809, 2.386, 2.3855, 
    2.3818, 2.3877, 2.3872, 2.3959, 2.3945, 2.4045, 2.4294, 2.4317, 2.4517, 
    2.4603, 2.4601, 2.4456, 2.4523, 2.4513, 2.4402, 2.4495, 2.4571, 2.4304, 
    2.4175, 2.4227, 2.4123, 2.4133, 2.4078, 2.4154, 2.4273, 2.4217, 2.4257, 
    2.4388, 2.446, 2.4514, 2.4512, 2.4559, 2.4643, 2.4873, 2.4913, 2.4775, 
    2.4705, 2.4759, 2.4684, 2.4711, 2.4787, 2.4976, 2.4801, 2.4781, 2.4792, 
    2.469, 2.468, 2.4583, 2.464, 2.4711, 2.4885, 2.49, 2.5244, 2.541, 
    2.5428, 2.5584, 2.6028, 2.6206, 2.6355, 2.6013, 2.566, 2.5808, 2.5517, 
    2.531, 2.5246, 2.5458, 2.5483, 2.5427, 2.5102, 2.5203, 2.5689, 2.5767, 
    2.5727, 2.5766, 2.5652, 2.5608, 2.5747, 2.5752, 2.562, 2.5942, 2.6184, 
    2.6427, 2.6321, 2.6115, 2.5941, 2.6564, 2.6376, 2.6345, 2.6359, 2.633, 
    2.629, 2.633, 2.625, 2.6441, 2.617, 2.6026, 2.578, 2.5776, 2.5967, 
    2.5817, 2.5515, 2.5278, 2.5269, 2.5239, 2.5397, 2.5453, 2.5634, 2.5669, 
    2.5818, 2.6063, 2.598, 2.6003, 2.5574, 2.5635, 2.5698, 2.553, 2.545, 
    2.5326, 2.5269, 2.5357, 2.5541, 2.5528, 2.5517, 2.5408, 2.5557, 2.5589, 
    2.5702, 2.5919, 2.6009, 2.5978, 2.5787, 2.5844, 2.5955, 2.6028, 2.6047, 
    2.6014, 2.5942, 2.6113, 2.5809, 2.5448, 2.5327, 2.5179, 2.4994, 2.4933, 
    2.4835, 2.4663, 2.4606, 2.5218, 2.5273, 2.5232, 2.5281, 2.5323, 2.5077, 
    2.499, 2.5123, 2.5085, 2.5232, 2.5076, 2.5139, 2.4809, 2.4754, 2.4849, 
    2.4785, 2.4478, 2.4357, 2.4368, 2.4424, 2.4329, 2.4187, 2.4347, 2.431, 
    2.4454, 2.4506, 2.4296, 2.4231, 2.432, 2.4154, 2.3937, 2.4186, 2.4164, 
    2.4471, 2.4531, 2.4638, 2.4509, 2.4386, 2.4378, 2.4531, 2.4794, 2.4858, 
    2.489, 2.4782, 2.4827, 2.4804, 2.4767, 2.4704, 2.4541, 2.4658, 2.4679, 
    2.464, 2.4519, 2.4421, 2.4378, 2.4366, 2.4113, 2.4002, 2.3943, 2.3966, 
    2.411, 2.4136, 2.4107, 2.405, 2.408, 2.4, 2.3942, 2.383, 2.4048, 2.394, 
    2.3877, 2.3881, 2.3834, 2.3919, 2.3841, 2.3624, 2.3448, 2.3325, 2.3558, 
    2.3787, 2.3877, 2.3837, 2.378, 2.3588, 2.3655, 2.3631, 2.3676, 2.3647, 
    2.3664, 2.3714, 2.3636, 2.3381, 2.338, 2.3495, 2.354, 2.3584, 2.3666, 
    2.3892, 2.3797, 2.3703, 2.3631, 2.3589, 2.3601, 2.3551, 2.3375, 2.3507, 
    2.3474, 2.3338, 2.3157, 2.3268, 2.3317, 2.3413, 2.3369, 2.3265, 2.329, 
    2.3263, 2.3264, 2.3291, 2.3203, 2.313, 2.3109, 2.3039, 2.2936, 2.2892, 
    2.2863, 2.3046, 2.308, 2.3154, 2.3231, 2.3165, 2.3315, 2.3419, 2.3665, 
    2.3807, 2.3803, 2.4016, 2.4058, 2.4093, 2.4329, 2.4401, 2.4175, 2.4131, 
    2.4202, 2.4286, 2.4135, 2.4269, 2.4011, 2.3887, 2.3722, 2.3726, 2.3704, 
    2.3744, 2.4175, 2.4118, 2.404, 2.3849, 2.3726, 2.3858, 2.3705, 2.3706, 
    2.4064, 2.3811, 2.4034, 2.3911, 2.3977, 2.4078, 2.4172, 2.384, 2.3663, 
    2.3699, 2.3779, 2.3986, 2.3916, 2.4006, 2.401, 2.4042, 2.4158, 2.4207, 
    2.4213, 2.4155, 2.4044, 2.4201, 2.4365, 2.4338, 2.4272, 2.4332, 2.431, 
    2.4344, 2.4395, 2.4489, 2.4573, 2.4784, 2.4996, 2.4916, 2.4813, 2.4897, 
    2.4486, 2.4519, 2.4316, 2.4239, 2.4143, 2.4182, 2.4087, 2.416, 2.4147, 
    2.4002, 2.3703, 2.3721, 2.3898, 2.3921, 2.359, 2.3778, 2.3762, 2.3817, 
    2.3666, 2.3635, 2.3693, 2.3785, 2.3631, 2.3713, 2.3568, 2.364, 2.3501, 
    2.353, 2.3644, 2.3688, 2.3931, 2.3948, 2.4043, 2.3933, 2.3988, 2.4024, 
    2.3888, 2.3863, 2.3844, 2.3747, 2.3737, 2.3807, 2.3717, 2.3819, 2.3716, 
    2.3459, 2.343, 2.3314, 2.3771, 2.3816, 2.4014, 2.3925, 2.3849, 2.4021, 
    2.3952, 2.3893, 2.3713, 2.3655, 2.3753, 2.3877, 2.3957, 2.3776, 2.3796, 
    2.3725, 2.3576, 2.3472, 2.3473, 2.3485, 2.3394, 2.3173, 2.3103, 2.3005, 
    2.3012, 2.3306, 2.3285, 2.329, 2.311, 2.3121, 2.3266, 2.3279, 2.3315, 
    2.3195, 2.3244, 2.3586, 2.3507, 2.3307, 2.3086, 2.3184, 2.2872, 2.2943, 
    2.2939, 2.2937, 2.2889, 2.2913, 2.3126, 2.3048, 2.303, 2.3063, 2.3007, 
    2.3105, 2.3184, 2.3148, 2.306, 2.3059, 2.3115, 2.3326, 2.3308, 2.315, 
    2.3079, 2.3106, 2.3165, 2.3141, 2.3248, 2.3219, 2.3134, 2.2949, 2.3074, 
    2.3128, 2.3239, 2.3217, 2.3287, 2.3419, 2.354, 2.3403, 2.329, 2.3263, 
    2.3325, 2.3217, 2.3316, 2.3299, 2.3056, 2.2992, 2.3162, 2.3305, 2.3126, 
    2.2894, 2.2849, 2.2894, 2.267, 2.2334, 2.2562, 2.245, 2.2303, 2.2349, 
    2.2221, 2.2216, 2.2151, 2.2261, 2.2142, 2.2369, 2.2347, 2.2465, 2.2423, 
    2.223, 2.2288, 2.2382, 2.2519, 2.252, 2.2317, 2.2267, 2.2323, 2.2222, 
    2.2073, 2.2292, 2.2304, 2.2207, 2.221, 2.2052, 2.2263, 2.2201, 2.2221, 
    2.224, 2.2414, 2.2511, 2.2383, 2.2477, 2.2538, 2.2455, 2.2428, 2.2404, 
    2.2385, 2.232, 2.2207, 2.2251, 2.2103, 2.2089, 2.2052, 2.2257, 2.2333, 
    2.2168, 2.226, 2.2231, 2.223, 2.2199, 2.2287, 2.2334, 2.2101, 2.2191, 
    2.2383, 2.2376, 2.2278, 2.2209, 2.2324, 2.2366, 2.2399, 2.2143, 2.2121, 
    2.1962, 2.2022, 2.1837, 2.2039, 2.2011, 2.1854, 2.1847, 2.1733, 2.1551, 
    2.1361, 2.1515, 2.1422, 2.1431, 2.1247, 2.0988, 2.0892, 2.0761, 2.0959, 
    2.0895, 2.0781, 2.0867, 2.0956, 2.0926, 2.1236, 2.1724, 2.1646, 2.1722, 
    2.1857, 2.2005, 2.2288, 2.2153, 2.1931, 2.1823, 2.1694, 2.1679, 2.1758, 
    2.1987, 2.2216, 2.2081, 2.1965, 2.1906, 2.1992, 2.1806, 2.1866, 2.1953, 
    2.1912, 2.1732, 2.1749, 2.2168, 2.2253, 2.2157, 2.2069, 2.2143, 2.2221, 
    2.1893, 2.192, 2.1955, 2.2011, 2.2115, 2.2102, 2.2121, 2.2088, 2.2194, 
    2.2172, 2.2029)), class = "data.frame", row.names = c("1997/01/02", 
    "1997/01/03", "1997/01/06", "1997/01/07", "1997/01/08", "1997/01/09", 
    "1997/01/10", "1997/01/13", "1997/01/14", "1997/01/15", "1997/01/16", 
    "1997/01/17", "1997/01/20", "1997/01/21", "1997/01/22", "1997/01/23", 
    "1997/01/24", "1997/01/27", "1997/01/28", "1997/01/29", "1997/01/30", 
    "1997/01/31", "1997/02/03", "1997/02/04", "1997/02/05", "1997/02/06", 
    "1997/02/07", "1997/02/10", "1997/02/11", "1997/02/12", "1997/02/13", 
    "1997/02/14", "1997/02/17", "1997/02/18", "1997/02/19", "1997/02/20", 
    "1997/02/21", "1997/02/24", "1997/02/25", "1997/02/26", "1997/02/27", 
    "1997/02/28", "1997/03/03", "1997/03/04", "1997/03/05", "1997/03/06", 
    "1997/03/07", "1997/03/10", "1997/03/11", "1997/03/12", "1997/03/13", 
    "1997/03/14", "1997/03/17", "1997/03/18", "1997/03/19", "1997/03/20", 
    "1997/03/21", "1997/03/24", "1997/03/25", "1997/03/26", "1997/03/27", 
    "1997/03/31", "1997/04/01", "1997/04/02", "1997/04/03", "1997/04/04", 
    "1997/04/07", "1997/04/08", "1997/04/09", "1997/04/10", "1997/04/11", 
    "1997/04/14", "1997/04/15", "1997/04/16", "1997/04/17", "1997/04/18", 
    "1997/04/21", "1997/04/22", "1997/04/23", "1997/04/24", "1997/04/25", 
    "1997/04/28", "1997/04/29", "1997/04/30", "1997/05/01", "1997/05/02", 
    "1997/05/05", "1997/05/06", "1997/05/07", "1997/05/08", "1997/05/09", 
    "1997/05/12", "1997/05/13", "1997/05/14", "1997/05/15", "1997/05/16", 
    "1997/05/20", "1997/05/21", "1997/05/22", "1997/05/23", "1997/05/26", 
    "1997/05/27", "1997/05/28", "1997/05/29", "1997/05/30", "1997/06/02", 
    "1997/06/03", "1997/06/04", "1997/06/05", "1997/06/06", "1997/06/09", 
    "1997/06/10", "1997/06/11", "1997/06/12", "1997/06/13", "1997/06/16", 
    "1997/06/17", "1997/06/18", "1997/06/19", "1997/06/20", "1997/06/23", 
    "1997/06/24", "1997/06/25", "1997/06/26", "1997/06/27", "1997/06/30", 
    "1997/07/02", "1997/07/03", "1997/07/04", "1997/07/07", "1997/07/08", 
    "1997/07/09", "1997/07/10", "1997/07/11", "1997/07/14", "1997/07/15", 
    "1997/07/16", "1997/07/17", "1997/07/18", "1997/07/21", "1997/07/22", 
    "1997/07/23", "1997/07/24", "1997/07/25", "1997/07/28", "1997/07/29", 
    "1997/07/30", "1997/07/31", "1997/08/01", "1997/08/05", "1997/08/06", 
    "1997/08/07", "1997/08/08", "1997/08/11", "1997/08/12", "1997/08/13", 
    "1997/08/14", "1997/08/15", "1997/08/18", "1997/08/19", "1997/08/20", 
    "1997/08/21", "1997/08/22", "1997/08/25", "1997/08/26", "1997/08/27", 
    "1997/08/28", "1997/08/29", "1997/09/02", "1997/09/03", "1997/09/04", 
    "1997/09/05", "1997/09/08", "1997/09/09", "1997/09/10", "1997/09/11", 
    "1997/09/12", "1997/09/15", "1997/09/16", "1997/09/17", "1997/09/18", 
    "1997/09/19", "1997/09/22", "1997/09/23", "1997/09/24", "1997/09/25", 
    "1997/09/26", "1997/09/29", "1997/09/30", "1997/10/01", "1997/10/02", 
    "1997/10/03", "1997/10/06", "1997/10/07", "1997/10/08", "1997/10/09", 
    "1997/10/10", "1997/10/14", "1997/10/15", "1997/10/16", "1997/10/17", 
    "1997/10/20", "1997/10/21", "1997/10/22", "1997/10/23", "1997/10/24", 
    "1997/10/27", "1997/10/28", "1997/10/29", "1997/10/30", "1997/10/31", 
    "1997/11/03", "1997/11/04", "1997/11/05", "1997/11/06", "1997/11/07", 
    "1997/11/10", "1997/11/12", "1997/11/13", "1997/11/14", "1997/11/17", 
    "1997/11/18", "1997/11/19", "1997/11/20", "1997/11/21", "1997/11/24", 
    "1997/11/25", "1997/11/26", "1997/11/27", "1997/11/28", "1997/12/01", 
    "1997/12/02", "1997/12/03", "1997/12/04", "1997/12/05", "1997/12/08", 
    "1997/12/09", "1997/12/10", "1997/12/11", "1997/12/12", "1997/12/15", 
    "1997/12/16", "1997/12/17", "1997/12/18", "1997/12/19", "1997/12/22", 
    "1997/12/23", "1997/12/24", "1997/12/29", "1997/12/30", "1997/12/31", 
    "1998/01/02", "1998/01/05", "1998/01/06", "1998/01/07", "1998/01/08", 
    "1998/01/09", "1998/01/12", "1998/01/13", "1998/01/14", "1998/01/15", 
    "1998/01/16", "1998/01/19", "1998/01/20", "1998/01/21", "1998/01/22", 
    "1998/01/23", "1998/01/26", "1998/01/27", "1998/01/28", "1998/01/29", 
    "1998/01/30", "1998/02/02", "1998/02/03", "1998/02/04", "1998/02/05", 
    "1998/02/06", "1998/02/09", "1998/02/10", "1998/02/11", "1998/02/12", 
    "1998/02/13", "1998/02/16", "1998/02/17", "1998/02/18", "1998/02/19", 
    "1998/02/20", "1998/02/23", "1998/02/24", "1998/02/25", "1998/02/26", 
    "1998/02/27", "1998/03/02", "1998/03/03", "1998/03/04", "1998/03/05", 
    "1998/03/06", "1998/03/09", "1998/03/10", "1998/03/11", "1998/03/12", 
    "1998/03/13", "1998/03/16", "1998/03/17", "1998/03/18", "1998/03/19", 
    "1998/03/20", "1998/03/23", "1998/03/24", "1998/03/25", "1998/03/26", 
    "1998/03/27", "1998/03/30", "1998/03/31", "1998/04/01", "1998/04/02", 
    "1998/04/03", "1998/04/06", "1998/04/07", "1998/04/08", "1998/04/09", 
    "1998/04/13", "1998/04/14", "1998/04/15", "1998/04/16", "1998/04/17", 
    "1998/04/20", "1998/04/21", "1998/04/22", "1998/04/23", "1998/04/24", 
    "1998/04/27", "1998/04/28", "1998/04/29", "1998/04/30", "1998/05/01", 
    "1998/05/04", "1998/05/05", "1998/05/06", "1998/05/07", "1998/05/08", 
    "1998/05/11", "1998/05/12", "1998/05/13", "1998/05/14", "1998/05/15", 
    "1998/05/19", "1998/05/20", "1998/05/21", "1998/05/22", "1998/05/25", 
    "1998/05/26", "1998/05/27", "1998/05/28", "1998/05/29", "1998/06/01", 
    "1998/06/02", "1998/06/03", "1998/06/04", "1998/06/05", "1998/06/08", 
    "1998/06/09", "1998/06/10", "1998/06/11", "1998/06/12", "1998/06/15", 
    "1998/06/16", "1998/06/17", "1998/06/18", "1998/06/19", "1998/06/22", 
    "1998/06/23", "1998/06/24", "1998/06/25", "1998/06/26", "1998/06/29", 
    "1998/06/30", "1998/07/02", "1998/07/03", "1998/07/06", "1998/07/07", 
    "1998/07/08", "1998/07/09", "1998/07/10", "1998/07/13", "1998/07/14", 
    "1998/07/15", "1998/07/16", "1998/07/17", "1998/07/20", "1998/07/21", 
    "1998/07/22", "1998/07/23", "1998/07/24", "1998/07/27", "1998/07/28", 
    "1998/07/29", "1998/07/30", "1998/07/31", "1998/08/04", "1998/08/05", 
    "1998/08/06", "1998/08/07", "1998/08/10", "1998/08/11", "1998/08/12", 
    "1998/08/13", "1998/08/14", "1998/08/17", "1998/08/18", "1998/08/19", 
    "1998/08/20", "1998/08/21", "1998/08/24", "1998/08/25", "1998/08/26", 
    "1998/08/27", "1998/08/28", "1998/08/31", "1998/09/01", "1998/09/02", 
    "1998/09/03", "1998/09/04", "1998/09/08", "1998/09/09", "1998/09/10", 
    "1998/09/11", "1998/09/14", "1998/09/15", "1998/09/16", "1998/09/17", 
    "1998/09/18", "1998/09/21", "1998/09/22", "1998/09/23", "1998/09/24", 
    "1998/09/25", "1998/09/28", "1998/09/29", "1998/09/30", "1998/10/01", 
    "1998/10/02", "1998/10/05", "1998/10/06", "1998/10/07", "1998/10/08", 
    "1998/10/09", "1998/10/13", "1998/10/14", "1998/10/15", "1998/10/16", 
    "1998/10/19", "1998/10/20", "1998/10/21", "1998/10/22", "1998/10/26", 
    "1998/10/27", "1998/10/28", "1998/10/29", "1998/10/30", "1998/11/02", 
    "1998/11/03", "1998/11/04", "1998/11/05", "1998/11/06", "1998/11/09", 
    "1998/11/10", "1998/11/12", "1998/11/13", "1998/11/16", "1998/11/17", 
    "1998/11/18", "1998/11/20", "1998/11/23", "1998/11/24", "1998/11/25", 
    "1998/11/26", "1998/11/27", "1998/11/30", "1998/12/01", "1998/12/02", 
    "1998/12/03", "1998/12/04", "1998/12/07", "1998/12/08", "1998/12/09", 
    "1998/12/10", "1998/12/11", "1998/12/14", "1998/12/15", "1998/12/16", 
    "1998/12/17", "1998/12/18", "1998/12/21", "1998/12/22", "1998/12/23", 
    "1998/12/24", "1998/12/29", "1998/12/30", "1998/12/31", "1999/01/04", 
    "1999/01/05", "1999/01/06", "1999/01/07", "1999/01/08", "1999/01/11", 
    "1999/01/12", "1999/01/13", "1999/01/14", "1999/01/15", "1999/01/18", 
    "1999/01/19", "1999/01/20", "1999/01/21", "1999/01/22", "1999/01/25", 
    "1999/01/26", "1999/01/27", "1999/01/28", "1999/01/29", "1999/02/01", 
    "1999/02/02", "1999/02/03", "1999/02/04", "1999/02/05", "1999/02/08", 
    "1999/02/09", "1999/02/10", "1999/02/11", "1999/02/12", "1999/02/15", 
    "1999/02/16", "1999/02/17", "1999/02/18", "1999/02/19", "1999/02/22", 
    "1999/02/23", "1999/02/24", "1999/02/25", "1999/02/26", "1999/03/01", 
    "1999/03/02", "1999/03/03", "1999/03/04", "1999/03/05", "1999/03/08", 
    "1999/03/09", "1999/03/10", "1999/03/11", "1999/03/12", "1999/03/15", 
    "1999/03/16", "1999/03/17", "1999/03/18", "1999/03/19", "1999/03/22", 
    "1999/03/23", "1999/03/24", "1999/03/25", "1999/03/26", "1999/03/29", 
    "1999/03/30", "1999/03/31", "1999/04/01", "1999/04/05", "1999/04/06", 
    "1999/04/07", "1999/04/08", "1999/04/09", "1999/04/12", "1999/04/13", 
    "1999/04/14", "1999/04/15", "1999/04/16", "1999/04/19", "1999/04/20", 
    "1999/04/21", "1999/04/22", "1999/04/23", "1999/04/26", "1999/04/27", 
    "1999/04/28", "1999/04/29", "1999/04/30", "1999/05/03", "1999/05/04", 
    "1999/05/05", "1999/05/06", "1999/05/07", "1999/05/10", "1999/05/11", 
    "1999/05/12", "1999/05/13", "1999/05/14", "1999/05/17", "1999/05/18", 
    "1999/05/19", "1999/05/20", "1999/05/21", "1999/05/25", "1999/05/26", 
    "1999/05/27", "1999/05/28", "1999/05/31", "1999/06/01", "1999/06/02", 
    "1999/06/03", "1999/06/04", "1999/06/07", "1999/06/08", "1999/06/09", 
    "1999/06/10", "1999/06/11", "1999/06/14", "1999/06/15", "1999/06/16", 
    "1999/06/17", "1999/06/18", "1999/06/21", "1999/06/22", "1999/06/23", 
    "1999/06/24", "1999/06/25", "1999/06/28", "1999/06/29", "1999/06/30", 
    "1999/07/02", "1999/07/05", "1999/07/06", "1999/07/07", "1999/07/08", 
    "1999/07/09", "1999/07/12", "1999/07/13", "1999/07/14", "1999/07/15", 
    "1999/07/16", "1999/07/19", "1999/07/20", "1999/07/21", "1999/07/22", 
    "1999/07/23", "1999/07/26", "1999/07/27", "1999/07/28", "1999/07/29", 
    "1999/07/30", "1999/08/03", "1999/08/04", "1999/08/05", "1999/08/06", 
    "1999/08/09", "1999/08/10", "1999/08/11", "1999/08/12", "1999/08/13", 
    "1999/08/16", "1999/08/17", "1999/08/18", "1999/08/19", "1999/08/20", 
    "1999/08/23", "1999/08/24", "1999/08/25", "1999/08/26", "1999/08/27", 
    "1999/08/30", "1999/08/31", "1999/09/01", "1999/09/02", "1999/09/03", 
    "1999/09/07", "1999/09/08", "1999/09/09", "1999/09/10", "1999/09/13", 
    "1999/09/14", "1999/09/15", "1999/09/16", "1999/09/17", "1999/09/20", 
    "1999/09/21", "1999/09/22", "1999/09/23", "1999/09/24", "1999/09/27", 
    "1999/09/28", "1999/09/29", "1999/09/30", "1999/10/01", "1999/10/04", 
    "1999/10/05", "1999/10/06", "1999/10/07", "1999/10/08", "1999/10/12", 
    "1999/10/13", "1999/10/14", "1999/10/15", "1999/10/18", "1999/10/19", 
    "1999/10/20", "1999/10/21", "1999/10/22", "1999/10/25", "1999/10/26", 
    "1999/10/27", "1999/10/28", "1999/10/29", "1999/11/01", "1999/11/02", 
    "1999/11/03", "1999/11/04", "1999/11/05", "1999/11/08", "1999/11/09", 
    "1999/11/10", "1999/11/12", "1999/11/15", "1999/11/16", "1999/11/17", 
    "1999/11/18", "1999/11/19", "1999/11/22", "1999/11/23", "1999/11/24", 
    "1999/11/25", "1999/11/26", "1999/11/29", "1999/11/30", "1999/12/01", 
    "1999/12/02", "1999/12/03", "1999/12/06", "1999/12/07", "1999/12/08", 
    "1999/12/09", "1999/12/10", "1999/12/13", "1999/12/14", "1999/12/15", 
    "1999/12/16", "1999/12/17", "1999/12/20", "1999/12/21", "1999/12/22", 
    "1999/12/23", "1999/12/24", "1999/12/29", "1999/12/30", "1999/12/31", 
    "2000/01/04", "2000/01/05", "2000/01/06", "2000/01/07", "2000/01/10", 
    "2000/01/11", "2000/01/12", "2000/01/13", "2000/01/14", "2000/01/17", 
    "2000/01/18", "2000/01/19", "2000/01/20", "2000/01/21", "2000/01/24", 
    "2000/01/25", "2000/01/26", "2000/01/27", "2000/01/28", "2000/01/31", 
    "2000/02/01", "2000/02/02", "2000/02/03", "2000/02/04", "2000/02/07", 
    "2000/02/08", "2000/02/09", "2000/02/10", "2000/02/11", "2000/02/14", 
    "2000/02/15", "2000/02/16", "2000/02/17", "2000/02/18", "2000/02/21", 
    "2000/02/22", "2000/02/23", "2000/02/24", "2000/02/25", "2000/02/28", 
    "2000/02/29", "2000/03/01", "2000/03/02", "2000/03/03", "2000/03/06", 
    "2000/03/07", "2000/03/08", "2000/03/09", "2000/03/10", "2000/03/13", 
    "2000/03/14", "2000/03/15", "2000/03/16", "2000/03/17", "2000/03/20", 
    "2000/03/21", "2000/03/22", "2000/03/23", "2000/03/24", "2000/03/27", 
    "2000/03/28", "2000/03/29", "2000/03/30", "2000/03/31", "2000/04/03", 
    "2000/04/04", "2000/04/05", "2000/04/06", "2000/04/07", "2000/04/10", 
    "2000/04/11", "2000/04/12", "2000/04/13", "2000/04/14", "2000/04/17", 
    "2000/04/18", "2000/04/19", "2000/04/20", "2000/04/24", "2000/04/25", 
    "2000/04/26", "2000/04/27", "2000/04/28", "2000/05/01", "2000/05/02", 
    "2000/05/03", "2000/05/04", "2000/05/05", "2000/05/08", "2000/05/09", 
    "2000/05/10", "2000/05/11", "2000/05/12", "2000/05/15", "2000/05/16", 
    "2000/05/17", "2000/05/18", "2000/05/19", "2000/05/23", "2000/05/24", 
    "2000/05/25", "2000/05/26", "2000/05/29", "2000/05/30", "2000/05/31", 
    "2000/06/01", "2000/06/02", "2000/06/05", "2000/06/06", "2000/06/07", 
    "2000/06/08", "2000/06/09", "2000/06/12", "2000/06/13", "2000/06/14", 
    "2000/06/15", "2000/06/16", "2000/06/19", "2000/06/20", "2000/06/21", 
    "2000/06/22", "2000/06/23", "2000/06/26", "2000/06/27", "2000/06/28", 
    "2000/06/29", "2000/06/30", "2000/07/04", "2000/07/05", "2000/07/06", 
    "2000/07/07", "2000/07/10", "2000/07/11", "2000/07/12", "2000/07/13", 
    "2000/07/14", "2000/07/17", "2000/07/18", "2000/07/19", "2000/07/20", 
    "2000/07/21", "2000/07/24", "2000/07/25", "2000/07/26", "2000/07/27", 
    "2000/07/28", "2000/07/31", "2000/08/01", "2000/08/02", "2000/08/03", 
    "2000/08/04", "2000/08/08", "2000/08/09", "2000/08/10", "2000/08/11", 
    "2000/08/14", "2000/08/15", "2000/08/16", "2000/08/17", "2000/08/18", 
    "2000/08/21", "2000/08/22", "2000/08/23", "2000/08/24", "2000/08/25", 
    "2000/08/28", "2000/08/29", "2000/08/30", "2000/08/31", "2000/09/01", 
    "2000/09/05", "2000/09/06", "2000/09/07", "2000/09/08", "2000/09/11", 
    "2000/09/12", "2000/09/13", "2000/09/14", "2000/09/15", "2000/09/18", 
    "2000/09/19", "2000/09/20", "2000/09/21", "2000/09/22", "2000/09/25", 
    "2000/09/26", "2000/09/27", "2000/09/28", "2000/09/29", "2000/10/02", 
    "2000/10/03", "2000/10/04", "2000/10/05", "2000/10/06", "2000/10/10", 
    "2000/10/11", "2000/10/12", "2000/10/13", "2000/10/16", "2000/10/17", 
    "2000/10/18", "2000/10/19", "2000/10/20", "2000/10/23", "2000/10/24", 
    "2000/10/25", "2000/10/26", "2000/10/27", "2000/10/30", "2000/10/31", 
    "2000/11/01", "2000/11/02", "2000/11/03", "2000/11/06", "2000/11/07", 
    "2000/11/08", "2000/11/09", "2000/11/10", "2000/11/14", "2000/11/15", 
    "2000/11/16", "2000/11/17", "2000/11/20", "2000/11/21"))

"fremantle"<-
structure(.Data = list(Year = c(1897, 1898, 1899, 1900, 1901, 1903, 1904, 1905, 
    1906, 1908, 1909, 1912, 1914, 1915, 1916, 1917, 1918, 1919, 1920, 1921, 
    1922, 1923, 1924, 1925, 1927, 1928, 1929, 1930, 1931, 1932, 1933, 1934, 
    1935, 1936, 1937, 1938, 1939, 1940, 1941, 1943, 1944, 1945, 1946, 1947, 
    1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 
    1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 
    1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 
    1984, 1985, 1986, 1987, 1988, 1989), "SeaLevel" = c(1.58, 1.71, 1.4, 
    1.34, 1.43, 1.19, 1.55, 1.34, 1.37, 1.46, 1.92, 1.37, 1.19, 1.4, 1.28, 
    1.52, 1.52, 1.58, 1.49, 1.65, 1.37, 1.49, 1.46, 1.34, 1.74, 1.62, 1.46, 
    1.71, 1.74, 1.55, 1.43, 1.62, 1.49, 1.58, 1.34, 1.37, 1.62, 1.31, 1.43, 
    1.49, 1.55, 1.71, 1.49, 1.46, 1.52, 1.58, 1.65, 1.49, 1.52, 1.52, 1.49, 
    1.62, 1.86, 1.58, 1.62, 1.46, 1.43, 1.46, 1.62, 1.68, 1.83, 1.62, 1.46, 
    1.58, 1.77, 1.62, 1.71, 1.46, 1.6, 1.5, 1.6, 1.9, 1.7, 1.4, 1.8, 1.37, 
    1.46, 1.61, 1.43, 1.67, 1.62, 1.57, 1.56, 1.46, 1.7, 1.51), SOI = c(
    -0.67, 0.57, 0.16, -0.65, 0.06, 0.47, 0.39, -1.78, 0.2, 0.28, 0.28, 
    -0.97, -0.92, 0.16, 0.62, 2.12, 0.05, -1.09, 0.08, 0.66, 0.33, -0.36, 
    0.33, -0.24, 0.27, 0.43, 0.46, 0.03, 0.39, -0.68, 0.09, -0.01, 0.14, 
    0.03, 0.09, 0.86, 0.02, -1.52, -1.44, 0.35, -0.27, 0.42, -0.79, 0.16, 
    -0.24, -0.21, 1.49, -0.6899999999999999, -0.23, -0.76, 0.23, 0.89, 1, 
    -0.45, -0.5, -0.11, 0.28, -0.01, 0.38, -0.32, 0.53, -0.97, -0.53, 0.25, 
    0.19, -0.66, 0.28, 1.06, -0.88, 0.63, 0.97, 1.32, 0.06, -1.13, -0.3, 
    -0.08, -0.43, 0.06, -1.44, -0.94, -0.14, -0.07000000000000001, -0.32, 
    -1.47, 0.73, 0.61)), class = "data.frame", row.names = c("1", "2", "3", 
    "4", "5", "7", "8", "9", "10", "12", "13", "16", "18", "19", "20", "21",
    "22", "23", "24", "25", "26", "27", "28", "29", "31", "32", "33", "34", 
    "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "47", 
    "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", 
    "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", 
    "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", 
    "84", "85", "86", "87", "88", "89", "90", "91", "92", "93"))

"glass"<-
c(0.55, 0.74, 0.77, 0.8100000000000001, 0.84, 0.93, 1.04, 1.11, 1.13, 1.24, 
    1.25, 1.27, 1.28, 1.29, 1.3, 1.36, 1.39, 1.42, 1.48, 1.48, 1.49, 1.49, 
    1.5, 1.5, 1.51, 1.52, 1.53, 1.54, 1.55, 1.55, 1.58, 1.59, 1.6, 1.61, 
    1.61, 1.61, 1.61, 1.62, 1.62, 1.63, 1.64, 1.66, 1.66, 1.66, 1.67, 1.68, 
    1.68, 1.69, 1.7, 1.7, 1.73, 1.76, 1.76, 1.77, 1.78, 1.81, 1.82, 1.84, 
    1.84, 1.89, 2, 2.01, 2.24)


"portpirie"<-
structure(.Data = list(Year = c(1923, 1924, 1925, 1926, 1927, 1928, 1929, 1930, 
    1931, 1932, 1933, 1934, 1935, 1936, 1937, 1938, 1939, 1940, 1941, 1942, 
    1943, 1944, 1945, 1946, 1947, 1948, 1949, 1950, 1951, 1952, 1953, 1954, 
    1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 
    1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 
    1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987), "SeaLevel" = c(
    4.03, 3.83, 3.65, 3.88, 4.01, 4.08, 4.18, 3.8, 4.36, 3.96, 3.98, 4.69, 
    3.85, 3.96, 3.85, 3.93, 3.75, 3.63, 3.57, 4.25, 3.97, 4.05, 4.24, 4.22, 
    3.73, 4.37, 4.06, 3.71, 3.96, 4.06, 4.55, 3.79, 3.89, 4.11, 3.85, 3.86, 
    3.86, 4.21, 4.01, 4.11, 4.24, 3.96, 4.21, 3.74, 3.85, 3.88, 3.66, 4.11, 
    3.71, 4.18, 3.9, 3.78, 3.91, 3.72, 4, 3.66, 3.62, 4.33, 4.55, 3.75, 
    4.08, 3.9, 3.88, 3.94, 4.33)), class = "data.frame", row.names = c("1", 
    "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", 
    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", 
    "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", 
    "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", 
    "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", 
    "63", "64", "65"))


"rain"<-
c(0, 2.3, 1.3, 6.9, 4.6, 0, 1, 1.5, 1.8, 1.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 3.3, 2.8, 3, 0, 8.1, 1.5, 4.1, 0, 0, 0, 0, 0, 4.8, 31.8, 0, 
    1.5, 25.4, 5.1, 15, 16.8, 16.3, 0, 0, 11.7, 2.3, 2, 10.9, 8.1, 2.3, 1.5,
    0, 0, 0, 3, 1.8, 2.5, 3, 6.6, 2, 8.4, 7.4, 11.9, 32.5, 10.7, 2.5, 18.3, 
    5.1, 13.5, 10.9, 8.1, 0.8, 12.7, 0.3, 15.7, 18.5, 2.3, 1.8, 5.8, 2, 7.1,
    2.3, 0, 10.7, 6.9, 4.8, 0, 3.8, 3.8, 5.8, 8.4, 7.6, 3, 3.6, 3.6, 4.8, 
    14.7, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    3, 14.2, 5.6, 2.5, 1.8, 6.4, 0.8, 2, 5.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 1.3, 0, 0, 0, 0, 2.3, 5.1, 0, 1.3, 4.6, 0, 0, 0, 0, 0.3, 0.8, 6.4,
    17, 0.5, 5.1, 0, 0, 0.5, 0, 0, 0, 0, 0, 0, 15.5, 1.3, 0, 0, 0, 0, 0, 0, 
    0, 0, 3, 3.8, 0, 17.8, 0, 13, 8.1, 0, 0, 0, 0, 0, 5.1, 0, 2.3, 1.3, 0.5,
    0.3, 27.9, 0, 0, 0, 4.6, 2, 3, 7.9, 1, 0, 4.6, 0, 20.3, 14.7, 1, 7.6, 
    3.6, 0, 3.3, 7.1, 4.1, 2.5, 0, 0, 0, 0, 24.1, 4.3, 0, 0, 0, 0, 4.1, 0, 
    0.5, 0, 3.6, 17.5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 5.6, 0, 0, 7.4, 0, 11.7, 
    7.6, 1, 6.6, 11.4, 1, 3.6, 6.6, 1.3, 7.6, 1.8, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.8, 1.3, 0.8, 0, 0, 0, 
    0, 0, 8.1, 15, 2, 0.3, 11.9, 1.3, 5.1, 10.2, 1.8, 3.3, 14.7, 13, 0, 0, 
    9.4, 0, 2.5, 0, 0, 0, 0, 2, 3.3, 11.4, 13.2, 7.9, 0, 0, 0, 4.6, 0, 0, 0,
    0, 2.5, 3, 8.9, 6.4, 6.4, 22.9, 15.7, 0, 16.8, 3.8, 22.1, 5.1, 11.9, 
    6.9, 0, 0, 0, 11.9, 2.3, 4.3, 4.6, 18.3, 0, 31.8, 8.1, 1.5, 5.8, 3, 0, 
    0, 0, 11.4, 21.3, 11.7, 20.3, 1.3, 44.5, 14, 15, 6.9, 1.3, 1, 1.8, 22.9,
    8.1, 5.1, 1, 8.4, 3.8, 4.3, 0.8, 4.8, 2.8, 1.5, 0.8, 0.5, 1, 15.7, 8.6, 
    2.5, 5.8, 0, 0, 0, 0, 0, 0, 6.6, 1.8, 7.6, 13.7, 11.7, 20.6, 28.7, 6.4, 
    20.3, 13.5, 5.1, 16.8, 0, 10.2, 30.5, 0, 0, 43.2, 4.3, 6.6, 1, 0, 3, 
    3.6, 1.3, 0, 0, 9.1, 2, 1.8, 0, 2.5, 3.8, 0, 0.3, 0, 0.5, 0, 0, 2, 4.6, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.8, 
    2.3, 0, 5.1, 11.2, 2.3, 0, 5.1, 2, 1.3, 0, 0, 0, 0, 0, 0, 0, 0.5, 6.1, 
    0, 4.6, 0, 0, 0, 0, 0, 0, 0, 0, 2.5, 0, 5.6, 0.8, 0, 0, 0, 0, 0, 0, 0, 
    15.2, 21.1, 0, 7.4, 7.6, 0, 0, 11.2, 0, 6.4, 0, 0, 0, 0, 0.8, 0, 0, 0, 
    0, 0, 0, 1.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.1, 
    5.3, 4.1, 0, 3.6, 4.3, 12.7, 7.6, 6.6, 22.9, 3.6, 0, 0, 2.8, 0, 0, 15.2,
    15.5, 1.3, 0, 0.3, 1, 0, 0, 5.1, 5.6, 25.9, 0, 0, 4.1, 0, 2.5, 7.6, 
    12.2, 2.8, 7.1, 5.1, 3.6, 0, 0, 0.5, 4.8, 19.8, 14.2, 0.3, 0, 5.1, 1.3, 
    0, 1.8, 0, 6.9, 5.1, 21.8, 10.2, 2.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 1.3, 0.3, 0, 0.8, 21.6, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.3, 2, 
    1.8, 0, 0, 0, 0, 0, 0, 0, 2.5, 3, 0, 1.8, 0, 6.9, 3.3, 2.8, 5.8, 0, 0, 
    0, 0, 0, 3.8, 0, 0, 2.8, 0, 4.8, 0, 1.5, 0, 0.5, 0, 0, 0, 4.3, 1.3, 0.8,
    19.3, 8.4, 0, 0, 16.5, 8.4, 0, 12.7, 7.6, 1.5, 0, 0, 0, 0, 0, 0.8, 1.3, 
    18.8, 4.1, 21.8, 9.4, 6.4, 0, 3.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 4.8, 14.5, 7.9, 10.2, 13.5, 7.6, 26.2, 3.3, 7.6, 22.4, 20.3, 3, 4.1, 
    1.3, 4.1, 35.6, 3.6, 2.5, 0, 0, 0, 0, 6.4, 11.4, 22.9, 16.8, 14.2, 18.5,
    0, 0, 4.3, 19.1, 10.2, 7.1, 8.9, 0.5, 7.4, 0.5, 1.3, 4.1, 0, 0, 0, 0, 
    2.5, 0, 1.8, 0.5, 5.1, 0, 0.8, 6.4, 3.6, 2.5, 0, 1.8, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 2.5, 38.1, 5.3, 1.3, 5.6, 7.6, 6.4, 6.9, 20.1, 0, 1, 9.4, 13, 
    9.1, 3.8, 5.1, 8.6, 2.5, 7.6, 0, 0, 0.5, 12.7, 2.5, 1.8, 0.8, 1.5, 3.6, 
    7.6, 2, 3, 2.3, 5.3, 1.5, 3.6, 0.8, 1.8, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 
    6.6, 2.8, 0.5, 0, 1.3, 6.9, 3.6, 27.7, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 2.5, 2.8, 2.3, 0, 0, 4.1, 3.8, 2.5, 6.4, 0.8, 0, 0, 0, 0.5, 
    6.6, 0, 0, 0, 0, 0, 0, 0.8, 1.5, 5.8, 2.8, 0.3, 4.3, 5.1, 4.6, 0.8, 0.3,
    0.3, 3.8, 0, 2.8, 0, 0, 0, 0, 0, 0, 0, 0.3, 0.3, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 2.5, 10.4, 3.8, 4.6, 2.5, 0, 6.4, 0, 2.5, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 3.8, 3.8, 0, 0, 6.1, 0, 0, 12.7, 0.5, 6.4, 0, 5.1, 4.3, 0, 15.7, 
    2.3, 0, 0.3, 0, 0, 4.1, 0.3, 0, 0, 1, 0, 0, 0, 0, 0, 0, 4.3, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 11.4, 4.3, 5.1, 15.2, 
    0.3, 0, 0, 0, 0, 0, 8.9, 3.8, 21.3, 6.6, 0, 0.3, 32, 0, 2.3, 0, 6.4, 
    0.5, 3, 0, 0, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, 0, 10.2, 6.9, 0.5, 0, 0, 0, 
    0, 0, 0, 4.3, 9.1, 6.1, 8.4, 0, 4.1, 5.1, 17.8, 2, 2.5, 7.6, 0.8, 0, 
    7.6, 0, 0, 5.1, 1.8, 2.5, 0.3, 2.5, 6.6, 2.3, 0, 0, 0, 22.9, 5.6, 12.7, 
    3.8, 19.1, 17.3, 12.7, 6.6, 9.7, 3.8, 3.8, 7.6, 12.7, 31.8, 13, 22.9, 
    27.4, 4.3, 4.1, 0.5, 0, 0, 0, 0, 0, 0, 19.1, 0.5, 12.7, 4.1, 5.6, 0.3, 
    7.6, 0.5, 8.6, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.1, 5.1, 16.5, 6.9, 0, 
    0, 0.8, 0, 0, 0, 0, 0, 0, 14.7, 5.6, 10.2, 5.1, 6.4, 0, 0, 0, 16.5, 
    12.7, 0, 2.3, 0.5, 0.5, 2.5, 0, 6.4, 0, 15.5, 0.5, 0, 6.4, 0, 11.7, 5.8,
    2.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2.8, 0, 0, 13, 6.6, 0, 0, 2.5, 0.3, 1.3, 
    0, 0, 2.3, 0, 0, 0, 11.4, 10.7, 7.6, 0, 2.3, 0.5, 11.2, 6.4, 0, 6.1, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6.6, 5.1, 6.6, 0, 13,
    0, 0, 4.1, 2.5, 8.9, 6.4, 0.3, 0.5, 3, 0, 0, 6.9, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12.7, 0, 0, 0, 0, 
    0, 0, 0, 7.6, 0, 0, 12.7, 0, 18.3, 0, 3.8, 0, 0, 10.2, 5.3, 2.5, 0, 0, 
    6.9, 7.6, 0, 4.1, 0, 0, 0, 0, 0.3, 0, 0, 2.5, 0, 0.3, 3.8, 3.6, 0, 0, 0,
    3.8, 0, 2.5, 6.4, 33, 6.4, 0, 0, 0, 0, 0, 0, 0.3, 0, 5.1, 0.8, 0, 0, 0, 
    0, 0, 0, 5.1, 7.4, 7.6, 0, 0, 0, 0, 0, 0.8, 0.3, 0, 0, 0, 0, 0.8, 0, 
    4.3, 0, 0, 0, 0.5, 9.1, 0.3, 10.9, 0, 7.6, 15.5, 4.3, 16.5, 12.7, 1, 
    0.3, 4.8, 0, 0, 0.8, 0, 16.5, 26.4, 0.8, 9.7, 10.7, 39.1, 17.3, 0, 6.4, 
    0, 2.5, 0, 0, 0, 0, 9.1, 0, 2.5, 0, 0, 0, 0, 9.4, 2.8, 0, 0, 11.4, 16.5,
    0.5, 2.5, 0, 0, 0, 0, 0.5, 3, 0, 0, 0, 0, 0, 0, 8.9, 11.4, 4.8, 0.5, 
    7.6, 13.7, 12.7, 1, 10.2, 13.5, 10.2, 7.6, 11.4, 6.6, 6.4, 0.3, 0, 0, 
    0.3, 5.1, 6.4, 10.9, 6.4, 0.8, 2.8, 0.3, 22.4, 0.5, 5.1, 0.3, 0.3, 16.3,
    0, 4.3, 6.6, 3.8, 7.4, 9.1, 1.5, 7.6, 0, 0, 0.3, 0, 0, 0, 0, 0, 3.8, 
    6.9, 0, 2.3, 2.5, 0.3, 2.3, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 2.5, 9.4, 
    0, 0, 0, 0, 2.3, 0.3, 6.6, 15.2, 0, 0, 0, 0, 5.1, 0, 0, 0, 0, 0, 0, 0, 
    2.5, 0, 0, 0, 0, 0, 0, 0, 0, 6.9, 0, 0, 3.8, 6.4, 0, 0.8, 8.9, 30.5, 13,
    10.2, 31.8, 6.6, 7.1, 8.1, 0, 3.8, 1, 0, 0.5, 0, 0, 0, 0, 0, 6.4, 15.2, 
    8.9, 0.3, 0, 7.1, 6.9, 0.8, 0, 0, 2.5, 2.5, 0.3, 0, 0, 0, 0, 0, 1.5, 
    3.8, 0, 0, 0, 4.8, 0, 6.6, 8.1, 6.4, 0, 0, 0, 0, 5.1, 5.1, 0, 0, 0, 0, 
    6.4, 0, 0, 0, 0, 0, 0, 4.8, 2.5, 0, 0, 0, 0, 0, 0, 0, 3, 7.6, 3.8, 12.7,
    12.4, 4.1, 0.3, 2.5, 0, 2.5, 8.9, 0.5, 0, 0, 3.8, 1.8, 0, 0, 0, 0, 16.5,
    2.5, 0, 0.5, 4.8, 0, 0, 0, 0, 0, 7.6, 0, 0, 0, 6.4, 0, 0, 0.3, 0, 4.8, 
    0, 0, 0, 0, 0, 0, 5.6, 6.6, 0, 0, 0, 0.3, 0, 0, 0, 0, 7.6, 0, 0, 7.6, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.8, 0, 8.9, 0, 0, 0, 0, 2.5, 2.8, 0, 
    0, 20.1, 0, 0, 11.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    8.9, 7.6, 3.8, 5.1, 2.5, 0.5, 12.7, 4.3, 2.5, 3.8, 7.4, 3.8, 3.6, 25.4, 
    3.8, 0, 5.1, 9.4, 0, 0, 0, 0, 0, 11.7, 0, 0, 5.1, 6.1, 4.1, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 2.5, 0, 0, 0, 0, 9.9, 13.2, 0, 4.8, 0, 0, 3.8, 
    0, 0, 0, 0, 4.3, 21.8, 0, 10.2, 5.6, 11.7, 0, 13, 3.8, 8.6, 7.6, 4.1, 
    11.4, 3.6, 7.4, 5.1, 7.6, 13.7, 8.9, 3.3, 5.1, 0, 2, 7.6, 0, 20.3, 0.8, 
    0, 0, 4.3, 0, 6.6, 5.3, 6.4, 12.7, 1, 4.3, 0.8, 0, 2.5, 2.8, 12.7, 0, 0,
    6.4, 0, 0, 0.5, 4.8, 0, 0, 2.3, 0, 0, 1, 0, 2.5, 2.3, 32.3, 2.3, 7.4, 
    22.9, 0.3, 0, 15.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.1, 
    8.9, 5.1, 0, 8.4, 7.6, 3, 0, 0, 7.1, 0.5, 20.3, 5.1, 7.6, 15.7, 12.7, 
    10.2, 5.1, 5.1, 6.4, 12.7, 3.3, 7.4, 9.1, 8.9, 0, 8.9, 8.4, 0, 5.3, 7.4,
    0.5, 2.5, 3.8, 0, 0, 0, 2.5, 0, 20.3, 7.9, 2.5, 10.2, 4.8, 20.3, 10.2, 
    2.5, 5.8, 2.3, 0.3, 7.6, 6.9, 11.9, 4.8, 3.8, 0, 3.8, 11.9, 17.8, 6.6, 
    0, 0, 0, 6.3, 4.3, 6.3, 0, 0, 0, 0, 0, 0, 0, 6.9, 7.1, 8.4, 0, 0, 0, 0, 
    0, 0, 0, 3.8, 0, 19.8, 11.9, 3.8, 7.9, 0, 22.9, 0, 0, 0, 0.8, 0, 0, 0, 
    6.1, 0.3, 7.6, 13.2, 16.5, 6.3, 7.4, 5.3, 1, 13, 19, 2, 2.8, 0, 0, 0, 0,
    16.5, 6.6, 0.3, 0, 0, 0, 0, 0, 2.5, 9.7, 1.3, 2.5, 9.7, 2.8, 5.1, 0, 0, 
    0, 0, 0, 4.3, 2.3, 11.4, 2.5, 5.1, 2.3, 11.2, 15.2, 1.5, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 8.9, 3.8, 4.6, 2.5, 4.1, 3.3, 1.5, 1.8, 0.5, 0, 0, 0, 0, 
    1.3, 2.8, 8.6, 0, 0, 14.2, 0, 0.8, 0, 0, 0, 1.3, 2.5, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 1.5, 0, 0, 0, 0.5, 0, 0, 0, 0.5, 2.5, 0, 0, 0, 1.3, 0,
    0, 2.8, 4.3, 0.3, 0, 0, 6.9, 0.8, 0.3, 0, 0, 0, 3, 2.3, 0, 0, 6.9, 0, 0,
    0, 0, 0, 0, 3.6, 0, 2, 2, 0, 0, 0, 4.6, 10.9, 0, 5.1, 4.8, 0, 0, 0, 0, 
    0, 0, 0, 0, 3.3, 0, 0, 0, 5.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.8, 
    1.3, 3, 15.5, 3.6, 0.3, 0, 6.3, 0, 19.8, 3.6, 20.3, 0, 6.9, 10.4, 0, 
    4.3, 1.3, 4.1, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 9.7, 4.6,
    2.5, 8.1, 0, 1.3, 0, 4.1, 0, 0, 0, 0, 8.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    4.6, 5.1, 3.3, 3.3, 0, 0, 0, 0, 0, 0, 0, 11.4, 6.3, 1.3, 0, 0, 1.8, 2.5,
    0, 0, 0, 0, 0.5, 0, 0, 0, 0, 12.2, 14.7, 0, 10.2, 0, 0, 0, 0, 13.2, 
    11.9, 9.9, 3.6, 0, 8.9, 0, 1.8, 5.1, 9.7, 2.3, 0, 7.6, 0, 16.5, 25.4, 
    2.5, 3.6, 3.8, 2.5, 7.6, 4.1, 0, 2.5, 21.3, 14, 0, 7.9, 8.4, 19.8, 0, 
    0.8, 0, 4.6, 1.8, 2.5, 3.8, 16.3, 10.4, 3.6, 16.3, 3, 13, 3, 21.3, 4.3, 
    26.9, 4.1, 3.3, 1.8, 4.1, 8.9, 6.6, 2, 1.3, 4.6, 2.3, 12.7, 5.6, 0.3, 2,
    1.5, 3.8, 5.6, 0, 1.8, 1.3, 2, 3.6, 2.3, 2.5, 10.2, 3, 2.8, 3.8, 5.8, 
    0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 9.1, 4.8, 0, 0, 0, 2.5, 0, 3.8, 4.1, 1.3, 0.5, 0, 1.3, 0, 5.3, 0, 
    19.1, 33, 9.7, 8.9, 9.4, 4.1, 4.3, 3.8, 1, 1.8, 0.3, 0, 0, 0, 2.3, 2.8, 
    2, 8.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.1, 0, 5.1, 1.3, 6.4,
    0, 0, 0, 0, 5.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.8, 3.8, 5.6, 12.7, 
    12.2, 7.9, 0.3, 0.8, 0.3, 0, 8.9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1.8, 8.6, 2.8, 2.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0.5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0.5, 2.3, 0, 0, 0.3, 0, 0, 1.3, 5.6, 1.5, 1.8, 3.8, 0, 21.1,
    0, 0, 1.3, 11.4, 1, 1.3, 4.1, 20.3, 0, 1.3, 0, 0, 6.9, 7.1, 10.2, 3.8, 
    0, 0, 0, 0, 7.4, 0, 0, 0, 6.1, 0, 0, 0.5, 0, 2.8, 0.8, 1.5, 0, 2.8, 
    10.9, 0, 0, 0, 0, 0, 0, 0, 3, 1.8, 10.9, 2.5, 2.5, 1.3, 0, 0, 8.9, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.8, 2.5, 4.1, 0, 0, 0, 0, 0, 0, 6.4, 
    0, 0, 2, 0, 0, 0, 0, 13, 0.5, 0.8, 1.3, 3.8, 10.2, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 9.4, 5.6, 1.5, 4.3, 0.5, 4.6, 0, 0, 0, 0, 0, 28.4, 7.6, 4.3, 7.4, 
    0, 0, 0, 0, 4.8, 0, 0, 0, 0, 3.3, 0, 0, 1.3, 17.8, 2, 0, 0, 0, 0, 0, 
    1.3, 1.5, 0, 2.3, 0, 0, 0, 11.2, 0, 0, 1.3, 1.3, 3.8, 2.5, 7.4, 7.9, 
    0.5, 7.6, 6.1, 13, 3.3, 4.6, 2.5, 6.6, 0, 0, 2.3, 4.6, 6.4, 2.8, 4.6, 
    3.3, 0, 5.8, 0, 0, 0, 16.8, 3.3, 21.8, 0, 0, 24.6, 2.8, 14.7, 0.8, 0, 0,
    0, 1.3, 0, 12.2, 1.5, 13.7, 7.1, 14.5, 7.9, 11.7, 12.4, 1.5, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 0, 8.9, 3.3, 2.8, 6.1, 7.6, 4.1, 9.7, 0, 7.6, 5.1, 
    19.1, 20.8, 13.5, 4.8, 7.9, 6.6, 7.6, 7.1, 16.5, 2, 0, 0, 0, 0, 0, 10.9,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 11.2, 5.6, 0, 2.3, 2.8, 2.5, 2, 11.4, 0, 8.1,
    9.4, 1.5, 3.8, 1, 2.8, 0, 0, 0, 0, 13, 21.3, 8.6, 13.2, 9.1, 0, 0, 0, 0,
    0, 0, 4.6, 3.8, 4.1, 2.5, 1.3, 9.4, 3.8, 0, 0, 4.8, 14.7, 2.5, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 14.2, 0, 2.8, 0, 0, 0, 1.8, 4.1, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 3.6, 0, 0, 0, 0, 0, 0, 0, 
    0, 4.8, 0, 7.9, 7.9, 0, 0, 7.4, 2.3, 1.3, 19.6, 1.5, 7.6, 2, 21.6, 3.8, 
    13, 12.4, 1, 0, 0, 1.8, 2.5, 3.8, 2.5, 0, 2.8, 0, 0.8, 1.5, 3.3, 0, 0, 
    0, 0, 8.9, 0, 0, 2.5, 0, 3.6, 0, 1.3, 4.6, 2.3, 3.8, 6.4, 11.2, 11.9, 
    0.3, 16.5, 1.3, 11.2, 0, 0, 0, 4.8, 0, 0, 0, 0, 1.3, 6.6, 0, 2.5, 0, 
    1.8, 3.8, 1.8, 0, 11.7, 5.8, 8.6, 0, 3.3, 0, 0, 0, 0, 0, 0, 0, 0, 30.5, 
    3.3, 2.3, 0, 1.5, 0, 0, 1.3, 0, 0, 0, 0, 0, 16, 15.2, 0, 0, 0, 6.1, 5.8,
    0, 0, 16.3, 5.8, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 10.4, 2.5, 16.5, 2.3, 3.6, 0, 2.8, 16.5, 22.1, 10.2, 2.8,
    5.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8.6,
    0, 0, 0, 0, 0, 4.3, 2, 0, 0, 0, 0, 0, 0, 4.6, 5.3, 8.9, 4.3, 32.5, 1.3, 
    30, 1.3, 2.5, 5.1, 8.9, 2.5, 9.4, 9.7, 23.9, 6.1, 1.5, 4.1, 5.1, 0.8, 
    1.3, 22.9, 11.4, 2.8, 2.5, 7.1, 6.4, 0.3, 0, 0.8, 1.3, 1.3, 0, 0.5, 0, 
    0, 2, 2.8, 0, 0, 0.8, 0, 1, 1.3, 1.8, 5.3, 6.9, 3.6, 2.5, 4.3, 0, 0, 0, 
    48.5, 35.3, 1.8, 40.6, 15.7, 3.8, 6.1, 10.7, 6.4, 8.1, 20.8, 13, 5.1, 0,
    3.3, 7.9, 19.6, 16, 7.1, 16.5, 2.8, 5.1, 5.1, 11.4, 8.9, 1, 0, 1.5, 3.3,
    2.3, 0, 0, 0, 0, 13.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 5.1, 13.2,
    3, 0, 0, 0, 2.3, 0, 0, 5.6, 2, 0, 0, 2, 0, 18.3, 1.5, 0, 12.7, 11.9, 
    10.7, 0, 1.5, 2.5, 0, 0, 0, 3.8, 0, 0, 10.2, 5.1, 0, 7.4, 0, 0, 0, 2.8, 
    0, 0, 3.6, 0, 0, 0, 2.5, 7.6, 6.1, 13.2, 5.1, 6.9, 0, 6.6, 0, 0, 0, 4.1,
    0, 0, 2.8, 0, 5.1, 0, 1, 0, 5.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 2, 0, 
    0, 0, 0, 0, 14.7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 6.4, 0, 0, 0, 0, 0, 0, 6.4, 0, 0, 1.5, 0, 5.1, 0, 0, 0, 0.3, 0, 
    0.5, 2.5, 1.8, 0, 0, 1.8, 0, 19.1, 0, 0, 0, 6.1, 0, 0, 0, 0, 0, 1, 0.8, 
    1.3, 0.3, 0, 0, 0.8, 5.3, 0, 3, 0.8, 0, 0, 17.8, 0.8, 1.8, 0, 8.1, 1, 
    26.4, 2, 3.6, 4.1, 0, 9.4, 0.3, 0, 0, 1, 0, 0, 0, 0, 0, 1.5, 12.7, 3.8, 
    0.8, 27.2, 2.3, 5.8, 6.6, 0.3, 10.9, 9.9, 0.3, 18.5, 1, 3.8, 0, 0.8, 0, 
    6.4, 8.1, 3.8, 0, 0.5, 0.8, 17.3, 2.5, 0.5, 7.6, 29.2, 12.4, 6.4, 2.5, 
    0, 0, 2.5, 7.6, 2.5, 7.6, 12.7, 7.6, 3.3, 4.3, 8.9, 30.5, 5.8, 0.3, 5.8,
    6.4, 0, 0, 0, 4.1, 3.8, 11.4, 6.6, 0.3, 7.6, 5.6, 0, 0, 0, 24.1, 2.5, 
    12.7, 6.4, 8.9, 2.5, 3.8, 5.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 26.7, 13.2,
    11.4, 0, 0, 6.4, 11.4, 2.5, 0, 2.3, 2.5, 4.1, 0, 0, 0, 0.5, 1.3, 2.3, 0,
    1.8, 2.3, 2.5, 6.6, 11.4, 12.7, 20.8, 5.1, 0, 4.3, 5.1, 0, 0, 7.6, 7.1, 
    1.3, 0, 1.5, 2.8, 15.7, 9.4, 3.3, 11.4, 15.7, 6.1, 2.5, 0.3, 4.1, 7.6, 
    13.2, 5.1, 2.5, 12.7, 13, 0.3, 13.2, 0, 0, 0, 0, 2.5, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 9.1, 0, 0, 11.4, 8.9, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.8, 
    0, 0, 0.3, 0.5, 2.5, 0, 0, 6.9, 5.1, 4.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 2.5, 24.1, 11.4, 3.6, 0, 3, 0, 0.8, 6.6, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 4.3, 0, 11.4, 6.4, 0, 34.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    11.9, 5.3, 8.9, 24.4, 0, 8.9, 0, 0, 3, 0, 0.8, 3.8, 0.5, 0, 3.8, 3, 
    19.3, 8.9, 4.8, 0.3, 0, 0, 0, 0, 4.6, 0.8, 2.5, 10.2, 2.5, 10.2, 10.2, 
    0, 0, 0, 0, 5.1, 2.5, 4.8, 0, 0, 0, 5.1, 0.8, 0, 3.8, 0, 5.1, 7.6, 29.2,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 12.7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.6, 5.6,
    5.1, 13.5, 0, 0, 4.1, 0, 0, 0, 0, 0, 0, 0, 0, 19.1, 0, 0, 29.2, 0.3, 0, 
    0, 0, 5.6, 0, 17.3, 13.2, 1.5, 0, 0, 6.4, 0, 11.4, 4.1, 0, 4.1, 0, 0, 0,
    0, 0, 12.7, 0, 4.6, 5.1, 11.7, 7.1, 16.3, 3, 2.5, 13.2, 12.7, 5.1, 0, 
    2.5, 0, 0, 6.4, 8.1, 3.8, 4.6, 0, 0, 0, 0, 0, 2.5, 7.6, 5.1, 6.6, 5.1, 
    2.5, 5.1, 22.9, 0, 2.3, 0.3, 2.5, 0, 6.6, 12.7, 0.8, 8.1, 6.6, 16, 9.1, 
    0.5, 0, 0, 8.9, 3.3, 1, 0.5, 1.5, 0, 6.6, 7.9, 32.8, 2.3, 0, 2.5, 0, 0, 
    0, 0, 0, 0, 0, 6.9, 3.8, 8.1, 5.1, 0, 0, 0, 4.8, 8.4, 13.5, 7.6, 22.6, 
    19.6, 17.5, 27.4, 0, 0, 0, 0, 0, 0, 0, 3.8, 0.5, 7.4, 0, 0, 0.5, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 5.1, 11.4, 10.2, 2.5, 13.7, 27.9, 8.9, 8.9, 0.5, 
    0.8, 14, 2.5, 0, 0, 0, 0, 0, 0, 0, 9.7, 11.4, 13.2, 0, 0.3, 0.5, 0, 0, 
    0, 14, 10.2, 8.9, 14, 21.6, 9.4, 3.3, 24.1, 7.6, 4.6, 25.1, 8.9, 2, 3, 
    1.8, 0, 0, 6.6, 0, 0, 0, 0, 0.8, 6.9, 1.5, 1, 0, 0, 0, 0, 0, 6.4, 0.5, 
    7.6, 22.9, 3.8, 6.1, 8.6, 0.5, 0, 6.4, 0, 0, 1.3, 0, 11.4, 2.5, 14, 2.3,
    1, 6.1, 30.5, 17, 1, 5.1, 5.3, 7.6, 0.5, 1.8, 0.5, 0.8, 10.2, 18.5, 7.6,
    10.4, 19.1, 8.9, 0, 0.8, 0, 0, 0, 0, 0, 0, 0.5, 0, 1.3, 3.6, 0, 0, 7.6, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 4.3, 2.5, 5.1, 0, 0, 0, 0, 0, 0, 3.8, 0.8, 0,
    20.3, 11.9, 3.6, 0, 0.5, 6.4, 3, 0, 17.5, 2.8, 11.9, 1.5, 0, 0, 8.4, 0, 
    0, 0, 4.6, 2.3, 0, 0.8, 1.3, 0.5, 3.3, 2.5, 3, 0, 0, 6.9, 9.9, 11.7, 
    20.3, 2.5, 1.3, 2.8, 0, 0, 0, 0, 0, 0, 5.6, 3.6, 2.5, 0, 0, 0, 11.4, 
    2.5, 7.6, 4.1, 19.1, 4.6, 4.8, 5.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6.4, 0, 
    0, 5.3, 0, 0, 9.4, 2.3, 0.5, 0, 0, 0, 0, 0, 0, 3, 10.9, 0, 0, 15.2, 
    12.2, 3.3, 0, 6.1, 24.1, 12.2, 5.1, 0.3, 15.2, 2.5, 0.8, 0, 0, 0, 6.9, 
    0.5, 2.5, 17, 0, 19.8, 7.6, 19.1, 0, 0, 0, 0, 0, 0, 0, 2.5, 12.7, 3.8, 
    0, 0, 0, 0, 8.9, 0, 0, 0, 0, 5.1, 0, 0, 0, 9.1, 0.3, 0, 2.3, 2.5, 0, 0, 
    0, 0, 0, 5.1, 0, 0.5, 4.6, 45.7, 9.4, 4.3, 25.4, 1.5, 0, 9.7, 0, 0, 0.8,
    0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8.4, 0, 0.8, 8.9, 
    18, 5.6, 31.8, 7.1, 2.5, 14, 12.7, 2.5, 2.5, 10.2, 13, 0.5, 33.5, 12.7, 
    0.5, 8.9, 5.3, 20.6, 9.4, 3.6, 4.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    9.9, 7.6, 6.9, 2.5, 0, 0, 0, 11.9, 5.8, 2.5, 0, 0, 0, 0, 0, 0, 9.9, 13, 
    2, 0.5, 0, 0.5, 0, 6.4, 2.5, 0, 0, 0, 17.3, 2.5, 4.3, 7.4, 3.8, 2.3, 
    5.1, 7.9, 6.4, 16, 18.8, 14, 2.3, 33.5, 15, 0.8, 6.6, 1.8, 6.6, 0, 1.8, 
    0, 0, 0, 0, 0.5, 1, 12.7, 0, 11.7, 8.1, 0, 7.6, 6.4, 10.7, 10.2, 5.3, 
    1.8, 8.1, 2.5, 4.3, 6.4, 5.1, 19.8, 5.1, 0, 11.4, 11.9, 13.7, 0, 0, 0, 
    0, 0, 0, 0, 3.8, 7.6, 2.5, 6.4, 15.2, 2.5, 2.8, 2.3, 0, 0, 0.3, 0.8, 0, 
    0, 0, 0, 0, 0, 5.3, 8.6, 3.8, 0.3, 0.5, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 2.5, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 9.4, 0, 
    0, 0, 0, 0, 0, 13.2, 10.2, 4.8, 7.1, 11.7, 1.8, 0, 0, 3, 0, 0, 0, 0, 
    1.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 14, 5.6, 0, 5.8, 2.3, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 2.5, 1, 1.5, 4.3, 6.9, 8.1, 0, 19.1, 2.5, 0, 0, 0, 
    0, 7.1, 0, 16.5, 6.4, 18.3, 2.5, 9.4, 0, 0, 0.5, 8.9, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 12.7, 4.1, 0, 2.8, 0, 0.5, 0, 0, 0, 0, 
    0, 0, 1.3, 2.8, 18, 0, 0.3, 0, 0, 11.4, 0, 2.5, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 1.3, 3, 5.8, 11.4, 0, 0.5, 0, 0.5, 0, 0, 0, 6.4, 0, 2.3, 8.9, 0, 
    2.5, 1, 0, 0, 0, 0, 0, 0, 3, 0, 5.3, 0, 0, 0, 0.5, 0, 4.3, 0, 0, 3.3, 0,
    0.5, 0, 4.3, 0, 0, 0, 5.6, 0, 0, 0, 0, 0, 0, 5.8, 8.1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 7.1, 0.5, 0.3, 0, 7.6, 2.5, 11.7, 2.3, 4.1, 0, 0, 0, 0, 
    0, 0, 0, 18.3, 8.9, 0, 6.4, 7.1, 0, 0, 0, 31.8, 0, 0, 16.3, 0, 7.9, 5.1,
    34.8, 35.3, 22.9, 0, 8.6, 8.9, 1, 8.9, 10.2, 13.2, 6.4, 8.1, 3.8, 24.9, 
    6.4, 0, 0, 5.3, 2.5, 2.8, 11.9, 4.6, 0.5, 0.3, 8.1, 3.3, 3, 3.8, 0.5, 
    0.3, 0, 0, 0, 0, 0, 0, 3.8, 0, 0, 0.8, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 5.6, 6.4, 3, 2.3, 4.1, 3.3, 7.6, 0, 5.6, 2, 5.3, 19.1, 
    13, 10.2, 5.8, 7.4, 0, 0, 9.4, 3.6, 5.1, 0, 20.8, 1.3, 15.7, 1.3, 37.8, 
    4.3, 6.4, 8.4, 8.6, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    11.4, 2.8, 9.9, 8.9, 11.4, 4.1, 7.9, 25.9, 5.1, 14, 4.3, 0.8, 7.6, 6.6, 
    3.3, 5.6, 7.6, 2.5, 1.8, 4.1, 0, 0, 0, 0, 0, 0.3, 0.5, 0.3, 0, 1, 1.3, 
    17.3, 12.7, 12.2, 10.2, 3.8, 2.8, 7.9, 4.6, 3, 20.3, 0, 6.9, 1, 3.8, 
    11.4, 15.2, 3.3, 10.2, 1, 0, 0, 0, 0, 0, 1.3, 0, 0, 0, 0, 0, 0, 2, 0.3, 
    0.8, 1.5, 0, 0, 2.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.3, 0, 0.8, 0, 0, 
    1.8, 1.3, 10.4, 0, 0, 0, 3.3, 0, 0, 0, 0, 0, 0, 3, 2.5, 3.3, 0, 0, 0, 0,
    0, 0, 3, 0, 2.5, 0, 0, 0, 0, 0, 0, 0, 0, 25.4, 2.8, 8.9, 0, 0.5, 2.5, 
    2.8, 6.4, 3.3, 5.6, 2.8, 0, 3.8, 3.3, 3.3, 5.1, 0, 12.7, 28.4, 4.1, 6.9,
    0, 0, 0, 0, 2.5, 0, 0, 76.7, 6.1, 0, 0, 0, 0.3, 4.3, 0.8, 1, 0.5, 5.1, 
    5.8, 10.2, 25.9, 1, 4.8, 2.5, 0, 0, 0, 0, 0, 0, 3, 8.9, 6.6, 3.8, 0.5, 
    2.5, 3.3, 6.9, 0.5, 8.1, 0.3, 17.3, 1, 5.3, 9.9, 5.1, 0, 0.5, 2.8, 0.3, 
    0, 0, 0, 0, 3.8, 0, 0, 0, 0, 0, 0, 20.8, 0.3, 4.8, 11.2, 12.7, 2.3, 2, 
    5.1, 11.7, 15.5, 3.8, 4.3, 5.1, 0, 0, 15.2, 15.7, 12.7, 6.1, 6.6, 6.9, 
    0.8, 7.9, 8.6, 2.5, 5.1, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 6.9, 3.8, 0.3, 16.5, 16, 0, 4.6, 3, 1.5, 11.4, 0.5, 7.6, 7.1, 0.5, 
    6.4, 0, 0.5, 4.6, 6.4, 8.9, 5.3, 0, 4.1, 4.6, 5.1, 0, 0, 3, 0.5, 0, 
    15.7, 11.4, 9.9, 7.9, 6.4, 2.3, 6.6, 0, 0, 0, 2.3, 5.1, 0, 0, 0, 0, 0, 
    0, 13, 20.3, 0, 3.6, 0, 0, 0, 0, 3, 6.6, 0, 0, 0, 0, 0, 13.2, 32.3, 
    11.4, 8.1, 14.7, 25.4, 0, 0, 0, 0, 0, 0, 26.9, 3, 0, 3, 2, 2.8, 0, 0.8, 
    1.5, 5.6, 0, 16.8, 0, 5.6, 6.4, 0, 0, 6.9, 0, 8.1, 27.2, 0, 14.5, 8.1, 
    0.8, 8.6, 1.8, 34, 1.3, 6.1, 6.4, 3.6, 8.6, 2.3, 8.6, 3, 2.3, 1.8, 3.6, 
    1, 7.9, 0.3, 27.9, 1, 3.6, 1.5, 4.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    3.3, 11.2, 0, 2.8, 3.8, 1.3, 0.5, 3.6, 0.8, 0, 0, 0, 0, 0, 0, 0, 1.3, 
    6.9, 1, 6.1, 25.9, 20.8, 15.5, 8.6, 6.6, 0.3, 6.1, 3.6, 2, 7.9, 9.9, 
    6.1, 1.3, 0, 4.1, 6.4, 1.8, 2.3, 0, 0.5, 8.1, 6.1, 4.6, 2.8, 3, 1, 1.8, 
    1.8, 0.3, 0, 1.3, 0, 0, 0, 0, 0, 0, 0, 1.3, 1.8, 0, 0, 0.3, 0, 0, 0, 0, 
    0, 0, 3.3, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 6.6, 1, 1.3, 3.8, 0, 1.3, 19.1, 
    0, 0, 0.5, 0.3, 0, 0, 0.5, 4.6, 0, 0, 0, 8.9, 0, 4.3, 2.5, 9.1, 0.5, 
    1.8, 0, 0, 0, 7.6, 0.5, 0, 0, 5.3, 2.5, 0, 2.5, 0.5, 0, 0, 1.5, 4.3, 
    0.8, 3, 0.3, 0, 0, 0, 4.3, 0, 0, 0.8, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 1.5, 0.8, 2.5, 11.4, 5.1, 3.6, 16.5, 11.4, 0, 0, 0, 0,
    0, 0, 3.8, 0, 0, 3.3, 13.5, 3.8, 3.6, 4.1, 0, 0, 0, 0, 3, 12.2, 4.3, 
    0.5, 0, 3.8, 0.3, 11.7, 4.6, 4.3, 0.8, 0, 0, 0, 0, 0, 0, 4.1, 0, 10.2, 
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2.8, 0, 0, 12.7, 17.3, 
    0, 0, 0, 0, 0, 17.5, 0, 0, 7.1, 7.6, 9.1, 10.4, 6.4, 2, 0, 8.1, 3.8, 
    7.9, 0, 8.6, 19.6, 2.8, 1, 18.8, 4.8, 3.8, 1.8, 21.3, 6.1, 0.3, 3.6, 
    3.8, 1.8, 2.5, 1, 0, 0, 1.5, 1.5, 0, 0, 0, 8.9, 3.8, 2.8, 0.3, 15, 9.1, 
    12.4, 3.8, 4.8, 9.4, 10.2, 10.9, 8.6, 13, 6.9, 7.1, 5.1, 3, 1.8, 3.3, 
    0.3, 1.3, 0.3, 0, 0.8, 0.5, 3, 1, 1.3, 5.8, 33.8, 0.3, 0, 0, 0, 7.6, 13,
    0.5, 1.8, 3.6, 0, 0.3, 0.8, 2, 4.3, 9.9, 2, 28.2, 4.6, 6.6, 6.1, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 3.6, 0.3, 0, 0, 0, 0.5, 1.8, 0, 1.3, 1.3, 0, 0, 0, 
    11.9, 0, 0, 0, 0, 0, 18, 0, 6.1, 0, 36.6, 2, 0, 0, 3.3, 2.8, 0, 3.3, 
    4.3, 1.5, 0, 0, 0, 0, 0.8, 1.3, 0, 0, 0, 3.3, 0, 0, 0.5, 0, 1.5, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2.3, 8.4, 0,
    0, 2, 1.8, 0, 0, 0, 0, 0, 0, 1.3, 0, 0, 5.1, 1.3, 0, 0, 0, 0, 0, 0.3, 
    0.3, 0, 4.3, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 2.3, 0, 7.6, 0.5, 0.8, 0, 
    0.5, 0, 1.5, 18, 28.7, 0.8, 2.8, 1, 0, 1.3, 7.1, 0, 2.5, 22.6, 0, 0, 0, 
    0, 0, 0, 0, 0.3, 5.8, 0, 0, 0, 3.6, 0, 5.8, 0, 0, 2.3, 0.8, 0, 0, 10.9, 
    0.5, 0, 4.1, 0, 0, 0, 18, 7.1, 11.7, 6.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 5.8, 7.4, 0, 0, 3.8, 0.5, 11.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 4.3, 2, 0.3, 0, 0, 0, 0, 30.5, 1.3, 1.8, 9.9, 0.3, 0, 
    45.7, 11.7, 9.1, 10.9, 0, 4.3, 0, 0, 2, 0, 0.5, 0, 3.8, 6.1, 5.1, 2.3, 
    0, 2.5, 2.5, 0, 0.3, 4.6, 0, 0, 0, 0, 0, 0, 3.6, 0, 0, 0.3, 0, 0, 0, 0, 
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0.5, 1.3, 0, 0.3, 0, 0, 0, 0, 0, 0, 0, 
    4.6, 8.6, 13.7, 7.1, 0.5, 13.2, 86.59999999999999, 2.5, 21.3, 3, 0, 0, 
    4.6, 0.3, 0, 0, 0, 0, 1.8, 12.2, 0, 12.7, 0, 1.8, 3.8, 21.1, 2, 0, 3.6, 
    7.9, 5.6, 0, 0, 0, 2.3, 0, 0, 24.4, 2.3, 14, 0, 8.9, 2.5, 29.5, 4.8, 
    0.5, 0, 12.7, 1.8, 0, 35.6, 47.8, 8.1, 12.4, 20.1, 9.1, 47.5, 7.4, 2, 
    28.2, 17.5, 19.1, 2.3, 13.5, 10.9, 4.8, 34.3, 8.1, 22.9, 20.1, 13.2, 
    10.9, 11.4, 1, 2.3, 2.5, 6.4, 0, 0, 0, 0, 4.6, 26.7, 0.5, 9.1, 17.5, 
    27.7, 3.6, 5.3, 3.3, 23.1, 8.6, 10.9, 1, 4.3, 0.3, 4.6, 21.1, 5.3, 0.8, 
    10.9, 0.3, 9.1, 16.3, 9.4, 7.6, 2, 18.8, 0.8, 0, 0, 0, 0, 0, 0, 23.9, 1,
    11.2, 1.3, 11.4, 0, 2.3, 4.6, 2, 30, 6.4, 0, 11.9, 3.8, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 1, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9.7, 3.3, 0, 0, 0, 0, 0, 
    1.8, 0, 9.4, 0, 0, 16.5, 5.8, 4.6, 1, 2.3, 7.9, 1.3, 0, 2.3, 2.5, 0.5, 
    0.5, 1, 4.8, 6.9, 0, 0, 0.5, 2, 2.8, 1, 13.7, 0.3, 20.6, 1.8, 10.4, 5.3,
    0, 0, 0, 6.1, 16, 0.8, 2.8, 1, 8.6, 0.5, 1.8, 0, 0.5, 0, 3.3, 1, 0, 1, 
    1, 0.8, 2.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6.4, 0, 0.5, 1.3, 5.3, 5.1, 0, 
    13.2, 7.1, 0.5, 0, 0, 4.1, 4.8, 1.5, 0, 0, 2.3, 0, 0, 5.1, 3, 0, 1, 1.5,
    2.5, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.3, 1.8, 0, 0, 2.3, 0, 0, 0, 0, 2,
    2, 2.8, 0, 7.4, 1, 0.5, 0, 1.8, 0, 0, 1.8, 0.3, 11.2, 3.3, 0, 1.8, 0.3, 
    1.8, 0.3, 0, 0, 0, 0, 2.8, 2.3, 13.2, 1.3, 3, 2, 4.8, 10.9, 0.3, 0, 0, 
    0, 3, 1.8, 6.6, 1.8, 6.9, 0, 0.3, 15.7, 2, 16.8, 2.8, 17.8, 11.7, 0.3, 
    0.3, 0, 1.8, 0.3, 1.3, 3.6, 0, 1.8, 0, 4.1, 2.8, 1.8, 48.5, 1, 13.7, 
    0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 1.5, 0.3, 4.6, 0, 8.4, 1, 15.5, 
    0, 1.5, 14.5, 0.3, 3.3, 4.6, 25.4, 26.7, 17.5, 15.5, 1.5, 2.3, 2.5, 2.5,
    4.1, 0.3, 0, 1.5, 0, 0, 0, 0, 4.8, 8.6, 6.9, 5.8, 9.4, 2.5, 0.5, 0.3, 
    1.8, 0, 1, 11.9, 24.1, 1.8, 2.5, 5.3, 8.4, 4.6, 5.6, 21.6, 1.3, 21.6, 
    1.8, 2.5, 3.8, 2.5, 1, 0.5, 12.2, 9.4, 12.2, 0, 3.3, 12.2, 0.8, 11.4, 
    1.3, 1, 0, 0, 0, 0, 0.3, 6.1, 0, 0, 7.6, 18.3, 18.5, 17.5, 3, 24.1, 4.1,
    4.8, 8.6, 0, 12.2, 0, 0, 0, 2.5, 0, 0, 0, 1, 3.8, 7.1, 10.4, 5.1, 22.6, 
    5.1, 3.8, 3.8, 20.8, 4.1, 2, 7.6, 1.5, 0, 0, 0, 1.3, 1.3, 2.5, 1.8, 3.6,
    13, 7.6, 10.7, 1.5, 0, 3.3, 9.1, 1, 0, 0, 0, 0, 0, 1.8, 10.7, 1.3, 0, 
    2.3, 0.5, 1.3, 0.5, 1.3, 2.5, 0.8, 10.7, 17.5, 7.4, 5.3, 2.5, 0.3, 4.1, 
    1.8, 1.3, 2, 23.4, 0.8, 0.8, 0, 0, 16, 1.5, 2.3, 3.6, 6.1, 2.5, 10.9, 
    5.8, 4.3, 3.8, 3, 5.6, 1, 0, 3.6, 4.1, 2.8, 1.8, 1, 2.5, 3, 4.1, 3.6, 
    5.6, 0, 0, 16, 0.5, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 5.1, 8.6, 
    0.5, 1, 0, 0, 0, 0, 0, 5.8, 0.5, 0, 0, 6.1, 12.2, 0, 0.5, 8.6, 1.8, 0, 
    4.6, 0, 0, 0, 0, 0, 0, 0.3, 2.5, 1, 2.5, 1.3, 0, 0.8, 0, 24.6, 16, 4.6, 
    5.3, 0.5, 0.8, 0, 0, 0, 0, 0, 0, 0.3, 3.6, 6.9, 0, 0, 5.8, 0, 1.3, 3, 
    4.6, 2.3, 2.5, 0, 0, 0, 0, 4.6, 9.4, 13.5, 0.3, 0, 0, 17.3, 0, 3.8, 7.1,
    17.5, 0, 0, 0, 0, 11.2, 4.6, 5.3, 10.7, 1.8, 3.3, 5.8, 0, 0, 5.3, 0, 
    26.9, 0, 4.8, 3.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.6, 0, 4.6, 7.4, 0,
    4.6, 10.2, 0, 1.5, 5.1, 5.6, 20.6, 5.1, 18.3, 2.3, 0.5, 8.6, 5.1, 1.3, 
    0.3, 0, 0.5, 0, 28.4, 2.5, 7.1, 2.8, 0.3, 0.3, 1.8, 0.5, 0, 0, 0, 30.7, 
    9.4, 0.3, 5.6, 15, 1.5, 2.8, 0.5, 0.8, 1.5, 14, 23.6, 3.8, 0, 23.9, 5.1,
    9.4, 0, 0, 0, 3, 0, 0, 0, 0.8, 7.1, 0, 0, 16.3, 11.9, 43.4, 2.5, 5.6, 
    0.3, 0, 0, 0, 0.5, 1, 3, 0, 2.8, 0, 2.3, 1.8, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 3, 0.3, 0.5, 0, 0.5, 0, 15.5, 0, 0, 0, 0, 0, 8.9, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 5.3, 1.3, 0.3, 0, 0, 7.4, 59.4, 0, 0.3,
    1.5, 15, 0.8, 10.9, 11.2, 12.4, 0, 0.5, 23.1, 0, 0, 0.3, 10.2, 9.9, 2.5,
    1.3, 1, 19.1, 0, 3.6, 29, 4.8, 0.5, 4.3, 0, 0, 8.4, 4.3, 0.8, 7.6, 10.7,
    3.6, 1.5, 0.5, 1.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 1, 0, 0,
    8.6, 0.8, 1, 13, 3.8, 0.3, 1, 2.5, 11.4, 15.7, 9.9, 0, 35.1, 17, 1.5, 
    23.9, 3, 6.4, 1.3, 11.4, 1, 0.5, 1, 0.3, 0.5, 0.3, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 1, 0, 0, 0, 0, 0, 0, 0, 4.1, 0, 1, 1, 0.5, 7.9, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 3.8, 0.3, 0, 0, 4.3, 5.8, 11.4, 2.8, 18.8, 9.7, 5.8, 3, 
    4.3, 0, 0, 0, 4.3, 0.3, 0.5, 10.7, 9.4, 3.8, 0.3, 9.4, 1.5, 1.3, 3.6, 
    0.3, 1, 1.5, 0.8, 1, 2.3, 0, 0, 1.8, 3.3, 9.9, 1.5, 7.6, 7.4, 0, 0.8, 0,
    0, 0.3, 2, 0, 7.1, 2.8, 0.8, 9.4, 15.7, 2, 6.4, 1.8, 0, 0.8, 18, 8.6, 
    18.3, 0.3, 1, 0, 0, 0.5, 0.3, 4.3, 3.8, 5.1, 1, 0.5, 0, 0, 7.9, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15.2, 
    53.3, 3, 0, 1.5, 2.5, 3, 6.4, 1, 0.3, 0, 0, 0, 0, 4.8, 0.5, 0, 3.8, 0, 
    0, 1.5, 0.5, 0.5, 2.3, 5.1, 17.5, 7.9, 1.8, 0.8, 3.8, 0.5, 14.7, 0, 3.6,
    0.5, 0, 1.5, 0, 0, 0, 0, 0, 0, 0, 0, 3, 1, 0, 0, 0, 0, 0, 4.6, 25.9, 0, 
    0, 0, 0, 0, 0, 0, 11.9, 1, 0, 1, 13.2, 13.5, 0, 3.8, 11.4, 18.8, 2.8, 0,
    1.8, 0, 1.8, 0.3, 0, 0, 0, 0, 21.6, 2, 0, 0, 11.2, 0, 2.8, 5.6, 0, 0, 0,
    10.7, 19.3, 0.3, 0.5, 3.8, 0, 0, 0.3, 33.5, 7.6, 0.3, 20.3, 2.5, 8.4, 
    23.4, 4.1, 4.3, 1.3, 1.3, 0.3, 9.7, 17.5, 21.3, 10.7, 14.5, 3.6, 10.9, 
    5.1, 9.9, 1, 14.5, 3.3, 0.3, 0.5, 0.5, 0.3, 13.2, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 4.6, 0.8, 9.7, 2, 11.4, 13.7, 0.5, 3.8, 5.3, 1, 0, 
    6.6, 19.1, 0.3, 10.2, 1, 0, 0, 0, 0, 0, 0, 0, 1.3, 0, 8.1, 1.8, 0.8, 0, 
    0.5, 0, 0, 2, 14.5, 3.3, 7.9, 0, 0.3, 0, 4.8, 1.8, 23.1, 0, 18.8, 9.7, 
    19.8, 1.5, 5.1, 1, 0, 1.3, 2.5, 0, 0.3, 2.8, 0, 1.8, 7.9, 10.9, 0.8, 
    5.3, 2.3, 5.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2.3, 7.6, 19.1, 0.5, 2.3, 
    0.5, 0.5, 14, 4.6, 0.3, 1, 4.3, 0, 0, 0, 0, 0, 0.5, 3.3, 0, 0.5, 2.3, 2,
    0, 24.4, 3, 5.6, 7.6, 8.4, 20.6, 9.4, 7.9, 3.8, 0, 19.1, 1.3, 0.8, 0, 0,
    0, 0, 0, 0, 0, 9.1, 23.6, 6.4, 6.1, 12.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0.5, 2.5, 3.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.1, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 5.6, 6.1, 7.6, 2.8, 7.4, 0, 5.8, 4.1, 8.6, 0.5, 4.3, 2.3, 3.8, 
    17.3, 11.4, 3, 1.8, 3.8, 0, 0, 0.3, 4.3, 0, 0, 0, 0, 0, 0, 1.5, 0, 0, 0,
    0.8, 0.8, 1.3, 0, 0, 0, 0, 1.3, 5.8, 14, 1, 0, 0, 0, 0, 0, 0, 0, 0, 5.8,
    3.3, 0, 0, 1.3, 3, 3.6, 9.1, 8.9, 3.6, 1.5, 5.6, 0.3, 1.5, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 2, 4.1, 3, 8.1, 2, 3.3, 5.8, 11.7, 11.2, 30.5, 0.8, 
    0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10.4, 2, 2.5, 0, 0, 0, 0, 0, 0, 0, 0,
    1.5, 0.3, 0, 15, 0, 2.5, 2.5, 0.5, 0, 0.3, 0, 0, 2.5, 5.6, 9.4, 0.3, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6.4, 0, 0, 0, 0, 
    4.1, 0, 8.9, 18.5, 0, 11.7, 5.3, 1.3, 0.5, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0,
    1.5, 0.5, 0, 8.9, 30.2, 1.3, 0, 4.6, 0, 4.3, 0, 0, 0.3, 1.8, 0, 3.8, 0, 
    29, 2, 0, 6.9, 9.9, 10.4, 0.8, 2, 2.8, 3, 2.5, 0, 0, 0, 0, 0, 0, 3, 4.8,
    0, 0, 4.1, 5.1, 2.5, 0, 0, 0.3, 11.9, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    21.8, 5.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 23.1, 1.3, 1.8, 1.3, 0, 3.8, 3, 1.3, 3.8, 0, 3.8, 1.8, 0, 0,
    21.3, 10.4, 2.5, 14.2, 16.3, 9.7, 1, 8.9, 1.3, 0, 0, 0, 2, 0, 0, 11.7, 
    0.5, 0, 0, 0, 0.5, 0, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 4.1, 0, 0, 0, 0, 0, 2, 0, 2.8, 2.8, 7.1, 0, 0, 7.9,
    13, 1.5, 0, 0, 19.1, 4.1, 7.9, 2.8, 8.4, 3, 0, 3.8, 1.8, 0, 1.3, 0, 2.3,
    0, 0, 0, 0, 0, 0, 0, 0, 1.3, 0, 5.1, 0, 0, 0, 3.6, 4.1, 0.8, 16.5, 0.3, 
    0, 0, 0.5, 16.5, 0, 6.1, 0, 0, 6.9, 2, 9.4, 1, 0, 1.3, 0, 0, 0, 0, 0, 
    13, 15, 20.3, 0, 2.8, 0, 0, 0, 0, 0, 0, 3.8, 1.8, 0.3, 0, 1.8, 0.3, 1.8,
    0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 1.8, 1.3, 4.6, 0, 0, 0, 0, 
    0, 0.3, 0, 3.3, 0, 0, 0.3, 1.3, 0, 5.8, 0, 9.4, 2.8, 20.6, 10.2, 6.4, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10.7, 0, 0, 0, 0, 0, 0.5, 0, 3.3,
    0, 0, 0, 1.5, 0, 0.3, 0, 0.5, 1.5, 0.5, 6.6, 21.8, 0.3, 7.6, 2, 40.9, 1,
    1.3, 1.5, 0, 1, 6.6, 12.2, 0, 0, 0, 0, 0, 0, 0.8, 0, 1.8, 1.8, 2, 0, 0, 
    0, 1, 15.5, 2.3, 12.2, 0.5, 0, 23.4, 0, 0, 1, 1.5, 0.3, 1.5, 0, 0, 0, 0,
    0, 0, 1.8, 12.2, 2.8, 0, 4.8, 4.8, 0, 12.2, 6.1, 0, 0.3, 4.6, 1, 10.9, 
    1, 3.6, 0, 0, 5.3, 11.2, 6.4, 3.6, 4.3, 0, 0.5, 0, 0, 0, 1, 1.8, 3.8, 
    1.5, 4.3, 0.5, 0.5, 0, 7.4, 0, 2, 3.8, 18, 1, 2.8, 3.3, 1.8, 6.1, 5.8, 
    12.2, 2, 0, 0, 2.5, 4.8, 1.3, 7.9, 4.1, 3.6, 0, 0, 0, 0, 6.1, 0, 0, 0, 
    0, 17.5, 1, 0.8, 0, 0, 0, 1.5, 3.8, 0.5, 0.3, 1.3, 22.9, 1, 19.1, 8.1, 
    20.1, 13, 24.1, 42.7, 2.5, 6.6, 8.4, 5.1, 4.8, 7.1, 7.9, 8.4, 7.9, 17, 
    7.4, 3.6, 1, 8.4, 4.3, 3.8, 15.7, 8.4, 4.3, 30, 1.5, 6.6, 1.5, 6.1, 0.3,
    0.8, 0, 0, 0, 0, 0, 0, 0, 6.6, 4.8, 2.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1.5, 6.4, 0, 0, 0, 0, 2.8, 0, 3, 2.3, 0.3, 11.2, 6.4, 0, 0, 0, 0, 0, 0, 
    4.3, 3.8, 3, 1.5, 2.3, 0, 3, 1.3, 10.4, 18.5, 1, 3.8, 13, 15.5, 14.5, 
    5.3, 5.6, 15.7, 3, 2, 0, 0.8, 0, 0, 0, 0, 8.9, 0, 0, 0, 0, 0, 1, 0, 0, 
    0, 0, 0.3, 4.1, 3, 0.5, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0.8, 2.8, 1.8, 
    11.4, 11.9, 7.6, 14.5, 5.3, 0.3, 0, 7.1, 3, 15.7, 4.8, 2.8, 13.2, 2.8, 
    8.6, 0.8, 12.4, 1.3, 5.3, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 4.8, 0, 19.8, 3.6, 14.5, 1, 0, 0, 0, 0.8, 1.3, 0, 0, 0, 
    8.6, 0, 0, 5.3, 0.5, 3, 4.6, 11.9, 15.7, 1.3, 0, 0.8, 2.3, 6.9, 8.1, 0, 
    7.1, 7.4, 1, 8.4, 0.3, 0.5, 0.3, 0, 0, 0, 11.4, 11.2, 1.3, 0, 0, 0, 2, 
    1, 0, 0.3, 0, 0, 0, 0, 0.5, 0, 0.8, 0, 0, 0, 0, 0, 0.5, 0, 0, 3.6, 1, 
    6.1, 0, 0, 0, 0, 0, 1.8, 2.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    4.6, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 8.9, 6.1, 0, 7.6, 3.3, 1.5, 0, 16.3, 
    1.5, 12.2, 6.4, 14.5, 1, 2.8, 0, 0, 0, 0, 0, 4.8, 1, 0.5, 16, 3.3, 10.4,
    2.3, 7.9, 2.5, 0, 4.8, 2.5, 6.9, 2, 0, 4.6, 3, 7.6, 8.1, 4.1, 2.8, 9.4, 
    8.6, 21.6, 83.3, 1.8, 7.9, 3.8, 17.8, 1.3, 1, 0.3, 0.5, 0.3, 0.3, 2, 0, 
    2.3, 6.6, 0.8, 0.8, 3, 5.3, 3.3, 0, 2.8, 2.3, 3, 0.5, 0.8, 13.7, 0.5, 
    3.3, 7.4, 4.6, 2.5, 1, 0.8, 8.1, 20.6, 0.5, 15.7, 4.6, 10.7, 54.9, 5.6, 
    20.3, 0.3, 0.5, 4.3, 3.6, 0, 0, 0, 0, 0, 2.5, 2.3, 1.8, 3.6, 20.1, 10.2,
    3.8, 5.3, 8.9, 3.8, 5.6, 3.6, 1.8, 0, 0, 0, 0, 0, 10.7, 3.3, 0, 0, 3, 0,
    0, 0, 0, 4.3, 9.9, 3.8, 15.5, 7.6, 1.3, 15.2, 19.8, 8.9, 1, 0.3, 2.5, 
    1.5, 9.9, 4.3, 6.6, 8.4, 9.4, 1.5, 7.9, 0.5, 0, 0.5, 0, 0.3, 8.6, 2, 
    12.4, 5.3, 1, 7.9, 1, 5.8, 6.4, 13.7, 9.1, 18.3, 2, 12.4, 4.6, 2.5, 0.3,
    0, 1.3, 6.4, 0, 0, 0, 21.6, 13.7, 0.3, 0, 0, 0.3, 1, 3.8, 23.9, 5.1, 
    1.3, 8.1, 0, 3.3, 2.3, 0.8, 0, 2.8, 5.1, 1.3, 3.6, 0.5, 0, 3.6, 3.8, 
    2.5, 1, 4.6, 17, 5.6, 0, 0, 0, 0, 0, 0, 0, 0, 2, 4.3, 0.3, 8.1, 0.3, 
    1.3, 2.8, 7.1, 2, 1.5, 24.6, 0.3, 0, 17.3, 7.9, 0, 13.2, 0, 0, 0, 0, 0, 
    0, 0, 0, 2.5, 0, 0, 0.8, 0.5, 0.3, 0, 3.6, 1.8, 11.9, 0, 5.3, 1.5, 5.3, 
    0, 1.3, 0, 0, 0, 0, 0, 0, 0, 0.8, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 2.3, 4.3,
    1.8, 0, 0, 0, 0, 0, 1.5, 3, 0, 0, 0, 0, 0, 2.8, 3.3, 1.5, 9.9, 0.5, 1.8,
    0, 0.8, 1, 0, 0.5, 0, 9.9, 0.8, 0, 1.3, 0, 0, 0, 0, 1.3, 1.5, 0, 0, 0, 
    2.8, 1.5, 0, 0, 0, 59.2, 4.1, 26.9, 10.4, 1, 0.3, 17.8, 12.2, 1.5, 21.3,
    21.1, 4.8, 3.8, 8.6, 2, 22.9, 2.5, 2.8, 24.1, 11.9, 2.3, 1.3, 0.3, 10.4,
    2.5, 0.5, 2.3, 1.3, 4.1, 0.5, 0, 5.6, 8.1, 1, 0.3, 1.3, 0, 2.5, 0.8, 0, 
    0, 0, 0, 3.8, 0.3, 0.5, 0, 0.3, 0, 0.5, 0, 5.1, 1, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 15.5, 8.9, 10.7, 2.3, 12.4, 1, 0, 6.1, 0.3, 4.3, 2.8, 1, 
    4.6, 1, 0, 0, 0, 0, 13.5, 0.3, 0, 0, 1.8, 1.8, 0.5, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 5.1, 4.6, 0, 0, 1.8, 0.3, 0.8, 0, 0.8, 0.5, 
    4.6, 10.7, 7.4, 2, 7.1, 0.5, 4.1, 1, 0, 10.9, 4.1, 3, 4.6, 0.5, 28.7, 
    16.8, 20.3, 8.4, 3.8, 22.9, 5.1, 4.3, 0.5, 9.4, 3.8, 2, 0, 0, 0, 0, 0, 
    0, 0.3, 0, 0, 1.8, 0, 3, 0.8, 0.5, 0, 5.1, 11.4, 3.8, 0.8, 0.8, 4.8, 0, 
    0, 1.8, 2, 31.8, 20.8, 26.7, 5.8, 23.1, 1.5, 0.8, 0.5, 0.3, 0.8, 0.5, 
    1.3, 0, 0, 0, 7.6, 4.3, 0.3, 16.8, 8.9, 2.3, 0, 4.6, 11.7, 2.5, 0, 0, 
    2.5, 4.8, 6.9, 29.5, 0, 0, 27.4, 15.5, 28.4, 1.8, 5.6, 30, 37.3, 0.3, 
    13, 16, 0.3, 0.3, 2.5, 0.5, 0.5, 11.2, 3.8, 1, 28.2, 19.1, 15.7, 0.3, 
    6.6, 12.2, 4.3, 3, 1, 0, 7.9, 7.4, 3.3, 8.1, 2.8, 3.3, 9.4, 1, 0.3, 
    21.1, 1, 8.4, 15.7, 32.5, 8.6, 9.9, 10.9, 1.3, 0, 0, 0, 0, 5.6, 0.8, 0, 
    3.8, 20.1, 10.7, 3.6, 6.1, 2, 9.1, 12.7, 13.7, 18.5, 5.6, 12.2, 0, 2.5, 
    1.3, 1.8, 18.3, 0, 0, 0, 0, 0, 0, 5.6, 7.1, 2.8, 0.3, 1, 9.4, 7.6, 0.5, 
    6.6, 17, 4.8, 5.1, 2.5, 0, 17, 1.5, 0, 0, 4.8, 5.3, 1.8, 0, 0, 0, 0, 0, 
    0.3, 0, 0, 0, 0, 0, 0, 4.8, 0, 1.8, 0, 11.9, 1.3, 0, 7.6, 15.2, 0, 0, 0,
    0, 0, 0, 3.3, 7.6, 13, 16.3, 6.4, 0.5, 0.3, 1.3, 0, 0, 0, 0, 0, 0, 0, 
    0.8, 0, 0, 0, 7.4, 5.1, 0.5, 2, 2.8, 8.4, 2.5, 0.3, 0, 0.3, 0, 5.3, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 1.3, 0.3, 2.3, 0.5, 0, 4.8, 0, 16.8, 1.5, 0, 
    2.5, 1, 0, 19.3, 7.6, 0, 0.5, 0, 2.3, 0, 0, 1.5, 0, 3.3, 3.6, 0, 1, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 5.8, 1, 0, 2.8, 21.8, 0.5, 
    0, 6.4, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.1, 0, 0, 0, 
    0, 3, 0, 12.7, 0, 0, 17, 7.6, 2, 13.7, 8.6, 7.1, 3.6, 6.1, 0, 4.8, 0.3, 
    0, 2.3, 0, 0, 0, 0, 0, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 6.6, 16.5, 8.6, 14.7, 5.6, 0, 13.7, 2.5, 13.7, 0.5, 6.4, 
    0.3, 0, 0, 0, 0, 0, 1.3, 0, 0, 0, 0, 0.3, 0, 0, 0, 0.5, 2.3, 7.9, 3.3, 
    0, 4.3, 23.9, 0, 0, 0, 0, 0, 0, 8.6, 15.7, 20.3, 0.5, 0, 18.3, 2.8, 0, 
    3, 0, 0, 11.4, 2.8, 20.8, 5.3, 6.4, 4.8, 0, 0, 0, 3, 15.7, 1.8, 1.3, 
    0.5, 0.8, 1.3, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.8, 1.5, 13, 23.6, 12.7,
    11.9, 4.1, 8.1, 2.3, 23.9, 13.7, 8.6, 0, 11.2, 0, 1.8, 0, 1.5, 0.3, 0, 
    5.1, 13.5, 20.1, 5.8, 5.1, 2, 16.3, 6.6, 0.8, 0, 0, 0, 0, 0, 2.5, 4.6, 
    0, 0, 0.8, 0, 0, 0, 0.8, 2.8, 0, 0, 0, 0, 0, 0, 1.3, 19.3, 7.4, 3.8, 
    5.3, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 
    2.5, 0, 6.4, 0, 1.5, 2, 0, 0.5, 1.8, 0.8, 1.3, 0, 3.3, 0, 0, 0.5, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0.5, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10.9, 16.8, 11.7, 6.1, 1, 5.8, 1.8, 0, 
    0, 0, 0, 0, 0, 4.1, 20.6, 5.8, 1.5, 9.1, 0.3, 8.9, 22.9, 0, 1.8, 0.3, 3,
    9.4, 0, 0, 0.5, 0, 0.8, 0, 0, 0, 0, 0, 0, 3.3, 0, 0, 0, 5.3, 1.5, 0, 0, 
    0, 6.4, 5.8, 3, 0.3, 0.5, 0.3, 2.3, 12.7, 3.8, 34, 10.4, 8.1, 0, 8.1, 
    3.6, 0, 0, 11.9, 0, 0, 1.5, 1, 1, 0, 0, 0, 0, 0.5, 4.1, 1.3, 19.8, 1, 
    0.3, 1, 0, 14.2, 0, 67.3, 4.6, 3, 0, 2, 0.8, 2, 3.8, 0, 0.8, 0.3, 0, 
    1.5, 2, 0, 3.8, 0, 1.8, 4.8, 31.2, 0.8, 1.3, 0, 0, 24.1, 10.4, 0, 0, 0, 
    0, 0, 0, 0, 2.3, 2.5, 5.1, 0.3, 0, 0, 0, 0, 0, 2, 0, 0, 4.3, 13.5, 3.8, 
    1.3, 6.1, 1.8, 0.5, 0.8, 0, 0, 0, 2.5, 4.8, 4.1, 19.6, 9.4, 25.4, 9.9, 
    11.4, 4.8, 3.6, 14.2, 1.3, 0.3, 0.5, 14.7, 2.8, 0, 4.8, 0, 4.8, 2.5, 0, 
    0, 0, 1.8, 0, 0, 4.8, 4.3, 3, 3.6, 4.3, 0, 8.6, 1.8, 0, 5.3, 1, 0.3, 
    0.5, 0.8, 0, 0.3, 0, 21.8, 30.2, 1, 0, 6.9, 1.3, 0.5, 14.7, 25.9, 8.6, 
    12.4, 9.4, 13.2, 6.6, 36.1, 1.5, 12.2, 4.1, 6.4, 6.1, 4.3, 1.8, 1, 6.6, 
    1.8, 1.5, 5.3, 2.3, 36.8, 16.5, 17.8, 3.8, 22.6, 0, 18.5, 1, 0.8, 0, 0, 
    0.5, 0, 0, 0, 0, 0, 6.4, 0, 1.8, 2.8, 1.8, 8.6, 6.6, 2.8, 10.9, 2.5, 0, 
    14, 0, 3.8, 2.5, 8.4, 3.6, 3.3, 0.3, 29.5, 17, 3.8, 24.6, 2, 7.1, 15.2, 
    14, 19.8, 0.3, 15.5, 9.1, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8.9, 16,
    0.8, 0, 0.3, 1.5, 0, 0.5, 0.3, 1, 1.8, 3.6, 0, 0, 24.4, 12.7, 0.8, 1, 
    4.8, 4.3, 15.7, 3.8, 4.1, 0, 5.8, 16, 1, 1.8, 0, 2, 3.6, 1, 4.8, 0, 0, 
    0, 0, 0.8, 0, 2.5, 0, 0, 2, 5.8, 3.6, 5.8, 0.3, 0, 0, 0, 0, 0, 0, 4.8, 
    4.6, 17.8, 38.4, 2, 7.4, 0, 0, 0, 0, 0, 5.8, 8.6, 4.3, 1, 3.6, 0, 0, 0, 
    0, 1, 0.8, 9.4, 3.8, 3.3, 1, 0, 0.3, 2.8, 6.1, 0.3, 0, 0, 15, 7.4, 0.5, 
    0, 2.5, 0, 0, 0, 0, 0, 3.8, 1.5, 0.5, 0.3, 0, 0, 2.3, 2.5, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9.9, 0, 0, 2.3, 0, 0, 3, 3.6, 21.8, 
    0, 6.1, 0.5, 0, 3.6, 4.1, 7.1, 0, 8.1, 9.7, 0, 4.3, 2, 2.8, 0.5, 0.3, 
    0.5, 0, 12.2, 11.7, 27.7, 2, 4.1, 0, 0, 0, 0, 18.8, 27.9, 21.1, 7.9, 
    9.9, 10.2, 7.6, 3, 7.6, 8.4, 6.4, 3.3, 1.8, 0.5, 1.5, 3, 9.7, 2.5, 2.8, 
    1.5, 15, 0.5, 2.5, 0.5, 1.5, 2.3, 1, 6.9, 2.5, 0, 0, 0, 0, 0, 0, 0, 0, 
    6.4, 10.9, 2.5, 2.8, 0, 0, 3.6, 7.4, 0.5, 0, 0, 0, 9.4, 0, 8.1, 26.9, 
    0.5, 0, 0, 0, 0, 0, 4.1, 0, 6.4, 0, 0, 0, 0, 0, 0, 0, 0, 1.3, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 9.1, 21.6, 2, 0, 23.1, 0, 2.5, 2.5, 3.6, 6.4,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1.3, 5.3, 3.8, 2.5, 0, 0, 5.3, 3, 10.9, 9.4, 
    6.4, 3.6, 9.7, 31, 3.3, 12.4, 14.2, 4.1, 0, 0.3, 1, 4.8, 8.9, 10.9, 
    10.2, 6.6, 29.2, 0.8, 0, 0, 16.5, 25.4, 0, 15, 16.3, 4.8, 12.4, 2.3, 
    6.1, 3.3, 1.3, 19.8, 9.7, 5.3, 0.5, 15.2, 5.6, 3.6, 13, 1, 0, 0, 0.8, 0,
    0, 0, 1.5, 2.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.8, 
    14.5, 2, 0, 0, 0, 0, 0, 0, 0, 0, 5.1, 0, 20.3, 0, 0, 0, 0, 0, 2.5, 16.5,
    12.7, 15.2, 3.6, 2.5, 0.8, 0.3, 0, 3, 14.2, 10.9, 3, 19.1, 0.8, 0, 0, 0,
    0, 0, 0, 4.1, 2.3, 33.3, 11.9, 13.7, 3, 0, 0, 0, 3, 0, 0, 24.6, 6.9, 3, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6.9, 10.2, 0.8, 0, 18, 15, 12.7, 
    8.9, 0.8, 0, 4.6, 7.1, 0.8, 0, 0, 0, 1, 6.4, 0.8, 2, 2, 2.5, 2, 5.3, 0, 
    0.5, 1.3, 0, 0, 0, 0, 2, 0, 0, 5.8, 1.5, 0, 11.7, 4.3, 5.1, 0, 2.3, 0.8,
    0.5, 0.3, 1.5, 1.5, 2, 25.9, 4.8, 3.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 1.8, 0, 1, 0.5, 7.9, 10.2, 8.4, 8.1, 0, 1, 0, 0,
    0, 0, 4.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 
    0.3, 9.1, 2.8, 0, 0, 3.6, 0, 0, 1.5, 0, 3.6, 5.1, 6.1, 2, 1.3, 17.3, 
    10.2, 12.4, 47, 18.5, 1, 0.8, 3.8, 8.6, 1.5, 10.7, 0.5, 0.3, 0, 6.4, 0, 
    3.3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 2.5, 2.3, 0.3, 1.5, 
    0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    2, 3.6, 0.5, 3, 0, 18, 0.5, 6.6, 6.1, 6.6, 1.5, 8.6, 7.6, 1, 0, 11.9, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 2.8, 6.9, 3, 8.9, 1.8, 10.7, 3, 5.6, 0, 0, 
    3.8, 10.9, 3.3, 0, 0, 0, 2.5, 7.4, 0, 0.3, 1.8, 0, 0, 0, 0, 0, 0, 12.7, 
    29.2, 14.2, 32, 33, 10.7, 12.7, 9.1, 0, 11.4, 24.9, 8.1, 26.7, 5.3, 
    21.6, 2.8, 13.7, 2.8, 2, 1.3, 8.9, 3.8, 14.7, 0, 1.3, 1.3, 0, 0, 0, 0, 
    0, 0, 0, 0, 5.6, 2.3, 10.9, 9.7, 2.3, 13.2, 11.9, 7.1, 0, 11.7, 29.7, 
    1.5, 0, 0, 6.9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.6, 4.8, 10.9, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 5.1, 1.8, 11.4, 7.9, 5.8, 9.1, 
    0.3, 3.8, 10.7, 9.7, 4.3, 9.7, 4.6, 1.8, 4.3, 0.3, 1.8, 14.2, 0, 0, 9.4,
    8.9, 9.1, 4.3, 4.6, 0, 0.8, 7.6, 16.5, 8.4, 4.6, 10.7, 0.5, 1.8, 0, 5.6,
    0, 13, 0, 0, 0, 16.3, 12.7, 8.6, 15.7, 2.8, 3, 0.8, 3.8, 15, 3.8, 0.8, 
    0.3, 3, 2.3, 0, 0, 7.1, 0, 0, 0, 3.8, 0, 0, 1.3, 0, 1.8, 1.3, 7.4, 10.4,
    1, 7.9, 0, 0, 25.4, 1.5, 0, 1, 4.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2.3, 0, 
    0, 7.6, 1.5, 4.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 8.1, 15.2, 1.3, 3.8, 14.7, 
    10.9, 6.4, 10.9, 0.5, 0, 0, 0, 0, 8.4, 18.3, 29.2, 0.5, 0.8, 4.3, 0, 2, 
    3, 0, 0, 4.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 4.1, 4.3, 4.1, 4.1, 5.8, 9.1, 1.8, 0, 0, 0, 
    0, 0, 0, 0, 0, 4.1, 7.6, 8.1, 2, 0, 0, 0, 0, 0, 0, 10.2, 3.6, 12.7, 
    10.9, 2, 3.3, 0, 1, 4.1, 20.8, 22.6, 0, 1.8, 0, 0, 6.4, 38.1, 3.6, 0.5, 
    0, 8.9, 1.5, 1.8, 0, 0, 5.6, 1.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8.4, 0, 3, 0, 0, 0, 0, 0, 3, 0, 0, 
    10.2, 5.8, 9.4, 0, 0, 0, 3.8, 0, 4.1, 6.4, 10.4, 5.6, 3.3, 0, 0, 0, 0, 
    1.5, 0, 2.5, 3.8, 0, 5.6, 0, 0, 0, 0, 0, 0, 4.6, 1.5, 1, 0.8, 9.1, 0.5, 
    0.8, 0, 0, 2.3, 9.9, 2.5, 0, 7.6, 4.3, 4.1, 14.2, 0.8, 5.8, 0, 0.3, 
    11.4, 3, 0, 0, 7.1, 0, 0, 0, 0, 19.1, 0, 2.5, 1.5, 2.8, 4.3, 3.8, 10.7, 
    10.2, 6.4, 3.8, 0, 0.8, 0, 1.3, 1, 3.3, 0, 0.3, 0, 1, 20.6, 0, 0, 0, 0, 
    0, 0, 12.2, 5.6, 0, 0, 2, 3, 1, 0, 0, 27.2, 0, 0, 5.8, 6.6, 9.1, 5.1, 
    10.7, 0, 0, 3.3, 26.4, 5.6, 1.5, 1, 15.2, 6.6, 1, 7.9, 6.6, 2.8, 1.8, 
    2.5, 2.3, 0, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 21.6, 0.5, 0, 0, 0, 0, 5.3, 8.1, 1.5, 0, 1.8, 6.6, 6.4, 
    26.2, 7.1, 5.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 9.7, 2.8, 2.5, 0.3, 
    10.7, 19.1, 3, 11.9, 6.4, 18, 4.3, 0, 0, 0, 4.3, 0, 0, 0, 0, 0, 0, 0, 0,
    1.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11.9, 3, 8.6, 0,
    0, 21.3, 3.8, 30.5, 19.6, 0, 4.3, 0, 2.3, 23.9, 13.5, 11.2, 12.4, 13.7, 
    5.1, 9.7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.1, 5.1, 1, 4.1, 0.3, 
    5.1, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 13.5, 25.9, 
    1.3, 0.5, 4.8, 2, 7.4, 0.5, 1, 4.6, 0.5, 0.3, 1.8, 0, 0, 0, 0, 0.5, 0.5,
    1.8, 0, 1.5, 0, 9.9, 0, 0, 0, 0, 0, 0.8, 8.1, 8.4, 0, 0, 1, 6.6, 8.4, 
    2.5, 5.8, 3.3, 0, 0, 2, 9.1, 0, 0, 5.8, 0, 0, 11.9, 1.5, 0, 5.8, 5.8, 0,
    0, 0, 6.4, 16.5, 9.4, 4.3, 5.8, 6.1, 0, 3.8, 0, 0, 0, 0, 0, 0, 0, 0, 
    1.3, 0, 0.5, 0, 0, 9.7, 8.1, 7.4, 5.3, 2.8, 3.3, 9.7, 0, 0, 12.4, 23.4, 
    0, 0, 0, 0.5, 5.8, 0, 0, 2.8, 2.8, 3.8, 1.8, 0.8, 1, 3.6, 0, 0, 0, 2.8, 
    1, 0.8, 3.8, 1.3, 2, 17.8, 1.8, 18.5, 3.8, 0, 0, 6.1, 0, 8.6, 0, 3.6, 0,
    3.3, 10.2, 4.6, 2, 0, 0, 0, 0.5, 5.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 1, 1.5, 3, 5.8, 1, 14.7, 0, 0.5, 0, 3.6, 1.8, 9.1, 
    72.40000000000001, 4.1, 16, 4.1, 11.4, 24.9, 15, 7.1, 0, 15.7, 7.6, 
    27.2, 0, 0, 0, 1.8, 0, 0, 5.3, 2.5, 6.4, 4.6, 4.1, 2.5, 0, 7.1, 0, 2.5, 
    0, 0, 6.4, 11.4, 34.3, 12.4, 8.6, 5.8, 1.5, 9.9, 2, 0.5, 13.2, 10.9, 
    2.3, 2, 0.8, 9.1, 11.4, 0.5, 2, 24.6, 19.3, 37.1, 26.7, 18.5, 6.1, 5.8, 
    0, 5.8, 4.6, 0, 33, 6.6, 3, 0.8, 1, 0.3, 1.8, 2.3, 0, 0.5, 0, 0, 0, 0, 
    0, 0, 0, 1.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 1.3, 0, 0, 0, 0, 
    0.5, 0, 0, 0, 0, 0, 0, 1, 9.1, 3.6, 2, 1.8, 0, 1.8, 3.8, 1, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 2.8, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 2, 2.3, 13, 10.2, 
    1.3, 0, 0, 2.3, 6.6, 0, 2.3, 0, 0, 0, 4.1, 0, 19.6, 21.1, 28.4, 22.9, 1,
    4.8, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 3.6, 7.4, 11.9, 0, 2, 0, 0, 0, 0, 
    17.3, 40.9, 2.8, 0, 4.3, 0.8, 3.6, 7.6, 4.8, 0, 3, 1, 0, 3.8, 2.8, 11.2,
    1.3, 4.1, 0.5, 2.5, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2.5, 
    8.9, 2.8, 0, 0, 3.6, 8.1, 8.6, 2, 0.8, 3.8, 21.6, 0, 2, 2.5, 20.1, 0.3, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 5.6, 0.8, 0.5, 12.4, 17.5, 0, 0.5, 3.3,
    1.3, 2, 0, 1.3, 8.4, 0.3, 0, 0, 0, 0, 2.5, 23.6, 11.2, 9.4, 3, 3.6, 5.1,
    2.3, 2.8, 2.3, 7.4, 0.3, 5.1, 14, 0.3, 0, 8.9, 0.5, 1.3, 2.3, 0, 0.3, 
    2.8, 0, 6.4, 20.8, 26.2, 0, 9.7, 7.1, 11.2, 5.1, 1, 4.8, 0, 0, 1, 10.2, 
    1.5, 4.1, 0, 1.5, 0.5, 2.3, 0.3, 0, 0.8, 6.4, 4.8, 0, 0, 0, 0, 0, 1.5, 
    0.5, 0, 5.1, 26.7, 5.3, 12.4, 21.1, 2, 0.8, 20.8, 9.4, 0.8, 0, 0, 0, 0, 
    0, 2.3, 18.3, 5.3, 3.8, 2, 0, 3.6, 0.3, 2.3, 1, 0, 2, 3.8, 5.1, 18.3, 
    1.5, 0, 0.8, 2.5, 0, 0, 0, 6.6, 1.5, 6.4, 5.8, 1.5, 1, 3.3, 5.8, 12.2, 
    2.3, 7.9, 4.8, 0, 0, 0, 2.3, 1.3, 20.8, 1.5, 0, 0, 0, 0, 0, 0, 0, 8.9, 
    28.2, 5.6, 4.3, 18.3, 1.8, 0.5, 0.5, 1.8, 2, 0, 0, 1, 0, 0.5, 0.5, 0.5, 
    1.3, 0, 0, 0, 0.8, 3.3, 8.4, 6.4, 3.3, 4.8, 4.6, 0, 0, 0, 6.9, 4.6, 4.3,
    1.3, 2, 21.8, 5.3, 13.2, 9.1, 1.5, 0.3, 0, 0, 0, 1, 3, 3.3, 0.3, 1, 0, 
    0.8, 5.1, 0, 7.6, 0, 0, 0.5, 1.5, 2.3, 8.1, 0.3, 0, 0, 0.5, 0.5, 0, 0, 
    0, 0, 0, 0, 0, 0, 2.5, 0.3, 0.3, 0.3, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1.5, 0, 1.5, 0, 1.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.8, 3, 4.6, 2.3, 
    10.2, 6.4, 0.3, 0, 0, 9.4, 0.3, 2, 2, 0.3, 7.9, 7.1, 4.8, 0, 0, 0, 13.7,
    0, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 5.1, 3.3, 0, 0, 0, 0, 0, 
    0, 0, 1.5, 0, 0, 4.8, 1.3, 0.3, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 3.6, 
    4.1, 0.8, 0, 0, 3.6, 0.3, 0, 0, 4.8, 0.3, 0, 1, 4.6, 2, 0, 3.8, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 27.9, 4.8, 4.8, 20.8, 17.5, 20.1, 7.9, 14.7, 0, 1.3, 
    0.3, 0, 7.4, 3.8, 3.3, 1.3, 0, 0, 5.1, 0.5, 6.1, 0.5, 0, 0.5, 0, 1, 9.1,
    0, 0, 0, 3.8, 13.5, 5.6, 4.6, 3.3, 0, 0, 0, 0, 0, 0, 0, 4.6, 1, 0, 0, 0,
    0.5, 0, 0, 0, 0, 4.8, 1, 18.8, 2.3, 0.5, 2.5, 26.2, 1, 1.5, 0, 0, 6.6, 
    7.4, 3, 2, 4.1, 19.3, 3.3, 6.6, 22.1, 2, 0, 4.8, 1.5, 0, 0, 0, 0, 0, 
    10.9, 4.3, 0, 0, 0, 0.3, 0, 0, 2.3, 13.2, 0.5, 2.5, 0, 0, 0.8, 2.5, 13, 
    0.8, 5.1, 7.9, 4.3, 0, 0, 0, 0, 0, 15.7, 24.4, 2, 15.5, 0.5, 8.4, 14.5, 
    15.2, 10.2, 26.7, 17.8, 2, 2.8, 5.8, 0, 0, 4.8, 7.4, 9.9, 0, 0, 0, 0, 0,
    2.3, 0.5, 13.7, 6.1, 12.4, 7.4, 0, 2.3, 2.5, 4.6, 7.1, 3, 0, 39.9, 17, 
    20.3, 17.3, 9.9, 12.4, 21.1, 2.8, 4.6, 3.6, 11.4, 19.8, 2.3, 2, 0.3, 
    4.8, 8.6, 5.8, 4.6, 14.2, 13, 9.1, 3, 5.1, 10.4, 5.3, 1, 0, 4.8, 4.3, 
    47, 3, 1.8, 0.3, 1, 2.8, 0, 0, 0, 0, 0, 1, 0, 0, 1.5, 0.8, 0.3, 2.3, 
    11.4, 2.5, 0, 1, 4.6, 20.8, 0, 0, 2.3, 0, 0, 0, 0, 3, 1.5, 9.4, 13.2, 0,
    0, 0, 1.5, 0, 0, 15.2, 0, 0, 12.7, 1.8, 7.4, 12.4, 0, 12.7, 9.1, 1, 3.8,
    2.8, 9.7, 16.3, 11.4, 6.6, 4.1, 0, 0, 0, 2.3, 0.5, 0, 0.8, 0, 0, 0, 1.3,
    0, 0.5, 0.8, 0, 1, 3, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0.8, 0, 
    0, 1.8, 14.7, 0, 0, 0.5, 0, 5.6, 0, 0, 3.6, 3.6, 3, 0, 5.3, 9.1, 0, 0, 
    0, 0, 0, 0, 0, 0, 1.8, 2.5, 0, 0, 1.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1.8, 2.8, 5.6, 0, 1, 0.8, 12.4, 23.6, 14.7, 8.9, 0, 1.3, 2, 0, 7.4, 0, 
    2.3, 0.8, 1.5, 0, 0, 0, 0.5, 11.2, 4.6, 0.5, 4.1, 1.3, 0, 0.8, 3.8, 0, 
    16.8, 7.6, 7.4, 0.5, 1, 11.4, 21.6, 2, 29.2, 6.1, 1.3, 1, 8.4, 8.4, 2.3,
    1.3, 0, 2, 0, 0, 0, 0, 5.8, 4.6, 0, 8.6, 0.5, 0, 0, 0.5, 8.9, 2.8, 1.5, 
    5.6, 0, 0, 16, 1, 0, 0, 0, 0, 0, 0, 4.3, 1.5, 1.3, 10.2, 28.7, 0.3, 0, 
    2.3, 18.3, 0.5, 1, 0, 0, 0, 13.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.5, 0, 
    3.6, 0, 0, 0, 0, 0, 0, 6.4, 0.3, 1, 1.5, 0, 1.8, 9.9, 27.9, 5.1, 0.5, 2,
    0.8, 0, 6.4, 0, 2.5, 0, 6.9, 13.5, 1, 0.5, 0, 0, 0, 0, 0.3, 0, 4.6, 0.3,
    0, 4.3, 4.8, 1.8, 0, 0, 4.3, 5.3, 1.8, 9.1, 3.3, 2.5, 0, 0.5, 2.5, 0.5, 
    0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    36.3, 29, 12.4, 29.2, 12.4, 17.5, 14.2, 14.7, 0, 0, 0, 0, 0, 0, 3.3, 
    0.8, 0, 0, 1.3, 0, 0, 0, 0.5, 0, 0, 4.8, 0, 0, 0, 0, 5.1, 0.5, 0.8, 3.6,
    0, 0, 1.3, 0, 0, 0, 0, 0, 8.1, 4.6, 10.9, 18, 0, 2, 13.5, 0, 0.8, 0, 
    0.5, 0, 1.3, 6.1, 8.1, 7.4, 16.5, 29.5, 1.3, 0.5, 5.6, 9.9, 13, 5.1, 
    4.6, 4.6, 15, 8.6, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11.7, 22.4, 14.5, 
    18.3, 2.5, 0, 0, 0, 0, 0, 0, 2, 0, 0, 21.3, 1.5, 3.3, 19.8, 1, 1.5, 
    12.4, 9.4, 2, 14.5, 6.9, 13.2, 10.2, 2.3, 9.4, 1.8, 1.5, 5.3, 0, 1.8, 
    1.5, 1.8, 0, 0, 0, 0, 0, 1.5, 1, 0, 0, 1.3, 6.6, 3.3, 9.9, 2.3, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11.4, 0, 0, 0, 0, 4.1, 1.8, 0, 8.9, 22.9, 
    3.6, 6.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11.9, 1.5, 0, 0, 1, 0, 0, 0, 
    0, 0, 0, 0, 0.5, 2, 0, 0, 0, 0, 0, 0, 0, 7.6, 13.2, 8.1, 3, 0, 0, 0, 0, 
    0, 0, 0, 0, 18.8, 0, 0, 0, 0, 0, 0, 2.3, 5.3, 0, 10.2, 12.4, 8.1, 4.6, 
    2.5, 0, 0, 0, 4.6, 11.9, 4.3, 1, 2.3, 12.4, 20.1, 4.3, 2, 0, 9.1, 0.8, 
    0, 0, 0, 10.9, 7.1, 4.3, 0, 0, 4.6, 4.6, 1.8, 3.6, 7.6, 9.7, 0, 0, 0, 0,
    2.3, 10.7, 0, 19.1, 0, 0, 0, 0, 1.5, 1.3, 4.8, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 5.8, 11.4, 14, 6.9, 1.8, 1.3, 0, 9.1, 6.6, 0, 1.3, 0, 4.6, 4.1, 12.7,
    0, 0.5, 0.8, 0.5, 0.3, 0.3, 0, 4.6, 0.5, 2, 2.5, 30.5, 30.5, 2.8, 6.1, 
    3.8, 3, 0, 0, 5.1, 0, 0, 0, 2.3, 1.8, 0, 3.3, 0, 4.8, 11.4, 14.2, 1.5, 
    15, 8.4, 4.6, 5.6, 55.9, 5.6, 7.6, 7.9, 31.8, 4.6, 1.3, 1.8, 0.8, 9.9, 
    0.5, 2, 0, 0.3, 1.5, 18.5, 12.7, 16.5, 0.5, 6.9, 0, 0.5, 0.3, 1.3, 0, 0,
    16.3, 1.3, 7.1, 14.2, 1.5, 1, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    3.8, 29, 7.4, 1.8, 0, 0, 0, 16, 3, 0, 0, 0, 0, 0, 5.1, 0.3, 0, 0.5, 1, 
    0, 0, 0, 0, 0, 1, 2, 4.3, 7.9, 2.5, 1.3, 3.8, 5.3, 51.3, 6.4, 0, 5.1, 
    85.3, 6.1, 5.1, 26.9, 41.9, 10.4, 6.4, 6.4, 10.9, 12.4, 5.1, 0, 12.7, 
    14, 19.3, 4.8, 3, 24.4, 2.5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2.3, 22.9, 3, 
    3.8, 4.1, 10.7, 1, 0, 1.3, 11.4, 3, 5.3, 6.6, 14.5, 0, 29.7, 7.9, 1.5, 
    21.8, 2, 16.8, 4.6, 19.1, 13.7, 0, 0, 6.6, 0.8, 1.3, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 7.6, 22.9, 0, 0, 0, 0, 4.6, 0, 0.8, 0, 0, 1.5, 0.8, 0, 0, 0,
    0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 7.6,
    8.9, 14.2, 6.1, 30.5, 4.8, 2.3, 7.6, 10.4, 9.1, 10.7, 0, 13, 9.7, 17, 
    9.9, 0.5, 21.6, 6.9, 22.4, 5.6, 3.8, 15.2, 2, 0.5, 4.3, 5.3, 3.6, 19.3, 
    1.3, 8.9, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.9, 0, 11.7, 7.1, 9.1, 10.7, 
    1.3, 0, 0, 0, 0.5, 4.6, 0.8, 1.5, 4.6, 4.3, 4.8, 7.4, 1.5, 0, 0, 3, 0, 
    0, 6.1, 0, 2, 2.8, 4.6, 20.8, 0, 0, 0, 0, 0.5, 0, 8.1, 3, 0, 0, 0, 0, 
    11.9, 0.8, 0, 4.6, 9.1, 4.1, 21.3, 0, 0, 0, 0, 0, 0, 0, 18.8, 2.3, 0, 0,
    0, 2.3, 6.4, 0, 0, 0, 0, 0, 0.5, 11.7, 14.5, 0, 0, 8.1, 0, 0, 0, 6.9, 0,
    1.8, 7.6, 10.2, 6.4, 4.3, 0.3, 0.8, 0, 0, 2.3, 0, 0, 0, 1.5, 33, 9.9, 0,
    0, 0, 0, 0, 0, 0, 0, 3, 0, 5.6, 1.3, 5.3, 0, 10.7, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 35.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 5.6, 2.3, 2.3, 0, 0, 10.2, 8.9, 0, 1.5, 11.4, 0, 2, 4.6, 2.5, 4.8, 0,
    7.1, 6.1, 0, 0, 0, 0, 0.8, 2, 0, 0, 0, 0, 0, 0, 0, 0, 3, 4.6, 0, 0, 0, 
    2.5, 1, 0, 0, 0, 0, 0, 0, 21.3, 21.8, 0, 0, 0, 0, 0, 0, 0, 5.6, 1, 6.1, 
    14, 2.5, 3.6, 7.6, 0, 0, 0, 7.1, 1.5, 0.5, 8.1, 2.5, 5.6, 0, 1.5, 5.6, 
    2, 0.5, 0.8, 2, 3.6, 2.3, 3.8, 8.9, 0, 0, 0, 0, 0, 2, 0, 2, 8.1, 8.6, 
    6.6, 11.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.3, 12.2, 12.7,
    14.7, 4.6, 1.5, 2.5, 1.3, 10.9, 1, 6.1, 5.1, 21.6, 0, 14, 7.4, 5.3, 2.3,
    20.6, 12.7, 17.8, 10.7, 8.4, 5.8, 2.5, 13, 10.9, 4.8, 0.3, 1.5, 0, 0.5, 
    0.8, 21.3, 18.3, 3.6, 21.1, 15, 1.8, 1, 12.7, 9.9, 10.9, 0.3, 8.6, 2, 
    1.5, 0.8, 1.8, 0, 3.3, 3, 1.8, 1.8, 0, 0, 0, 0, 1.5, 2, 0, 0, 0, 0, 0, 
    0, 2, 0, 0, 0, 0, 0, 0, 0, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1.8, 4.1, 
    0.8, 1.5, 0.3, 0, 0, 0, 0, 0, 0, 0, 5.1, 6.6, 8.6, 26.2, 4.6, 15.2, 
    15.5, 4.3, 0.8, 4.1, 0, 3.8, 0, 0, 0, 0, 0, 0, 0, 0, 1.5, 0, 5.1, 1.5, 
    11.2, 5.1, 0, 0, 0, 0, 0, 5.8, 3.6, 3, 2.3, 3, 19.3, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.6, 0, 9.4, 0, 0, 0, 2.5, 23.9, 
    8.9, 10.9, 1, 0.8, 3.8, 2.5, 19.6, 0, 0, 2.3, 0, 0, 0, 0, 0, 1.5, 4.8, 
    4.3, 14, 4.1, 5.6, 4.6, 2.5, 0, 0, 0, 13.2, 3, 4.6, 0, 0, 0, 0, 4.6, 
    6.4, 0, 0, 0, 0, 0, 0, 2.3, 5.1, 0.3, 0, 0, 5.3, 20.1, 1, 1.8, 9.1, 0, 
    0, 6.1, 1.3, 1.8, 0, 0, 0, 0, 0, 0, 0, 5.6, 0, 5.1, 3.8, 24.1, 7.1, 7.4,
    1.8, 2.3, 7.6, 0.8, 1.3, 2.5, 0, 2.3, 3.8, 0.5, 3, 0, 23.1, 0, 0, 2.3, 
    0, 0, 0, 0, 0, 0, 3.6, 1.5, 29.2, 0, 8.9, 1.5, 0, 1.8, 1.5, 0, 2, 3, 
    10.7, 3.3, 1.5, 0.5, 0, 0, 0, 1.8, 0, 0, 0, 0, 0, 0, 0, 8.9, 0, 0, 0, 0,
    0, 0, 0, 0, 1.8, 0, 0, 0, 0, 18.5, 0, 15.2, 2.5, 4.6, 9.7, 19.6, 9.9, 
    4.1, 3.3, 1.8, 2, 0.8, 4.1, 14.5, 5.3, 3.6, 0, 0, 0, 2.8, 6.9, 1.5, 9.4,
    4.8, 1.3, 10.7, 0, 0, 0, 0, 0, 0, 0, 6.6, 21.3, 3.8, 0, 0, 3, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 5.1, 55.9, 4.8, 1, 3.3, 5.3, 15.2, 19.3, 20.1, 
    22.9, 25.4, 14, 5.8, 8.1, 1.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.3, 
    6.4, 4.3, 16, 6.9, 8.4, 0, 7.4, 0.5, 2.5, 0, 1.5, 1, 1.3, 3.8, 1, 0.5, 
    2, 6.6, 8.4, 0, 1.3, 0.5, 0.3, 1, 0, 0.3, 1.8, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 1, 15.5, 18.8, 0.5, 0, 0, 4.3, 1.8, 1, 0, 0, 0, 3.6, 0, 
    0, 3, 2.3, 0.5, 1.3, 0, 0, 0.3, 0, 1, 0, 0, 15.2, 6.9, 11.7, 0, 0, 0, 0,
    0, 1.3, 2.3, 6.6, 0, 0, 0, 0, 0, 5.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 9.7, 17.8, 2.3, 5.3, 7.9, 1.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    10.7, 0, 0, 6.1, 0, 0, 1.3, 9.9, 0, 0, 0, 0, 0, 1.3, 1.5, 0.3, 0, 0, 0, 
    0, 0, 0, 0, 0.5, 10.2, 5.8, 1.3, 2, 8.6, 0, 0, 2.5, 10.9, 0.8, 0, 0.5, 
    19.6, 0, 7.1, 7.1, 1.5, 6.6, 6.1, 5.1, 0.3, 0.3, 0, 0.8, 0, 0, 1.5, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 
    0, 2.5, 0, 0, 0, 0, 0, 0.5, 0, 11.2, 2.3, 14.2, 44.2, 0.5, 0, 0, 0, 0, 
    0, 0, 0, 1, 1, 1.3, 0, 4.1, 0, 14, 4.8, 1.8, 0, 5.1, 1.3, 15.2, 2, 2, 
    3.8, 6.1, 0, 0, 0, 0, 0, 0, 2.8, 0, 0, 0, 0, 0, 0, 38.1, 0, 0, 0, 0, 0, 
    0, 0, 19.1, 1, 1.3, 0, 7.4, 0, 0, 0, 0, 0, 0, 5.8, 26.7, 17, 0, 2.5, 
    4.6, 5.1, 19.1, 8.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.9, 7.4,
    2.5, 2.3, 7.6, 6.4, 0, 0, 5.6, 2.5, 8.9, 34.3, 31.8, 8.6, 11.9, 2.5, 
    15.5, 32, 12.7, 9.1, 3.8, 0, 0, 3.8, 0, 0, 0, 0, 0, 6.4, 11.4, 6.1, 
    11.9, 13.5, 1.3, 2.8, 2.5, 13.2, 11.4, 10.2, 7.1, 22.4, 31.8, 1.8, 2.5, 
    6.1, 9.9, 7.9, 1.8, 1.5, 0.5, 2.3, 3.8, 0.5, 4.3, 4.6, 3, 1.3, 0, 0, 
    1.3, 1.8, 10.2, 6.4, 3.8, 0, 0, 0, 4.1, 9.1, 2.5, 2, 0, 3.8, 2.5, 3, 0, 
    1.8, 0, 1.3, 0, 1.8, 0, 0, 0, 0, 0, 0, 1.3, 0.5, 0, 5.1, 2.5, 0, 0.3, 0,
    0, 1, 0.5, 0.8, 3.8, 11.4, 5.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.8,
    3.8, 14, 18.8, 19.3, 4.8, 8.4, 17.8, 3.6, 4.6, 14, 9.7, 12.4, 9.7, 12.4,
    3.8, 5.6, 0.3, 0, 0, 0, 22.4, 2.8, 0, 0, 6.9, 10.2, 0.5, 0, 0, 3.6, 0, 
    3.6, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8.9, 14.2, 12.2, 3.6, 14.5, 
    11.2, 3.6, 1.3, 1.8, 4.8, 0, 0, 0, 0, 0, 0, 0, 0, 1.5, 4.8, 3.8, 1.3, 0,
    0, 0, 5.1, 1.5, 4.8, 3.8, 1.8, 0, 0, 2.3, 4.6, 16, 5.1, 0, 0.5, 0, 2.5, 
    1.5, 3, 4.1, 3.3, 0, 6.4, 2.5, 2, 6.9, 1.3, 0, 0, 1, 0, 5.1, 7.9, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 7.4, 0.8, 2, 1, 5.6, 16.3, 0, 0, 2.5, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8.9, 6.4, 0, 0, 2, 29.2, 3, 2.5, 
    13.5, 4.8, 2, 5.6, 0.5, 0.3, 1, 0, 0, 0, 0, 0, 0, 0.8, 3.8, 1.3, 0, 0, 
    0, 11.4, 4.8, 0, 16.8, 0.5, 23.6, 9.7, 1.3, 0.3, 0.5, 3.3, 4.6, 19.3, 
    1.8, 1.3, 6.6, 0.8, 0, 0, 0, 1.3, 5.8, 11.2, 10.2, 0, 0, 0, 0, 0, 35.6, 
    0.5, 0, 0.5, 15.2, 0, 1, 14, 6.1, 7.6, 3.3, 6.1, 45.2, 2, 0.8, 7.9, 5.6,
    10.2, 0.5, 5.1, 8.1, 12.4, 2.5, 0, 10.4, 6.6, 0.5, 2.3, 0, 10.7, 0.3, 
    25.9, 1.8, 1.3, 10.9, 1.8, 7.1, 8.9, 4.6, 23.9, 3, 1, 8.6, 8.4, 3.6, 
    4.1, 0.3, 9.1, 11.7, 0.3, 11.4, 2, 2.5, 27.4, 5.6, 2.3, 0, 0, 0, 0.5, 
    0.3, 0.5, 2.3, 0.5, 0, 0, 10.4, 2.3, 2, 0.5, 1, 0.5, 0, 0, 0, 2.3, 1, 0,
    0, 3.3, 10.4, 0, 1, 7.9, 0, 2.5, 11.4, 0, 0, 0, 0, 0, 3.8, 3.6, 12.4, 
    18.3, 17, 13, 2.3, 4.3, 4.1, 7.6, 15.5, 23.1, 30.5, 8.9, 6.4, 0, 0, 0, 
    22.9, 10.2, 0.5, 0, 21.3, 2, 3, 1.3, 0.8, 1.5, 4.6, 2, 0, 12.4, 20.3, 
    4.8, 4.1, 18.8, 7.1, 2.5, 9.1, 5.6, 1, 0.8, 2.3, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 19.6, 13.5, 4.8, 0, 7.9, 10.9, 22.4, 9.4, 15, 2.3, 2.5, 24.6, 4.6, 
    7.1, 4.3, 14.5, 0, 4.8, 0, 0, 1.3, 0.5, 1, 4.8, 0.5, 1, 2.3, 4.8, 0, 0, 
    0, 0.8, 0.8, 1, 5.3, 39.4, 16.3, 4.3, 1.5, 6.1, 0.5, 2.8, 1, 0, 0, 0, 0,
    30.2, 11.7, 6.4, 9.7, 8.4, 5.6, 6.1, 0.8, 2.5, 3.6, 5.1, 3, 0, 0, 0, 0, 
    0, 0, 0, 44.5, 0.8, 1.3, 0.3, 2.5, 5.1, 17.5, 17, 3.8, 0.3, 10.9, 11.9, 
    1.3, 5.6, 0, 5.8, 0, 2.5, 0.3, 7.6, 3.8, 0, 3.8, 7.4, 0, 7.6, 2.8, 0, 
    8.9, 1.5, 6.9, 11.4, 1.8, 19.1, 1, 1, 0.5, 15.2, 0, 0, 0, 1.3, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 2.5, 2, 1.8, 3.3, 12.4, 11.7, 0, 7.4, 0, 0, 0, 5.6,
    0, 0, 0, 0, 0, 0, 0, 2.5, 0, 1.3, 2.3, 10.7, 5.6, 31.8, 15.2, 0, 0, 
    17.5, 10.2, 0.8, 1.3, 0, 0, 0, 0, 0, 0, 0, 0.8, 0, 0.8, 33.8, 3, 3.6, 
    3.3, 3.8, 2.5, 0, 0, 0, 0, 0, 0, 0, 4.8, 0, 0, 0, 0, 2, 4.6, 1.5, 7.6, 
    0, 0, 0, 0, 0.8, 0, 0.5, 0, 2.3, 18, 17.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    26.7, 14, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 10.7, 19.3, 0.5, 15, 11.2, 0.8, 
    0, 0.3, 5.1, 24.9, 2, 1, 0, 0, 1.5, 3.6, 15.7, 1.5, 1.8, 3.6, 0, 5.6, 
    3.8, 14.5, 7.1, 4.3, 7.1, 4.8, 4.6, 0, 3.3, 0, 9.4, 1.3, 1, 0, 0, 0.5, 
    0, 4.3, 1, 5.3, 5.3, 22.4, 0.5, 3.8, 0, 0, 0, 0, 0, 7.9, 3.8, 26.9, 4.6,
    5.1, 15, 0.3, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.3, 0, 0, 0, 0.8, 
    0, 0, 0.5, 4.8, 5.1, 0.3, 0, 0, 0, 0, 15.2, 5.8, 11.9, 4.8, 5.3, 7.9, 
    10.7, 26.9, 51.6, 13, 6.9, 6.4, 0.5, 7.6, 2, 0, 0, 0, 14.5, 6.1, 14, 
    29.7, 8.9, 7.4, 13, 3.6, 6.9, 5.1, 15.7, 1.5, 0, 2.3, 0, 1.8, 2, 0.5, 
    1.3, 3.6, 0, 4.1, 3.6, 0, 17, 2, 0, 0, 0, 0, 0, 0, 0, 0.8, 5.6, 6.4, 0, 
    0, 0, 11.4, 12.4, 7.4, 22.1, 8.4, 26.9, 5.8, 0, 3.6, 18.5, 4.8, 1.3, 
    4.3, 0, 1.8, 0, 11.9, 14, 19.8, 1.8, 0, 5.1, 1, 5.6, 11.2, 2, 0, 0, 0, 
    0, 0, 0, 4.6, 10.9, 8.1, 1.5, 5.8, 8.4, 10.2, 7.6, 1.3, 2.5, 4.1, 0, 0, 
    3.8, 2.8, 0.5, 1, 2.8, 2.3, 4.1, 7.6, 2.5, 0, 1.8, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 8.1, 12.7, 3.3, 21.1, 4.6, 13, 0, 0, 0, 0, 0, 0,
    19.8, 0, 5.1, 0, 3, 1, 6.1, 1, 6.1, 9.1, 0, 0, 0, 0.5, 0.8, 0, 0, 0, 0, 
    0, 9.7, 15, 7.4, 4.6, 1.3, 11.2, 4.1, 0, 2.8, 8.6, 6.9, 0.5, 5.8, 0, 0, 
    2.8, 9.1, 4.8, 2.8, 0, 0, 0, 0, 0.8, 0, 1.5, 7.6, 1.3, 7.4, 7.6, 1, 
    18.3, 5.8, 6.9, 1.8, 11.4, 25.4, 7.9, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0.3, 2.5, 5.1, 5.8, 10.2, 8.1, 0, 0, 0, 2, 1.8, 17, 0, 0,
    0, 5.1, 1.3, 0, 0, 0, 4.8, 0.5, 0, 1, 4.1, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 
    2.3, 0, 0.3, 0.5, 3.8, 0.8, 35.3, 0, 0, 0, 0, 0, 0, 0.8, 0, 0.8, 2.3, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.1, 0, 3.6, 0, 10.7, 0.3, 5.1, 16, 
    29.2, 2.8, 0.8, 21.8, 0.5, 0, 14.7, 59.4, 9.9, 3.3, 5.1, 0, 0, 0, 0, 0, 
    0, 0, 0, 0.8, 0, 0, 0, 0.8, 1.8, 0.3, 2, 2.3, 0, 3.8, 0, 19.8, 7.9, 0.8,
    0.3, 0.5, 0, 0, 0, 0, 1.8, 2.8, 0.3, 13.5, 2.3, 0, 0, 5.8, 4.8, 3.3, 
    10.7, 15.7, 6.4, 9.1, 6.9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0.5, 11.4, 15.2, 
    0, 0, 0.5, 0, 3, 0.8, 0, 15.2, 10.7, 2.8, 6.9, 2, 7.9, 28.7, 4.8, 5.8, 
    14.7, 13, 2, 3, 1, 7.4, 0.8, 0.3, 5.1, 3.6, 1.3, 0.5, 0, 0, 0, 4.6, 4.8,
    0, 0, 3.3, 5.1, 4.6, 20.3, 0, 0, 0, 3.6, 10.4, 8.9, 4.1, 5.8, 0, 0, 0, 
    0, 0, 0, 1.3, 0.5, 1, 6.1, 2.3, 6.1, 7.1, 1.8, 3, 6.9, 4.1, 2, 15.7, 1, 
    22.1, 2.3, 1.3, 3.8, 5.3, 4.3, 3.3, 2.8, 3.8, 0.5, 6.1, 4.1, 0, 0, 0, 0,
    6.1, 0.3, 0, 0, 2, 0.5, 0.8, 0, 0.3, 0, 1.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 1.3, 0.3, 0.3, 13.2, 0.3, 0, 0, 0, 0, 0.5, 0.3, 0, 8.6, 24.9, 
    3.8, 0.5, 0.3, 8.9, 0, 0, 0.8, 3, 3.6, 2.3, 1.8, 1.8, 0.8, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 6.1, 3.6, 3.6, 12.2, 2.8, 0.3, 1.3, 11.9, 3.3, 10.2, 7.4, 
    1.8, 0, 0, 0.8, 0, 10.7, 2.8, 0.3, 0, 1.3, 2.8, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 10.2, 5.3, 25.4, 1.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.3, 13.7, 
    33.5, 2.8, 5.3, 2.3, 35.3, 1.5, 15.2, 3.3, 8.9, 0.5, 0, 0, 0.3, 0, 0, 0,
    7.1, 0.3, 1.5, 2.5, 0, 0, 0, 0, 0, 0, 0.8, 0, 0, 0.8, 0, 4.3, 11.4, 3.6,
    0.5, 5.6, 6.9, 10.2, 30.5, 1, 0, 0, 0, 2.3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 
    0, 0.3, 8.4, 0, 0, 21.3, 1.3, 3, 1, 1.3, 11.4, 0.8, 1.8, 4.6, 1, 5.8, 
    3.3, 0.3, 0, 8.6, 3.6, 1.3, 4.6, 0.8, 24.9, 0, 0, 0, 0, 2, 1.3, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 2.8, 2.3, 3.6, 0, 11.9, 4.8, 14.2, 2.5, 13.2, 0.5, 0, 
    0, 0, 1.3, 12.7, 0, 0, 12.4, 2.3, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 19.8,
    3.3, 29.7, 0.5, 0, 25.9, 4.6, 20.1, 6.6, 1, 2.5, 0, 0, 5.6, 0, 3.6, 6.4,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.8, 0, 0, 0, 2.8, 0, 0, 0, 0, 2.3, 3, 
    9.4, 36.8, 1.8, 47.8, 1.8, 0, 9.9, 4.6, 21.1, 16.8, 12.7, 6.4, 1.5, 1.3,
    0.8, 3.3, 14, 3, 0, 1.3, 1, 1.3, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 11.4, 1, 
    6.9, 11.2, 25.9, 0, 0.5, 0.5, 0, 0, 12.2, 4.1, 0, 0, 0, 0.5, 0, 0, 0.5, 
    0, 0.5, 1.8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2.3, 7.9, 1.8, 0.3, 0, 0, 1.5, 
    0, 0, 1.8, 0, 0, 0, 2.5, 5.1, 3.3, 0.3, 0, 0.5, 5.6, 16, 3.3, 0.5, 1.3, 
    0, 0, 0, 3.8, 2.3, 4.3, 0, 0, 11.9, 0.8, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 
    20.8, 0, 15.5, 11.9, 5.1, 3, 14, 11.7, 0.3, 0, 0, 15, 8.4, 1.5, 2.5, 0, 
    4.1, 1.3, 0, 10.9, 9.1, 3.6, 9.4, 5.8, 15.2, 7.4, 0.5, 1, 23.1, 0, 0, 1,
    0, 0, 0, 0, 8.1, 0, 0, 0, 20.1, 2.5, 0.8, 22.6, 5.1, 0.5, 0, 0.5, 1, 0, 
    0, 10.7, 5.1, 7.9, 3.8, 3.6, 6.1, 0, 4.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.3, 21.8, 13, 4.6, 4.3, 6.6, 
    0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.4, 28.2, 7.9, 2, 
    0.3, 20.1, 11.4, 4.3, 0.3, 0, 0, 0, 0, 2.5, 1.3, 29.2, 10.2, 25.9, 8.4, 
    16, 8.9, 0, 0, 5.1, 2.5, 0.5, 2.3, 0, 2.5, 1, 0.8, 0, 1.5, 2, 0.8, 4.3, 
    2.3, 0, 1.3, 4.6, 0.3, 6.9, 2, 11.9, 0.3, 4.1, 2.5, 2.3, 0, 0, 0, 7.1, 
    0, 1.5, 13.7, 7.9, 0.3, 0, 1.3, 0, 0, 2.5, 2.8, 42.9, 4.1, 4.3, 3, 1, 0,
    1.8, 2.8, 0.8, 0, 0.3, 18.5, 3.3, 2.5, 8.9, 2, 2.3, 3.3, 16.8, 0, 6.9, 
    0, 0, 15.2, 2.5, 1.3, 2.5, 4.8, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2.3, 
    0, 1.8, 8.9, 4.3, 7.6, 4.8, 2.8, 11.4, 7.4, 2.3, 5.1, 3.6, 6.6, 1.3, 1, 
    0, 2.8, 2, 0.5, 0, 27.7, 13, 5.8, 7.1, 1.3, 1, 0.3, 2.3, 1.3, 0, 11.9, 
    11.2, 1.5, 0, 3.6, 1.8, 2.3, 0.8, 0.3, 0.8, 0.3, 0.5, 7.6, 10.7, 2.8, 
    4.3, 3.8, 2.8, 3, 0, 18.5, 3.8, 1, 12.4, 0.5, 14, 2.3, 10.9, 4.8, 1.8, 
    0, 0, 7.4, 4.6, 1, 0, 13.5, 3, 10.4, 6.6, 2.5, 4.8, 0, 0, 0, 0, 0, 1, 0,
    16.5, 37.6, 23.4, 18, 24.9, 55.4, 26.7, 4.3, 19.3, 2.3, 2, 4.8, 0, 0, 
    9.4, 11.9, 27.9, 10.2, 4.3, 5.6, 22.6, 10.9, 2.5, 1.8, 0.5, 4.6, 0, 0, 
    2.3, 0, 0, 3.8, 0, 0.8, 3, 1, 1.5, 0, 0, 0, 0, 0, 0, 16.3, 9.9, 1, 0, 0,
    0, 0, 14.7, 1.3, 0, 21.1, 8.4, 15, 0, 0, 0, 8.6, 5.3, 2.8, 2, 0, 0, 3.8,
    1.5, 0, 22.1, 2.5, 13.5, 6.9, 7.1, 16.8, 5.6, 12.4, 0, 21.1, 2.3, 1.3, 
    2, 0, 0, 1.3, 3, 6.9, 0.5, 3.3, 0, 4.8, 0, 7.9, 0, 0, 0, 0, 0, 6.9, 0, 
    0, 0, 0, 0, 0, 0, 4.3, 0.3, 0, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.4, 0, 
    0, 9.9, 7.9, 0.5, 20.6, 3.3, 0, 0, 0, 0, 0, 0.8, 0, 0, 3, 2.3, 0, 0, 
    7.4, 2.5, 1.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 4.8, 0, 6.4, 
    7.9, 4.3, 0, 8.9, 4.1, 4.3, 1, 9.4, 2.3, 0.3, 3.8, 6.9, 1.8, 0, 7.1, 
    6.6, 7.9, 2.5, 4.8, 22.6, 2.5, 0, 0, 0, 0, 0, 0, 0, 5.8, 6.4, 0, 0, 0, 
    0, 1.5, 1, 11.2, 2.3, 0, 13.7, 2.8, 0.5, 0, 0, 0, 22.6, 0, 0, 0, 0, 0, 
    8.4, 0, 0, 0, 0, 2.8, 1.3, 0, 0.5, 0, 6.4, 0, 1.5, 0, 0, 3.3, 1.3, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 2.5, 0, 5.3, 20.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0.8, 0, 2.8, 0, 
    0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2.5, 0, 0, 8.4, 0, 0, 0, 0, 
    2.8, 7.9, 0, 0, 14.5, 2.8, 2.5, 1.8, 0, 0, 0, 3, 20.8, 2.8, 0, 0.3, 0, 
    0, 0, 0, 2.3, 0, 0, 1.5, 1, 3.3, 17.5, 3.6, 10.9, 0.5, 0, 0, 0, 0, 0, 0,
    0.8, 0, 2.3, 35.3, 3.6, 3.3, 0, 0, 0, 0, 0, 0, 1.5, 0, 0.5, 0, 0, 0, 
    42.4, 33, 11.9, 2.5, 9.1, 4.1, 21.6, 4.1, 29, 0.5, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 1.3, 0, 0, 0.8, 1, 1, 0.5, 5.1,
    0, 5.8, 10.9, 26.9, 12.4, 5.6, 2, 1.3, 0, 33, 2.5, 0, 4.8, 7.6, 2.5, 
    7.1, 4.1, 0.3, 6.4, 7.6, 20.8, 1.8, 0.8, 0.5, 0, 0, 0.3, 0, 0.5, 3.3, 
    15.2, 11.2, 1.5, 4.3, 40.1, 4.1, 2.5, 0.8, 2.8, 0, 3.6, 9.7, 6.6, 18.8, 
    2.3, 1.5, 10.7, 1.5, 0.8, 5.6, 1, 11.4, 1.3, 0, 0, 0, 2.5, 0.3, 0, 0, 0,
    0, 0, 0, 1.5, 0.3, 0, 0, 0, 2, 3.8, 0.8, 2.5, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0.8, 1.3, 2.5, 0, 0, 2.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.1, 0.3, 0, 0, 
    6.9, 13.5, 5.1, 2.8, 4.3, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 2.5, 0, 0, 
    0, 0.3, 0, 0, 0, 9.7, 0, 4.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 9.7, 8.9, 0,
    0, 0, 4.3, 0.8, 9.1, 0.8, 0, 0, 0, 0, 2, 5.1, 2.5, 0.5, 0, 0, 0.8, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 4.3, 0.5, 0, 3, 0, 0, 0, 1, 0.5, 0.5, 18.8, 4.8, 
    0.8, 11.7, 6.6, 0, 0, 0, 0, 0.3, 0, 0, 7.1, 7.1, 0, 0.5, 0.8, 0.8, 0, 0,
    0, 0, 6.4, 0, 0, 0.8, 0.3, 14, 7.6, 4.1, 2.5, 4.3, 0.3, 0.5, 0, 0, 0, 0,
    0, 0, 0.8, 0, 0.3, 0.5, 24.9, 34.8, 2.5, 0, 0, 0, 0.5, 0, 0, 0, 0.5, 
    38.1, 9.9, 1.5, 10.9, 16.3, 0, 0, 0.8, 4.3, 3.3, 0, 0, 0, 2.8, 0.3, 9.9,
    0.8, 0, 0.8, 3, 13, 4.8, 0.3, 0, 1, 0, 7.1, 5.6, 13.7, 22.9, 0.8, 2.5, 
    2, 2, 0, 14, 15.5, 6.6, 6.4, 24.1, 9.1, 0, 28.2, 1.3, 0, 4.1, 0.3, 0, 0,
    0, 0, 5.6, 0, 0, 1.8, 11.9, 17.8, 1.5, 0.3, 0, 0, 39.4, 2, 0.3, 1.8, 
    22.1, 0.3, 1.5, 6.4, 2.3, 1.5, 0.8, 3.3, 0, 0, 0, 0, 0, 0, 0, 6.6, 7.9, 
    0.3, 4.8, 0, 0, 0, 0.5, 3.8, 5.8, 1.3, 1, 4.1, 1.5, 0.5, 0, 0, 0, 0, 0, 
    0, 0, 0, 15, 1, 7.4, 2.8, 0, 2, 0, 0, 0, 0, 0, 5.1, 1, 0, 0, 0, 0.8, 
    4.3, 2.3, 14.7, 6.9, 1.8, 3.8, 0.8, 1.5, 3.6, 1.5, 0, 0.3, 1, 0.5, 4.8, 
    4.1, 1, 6.6, 4.3, 16.5, 27.7, 5.8, 0.5, 0, 0, 0, 0, 0, 11.9, 0, 21.1, 
    3.6, 34, 2, 21.6, 3.3, 0, 5.1, 1, 1.5, 2.8, 10.4, 0, 1.8, 0.3, 1.3, 0, 
    1, 2, 0, 0, 0, 0, 0, 0, 0, 4.6, 5.3, 0.5, 9.4, 4.3, 7.9, 5.3, 0.3, 10.2,
    11.4, 9.1, 35.6, 1.8, 0.5, 17.8, 22.6, 15, 23.4, 34.3, 13.5, 3, 15.7, 
    5.8, 7.4, 9.4, 9.7, 2, 5.8, 2.3, 3, 0, 3.8, 1, 3.6, 7.4, 7.1, 6.1, 1, 0,
    0, 0, 0, 0, 2.3, 14.2, 8.9, 3.3, 22.1, 33.5, 1.5, 0, 0, 0, 0, 0, 6.9, 
    1.8, 2, 18.5, 0.3, 0.3, 0, 1, 2, 10.2, 6.4, 2, 0.3, 0, 0, 1.5, 0, 0.3, 
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0.8, 0.5, 0.8, 0.3, 0, 0.5, 1.3, 0.8, 1, 0, 
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.5, 5.3, 0, 8.9, 11.4, 
    5.6, 1, 2, 18.5, 1, 17, 1.5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0.8, 0, 0, 1.5, 5.1, 17, 20.3, 0, 0, 0, 0, 0, 0, 0, 0, 21.8, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 15.7, 0, 7.4, 0, 0, 0, 5.1, 10.7, 0.5, 2.3, 0.5,
    15, 3, 14.5, 1.3, 2.5, 1.3, 14.7, 11.2, 1.3, 14.7, 1.5, 2.5, 3, 0, 2.8, 
    0.5, 0.5, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.3, 31, 10.7, 15.2, 9.1, 
    15.7, 5.6, 1.8, 0, 0.5, 0, 0, 0.3, 0, 1.5, 2, 2.3, 5.8, 0.5, 4.8, 3, 0, 
    0, 3.8, 1.5, 0, 2.5, 0, 4.6, 0.5, 10.7, 0, 0, 17.3, 24.1, 9.9, 8.4, 
    18.8, 9.1, 1, 0.3, 0, 0, 0, 0.5, 15.2, 1.8, 4.1, 8.4, 6.9, 0, 0, 4.1, 
    1.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20.3, 23.1, 3.8, 
    2.3, 1.8, 3.8, 0, 1.3, 2.8, 2.3, 11.9, 1, 11.9, 10.7, 0.5, 11.4, 10.7, 
    15.2, 17.5, 6.1, 7.6, 0, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, 0, 0, 1.3, 0, 0, 
    8.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9.4, 6.1, 4.1, 9.4, 36.6,
    10.9, 2.5, 6.9, 0, 0, 0, 4.8, 1, 2, 5.1, 4.1, 2.5, 3.8, 4.1, 0, 0.3, 
    1.8, 0, 0, 1.3, 0.5, 0, 0, 0, 22.1, 10.7, 7.9, 3.6, 10.4, 5.8, 16.8, 
    5.8, 1, 0, 0.3, 1, 1.3, 0.8, 1.8, 2, 1.8, 1.5, 2.5, 9.1, 3, 16.8, 1.3, 
    22.1, 6.1, 0.3, 0, 0, 0, 0, 0, 2.8, 7.4, 4.3, 2.5, 4.1, 36.3, 16.3, 6.9,
    0, 9.4, 0, 0, 0.8, 1.3, 0, 0.8, 0.3, 1.8, 14.5, 16, 15.2, 0.3, 0, 0, 
    7.6, 0.8, 0, 0, 0, 1.8, 0.5, 3.6, 0, 0.8, 0, 4.3, 2, 0, 0.5, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1.3, 1.8, 0, 16, 6.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    3, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 2.3, 13.2, 2, 0, 0, 0, 0, 0, 
    0, 0, 0, 2.8, 4.1, 1.3, 2.8, 1, 0.5, 5.3, 0, 5.6, 0, 18.8, 1, 0.8, 0, 
    0.5, 0.8, 0.5, 28.2, 25.7, 5.6, 0, 0, 0, 5.6, 0.8, 2.3, 2, 12.7, 6.4, 3,
    0.5, 1, 18.8, 0.5, 0, 7.4, 1.8, 0, 0, 0, 0, 0, 0, 0, 2.5, 7.9, 1.5, 8.4,
    16.5, 4.8, 8.9, 7.4, 0, 0, 0, 2.5, 0, 0, 0, 2.8, 0.3, 0, 3.6, 0, 0, 0, 
    0, 11.2, 21.3, 0.8, 0, 2.8, 0.8, 0, 0, 1.3, 5.8, 7.6, 0, 2.8, 3.6, 1, 
    7.6, 12.7, 4.3, 0, 0.8, 2.8, 2.3, 7.1, 10.9, 7.9, 3.8, 12.4, 14.5, 4.3, 
    0.3, 11.4, 9.1, 0.8, 8.6, 1, 0, 0.8, 0, 4.3, 17.8, 2.5, 0.5, 6.4, 10.4, 
    38.4, 0.3, 0.3, 0, 0, 0, 0.5, 0.8, 0, 38.1, 0, 0, 0, 5.1, 11.4, 0, 0.8, 
    0, 0, 0.5, 0, 19.1, 0.5, 0, 1.8, 4.1, 12.4, 8.4, 8.9, 3.6, 21.3, 0.3, 0,
    0, 5.8, 22.9, 18, 11.2, 10.7, 21.8, 47, 7.9, 5.6, 9.9, 8.6, 0.5, 1.3, 
    0.3, 6.6, 1.8, 9.7, 0.5, 0.3, 0.5, 0, 2.3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 0.5, 8.1, 0, 20.6, 7.9, 0.3, 6.1, 9.7, 0, 0, 10.4, 6.4, 4.8,
    0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 12.2, 17, 5.3, 0, 0, 0, 18.8, 6.4, 0, 25.7, 20.8, 1.3, 0, 0, 0, 0, 0,
    7.9, 8.1, 11.7, 14.2, 11.7, 0, 17, 4.1, 7.6, 31, 13.2, 0, 0, 0, 0, 0, 0,
    6.6, 0, 0, 0, 0, 23.6, 22.1, 9.1, 22.6, 4.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 7.9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.8, 0, 0,
    0, 0, 0.3, 0, 0.8, 1.5, 21.8, 15.7, 21.1, 10.9, 12.2, 2, 0, 0, 10.9, 
    4.3, 1.8, 3, 9.9, 0, 0, 0, 0, 0, 0, 3, 0, 13.7, 1.5, 3.8, 3.8, 2.3, 
    15.2, 16.3, 1.5, 0.5, 0.3, 0, 0, 0, 0, 1.3, 2.3, 13.5, 5.8, 4.3, 10.2, 
    5.8, 2.5, 3.3, 2.8, 23.4, 0, 0, 0, 5.6, 0, 0, 0, 9.4, 30.5, 3.6, 5.6, 
    0.5, 0, 0, 4.6, 0, 5.6, 0, 0, 0, 0, 0, 4.6, 0.5, 1.3, 0, 0, 0, 0, 0, 0, 
    0, 0, 1.3, 5.8, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 3.6, 0, 0, 0, 2.3, 1.5, 
    7.6, 2, 2.5, 1.8, 0, 0, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.6, 0, 0, 3.3, 
    10.2, 0.8, 3.3, 2.8, 0.5, 2, 0, 0, 0, 0, 0, 0, 0, 0, 6.4, 3.8, 1.3, 0, 
    0, 1.3, 7.1, 3.3, 0, 0, 0, 0, 0, 0, 0, 2.8, 7.1, 5.8, 18.8, 2.3, 0, 0, 
    0, 0.5, 0, 0, 0, 0, 0, 0, 0, 6.4, 6.6, 4.6, 12.7, 0.3, 0, 0, 0, 0, 0, 
    8.9, 1, 0.3, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.6, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.4, 0, 0, 0, 1.3, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 10.2, 0, 19.1, 1.5, 5.6, 8.6, 2.3, 0, 0, 0, 8.4, 31.2, 5.1, 
    0.8, 8.4, 0.3, 2.3, 0, 3, 3.8, 18.5, 7.9, 10.7, 1.3, 3.8, 2.3, 2.3, 13, 
    0.5, 0.3, 1, 0, 0, 3.6, 7.9, 10.9, 2.8, 18.3, 22.9, 35.6, 3.8, 1.3, 0, 
    19.8, 18.8, 3.6, 0, 3.6, 10.9, 25.7, 48.8, 1, 0.5, 15.2, 20.3, 5.6, 3.8,
    5.1, 3.8, 4.1, 17.8, 24.6, 11.2, 2.5, 1.5, 0, 0, 0, 14.5, 7.6, 16.5, 3, 
    9.1, 7.6, 19.1, 3.3, 2.3, 16, 2.3, 11.2, 10.9, 28.7, 23.6, 12.4, 3.3, 
    1.3, 12.7, 1.8, 10.9, 3.3, 1.3, 0, 0, 0, 0, 0, 0, 0, 5.6, 0, 0, 0, 1.5, 
    5.1, 1.5, 4.1, 11.9, 4.3, 23.6, 41.9, 3.6, 0.5, 1.3, 1.3, 3.3, 0.3, 0, 
    11.9, 4.1, 29.2, 13.7, 3.8, 0, 0, 0, 0, 0, 0, 3, 0, 0, 5.1, 1.3, 0, 0, 
    5.1, 2.3, 1.3, 1.5, 1.5, 0, 19.3, 3.8, 31.7, 1.8, 0, 1.5, 6.3, 5.6, 0, 
    0, 0, 0, 0, 1, 0.8, 0.3, 4.1, 26.9, 3.8, 1.5, 0.8, 0, 0.5, 4.8, 0.8, 0, 
    0, 0, 0, 0, 0, 0, 0, 4.6, 1.3, 0.8, 0, 31.2, 17.8, 13, 4.1, 1.5, 2.8, 0,
    3.3, 0, 1.3, 0, 14.2, 0, 1.3, 16.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 1.5, 0, 0, 0, 0, 0, 0, 8.6, 10.4, 14, 16, 4.3, 0, 3.6, 0,
    0, 0, 1, 0, 0, 2.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.3, 0.5, 18.5, 
    10.2, 1, 0.5, 0.5, 5.6, 11.2, 0.5, 0.8, 1.3, 0, 0, 0, 0, 0, 0, 22.9, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 14, 1, 2.3, 3, 0.5, 10.2, 15.2, 0.3, 0, 4.6, 
    1.5, 0, 18, 7.4, 18.5, 5.8, 0, 3.3, 6.6, 2.3, 0.5, 0.8, 0.3, 20.8, 2, 
    0.5, 10.7, 4.8, 0.8, 0, 1.8, 0, 0, 1.5, 2.3, 3.3, 11.2, 3, 1.3, 3.3, 
    10.2, 2.5, 0, 2, 0, 0.5, 0.3, 0, 5.1, 5.3, 14.2, 6.6, 7.6, 1.3, 15.2, 
    4.3, 4.8, 1.3, 2.5, 0.5, 10.2, 8.4, 0, 0, 10.2, 0, 0, 0, 0, 1.3, 0, 0.3,
    12.7, 0, 15.2, 5.1, 0.5, 3.8, 0.5, 3.8, 22.9, 0, 0, 0, 0, 19, 3.8, 29.7,
    26.4, 17.8, 18.5, 7.9, 6.3, 7.9, 2.3, 2.5, 19, 4.8, 1.8, 6.3, 0.5, 0, 0,
    0, 0, 0.5, 12.7, 0.5, 2.8, 3.3, 1.3, 11.4, 2.3, 0.8, 51.3, 4.3, 0, 0, 
    0.8, 17.8, 11.2, 4.3, 8.9, 4.3, 9.1, 0, 0, 10.9, 16.5, 5.8, 2, 10.7, 
    8.1, 5.1, 2, 18.3, 5.8, 2, 7.1, 2, 5.6, 2.8, 3.8, 5.8, 12.2, 6.3, 0.5, 
    2, 8.9, 20.3, 13, 8.4, 33.5, 8.1, 10.7, 1.5, 0, 0, 0, 0, 6.1, 0.8, 0, 0,
    0, 0.5, 0.3, 1.3, 12.7, 1.3, 1.5, 1.8, 0.8, 2.5, 19, 6.6, 16, 6.9, 8.1, 
    5.8, 2.5, 21.1, 10.2, 3.8, 0, 16, 3.3, 3.8, 5.1, 3.8, 1.5, 0, 0.5, 2.8, 
    0, 0, 0, 2.8, 8.9, 7.4, 1.5, 0.3, 1.3, 0, 2.5, 0, 28.4, 5.1, 28.7, 22.9,
    4.3, 2, 4.3, 8.9, 5.8, 3.8, 8.1, 6.9, 2.5, 3.3, 0, 2, 1, 0, 0, 0.5, 0, 
    0.3, 0, 0, 0, 0, 0, 0.3, 25.1, 3.3, 10.2, 15.2, 11.4, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0.3, 0, 0, 0, 0, 0, 0, 1, 0, 
    0.5, 5.3, 0, 5.1, 5.1, 2, 5.1, 16.5, 19.1, 1.3, 0, 0, 0, 7.6, 8.9, 5.1, 
    4.1, 0, 0, 0, 0, 1, 25.7, 18.5, 4.8, 4.6, 0.5, 10.2, 8.9, 2.3, 2.3, 0, 
    1, 1.3, 5.8, 2.3, 8.6, 8.9, 0, 4.3, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 8.6, 
    5.6, 7.9, 4.3, 10.2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1.3, 0.8, 0, 0,
    0, 0, 20.6, 0, 0, 0, 0, 0, 0, 1.3, 0, 0, 10.7, 24.1, 0.5, 14.2, 2, 0, 0,
    1, 0, 0, 0, 0.5, 1.5, 0, 2.3, 0, 6.4, 0, 0, 0, 0.8, 0, 0, 0, 7.6, 1.3, 
    0, 17.5, 1.5, 4.6, 37.6, 0, 1.5, 1, 1.3, 0.5, 1.8, 1.5, 1.3, 1.3, 0.5, 
    1.8, 0, 0, 0, 3.8, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.5, 1.8, 3.3, 0, 0, 
    5.1, 7.6, 0.3, 15.2, 16, 2.8, 5.1, 2.5, 0, 0, 0, 0, 0.5, 0.8, 0, 0, 0, 
    3.8, 10.7, 1.8, 19.1, 2.8, 4.3, 3.8, 15.7, 3.3, 12.7, 39.4, 2.5, 10.2, 
    18.3, 3.8, 0.3, 0, 0, 0, 1.3, 2.8, 14, 5.8, 0.5, 0.8, 4.8, 15.7, 16.3, 
    3.8, 16, 3.6, 0.3, 2.5, 0.8, 0, 1.3, 7.1, 2, 1.3, 0, 8.1, 1.8, 10.7, 
    1.5, 0.5, 5.1, 1.8, 1.3, 0, 0, 0, 0, 0, 0, 0, 0, 1.3, 0.5, 0, 0.8, 1, 0,
    0, 1.8, 5.6, 8.4, 5.3, 5.1, 3.8, 0.5, 1.3, 8.6, 3.8, 6.4, 11.4, 7.6, 
    9.4, 10.4, 3, 0.5, 0.3, 0, 0, 1.3, 0.5, 0, 0, 0, 0, 0, 0.3, 0.8, 0, 3.8,
    12.7, 0, 0, 0, 0, 0, 0, 0.3, 1.3, 1, 3.8, 21.6, 18.3, 11.7, 5.8, 3.8, 
    4.6, 39.4, 5.8, 1.3, 13.7, 0.8, 26.7, 8.6, 4.1, 3.8, 6.1, 1.3, 6.6, 1.5,
    0, 0, 0.5, 2.5, 0.5, 6.4, 1, 5.3, 1, 6.4, 3.8, 0, 1.3, 0, 1, 6.9, 0.3, 
    0.8, 0.3, 0.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 2.5, 5.1, 0, 0, 0, 0.8, 0, 
    0, 0, 4.6, 4.3, 3, 2.8, 1, 0.3, 0, 0, 15.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    5.1, 0, 0, 14.2, 2.3, 0.5, 1.5, 1.5, 12.7, 2, 9.7, 0, 0, 9.1, 1, 1.8, 
    3.8, 1.8, 0.5, 0, 0, 1.8, 0.8, 0.3, 4.3, 2.3, 13.7, 5.1, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 10.7, 7.1, 10.2, 2.5, 2.5, 12.7, 4.3, 0, 0, 0.3, 
    1.3, 3, 0.3, 1.5, 19.8, 0.5, 18.3, 0.8, 0, 0, 0, 0.8, 0, 2.5, 1, 1.3, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1.3, 0, 1.8, 1.3, 3.8, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.1, 2.5, 9.1, 0, 
    3.8, 7.9, 0, 0, 0, 12.4, 0.5, 3.3, 0.3, 3.3, 0.5, 0, 0.5, 0, 0, 0, 0, 2,
    0, 3.3, 3.8, 17.3, 0, 2.5, 16.5, 0.3, 0, 1.8, 2.5, 4.1, 1.5, 0, 7.4, 
    1.5, 9.4, 0, 0, 2.8, 3.8, 1.3, 0, 15.2, 0, 0.8, 8.4, 2.5, 0, 0, 0, 0, 0,
    0.5, 1.5, 10.2, 3.8, 2, 0, 0, 0.8, 6.4, 11.9, 0.8, 0, 1.3, 2.5, 5.1, 0, 
    0, 0, 0, 0, 0, 0, 0, 3.3, 4.3, 6.4, 5.1, 45.7, 10.2, 0, 0, 0, 1.5, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.1, 7.6, 0, 6.6, 
    0.3, 10.2, 6.4, 19.1, 10.2, 7.9, 0, 20.8, 0, 0.5, 1, 0.3, 1, 2.3, 0, 0, 
    0, 6.1, 0.8, 24.9, 10.7, 1.8, 0, 13.7, 0.3, 5.1, 2.5, 0.8, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 12.7, 5.1, 5.1, 13, 1.3, 3.8, 2, 3.3, 0, 2, 
    3, 7.6, 1.3, 0, 0, 0, 0, 0, 2.3, 1.3, 0, 1.8, 3.8, 5.1)


"venice"<-
structure(.Data = list(Year = c(1931, 1932, 1933, 1934, 1935, 1936, 1937, 1938, 
    1939, 1940, 1941, 1942, 1943, 1944, 1945, 1946, 1947, 1948, 1949, 1950, 
    1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962, 
    1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 
    1975, 1976, 1977, 1978, 1979, 1980, 1981), r1 = c(103, 78, 121, 116, 
    115, 147, 119, 114, 89, 102, 99, 91, 97, 106, 105, 136, 126, 132, 104, 
    117, 151, 116, 107, 112, 97, 95, 119, 124, 118, 145, 122, 114, 118, 107,
    110, 194, 138, 144, 138, 123, 122, 120, 114, 96, 125, 124, 120, 132, 
    166, 134, 138), r2 = c(99, 78, 113, 113, 107, 106, 107, 97, 86, 101, 98,
    91, 88, 95, 102, 104, 108, 126, 102, 96, 117, 104, 102, 100, 96, 91, 
    107, 114, 117, 126, 108, 110, 116, 104, 108, 127, 118, 132, 120, 122, 
    116, 118, 111, 95, 110, 122, 102, 114, 140, 114, 136), r3 = c(98, 74, 
    106, 91, 105, 93, 107, 85, 82, 98, 96, 87, 82, 94, 98, 103, 101, 119, 
    102, 91, 114, 103, 98, 95, 96, 90, 100, 113, 108, 123, 104, 108, 114, 
    104, 106, 126, 118, 123, 116, 119, 116, 113, 99, 95, 109, 114, 100, 110,
    131, 111, 130), r4 = c(96, 73, 105, 91, 101, 90, 106, 83, 81, 97, 95, 
    83, 79, 90, 88, 101, 99, 107, 101, 89, 109, 98, 98, 94, 95, 85, 98, 110,
    107, 116, 100, 107, 112, 103, 102, 104, 107, 114, 114, 110, 109, 111, 
    98, 93, 103, 109, 98, 107, 130, 109, 128), r5 = c(94, 73, 102, 91, 93, 
    87, 105, 82, 80, 96, 94, 83, 78, 89, 86, 100, 98, 101, 93, 88, 106, 91, 
    92, 94, 94, 85, 98, 108, 105, 114, 100, 106, 110, 102, 101, 103, 100, 
    112, 108, 105, 104, 96, 97, 92, 102, 108, 96, 105, 122, 107, 119), r6
     = c(89, 72, 89, 89, 91, 87, 102, 81, 80, 94, 94, 81, 78, 84, 84, 91, 
    98, 98, 92, 86, 104, 91, 89, 90, 92, 84, 97, 104, 102, 110, 95, 104, 
    109, 98, 101, 102, 96, 110, 106, 99, 101, 92, 97, 90, 101, 108, 96, 102,
    118, 106, 110), r7 = c(86, 71, 89, 88, NA, 87, 98, 79, 78, 94, 89, 78, 
    76, 84, 84, 88, 96, 92, 90, 86, 100, 90, 89, 86, 90, 82, 93, 104, 96, 
    108, 94, 99, 104, 92, 100, 102, 95, 108, 104, 99, 100, 91, 96, 90, 101, 
    104, 95, 100, 116, 104, 107), r8 = c(85, 70, 88, 88, NA, 84, 95, 76, 78,
    91, 88, 75, 76, 83, 80, 84, 96, 86, 88, 86, 99, 90, 89, 82, 90, 82, 92, 
    102, 96, 108, 93, 99, 103, 90, 98, 99, 93, 107, 103, 97, 100, 91, 92, 
    89, 97, 104, 94, 100, 115, 103, 104), r9 = c(84, 70, 86, 86, NA, 82, 94,
    74, 77, 90, 87, 75, 74, 82, 80, 83, 94, 85, 88, 86, 99, 87, 84, 81, 90, 
    82, 91, 100, 94, 107, 92, 98, 103, 90, 98, 98, 92, 107, 103, 96, 99, 90,
    90, 88, 90, 102, 91, 100, 115, 102, 104), r10 = c(79, 69, 85, 81, NA, 
    81, 94, 69, 77, 89, 86, 73, 71, 80, 80, 82, 93, 83, 86, 85, 99, 86, 84, 
    81, 88, 80, 90, 99, 94, 106, 91, 95, 102, 90, 98, 97, 92, 106, 103, 96, 
    98, 89, 90, 88, 89, 100, 90, 99, 112, 99, 104)), class = "data.frame", 
    row.names = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", 
    "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
    "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
    "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", 
    "48", "49", "50", "51"))

"wavesurge"<-
structure(.Data = list(wave = c(1.5, 1.83, 2.44, 1.68, 1.49, 1.2, 1.35, 1.15, 
    1.2, 1.07, 1.72, 1.18, 0.97, 2.12, 2.26, 1.1, 1.31, 1.53, 2.51, 2.07, 
    1.6, 1.17, 1.45, 0.7, 2.01, 2.28, 1.95, 1.32, 0.88, 0.48, 0.78, 0.83, 
    1.14, 1.91, 1.48, 2.01, 1.73, 2.76, 2.94, 2.2, 1.93, 1.74, 1.44, 1.05, 
    1.16, 2.72, 2.67, 1.72, 2.39, 1.83, 2.35, 2.16, 2.18, 2.47, 4.37, 3.04, 
    3.56, 3.32, 2.59, 2.07, 1.91, 1.46, 2.46, 1.97, 4.23, 3.37, 1.91, 2.43, 
    2.4, 3.73, 2.94, 2.4, 2.63, 1.58, 0.94, 1.49, 2.01, 2.61, 1.59, 1.42, 
    1.39, 1.51, 1.88, 2.24, 2.42, 2, 1.78, 1.15, 1.41, 1.41, 1.21, 2.24, 
    2.64, 3.17, 2.47, 2.54, 2.21, 2.8, 2.65, 3.91, 4.18, 3.14, 2.02, 2.16, 
    2.64, 1.84, 1.85, 1.82, 1.95, 2.12, 1.69, 1.77, 1.94, 1.78, 1.37, 2.65, 
    2.4, 2.13, 1.68, 1.01, 1.08, 1.13, 1.19, 1.15, 1.29, 1.44, 1.45, 1.57, 
    1.32, 1.02, 1.27, 1.25, 1.22, 1.19, 1.2, 2.23, 1.99, 1.79, 1.64, 4.38, 
    4.93, 3.29, 1.55, 1.47, 1.94, 2.3, 2.51, 2.24, 1.58, 1.37, 3.16, 2.37, 
    2.04, 2.14, 2.39, 1.83, 1.83, 2.18, 1.79, 1.76, 1.07, 1.41, 2.56, 3.04, 
    2.26, 2.45, 1.79, 3.44, 2.3, 3.61, 4.42, 3.05, 3.59, 4.72, 5.38, 6.38, 
    5.650000000000001, 4.99, 3.52, 4.68, 3.38, 3.74, 3.8, 3.03, 2.18, 1.88, 
    1.34, 2.5, 2.35, 1.92, 2.2, 2.17, 3.36, 2.84, 3, 2.52, 1.74, 3.14, 2.64,
    3.44, 2.85, 2.19, 3.22, 3.85, 3.72, 5.2, 2.86, 3.75, 5.82, 4.99, 4.44, 
    3.6, 1.4, 1.24, 1.74, 2.05, 4.21, 3.01, 1.76, 1.5, 2.51, 2.7, 2.4, 3.61,
    4.13, 5.24, 3.01, 4.08, 6.75, 7.28, 5.89, 4.93, 4.63, 3.22, 2.1, 1.9, 
    1.57, 1.06, 1.87, 3.44, 4.400000000000001, 4.23, 4.96, 
    5.150000000000001, 5.27, 3.08, 4.55, 3.87, 3.33, 2.15, 1.9, 1.86, 3.05, 
    2.78, 2.35, 1.84, 1.5, 1.43, 2.41, 1.86, 1.71, 1.39, 1.8, 1.82, 2.97, 
    3.58, 3.52, 5.650000000000001, 4.5, 4.45, 4.22, 3.56, 4.400000000000001,
    5.36, 8.1, 7.44, 4.66, 4.7, 3.69, 3.6, 3.12, 3.51, 4.43, 4.63, 5.3, 
    4.79, 4, 2.9, 3.11, 3.74, 4.26, 4.82, 4.38, 3.33, 2.34, 2.02, 1.34, 1.3,
    1.14, 2.02, 2.39, 1.72, 2.33, 4.03, 3.58, 4.72, 6.26, 5.43, 4, 6.59, 
    7.7, 5.97, 6.16, 4.76, 4.46, 5.55, 6.1, 5.27, 4.74, 8.27, 6.95, 4.76, 
    4.650000000000001, 4.24, 3.57, 3.65, 3.66, 2.84, 3.02, 4.19, 4.91, 5.33,
    5.85, 4.97, 7.91, 7.88, 6.47, 5.17, 3.72, 3.1, 2.91, 2.01, 5.88, 6.94, 
    6.94, 8.449999999999999, 7.46, 5.53, 5.03, 5.28, 4.33, 3.58, 2.76, 2.42,
    2.66, 4.78, 4.05, 4.41, 3.6, 5.44, 7.96, 7.74, 5.7, 3.97, 3.36, 5.86, 
    6.84, 5.71, 4.6, 2.83, 5.22, 3.68, 1.76, 1.44, 1.48, 2.9, 2.97, 2.49, 
    2.35, 2.94, 2.83, 2.69, 1.79, 1.45, 4.04, 4.150000000000001, 5.18, 5.82,
    6.12, 7.03, 5.82, 4.6, 5.57, 8.07, 6.89, 5.17, 4.57, 9.47, 9.89, 7.23, 
    5.02, 2.76, 1.91, 3.28, 3.64, 4.77, 3.98, 4.35, 3.37, 2.13, 1.17, 1.23, 
    1.51, 1.16, 1.88, 2.78, 2.79, 2.27, 2.36, 1.97, 2.16, 1.69, 1.69, 1.35, 
    1.14, 2.09, 2.23, 1.96, 4.33, 4.79, 6.67, 6.5, 4.97, 3.12, 2.7, 3.43, 
    2.04, 2.53, 2.83, 3.1, 3.9, 2.83, 3.35, 5.76, 7.16, 4.900000000000001, 
    4.41, 5.26, 6.98, 5.87, 3.61, 3.29, 4.8, 8.279999999999999, 6.64, 4.99, 
    2.09, 1.93, 3.36, 4.44, 2.46, 2.3, 1.65, 1.68, 1.77, 2.73, 2.84, 2.51, 
    2.13, 2.02, 1.87, 2.38, 3.12, 2.55, 2.6, 2.37, 2.65, 2.09, 1.34, 2.02, 
    1.91, 1.65, 3.89, 4.87, 3.94, 3.52, 2.5, 2.18, 1.44, 2.95, 2.87, 2.77, 
    1.86, 1.86, 1.79, 2.7, 2.27, 3.1, 2.76, 4.44, 4.03, 3.36, 4.01, 
    5.400000000000001, 4.73, 3.86, 3.28, 2.84, 2.54, 2.17, 1.96, 0.77, 1.04,
    1.39, 2.12, 2.44, 2.01, 2.27, 3.62, 3.59, 3.94, 4.11, 3.5, 3.93, 6.27, 
    5.900000000000001, 6.29, 7.58, 5.5, 3.11, 4.77, 5, 4.21, 3.08, 1.98, 
    1.62, 1.11, 2.1, 2.78, 2.79, 2.39, 1.82, 2.15, 2.21, 1.72, 2.32, 2.47, 
    2.58, 2.51, 2.49, 2.13, 1.44, 1.2, 1.84, 2.65, 2.38, 1.31, 1.24, 0.86, 
    0.95, 0.73, 1.43, 2.73, 3.69, 2.82, 2.92, 2.61, 4.05, 4.6, 3.33, 3.05, 
    2.32, 2.43, 1.69, 2.16, 2.7, 2.65, 2.17, 2.32, 2.11, 1.44, 1.22, 1.67, 
    3.16, 3.26, 3.22, 3.65, 2.45, 2.48, 2.84, 1.95, 1.3, 1.5, 2.22, 1.56, 
    1.67, 1.97, 2.45, 2.45, 2.02, 1.64, 1.28, 1.12, 1.03, 0.85, 1.28, 1.02, 
    0.85, 1.16, 0.89, 1.08, 0.67, 0.63, 0.41, 0.51, 0.44, 0.45, 0.32, 0.78, 
    0.95, 0.75, 0.6899999999999999, 0.57, 0.62, 0.71, 1.1, 1.49, 1.55, 2.72,
    4.14, 4.8, 3.65, 2.41, 1.37, 0.75, 2.65, 1.83, 1.98, 1.95, 1.9, 2.4, 
    3.83, 3.55, 2.97, 1.78, 1.34, 1.05, 0.87, 3.33, 3.55, 2.41, 2.1, 1.23, 
    1.51, 1.2, 1.21, 2.27, 1.97, 1.62, 1.16, 1.02, 0.91, 1.35, 1.3, 1.08, 
    0.8100000000000001, 0.54, 0.43, 0.73, 1.1, 2.36, 2.01, 1.55, 1.29, 0.72,
    0.67, 1.31, 1.94, 1.82, 1.57, 1.4, 1.17, 1.05, 0.92, 1.52, 1.95, 1.42, 
    0.83, 0.59, 1.04, 1.26, 1.43, 3.25, 3.22, 1.77, 1.19, 1.19, 1.14, 0.83, 
    1.46, 1.94, 1.69, 1.14, 0.8, 0.75, 0.73, 0.9, 0.87, 2.24, 1.86, 1.4, 
    1.11, 0.8, 2.28, 2.4, 2.19, 2.92, 2.08, 1.94, 1.54, 1.38, 1.41, 2.03, 
    1.98, 1.53, 1.41, 1.88, 2.33, 3.3, 2.42, 1.96, 1.62, 1.57, 2.32, 3.02, 
    2.77, 1.74, 1.93, 1.2, 1.58, 2.06, 2.05, 3.09, 3.19, 2.68, 2.73, 1.56, 
    2.07, 2.18, 2.02, 2.18, 2.6, 2.21, 2.05, 2.16, 2.27, 3, 2.34, 2.37, 
    2.73, 1.75, 1.98, 2.27, 1.91, 1.69, 1.62, 1.24, 1.2, 3.22, 2.74, 4.14, 
    6.08, 3.67, 5.07, 4.82, 3.07, 2.12, 2.15, 1.95, 2.38, 2.09, 2.99, 2.35, 
    2.78, 2.09, 1.91, 1.83, 3.27, 3.51, 2.88, 2.67, 5.23, 4.07, 4.91, 4.22, 
    5.1, 7.19, 5.1, 2.66, 2.09, 1.64, 3.58, 2.66, 2.99, 2.6, 5.3, 4.32, 5.1,
    7.58, 6.25, 4.83, 3.05, 2.35, 2.87, 2.13, 1.24, 0.82, 1.56, 1.34, 1.94, 
    4.11, 5.54, 5.72, 4.24, 4.29, 5.93, 6.7, 5.47, 4.13, 4.63, 5.33, 6.31, 
    7.64, 6.71, 5.99, 7.22, 4.84, 5.62, 4.17, 3.57, 4.14, 5.54, 6.6, 5.95, 
    4.84, 6.7, 5.57, 4.31, 4, 3.69, 5.17, 4.8, 3.78, 4.33, 3.56, 2.37, 2.28,
    3.54, 3.52, 3.05, 2.65, 2.12, 2.53, 3.22, 4.47, 6.1, 5.150000000000001, 
    7.04, 5.2, 7.46, 7.54, 2.4, 2.37, 3.5, 3.03, 3.44, 2.85, 2.83, 2.59, 
    2.44, 1.78, 1.84, 1.95, 2.53, 1.97, 1.2, 1.34, 1.21, 1.23, 2.26, 2.65, 
    3.61, 3.49, 2.77, 4.04, 3.49, 7.92, 6.33, 5.84, 4.47, 3.7, 3.15, 2.69, 
    6.27, 7.1, 4.07, 4.24, 3.57, 3.18, 3.33, 2.82, 3.59, 3.3, 3.23, 3.21, 
    3.76, 4.01, 4.53, 3.33, 2.76, 2.71, 2.77, 3.91, 3.83, 3.5, 2.49, 1.95, 
    2, 1.55, 2.57, 3.11, 2.9, 2.33, 2.4, 2.64, 2.89, 3.85, 3.38, 5.25, 4.49,
    3.82, 4.150000000000001, 8.300000000000001, 8.390000000000001, 7.84, 
    5.96, 4.87, 4.88, 3.54, 2.2, 1.24, 2.15, 1.92, 1.94, 1.92, 1.62, 1.68, 
    3.01, 3.55, 4.5, 4.97, 3.22, 3.35, 3.64, 4.400000000000001, 3.22, 2.78, 
    2.67, 2.89, 2.17, 2.77, 3.65, 4.84, 4.23, 3.47, 2.76, 2.56, 3.58, 4.71, 
    2.28, 1.44, 1.98, 1.93, 2.05, 2.23, 2.15, 1.67, 1.01, 1.09, 1.36, 1.56, 
    1.49, 1.51, 1.13, 1.01, 1.33, 1.1, 0.92, 0.73, 0.5600000000000001, 0.71,
    1.28, 1.2, 0.89, 0.95, 2.03, 3.96, 6.14, 4.95, 3.22, 1.93, 3.4, 3.2, 
    3.33, 2.1, 2.5, 3.58, 3.2, 3.4, 3.5, 5.48, 5.8, 3.37, 2.57, 3.78, 3.39, 
    2.83, 4.08, 1.58, 1.24, 2.92, 2.22, 2.27, 2.13, 1.43, 1.42, 1.23, 1, 
    1.55, 1.69, 2.15, 1.32, 2.26, 2.63, 2.62, 1.83, 2.57, 2.79, 3.64, 3.43, 
    1.76, 1.95, 1.51, 1.34, 0.83, 1.65, 1.05, 1.51, 1.54, 1.41, 1.46, 2.61, 
    2.68, 1.64, 2.98, 2.48, 2.97, 5.86, 3.05, 3.07, 3.49, 4.03, 3.47, 2.95, 
    2.54, 2.23, 3.04, 2.82, 2.33, 1.63, 1.11, 1.8, 1.64, 1.64, 1.51, 1.79, 
    3.84, 4, 3.49, 1.41, 1.42, 1.29, 1.69, 1.96, 2.58, 2.12, 1.82, 1.63, 
    1.76, 1.36, 0.9, 1.05, 1.58, 1.04, 1.1, 0.88, 1.28, 1.44, 2.34, 2.07, 
    3.37, 1.99, 1.67, 2.58, 3.17, 3.45, 2.05, 1.62, 1.01, 1.06, 1.3, 1.19, 
    1.31, 1.17, 1.77, 2.52, 2.05, 1.83, 1.35, 1.42, 2.07, 2.27, 1.84, 1.27, 
    1.36, 2.15, 2.01, 1.91, 2.08, 1.73, 2.06, 4.27, 3.73, 3.02, 2.33, 2.52, 
    2.68, 2.58, 1.8, 1.6, 1.27, 1.48, 1.12, 1.51, 1.78, 1.65, 1.8, 1.92, 
    2.05, 3.93, 3.18, 2.34, 1.69, 1.65, 1.39, 1.34, 1.04, 1.17, 2.19, 2.01, 
    1.54, 1.5, 0.91, 1.18, 1.18, 1.75, 2.14, 1.73, 1.52, 1.19, 2.05, 1.62, 
    1.39, 1.19, 1.94, 1.54, 1.94, 1.52, 2.68, 2.72, 3.12, 2.71, 2.41, 1.94, 
    1.21, 1.14, 1.18, 2.14, 2.45, 1.74, 1.73, 1.09, 1.48, 1.34, 1.23, 1.07, 
    0.88, 0.66, 0.67, 1.23, 1.69, 1.34, 1.8, 2.4, 3.16, 4.17, 
    4.150000000000001, 5.04, 3.92, 4.62, 2.94, 2.42, 2.66, 2.76, 2.27, 1.5, 
    1.36, 1.03, 1.1, 1.09, 0.79, 0.97, 1.12, 0.99, 1.35, 1.55, 1.4, 0.85, 
    0.66, 0.94, 1.02, 0.88, 1.32, 1.66, 1.28, 1.48, 1.36, 0.86, 0.98, 1.34, 
    1.38, 2.39, 2.6, 1.92, 3.12, 3.15, 2.69, 2.18, 3.02, 3.55, 4.5, 2.35, 
    2.05, 1.51, 1.56, 1.05, 0.92, 1.43, 1.83, 1.81, 1.73, 1.62, 1.74, 0.99, 
    1.86, 2.06, 1.67, 1.72, 2.29, 2.09, 2.69, 1.71, 2.66, 2.28, 2.29, 4.06, 
    5.35, 4.3, 2.92, 2.15, 1.84, 2.79, 3.08, 3.9, 3.5, 1.98, 4.01, 3.63, 
    2.76, 3.04, 2.7, 5.01, 4.35, 5.86, 5.89, 5.16, 3.03, 1.8, 1.48, 2.28, 
    2.8, 2.26, 2.22, 2.31, 2.51, 1.93, 1.12, 1, 1.83, 1.76, 1.96, 1.22, 
    1.69, 2.64, 3.87, 4.76, 3.74, 2.05, 2.12, 2.66, 3.22, 4.45, 2.5, 1.72, 
    1.16, 3.08, 2.19, 2.38, 2.71, 2.09, 1.33, 1.93, 2.28, 2.26, 1.18, 2.95, 
    2.05, 1.88, 1.65, 1.8, 2.03, 1.86, 1.16, 1.49, 2.34, 2.27, 2, 2.25, 
    1.91, 2.15, 1.95, 2.27, 4.5, 3.81, 2.45, 1.72, 1.56, 3.09, 3.04, 3.72, 
    3.45, 3.13, 2.93, 2.44, 4.45, 4.33, 4.18, 4, 4.45, 4.03, 2.93, 2.53, 
    1.27, 2.35, 1.92, 1.42, 1.19, 1.17, 1.26, 1.69, 1.31, 1.51, 1.48, 2, 
    2.05, 2.17, 2.2, 1.9, 2.01, 4.01, 5.86, 4.12, 1.98, 2.81, 5.03, 3.9, 
    1.82, 2.75, 2.81, 1.89, 1.36, 1.97, 2.19, 3.84, 7.05, 6.84, 3.54, 1.74, 
    2.72, 3.78, 2.58, 2.53, 4.86, 4.75, 4.400000000000001, 4.08, 2.48, 2.41,
    2.87, 3.14, 2.78, 2.57, 5.62, 5.39, 3.8, 2.97, 4.55, 5.7, 4.61, 5.76, 
    4.2, 3.36, 2.08, 1.58, 2.16, 2.3, 3.82, 2.98, 2.89, 5.150000000000001, 
    4.17, 3.47, 3.44, 1.65, 3.51, 5.13, 6.81, 4.93, 5.89, 7.37, 6.21, 6.21, 
    5.59, 6.29, 7.1, 6.03, 4.64, 4.53, 5.77, 8.51, 8.279999999999999, 8.09, 
    5.04, 5.08, 4.38, 4.85, 4.91, 9.26, 10.43, 4.66, 5.85, 5.16, 4.79, 4.58,
    5.23, 3.53, 4.77, 3.73, 5.32, 5.650000000000001, 4.5, 2.85, 4.19, 5.49, 
    6.82, 6.79, 6.400000000000001, 4.67, 6.3, 8.050000000000001, 7.14, 5.82,
    6.61, 8.34, 7.01, 4.03, 3.03, 3.78, 3.98, 5.19, 7.17, 8.029999999999999,
    7.44, 4.12, 3.63, 4.46, 6.38, 7.67, 9.31, 8.44, 5.71, 4.67, 4.94, 4.63, 
    5.22, 7.79, 6.42, 5.94, 5.03, 2.72, 2.45, 4.59, 4.06, 3.58, 2.26, 1.98, 
    2.6, 3.15, 2.85, 2.08, 1.39, 1.29, 1.3, 1.97, 1.66, 1.46, 4.07, 5.26, 
    5.38, 2.83, 2.69, 2.85, 2.31, 2.09, 1.19, 2.78, 2.8, 3.22, 2.89, 3.25, 
    2.67, 2.46, 2.09, 2.03, 2.72, 1.83, 2.15, 2.14, 1.57, 3.63, 4.1, 5.27, 
    4.46, 4.650000000000001, 5, 5.02, 6.12, 5.1, 4.150000000000001, 2.18, 
    1.53, 2.76, 3.78, 3.2, 3.04, 2.37, 1.7, 2.41, 2.88, 2.15, 1.94, 1.33, 
    1.2, 1.34, 1.97, 2.71, 3.08, 2.89, 4.13, 3.84, 3.05, 3.51, 1.9, 1.8, 
    1.66, 1.94, 1.53, 1.69, 1.99, 1.88, 1.24, 0.79, 0.75, 1.55, 1.95, 2.35, 
    2.26, 2.08, 2.05, 1.36, 1.03, 0.79, 0.7, 1.46, 2.16, 1.91, 1.47, 1.55, 
    1.66, 1.47, 1.67, 1.16, 1.45, 1.39, 0.84, 1.56, 2.08, 2.13, 2.08, 2.39, 
    2.21, 2.16, 1.34, 2.3, 4.1, 4.53, 2.53, 3.73, 3.38, 2.8, 2.35, 1.82, 
    2.04, 1.39, 0.85, 1.04, 3.29, 3.4, 4.81, 6.07, 4.23, 4.19, 4.29, 4.25, 
    3.39, 3.22, 2.22, 2.04, 2.94, 1.63, 1.83, 2.31, 2.44, 1.97, 1.78, 2.76, 
    4.5, 3.25, 3.43, 4.36, 4.2, 3.54, 3.34, 2.09, 1.23, 1.12, 1.03, 0.77, 
    2.26, 1.87, 1.52, 1.74, 1.69, 1.8, 1.49, 2.35, 3.35, 2.53, 2.2, 1.75, 
    1.47, 3.15, 4.66, 4.150000000000001, 2.51, 1.95, 1.62, 2.23, 2.94, 2.78,
    2.69, 1.49, 1.01, 1.18, 2.2, 2.21, 1.71, 1.58, 1.35, 1.48, 2.12, 2.88, 
    1.81, 1.68, 1.46, 1.28, 1.68, 2.75, 2.49, 1.41, 1.57, 1.03, 2.58, 2.63, 
    1.17, 1.23, 1.52, 1.14, 1.03, 1.66, 1.76, 2.23, 1.84, 2.72, 2.75, 2.74, 
    3.33, 2.9, 2.39, 1.39, 2.13, 1.83, 3.22, 4.39, 4.02, 3.9, 1.86, 2.59, 
    2.71, 1.53, 1.71, 1.37, 2.9, 3.39, 4.02, 6.58, 3.98, 2.86, 2.57, 3.07, 
    4.86, 4.36, 3.11, 3.01, 3.09, 2.65, 2.14, 1.87, 1.98, 1.76, 1.23, 1.04, 
    0.94, 1.35, 2.45, 1.28, 2.68, 3.58, 3.4, 3.06, 3.71, 2.93, 2.08, 1.91, 
    1.2, 1.34, 1.77, 2.63, 3.26, 2.75, 3.68, 3.77, 2.91, 2.12, 2.29, 1.85, 
    1.55, 1.86, 2.09, 2.04, 3.69, 3.01, 2.08, 1.64, 1.6, 1.85, 1.24, 0.99, 
    0.91, 1.69, 1.59, 1.55, 1.76, 2.1, 2.29, 2.3, 1.74, 1.2, 1.59, 1.03, 
    1.55, 2.05, 2.11, 1.85, 1.33, 1.17, 0.88, 1.12, 1.85, 1.77, 1.42, 2.95, 
    3.1, 2.11, 1.03, 1.7, 1.48, 2.13, 2.2, 1.79, 1.99, 2.01, 1.69, 2.42, 
    2.48, 2.06, 1.58, 1.58, 1.57, 1.49, 1.87, 2.01, 1.89, 1.92, 2.13, 2.43, 
    2.52, 1.86, 3.25, 2.99, 2.31, 1.91, 0.91, 0.89, 0.97, 1.37, 1.84, 2.06, 
    1.56, 1.26, 2.53, 2.59, 1.94, 1.95, 1.77, 1.61, 0.65, 1, 1.04, 1.21, 
    1.01, 1.23, 1.62, 1.12, 0.8100000000000001, 0.86, 1.48, 1.76, 1.75, 
    2.85, 1.66, 2.34, 2.03, 2.53, 2.08, 3.22, 2.12, 1.98, 2.53, 4.27, 3.56, 
    2.25, 1.75, 1.14, 1.18, 1.6, 2.18, 1.97, 2.21, 2.26, 2.73, 3.28, 4.01, 
    3.94, 3.24, 3.44, 1.86, 1.07, 0.89, 0.82, 0.87, 0.8, 0.74, 1.12, 1.13, 
    1.9, 1.89, 1.29, 1.28, 1.08, 0.79, 1.59, 1.48, 0.86, 1.78, 4.03, 3.03, 
    2.63, 2.4, 2.73, 3.19, 2.51, 1.28, 1.13, 1.6, 1.99, 1.76, 1.22, 1.24, 
    2.44, 2.51, 2.5, 2.31, 2.04, 2.03, 2.17, 3.05, 2.18, 2.53, 2.37, 2.33, 
    2.21, 1.4, 2.08, 1.58, 1.32, 1.11, 2.11, 2.63, 2.21, 1.71, 1.28, 2.37, 
    2.53, 3.23, 2.62, 1.54, 1.64, 1.6, 1.69, 1.35, 1.57, 1.62, 1.66, 1.61, 
    1.51, 1.27, 1.23, 2.03, 2.21, 3.1, 2.44, 3.65, 5.82, 6.02, 2.81, 5.5, 
    4.12, 3.26, 2.43, 1.58, 1.01, 1.52, 1.68, 2.16, 1.98, 2.03, 2.89, 2.55, 
    2.21, 1.68, 3.19, 2.89, 2.54, 4.61, 3.94, 3.5, 3.09, 7.52, 3.96, 2.46, 
    3.76, 3.95, 3.23, 3.34, 3.29, 4.26, 5.41, 2.51, 2.68, 2.51, 3.18, 3.67, 
    3.05, 2.1, 1.44, 1.14, 1.38, 1.76, 1.92, 2.52, 2.51, 2.33, 2.49, 2.64, 
    3.44, 4.09, 4.59, 3.73, 3.9, 1.73, 1.23, 1.33, 1.92, 3.47, 3.91, 3.31, 
    3.47, 4.16, 3.56, 3.84, 2.67, 2.86, 2.87, 2.3, 2.29, 1.7, 1.69, 1.49, 
    2.72, 3.15, 3.02, 2.74, 4.66, 4.650000000000001, 3.33, 3.96, 5.33, 4.07,
    3.36, 3.46, 3.66, 2.27, 2.08, 2.39, 2.99, 2.49, 2.12, 2.03, 2.66, 4.66, 
    2.99, 2.16, 2.07, 1.29, 1.75, 4.6, 6.49, 6.84, 6.95, 4.27, 2.05, 3.48, 
    3.83, 1.95, 1.31, 0.9, 2.8, 5.150000000000001, 5.36, 4.01, 3.78, 3.95, 
    5.43, 5.52, 5.46, 3.88, 3.68, 3.59, 3.5, 4.32, 5.7, 6.85, 6.57, 5.77, 
    2.59, 1.57, 1.42, 1.3, 1.25, 1.05, 1.2, 1.3, 1.37, 1.24, 1.37, 1.79, 
    1.97, 2.72, 5.22, 3.82, 2.69, 1.7, 1.89, 1.63, 2.01, 2.49, 3.94, 2.46, 
    1.32, 0.97, 0.79, 0.61, 1.5, 2.03, 2.36, 3.11, 3.3, 2.7, 3.66, 3.22, 
    2.37, 1.64, 1.91, 2.55, 2.08, 2.4, 2.54, 3.74, 3.95, 3.68, 4.79, 
    6.650000000000001, 6.62, 5.72, 2.33, 2.51, 2.03, 2.42, 3.08, 3.18, 3.69,
    4.14, 3.19, 3.1, 4.84, 4.94, 4.06, 3.51, 2.93, 2.51, 2.21, 2.25, 3.59, 
    2.8, 2.93, 2.43, 1.75, 2.4, 4.45, 5, 5.77, 6.09, 4.72, 4.37, 4.97, 5.41,
    5.71, 4.54, 3.93, 2.81, 3.18, 2.29, 3.73, 4.36, 8.02, 7.82, 6.55, 4.46, 
    2.55, 1.52, 2.23, 3.4, 3.93, 2.4, 3.64, 2.84, 1.72, 2.12, 2.9, 6.61, 
    5.42, 5.78, 4.07, 5.28, 6.47, 4.94, 6.13, 4.93, 3.96, 3.29, 2.43, 2.84, 
    2.44, 3, 2.87, 2.56, 2.09, 2.35, 2.96, 2.87, 3.73, 4.11, 4.45, 4.26, 
    3.64, 2.95, 2.35, 3.71, 3.75, 3.75, 2.65, 2.15, 1.98, 1.52, 1.82, 1.57, 
    2.43, 2.33, 1.77, 2.69, 3.11, 3.15, 3.77, 4.11, 4.01, 3.29, 2.36, 1.73, 
    2.01, 3.4, 5.51, 4.400000000000001, 11.05, 9.97, 7.26, 4.36, 3.39, 6.2, 
    5.17, 3.84, 3.49, 3.2, 2.29, 2.29, 3.28, 3.15, 3.84, 5.76, 5.48, 3.6, 
    2.78, 3.4, 2.9, 4.11, 4.02, 3.81, 3.58, 3.55, 3.36, 3.15, 3.13, 2.67, 
    2.52, 2.59, 2.09, 1.99, 3.2, 2.69, 2.96, 3.25, 2.37, 2.08, 1.81, 1.98, 
    2, 2.02, 1.23, 1.08, 0.95, 0.9, 2.46, 2.69, 3.35, 2.3, 3.27, 4.94, 5.19,
    3.95, 1.83, 1.39, 1.3, 1.14, 1.5, 2.03, 1.78, 1.94, 2.87, 2.54, 2.69, 
    2.55, 1.67, 1.16, 1.75, 2.03, 1.94, 1.63, 1.92, 2.15, 1.6, 0.96, 
    0.8100000000000001, 0.8, 2.27, 2.4, 2, 3.44, 2.9, 1.66, 1.41, 1.33, 1.1,
    0.65, 0.99, 1.45, 2.76, 2.96, 3.04, 3.25, 3.29, 4.13, 4.150000000000001,
    2.97, 1.83, 2.74, 3.37, 3.75, 3.69, 4.37, 2.75, 2.44, 2.95, 3.1, 3.72, 
    2.87, 1.86, 2.36, 2.54, 1.78, 2.06, 2.33, 2.74, 2.42, 1.93, 1.33, 2.99, 
    2.51, 2.49, 2.47, 2.44, 2.43, 3.11, 2.88, 1.98, 1.34, 1.41, 3.03, 2.5, 
    1.79, 1.86, 1.86, 2.13, 1.53, 0.9, 1.29, 2.14, 2.7, 3.37, 2.26, 1.97, 
    1.71, 1.6, 1.58, 3.32, 2.71, 1.51, 3.71, 3.81, 3.79, 2.7, 2.73, 3.77, 
    3.45, 2.65, 1.69, 1.39, 1.55, 1.36, 1.15, 1.74, 2.44, 1.56, 1.41, 1.36, 
    0.86, 1.6, 1.4, 1.26, 1.61, 1.84, 1.44, 1.32, 1.05, 0.65, 0.6, 0.51, 
    0.57, 1.42, 1.64, 1.72, 1.09, 1.1, 1.78, 2.87, 3.06, 2.98, 2.98, 2.55, 
    2.75, 2.57, 3.77, 2.4, 2.58, 2.62, 1.98, 1.78, 1.33, 2.4, 2.47, 2.49, 
    2.03, 2.15, 1.75, 1.16, 0.94, 0.93, 0.98, 1.24, 1.17, 1.18, 1.19, 1.26, 
    1.26, 1.01, 1.75, 0.9, 0.98, 1.3, 1.37, 1.68, 1.12, 1.69, 0.99, 1.02, 
    0.82, 0.77, 0.84, 0.92, 0.86, 1.47, 1.53, 1.11, 0.8, 1.14, 0.98, 1.17, 
    1.39, 1.38, 1.22, 1.23, 1.03, 0.79, 0.85, 0.74, 0.67, 0.71, 0.59, 0.87, 
    1.18, 1.45, 1.7, 2.37, 2.02, 2.04, 1.74, 1.3, 1.3, 1.07, 0.88, 0.78, 
    1.05, 0.72, 1.34, 2.02, 1.53, 0.73, 2.39, 2.56, 1.66, 0.91, 2.04, 2.9, 
    2.27, 1.12, 1, 0.94, 0.62, 0.7, 0.75, 0.84, 0.77, 3.22, 4.64, 3.96, 
    4.59, 4.07, 6.43, 4.66, 3.31, 1.17, 3.85, 4.82, 2.65, 1.4, 0.85, 1.74, 
    2.65, 3.19, 2.9, 2.1, 2.25, 2.66, 2.58, 2.67, 3.1, 2.83, 2.12, 1.86, 
    2.25, 2.38, 2.67, 2.6, 2.62, 1.81, 2.18, 1.46, 1.47, 1.8, 1.68, 1.65, 
    3.89, 3.6, 3.68, 4.81, 4.99, 5.61, 6.23, 5.12, 2.44, 2.37, 1.87, 1.36, 
    1.89, 3.88, 8.220000000000001, 3.69, 2.45, 5.44, 8.369999999999999, 5.7,
    3.65, 3.51, 4.09, 3.5, 5.22, 4.46, 3.15, 3.52, 2.17, 3.38, 4.04, 3.58, 
    3.16, 2.52, 2.57, 2.4, 1.95, 2.55, 3.92, 6.37, 6.400000000000001, 4.93, 
    3.7, 4.03, 4.27, 4.29, 5.43, 5.650000000000001, 5.36, 4.29, 3.13, 2.79, 
    1.47, 2.44, 2.65, 2.3, 2.64, 2.87, 2.52, 3.19, 3.51, 3.15, 4.55, 3.42, 
    1.89, 2.66, 2.94, 2.08, 1.85, 1.83, 1.91, 2.27, 2.2, 2.41, 2.55, 1.76, 
    1.75, 2.16, 3.7, 4.06, 4.21, 5.13, 6.03, 6.78, 5.25, 6.54, 5.72, 9.37, 
    8.35, 7.03, 5.56, 5.650000000000001, 5.13, 5.07, 7.49, 7.84, 6.92, 6.07,
    4.67, 2.42, 1.6, 1.51, 1.6, 2.96, 3.49, 4.04, 3.73, 3.84, 3.79, 2.93, 
    2.1, 2.5, 2.51, 3.45, 2.49, 2.45, 1.94, 2.17, 1.56, 2.81, 3.6, 3.38, 
    4.29, 3.98, 3.78, 3.17, 2.23, 1.22, 1.07, 4.03, 2.51, 3.21, 5.47, 4.44, 
    3.21, 2.86, 4.650000000000001, 3.57, 2.76, 2.57, 3.72, 4.12, 3.47, 4.46,
    3.43, 3, 2.73, 2.21, 3.76, 4.48, 4.44, 3.71, 2.79, 2.06, 4.1, 6.61, 
    6.77, 6.67, 5.72, 4.17, 1.82, 4.74, 5.400000000000001, 3.08, 4.86, 4.13,
    7.11, 4.16, 4.37, 7.85, 7.03, 4.23, 6.3, 6.6, 3.46, 3.15, 2.54, 1.56, 
    2.79, 2.92, 2.5, 3.37, 3.83, 5.04, 4.98, 3.88, 3.75, 5.84, 6.01, 7.53, 
    5.99, 5.35, 3.61, 3.06, 4.8, 5.58, 4.57, 3.76, 3.66, 3.82, 4.16, 4.38, 
    3.42, 4.82, 4.650000000000001, 5, 6.46, 6.92, 6.01, 3.89, 3.9, 4.89, 
    6.16, 6.02, 3.72, 3.14, 2.35, 2.22, 1.68, 1.44, 1.68, 5.03, 5.81, 4.54, 
    3.55, 2.61, 2.76, 3.1, 3.27, 2.77, 3.29, 2.29, 2.11, 2.7, 2.41, 3.04, 
    3.79, 4.98, 5.13, 6, 6.35, 6.96, 4.77, 3.72, 4.150000000000001, 6.73, 
    6.68, 7.25, 7.97, 6, 5.86, 5.89, 6.08, 5.22, 4.07, 3.31, 3.78, 4.13, 
    3.57, 2.34, 1.64, 1.45, 1.44, 3.18, 3.83, 3.03, 2.47, 5.25, 4.5, 3.58, 
    1.8, 1.98, 4.28, 5.72), surge = c(-0.008999999999999999, -0.053, -0.024,
    0, 0.079, 0.068, -0.008999999999999999, -0.003, 0.011, 0.024, 0.029, 
    0.094, 0.091, 0.101, 0.147, 0.124, 0.14, 0.121, 0.047, 0.046, 0.002, 
    -0.028, 0.013, 0.032, 0.153, 0.141, 0.122, 0.167, 0.119, 
    0.08799999999999999, 0.062, 0.074, 0.125, 0.215, 0.238, 0.27, 0.322, 
    0.319, 0.251, 0.231, 0.134, 0.137, 0.042, 0.021, -0.003, 0.023, 0.039, 
    0.113, -0.019, 0.066, 0.144, 0.08699999999999999, 0.177, 0.241, 0.166, 
    0.27, 0.306, 0.246, 0.235, 0.171, 0.07099999999999999, 
    0.08500000000000001, 0.064, 0.119, 0.249, 0.2, 0.123, 0.242, 0.265, 
    0.281, 0.326, 0.244, 0.117, 0.17, 0.059, 0.08500000000000001, 0.187, 
    0.196, 0.221, 0.217, 0.202, 0.133, 0.123, 0.062, 0.061, 0.124, 0.064, 
    -0.008, 0.041, 0.133, 0.076, 0.112, 0.044, 0.047, 0.129, 0.065, 
    0.06900000000000001, 0.045, 0.005, -0.018, 0.035, 0.033, -0.031, 0.041, 
    -0.042
    , 0.044, 0.038, 0.07199999999999999, 0.046, 0.096, 0.126, 0.163, 0.215, 
    0.222, 0.247, 0.25, 0.195, 0.106, 0.098, 0.08, 0.043, 0.035, 0.03, 
    0.003, 0.035, 0.021, 0.07099999999999999, 0.052, -0.017, -0.027, 0.006, 
    0.034, 0.052, 0.091, 0.075, 0.104, 0.07199999999999999, 0.058, 0.134, 
    0.121, 0.055, 0.037, 0.081, 0.065, 0.01, -0.032, 0.003, -0.035, 0.034, 
    0.05, -0.022, -0.011, -0.002, 0.041, -0.05, -0.02, 0.044, 
    0.08500000000000001, 0.074, 0.16, 0.097, 0.109, 0.139, 0.245, 0.188, 
    0.127, 0.193, 0.159, 0.044, 0.158, 0.147, 0.024, 0.092, 0.055, 0, 0.201,
    0.259, -0.025, -0.033, -0.037, -0.055, 0.06900000000000001, 0.093, 
    0.021, 0.07099999999999999, 0.028, 0.049, 0.049, 0.031, 0.042, 0.103, 
    0.08, 0.022, 0.081, 0.034, 0.014, 0.018, 0.011, -0.046, -0.073, 0, 
    -0.039, 0.154, 0.195, 0.128, 0.091, 0.17, 0.183, 0.09, -0.028, -0.042, 
    -0.191
    , -0.176, -0.116, -0.102, -0.151, -0.101, -0.092, -0.215, -0.199, 
    -0.193, -0.018, -0.044, -0.117, -0.062, -0.063, -0.176, 0.063, 0.006, 
    -0.07199999999999999, -0.048, -0.008, 0.041, 0.046, -0.042, 0.014, 
    -0.008999999999999999, -0.02, 0.101, 0.11, 0.08, 0.146, 0.249, 0.006, 
    -0.027, 0.157, 0.101, -0.026, -0.056, -0.098, -0.051, -0.003, -0.03, 
    -0.123, -0.178, -0.214, -0.227, -0.325, -0.276, -0.233, -0.232, -0.274, 
    -0.167
    , -0.218, -0.048, -0.059, -0.136, -0.186, -0.04, 0.048, -0.094, 0.04, 
    0.047, 0.25, 0.012, 0.01, -0.013, -0.016, 0.109, 0.051, -0.044, -0.12, 
    -0.077
    , 0.057, 0.132, 0.124, 0.107, -0.005, -0.024, -0.074, -0.124, -0.129, 
    -0.03, 0.015, -0.036, -0.06900000000000001, -0.08799999999999999, 
    -0.051, -0.036, -0.055, -0.024, -0.008999999999999999, 0.022, 0.039, 
    0.016, 0.171, 0.108, 0.025, 0.124, 0.124, 0.282, 0.091, 0.13, 0.156, 
    0.175, 0.39, 0.335, 0.253, 0.373, 0.34, 0.091, 0.003, 0.143, 0.052, 
    0.228, -0.066, -0.211, -0.152, -0.156, -0.095, 0, -0.07099999999999999, 
    -0.034
    , -0.017, -0.031, -0.236, -0.137, -0.141, -0.104, -0.005, -0.044, 0.048,
    0.314, 0.326, 0.437, 0.49, 0.296, 0.147, 0.348, 0.376, 0.253, 0.252, 
    0.091, 0.129, 0.188, 0.124, -0.006, 0.138, 0.157, 0.245, 0.337, 0.004, 
    0.188, -0.04, 0.062, 0.384, 0.297, 0.075, 0.031, 0.003, -0.057, -0.012, 
    -0.035
    , 0.024, 0.032, -0.026, -0.06900000000000001, -0.048, -0.026, 0.08, 
    0.101, 0.075, 0.07000000000000001, 0.063, 0.023, 0.089, 0.133, 0.065, 
    0.117, -0.028, 0.049, 0.179, 0.373, 0.12, 0.253, 0.19, 0.265, 0.451, 
    0.384, 0.224, 0.133, 0.157, 0.168, 0.08, -0.082, -0.093, -0.119, -0.008,
    0.097, 0.139, 0.09, 0.065, -0.062, -0.051, -0.014, 0.001, 0.057, 0.079, 
    0.064, 0.078, 0.08400000000000001, 0.065, 0.064, 0.049, 
    0.08400000000000001, 0.108, 0.08500000000000001, 0.181, 0.152, 0.031, 
    0.06900000000000001, 0.001, 0.01, 0.063, 0.107, 0.09, 0.023, -0.013, 
    -0.04, -0.053, -0.043, 0.05, 0.231, 0.246, 0.075, 0.212, 0.191, 0.328, 
    0.212, 0.055, 0.064, 0.144, 0.183, 0.08599999999999999, 0.057, -0.063, 
    0.013, 0.124, 0.126, 0.141, -0.107, -0.06900000000000001, -0.008, 
    -0.039, -0.013, -0.079, -0.043, -0.002, -0.053, -0.045, -0.066, -0.021, 
    -0.038
    , -0.033, -0.021, 0.001, -0.062, -0.125, -0.114, -0.093, -0.115, -0.029,
    0.187, 0.199, 0.286, 0.252, 0.196, 0.03, 0.02, -0.034, -0.052, 0.034, 
    0.101, 0.121, 0.102, 0.08699999999999999, 0.092, 0.142, 0.115, 0.049, 
    -0.01, 0.08699999999999999, 0.186, 0.13, 0.172, 0.175, 0.076, 0.044, 
    0.026, -0.023, -0.023, 0.046, 0.062, 0.134, 0.17, 0.159, 0.122, 0.123, 
    0.163, 0.08599999999999999, 0.11, 0.229, 0.129, 0.08699999999999999, 
    0.094, 0.16, 0.018, -0.055, -0.007, 0.067, 0.029, 0.058, 0.021, -0.035, 
    0.007, 0.06, 0.154, 0.047, 0.046, 0.129, 0.145, 0.181, 0.155, 0.195, 
    0.2, 0.115, 0.025, 0.134, 0.174, 0.159, 0.139, 0.178, 0.233, 0.232, 
    0.214, 0.205, 0.083, 0.054, 0.036, 0.02, 0.017, 0.047, 0.079, 0.111, 
    0.048, 0.002, 0.017, -0.049, -0.07000000000000001, 0.006, -0.003, 0.016,
    0.059, 0.008999999999999999, 0.062, 0.049, 0.108, 0.08599999999999999, 
    0.017, 0.062, 0.081, 0.077, 0.152, 0.039, -0.01, 0.005, -0.024, -0.002, 
    0.078, 0.109, 0.08500000000000001, 0.07000000000000001, 0.05, 0.042, 
    0.07000000000000001, 0.091, 0.103, -0.007, -0.012, 0.013, 0.012, 0.059, 
    -0.027
    , -0.06900000000000001, -0.036, -0.025, -0.059, -0.014, 0.141, 0.16, 
    0.097, 0.061, 0.064, 0.041, 0.053, 0.014, 0.055, 0.083, 0.046, 0.056, 
    0.014, 0.038, 0.025, 0.019, 0.026, 0.035, 0.036, 0.08699999999999999, 
    0.148, 0.109, 0.032, -0.008, 0.037, 0.052, 0.047, 0.089, 0.041, 0.118, 
    0.207, 0.218, 0.28, 0.417, 0.25, 0.131, 0.051, 0.043, -0.016, 0.056, 
    0.073, 0.096, 0.037, 0.027, -0.077, -0.07199999999999999, -0.077, 
    -0.074, -0.07099999999999999, -0.044, -0.08, -0.044, 
    -0.07099999999999999, -0.079, -0.076, -0.074, -0.095, -0.062, 
    -0.07000000000000001, -0.032, -0.018, 0.022, 0.076, 0.074, 0.112, 0.139,
    0.112, 0.122, 0.132, 0.08400000000000001, 0.07099999999999999, 0.073, 
    0.04, 0.04, 0.074, 0.091, 0.108, 0.068, 0.08799999999999999, 0.125, 
    0.217, 0.267, 0.26, 0.262, 0.282, 0.165, 0.05, -0.064, -0.014, -0.055, 
    -0.052
    , -0.04, -0.031, -0.051, -0.002, -0.077, -0.07199999999999999, -0.014, 
    0.005, -0.014, -0.007, 0.031, 0.045, 0.014, -0.023, 0.066, 0.08, 0.12, 
    0.111, 0.161, 0.132, 0.124, 0.095, 0.065, 0.054, 0.04, 0.05, 
    0.07199999999999999, 0.081, 0.155, 0.193, 0.153, 0.14, 0.148, 0.166, 
    0.254, 0.326, 0.318, 0.213, 0.157, 0.098, 0.08699999999999999, 0.118, 
    0.116, 0.173, 0.186, 0.167, 0.191, 0.162, 0.115, 0.073, 
    0.07000000000000001, 0.023, 0.005, -0.007, -0.018, 0.005, 0.007, 0.005, 
    0.002, 0.018, 0.068, -0.035, -0.097, -0.074, -0.055, 0.055, 0.073, 
    0.046, 0.148, 0.315, 0.365, 0.267, 0.109, 0.15, 0.351, 0.169, 0.12, 
    0.222, 0.08400000000000001, 0.027, 0.008, -0.033, -0.037, -0.059, 
    -0.083, -0.131, -0.132, -0.138, -0.046, -0.037, -0.127, 
    -0.07199999999999999, 0.003, 0.074, 0.025, -0.118, 0.162, 0.18, 0.081, 
    0.131, 0, -0.031, 0.263, 0.215, 0.119, 0.052, 0.228, 0.338, 0.339, 
    0.174, 0.12, 0.179, 0.179, 0.098, -0.017, -0.066, -0.076, -0.159, 
    -0.128, -0.18, -0.141, -0.08799999999999999, -0.094, -0.055, 0.089, 
    0.032, 0.219, 0.223, 0.272, 0.228, 0.116, 0.302, 0.135, 0.162, 0.158, 
    0.262, 0.128, 0.11, 0.144, 0.025, -0.02, -0.002, -0.061, 0.238, -0.005, 
    0.257, 0.034, 0.047, 0.135, 0.099, 0.16, 0.101, 0.12, 0.049, 0.042, 
    0.025, 0.001, -0.032, 0, -0.068, -0.059, -0.02, -0.01, 0.007, 0.172, 
    0.212, 0.224, 0.344, 0.629, 0.585, 0.14, -0.117, -0.067, -0.045, -0.096,
    -0.063, -0.061, -0.163, -0.119, -0.178, -0.22, -0.233, -0.217, -0.203, 
    -0.195
    , -0.17, -0.19, -0.251, -0.259, -0.226, -0.136, -0.074, -0.006, -0.011, 
    0.053, 0.121, 0.213, 0.696, 0.33, 0.131, 0.177, 0.328, 0.257, 0.076, 
    0.276, 0.379, 0.395, 0.187, 0.002, -0.006, -0.025, -0.106, -0.154, 
    -0.206, -0.166, -0.174, -0.141, -0.18, -0.08799999999999999, -0.221, 
    -0.218, -0.041, -0.06900000000000001, -0.174, -0.138, -0.179, -0.169, 
    -0.157, -0.208, -0.198, -0.176, -0.18, -0.171, -0.219, -0.14, -0.068, 
    0.002, 0.128, 0.107, -0.049, -0.181, -0.099, -0.146, 0.039, 0.076, 
    0.268, 0.277, 0.265, 0.323, 0.135, 0.066, 0.015, 0.001, -0.057, -0.148, 
    -0.129
    , -0.167, -0.075, -0.177, -0.116, -0.285, -0.23, -0.1, -0.146, 0.019, 
    -0.067, -0.115, -0.137, -0.123, -0.093, -0.06900000000000001, -0.198, 
    -0.158, -0.186, -0.098, -0.038, -0.103, -0.11, -0.196, -0.277, -0.238, 
    -0.189
    , -0.149, -0.146, -0.133, -0.029, 0.008999999999999999, -0.051, -0.019, 
    -0.066
    , -0.108, -0.08599999999999999, -0.103, -0.101, -0.106, 
    -0.08599999999999999, -0.045, -0.042, -0.025, -0.068, -0.09, -0.092, 
    -0.08500000000000001, -0.096, -0.109, -0.06, 0.08699999999999999, 0.108,
    0.052, -0.083, -0.061, -0.031, 0.142, 0.114, -0.12, -0.054, -0.064, 
    -0.106, -0.078, -0.114, 0.074, 0.129, -0.119, -0.089, -0.105, -0.139, 
    -0.074, -0.056, -0.026, -0.11, -0.044, -0.014, 0.001, 
    -0.08599999999999999, -0.128, -0.092, -0.08, -0.08799999999999999, 
    -0.096, -0.173, -0.174, -0.136, -0.171, -0.143, -0.09, -0.118, -0.08, 
    -0.018, -0.032, 0.048, 0.038, 0.127, 0.09, 0.03, 0.025, -0.014, 0.048, 
    0.143, 0.139, 0.177, 0.17, 0.302, 0.258, 0.137, 0.177, 0.283, 0.287, 
    0.321, 0.445, 0.225, 0.163, 0.184, 0.179, 0.056, -0.097, -0.056, 
    0.07099999999999999, 0.054, -0.008, -0.044, -0.101, -0.063, -0.016, 
    -0.011, 0.029, -0.051, -0.023, 0.135, 0.236, 0.193, 0.185, 0.216, 0.257,
    0.322, 0.376, 0.3, 0.222, 0.259, 0.229, 0.115, 0.06, 0.047, 0.052, 
    0.031, 0.08500000000000001, 0.106, 0.132, 0.102, 0.178, 0.239, 0.295, 
    0.262, 0.206, 0.158, 0.282, 0.227, 0.103, 0.068, 0.013, -0.049, 
    -0.06900000000000001, -0.052, -0.019, -0.015, -0.002, -0.027, 0.003, 
    0.074, 0.061, 0.01, 0.039, 0.04, -0.044, -0.059, -0.031, 0.04, 0.049, 
    0.063, 0.02, 0.003, 0.01, 0.042, -0.007, -0.117, -0.111, -0.099, -0.075,
    -0.089, -0.138, -0.111, -0.065, -0.026, 0.059, 0.131, 0.222, 0.252, 
    0.147, 0.101, 0.114, 0.151, 0.197, 0.173, 0.126, 0.126, 0.134, 0.157, 
    0.198, 0.243, 0.175, 0.188, 0.138, 0.08599999999999999, 
    0.08400000000000001, 0.007, -0.02, -0.024, 0.002, -0.029, 0.013, 0.051, 
    0.097, 0.134, 0.193, 0.256, 0.288, 0.259, 0.203, 0.165, 0.11, 0.162, 
    0.164, 0.2, 0.127, 0.125, 0.066, 0.109, 0.154, 0.22, 0.206, 0.167, 
    0.172, 0.201, 0.112, 0.121, 0.042, 0.038, 0.033, 0.08400000000000001, 
    0.112, 0.08500000000000001, 0.137, 0.153, 0.135, 0.114, 0.115, 0.184, 
    0.123, 0.307, 0.302, 0.108, 0.029, 0.013, -0.004, -0.011, 0.001, 0.144, 
    0.029, 0.01, -0.012, 0.067, 0.035, 0.056, 0.116, 0.166, 0.176, 0.062, 
    0.014, 0.039, 0.026, 0.043, 0.058, 0.06900000000000001, 
    0.08400000000000001, 0.133, 0.193, 0.134, 0.115, 0.122, 0.214, 0.182, 
    0.117, 0.158, 0.124, 0.063, 0.044, 0.14, 0.043, -0.013, 0.001, -0.018, 
    0.012, 0.016, 0.033, 0.028, 0.083, 0.19, 0.124, 0.13, 0.029, 
    0.07099999999999999, 0.08400000000000001, 0.174, 0.192, 0.179, 0.099, 
    0.075, 0.093, 0.103, 0.148, 0.122, 0.125, 0.219, 0.214, 0.261, 0.282, 
    0.241, 0.228, 0.438, 0.211, 0.221, 0.299, 0.289, 0.251, 0.244, 0.16, 
    0.102, 0.136, 0.207, 0.149, 0.128, 0.097, 0.143, 0.178, 0.111, 0.098, 
    -0.042, -0.018, -0.031, -0.079, -0.062, -0.053, 0.101, 0.079, 0.133, 
    0.178, 0.091, 0.041, 0.01, -0.034, 0.016, 0.047, 0.083, 0.005, 0.047, 
    0.06, 0.229, 0.45, 0.35, 0.365, 0.401, 0.503, 0.487, 0.349, 0.184, 
    0.124, 0.116, 0.041, 0.094, 0.054, -0.033, 0.061, -0.002, -0.031, 
    -0.032, -0.045, -0.055, -0.006, 0.033, 0.008999999999999999, -0.01, 
    -0.03, -0.063, -0.011, 0.092, 0.075, 0.067, 0.019, 0.046, 0.04, 0.063, 
    0.148, 0.229, 0.284, 0.214, 0.039, -0.051, 0.094, -0.119, -0.165, 
    -0.164, -0.059, -0.015, -0.059, -0.062, -0.003, -0.127, 0.124, 0.137, 
    0.067, -0.059, 0.089, 0.101, 0.036, 0.036, 0.053, -0.002, 
    0.06900000000000001, 0.152, -0.074, -0.015, 0.025, 0.004, -0.049, 
    -0.052, -0.164, 0.07000000000000001, -0.001, -0.079, -0.062, -0.053, 
    -0.105, -0.091, -0.121, -0.055, 0.003, 0.044, -0.004, -0.216, -0.261, 
    -0.233, -0.223, -0.235, -0.191, -0.038, 0.141, -0.044, -0.184, -0.089, 
    -0.114
    , -0.003, 0.019, -0.032, -0.108, -0.081, -0.161, -0.08799999999999999, 
    -0.089
    , 0.005, -0.032, 0.005, -0.011, 0.037, 0.162, 0.503, 0.478, 0.386, 
    0.275, 0.125, 0.254, 0.335, 0.372, 0.265, 0.115, -0.119, -0.127, -0.098,
    -0.096, -0.175, -0.11, -0.101, -0.018, -0.189, -0.112, -0.172, -0.152, 
    -0.061
    , 0.211, 0.255, 0.244, 0.411, 0.588, 0.325, 0.22, 0.32, 0.374, 0.483, 
    0.34, 0.191, 0.258, 0.394, 0.378, 0.591, 0.339, 0.257, 0.204, 0.164, 
    0.08799999999999999, -0.03, 0.363, -0.092, -0.119, -0.305, -0.311, 
    -0.216, -0.177, -0.062, -0.063, 0.046, -0.096, -0.035, -0.06, -0.066, 
    -0.037, 0.098, 0.332, 0.063, 0.418, 0.301, 0.038, 0.294, 0.468, 0.334, 
    0.379, 0.572, 0.454, 0.177, 0.102, 0.184, 0.288, 0.249, 0.299, 0.299, 
    0.104, -0.021, 0.13, 0.213, 0.297, 0.295, 0.463, 0.654, 0.716, 0.281, 
    0.285, 0.192, 0.21, 0.413, 0.491, 0.265, 0.036, -0.018, -0.028, 0.026, 
    0.008999999999999999, -0.112, -0.189, -0.215, -0.19, -0.19, -0.221, 
    -0.249, -0.279, -0.308, -0.257, -0.254, -0.191, -0.169, -0.123, 0.053, 
    0.065, -0.147, 0.017, 0.05, -0.041, 0.057, 0.059, 0.016, 0.006, 0.046, 
    -0.042
    , -0.015, 0.054, 0.016, 0.014, -0.008999999999999999, 0.074, 0.026, 
    0.021, 0.038, -0.08400000000000001, -0.051, 0.055, 0.14, 0.003, -0.001, 
    0.103, 0.211, 0.263, 0.2, 0.109, 0.027, -0.033, -0.081, 0.056, 0.111, 
    -0.021, -0.04, -0.04, -0.005, 0.022, 0.008999999999999999, 0.003, 0.038,
    0.048, 0.082, -0.024, -0.031, 0.013, 0.061, 0.275, 0.363, 0.187, 0.126, 
    0.15, 0.149, 0.092, 0.092, 0.077, 0.104, 0.091, 0.103, 0.174, 0.169, 
    0.154, 0.197, 0.191, 0.169, 0.122, 0.018, -0.034, -0.052, -0.026, 
    -0.051, -0.123, -0.07000000000000001, -0.107, -0.117, -0.132, -0.092, 
    -0.064, -0.065, -0.045, -0.051, -0.082, -0.046, 0.013, 0.048, 0.064, 
    0.117, 0.093, 0.121, 0.114, 0.091, 0.12, 0.131, 0.184, 0.112, -0.022, 
    0.288, 0.465, 0.433, 0.332, 0.258, 0.18, 0.098, 0.092, 0.034, -0.018, 
    0.089, 0.191, 0.336, 0.334, 0.28, 0.147, 0.291, 0.3, 
    0.08400000000000001, 0.013, 0.022, 0.083, 0.156, 0.065, 
    0.07199999999999999, 0.037, -0.008, -0.059, -0.011, 0.018, -0.016, 
    0.052, 0.003, 0.061, 0.013, 0.035, 0.091, 0.124, 0.058, 0.063, 0.075, 
    0.06900000000000001, 0.055, 0.09, 0.121, 0.205, 0.132, 0.098, 0.118, 
    0.109, 0.073, 0.028, -0.06900000000000001, -0.031, 0.045, 0.003, 0.018, 
    0.008, 0.005, 0.07199999999999999, -0.024, -0.035, 
    -0.008999999999999999, -0.006, -0.031, -0.022, -0.105, -0.041, -0.083, 
    -0.049
    , 0.003, 0.124, 0.133, 0.149, 0.133, 0.119, 0.161, 0.145, 0.172, 0.099, 
    0.195, 0.239, 0.232, 0.239, 0.24, 0.269, 0.323, 0.359, 0.405, 0.404, 
    0.379, 0.358, 0.391, 0.323, 0.277, 0.19, -0.104, -0.043, 0.167, 0.028, 
    0.11, 0.08, 0.075, 0.081, -0.003, 0.063, -0.022, -0.007, -0.081, 
    -0.06900000000000001, -0.081, -0.121, -0.14, -0.125, -0.091, -0.035, 
    0.016, 0.052, 0.181, 0.058, -0.004, 0.059, 0.083, 0.092, 0.18, 0.039, 
    -0.056, 0.018, 0.159, 0.15, 0.036, 0.023, 0.066, 0.073, 0.073, 0.101, 
    0.07099999999999999, -0.026, 0.022, 0.04, 0.149, 0.08, -0.004, -0.014, 
    -0.048
    , -0.126, -0.151, -0.135, -0.147, -0.155, -0.123, -0.116, -0.167, 
    -0.023, -0.04, -0.051, 0.06, 0.079, 0.132, 0.08699999999999999, 0.036, 
    0.099, 0.214, 0.209, 0.217, 0.186, 0.198, 0.242, 0.132, 0.138, 0.028, 
    0.026, 0.005, 0.004, 0.026, 0.067, 0.049, 0.047, 0.074, 0.024, 0.079, 
    0.096, 0.06900000000000001, 0.073, 0.036, 0.011, 0.04, 0.064, 0.103, 
    0.097, 0.111, 0.11, 0.091, 0.052, 0.067, -0.077, 0.016, -0.028, 0.013, 
    0.033, -0.053, 0.046, 0.073, -0.007, 0.054, 0.104, 0.101, 0.097, 
    0.07000000000000001, 0.066, 0.09, 0.105, 0.096, 0.078, 0.091, 0.09, 
    0.097, 0.179, 0.19, 0.08799999999999999, 0.042, 0.073, 0.078, 0.082, 
    0.07000000000000001, 0.038, 0.051, 0.031, 0.006, -0.002, 0.054, 0.139, 
    0.091, 0.104, 0.112, 0.108, 0.106, 0.079, 0.07199999999999999, 0.1, 
    -0.046, -0.046, -0.07099999999999999, -0.07199999999999999, -0.044, 
    -0.005, -0.054, -0.011, 0.057, 0.098, 0.12, 0.182, 0.286, 0.248, 0.223, 
    0.21, 0.233, 0.192, 0.192, 0.285, 0.198, 0.214, 0.257, 0.212, 0.153, 
    0.141, 0.218, 0.197, 0.142, 0.156, 0.165, 0.146, 0.188, 0.107, 0.073, 
    0.134, 0.178, 0.132, 0.114, 0.037, 0.027, -0.002, -0.017, -0.035, 0.025,
    0.056, 0.037, 0.052, -0.005, 0.003, -0.001, 0.004, 0.05, 0.093, 0.104, 
    0.141, 0.189, 0.256, 0.253, 0.188, 0.147, 0.159, 0.169, 0.177, 0.17, 
    0.183, 0.196, 0.169, 0.203, 0.203, 0.175, 0.155, 0.182, 0.203, 0.168, 
    0.195, 0.166, 0.12, 0.082, 0.078, 0.081, 0.079, 0.03, 0.053, 0.004, 
    0.006, 0.036, 0.041, -0.011, -0.018, -0.065, -0.095, -0.09, -0.047, 
    -0.019, 0.052, 0.032, -0.02, -0.005, -0.02, 0.037, 0.056, 0.079, 0.125, 
    0.137, 0.129, 0.129, 0.083, 0.15, 0.132, 0.147, 0.176, 0.211, 0.247, 
    0.186, 0.233, 0.242, 0.204, 0.105, 0.207, 0.43, 0.311, 0.2, 0.076, 0.09,
    0.103, 0.121, 0.08400000000000001, 0.003, 0.067, 0.053, -0.101, -0.066, 
    -0.126
    , -0.064, -0.034, -0.064, 0, 0.172, 0.231, 0.22, 0.378, 0.487, 0.164, 
    0.302, 0.287, 0.296, 0.229, 0.206, 0.194, 0.288, -0.036, -0.001, -0.005,
    -0.021, -0.014, 0.059, 0.029, 0.08500000000000001, 0.07000000000000001, 
    0.028, 0.048, 0.032, 0.029, -0.063, -0.056, 0.019, 0.066, 0.166, 0.133, 
    0.103, 0.138, 0.08799999999999999, 0.003, -0.041, 0.028, 0.04, 0.078, 
    0.158, 0.162, 0.193, 0.197, 0.185, 0.08400000000000001, -0.021, -0.004, 
    0.045, 0.021, 0.068, 0.011, 0.05, 0.022, 0.113, 0.252, 0.207, 0.323, 
    0.266, 0.225, 0.093, 0.24, 0.108, 0.052, 0.08599999999999999, 0.042, 
    0.036, -0.043, -0.134, -0.128, -0.057, 0.039, 0.091, 0.025, 0.06, 
    0.08799999999999999, 0.126, 0.122, 0.058, -0.052, -0.011, 
    -0.008999999999999999, -0.003, -0.128, -0.176, -0.111, -0.103, -0.079, 
    -0.054
    , -0.025, -0.03, -0.159, -0.102, 0.002, 0.09, 0.07000000000000001, 
    -0.04, -0.066, 0.027, -0.02, 0.076, 0.09, 0.277, 0.232, 0.126, 0.135, 
    0.116, 0.229, 0.175, 0.054, -0.004, -0.056, -0.154, -0.111, -0.148, 
    -0.059, -0.198, -0.234, -0.108, -0.187, -0.182, -0.178, -0.188, -0.152, 
    -0.12
    , -0.185, -0.113, -0.269, -0.133, -0.12, -0.177, -0.185, -0.068, -0.113,
    -0.225, -0.222, -0.167, -0.213, -0.231, -0.188, -0.162, -0.177, -0.121, 
    -0.08500000000000001
    , -0.188, -0.124, -0.111, -0.222, -0.119, -0.11, -0.065, -0.094, -0.073,
    -0.083, -0.044, 0.037, 0.002, 0.019, 0.08699999999999999, 0.066, -0.013,
    0.105, -0.053, 0.021, -0.067, -0.138, -0.126, -0.112, -0.127, -0.15, 
    -0.233, -0.289, -0.226, -0.312, -0.291, -0.227, -0.178, -0.231, -0.269, 
    -0.216
    , -0.26, -0.225, -0.234, -0.188, -0.203, -0.107, -0.107, -0.101, -0.066,
    -0.128, -0.039, -0.14, -0.112, -0.219, -0.161, -0.151, -0.157, -0.111, 
    0.004, 0.122, 0.217, 0.443, 0.517, 0.294, 0.131, 0.044, 0.04, 
    0.08500000000000001, 0.202, 0.178, 0.024, 0.058, 0.08500000000000001, 
    0.035, -0.056, -0.099, -0.024, -0.201, -0.141, -0.135, 0.076, -0.183, 
    -0.176, -0.025, -0.044, -0.015, -0.052, -0.058, -0.07000000000000001, 
    -0.135, -0.024, 0.01, 0.03, 0.092, 0.107, 0.1, 0.154, 0.162, 0.169, 
    0.074, 0.016, -0.103, -0.127, -0.184, -0.147, -0.187, -0.13, -0.073, 
    -0.048, -0.05, -0.055, -0.099, -0.065, -0.08699999999999999, -0.028, 
    -0.04, -0.022, -0.083, -0.054, 0.013, 0.18, 0.182, 0.057, -0.013, 
    -0.097, -0.07000000000000001, -0.013, 0.144, 0.108, 0.341, 0.397, 
    -0.008999999999999999, 0.068, 0.228, 0.234, 0.215, 0.195, 0.055, 0.106, 
    0.167, 0.14, 0.159, 0.127, 0.14, 0.08500000000000001, 0.007, -0.14, 
    -0.018, 0.044, -0.063, -0.08400000000000001, -0.08799999999999999, 
    -0.188, -0.177, -0.192, -0.237, -0.13, -0.173, -0.124, -0.166, -0.111, 
    0.034, 0.073, -0.066, -0.037, -0.012, -0.034, -0.099, -0.053, -0.09, 
    -0.013, -0.092, -0.036, -0.12, -0.132, -0.134, -0.07099999999999999, 
    0.022, 0.068, 0.056, 0.035, 0.029, 0.111, 0.101, 0.07099999999999999, 
    -0.102, 0.161, -0.012, 0.008999999999999999, 0.06900000000000001, 0.024,
    -0.025, 0.028, 0.059, 0.083, 0.081, -0.008999999999999999, -0.056, 
    -0.078, -0.077, -0.059, -0.081, -0.042, -0.08400000000000001, -0.027, 
    -0.113, -0.152, -0.104, -0.127, -0.048, -0.03, 0.096, -0.015, 0.017, 
    0.06, 0.047, 0.016, -0.051, 0.031, -0.035, 0.017, 0.052, 0.106, 0.034, 
    -0.073
    , 0.025, 0.01, -0.006, 0.045, 0.048, 0.118, 0.136, 0.038, 0.054, 0.146, 
    0.073, 0.097, 0.076, 0.058, 0.067, 0.019, -0.017, -0.031, -0.013, 
    -0.017, 0.007, -0.025, -0.063, -0.111, -0.09, -0.026, 0.015, 0.036, 
    0.012, -0.029, 0.012, 0.029, 0.034, -0.08400000000000001, -0.115, 
    -0.08699999999999999, -0.098, -0.125, -0.08599999999999999, -0.083, 
    -0.066, -0.042, 0.007, 0.109, 0.12, 0.13, 0.133, 0.061, 0.08, 0.141, 
    0.107, 0.07099999999999999, 0.077, 0.058, 0.073, -0.016, 0.033, 0.026, 
    0.008, -0.019, 0.045, 0.063, 0.034, 0.038, 0.012, -0.032, -0.075, 
    -0.037, -0.005, -0.092, -0.063, -0.048, -0.017, -0.017, -0.031, 0.035, 
    0.033, 0.039, 0.074, 0.054, 0.115, 0.144, 0.126, 0.117, 
    0.08699999999999999, 0.126, 0.158, 0.123, 0.115, 0.135, 0.116, 0.152, 
    0.209, 0.258, 0.227, 0.191, 0.197, 0.202, 0.237, 0.218, 0.205, 0.158, 
    0.149, 0.123, 0.123, 0.054, 0.008999999999999999, -0.04, -0.014, 0.004, 
    -0.035
    , -0.015, -0.053, -0.128, -0.141, -0.1, -0.096, -0.104, -0.119, -0.094, 
    -0.092
    , -0.036, -0.011, -0.016, 0.016, 0.029, 0.07000000000000001, 0.062, 
    0.117, 0.08500000000000001, 0.034, 0.001, 0.042, 0.029, 0.044, 0.053, 
    0.016, 0.001, 0.015, -0.008999999999999999, 0.014, 0.022, 0.058, 0.052, 
    -0.024
    , 0.048, 0.028, 0.066, 0.006, -0.025, 0, 0.013, 0.013, -0.008, 0.03, 
    0.054, -0.016, -0.013, -0.052, -0.018, 0.021, 0.031, 0.12, 0.165, 0.172,
    0.209, 0.204, 0.204, 0.191, 0.164, 0.173, 0.188, 0.155, 0.153, 0.215, 
    0.247, 0.254, 0.245, 0.173, 0.144, 0.227, 0.27, 0.266, 0.097, 0.053, 
    0.066, 0.081, 0.061, 0.099, 0.037, 0.054, 0.066, 0.067, -0.057, 0.018, 
    0.229, 0.22, 0.029, -0.006, -0.015, 0.063, 0.044, 0, -0.029, 0.004, 
    0.067, 0.115, 0.045, 0.027, 0.015, 0.028, 0.061, 0.095, 0.082, 0.046, 
    0.262, 0.26, 0.333, 0.365, 0.314, 0.438, 0.466, 0.402, 0.367, 0.318, 
    0.277, 0.25, 0.259, 0.202, 0.231, 0.219, 0.412, 0.328, 0.265, 0.487, 
    0.438, 0.27, 0.276, 0.133, 0.176, 0.168, 0.176, 0.165, 0.315, 0.589, 
    0.542, 0.388, 0.351, 0.819, 0.444, 0.112, 0.161, 0.245, 0.399, 0.381, 
    0.482, 0.312, 0.428, 0.4, 0.217, 0.415, 0.435, 0.333, 0.307, 0.232, 
    0.172, 0.151, 0.134, 0.198, 0.231, 0.151, 0.17, 0.118, 0.132, 0.152, 
    0.152, 0.184, 0.236, 0.205, 0.163, 0.216, 0.196, 0.22, 0.166, 0.13, 
    0.08400000000000001, 0.163, 0.134, -0.006, -0.076, -0.027, -0.032, -0.1,
    -0.091, -0.042, -0.133, -0.04, -0.108, -0.068, -0.034, -0.135, -0.079, 
    -0.083
    , -0.052, -0.065, -0.119, -0.116, -0.113, -0.117, 0.035, -0.015, 0.05, 
    0.128, 0.006, 0.256, 0.245, 0.095, 0.351, 0.345, -0.001, 0.177, 0.325, 
    0.283, 0.204, 0.533, 0.405, 0.305, 0.327, 0.09, 0.05, 0.064, 0.036, 
    -0.022, -0.028, -0.08599999999999999, -0.066, -0.09, -0.037, 
    -0.07099999999999999, -0.092, -0.002, 0.034, 0.08500000000000001, 0.094,
    0.182, 0.196, 0.147, 0.093, 0.059, 0.102, 0.144, 0.149, 0.12, 0.141, 
    0.073, 0.026, -0.058, -0.025, -0.055, 0.037, 0.059, -0.027, 0.005, 
    0.128, 0.144, 0.182, 0.275, -0.032, -0.167, -0.14, -0.213, -0.156, 
    -0.123, -0.156, -0.18, -0.211, -0.265, -0.258, -0.246, -0.138, 
    0.07099999999999999, 0.053, 0.08599999999999999, 0.144, 0.145, 0.34, 
    0.444, 0.025, -0.159, -0.228, -0.16, -0.019, 0.173, 0.187, 0.073, 0.001,
    0.293, 0.437, 0.194, 0.191, 0.421, 0.265, 0.224, 0.23, 0.166, 0.119, 
    0.116, 0.057, 0.064, 0.047, 0.012, -0.063, 0.029, 0.19, 0.19, 
    0.008999999999999999, 0.207, 0.137, 0.17, 0.136, 0.117, 
    0.08500000000000001, 0.12, 0.179, 0.22, 0.398, 0.351, 0.313, 0.304, 
    0.272, 0.288, 0.391, 0.158, 0.102, 0.217, 0.265, 0.187, 0.362, 0.458, 
    0.336, 0.224, 0.432, 0.375, 0.374, 0.36, 0.376, 0.282, 0.222, 0.257, 
    0.131, 0.092, 0.008, 0.137, 0.2, 0.027, 0.07000000000000001, -0.025, 0, 
    0.008, -0.038, 0.016, 0.021, -0.029, -0.035, 0.078, 0.1, 0.123, 
    0.08699999999999999, 0.242, 0.29, 0.431, 0.455, 0.302, 0.224, 0.36, 
    0.273, 0.44, 0.138, 0.521, 0.556, 0.336, 0.36, 0.307, 0.27, 0.188, 
    0.189, 0.166, 0.08799999999999999, 0.075, 0.054, 0.103, 0.055, -0.042, 
    -0.053
    , 0.035, 0.036, 0.038, -0.004, 0.004, -0.051, -0.11, -0.175, 
    -0.07099999999999999, 0.008999999999999999, 0.08599999999999999)), 
    class = "data.frame", row.names = c("1", "2", "3", "4", "5", "6", "7", 
    "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 
    "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", 
    "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", 
    "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", 
    "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", 
    "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", 
    "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", 
    "92", "93", "94", "95", "96", "97", "98", "99", "100", "101", "102", 
    "103", "104", "105", "106", "107", "108", "109", "110", "111", "112", 
    "113", "114", "115", "116", "117", "118", "119", "120", "121", "122", 
    "123", "124", "125", "126", "127", "128", "129", "130", "131", "132", 
    "133", "134", "135", "136", "137", "138", "139", "140", "141", "142", 
    "143", "144", "145", "146", "147", "148", "149", "150", "151", "152", 
    "153", "154", "155", "156", "157", "158", "159", "160", "161", "162", 
    "163", "164", "165", "166", "167", "168", "169", "170", "171", "172", 
    "173", "174", "175", "176", "177", "178", "179", "180", "181", "182", 
    "183", "184", "185", "186", "187", "188", "189", "190", "191", "192", 
    "193", "194", "195", "196", "197", "198", "199", "200", "201", "202", 
    "203", "204", "205", "206", "207", "208", "209", "210", "211", "212", 
    "213", "214", "215", "216", "217", "218", "219", "220", "221", "222", 
    "223", "224", "225", "226", "227", "228", "229", "230", "231", "232", 
    "233", "234", "235", "236", "237", "238", "239", "240", "241", "242", 
    "243", "244", "245", "246", "247", "248", "249", "250", "251", "252", 
    "253", "254", "255", "256", "257", "258", "259", "260", "261", "262", 
    "263", "264", "265", "266", "267", "268", "269", "270", "271", "272", 
    "273", "274", "275", "276", "277", "278", "279", "280", "281", "282", 
    "283", "284", "285", "286", "287", "288", "289", "290", "291", "292", 
    "293", "294", "295", "296", "297", "298", "299", "300", "301", "302", 
    "303", "304", "305", "306", "307", "308", "309", "310", "311", "312", 
    "313", "314", "315", "316", "317", "318", "319", "320", "321", "322", 
    "323", "324", "325", "326", "327", "328", "329", "330", "331", "332", 
    "333", "334", "335", "336", "337", "338", "339", "340", "341", "342", 
    "343", "344", "345", "346", "347", "348", "349", "350", "351", "352", 
    "353", "354", "355", "356", "357", "358", "359", "360", "361", "362", 
    "363", "364", "365", "366", "367", "368", "369", "370", "371", "372", 
    "373", "374", "375", "376", "377", "378", "379", "380", "381", "382", 
    "383", "384", "385", "386", "387", "388", "389", "390", "391", "392", 
    "393", "394", "395", "396", "397", "398", "399", "400", "401", "402", 
    "403", "404", "405", "406", "407", "408", "409", "410", "411", "412", 
    "413", "414", "415", "416", "417", "418", "419", "420", "421", "422", 
    "423", "424", "425", "426", "427", "428", "429", "430", "431", "432", 
    "433", "434", "435", "436", "437", "438", "439", "440", "441", "442", 
    "443", "444", "445", "446", "447", "448", "449", "450", "451", "452", 
    "453", "454", "455", "456", "457", "458", "459", "460", "461", "462", 
    "463", "464", "465", "466", "467", "468", "469", "470", "471", "472", 
    "473", "474", "475", "476", "477", "478", "479", "480", "481", "482", 
    "483", "484", "485", "486", "487", "488", "489", "490", "491", "492", 
    "493", "494", "495", "496", "497", "498", "499", "500", "501", "502", 
    "503", "504", "505", "506", "507", "508", "509", "510", "511", "512", 
    "513", "514", "515", "516", "517", "518", "519", "520", "521", "522", 
    "523", "524", "525", "526", "527", "528", "529", "530", "531", "532", 
    "533", "534", "535", "536", "537", "538", "539", "540", "541", "542", 
    "543", "544", "545", "546", "547", "548", "549", "550", "551", "552", 
    "553", "554", "555", "556", "557", "558", "559", "560", "561", "562", 
    "563", "564", "565", "566", "567", "568", "569", "570", "571", "572", 
    "573", "574", "575", "576", "577", "578", "579", "580", "581", "582", 
    "583", "584", "585", "586", "587", "588", "589", "590", "591", "592", 
    "593", "594", "595", "596", "597", "598", "599", "600", "601", "602", 
    "603", "604", "605", "606", "607", "608", "609", "610", "611", "612", 
    "613", "614", "615", "616", "617", "618", "619", "620", "621", "622", 
    "623", "624", "625", "626", "627", "628", "629", "630", "631", "632", 
    "633", "634", "635", "636", "637", "638", "639", "640", "641", "642", 
    "643", "644", "645", "646", "647", "648", "649", "650", "651", "652", 
    "653", "654", "655", "656", "657", "658", "659", "660", "661", "662", 
    "663", "664", "665", "666", "667", "668", "669", "670", "671", "672", 
    "673", "674", "675", "676", "677", "678", "679", "680", "681", "682", 
    "683", "684", "685", "686", "687", "688", "689", "690", "691", "692", 
    "693", "694", "695", "696", "697", "698", "699", "700", "701", "702", 
    "703", "704", "705", "706", "707", "708", "709", "710", "711", "712", 
    "713", "714", "715", "716", "717", "718", "719", "720", "721", "722", 
    "723", "724", "725", "726", "727", "728", "729", "730", "731", "732", 
    "733", "734", "735", "736", "737", "738", "739", "740", "741", "742", 
    "743", "744", "745", "746", "747", "748", "749", "750", "751", "752", 
    "753", "754", "755", "756", "757", "758", "759", "760", "761", "762", 
    "763", "764", "765", "766", "767", "768", "769", "770", "771", "772", 
    "773", "774", "775", "776", "777", "778", "779", "780", "781", "782", 
    "783", "784", "785", "786", "787", "788", "789", "790", "791", "792", 
    "793", "794", "795", "796", "797", "798", "799", "800", "801", "802", 
    "803", "804", "805", "806", "807", "808", "809", "810", "811", "812", 
    "813", "814", "815", "816", "817", "818", "819", "820", "821", "822", 
    "823", "824", "825", "826", "827", "828", "829", "830", "831", "832", 
    "833", "834", "835", "836", "837", "838", "839", "840", "841", "842", 
    "843", "844", "845", "846", "847", "848", "849", "850", "851", "852", 
    "853", "854", "855", "856", "857", "858", "859", "860", "861", "862", 
    "863", "864", "865", "866", "867", "868", "869", "870", "871", "872", 
    "873", "874", "875", "876", "877", "878", "879", "880", "881", "882", 
    "883", "884", "885", "886", "887", "888", "889", "890", "891", "892", 
    "893", "894", "895", "896", "897", "898", "899", "900", "901", "902", 
    "903", "904", "905", "906", "907", "908", "909", "910", "911", "912", 
    "913", "914", "915", "916", "917", "918", "919", "920", "921", "922", 
    "923", "924", "925", "926", "927", "928", "929", "930", "931", "932", 
    "933", "934", "935", "936", "937", "938", "939", "940", "941", "942", 
    "943", "944", "945", "946", "947", "948", "949", "950", "951", "952", 
    "953", "954", "955", "956", "957", "958", "959", "960", "961", "962", 
    "963", "964", "965", "966", "967", "968", "969", "970", "971", "972", 
    "973", "974", "975", "976", "977", "978", "979", "980", "981", "982", 
    "983", "984", "985", "986", "987", "988", "989", "990", "991", "992", 
    "993", "994", "995", "996", "997", "998", "999", "1000", "1001", "1002",
    "1003", "1004", "1005", "1006", "1007", "1008", "1009", "1010", "1011", 
    "1012", "1013", "1014", "1015", "1016", "1017", "1018", "1019", "1020", 
    "1021", "1022", "1023", "1024", "1025", "1026", "1027", "1028", "1029", 
    "1030", "1031", "1032", "1033", "1034", "1035", "1036", "1037", "1038", 
    "1039", "1040", "1041", "1042", "1043", "1044", "1045", "1046", "1047", 
    "1048", "1049", "1050", "1051", "1052", "1053", "1054", "1055", "1056", 
    "1057", "1058", "1059", "1060", "1061", "1062", "1063", "1064", "1065", 
    "1066", "1067", "1068", "1069", "1070", "1071", "1072", "1073", "1074", 
    "1075", "1076", "1077", "1078", "1079", "1080", "1081", "1082", "1083", 
    "1084", "1085", "1086", "1087", "1088", "1089", "1090", "1091", "1092", 
    "1093", "1094", "1095", "1096", "1097", "1098", "1099", "1100", "1101", 
    "1102", "1103", "1104", "1105", "1106", "1107", "1108", "1109", "1110", 
    "1111", "1112", "1113", "1114", "1115", "1116", "1117", "1118", "1119", 
    "1120", "1121", "1122", "1123", "1124", "1125", "1126", "1127", "1128", 
    "1129", "1130", "1131", "1132", "1133", "1134", "1135", "1136", "1137", 
    "1138", "1139", "1140", "1141", "1142", "1143", "1144", "1145", "1146", 
    "1147", "1148", "1149", "1150", "1151", "1152", "1153", "1154", "1155", 
    "1156", "1157", "1158", "1159", "1160", "1161", "1162", "1163", "1164", 
    "1165", "1166", "1167", "1168", "1169", "1170", "1171", "1172", "1173", 
    "1174", "1175", "1176", "1177", "1178", "1179", "1180", "1181", "1182", 
    "1183", "1184", "1185", "1186", "1187", "1188", "1189", "1190", "1191", 
    "1192", "1193", "1194", "1195", "1196", "1197", "1198", "1199", "1200", 
    "1201", "1202", "1203", "1204", "1205", "1206", "1207", "1208", "1209", 
    "1210", "1211", "1212", "1213", "1214", "1215", "1216", "1217", "1218", 
    "1219", "1220", "1221", "1222", "1223", "1224", "1225", "1226", "1227", 
    "1228", "1229", "1230", "1231", "1232", "1233", "1234", "1235", "1236", 
    "1237", "1238", "1239", "1240", "1241", "1242", "1243", "1244", "1245", 
    "1246", "1247", "1248", "1249", "1250", "1251", "1252", "1253", "1254", 
    "1255", "1256", "1257", "1258", "1259", "1260", "1261", "1262", "1263", 
    "1264", "1265", "1266", "1267", "1268", "1269", "1270", "1271", "1272", 
    "1273", "1274", "1275", "1276", "1277", "1278", "1279", "1280", "1281", 
    "1282", "1283", "1284", "1285", "1286", "1287", "1288", "1289", "1290", 
    "1291", "1292", "1293", "1294", "1295", "1296", "1297", "1298", "1299", 
    "1300", "1301", "1302", "1303", "1304", "1305", "1306", "1307", "1308", 
    "1309", "1310", "1311", "1312", "1313", "1314", "1315", "1316", "1317", 
    "1318", "1319", "1320", "1321", "1322", "1323", "1324", "1325", "1326", 
    "1327", "1328", "1329", "1330", "1331", "1332", "1333", "1334", "1335", 
    "1336", "1337", "1338", "1339", "1340", "1341", "1342", "1343", "1344", 
    "1345", "1346", "1347", "1348", "1349", "1350", "1351", "1352", "1353", 
    "1354", "1355", "1356", "1357", "1358", "1359", "1360", "1361", "1362", 
    "1363", "1364", "1365", "1366", "1367", "1368", "1369", "1370", "1371", 
    "1372", "1373", "1374", "1375", "1376", "1377", "1378", "1379", "1380", 
    "1381", "1382", "1383", "1384", "1385", "1386", "1387", "1388", "1389", 
    "1390", "1391", "1392", "1393", "1394", "1395", "1396", "1397", "1398", 
    "1399", "1400", "1401", "1402", "1403", "1404", "1405", "1406", "1407", 
    "1408", "1409", "1410", "1411", "1412", "1413", "1414", "1415", "1416", 
    "1417", "1418", "1419", "1420", "1421", "1422", "1423", "1424", "1425", 
    "1426", "1427", "1428", "1429", "1430", "1431", "1432", "1433", "1434", 
    "1435", "1436", "1437", "1438", "1439", "1440", "1441", "1442", "1443", 
    "1444", "1445", "1446", "1447", "1448", "1449", "1450", "1451", "1452", 
    "1453", "1454", "1455", "1456", "1457", "1458", "1459", "1460", "1461", 
    "1462", "1463", "1464", "1465", "1466", "1467", "1468", "1469", "1470", 
    "1471", "1472", "1473", "1474", "1475", "1476", "1477", "1478", "1479", 
    "1480", "1481", "1482", "1483", "1484", "1485", "1486", "1487", "1488", 
    "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", 
    "1498", "1499", "1500", "1501", "1502", "1503", "1504", "1505", "1506", 
    "1507", "1508", "1509", "1510", "1511", "1512", "1513", "1514", "1515", 
    "1516", "1517", "1518", "1519", "1520", "1521", "1522", "1523", "1524", 
    "1525", "1526", "1527", "1528", "1529", "1530", "1531", "1532", "1533", 
    "1534", "1535", "1536", "1537", "1538", "1539", "1540", "1541", "1542", 
    "1543", "1544", "1545", "1546", "1547", "1548", "1549", "1550", "1551", 
    "1552", "1553", "1554", "1555", "1556", "1557", "1558", "1559", "1560", 
    "1561", "1562", "1563", "1564", "1565", "1566", "1567", "1568", "1569", 
    "1570", "1571", "1572", "1573", "1574", "1575", "1576", "1577", "1578", 
    "1579", "1580", "1581", "1582", "1583", "1584", "1585", "1586", "1587", 
    "1588", "1589", "1590", "1591", "1592", "1593", "1594", "1595", "1596", 
    "1597", "1598", "1599", "1600", "1601", "1602", "1603", "1604", "1605", 
    "1606", "1607", "1608", "1609", "1610", "1611", "1612", "1613", "1614", 
    "1615", "1616", "1617", "1618", "1619", "1620", "1621", "1622", "1623", 
    "1624", "1625", "1626", "1627", "1628", "1629", "1630", "1631", "1632", 
    "1633", "1634", "1635", "1636", "1637", "1638", "1639", "1640", "1641", 
    "1642", "1643", "1644", "1645", "1646", "1647", "1648", "1649", "1650", 
    "1651", "1652", "1653", "1654", "1655", "1656", "1657", "1658", "1659", 
    "1660", "1661", "1662", "1663", "1664", "1665", "1666", "1667", "1668", 
    "1669", "1670", "1671", "1672", "1673", "1674", "1675", "1676", "1677", 
    "1678", "1679", "1680", "1681", "1682", "1683", "1684", "1685", "1686", 
    "1687", "1688", "1689", "1690", "1691", "1692", "1693", "1694", "1695", 
    "1696", "1697", "1698", "1699", "1700", "1701", "1702", "1703", "1704", 
    "1705", "1706", "1707", "1708", "1709", "1710", "1711", "1712", "1713", 
    "1714", "1715", "1716", "1717", "1718", "1719", "1720", "1721", "1722", 
    "1723", "1724", "1725", "1726", "1727", "1728", "1729", "1730", "1731", 
    "1732", "1733", "1734", "1735", "1736", "1737", "1738", "1739", "1740", 
    "1741", "1742", "1743", "1744", "1745", "1746", "1747", "1748", "1749", 
    "1750", "1751", "1752", "1753", "1754", "1755", "1756", "1757", "1758", 
    "1759", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1767", 
    "1768", "1769", "1770", "1771", "1772", "1773", "1774", "1775", "1776", 
    "1777", "1778", "1779", "1780", "1781", "1782", "1783", "1784", "1785", 
    "1786", "1787", "1788", "1789", "1790", "1791", "1792", "1793", "1794", 
    "1795", "1796", "1797", "1798", "1799", "1800", "1801", "1802", "1803", 
    "1804", "1805", "1806", "1807", "1808", "1809", "1810", "1811", "1812", 
    "1813", "1814", "1815", "1816", "1817", "1818", "1819", "1820", "1821", 
    "1822", "1823", "1824", "1825", "1826", "1827", "1828", "1829", "1830", 
    "1831", "1832", "1833", "1834", "1835", "1836", "1837", "1838", "1839", 
    "1840", "1841", "1842", "1843", "1844", "1845", "1846", "1847", "1848", 
    "1849", "1850", "1851", "1852", "1853", "1854", "1855", "1856", "1857", 
    "1858", "1859", "1860", "1861", "1862", "1863", "1864", "1865", "1866", 
    "1867", "1868", "1869", "1870", "1871", "1872", "1873", "1874", "1875", 
    "1876", "1877", "1878", "1879", "1880", "1881", "1882", "1883", "1884", 
    "1885", "1886", "1887", "1888", "1889", "1890", "1891", "1892", "1893", 
    "1894", "1895", "1896", "1897", "1898", "1899", "1900", "1901", "1902", 
    "1903", "1904", "1905", "1906", "1907", "1908", "1909", "1910", "1911", 
    "1912", "1913", "1914", "1915", "1916", "1917", "1918", "1919", "1920", 
    "1921", "1922", "1923", "1924", "1925", "1926", "1927", "1928", "1929", 
    "1930", "1931", "1932", "1933", "1934", "1935", "1936", "1937", "1938", 
    "1939", "1940", "1941", "1942", "1943", "1944", "1945", "1946", "1947", 
    "1948", "1949", "1950", "1951", "1952", "1953", "1954", "1955", "1956", 
    "1957", "1958", "1959", "1960", "1961", "1962", "1963", "1964", "1965", 
    "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", 
    "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", 
    "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", 
    "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", 
    "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", 
    "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", 
    "2020", "2021", "2022", "2023", "2024", "2025", "2026", "2027", "2028", 
    "2029", "2030", "2031", "2032", "2033", "2034", "2035", "2036", "2037", 
    "2038", "2039", "2040", "2041", "2042", "2043", "2044", "2045", "2046", 
    "2047", "2048", "2049", "2050", "2051", "2052", "2053", "2054", "2055", 
    "2056", "2057", "2058", "2059", "2060", "2061", "2062", "2063", "2064", 
    "2065", "2066", "2067", "2068", "2069", "2070", "2071", "2072", "2073", 
    "2074", "2075", "2076", "2077", "2078", "2079", "2080", "2081", "2082", 
    "2083", "2084", "2085", "2086", "2087", "2088", "2089", "2090", "2091", 
    "2092", "2093", "2094", "2095", "2096", "2097", "2098", "2099", "2100", 
    "2101", "2102", "2103", "2104", "2105", "2106", "2107", "2108", "2109", 
    "2110", "2111", "2112", "2113", "2114", "2115", "2116", "2117", "2118", 
    "2119", "2120", "2121", "2122", "2123", "2124", "2125", "2126", "2127", 
    "2128", "2129", "2130", "2131", "2132", "2133", "2134", "2135", "2136", 
    "2137", "2138", "2139", "2140", "2141", "2142", "2143", "2144", "2145", 
    "2146", "2147", "2148", "2149", "2150", "2151", "2152", "2153", "2154", 
    "2155", "2156", "2157", "2158", "2159", "2160", "2161", "2162", "2163", 
    "2164", "2165", "2166", "2167", "2168", "2169", "2170", "2171", "2172", 
    "2173", "2174", "2175", "2176", "2177", "2178", "2179", "2180", "2181", 
    "2182", "2183", "2184", "2185", "2186", "2187", "2188", "2189", "2190", 
    "2191", "2192", "2193", "2194", "2195", "2196", "2197", "2198", "2199", 
    "2200", "2201", "2202", "2203", "2204", "2205", "2206", "2207", "2208", 
    "2209", "2210", "2211", "2212", "2213", "2214", "2215", "2216", "2217", 
    "2218", "2219", "2220", "2221", "2222", "2223", "2224", "2225", "2226", 
    "2227", "2228", "2229", "2230", "2231", "2232", "2233", "2234", "2235", 
    "2236", "2237", "2238", "2239", "2240", "2241", "2242", "2243", "2244", 
    "2245", "2246", "2247", "2248", "2249", "2250", "2251", "2252", "2253", 
    "2254", "2255", "2256", "2257", "2258", "2259", "2260", "2261", "2262", 
    "2263", "2264", "2265", "2266", "2267", "2268", "2269", "2270", "2271", 
    "2272", "2273", "2274", "2275", "2276", "2277", "2278", "2279", "2280", 
    "2281", "2282", "2283", "2284", "2285", "2286", "2287", "2288", "2289", 
    "2290", "2291", "2292", "2293", "2294", "2295", "2296", "2297", "2298", 
    "2299", "2300", "2301", "2302", "2303", "2304", "2305", "2306", "2307", 
    "2308", "2309", "2310", "2311", "2312", "2313", "2314", "2315", "2316", 
    "2317", "2318", "2319", "2320", "2321", "2322", "2323", "2324", "2325", 
    "2326", "2327", "2328", "2329", "2330", "2331", "2332", "2333", "2334", 
    "2335", "2336", "2337", "2338", "2339", "2340", "2341", "2342", "2343", 
    "2344", "2345", "2346", "2347", "2348", "2349", "2350", "2351", "2352", 
    "2353", "2354", "2355", "2356", "2357", "2358", "2359", "2360", "2361", 
    "2362", "2363", "2364", "2365", "2366", "2367", "2368", "2369", "2370", 
    "2371", "2372", "2373", "2374", "2375", "2376", "2377", "2378", "2379", 
    "2380", "2381", "2382", "2383", "2384", "2385", "2386", "2387", "2388", 
    "2389", "2390", "2391", "2392", "2393", "2394", "2395", "2396", "2397", 
    "2398", "2399", "2400", "2401", "2402", "2403", "2404", "2405", "2406", 
    "2407", "2408", "2409", "2410", "2411", "2412", "2413", "2414", "2415", 
    "2416", "2417", "2418", "2419", "2420", "2421", "2422", "2423", "2424", 
    "2425", "2426", "2427", "2428", "2429", "2430", "2431", "2432", "2433", 
    "2434", "2435", "2436", "2437", "2438", "2439", "2440", "2441", "2442", 
    "2443", "2444", "2445", "2446", "2447", "2448", "2449", "2450", "2451", 
    "2452", "2453", "2454", "2455", "2456", "2457", "2458", "2459", "2460", 
    "2461", "2462", "2463", "2464", "2465", "2466", "2467", "2468", "2469", 
    "2470", "2471", "2472", "2473", "2474", "2475", "2476", "2477", "2478", 
    "2479", "2480", "2481", "2482", "2483", "2484", "2485", "2486", "2487", 
    "2488", "2489", "2490", "2491", "2492", "2493", "2494", "2495", "2496", 
    "2497", "2498", "2499", "2500", "2501", "2502", "2503", "2504", "2505", 
    "2506", "2507", "2508", "2509", "2510", "2511", "2512", "2513", "2514", 
    "2515", "2516", "2517", "2518", "2519", "2520", "2521", "2522", "2523", 
    "2524", "2525", "2526", "2527", "2528", "2529", "2530", "2531", "2532", 
    "2533", "2534", "2535", "2536", "2537", "2538", "2539", "2540", "2541", 
    "2542", "2543", "2544", "2545", "2546", "2547", "2548", "2549", "2550", 
    "2551", "2552", "2553", "2554", "2555", "2556", "2557", "2558", "2559", 
    "2560", "2561", "2562", "2563", "2564", "2565", "2566", "2567", "2568", 
    "2569", "2570", "2571", "2572", "2573", "2574", "2575", "2576", "2577", 
    "2578", "2579", "2580", "2581", "2582", "2583", "2584", "2585", "2586", 
    "2587", "2588", "2589", "2590", "2591", "2592", "2593", "2594", "2595", 
    "2596", "2597", "2598", "2599", "2600", "2601", "2602", "2603", "2604", 
    "2605", "2606", "2607", "2608", "2609", "2610", "2611", "2612", "2613", 
    "2614", "2615", "2616", "2617", "2618", "2619", "2620", "2621", "2622", 
    "2623", "2624", "2625", "2626", "2627", "2628", "2629", "2630", "2631", 
    "2632", "2633", "2634", "2635", "2636", "2637", "2638", "2639", "2640", 
    "2641", "2642", "2643", "2644", "2645", "2646", "2647", "2648", "2649", 
    "2650", "2651", "2652", "2653", "2654", "2655", "2656", "2657", "2658", 
    "2659", "2660", "2661", "2662", "2663", "2664", "2665", "2666", "2667", 
    "2668", "2669", "2670", "2671", "2672", "2673", "2674", "2675", "2676", 
    "2677", "2678", "2679", "2680", "2681", "2682", "2683", "2684", "2685", 
    "2686", "2687", "2688", "2689", "2690", "2691", "2692", "2693", "2694", 
    "2695", "2696", "2697", "2698", "2699", "2700", "2701", "2702", "2703", 
    "2704", "2705", "2706", "2707", "2708", "2709", "2710", "2711", "2712", 
    "2713", "2714", "2715", "2716", "2717", "2718", "2719", "2720", "2721", 
    "2722", "2723", "2724", "2725", "2726", "2727", "2728", "2729", "2730", 
    "2731", "2732", "2733", "2734", "2735", "2736", "2737", "2738", "2739", 
    "2740", "2741", "2742", "2743", "2744", "2745", "2746", "2747", "2748", 
    "2749", "2750", "2751", "2752", "2753", "2754", "2755", "2756", "2757", 
    "2758", "2759", "2760", "2761", "2762", "2763", "2764", "2765", "2766", 
    "2767", "2768", "2769", "2770", "2771", "2772", "2773", "2774", "2775", 
    "2776", "2777", "2778", "2779", "2780", "2781", "2782", "2783", "2784", 
    "2785", "2786", "2787", "2788", "2789", "2790", "2791", "2792", "2793", 
    "2794", "2795", "2796", "2797", "2798", "2799", "2800", "2801", "2802", 
    "2803", "2804", "2805", "2806", "2807", "2808", "2809", "2810", "2811", 
    "2812", "2813", "2814", "2815", "2816", "2817", "2818", "2819", "2820", 
    "2821", "2822", "2823", "2824", "2825", "2826", "2827", "2828", "2829", 
    "2830", "2831", "2832", "2833", "2834", "2835", "2836", "2837", "2838", 
    "2839", "2840", "2841", "2842", "2843", "2844", "2845", "2846", "2847", 
    "2848", "2849", "2850", "2851", "2852", "2853", "2854", "2855", "2856", 
    "2857", "2858", "2859", "2860", "2861", "2862", "2863", "2864", "2865", 
    "2866", "2867", "2868", "2869", "2870", "2871", "2872", "2873", "2874", 
    "2875", "2876", "2877", "2878", "2879", "2880", "2881", "2882", "2883", 
    "2884", "2885", "2886", "2887", "2888", "2889", "2890", "2891", "2892", 
    "2893", "2894"))

"wind"<-
structure(.Data = list(Year = c(1944, 1945, 1946, 1947, 1948, 1949, 1950, 1951, 
    1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963, 
    1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 
    1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983), Hartford = c(49, 54, 
    60, 49, 57, 51, 79, 57, 45, 60, 54, 55, 49, 58, 51, 59, 51, 47, 46, 57, 
    57, 51, 47, 62, 45, 48, 45, 57, 53, 42, 53, 51, 55, 55, 60, 49, 48, 45, 
    50, 52), Albany = c(52, 46, 48, 44, 42, 41, 68, 50, 55, 68, 47, 43, 41, 
    47, 41, 55, 44, 46, 40, 49, 46, 44, 48, 49, 47, 46, 46, 62, 46, 38, 51, 
    49, 53, 46, 43, 48, 44, 46, 44, 40)), class = "data.frame", row.names
     = c("33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", 
    "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", 
    "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", 
    "68", "69", "70", "71", "72"))

"wooster"<-
c(23, 29, 19, 14, 27, 32, 31, 26, 21, 41, 30, 21, 20, 29, 20, 13, 6, 5, 6, 1, 
    19, 30, 33, 33, 29, 25, 18, 18, 27, 31, 27, 19, 38, 23, 20, 14, 21, 22, 
    13, 11, 15, 19, 15, 7, 15, 35, 34, 35, 30, 30, 32, 37, 33, 27, 26, 23, 
    21, 19, 29, 30, 23, 28, 45, 50, 51, 48, 45, 35, 32, 28, 28, 22, 40, 32, 
    28, 31, 42, 36, 32, 23, 18, 17, 19, 17, 19, 39, 39, 21, 19, 27, 32, 43, 
    40, 39, 38, 42, 47, 39, 42, 41, 39, 32, 42, 45, 35, 31, 29, 25, 23, 28, 
    30, 36, 35, 37, 34, 31, 56, 49, 42, 56, 53, 55, 47, 40, 41, 36, 59, 35, 
    29, 28, 33, 40, 46, 57, 42, 39, 32, 37, 53, 56, 48, 59, 51, 47, 49, 41, 
    35, 43, 49, 49, 46, 41, 39, 48, 57, 54, 51, 48, 46, 42, 51, 55, 57, 60, 
    62, 61, 62, 61, 61, 66, 66, 56, 59, 59, 66, 58, 65, 64, 65, 63, 63, 68, 
    65, 66, 69, 56, 48, 45, 51, 57, 56, 51, 68, 64, 59, 66, 69, 67, 67, 67, 
    68, 67, 65, 65, 66, 62, 57, 56, 61, 71, 70, 68, 67, 59, 59, 63, 67, 63, 
    58, 62, 61, 51, 63, 56, 50, 50, 54, 58, 62, 69, 64, 67, 56, 68, 64, 57, 
    60, 66, 67, 66, 60, 58, 65, 56, 55, 55, 61, 68, 66, 57, 50, 50, 62, 65, 
    64, 53, 45, 37, 56, 52, 51, 67, 68, 47, 37, 36, 30, 34, 51, 43, 45, 46, 
    43, 54, 46, 55, 62, 55, 46, 36, 48, 48, 48, 50, 56, 45, 40, 33, 40, 43, 
    41, 34, 43, 47, 44, 49, 48, 42, 34, 33, 45, 32, 25, 29, 46, 45, 37, 32, 
    35, 35, 41, 33, 32, 43, 25, 23, 21, 21, 34, 32, 31, 27, 46, 53, 41, 42, 
    52, 32, 29, 37, 38, 35, 33, 26, 22, 22, 29, 33, 35, 27, 25, 26, 26, 30, 
    31, 36, 35, 35, 31, 21, 16, 15, 9, 10, 12, 7, 3, -13, -12, -8, 11, 22, 
    -1
    , -4, 4, 23, 29, 26, 32, 30, 26, 19, 19, 22, 6, 4, 3, 9, 16, 1, 0, 18, 
    2, -3, -9, -16, -2, 7, 35, 30, 27, 23, 15, 27, 23, 7, 1, 20, 31, 29, 24,
    13, 11, 12, 23, 28, 42, 42, 49, 37, 28, 30, 42, 41, 39, 31, 24, 23, 28, 
    32, 16, 11, 24, 20, 14, 7, 8, 1, -2, 29, 24, 14, 9, -6, -4, 7, 0, 24, 
    27, 25, 26, 24, 28, 28, 35, 32, 30, 27, 23, 34, 28, 29, 33, 28, 28, 32, 
    30, 28, 30, 45, 36, 34, 30, 24, 39, 33, 27, 32, 44, 43, 43, 43, 33, 33, 
    34, 32, 35, 38, 38, 39, 42, 43, 54, 50, 44, 50, 38, 34, 41, 43, 42, 40, 
    52, 39, 37, 37, 54, 51, 39, 41, 37, 30, 32, 50, 59, 56, 55, 66, 55, 45, 
    55, 52, 45, 52, 44, 40, 42, 46, 47, 57, 50, 52, 63, 70, 70, 67, 67, 57, 
    49, 67, 57, 48, 54, 66, 69, 66, 61, 58, 54, 68, 65, 53, 47, 61, 53, 62, 
    57, 59, 55, 58, 65, 64, 62, 49, 45, 58, 65, 62, 58, 64, 63, 66, 61, 53, 
    56, 50, 53, 62, 59, 61, 66, 57, 60, 55, 54, 52, 53, 56, 63, 69, 67, 66, 
    66, 68, 69, 66, 68, 65, 67, 65, 65, 62, 57, 58, 62, 59, 58, 52, 45, 56, 
    60, 54, 47, 51, 53, 64, 63, 63, 49, 53, 58, 58, 50, 45, 39, 44, 57, 56, 
    60, 59, 54, 56, 57, 46, 38, 36, 40, 44, 58, 51, 53, 65, 64, 62, 42, 41, 
    37, 33, 41, 40, 37, 45, 38, 41, 43, 55, 56, 53, 51, 50, 50, 50, 49, 53, 
    59, 48, 41, 54, 38, 52, 48, 45, 39, 35, 60, 60, 59, 44, 42, 47, 46, 27, 
    25, 42, 40, 32, 25, 30, 45, 48, 30, 25, 25, 21, 40, 27, 22, 32, 26, 18, 
    27, 27, 28, 24, 25, 30, 39, 33, 31, 33, 31, 27, 25, 18, 14, 18, 6, 17, 
    12, 35, 34, 36, 37, 38, 44, 42, 47, 28, 30, 29, 30, 25, 23, 21, 11, 18, 
    28, 52, 43, 29, 27, 31, 26, 23, 24, 24, 29, 30, 15, 8, 16, 16, 11, 12, 
    18, 11, -1, 17, 17, -5, -19, -18, 9, 15, 19, 14, 7, 11, 20, 10, 7, 23, 
    10, 7, -5, -13, 13, 13, 5, -2, -1, -3, 24, 31, 15, 14, 6, 4, 24, 20, 18,
    13, 21, 42, 42, 38, 30, 29, 26, 24, 33, 27, 22, 35, 23, 20, 25, 38, 31, 
    31, 42, 34, 28, 34, 26, 22, 22, 20, 21, 25, 19, 31, 37, 35, 26, 20, 45, 
    55, 47, 37, 38, 30, 30, 32, 33, 45, 37, 30, 27, 16, 19, 39, 35, 43, 51, 
    50, 38, 31, 38, 62, 52, 51, 54, 57, 55, 53, 47, 44, 43, 35, 37, 49, 48, 
    41, 35, 49, 54, 47, 40, 33, 46, 53, 64, 57, 51, 62, 55, 43, 36, 37, 57, 
    43, 37, 48, 45, 44, 58, 61, 51, 45, 49, 64, 55, 57, 52, 47, 48, 47, 49, 
    57, 61, 53, 49, 47, 45, 46, 52, 57, 52, 55, 49, 52, 50, 63, 55, 55, 45, 
    49, 49, 49, 50, 54, 62, 62, 58, 51, 62, 61, 57, 67, 67, 61, 57, 61, 61, 
    69, 68, 56, 50, 53, 57, 69, 66, 58, 49, 48, 67, 57, 51, 52, 56, 61, 62, 
    58, 50, 50, 55, 59, 63, 68, 62, 54, 58, 59, 50, 66, 67, 67, 64, 57, 62, 
    57, 57, 52, 48, 52, 56, 64, 62, 58, 55, 59, 60, 53, 47, 61, 61, 69, 69, 
    70, 71, 70, 67, 63, 46, 44, 39, 38, 38, 38, 42, 49, 53, 53, 50, 51, 55, 
    42, 35, 47, 44, 38, 39, 44, 41, 32, 36, 39, 43, 37, 30, 49, 53, 51, 45, 
    45, 53, 51, 49, 36, 30, 46, 55, 46, 46, 50, 44, 55, 34, 30, 35, 32, 30, 
    42, 42, 50, 51, 49, 46, 45, 40, 34, 32, 43, 44, 42, 42, 50, 43, 41, 41, 
    40, 39, 59, 29, 23, 32, 31, 29, 30, 33, 35, 33, 36, 37, 41, 17, 17, 24, 
    30, 31, 31, 27, 30, 31, 38, 32, 18, 8, 9, 18, 12, 1, -2, 2, 6, 6, 31, 
    11, 0, -2, 17, 5, 16, 12, 18, 16, 29, 28, 28, 22, 13, 7, 3, 14, 27, 21, 
    30, 8, 11, -1, 13, 41, 41, 31, 29, 27, 31, 20, 17, 30, 21, 2, 1, 11, 16,
    14, 26, 31, 30, 34, 34, 30, 24, 23, 24, 16, 14, 8, 4, 15, 16, 18, 32, 
    42, 33, 31, 22, 21, 26, 17, 17, 18, 9, 6, 10, 23, 30, 28, 28, 22, 4, -1,
    27, 47, 34, 31, 30, 46, 38, 34, 32, 30, 32, 12, 10, 19, 34, 28, 35, 44, 
    29, 26, 49, 55, 41, 47, 36, 36, 53, 50, 50, 46, 40, 34, 32, 32, 27, 35, 
    31, 34, 29, 38, 33, 39, 53, 34, 26, 22, 27, 44, 51, 51, 55, 48, 44, 52, 
    38, 30, 29, 51, 61, 47, 46, 46, 44, 43, 43, 55, 57, 62, 64, 57, 66, 55, 
    47, 46, 47, 47, 49, 49, 52, 60, 60, 54, 62, 61, 56, 41, 37, 49, 68, 68, 
    67, 61, 54, 50, 72, 62, 59, 61, 59, 62, 51, 44, 53, 57, 53, 59, 65, 54, 
    45, 48, 63, 67, 64, 56, 56, 62, 54, 49, 63, 64, 68, 66, 61, 55, 65, 69, 
    71, 60, 57, 67, 71, 74, 74, 69, 63, 58, 56, 59, 64, 65, 61, 60, 64, 59, 
    56, 62, 61, 59, 52, 53, 58, 64, 64, 62, 64, 49, 43, 49, 61, 67, 67, 64, 
    63, 57, 62, 62, 60, 64, 51, 45, 67, 51, 44, 40, 41, 43, 48, 53, 56, 66, 
    55, 48, 42, 35, 36, 49, 65, 64, 45, 51, 52, 41, 36, 52, 62, 65, 58, 55, 
    67, 66, 67, 65, 63, 66, 69, 68, 63, 64, 64, 61, 51, 39, 32, 42, 42, 32, 
    36, 51, 59, 37, 37, 34, 40, 31, 27, 28, 36, 39, 52, 52, 50, 50, 50, 48, 
    47, 35, 33, 49, 44, 24, 39, 37, 42, 40, 49, 43, 26, 33, 28, 16, 11, 15, 
    34, 34, 32, 27, 30, 33, 31, 30, 27, 22, 28, 31, 31, 36, 33, 30, 32, 33, 
    24, 23, 22, 31, 37, 39, 17, 17, 19, 15, 15, 27, 25, 32, 30, 28, 27, 23, 
    17, 19, 26, 31, 29, 28, 25, 21, 26, 30, 26, 30, 23, 17, 13, 13, 32, 27, 
    25, 28, 28, 27, 25, 35, 31, 22, 19, 28, 24, 23, 15, 23, -1, -3, 1, 7, 0,
    13, 24, 26, 25, 25, 35, 32, 24, 16, 20, 29, 16, 15, 16, 18, 31, 21, 20, 
    8, 7, 8, 21, 12, 14, 20, 32, 31, 26, 20, 25, 32, 35, 36, 36, 27, 20, 25,
    28, 40, 35, 19, 14, 16, 25, 21, 27, 29, 23, 19, 25, 33, 26, 28, 27, 34, 
    36, 40, 40, 34, 40, 37, 29, 22, 21, 30, 27, 28, 33, 37, 39, 31, 28, 34, 
    44, 46, 45, 49, 50, 47, 48, 41, 43, 48, 51, 48, 51, 41, 37, 40, 38, 38, 
    39, 34, 34, 45, 45, 38, 33, 33, 40, 37, 39, 59, 57, 43, 37, 55, 43, 36, 
    47, 58, 62, 61, 58, 65, 57, 49, 48, 62, 65, 63, 65, 66, 63, 66, 65, 66, 
    53, 47, 54, 61, 66, 46, 41, 45, 51, 66, 61, 61, 57, 57, 54, 65, 70, 70, 
    68, 61, 58, 61, 61, 53, 51, 58, 64, 64, 64, 65, 61, 56, 67, 69, 71, 72, 
    70, 68, 67, 71, 58, 53, 57, 54, 57, 63, 70, 70, 69, 69, 63, 72, 71, 57, 
    53, 59, 63, 66, 68, 69, 69, 65, 62, 58, 56, 63, 70, 58, 56, 59, 65, 65, 
    60, 63, 67, 55, 59, 55, 56, 65, 55, 50, 48, 55, 53, 61, 55, 48, 53, 47, 
    47, 43, 43, 59, 62, 60, 62, 58, 54, 62, 62, 60, 53, 51, 63, 66, 64, 57, 
    50, 51, 49, 48, 47, 40, 33, 43, 49, 57, 43, 41, 46, 36, 30, 40, 44, 42, 
    35, 31, 42, 32, 31, 28, 26, 32, 35, 46, 39, 32, 40, 32, 30, 40, 35, 30, 
    25, 36, 30, 38, 32, 40, 45, 49, 55, 56, 31, 29, 39, 45, 36, 28, 25, 25, 
    34, 37, 29, 39, 53, 38, 30, 22, 16, 13, 24, 39, 42, 38, 35, 41, 39, 36, 
    30, 28, 29, 27, 27, 21, 26, 34, 43, 32, 29, 32, 31, 28, 30, 27, 26, 26, 
    30, 36, 28, 21, 32, 29, 35, 26, 25, 31, 18, 12, 26)

