
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
# BUILIN - PACKAGE DESCRIPTION:
#  Package: evir
#  Version: 1.1
#  Date: 2004-05-05
#  Title: Extreme Values in R
#  Author: S original (EVIS) by Alexander McNeil 
#    <mcneil@math.ethz.ch>, R port by Alec 
#    Stephenson <alec_stephenson@hotmail.com>.
#  Maintainer: Alec Stephenson <alec_stephenson@hotmail.com>
#  Depends: R (>= 1.5.0)
#  Description: Functions for extreme value theory, which may be 
#    divided into the following groups; exploratory data analysis, 
#    block maxima, peaks over thresholds (univariate and bivariate), 
#    point processes, gev/gpd distributions.
#  License: GPL (Version 2 or above)
#  URL: http://www.maths.lancs.ac.uk/~stephena/
#  Packaged: Wed May  5 15:29:24 2004; stephena
################################################################################
# BUILTIN - PACKAGE DESCRIPTION:
#  Package: ismev
#  Version: 1.1
#  Date: 2003/11/25
#  Title: An Introduction to Statistical Modeling of Extreme Values  
#  Author: Original S functions by Stuart Coles
#    <Stuart.Coles@bristol.ac.uk>, R port and R documentation files 
#    by Alec Stephenson <a.stephenson@lancaster.ac.uk>.
#  Maintainer: Alec Stephenson <a.stephenson@lancaster.ac.uk>
#  Depends: R (>= 1.5.0)
#  Description: Functions to support the computations carried out in
#    `An Introduction to Statistical Modeling of Extreme Values' by
#    Stuart Coles. The functions may be divided into the following 
#    groups; maxima/minima, order statistics, peaks over thresholds
#    and point processes.  
#  License: GPL (Version 2 or above)
#  URL: http://www.maths.lancs.ac.uk/~stephena/
################################################################################


# This file contains the following functions:
# gev.fit  gev.diag  gev.pp  gev.qq  gev.rl  gev.his
# gevf  gevq  gev.dens  gev.profxi  gev.prof

"gev.fit"<-
function(xdat, ydat = NULL, mul = NULL, sigl = NULL, shl = NULL, 
mulink = identity, siglink = identity, shlink = identity, show = TRUE, 
method = "Nelder-Mead", maxit = 10000, ...)
{
#
# obtains mles etc for gev distn
#
	z <- list()
        npmu <- length(mul) + 1
        npsc <- length(sigl) + 1
        npsh <- length(shl) + 1
	z$trans <- FALSE	# if maximization fails, could try
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
function(xdat, threshold, npy = 365, ydat = NULL, sigl = NULL, shl = NULL, 
siglink = identity, shlink = identity, show = TRUE, method = "Nelder-Mead", 
maxit = 10000, ...)
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
	#	points(1/(1 - (1:n)/(n + 1))/npy, 
#		sort(xdat))
#	abline(h = u, col = 3)
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
u + (a[1] * (p^( - a[2])	#
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
function(xdat, ydat = NULL, mul = NULL, sigl = NULL, mulink = identity, 
siglink = identity, show = TRUE, method = "Nelder-Mead", maxit = 10000, ...)
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
function(xdat, threshold, npy = 365, ydat = NULL, mul = NULL, sigl = NULL, 
shl = NULL, mulink = identity, siglink = identity, shlink = identity, 
show = TRUE, method = "Nelder-Mead", maxit = 10000, ...)
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
function(xdat, r = dim(xdat)[2], ydat = NULL, mul = NULL, sigl = NULL, 
shl = NULL, mulink = identity, siglink = identity, shlink = identity, 
show = TRUE, method = "Nelder-Mead", maxit = 10000, ...)
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



################################################################################


"gev" <- 
function(data, block = NA, ...)
{
    n.all <- NA
    if(!is.na(block)) {
        n.all <- length(data)
        if(is.character(block)) {
            times <- as.POSIXlt(attributes(data)$times)
            if(block %in% c("semester", "quarter")) {
                sem <- quart <- times$mon
                sem[sem %in% 0:5] <- quart[quart %in% 0:2] <- 0
                sem[sem %in% 6:11] <- quart[quart %in% 3:5] <- 1
                quart[quart %in% 6:8] <- 2
                quart[quart %in% 9:11] <- 3
            }
            grouping <- switch(block,
                semester = paste(times$year, sem),
                quarter = paste(times$year, quart),
                month = paste(times$year, times$mon),
                year = times$year,
                stop("unknown time period"))
            data <- tapply(data, grouping, max)
        }
        else {
            data <- as.numeric(data)
            nblocks <- (length(data) %/% block) + 1
            grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
            data <- tapply(data, grouping, max)
        }
    }
    data <- as.numeric(data)
    n <- length(data)
    sigma0 <- sqrt(6 * var(data))/pi
    mu0 <- mean(data) - 0.57722 * sigma0
    xi0 <- 0.1
    theta <- c(xi0, sigma0, mu0)
    negloglik <- function(theta, tmp)
    {
      	y <- 1 + (theta[1] * (tmp - theta[3]))/theta[2]
       	if((theta[2] < 0) || (min(y) < 0))
       	    out <- 1e+06
       	else {
       	    term1 <- length(tmp) * logb(theta[2])
       	    term2 <- sum((1 + 1/theta[1]) * logb(y))
       	    term3 <- sum(y^(-1/theta[1]))
       	    out <- term1 + term2 + term3
       	}
       	out
    }
    fit <- optim(theta, negloglik, hessian = TRUE, ..., tmp = data)
    if(fit$convergence)
        warning("optimization may not have succeeded")
    par.ests <- fit$par
    varcov <- solve(fit$hessian)
    par.ses <- sqrt(diag(varcov))
    out <- list(n.all = n.all, n = n, data = data, block = block, par.ests
       	 = par.ests, par.ses = par.ses, varcov = varcov, converged = 
       	fit$convergence, nllh.final = fit$value)
    names(out$par.ests) <- c("xi", "sigma", "mu")
    names(out$par.ses) <- c("xi", "sigma", "mu")
    class(out) <- "gev"
    out
}

"gumbel" <- 
function(data, block = NA, ...)
{
	n.all <- NA
	data <- as.numeric(data)
        if(!is.na(block)) {
	  n.all <- length(data)
	  if(fg <- n.all %% block) {
              data <- c(data, rep(NA, block - fg))
              warning(paste("final group contains only", fg, "observations"))
          }
          data <- apply(matrix(data, nrow = block), 2, max, na.rm = TRUE)
	}
	n <- length(data)
	sigma0 <- sqrt(6 * var(data))/pi
	mu0 <- mean(data) - 0.57722 * sigma0
	theta <- c(sigma0, mu0)
	negloglik <- function(theta, tmp)
	{
		y <- (tmp - theta[2])/theta[1]
		if(theta[1] < 0)
			out <- 1e+06
		else {
			term1 <- length(tmp) * logb(theta[1])
			term2 <- sum(y)
			term3 <- sum(exp( - y))
			out <- term1 + term2 + term3
		}
		out
	}
        fit <- optim(theta, negloglik, hessian = TRUE, ..., tmp = data)
        if(fit$convergence)
            warning("optimization may not have succeeded")
	par.ests <- fit$par
	varcov <- solve(fit$hessian)
	par.ses <- sqrt(diag(varcov))
	out <- list(n.all = n.all, n = n, data = data, block = block, par.ests
		 = par.ests, par.ses = par.ses, varcov = varcov, converged = 
		fit$convergence, nllh.final = fit$value)
	names(out$par.ests) <- c("sigma", "mu")
	names(out$par.ses) <- c("sigma", "mu")
	class(out) <- "gev"
	out
}

"plot.gev" <- 
function(x, ...)
{
	par.ests <- x$par.ests
	mu <- par.ests["mu"]
	sigma <- par.ests["sigma"]
	if(!("xi" %in% names(par.ests)))
	    xi <- 0
	else xi <- par.ests["xi"]
	if(xi != 0)
	    residuals <- (1 + (xi * (x$data - mu))/sigma)^(-1/xi)
	else residuals <- exp( - exp( - (x$data - mu)/sigma))
	choices <- c("Scatterplot of Residuals", "QQplot of Residuals")
	tmenu <- paste("plot:", choices)
	pick <- 1
	while(pick > 0) {
	    pick <- menu(tmenu, title =
                         "\nMake a plot selection (or 0 to exit):")
	    switch(pick,
		   {
		       plot(residuals, ylab = "Residuals",
                            xlab = "Ordering", ...)
		       lines(lowess(1:length(residuals), residuals))
		   },
		   qplot(residuals, ...))
	}
}

"rlevel.gev" <- 
function(out, k.blocks = 20, add = FALSE, ...)
{
	par.ests <- out$par.ests
	mu <- par.ests["mu"]
	sigma <- par.ests["sigma"]
	if(!("xi" %in% names(par.ests)))
	    stop("Use this function after a GEV rather than a Gumbel fit")
	else xi <- par.ests["xi"]
	pp <- 1/k.blocks
	v <- qgev((1 - pp), xi, mu, sigma)
	if(add) abline(h = v)
	data <- out$data
        overallmax <- out$nllh.final
	sigma0 <- sqrt(6 * var(data))/pi
	xi0 <- 0.01
	theta <- c(xi0, sigma0)
	parloglik <- function(theta, tmp, pp, rli)
	{
		mu <- rli + (theta[2] * (1 - ( - logb(1 - pp))^( - theta[
			1])))/theta[1]
		y <- 1 + (theta[1] * (tmp - mu))/theta[2]
		if((theta[2] < 0) | (min(y) < 0))
			out <- 1e+06
		else {
			term1 <- length(tmp) * logb(theta[2])
			term2 <- sum((1 + 1/theta[1]) * logb(y))
			term3 <- sum(y^(-1/theta[1]))
			out <- term1 + term2 + term3
		}
		out
	}
	parmax <- NULL
	rl <- v * c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95, 1, 1.1, 1.2,
                    1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 4.5)
	for(i in 1:length(rl)) {
		fit <- optim(theta, parloglik, hessian = FALSE, tmp = data,
                             pp = pp, rli = rl[i])
		parmax <- rbind(parmax, fit$value)
	}
	parmax <-  - parmax
	overallmax <-  - overallmax
	crit <- overallmax - qchisq(0.9999, 1)/2
	cond <- parmax > crit
	rl <- rl[cond]
	parmax <- parmax[cond]
	smth <- spline(rl, parmax, n = 200)
	aalpha <- qchisq(0.95, 1)
	if(!add) {
	    plot(rl, parmax, type = "p", ...)
	    abline(h = overallmax - aalpha/2)
	    abline(v = v)
	    lines(smth)
	}
        ind <- smth$y > overallmax - aalpha/2
	ci <- range(smth$x[ind])
	if(add) {
	    abline(h = ci[1], lty = 2, col = 2)
	    abline(h = ci[2], lty = 2, col = 2)
	}
	as.numeric(c(ci[1], v, ci[2]))
}


"gpdbiv" <- 
function(data1 = NA, data2 = NA, u1 = NA, u2 = NA, ne1 = NA,
    ne2 = NA, global = FALSE, method = "BFGS", ...)
{
    data1 <- as.numeric(data1)
    data2 <- as.numeric(data2)
    
    Zfunc <- function(y, u, lambda, xi, sigma)
        (lambda^-1) * (1 + (xi * pmax((y - u), 0))/sigma)^(1/xi)
    Kfunc <- function(y, u, lambda, xi, sigma)
        -lambda^(-xi) * (sigma^-1) * (Zfunc(y, u, lambda, xi, sigma))^(1 - xi)
    Vfunc <- function(x, y, alpha)
        (x^(-1/alpha) + y^(-1/alpha))^alpha
    Vfunc1 <- function(x, y, alpha)
        -x^(-(1/alpha) - 1) * (x^(-1/alpha) + y^(-1/alpha))^(alpha - 1)
    Vfunc2 <- function(x, y, alpha)
        -(alpha - 1) * (alpha^-1) * (x * y)^(-(1/alpha) - 1) *
          (x^(-1/alpha) + y^(-1/alpha))^(alpha - 2)
    fun <- list(Z = Zfunc, K = Kfunc, V = Vfunc, V1 = Vfunc1, V2 = Vfunc2) 
   
    if(is.na(ne1) && is.na(u1))
        stop(paste("Enter either a threshold or",
                   "the number of upper extremes for margin 1"))
    if(!is.na(ne1) && !is.na(u1))
        stop(paste("Enter EITHER a threshold or",
                   "the number of upper extremes for margin 1"))
    if(is.na(ne2) && is.na(u2))
        stop(paste("Enter either a threshold or",
                   "the number of upper extremes for margin 2"))
    if(!is.na(ne2) && !is.na(u2))
        stop(paste("Enter EITHER a threshold or",
                   "the number of upper extremes for margin 2"))

    out1 <- gpd(data1, threshold = u1, ne = ne1)
    par.ests1 <- out1$par.ests
    par.ses1 <- out1$par.ses
    
    out2 <- gpd(data2, threshold = u2, ne = ne2)
    par.ests2 <- out2$par.ests
    par.ses2 <- out2$par.ses

    uu <- c(out1$threshold, out2$threshold)
    ne <- c(out1$n.exceed, out2$n.exceed)
    mpar <- c(par.ests1, par.ests2) 
    
    delta1 <- as.numeric(data1 > uu[1])
    delta2 <- as.numeric(data2 > uu[2])
    lambda1 <- sum(delta1)/length(data1)
    lambda2 <- sum(delta2)/length(data2)

    theta <- 0.8
    if(global) {
        theta <- c(theta, mpar)
        mpar <- NULL
    }
	
    negloglik <- function(theta, data1, data2, uu, delta1, delta2,
        lambda1, lambda2, mpar, fun)
    {
      	alpha <- theta[1]
	if(is.null(mpar)) {
            xi1 <- theta[2] ; sigma1 <- theta[3]
	    xi2 <- theta[4] ; sigma2 <- theta[5]
	}
        else {
            xi1 <- mpar[1] ; sigma1 <- mpar[2]
	    xi2 <- mpar[3] ; sigma2 <- mpar[4]
        }
	cond1 <- (alpha <= 0) | (alpha >= 1)
	cond2 <- sigma1 <= 0
	cond3 <- sigma2 <= 0
	if(cond1 || cond2 || cond3)
	   out <- 1e+06
	else {
	    term4 <- (1 - delta1) * (1 - delta2) * logb(1 -
                fun$V(lambda1^-1, lambda2^-1, alpha))
	    term3 <- delta1 * (1 - delta2) * logb(fun$K(data1, uu[1], lambda1,
                xi1, sigma1) * fun$V1(fun$Z(data1, uu[1], lambda1, xi1,
                sigma1), lambda2^-1, alpha))
	    term2 <- delta2 * (1 - delta1) * logb(fun$K(data2, uu[2], lambda2,
                xi2, sigma2) * fun$V1(fun$Z(data2, uu[2], lambda2, xi2,
                sigma2), lambda1^-1, alpha))
	    term1 <- delta1 * delta2 * logb(fun$K(data1, uu[1], lambda1, xi1,
                sigma1) * fun$K(data2, uu[2], lambda2, xi2, sigma2) *
                fun$V2(fun$Z(data1, uu[1], lambda1, xi1, sigma1), fun$Z(data2,
                uu[2], lambda2, xi2, sigma2), alpha))
	    allterm <- term1 + term2 + term3 + term4
	    out <-  - sum(allterm)
	}
	out
    }
    fit <- optim(theta, negloglik, hessian = TRUE, method = method, ...,
                 data1 = data1, data2 = data2, uu = uu,
                 delta1 = delta1, delta2 = delta2, lambda1 = lambda1,
                 lambda2 = lambda2, mpar = mpar, fun = fun)
    if(fit$convergence)
        warning("optimization may not have succeeded")
    par.ests <- fit$par
    varcov <- solve(fit$hessian)
    par.ses <- sqrt(diag(varcov))
    alpha <- par.ests[1]
    alpha.se <- par.ses[1]
    if(global) {
        par.ests1 <- c(par.ests[2], par.ests[3])
        names(par.ests1) <- c("xi", "beta")
        par.ses1 <- c(par.ses[2], par.ses[3])
        par.ests2 <- c(par.ests[4], par.ests[5])
        names(par.ests2) <- c("xi", "beta")
        par.ses2 <- c(par.ses[4], par.ses[5])
    }
    out <- list(data1 = data1[delta1 == 1], delta1 = (delta1 ==
      	1 & delta2 == 1)[delta1 == 1], data2 = data2[
       	delta2 == 1], delta2 = (delta1 == 1 & delta2 == 1)[delta2 ==
       	1], u1 = uu[1], ne1 = ne[1], lambda1 = lambda1, u2 = uu[2],
        ne2 = ne[2], lambda2 = lambda2, alpha = alpha, alpha.se = alpha.se, 
       	par.ests1 = par.ests1, par.ses1 = par.ses1, par.ests2 = 
       	par.ests2, par.ses2 = par.ses2, converged = fit$convergence,
       	nllh.final = fit$value, dependence = "logistic", 
       	dep.func = Vfunc)
    class(out) <- "gpdbiv"
    out
}

"interpret.gpdbiv" <- 
function(out, x, y)
{
    Vfuncf <- out$dep.func
    newfunc <- function(x, y, alpha, u1, lambda1, xi1, sigma1, u2, lambda2,
	           xi2, sigma2, vfunc)
    {
        Zfunc <- function(y, u, lambda, xi, sigma)
	    (lambda^-1) * (1 + (xi * pmax((y - u), 0))/sigma)^(1/xi)
	1 - vfunc(Zfunc(x, u1, lambda1, xi1, sigma1), Zfunc(y, u2,
		  lambda2, xi2, sigma2), alpha)
    }
    marg <- function(x, u1, lambda1, xi1, sigma1)
    {
        1 - lambda1 * (1 + (xi1 * (x - u1))/sigma1)^(-1/xi1)
    }
    newfunc2 <- function(x, y, alpha, u1, lambda1, xi1, sigma1, u2, lambda2,
		    xi2, sigma2, marg, newfunc, vfunc)
    {
        1 - marg(x, u1, lambda1, xi1, sigma1) - marg(y, u2, lambda2, xi2,
        sigma2) + newfunc(x, y, alpha, u1, lambda1, xi1, sigma1, u2,
        lambda2, xi2, sigma2, vfunc)
    }
    
    if(out$u1 > x) stop("Point below x threshold")
    if(out$u2 > y) stop("Point below y threshold")
    p1 <- 1 - marg(x, out$u1, out$lambda1, out$par.ests1[1], out$
		par.ests1[2])
    p2 <- 1 - marg(y, out$u2, out$lambda2, out$par.ests2[1], out$
		par.ests2[2])
    p12 <- newfunc2(x, y, out$alpha, out$u1, out$lambda1, out$par.ests1[1],
                    out$par.ests1[2], out$u2, out$lambda2, out$par.ests2[1],
                    out$par.ests2[2], marg, newfunc, Vfuncf)
    
    cat("Thresholds:", out$u1, out$u2, "\n")
    cat("Extreme levels of interest (x,y):", x, y, "\n")
    cat("P(X exceeds x)", p1, "\n")
    cat("P(Y exceeds y)", p2, "\n")
    cat("P(X exceeds x AND Y exceeds y)", p12, "\n")
    cat("P(X exceeds x) * P(Y exceeds y)", p1 * p2, "\n")
    cat("P(Y exceeds y GIVEN X exceeds x)", p12/p1, "\n")
    cat("P(X exceeds x GIVEN Y exceeds y)", p12/p2, "\n")
    invisible(as.numeric(c(p1, p2, p12, p1 * p2, p12/p1, p12/p2)))
}

"plot.gpdbiv" <- 
function(x, extend = 1.1, n.contours = 15, ...)
{
    Zfunc <- function(y, u, lambda, xi, sigma)
        (lambda^-1) * (1 + (xi * pmax((y - u), 0))/sigma)^(1/xi)

    joint <- function(xx, y, alpha, u1, lambda1, xi1, sigma1, u2, lambda2,
		xi2, sigma2, Vfunc)
    {
        1 - Vfunc(Zfunc(xx, u1, lambda1, xi1, sigma1),
                  Zfunc(y, u2, lambda2, xi2, sigma2), alpha)
    }
    marg <- function(xx, u1, lambda1, xi1, sigma1)
    {
        1 - lambda1 * (1 + (xi1 * (xx - u1))/sigma1)^(-1/xi1)
    }
    survivor <- function(xx, y, alpha, u1, lambda1, xi1, sigma1, u2,
                    lambda2, xi2, sigma2, marg, joint, Vfunc)
    {
	1 - marg(xx, u1, lambda1, xi1, sigma1) - marg(y, u2, lambda2,
	    xi2, sigma2) + joint(xx, y, alpha, u1, lambda1, xi1,
	    sigma1, u2, lambda2, xi2, sigma2, Vfunc)
    }
    
    xx <- seq(from = x$u1, to = extend * max(x$data1), length = 200)
    y <- seq(from = x$u2, to = extend * max(x$data2), length = 200)
    choices <- c("Exceedance data", 
		 "Contours of Bivariate Distribution Function", 
		 "Contours of Bivariate Survival Function",
                 "Tail of Marginal 1", "Tail of Marginal 2")
    tmenu <- paste("plot:", choices)
    pick <- 1
    while(pick > 0) {
        par(mfrow = c(1, 1))
	pick <- menu(tmenu, title =
                     "\nMake a plot selection (or 0 to exit):")
	if(pick == 1) {
	    par(mfrow = c(2, 1))
	    plot(x$data1, main = "Marginal1", type = "n", ...)
	    points((1:length(x$data1))[x$delta1 == 0],
                   x$data1[x$delta1 == 0])
	    points((1:length(x$data1))[x$delta1 == 1],
                   x$data1[x$delta1 == 1], col = 2)
	    plot(x$data2, main = "Marginal2", type = "n", ...)
	    points((1:length(x$data2))[x$delta2 == 0],
                   x$data2[x$delta2 == 0])
	    points((1:length(x$data2))[x$delta2 == 1],
                   x$data2[x$delta2 == 1], col = 2)
	}
	if(pick == 4) {
	    x$name <- "Marginal1"
	    x$par.ests <- x$par.ests1
	    x$data <- x$data1
	    x$threshold <- x$u1
	    x$p.less.thresh <- 1 - x$lambda1
	    tailplot(x, ...)
	}
	if(pick == 5) {
	    x$name <- "Marginal2"
	    x$par.ests <- x$par.ests2
	    x$data <- x$data2
	    x$threshold <- x$u2
	    x$p.less.thresh <- 1 - x$lambda2
	    tailplot(x, ...)
	}
	if(pick == 2) {
	    z <- outer(xx, y, joint, alpha = x$alpha, u1 = x$u1,
                       lambda1 = x$lambda1, xi1 = x$par.ests1[1],
                       sigma1 = x$par.ests1[2], u2 = x$u2, lambda2 =
                       x$lambda2, xi2 = x$par.ests2[1], sigma2 =
                       x$par.ests2[2], Vfunc = x$dep.func)
	    par(xaxs = "i", yaxs = "i")
	    contour(xx, y, z, nlevels = n.contours, main = "Joint", ...)
	}
	if(pick == 3) {
	    z2 <- outer(xx, y, survivor, alpha = x$alpha, u1 = x$u1,
                        lambda1 = x$lambda1, xi1 = x$par.ests1[1],
                        sigma1 = x$par.ests1[2], u2 = x$u2, lambda2 =
                        x$lambda2, xi2 = x$par.ests2[1], sigma2 =
                        x$par.ests2[2], marg = marg, joint = joint,
                        Vfunc = x$dep.func)
	    level.thresh <- x$lambda1 + x$lambda2 - (x$lambda1^(1/x$alpha) +
                x$lambda2^(1/x$alpha))^x$alpha
	    contour(xx, y, z2, nlevels = n.contours, main = "Survival", ...)
	}
    }
}

"emplot" <- 
function(data, alog = "x", labels = TRUE, ...)
{
    data <- sort(as.numeric(data))
    ypoints <- 1 - ppoints(data)
    plot(data, ypoints, log = alog, xlab = "", ylab = "", ...)
    if(labels) {
        xxlab <- "x"
	yylab <- "1 - F(x)"
	if(alog != "")
	    xxlab <- paste(xxlab, "(on log scale)")
	if(alog == "xy" || alog == "yx")
	    yylab <- paste(yylab, "(on log scale)")
	 title(xlab = xxlab, ylab = yylab)
    }
    invisible(list(x = data, y = ypoints))
}

"exindex" <- 
function(data, block, start = 5, end = NA, reverse = FALSE,
    auto.scale = TRUE, labels = TRUE, ...)
{
    sorted <- rev(sort(as.numeric(data)))
    n <- length(sorted)
    if(is.character(block)) {
        times <- as.POSIXlt(attributes(data)$times)
        if(block %in% c("semester", "quarter")) {
            sem <- quart <- times$mon
            sem[sem %in% 0:5] <- quart[quart %in% 0:2] <- 0
            sem[sem %in% 6:11] <- quart[quart %in% 3:5] <- 1
            quart[quart %in% 6:8] <- 2
            quart[quart %in% 9:11] <- 3
        }
        grouping <- switch(block,
            semester = paste(times$year, sem),
            quarter = paste(times$year, quart),
            month = paste(times$year, times$mon),
            year = times$year,
            stop("unknown time period"))
        b.lengths <- as.numeric(tapply(data, grouping, length))
        b.maxima <- as.numeric(tapply(data, grouping, max))
    }
    else {
	data <- as.numeric(data)
	nblocks <- (length(data) %/% block) + 1
	grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
	b.lengths <- tapply(data, grouping, length)
	b.maxima <- tapply(data, grouping, max)
    }
    b.lengths <- b.lengths[!is.na(b.lengths)]
    b.maxima <- rev(sort(b.maxima[!is.na(b.maxima)]))
    if(is.numeric(block)) r <- block
    else r <- round(mean(b.lengths[2:(length(b.lengths) - 1)]))
    k <- round(n/r)
    un <- unique(b.maxima)[-1]
    K <- match(un, b.maxima) - 1
    N <- match(un, sorted) - 1
    if(is.na(end)) end <- k
    cond <- (K < end) & (K >= start)
    un <- un[cond]
    K <- K[cond]
    N <- N[cond]
    theta2 <- K/N
    theta <- logb(1 - K/k)/(r * logb(1 - N/n))
    out <- cbind(N, K, un, theta2, theta)
    yrange <- range(theta)
    index <- K
    if(reverse)	index <-  - K
    if(auto.scale)
        plot(index, theta, ylim = yrange, type = "l", xlab = "", ylab = "",
             axes = FALSE, ...)
    else plot(index, theta, type = "l", xlab = "", ylab = "", axes =
              FALSE, ...)
    axis(1, at = index, lab = paste(K), tick = FALSE)
    axis(2)
    axis(3, at = index, lab = paste(format(signif(un, 3))), tick = FALSE)
    box()
    if(labels) {
      	ylabel <- paste("theta (", k, " blocks of size ", r, ")", sep = "")
	title(xlab = "K", ylab = ylabel)
	mtext("Threshold", side = 3, line = 3)
    }
    invisible(out)
}

"hill" <- 
function(data, option = c("alpha","xi","quantile"), start = 15, end = NA,
    reverse = FALSE, p = NA, ci = 0.95, auto.scale = TRUE, labels = TRUE, ...)
{
    data <- as.numeric(data)
    ordered <- rev(sort(data))
    ordered <- ordered[ordered > 0]
    n <- length(ordered)
    option <- match.arg(option)
    if((option == "quantile") && (is.na(p)))
        stop("Input a value for the probability p")
    if((option == "quantile") && (p < 1 - start/n)) {
	cat("Graph may look strange !! \n\n")
	cat(paste("Suggestion 1: Increase `p' above",
                  format(signif(1 - start/n, 5)), "\n"))
	cat(paste("Suggestion 2: Increase `start' above ",
                  ceiling(length(data) * (1 - p)), "\n"))
    }
    k <- 1:n
    loggs <- logb(ordered)
    avesumlog <- cumsum(loggs)/(1:n)
    xihat <- c(NA, (avesumlog - loggs)[2:n])
    alphahat <- 1/xihat
    y <- switch(option,
	    alpha = alphahat,
	    xi = xihat,
	    quantile = ordered * ((n * (1 - p))/k)^(-1/alphahat))
    ses <- y/sqrt(k)
    if(is.na(end)) end <- n
    x <- trunc(seq(from = min(end, length(data)), to = start))
    y <- y[x]
    ylabel <- option
    yrange <- range(y)
    if(ci && (option != "quantile")) {
       	qq <- qnorm(1 - (1 - ci)/2)
       	u <- y + ses[x] * qq
       	l <- y - ses[x] * qq
       	ylabel <- paste(ylabel, " (CI, p =", ci, ")", sep = "")
       	yrange <- range(u, l)
    }
    if(option == "quantile") ylabel <- paste("Quantile, p =", p)
    index <- x
    if(reverse) index <-  - x
    if(auto.scale)
        plot(index, y, ylim = yrange, type = "l", xlab = "", ylab = "",
	     axes = FALSE, ...)
    else plot(index, y, type = "l", xlab = "", ylab = "", axes = FALSE, ...)
    axis(1, at = index, lab = paste(x), tick = FALSE)
    axis(2)
    threshold <- findthresh(data, x)
    axis(3, at = index, lab = paste(format(signif(threshold, 3))),
         tick = FALSE)
    box()
    if(ci && (option != "quantile")) {
       	lines(index, u, lty = 2, col = 2)
       	lines(index, l, lty = 2, col = 2)
    }
    if(labels) {
       	title(xlab = "Order Statistics", ylab = ylabel)
       	mtext("Threshold", side = 3, line = 3)
    }
    invisible(list(x = index, y = y))
}

"meplot" <- 
function(data, omit = 3, labels = TRUE, ...)
{
    data <- as.numeric(data)
    n <- length(data)
    myrank <- function(x, na.last = TRUE)
    {
        ranks <- sort.list(sort.list(x, na.last = na.last))
	if(is.na(na.last))
	     x <- x[!is.na(x)]
	for(i in unique(x[duplicated(x)])) {
	    which <- x == i & !is.na(x)
	    ranks[which] <- max(ranks[which])
	}
	ranks
    }
    data <- sort(data)
    n.excess <- unique(floor(length(data) - myrank(data)))
    points <- unique(data)
    nl <- length(points)
    n.excess <- n.excess[-nl]
    points <- points[-nl]
    excess <- cumsum(rev(data))[n.excess] - n.excess * points
    y <- excess/n.excess
    xx <- points[1:(nl-omit)] ; yy <- y[1:(nl-omit)]
    plot(xx, yy, xlab = "", ylab = "", ...)
    if(labels) title(xlab = "Threshold", ylab = "Mean Excess")
    invisible(list(x = xx, y = yy))
}

"qplot" <- 
function(data, xi = 0, trim = NA, threshold = NA, line = TRUE,
    labels = TRUE, ...)
{
    data <- as.numeric(data)
    if(!is.na(threshold)) data <- data[data >= threshold]
    if(!is.na(trim)) data <- data[data < trim]
    if(xi == 0) {
        add <- "Exponential Quantiles"
	y <- qexp(ppoints(data))
    }
    if(xi != 0) {
        add <- paste("GPD Quantiles; xi =", xi)
	y <- qgpd(ppoints(data), xi = xi)
    }
    plot(sort(data), y, xlab = "", ylab = "", ...)
    if(labels) title(xlab = "Ordered Data", ylab = add)
    if(line) abline(lsfit(sort(data), y))
    invisible(list(x = sort(data), y = y))
}

"records" <- 
function(data, do.plot = TRUE, conf.level = 0.95, ...)
{
    data <- as.numeric(data)
    record <- cummax(data)
    expected <- cumsum(1/(1:length(data)))
    se <- sqrt(expected - cumsum(1/((1:length(data))^2)))
    trial <- (1:length(data))[!duplicated(record)]
    record <- unique(record)
    number <- 1:length(record)
    expected <- expected[trial]
    se <- se[trial]
    if(do.plot) {
       	ci <- qnorm(0.5 + conf.level/2)
       	upper <- expected + ci * se
       	lower <- expected - ci * se
       	lower[lower < 1] <- 1
       	yr <- range(upper, lower, number)
       	plot(trial, number, log = "x", ylim = yr, xlab = "Trial",
             ylab = "Records", main = "Plot of Record Development", ...)
	lines(trial, expected)
	lines(trial, upper, lty = 2)
	lines(trial, lower, lty = 2)
    }
    data.frame(number, record, trial, expected, se)
}

"gpd" <- 
function(data, threshold = NA, nextremes = NA, method = c("ml","pwm"),
         information = c("observed","expected"), ...)
{
    data <- as.numeric(data)
    n <- length(data)
    if(is.na(nextremes) && is.na(threshold))
        stop("Enter either a threshold or the number of upper extremes")
    if(!is.na(nextremes) && !is.na(threshold))
        stop("Enter EITHER a threshold or the number of upper extremes")
    if(!is.na(nextremes))
        threshold <- findthresh(data, nextremes)
    exceedances <- data[data > threshold]
    excess <- exceedances - threshold
    Nu <- length(excess)
    xbar <- mean(excess)
    method <- match.arg(method)
    if(method == "ml") {
        s2 <- var(excess)
        xi0 <- -0.5 * (((xbar * xbar)/s2) - 1)
        beta0 <- 0.5 * xbar * (((xbar * xbar)/s2) + 1)
        theta <- c(xi0, beta0)
        negloglik <- function(theta, tmp)
        {
       	    xi <- theta[1]
            beta <- theta[2]
	    cond1 <- beta <= 0
	    cond2 <- (xi <= 0) && (max(tmp) > ( - beta/xi))
	    if(cond1 || cond2)
	  	f <- 1e+06
	    else {
	    	y <- logb(1 + (xi * tmp)/beta)
	        y <- y/xi
	        f <- length(tmp) * logb(beta) + (1 + xi) * sum(y)
	    }
	    f
	}
        fit <- optim(theta, negloglik, hessian = TRUE, ..., tmp = excess)
        if(fit$convergence)
            warning("optimization may not have succeeded")
        par.ests <- fit$par
        converged <- fit$convergence
        nllh.final <- fit$value
        information <- match.arg(information)
        if(information == "observed") varcov <- solve(fit$hessian)
        if(information == "expected") {
            one <- (1 + par.ests[1])^2 / Nu
	    two <- (2 * (1 + par.ests[1]) * par.ests[2]^2) / Nu
	    cov <-  - ((1 + par.ests[1]) * par.ests[2]) / Nu
	    varcov <- matrix(c(one, cov, cov, two), 2)
	}
    }
    if(method == "pwm") {
        a0 <- xbar
        gamma <- -0.35
        delta <- 0
        pvec <- ((1:Nu) + delta)/(Nu + delta)
        a1 <- mean(sort(excess) * (1 - pvec))
        xi <- 2 - a0/(a0 - 2 * a1)
        beta <- (2 * a0 * a1)/(a0 - 2 * a1)
        par.ests <- c(xi, beta)
        denom <- Nu * (1 - 2 * xi) * (3 - 2 * xi)
        if(xi > 0.5) {
            denom <- NA
      	    warning("Asymptotic standard errors not available for",
                    "PWM Method when xi > 0.5")
        }
        one <- (1 - xi) * (1 - xi + 2 * xi^2) * (2 - xi)^2
        two <- (7 - 18 * xi + 11 * xi^2 - 2 * xi^3) * beta^2
        cov <- beta * (2 - xi) * (2 - 6 * xi + 7 * xi^2 - 2 * xi^3)
        varcov <- matrix(c(one, cov, cov, two), 2) / denom
        information <- "expected"
        converged <- NA
        nllh.final <- NA
    }
    par.ses <- sqrt(diag(varcov))
    p.less.thresh <- 1 - Nu/n
    out <- list(n = length(data), data = exceedances, threshold =
        threshold, p.less.thresh = p.less.thresh, n.exceed = Nu,
        method = method, par.ests = par.ests, par.ses = par.ses,
        varcov = varcov, information = information, converged =
        converged, nllh.final = nllh.final)
    names(out$par.ests) <- c("xi", "beta")
    names(out$par.ses) <- c("xi", "beta")
    class(out) <- "gpd"
    out
}

"gpd.q" <- 
function(x, pp, ci.type = c("likelihood","wald"), ci.p = 0.95,
         like.num = 50)
{
    if(x$dist != "gpd")
        stop("This function is used only with GPD curves")
    if(length(pp) > 1)
	stop("One probability at a time please")
    threshold <- x$lastfit$threshold
    par.ests <- x$lastfit$par.ests
    xihat <- par.ests["xi"]
    betahat <- par.ests["beta"]
    varcov <- x$lastfit$varcov
    p.less.thresh <- x$lastfit$p.less.thresh
    lambda <- 1
    if(x$type == "tail") lambda <- 1/(1 - p.less.thresh)
    a <- lambda * (1 - pp)
    gfunc <- function(a, xihat) (a^( - xihat) - 1) / xihat
    gfunc.deriv <- function(a, xihat)
        ( - (a^( - xihat) - 1)/xihat - a^( - xihat) * logb(a)) / xihat
    q <- threshold + betahat * gfunc(a, xihat)
    if(q < x$plotmax) abline(v = q, lty = 2)
    out <- as.numeric(q)
    ci.type <- match.arg(ci.type)
    if(ci.type == "wald") {
        if(class(x$lastfit) != "gpd")
	    stop("Wald method requires model be fitted with gpd (not pot)")
	scaling <- threshold
	betahat <- betahat/scaling
	xivar <- varcov[1, 1]
        betavar <- varcov[2, 2]/(scaling^2)
	covar <- varcov[1, 2]/scaling
	term1 <- betavar * (gfunc(a, xihat))^2
	term2 <- xivar * (betahat^2) * (gfunc.deriv(a, xihat))^2
	term3 <- 2 * covar * betavar * gfunc(a, xihat) * gfunc.deriv(a, xihat)
	qvar <- term1 + term2 + term3
	if(qvar < 0) stop("Negative estimate of quantile variance")
	qse <- scaling * sqrt(qvar)
	qq <- qnorm(1 - (1 - ci.p)/2)
	upper <- q + qse * qq
	lower <- q - qse * qq
        if(upper < x$plotmax) abline(v = upper, lty = 2, col = 2)
	if(lower < x$plotmax) abline(v = lower, lty = 2, col = 2)
	out <- as.numeric(c(lower, q, qse, upper))
	names(out) <- c("Lower CI", "Estimate", "Std.Err", "Upper CI")
    }
    if(ci.type == "likelihood") {
        parloglik <- function(theta, tmp, a, threshold, xpi)
	{
	    beta <- (theta * (xpi - threshold))/(a^( - theta) - 1)
	    if((beta <= 0) || ((theta <= 0) && (max(tmp) > ( - beta/theta))))
	        f <- 1e+06
	    else {
	        y <- logb(1 + (theta * tmp)/beta)
		y <- y/theta
		f <- length(tmp) * logb(beta) + (1 + theta) * sum(y)
	    }
	    f
	}
	theta <- xihat
	parmax <- NULL
	xp <- exp(seq(from = logb(threshold), to = logb(x$plotmax),
                      length = like.num))
        excess <- as.numeric(x$lastfit$data - threshold)
	for(i in 1:length(xp)) {
            fit2 <- optim(theta, parloglik, method = "BFGS", hessian = FALSE,
                tmp = excess, a = a, threshold = threshold, xpi = xp[i])
	    parmax <- rbind(parmax, fit2$value)
	}
	parmax <-  - parmax
	overallmax <-  - parloglik(xihat, excess, a, threshold, q)
	crit <- overallmax - qchisq(0.999, 1)/2
	cond <- parmax > crit
	xp <- xp[cond]
	parmax <- parmax[cond]
	par(new = TRUE)
	dolog <- ""
        if(x$alog == "xy" || x$alog == "x") dolog <- "x"
	plot(xp, parmax, type = "n", xlab = "", ylab = "", axes = FALSE,
	     xlim = range(x$plotmin, x$plotmax),
	     ylim = range(overallmax, crit), log = dolog)
	axis(4, at = overallmax - qchisq(c(0.95, 0.99), 1)/2,
             labels = c("95", "99"), tick = TRUE)
	aalpha <- qchisq(ci.p, 1)
	abline(h = overallmax - aalpha/2, lty = 2, col = 2)
	cond <- !is.na(xp) & !is.na(parmax)
	smth <- spline(xp[cond], parmax[cond], n = 200)
	lines(smth, lty = 2, col = 2)
	ci <- smth$x[smth$y > overallmax - aalpha/2]
	out <- c(min(ci), q, max(ci))
	names(out) <- c("Lower CI", "Estimate", "Upper CI")
    }
    out
}

"gpd.sfall" <- 
function(x, pp, ci.p = 0.95, like.num = 50)
{
    if(x$dist != "gpd")
       	stop("This function is used only with GPD curves")
    if(length(pp) > 1)
       	stop("One probability at a time please")
    threshold <- x$lastfit$threshold
    par.ests <- x$lastfit$par.ests
    xihat <- par.ests["xi"]
    betahat <- par.ests["beta"]
    varcov <- x$lastfit$varcov
    p.less.thresh <- x$lastfit$p.less.thresh
    lambda <- 1
    if(x$type == "tail") lambda <- 1/(1 - p.less.thresh)
    a <- lambda * (1 - pp)
    gfunc <- function(a, xihat) (a^( - xihat) - 1) / xihat
    q <- threshold + betahat * gfunc(a, xihat)
    s <- q + (betahat + xihat * (q - threshold))/(1 - xihat)
    if(s < x$plotmax) abline(v = s, lty = 2)
    out <- as.numeric(s)
    parloglik <- function(theta, tmp, a, threshold, xpi)
    {
        beta <- ((1 - theta) * (xpi - threshold)) /
          (((a^( - theta) - 1)/theta) + 1)
	if((beta <= 0) || ((theta <= 0) && (max(tmp) > ( - beta/theta))))
	    f <- 1e+06
	else {
	    y <- logb(1 + (theta * tmp)/beta)
	    y <- y/theta
	    f <- length(tmp) * logb(beta) + (1 + theta) * sum(y)
	}
	f
    }
    theta <- xihat
    parmax <- NULL
    xp <- exp(seq(from = logb(threshold), to = logb(x$plotmax),
                  length = like.num))
    excess <- as.numeric(x$lastfit$data - threshold)
    for(i in 1:length(xp)) {
        fit2 <- optim(theta, parloglik, method = "BFGS", hessian = FALSE,
                tmp = excess, a = a, threshold = threshold, xpi = xp[i])
        parmax <- rbind(parmax, fit2$value)
    }
    parmax <-  - parmax
    overallmax <-  - parloglik(xihat, excess, a, threshold, s)
    crit <- overallmax - qchisq(0.999, 1)/2
    cond <- parmax > crit
    xp <- xp[cond]
    parmax <- parmax[cond]
    par(new = TRUE)
    dolog <- ""
    if(x$alog == "xy" || x$alog == "x") dolog <- "x"
    plot(xp, parmax, type = "n", xlab = "", ylab = "", axes = FALSE, xlim = 
	 range(x$plotmin, x$plotmax), ylim =
         range(overallmax, crit), log = dolog)
    axis(4, at = overallmax - qchisq(c(0.95, 0.99), 1)/2,
         labels = c("95", "99"), tick = TRUE)
    aalpha <- qchisq(ci.p, 1)
    abline(h = overallmax - aalpha/2, lty = 2, col = 2)
    cond <- !is.na(xp) & !is.na(parmax)
    smth <- spline(xp[cond], parmax[cond], n = 200)
    lines(smth, lty = 2, col = 2)
    ci <- smth$x[smth$y > overallmax - aalpha/2]
    out <- c(min(ci), s, max(ci))
    names(out) <- c("Lower CI", "Estimate", "Upper CI")
    out
}

"plot.gpd" <- 
function(x, optlog = NA, extend = 1.5, labels = TRUE, ...)
{
    data <- as.numeric(x$data)
    threshold <- x$threshold
    xi <- x$par.ests["xi"]
    beta <- x$par.ests["beta"]
    choices <- c("Excess Distribution", "Tail of Underlying Distribution",
      	"Scatterplot of Residuals", "QQplot of Residuals")
    tmenu <- paste("plot:", choices)
    pick <- 1
    lastcurve <- NULL
    while(pick > 0) {
        pick <- menu(tmenu, title =
                     "\nMake a plot selection (or 0 to exit):")
        if(pick >= 3) {
            excess <- data - threshold
       	    res <- logb(1 + (xi * excess)/beta) / xi
            lastcurve <- NULL
	}
        if(pick == 3) {
      	    plot(res, ylab = "Residuals", xlab = "Ordering", ...)
	    lines(lowess(1:length(res), res))
	}
        if(pick == 4) qplot(res, ...)
        if(pick == 1 || pick == 2) {
            plotmin <- threshold
     	    if(extend <= 1) stop("extend must be > 1")
	    plotmax <- max(data) * extend
            xx <- seq(from = 0, to = 1, length = 1000)
      	    z <- qgpd(xx, xi, threshold, beta)
       	    z <- pmax(pmin(z, plotmax), plotmin)
       	    ypoints <- ppoints(sort(data))
       	    y <- pgpd(z, xi, threshold, beta)
	}
	if(pick == 1) {
	    type <- "eplot"
	    if(!is.na(optlog))
                alog <- optlog
       	    else alog <- "x"
	    if(alog == "xy")
	        stop("Double log plot of Fu(x-u) does\nnot make much sense")
	    yylab <- "Fu(x-u)"
       	    shape <- xi
	    scale <- beta
	    location <- threshold
	}
	if(pick == 2) {
	    type <- "tail"
	    if(!is.na(optlog))
	        alog <- optlog
	    else alog <- "xy"
	    prob <- x$p.less.thresh
	    ypoints <- (1 - prob) * (1 - ypoints)
	    y <- (1 - prob) * (1 - y)
	    yylab <- "1-F(x)"
	    shape <- xi
	    scale <- beta * (1 - prob)^xi
	    location <- threshold - (scale * ((1 - prob)^( - xi) - 1))/xi
	}
	if(pick == 1 || pick == 2) {
	    plot(sort(data), ypoints, xlim = range(plotmin, plotmax),
                 ylim = range(ypoints, y, na.rm = TRUE), xlab = "",
                 ylab = "", log = alog, axes = TRUE, ...)
	    lines(z[y >= 0], y[y >= 0])
	    if(labels) {
	        xxlab <- "x"
		if(alog == "x" || alog == "xy" || alog == "yx")
		    xxlab <- paste(xxlab, "(on log scale)")
	        if(alog == "xy" || alog == "yx" || alog == "y")
		    yylab <- paste(yylab, "(on log scale)")
		title(xlab = xxlab, ylab = yylab)
	    }
	    details <- paste("threshold = ", format(signif(threshold, 3)),
                             "   xi = ", format(signif(shape, 3)),
                             "   scale = ", format(signif(scale, 3)),
                             "   location = ", format(signif(location, 3)),
                             sep = "")
	    print(details)
	    lastcurve <- list(lastfit = x, type = type, dist = "gpd",
                plotmin = plotmin, plotmax = plotmax, alog = alog,
                location = as.numeric(location), shape = as.numeric(shape),
                scale = as.numeric(scale))
	}
    }
    invisible(lastcurve)
}

"quant" <- 
function(data, p = 0.99, models = 30, start = 15, end = 500,
	reverse = TRUE, ci = 0.95, auto.scale = TRUE, labels = TRUE,
	...)
{
    data <- as.numeric(data)
    n <- length(data)
    if(ci) qq <- qnorm(1 - (1 - ci)/2)
    exceed <- trunc(seq(from = min(end, n), to = start, length = models))
    if(p < 1 - min(exceed)/n) {
        cat("Graph may look strange !! \n\n")
	cat(paste("Suggestion 1: Increase `p' above",
            format(signif(1 - min(exceed)/n, 5)), "\n"))
	cat(paste("Suggestion 2: Increase `start' above ",
            ceiling(length(data) * (1 - p)), "\n"))
    }
    gpd.dummy <- function(nex, data)
    {
	out <- gpd(data = data, nex = nex, information = "expected")
	c(out$threshold, out$par.ests[1], out$par.ests[2],
          out$varcov[1, 1], out$varcov[2, 2], out$varcov[1, 2])
    }
    mat <- apply(as.matrix(exceed), 1, gpd.dummy, data = data)
    thresh <- mat[1,  ]
    xihat <- mat[2,  ]
    betahat <- mat[3,  ]
    lambda <- length(data)/exceed
    a <- lambda * (1 - p)
    gfunc <- function(a, xihat) (a^( - xihat) - 1) / xihat
    qest <- thresh + betahat * gfunc(a, xihat)
    l <- u <- qest
    yrange <- range(qest)
    if(ci) {
        xivar <- mat[4,  ]
	betavar <- mat[5,  ]
	covar <- mat[6,  ]
	scaling <- thresh
	betahat <- betahat/scaling
	betavar <- betavar/(scaling^2)
	covar <- covar/scaling
	gfunc.deriv <- function(a, xihat)
	    ( - (a^( - xihat) - 1)/xihat - a^( - xihat) * logb(a)) / xihat
	term1 <- betavar * (gfunc(a, xihat))^2
	term2 <- xivar * (betahat^2) * (gfunc.deriv(a, xihat))^2
	term3 <- 2 * covar * betavar * gfunc(a, xihat) * gfunc.deriv(a, xihat)
	qvar <- term1 + term2 + term3
	if(min(qvar) < 0)
	    stop(paste("Conditioning problems lead to estimated negative",
                       "quantile variance", sep = "\n"))
	qse <- scaling * sqrt(qvar)
	u <- qest + qse * qq
	l <- qest - qse * qq
	yrange <- range(qest, u, l)
    }
    mat <- rbind(thresh, qest, exceed, l, u)
    dimnames(mat) <- list(c("threshold", "qest", "exceedances", "lower",
        "upper"), NULL)
    index <- exceed
    if(reverse) index <-  - exceed
    if(auto.scale)
        plot(index, qest, ylim = yrange, type = "l", xlab = "", ylab = "",
             axes = FALSE, ...)
    else plot(index, qest, type = "l", xlab = "", ylab = "",
              axes = FALSE, ...)
    axis(1, at = index, lab = paste(exceed))
    axis(2)
    axis(3, at = index, lab = paste(format(signif(thresh, 3))))
    box()
    if(ci) {
       	lines(index, l, lty = 2, col = 2)
       	lines(index, u, lty = 2, col = 2)
    }
    if(labels) {
       	labely <- paste(p, "Quantile")
       	if(ci) labely <- paste(labely, " (CI, p = ", ci, ")", sep = "")
	title(xlab = "Exceedances", ylab = labely)
	mtext("Threshold", side = 3, line = 3)
    }
    invisible(mat)
}

"riskmeasures" <- 
function(x, p)
{
    u <- x$threshold
    par.ests <- x$par.ests
    xihat <- par.ests["xi"]
    betahat <- par.ests["beta"]
    p.less.thresh <- x$p.less.thresh
    lambda <- 1/(1 - p.less.thresh)
    quant <- function(pp, xi, beta, u, lambda)
    {
     	a <- lambda * (1 - pp)
       	u + (beta * (a^( - xi) - 1))/xi
    }
    short <- function(pp, xi, beta, u, lambda)
    {
      	a <- lambda * (1 - pp)
       	q <- u + (beta * (a^( - xi) - 1))/xi
       	(q * (1 + (beta - xi * u)/q)) / (1 - xi)
    }
    q <- quant(p, xihat, betahat, u, lambda)
    es <- short(p, xihat, betahat, u, lambda)
    rtn <- cbind(p, quantile = q, sfall = es)
    row.names(rtn) <- NULL
    rtn
}

"shape" <- 
function(data, models = 30, start = 15, end = 500, reverse = TRUE, ci = 
	0.95, auto.scale = TRUE, labels = TRUE, ...)
{
    data <- as.numeric(data)
    qq <- 0
    if(ci) qq <- qnorm(1 - (1 - ci)/2)
    x <- trunc(seq(from = min(end, length(data)), to = start, length = models))
    gpd.dummy <- function(nex, data)
    {
        out <- gpd(data = data, nex = nex, information = "expected")
	c(out$threshold, out$par.ests[1], out$par.ses[1])
    }
    mat <- apply(as.matrix(x), 1, gpd.dummy, data = data)
    mat <- rbind(mat, x)
    dimnames(mat) <- list(c("threshold", "shape", "se", "exceedances"), NULL)
    thresh <- mat[1,  ]
    y <- mat[2,  ]
    yrange <- range(y)
    if(ci) {
        u <- y + mat[3,  ] * qq
	l <- y - mat[3,  ] * qq
	yrange <- range(y, u, l)
    }
    index <- x
    if(reverse) index <-  - x
    if(auto.scale)
        plot(index, y, ylim = yrange, type = "l", xlab = "", ylab = "",
	     axes = FALSE, ...)
    else plot(index, y, type = "l", xlab = "", ylab = "", axes = FALSE, ...)
    axis(1, at = index, lab = paste(x), tick = FALSE)
    axis(2)
    axis(3, at = index, lab = paste(format(signif(thresh, 3))), tick = FALSE)
    box()
    if(ci) {
        lines(index, u, lty = 2, col = 2)
	lines(index, l, lty = 2, col = 2)
    }
    if(labels) {
        labely <- "Shape (xi)"
	if(ci) labely <- paste(labely, " (CI, p = ", ci, ")", sep = "")
	title(xlab = "Exceedances", ylab = labely)
	mtext("Threshold", side = 3, line = 3)
    }
    invisible(mat)
}

"tailplot" <- 
function(x, optlog = NA, extend = 1.5, labels = TRUE, ...)
{
    data <- as.numeric(x$data)
    threshold <- x$threshold
    xi <- x$par.ests["xi"]
    beta <- x$par.ests["beta"]
    plotmin <- threshold
    if(extend <= 1) stop("extend must be > 1")
    plotmax <- max(data) * extend
    xx <- seq(from = 0, to = 1, length = 1000)
    z <- qgpd(xx, xi, threshold, beta)
    z <- pmax(pmin(z, plotmax), plotmin)
    ypoints <- ppoints(sort(data))
    y <- pgpd(z, xi, threshold, beta)
    type <- "tail"
    if(!is.na(optlog))
	    alog <- optlog
    else alog <- "xy"
    prob <- x$p.less.thresh
    ypoints <- (1 - prob) * (1 - ypoints)
    y <- (1 - prob) * (1 - y)
    yylab <- "1-F(x)"
    shape <- xi
    scale <- beta * (1 - prob)^xi
    location <- threshold - (scale * ((1 - prob)^( - xi) - 1))/xi
    plot(sort(data), ypoints, xlim = range(plotmin, plotmax), ylim =
         range(ypoints, y, na.rm = TRUE), xlab = "", ylab = "", log = alog,
         axes = TRUE, ...)
    lines(z[y >= 0], y[y >= 0])
    if(labels) {
        xxlab <- "x"
        if(alog == "x" || alog == "xy" || alog == "yx")
	    xxlab <- paste(xxlab, "(on log scale)")
        if(alog == "xy" || alog == "yx" || alog == "y")
	    yylab <- paste(yylab, "(on log scale)")
        title(xlab = xxlab, ylab = yylab)
    }
    lastcurve <- list(lastfit = x, type = type, dist = "gpd",
        plotmin = plotmin, plotmax = plotmax, alog = alog, location = 
	as.numeric(location), shape = as.numeric(shape), scale = 
	as.numeric(scale))
    invisible(lastcurve)
}
"plot.pot" <- 
function(x, ...)
{
    rawdata <- x$data
    n <- length(as.numeric(rawdata))
    times <- attributes(rawdata)$times
    if(is.character(times) || inherits(times, "POSIXt") ||
       inherits(x, "date") || inherits(x, "dates")) {
        times <- as.POSIXlt(times)
        gaps <- as.numeric(difftime(times[2:n], times[1:(n-1)],
            units = "days")) * x$intensity
    }
    else gaps <- as.numeric(diff(times)) * x$intensity
    data <- as.numeric(rawdata)
    threshold <- x$threshold
    par.ests <- x$par.ests
    xi <- par.ests[1]
    beta <- par.ests[4]
    residuals <- logb(1 + (xi * (data - threshold))/beta)/xi
    choices <- c("Point Process of Exceedances", "Scatterplot of Gaps",
		 "Qplot of Gaps", "ACF of Gaps", "Scatterplot of Residuals",
		 "Qplot of Residuals", "ACF of Residuals", "Go to GPD Plots")
    tmenu <- paste("plot:", choices)
    pick <- 1
    lastcurve <- NULL
    while(pick > 0) {
        pick <- menu(tmenu, title = 
		     "\nMake a plot selection (or 0 to exit):")
        if(pick %in% c(4,7)) require("ts", quietly = TRUE)
        if(pick %in% 1:7) lastcurve <- NULL
	switch(pick,
            {
	       plot(times, rawdata, type = "h", sub = paste("Point process of",
	         length(as.numeric(rawdata)), "exceedances of threshold",
                 format(signif(threshold, 3))), ...)  
            },
	    {
	      plot(gaps, ylab = "Gaps", xlab = "Ordering", ...)
	      lines(lowess(1:length(gaps), gaps))
	    },
	      qplot(gaps, ...),
	      acf(gaps, lag.max = 20, ...),
	    {
	      plot(residuals, ylab = "Residuals", xlab = "Ordering", ...)
	      lines(lowess(1:length(residuals), residuals))
	    },
	      qplot(residuals, ...),
	      acf(residuals, lag.max = 20, ...),
	      lastcurve <- plot.gpd(x, ...))
    }
    invisible(lastcurve)
}

"pot" <- 
function(data, threshold = NA, nextremes = NA, run = NA,
    picture = TRUE, ...)
{
    n <- length(as.numeric(data))
    times <- attributes(data)$times
    if(is.null(times)) {
        times <- 1:n
        attributes(data)$times <- times
        start <- 1
        end <- n
        span <- end - start
    }
    else {
        start <- times[1]
        end <- times[n]
        span <- as.numeric(difftime(as.POSIXlt(times)[n],
            as.POSIXlt(times)[1], units = "days"))
    }
  
    if(is.na(nextremes) && is.na(threshold))
       	stop("Enter either a threshold or the number of upper extremes")
    if(!is.na(nextremes) && !is.na(threshold))
	stop("Enter EITHER a threshold or the number of upper extremes")
    if(!is.na(nextremes))
	threshold <- findthresh(as.numeric(data), nextremes)
    if(threshold > 10) {
	factor <- 10^(floor(log10(threshold)))
	cat(paste("If singularity problems occur divide data",
                  "by a factor, perhaps", factor, "\n"))
    }
    exceedances.its <- structure(data[data > threshold], times =
        times[data > threshold])
    n.exceed <- length(as.numeric(exceedances.its))
    p.less.thresh <- 1 - n.exceed/n
    if(!is.na(run)) {
       	exceedances.its <- decluster(exceedances.its, run, picture)
       	n.exceed <- length(exceedances.its)
    }
    intensity <- n.exceed/span
    exceedances <- as.numeric(exceedances.its)
    xbar <- mean(exceedances) - threshold
    s2 <- var(exceedances)
    shape0 <- -0.5 * (((xbar * xbar)/s2) - 1)
    extra <- ((length(exceedances)/span)^( - shape0) - 1)/shape0
    betahat <- 0.5 * xbar * (((xbar * xbar)/s2) + 1)
    scale0 <- betahat/(1 + shape0 * extra)
    loc0 <- 0
    theta <- c(shape0, scale0, loc0)
    negloglik <- function(theta, exceedances, threshold, span)
    {
        if((theta[2] <= 0) || (min(1 + (theta[1] * (exceedances -
            theta[3])) / theta[2]) <= 0))
	    f <- 1e+06
	else {
	    y <- logb(1 + (theta[1] * (exceedances - theta[3])) / theta[2])
	    term3 <- (1/theta[1] + 1) * sum(y)
	    term1 <- span * (1 + (theta[1] * (threshold - theta[3])) /
                         theta[2])^(-1/theta[1])
	    term2 <- length(y) * logb(theta[2])
	    f <- term1 + term2 + term3
	}
	f
    }
    fit <- optim(theta, negloglik, hessian = TRUE, ..., exceedances =
                 exceedances, threshold = threshold, span = span)
    if(fit$convergence)
        warning("optimization may not have succeeded")
    par.ests <- fit$par
    varcov <- solve(fit$hessian)
    par.ses <- sqrt(diag(varcov))   
    beta <- par.ests[2] + par.ests[1] * (threshold - par.ests[3])
    par.ests <- c(par.ests, beta)
    out <- list(n = length(data), period = c(start, end), data = 
        exceedances.its, span = span, threshold = threshold,
        p.less.thresh = p.less.thresh, n.exceed = n.exceed, run = run,
	par.ests = par.ests, par.ses = par.ses, varcov = varcov, 
	intensity = intensity, nllh.final = fit$value, converged
	= fit$convergence)
    names(out$par.ests) <- c("xi", "sigma", "mu", "beta")
    names(out$par.ses) <- c("xi", "sigma", "mu")
    class(out) <- "pot"
    out
}

"decluster" <- 
function(series, run = NA, picture = TRUE)
{
    n <- length(as.numeric(series))
    times <- attributes(series)$times
    if(is.null(times)) stop("`series' must have a `times' attribute")
    as.posix <- is.character(times) || inherits(times, "POSIXt") ||
      inherits(times, "date") || inherits(times, "dates")
    if(as.posix)
        gaps <- as.numeric(difftime(as.POSIXlt(times)[2:n],
            as.POSIXlt(times)[1:(n-1)], units = "days"))
    else gaps <- as.numeric(diff(times))
    longgaps <- gaps > run
    if(sum(longgaps) <= 1)
        stop("Decluster parameter too large")
    cluster <- c(0, cumsum(longgaps))
    cmax <- tapply(as.numeric(series), cluster, max)
    newtimes <- times[match(cmax, series)]
    newseries <- structure(series[match(cmax, series)], times = newtimes)
    n <- length(as.numeric(newseries))

    if(as.posix) {
        newgaps <- as.numeric(difftime(as.POSIXlt(newtimes)[2:n],
            as.POSIXlt(newtimes)[1:(n-1)], units = "days"))
        times <- as.POSIXlt(times)
        newtimes <- as.POSIXlt(newtimes)
    }
    else newgaps <- as.numeric(diff(newtimes))
    
    if(picture) {
      	cat("Declustering picture...\n")
       	cat(paste("Data reduced from", length(as.numeric(series)),
       		"to", length(as.numeric(newseries)), "\n"))
       	par(mfrow = c(2, 2))
        plot(times, series, type = "h")
	qplot(gaps)
        plot(newtimes, newseries, type = "h")
       	qplot(newgaps)
       	par(mfrow = c(1, 1))
    }
    newseries
}

"findthresh" <- 
function(data, ne)
{
	data <- rev(sort(as.numeric(data)))
	thresholds <- unique(data)
	indices <- match(data[ne], thresholds)
	indices <- pmin(indices + 1, length(thresholds))
	thresholds[indices]
}


# ******************************************************************************

