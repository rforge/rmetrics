

# Slightly Modified Copy from:


# Package: mvtnorm
# Title: Multivariate Normal and T Distribution 
# Version: 0.7-1
# Date: $Date: 2004/10/14 13:40:21 $
# Author: Alan Genz <AlanGenz@wsu.edu>, 
#   Frank Bretz <frank.bretz@pharma.novartis.com>, 
#   R port by Torsten Hothorn <Torsten.Hothorn@rzmail.uni-erlangen.de>
# Maintainer: Torsten Hothorn <Torsten.Hothorn@rzmail.uni-erlangen.de>
# Description: computes the multivariate normal and t distribution
# Depends: R(>= 1.9.0)
# License: GPL
# Packaged: Thu Nov 18 13:36:44 2004; hothorn


################################################################################
# mvnorm


# $Id: mvnorm.R,v 1.4 2004/01/22 14:30:50 hothorn Exp $

.rmvnorm = 
function(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)))
{
    if(nrow(sigma) != ncol(sigma)){
        stop("sigma must be a square matrix")
    }

    if(length(mean) != nrow(sigma)){
        stop("mean and sigma have non-conforming size")
    }

    ev = eigen(sigma, sym = TRUE)$values
    if (!all(ev >= -sqrt(.Machine$double.eps) * abs(ev[1])))   
        warning("sigma is numerically not positive definite")

    sigsvd = svd(sigma)
    retval = t(sigsvd$v %*% (t(sigsvd$u) * sqrt(sigsvd$d)))
    retval = matrix(rnorm(n * ncol(sigma)), nrow = n) %*% retval
    retval = sweep(retval, 2, mean, "+")
    retval
}


# ------------------------------------------------------------------------------


.dmvnorm = 
function (x, mean, sigma, log = FALSE)
{
    if (is.vector(x)) {
        x = matrix(x, ncol = length(x))
    }
    if (missing(mean)) {
        mean = rep(0, length = ncol(x))
    }
    if (missing(sigma)) {
        sigma = diag(ncol(x))
    }
    if (ncol(x) != ncol(sigma)) {
        stop("x and sigma have non-conforming size")
    }
    if (nrow(sigma) != ncol(sigma)) {
        stop("sigma meanst be a square matrix")
    }
    if (length(mean) != nrow(sigma)) {
        stop("mean and sigma have non-conforming size")
    }
    distval = mahalanobis(x, center = mean, cov = sigma)
    logdet = sum(log(eigen(sigma, symmetric=TRUE,
                                   only.values=TRUE)$values))
    logretval = -(ncol(x)*log(2*pi) + logdet + distval)/2
    if(log) return(logretval)
    exp(logretval)
}
  

# ******************************************************************************


# $Id: mvt.R,v 1.30 2004/09/08 07:38:43 hothorn Exp $ 

.checkmvArgs = 
function(lower, upper, mean, corr, sigma) 
{
    UNI = FALSE
    if (is.null(lower) || any(is.na(lower)))
        stop(sQuote("lower"), " not specified or contains NA")
    if (is.null(upper) || any(is.na(upper)))
        stop(sQuote("upper"), " not specified or contains NA")
    rec = cbind(lower, upper, mean)
    lower = rec[,"lower"]
    upper = rec[,"upper"]
    if (!all(lower <= upper))
        stop("at least one element of ", sQuote("lower"), " is larger than ", 
             sQuote("upper"))
    mean = rec[,"mean"]
    if (any(is.na(mean)))
        stop("mean contains NA")
    if (is.null(corr) && is.null(sigma)) {
        corr = diag(length(lower))
        # warning("both ", sQuote("corr"), " and ", sQuote("sigma"),
        # " not specified: using sigma=diag(length(lower))")
    }
    if (!is.null(corr) && !is.null(sigma)) {
        sigma = NULL
        warning("both ", sQuote("corr"), " and ", sQuote("sigma"), 
                " specified: ignoring ", sQuote("sigma"))
    }
    if (!is.null(corr)) {
         if (!is.matrix(corr)) {
             if (length(corr) == 1)
                UNI = TRUE
             if (length(corr) != length(lower))
               stop(sQuote("diag(corr)"), " and ", sQuote("lower"), 
                    " are of different length")
         } else {
             if (length(corr) == 1) {
                 UNI = TRUE
                 corr = corr[1,1]
                 if (length(lower) != 1)
                   stop(sQuote("corr"), " and ", sQuote("lower"), 
                        " are of different length")
             } else {
                 if (length(diag(corr)) != length(lower))        
                     stop(sQuote("diag(corr)"), " and ", sQuote("lower"), 
                          " are of different length")
             }
         }
    }
    if (!is.null(sigma)) {
         if (!is.matrix(sigma)) {
            if (length(sigma) == 1)
                UNI = TRUE
            if (length(sigma) != length(lower))        
               stop(sQuote("diag(sigma)"), " and ", sQuote("lower"), 
                    " are of different length")
         } else {
            if (length(sigma) == 1) {
                UNI = TRUE       
                sigma = sigma[1,1]
                if (length(lower) != 1) 
                  stop(sQuote("sigma"), " and ", sQuote("lower"), 
                       " are of different length")
            } else {
              if (length(diag(sigma)) != length(lower))                     
                 stop(sQuote("diag(sigma)"), " and ", sQuote("lower"), 
                      " are of different length")
            }
         }
    }
    list(lower=lower, upper=upper, mean=mean, corr=corr, sigma=sigma, uni=UNI)
}


# ------------------------------------------------------------------------------


.pmvnorm = 
function(lower=-Inf, upper=Inf, mean=rep(0, length(lower)), 
corr=NULL, sigma=NULL, maxpts = 25000, abseps = 0.001, releps = 0)
{
    carg = .checkmvArgs(lower=lower, upper=upper, mean=mean, corr=corr,
                      sigma=sigma)
    if (!is.null(carg$corr)) {
      corr = carg$corr
      if (carg$uni) {
          stop(sQuote("sigma"), " not specified: cannot compute pnorm")
      } else {
          lower = carg$lower - carg$mean
          upper = carg$upper - carg$mean
          mean = rep(0, length(lower))
          RET = .mvt(lower=lower, upper=upper, df=0, corr=corr, delta=mean,
                     maxpts=maxpts, abseps=abseps,releps=releps)
      }
    } else {
      if (carg$uni) {
        RET = list(value = pnorm(carg$upper, mean=carg$mean, 
                                sd=sqrt(carg$sigma)) -
                            pnorm(carg$lower, mean=carg$mean, 
                                sd=sqrt(carg$sigma)),
                    error = 0, msg="univariate: using pnorm")
      } else {
          lower = (carg$lower - carg$mean)/sqrt(diag(carg$sigma))
          upper = (carg$upper - carg$mean)/sqrt(diag(carg$sigma))
          mean = rep(0, length(lower))
          corr = cov2cor(carg$sigma)
          RET = .mvt(lower=lower, upper=upper, df=0, corr=corr, delta=mean,
                     maxpts=maxpts, abseps=abseps,releps=releps)
      }
    }
    attr(RET$value, "error") = RET$error
    attr(RET$value, "msg") = RET$msg
    return(RET$value)
}


# ------------------------------------------------------------------------------


.pmvt = 
function(lower = -Inf, upper = Inf, delta = rep(0, length(lower)),
df = 1, corr = NULL, sigma = NULL, maxpts = 25000, abseps = 0.001, 
releps = 0)
{
    carg = .checkmvArgs(lower=lower, upper=upper, mean=delta, corr=corr,
                       sigma=sigma)
    if (is.null(df))
        stop(sQuote("df"), " not specified")
    if (any(df < 0))
        stop("cannot compute multivariate t distribution with ",
             sQuote("df"), " < 0")
    if (carg$uni) {
        if (df > 0)
            RET = list(value = pt(carg$upper, df=df, ncp=carg$mean) -
                                pt(carg$lower, df=df, ncp=carg$mean),
                       error = 0, msg="univariate: using pt")
        else
            RET = list(value = pnorm(carg$upper, mean = carg$mean) -
                                pnorm(carg$lower, mean=carg$mean),
                       error = 0, msg="univariate: using pnorm")
    } else {
        if (!is.null(carg$corr)) {
            RET = .mvt(lower=carg$lower, upper=carg$upper, df=df, 
                        corr=carg$corr,
                        delta=carg$mean,  maxpts=maxpts,
                        abseps=abseps,releps=releps)
        } else {
            lower = carg$lower/sqrt(diag(carg$sigma))
            upper = carg$upper/sqrt(diag(carg$sigma))
            corr = cov2cor(carg$sigma)
            RET = .mvt(lower=lower, upper=upper, df=df, corr=corr,
                        delta=carg$mean, maxpts=maxpts,
                        abseps=abseps,releps=releps)
        }
    }
    attr(RET$value, "error") = RET$error
    attr(RET$value, "msg") = RET$msg
    return(RET$value)
}


# ------------------------------------------------------------------------------


.mvt = 
function(lower, upper, df, corr, delta, maxpts = 25000,
abseps = 0.001, releps = 0)
{
    ### handle cases where the support is the empty set
    if (any(abs(lower - upper)) < sqrt(.Machine$double.eps) || 
        any(is.na(lower - upper))) {
        RET = list(value = 0, error = 0, msg = "lower == upper")
        return(RET)
    }

    n = ncol(corr)
    if (is.null(n) || n < 2) stop("dimension less then n = 2")

    if (length(lower) != n) stop("wrong dimensions")
    if (length(upper) != n) stop("wrong dimensions")

    if (n > 1000) stop("only dimensions 1 <= n <= 1000 allowed") 

    infin = rep(2, n)
    infin[upper == Inf] = 1
    infin[lower == -Inf] = 0
    infin[lower == -Inf & upper == Inf] = -1
    
    if (n > 1) {
        corrF = matrix(as.vector(corr), ncol = n, byrow = TRUE)
        corrF = corrF[upper.tri(corrF)]
    } else 
        corrF = corr 

    lower[lower == -Inf] = 0
    upper[upper == Inf] = 0

    error = 0; value = 0; inform = 0

    ### TOL argument re-added in version 0.6-3
    ### not yet exported

    tol = 1.0e-10

    ret = .Fortran("mvtdst", 
        N = as.integer(n), 
        NU = as.integer(df),
        LOWER = as.double(lower), 
        UPPER = as.double(upper), 
        INFIN = as.integer(infin),
        CORREL = as.double(corrF), 
        DELTA = as.double(delta), 
        MAXPTS = as.integer(maxpts),
        ABSEPS = as.double(abseps), 
        RELEPS = as.double(releps),  
        TOL = as.double(tol),
        error = as.double(error), 
        value = as.double(value),
        inform = as.integer(inform), PACKAGE = "fMultivar")
    
    error = ret$error; value = ret$value; inform = ret$inform

    msg = NULL
    if (inform == 0) msg = "Normal Completion"
    if (inform == 1) msg = "Completion with error > abseps"
    if (inform == 2) msg = "N greater 1000 or N < 1"
    if (inform == 3) msg = "Covariance matrix not positive semidefinite"
    if (is.null(msg)) msg = inform
    
    RET = list(value = value, error = error, msg = msg)
    return(RET)
}


# ------------------------------------------------------------------------------


.rmvt = 
function(n, sigma = diag(2), df = 1) 
{
  .rmvnorm(n,sigma=sigma)/sqrt(rchisq(n,df)/df)
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

