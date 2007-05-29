
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


################################################################################
# FUNCTION:             INTERNAL USE:
#  .cov.nnve             Builtin from Package 'covRobust'
#  .cov.shrink           Builtin from Package 'corpcor'
#  .cov.bagged           Builtin from Package 'corpcor'
################################################################################


################################################################################
#   Package: covRobust
#   Title: Robust Covariance Estimation via Nearest Neighbor Cleaning
#   Version: 1.0
#   Author: Naisyin Wang <nwang@stat.tamu.edu> and
#       Adrian Raftery <raftery@stat.washington.edu>
#       with contributions from Chris Fraley <fraley@stat.washington.edu>
#   Description: The cov.nnve() function for robust covariance estimation
#       by the nearest neighbor variance estimation (NNVE) method
#       of Wang and Raftery (2002,JASA)
#   Maintainer: Naisyin Wang <nwang@stat.tamu.edu>
#   License: GPL version 2 or newer
#   Notes:
#       Wang and Raftery(2002), "Nearest neighbor variance estimation (NNVE):
#           Robust covariance estimation via nearest neighbor cleaning
#           (with discussion)", 
#           Journal of the American Statistical Association 97:994-1019
#       Available as Technical Report 368 (2000) from
#           http://www.stat.washington.edu/www/research/report
    

.cov.nnve =
function(datamat, k = 12, pnoise = 0.05, emconv = 0.001, bound = 1.5, 
extension = TRUE, devsm = 0.01)
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Function to perform Nearest Neighbor Variance Estimation
    
    # Arguments:
    #   cov - robust covariance estimate
    #   mu - mean
    #   postprob - posterior probability
    #   classification - classification (0 = noise,  
    #       otherwise 1) (obtained by rounding postprob)
    #   innc - list of initial nearest-neighbor results (components 
    #       are the same as above)     
    
    # FUNCTION:
    
    # Settings:
    datamat = as.matrix(datamat)
    d = dim(datamat)[2]
    n = dim(datamat)[1]
    pd = dim(datamat)[2]
    S.mean = apply(datamat, 2, median)
    S.sd = apply(datamat, 2, mad)

    #  NNC based on original data
    orgNNC = .cov.nne.nclean.sub(datamat, k, convergence = 0.001, 
        S.mean = S.mean, S.sd = S.sd)
    nnoise = min(c(sum(1 - orgNNC$z), round(pnoise * n)))
    knnd = orgNNC$kthNND
    ord = (n + 1) - rank(knnd)
    muT = orgNNC$mu1
    SigT = orgNNC$Sig1
    SigT = (SigT + t(SigT))/2.
    SigTN = diag(orgNNC$sd1^2)
    if (nnoise > 6) {
        ncho = nnoise
        ncho1 = floor(ncho/2)
        ncho2 = ncho - ncho1
        cho = (1:n)[ord <= ncho1]
        xcho = datamat[cho, ]
        ev = eigen(SigT)
        evv = ev$values
        minv = max((1:d)[evv > 9.9999999999999998e-13])
        if (minv > 2) {
            vv1 = ev$vectors[, (minv - 1)]
            vv2 = ev$vectors[, minv]
        } else {
            vv1 = ev$vectors[, 1]
            vv2 = ev$vectors[, 2]
        }
        ot = acos(sum(vv1 * vv2)/(sum(vv1^2) * sum(vv2^2))^0.5)
        for (kk1 in 1:(ncho2)) {
            pseg = 1/(ncho2 + 1) * kk1 * ot
            xcho = rbind(xcho, (sin(pseg) * vv1 + cos(pseg) * vv2 + muT))
        }
    } else {
        nnoise = 3
        cho = (1:n)[ord <= nnoise]
        xcho = datamat[cho, ]
    }
    
    n2 = (dim(xcho))[1]
    schox = mahalanobis(xcho, muT, SigTN)
    Nc = matrix(rep(muT, n2), nrow = n2, byrow = TRUE)
    Ndir = (xcho - Nc)/(schox^0.5)

    # initial set up
    ch1 = c(qchisq(0.99, pd), qchisq(1 - 10^(-4), pd))
    Xa = seq(ch1[1], ch1[2], length = 6)
    gap = Xa[2] - Xa[1]
    initv = diag(orgNNC$Sig1)
    xa = Xa[1]
    SaveM = c(xa, orgNNC$mu1, .cov.nne.Mtovec(orgNNC$Sig1))
    OldP = orgNNC$probs
    SaveP = OldP
    Np = Nc - Ndir * (xa^0.5)
    updNNC = .cov.nne.nclean.sub(rbind(datamat, Np), k, convergence = 0.001, 
        S.mean = S.mean, S.sd = S.sd)
    SaveM = rbind(SaveM, c(xa, updNNC$mu1, .cov.nne.Mtovec(updNNC$Sig1)))
    SaveP = rbind(SaveP, (updNNC$probs)[1:n])
    
    # sda = .cov.nne.Mtovec(orgNNC$Sig1)  
    # sda save the results corresponding to xa = qchisq(.99, pd)
    stopv = diag(updNNC$Sig1)
    time1 = 2

    while ((time1 <= 6) && (all(stopv < (1 + bound) * initv))) {
        xa = Xa[time1]
        Np = Nc - Ndir * (xa^0.5)
        updNNC = .cov.nne.nclean.sub(rbind(datamat, Np), k, convergence = 0.001, 
            S.mean =  S.mean, S.sd = S.sd)
        SaveM = rbind(SaveM, c(xa, updNNC$mu1, .cov.nne.Mtovec(updNNC$Sig1)))
        SaveP = rbind(SaveP[2, ], (updNNC$probs)[1:n])
        time1 = time1 + 1
        stopv = diag(updNNC$Sig1)
        NULL
    }

    # Procedure stop if the added noise cause a "surge" within 
    # the range sdb save the results within the given "range"
    if (all(stopv < (1 + bound) * initv)) {
        dSaveM = dim(SaveM)[1]
        ans = SaveM[dSaveM, ]
        sdb = SaveM[dSaveM, ]
        NewP = SaveP[2, ]

        #  adding extension
        if (extension) {
            time2 = 1
            Fstop = FALSE
            tpv = stopv
            while ((time2 < 2) && (all(stopv < (1 + bound) * initv))) {
                xa = xa + gap
                startv = stopv
                Np = Nc - Ndir * (xa^0.5)
                updNNC = .cov.nne.nclean.sub(rbind(datamat, Np), k, 
                    convergence = 0.001, S.mean = S.mean, S.sd = S.sd)
                SaveM = rbind(SaveM, c(xa, updNNC$mu1, .cov.nne.Mtovec(
                    updNNC$Sig1)))
                SaveP = rbind(SaveP[2, ], (updNNC$probs)[
                    1:n])
                stopv = apply(rbind((startv * 2 - tpv), diag(
                    updNNC$Sig1)), 2, mean)
                tpv = diag(updNNC$Sig1)
                Fstop = all((abs(stopv - startv) <= ((1+abs(startv)) *
                    devsm)))
                if (Fstop)
                    time2 = time2 + 1
                else time2 = 1
                NULL
            }    
            # Checking the stop criterior at the end of extension
            if (all(stopv < (1 + bound) * initv)) {
                dSaveM = dim(SaveM)[1]
                ans = SaveM[dSaveM, ]
                NewP = SaveP[2, ]
            } else {
                dSaveM = dim(SaveM)[1]
                ans = SaveM[dSaveM - 1, ]
                NewP = SaveP[1, ]
            }
        }
    } else {
        dSaveM = dim(SaveM)[1]
        ans = SaveM[dSaveM - 1, ]
        sdb = ans[-1]
        NewP = SaveP[1, ]
    }
    nncvar = .cov.nne.vectoM(ans[ - (1:(1 + pd))], pd)
    mu = ans[2:(1 + pd)]
    
    # Return Value:
    list(cov = nncvar, mu = mu, postprob = NewP, classification = round(NewP), 
        innc = list(cov = orgNNC$Sig1, mu = orgNNC$mu1, postprob = OldP, 
        classification = round(OldP)))
}


# ------------------------------------------------------------------------------

 
.cov.nne.nclean.sub = 
function(datamat, k, distances = NULL, convergence = 0.001, S.mean = NULL, 
S.sd = NULL) 
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Internal Function called by .cov.nne()
    
    # FUNCTION:
    
    #  The Re-scale NNC function:
    d = dim(datamat)[2]
    n = dim(datamat)[1]
    kthNND = .cov.nne.splusNN(t((t(datamat) - S.mean)/S.sd), k = k)
    alpha.d = (2 * pi^(d/2))/(d * gamma(d/2))
    
    # Now use kthNND in E-M algorithm, first get starting guesses.
    delta = rep(0, n)
    delta[kthNND > (min(kthNND) + diff(range(kthNND))/3)] = 1
    p = 0.5
    lambda1 = k/(alpha.d * mean((kthNND[delta == 0])^d))
    lambda2 = k/(alpha.d * mean((kthNND[delta == 1])^d))
    loglik.old = 0
    loglik.new = 1
    
    # Iterator starts here ...
    while (abs(loglik.new - loglik.old)/(1+abs(loglik.new)) > convergence) 
    {
        # E - step
        delta = (p * .cov.nne.dDk(kthNND, lambda1, k = k, d = d, 
            alpha.d = alpha.d)) / (p * .cov.nne.dDk(kthNND, lambda1,
            k = k, d = d, alpha.d = alpha.d) + (1 - p) *
            .cov.nne.dDk(kthNND, lambda2, k = k, d = d, alpha.d = alpha.d))
        # M - step
        p = sum(delta) / n
        lambda1 = (k * sum(delta))/(alpha.d * sum((kthNND^d) * delta))
        lambda2 = (k * sum((1 - delta)))/(alpha.d * 
            sum((kthNND^d) * (1 - delta)))
        loglik.old = loglik.new
        loglik.new = sum( - p * lambda1 * alpha.d * ((kthNND^d) * delta) - 
            (1 - p) * lambda2 * alpha.d * ((kthNND^d) * (1 - delta)) + 
            delta * k * log(lambda1 * alpha.d) + (1 - delta) * k * 
            log(lambda2 * alpha.d))
    }

    # z will be the classifications. 1 = in cluster. 0 = in noise.
    probs = .cov.nne.dDk(kthNND, lambda1, k = k, d = d, alpha.d = alpha.d) /
        (.cov.nne.dDk(kthNND, lambda1, k = k, d = d, alpha.d = alpha.d) +
        .cov.nne.dDk(kthNND, lambda2, k = k, d = d, alpha.d = alpha.d))
    mprob = 1. - probs
    mu1 = apply((probs * datamat), 2, sum)/sum(probs)
    mu2 = apply((mprob * datamat), 2, sum)/sum(mprob)
    tpsig1 = t(datamat) - mu1
    tpsig2 = t(datamat) - mu2
    Sig1 = tpsig1 %*% (probs * t(tpsig1))/sum(probs)
    Sig2 = tpsig2 %*% (mprob * t(tpsig2))/sum(mprob)
    sd1 = sqrt(diag(Sig1))
    sd2 = sqrt(diag(Sig2))
    ans = rbind(mu1, sd1, mu2, sd2)
    
    zz = list(z = round(probs), kthNND = kthNND, probs = probs,
        p = p, mu1 = mu1, mu2 = mu2, sd1 = sd1, sd2 = sd2,
        lambda1 = lambda1, lambda2 = lambda2, Sig1 = Sig1,
        Sig2 = Sig2, ans = ans)
     
    # Return Value:   
    return(zz)
}


# ------------------------------------------------------------------------------


.cov.nne.dDk = 
function(x, lambda, k, d, alpha.d) 
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Internal Function called by .cov.nne()
    
    # FUNCTION:
    
    # Function to perform the Nearest Neighbour cleaning of
    # find the density of D_k
    ans = (exp( - lambda * alpha.d * x^d + log(2) + k * log(
        lambda * alpha.d) + log(x) * (d * k - 1) - log(
        gamma(k))))
     
    # Return Value:    
    ans
}


# ------------------------------------------------------------------------------


.cov.nne.splusNN = 
function(datamat, k)
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Internal Function called by .cov.nne()
    
    # FUNCTION:
    
    # Nearest-neighbor in S-PLUS
    n = nrow(datamat)
    distances = dist(datamat)
    
    #  This next part sorts through the Splus distance object 
    #  and forms kNNd, kth nearest neighbour distance, for each 
    #  point.
    kNNd = rep(0, n)
    N = (n - 1):0
    I = c(0, cumsum(N[-1]))
    J = c(0, I + n - 1)
    a = z = NULL
    for (j in 1:n) {
        if (j > 1)
            a = i + I[1:i]
        if (j < n)
            z = J[j] + 1:N[j]
        kNNd[j] = sort(distances[c(a, z)])[k]
        i = j
    }
    
    # Return Value: 
    kNNd
}


# ------------------------------------------------------------------------------



.cov.nne.Mtovec = 
function(M) 
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Internal Function called by .cov.nne()
    
    # FUNCTION:
    
    # Two procedures to link between a symmetric matrix and its vec(.)
    n = dim(M)[1]
    d = dim(M)[2]
    if (abs(n - d) > 0.01) {
        cat ("The input has to be a square matrix")
    } else {
        vec = rep(0, 0)
        for (i in (1:n)) {
            for (j in (i:d)) {
                vec = c(vec, M[i, j])
            }
        }
        vec
    }
}


# ------------------------------------------------------------------------------


.cov.nne.vectoM = 
function(vec, d) 
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Internal Function called by .cov.nne()
    
    # FUNCTION:
    
    n = length(vec)
    M = matrix(rep(0, d * d), d, d)
    L = 1
    for (i in 1:d) {
        for (j in i:d) {
            M[i, j] = vec[L]
            L = L + 1
            M[j, i] = M[i, j]
        }
    }
    
    # Return Value: 
    M
}
        

################################################################################
# Package: corpcor
# Version: 1.1.2
# Date: 2005-12-12
# Title: Efficient Estimation of Covariance and (Partial) Correlation
# Author: Juliane Schaefer <schaefer@stat.uni-muenchen.de> and
#   Korbinian Strimmer <korbinian.strimmer@lmu.de>.
# Maintainer: Korbinian Strimmer <korbinian.strimmer@lmu.de>
# Depends: R (>= 2.0.0)
# Suggests: 
# Description: This package implements a shrinkage estimator to allow
#   the efficient inference of large-scale covariance matrices 
#   from small sample data.  The resulting estimates are always
#   positive definite, more accurate than the empirical estimate,
#   well conditioned, computationally inexpensive, and require
#   only little a priori modeling.  The package also contains
#   similar functions for inferring correlations and partial
#   correlations.  In addition, it provides functions for fast svd 
#   computation, for computing the pseuoinverse, and 
#   for checking the rank and positive definiteness of a matrix.
# License: GPL version 2 or newer
# URL: http://www.statistik.lmu.de/~strimmer/software/corpcor/
# Packaged: Mon Dec 12 13:07:22 2005; strimmer


.cov.shrink =
function(x, lambda, verbose = FALSE)
{
   x = as.matrix(x)

   # Shrinkage correlation coefficients
   R.star = .cor.shrink(x, lambda = lambda, verbose=verbose)

   # Unbiased empirical variances
   V = apply(x, 2, var)
   
   resid.sd = sqrt(V)
   ans = sweep(sweep(R.star, 1, resid.sd, "*"), 2, resid.sd, "*") 
     
   # Return Value:
   ans
}


# ------------------------------------------------------------------------------


.cor.shrink = 
function(x, lambda, verbose = FALSE)
{
    # Standardize data (and turn x into a matrix)
    sx = scale(x)  
    
    p = dim(sx)[2]
    if(p == 1) return( as.matrix(1) ) 
    
    # Estimate variance of empirical correlation coefficients 
    vc = .varcov(sx, type = "unbiased", verbose)
    
    # Find optimal lambda:
    if(missing(lambda)) {   
        offdiagsum.rij.2 = sum(vc$S[lower.tri(vc$S)]^2)
        offdiagsum.v.rij = sum(vc$var.S[lower.tri(vc$var.S)])     
        lambda = offdiagsum.v.rij/offdiagsum.rij.2
        if(verbose) cat(paste("Estimated shrinkage intensity lambda: ",
            round(lambda,4), "\n"))
    }
    if(lambda > 1) {
        warning(paste("Overshrinking: lambda set to 1 (allowed range: 0-1)"))
        lambda = 1  
    } else if(lambda < 0) {
        warning(paste("Undershrinking: lambda set to 0 (allowed range: 0-1)"))
        lambda = 0  
    }
 
    # construct shrinkage estimator
    R.star = (1-lambda) * vc$S
    diag(R.star) = rep(1, p)
    attr(R.star, "lambda") = lambda
  
    # Return Value:
    R.star
}


# ------------------------------------------------------------------------------


.varcov = 
function(x, type = c("unbiased", "ML"), verbose = FALSE)
{
    # Details:
    #   compute the empirical covariance matrix S=cov(x) given a data 
    #   matrix x as well as the *variances* associated with the individual 
    #   entries S[i,j]

    x = as.matrix(x)     
    n = dim(x)[1]
    p = dim(x)[2]
         
    # Weights for the "unbiased" and "ML" cases
    type = match.arg(type)
    if(type == "unbiased") {
        h1 = 1/(n-1)
        h2 = n/(n-1)/(n-1) }    
    if(type == "ML") {
      h1 = 1/n
      h2 = (n-1)/n/n
    }
 
    s = matrix(NA, ncol = p, nrow = p)   
    vs = matrix(NA, ncol = p, nrow = p)
    xc = scale(x, scale=FALSE) # center the data
    
    # Diagonal elements:
    for (i in 1:p) {
        zii = xc[,i]^2
        s[i, i] = sum(zii)*h1
        vs[i, i] = var(zii)*h2
    }
    if(p == 1) 
        return(list(S = s, var.S = vs))
    if(verbose && p > 50)
        cat(paste("Computing ... wait for", p, "dots (50 per row):\n")) 
    
    # Off-diagonal elements
    for (i in 1:(p-1)) {
        if(verbose && p > 50) {
            cat(".")
            if(i %% 50 == 0) cat(paste(" ", i, "\n"))
        }
        for (j in (i+1):p) {
            zij = xc[,i]*xc[, j] 
            s[i, j] = sum(zij)*h1
            s[j, i] = s[i,j]
            vs[i, j] = var(zij)*h2
            vs[j, i] = vs[i, j]   
        }
    }
    if(verbose && p > 50) cat(paste(". ", i+1, "\n"))

    # Return Value:
    return(list(S = s, var.S = vs))
}


################################################################################
# cov.bagged.R  (2004-03-15)
#   Variance reduced estimators of cov, cor, and pcor
#       using bootstrap aggregation ("bagging")
#   Copyright 2003-04 Juliane Schaefer and Korbinian Strimmer
# Package: corpcor
# Version: 1.1.2
# Date: 2005-12-12
# Title: Efficient Estimation of Covariance and (Partial) Correlation
# Author: Juliane Schaefer <schaefer@stat.uni-muenchen.de> and
#         Korbinian Strimmer <korbinian.strimmer@lmu.de>.
# Maintainer: Korbinian Strimmer <korbinian.strimmer@lmu.de>
# Depends: R (>= 2.0.0)
# Suggests: 
# Description: This package implements a shrinkage estimator to allow
#   the efficient inference of large-scale covariance matrices 
#   from small sample data.  The resulting estimates are always
#   positive definite, more accurate than the empirical estimate,
#   well conditioned, computationally inexpensive, and require
#   only little a priori modeling.  The package also contains
#   similar functions for inferring correlations and partial
#   correlations.  In addition, it provides functions for fast svd 
#   computation, for computing the pseuoinverse, and 
#   for checking the rank and positive definiteness of a matrix.
# License: GPL version 2 or newer
# URL: http://www.statistik.lmu.de/~strimmer/software/corpcor/
# Packaged: Mon Dec 12 13:07:22 2005; strimmer


.cov.bagged = 
function(x, R = 1000, ...)
{
    vec.out = .bag.fun(cov, x, R = R, diag = TRUE, ...)
    mat.out = .vec2sm(vec.out, diag = TRUE)
  
    mat.out
}


# ------------------------------------------------------------------------------


.cor.bagged = 
function(x, R = 1000, ...)
{
    vec.out = .bag.fun(cor, x, R = R, diag = FALSE, ...)
    mat.out = .vec2sm(vec.out, diag = FALSE)
    
    # Fill diagonal with 1
    diag(mat.out) = rep(1, dim(mat.out)[1]) 
  
    mat.out
}


# ------------------------------------------------------------------------------


.bag.fun = 
function(fun, data, R, diag, ...)
{
    # Number of variables 
    p = dim(data)[2]
  
    # Index vector for lower triangle
    lo = lower.tri(matrix(NA, nrow=p, ncol=p), diag=diag)

    # bootstrap function
    .bootFun = function(data, i) {
        vec = as.vector( fun(data[i,], ...)[lo] )
        # if we get NAs flag result as being erroneous
        if(sum(is.na(vec)) > 0) class(vec) = "try-error"
        return( vec )
    }   
     
    # Bag variable 
    boot.out = .robust.cov.boot(data = data, statistic = .bootFun, R = R)
    bag = apply( boot.out$t, 2, mean)
    
    # Return Value:
    bag
}


# ------------------------------------------------------------------------------


.robust.cov.boot = 
function(data, statistic, R)
{
    # Description:
    #   Simple bootstrap function (robust against errors)
    
    idx = 1:dim(data)[1]
  
    # Determine dimension of statistic
    repeat {
        bx = sample(idx, replace = TRUE)
        val = try(statistic(data, bx))  
        if(class(val) != "try-error") break
    }
    dim.statistic = length(val)
    output = matrix(nrow = R, ncol = dim.statistic)
  
    replicate.count = 0
    error.count = 0
    
    while (replicate.count < R) {
        bx = sample(idx, replace=TRUE)
        val = try(statistic(data, bx)) 
        # if we get a numerical error we simply repeat the draw ..
        if(class(val) == "try-error") {
            error.count = error.count+1   
            if(error.count > R) 
                stop("Too many errors encountered during the bootstrap.")
        } else {
            replicate.count = replicate.count+1
            output[replicate.count, ] = val
        }
    }
    
    if(error.count > 0)  {
        warning(paste(error.count, "out of", R,
            "bootstrap samples were repeated due to errors."))
    }
    
    # Result:
    ans = list(t = output)
            
    # Return Value:
    ans
} 


################################################################################
# smtools.R  (2004-01-15)
#   Convert symmetric matrix to vector and back
#   Copyright 2003-04 Korbinian Strimmer
#
# This file is part of the 'corpcor' library for R and related languages.
# It is made available under the terms of the GNU General Public
# License, version 2, or at your option, any later version,
# incorporated herein by reference.
# 
# This program is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the GNU General Public License for more
# details.
# 
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA


.sm2vec = 
function(m, diag = FALSE)
{
    # Description:
    #   Convert symmetric matrix to vector
    
    ans = as.vector(m[lower.tri(m, diag)])
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.smindexes = 
function(m, diag = FALSE)
{
    # Descriiption:
    #   Corresponding indexes
      
    m.dim = length(diag(m))
 
    if(diag == TRUE) {
        num.entries = m.dim*(m.dim+1)/2
    } else {
        num.entries = m.dim*(m.dim-1)/2
    }   
    
    index1 = rep(NA, num.entries )
    index2 = rep(NA, num.entries )

    if(diag == TRUE) {
        delta = 0
    } else {
        delta = 1
    }

    z = 1
    for (i in 1:(m.dim-delta)) {
        for (j in (i+delta):m.dim) {
            index1[z] = i
            index2[z] = j
            z = z+1
        }
    }
    
    ans = cbind(index1, index2) 
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.vec2sm = 
function(vec, diag = FALSE, order = NULL)
{
    # Description:
    #   Convert vector to symmetric matrix
    
    # Note:
    #   If diag=FALSE then the diagonal will consist of NAs
    
    # dimension of matrix
    n = (sqrt(1+8*length(vec))+1)/2
    if(diag == TRUE) n = n-1
    if( ceiling(n) != floor(n) )
        stop("Length of vector incompatible with symmetric matrix")
       
    # fill lower triangle of matrix     
    m = matrix(NA, nrow = n, ncol = n)
    lo = lower.tri(m, diag)
    if(is.null(order)) {
        m[lo] = vec
    } else {
        # sort vector according to order
        vec.in.order = rep(NA, length(order))
        vec.in.order[order] = vec
        m[lo] = vec.in.order
    }
  
    # symmetrize
    for (i in 1:(n-1)) {
        for (j in (i+1):n) {
            m[i, j] = m[j, i]
        }
    }   
  
    # Return Value:
    m
}


################################################################################


.cov.ogk =  
function(x, niter = 2, beta = 0.9, control)
{
    metodo2 = function(XX) {
    
        n = nrow(XX)
        p = ncol(XX)
    
        sigma = apply(XX, 2, mrob)[2,]
        Y = XX %*% diag(1/sigma)
        U = matrix(1, p, p)
        for(i in 1:p)
            for(j in i:p) {
                U[j, i] = U[i, j] = vrob(Y[,i], Y[,j])
        }
    
        diag(U) = 1
        E = eigen(U)$vectors
        A = diag(sigma) %*% E
        Z = Y %*% E
    
        restau = apply(Z, 2, mrob)
        sigma = as.vector(restau[2,])
        cov = A %*% diag(sigma^2) %*% t(A)
        loc = A %*% restau[1,]
    
        list(cov = cov, center = loc, AA = A, ZZ = Z)
    }

    # Analize and validate the input parameters ...

    # If a control object was supplied, take the option parameters from it,
    #  but if single parameters were passed (not defaults) they will override the
    #  control object.
    # The functions 'mrob()' and 'vrob()' can be supplied only via the control 
    #  object. If no control object i spassed these function will be taken 
    #  from the default one
    
    defcontrol = CovControlOgk()           # default control
    mrob = defcontrol@mrob
    vrob = defcontrol@vrob
    if(!missing(control)){                  # a control object was supplied
        if(niter == defcontrol@niter)       niter = control@niter
        if(beta == defcontrol@beta)         beta = control@beta
        mrob = control@mrob
        vrob = control@vrob
    }

    if(is.data.frame(x))
        x = data.matrix(x)
    else if (!is.matrix(x))
        x = matrix(x, length(x), 1,
            dimnames = list(names(x), deparse(substitute(x))))

    # drop all rows with missing values (!!) :
    na.x = !is.finite(x %*% rep(1, ncol(x)))
    ok = !na.x
    x = x[ok, , drop = FALSE]
    dx = dim(x)
    if(!length(dx))
        stop("All observations have missing values!")
    dimn = dimnames(x)
    n = dx[1]
    p = dx[2]
    if(p < 2)
        stop("Need at least 2 columns ")

    call = match.call()

    #  iterate two times to obtain OGK2
    first = metodo2(x)
    cov = first$cov
    center = as.vector(first$center)
    ZZ = first$ZZ
    if(niter >= 2){
        second = metodo2(first$ZZ)
        cov  = first$AA %*% second$cov %*% t(first$AA)
        center = as.vector(first$AA %*% as.vector(second$center))
        ZZ = second$ZZ
    }
    
    dimnames(cov) = list(dimn[[2]], dimn[[2]])
    names(center) = dimn[[2]]
    
    #  compute distances and weights
    #  do not invert cov to compute the distances, use the transformed data
    #
    #  dist2 = mahalanobis(X, center, cov)
    #
    
    musigma = apply(ZZ,2,mrob)
    ZZ = sweep(ZZ, 2, musigma[1,])
    ZZ = sweep(ZZ, 2, musigma[2,], '/')
    dist2 = rowSums(ZZ^2)

    quantiel = qchisq(beta, p)
    qq = (quantiel * median(dist2))/qchisq(0.5, p)
    wt = ifelse(dist2 < qq, 1, 0)
    swt = sum(wt)

    #  compute the reweighted estimates:  OGK2(0.9)
    wcenter = colSums(x*wt)/swt
    X = sqrt(wt) * sweep(x, 2, wcenter)
    wcov = (t(X) %*% X)/swt

    #  Compute consistency correction factor for the reweighted  cov
    #    qdelta.rew = qchisq(sum(wt)/n, p)
    #    cdeltainvers.rew = pgamma(qdelta.rew/2, p/2 + 1)/(sum(wt)/n)
    #    cnp2 = 1/cdeltainvers.rew

    method = "Orthogonalized Gnanadesikan-Kettenring Estimator"
    ans = list(
               call = call,
               iter=niter,
               crit=1,
               cov=wcov,
               center=wcenter,
               n.obs=n,
               raw.cov=cov,
               raw.center=center,
               raw.mah = dist2,
               raw.wt = wt,
               X = x,
               method=method)
    ans
}


################################################################################

