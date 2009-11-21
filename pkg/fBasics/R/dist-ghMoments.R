
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


################################################################################
# FUNCTION:                    DESCRIPTION:
#  rhRawMean                    Returns raw mean
#  ghRawVar                     Returns raw variance
#  ghRawSkew                    Returns raw skewness
#  ghRawKurt                    Returns raw kurtosis
# FUNCTION:                    UTILITY FUNCTION:
#  .aRecursionGH                Computes the moment coefficients a recursively
#  .besselZ                     Computes Bessel/Power Function ratio
# FUNCTION:                    MOMENTS AND RELATED EXPRESSIONS:
#  .ghRawMoments                Computes raw moments from formula
#  .ghRawMomentsIntegrated      Computes raw moments by Integration
#  .checkGHRawMoments           Checks raw moments
#  .ghMuMoments                 Computes mu-moments from formula
#  .ghMuMomentsIntegrated       Computes mu-moments by integration
#  .checkGHMuMoments            Checks mu-moments
################################################################################


ghRawMean <-
function(alpha=1, beta=0, delta=1, lambda=-1/2)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    .ghRawMoments(k=1, alpha=1, beta=0, delta=1, lambda=-1/2)
}


# ------------------------------------------------------------------------------


ghRawVar <- 
function(alpha=1, beta=0, delta=1, lambda=-1/2)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    .ghRawMoments(k=2, alpha=1, beta=0, delta=1, lambda=-1/2)[[2]]
}


# ------------------------------------------------------------------------------


ghRawSkew <- 
function(alpha=1, beta=0, delta=1, lambda=-1/2)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    .ghRawMoments(k=3, alpha=1, beta=0, delta=1, lambda=-1/2)[[3]]
}


# ------------------------------------------------------------------------------


ghRawKurt <- 
function(alpha=1, beta=0, delta=1, lambda=-1/2)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    .ghRawMoments(k=4, alpha=1, beta=0, delta=1, lambda=-1/2)[[4]]
}


################################################################################


.aRecursionGH <- 
function(k = 12, trace = FALSE)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the moment coefficients recursively
    
    # Example:
    #   .aRecursionGH()
    #     [,1] [,2] [,3] [,4] [,5]  [,6]   [,7]   [,8]   [,9]  [,10] [,11] [,12] 
    # [1,]  1    0    0    0    0     0      0      0      0      0     0     0 
    # [2,]  1    1    0    0    0     0      0      0      0      0     0     0 
    # [3,]  0    3    1    0    0     0      0      0      0      0     0     0 
    # [4,]  0    3    6    1    0     0      0      0      0      0     0     0 
    # [5,]  0    0   15   10    1     0      0      0      0      0     0     0 
    # [6,]  0    0   15   45   15     1      0      0      0      0     0     0 
    # [7,]  0    0    0  105  105    21      1      0      0      0     0     0 
    # [8,]  0    0    0  105  420   210     28      1      0      0     0     0 
    # [9,]  0    0    0    0  945  1260    378     36      1      0     0     0 
    #[10,]  0    0    0    0  945  4725   3150    630     45      1     0     0 
    #[11,]  0    0    0    0    0 10395  17325   6930    990     55     1     0 
    #[12,]  0    0    0    0    0 10395  62370  51975  13860   1485    66     1 
    
    # FUNCTION:
    
    # Setting Start Values:
    a = matrix(rep(0, times = k*k), ncol = k) 
    a[1, 1] = 1
    if (k > 1) a[2, 1] = 1
    
    # Compute all Cofficients by Recursion:
    if (k > 1) {
        for (d in 2:k) {
            for (l in 2:d) {
                a[d,l] = a[d-1, l-1] + a[d-1, l]*(2*l+1-d)
            }
        }
    }
    rownames(a) = paste("k=", 1:k, sep = "")
    colnames(a) = paste("l=", 1:k, sep = "")
    
    # Trace:
    if (trace) {
        cat("\n")
        print(a)
        cat("\n")
    }
    for (K in 1:k) {
        L = trunc((K+1)/2):K  
        M = 2*L-K
        s1 = as.character(a[K, L])
        s2 = paste("delta^", 2*L, sep = "")
        s3 = paste("beta^", M, sep = "")
        s4 = paste("Z_lambda+", L, sep = "" )
        if (trace) {
            cat("k =", K, "\n")
            print(s1)
            print(s2)
            print(s3)
            print(s4)
            cat("\n")
        }
    }
    if (trace) cat("\n")
    
    # Return Value:
    list(a = a[k, L], delta = 2*L, beta = M, besselOffset = L) 
}
    
    
# ------------------------------------------------------------------------------
 

.besselZ <- 
function(x, lambda) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes Bessel/Power Function ratio
    
    # Ratio:
    ratio = besselK(x, lambda)/x^lambda
    
    # Return Value:
    ratio
}


################################################################################

    
.ghRawMoments <- 
function(k = 4, alpha = 1, beta = 0, delta = 1, lambda = -1/2)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes raw moments from formula
    
    # FUNCTION:
    
    # Check parameters:
    if (lambda >= 0) stopifnot(abs(beta) < alpha)
    if (lambda <  0) stopifnot(abs(beta) <= alpha)
    if (lambda >  0) stopifnot(delta >= 0)
    if (lambda <= 0) stopifnot(delta >  0)
    
    zeta = delta*sqrt(alpha^2-beta^2)
    cLambda = 1/.besselZ(zeta, lambda)
    
    a = .aRecursionGH(k)
    coeff = a$a
    deltaPow = a$delta
    betaPow = a$beta
    besselIndex = lambda + a$besselOffset
        
    M = coeff * delta^deltaPow * beta^betaPow * .besselZ(zeta, besselIndex)
    ans = cLambda * sum(M)
    attr(ans, "M") <- M
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.ghRawMomentsIntegrated <- 
function(k = 4, alpha = 1, beta = 0, delta = 1, lambda = -1/2)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes raw moments by integration
    
    # FUNCTION:
    
    # Check parameters:
    if (lambda >= 0) stopifnot(abs(beta) < alpha)
    if (lambda <  0) stopifnot(abs(beta) <= alpha)
    if (lambda >  0) stopifnot(delta >= 0)
    if (lambda <= 0) stopifnot(delta >  0)
      
    mgh <- function(x, k, alpha, beta, delta, lambda) {        
        x^k * dgh(x, alpha, beta, delta, mu=0, lambda) }
    
    M = integrate(mgh, -Inf, Inf, k = k, alpha = alpha,
        beta = beta, delta = delta, lambda = lambda,
        subdivisions = 1000, rel.tol = .Machine$double.eps^0.5)[[1]]
    
    # Return Value:
    M
}


# ------------------------------------------------------------------------------


.checkGHRawMoments <-
function(K = 10)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Checks raw moments
    
    # Example:
    #   .checkGHRawMoments()
    
    # FUNCTION:
    
    # Compute Raw Moments:
    raw = NULL
    for (k in 1:K) {
        raw = c(raw, .ghRawMoments(k))
    }
    names(raw) = 1:K
    int = NULL
    for (k in 1:K) {
        int = c(int, .ghRawMomentsIntegrated(k))
    }
    names(int) = NULL
    
    # Return Value:
    rbind(raw, int)
}


# ------------------------------------------------------------------------------


.ghMuMoments <-
function(k = 4, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes mu-moments from formula
    
    # FUNCTION:
    
    # Settings:
    binomCoeff = choose(k, 0:k)
    
    # Compute Mu Moments:
    M = 1
    for (l in 1:k) M = c(M, .ghRawMoments(l, alpha, beta, delta, lambda))
    muPower = mu^(k:0)
    muM = binomCoeff * muPower * M
    
    # Return Value:
    sum(muM)  
}


# ------------------------------------------------------------------------------


.ghMuMomentsIntegrated <- 
function(k = 4, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes mu-moments by integration

    # FUNCTION:
    
    # Check parameters:
    if (lambda >= 0) stopifnot(abs(beta) < alpha)
    if (lambda <  0) stopifnot(abs(beta) <= alpha)
    if (lambda >  0) stopifnot(delta >= 0)
    if (lambda <= 0) stopifnot(delta >  0)
        
    # Compute Mu Moments by Integration:
    mgh <- function(x, k, alpha, beta, delta, mu, lambda) {
        x^k * dgh(x, alpha, beta, delta, mu, lambda)  }
    M = integrate(mgh, -Inf, Inf, k = k, alpha = alpha,
        beta = beta, delta = delta, mu = mu, lambda = lambda,
        subdivisions = 1000, rel.tol = .Machine$double.eps^0.5)[[1]]
    
    # Return Value:
    M
}


# ------------------------------------------------------------------------------


.checkGHMuMoments <-
function(K = 10)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Checks mu-moments 
    
    # Example:
    #   .checkGHMuMoments()
    
    # FUNCTION:
    
    raw = NULL
    for (k in 1:K) {
        raw = c(raw, .ghMuMoments(k))
    }
    names(raw) = 1:K
    int = NULL
    for (k in 1:K) {
        int = c(int, .ghMuMomentsIntegrated(k))
    }
    names(int) = NULL
    
    # Return Value:
    rbind(raw,int)
}


################################################################################
    
    