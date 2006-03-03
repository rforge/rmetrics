
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


################################################################################


### cor.shrink.R  (2005-09-28)
###
###    Shrinkage Estimation of Covariance and Correlation Matrix
###
### Copyright 2005 Juliane Schaefer and Korbinian Strimmer
###
###
###
### This file is part of the `corpcor' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA



cor.shrink <- function(x, lambda, verbose=TRUE)
{
  sx <- scale(x)  # standardize data (and turn x into a matrix)
 
  p <- dim(sx)[2]
  if (p == 1) return( as.matrix(1) ) 
  
  # estimate variance of empirical correlation coefficients 
  vc <- varcov(sx, type="unbiased", verbose)
  
  # find optimal lambda
  if (missing(lambda))
  {   
    offdiagsum.rij.2 <- sum(vc$S[lower.tri(vc$S)]^2)
    offdiagsum.v.rij <- sum(vc$var.S[lower.tri(vc$var.S)])
        
    lambda <- offdiagsum.v.rij/offdiagsum.rij.2
    if (verbose) cat(paste("Estimated shrinkage intensity lambda: ",
        round(lambda,4), "\n"))
  }
  
  #########
  
  if (lambda > 1)
  {
    warning(paste("Overshrinking: intensity lambda set to 1 (allowed range: 0-1)"))
    lambda <- 1  
  }
  if (lambda < 0)
  {
     warning(paste("Undershrinking: intensity lambda set to 0 (allowed range: 0-1)"))
     lambda <- 0  
  }
  
  #########
 
  # construct shrinkage estimator
  R.star <- (1-lambda)*vc$S
  diag(R.star) <- rep(1, p)
  
  attr(R.star, "lambda") <- lambda
  
  return( R.star )
}


############### derived estimators ################


cov.shrink <- function(x, lambda, verbose=TRUE)
{
   if( !is.matrix(x) ) x <- as.matrix(x)

   # shrinkage correlation coefficients
   R.star <- cor.shrink(x, lambda=lambda, verbose=verbose)

   # unbiased empirical variances
   V <- apply(x, 2, var)
     
   return( rebuild.cov(R.star, V) )
}



# cor2pcor applied to cor.shrink 
pcor.shrink <- function(x, lambda, verbose=TRUE)
{
  return(
    cor2pcor( cor.shrink(x, lambda=lambda, verbose=verbose),
      exact.inversion=TRUE, check.eigenvalues=FALSE) )
}



### cor2pcor.R  (2004-09-25)
###
###    Partial Correlation computed by Inversion 
###    of the Covariance or Correlation Matrix
###    
###
### Copyright 2003-04 Juliane Schaefer and Korbinian Strimmer
###
### This file is part of the `corpcor' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


#
# partial correlation matrix
#
# input: covariance matrix or correlation matrix
# ouput: partial correlation matrix
#
cor2pcor <- function(m, exact.inversion=TRUE, check.eigenvalues=TRUE, tol)
{
  if (check.eigenvalues)
  {
    if ( !is.positive.definite(m, tol=tol) )
    {
      stop("Input matrix is not positive definite!")
    }
  }
  
  # standardize
  # m <- cov2cor(m)
  
  # invert, then negate off-diagonal entries
  if (exact.inversion)
  {
    m <- -solve(m)
  }
  else
  {
    m <- -pseudoinverse(m, tol=tol)
  }
  diag(m) <- -diag(m)

  # standardize and return  
  return(cov2cor(m))
}


#
# backtransformation to correlation matrix
#
# input: partial correlation matrix
# ouput: correlation matrix
pcor2cor <- function(m, exact.inversion=TRUE, check.eigenvalues=TRUE, tol)
{
  if (check.eigenvalues)
  {
    if ( !is.positive.definite(m, tol=tol) )
    {
      stop("Input matrix is not positive definite!")
    }
  }

  # standardize
  # m <- cov2cor(m)

  # negate off-diagonal entries, then invert
  m <- -m
  diag(m) <- -diag(m)
  if (exact.inversion)
  {
    m <- solve(m)
  }
  else
  {
    m <- pseudoinverse(m, tol=tol)
  }
  
  # standardize and return 
  return(cov2cor(m))
}


### cov.bagged.R  (2004-03-15)
###
###     Variance reduced estimators of cov, cor, and pcor
###     using bootstrap aggregation ("bagging")
###
### Copyright 2003-04 Juliane Schaefer and Korbinian Strimmer
###
###
### This file is part of the `corpcor' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA




# bagged estimators

cov.bagged <- function(x, R=1000, ...)
{
  vec.out <- bag.fun(cov, x, R=R, diag=TRUE, ...)
  mat.out <- vec2sm(vec.out, diag=TRUE)
  
  return( mat.out )
}

cor.bagged <- function(x, R=1000, ...)
{
  vec.out <- bag.fun(cor, x, R=R, diag=FALSE, ...)
  mat.out <- vec2sm(vec.out, diag=FALSE)
  diag(mat.out) <- rep(1, dim(mat.out)[1]) # fill diagonal with 1
  
  return( mat.out )
}

pcor.bagged <- function(x, R=1000, ...)
{
  vec.out <- bag.fun(pcor.pseudo, x, R=R, diag=FALSE, ...)
  mat.out <- vec2sm(vec.out, diag=FALSE)
  diag(mat.out) <- rep(1, dim(mat.out)[1]) # fill diagonal with 1
  
  return( mat.out )
}


#############################################################################

# internal

bag.fun <- function(fun, data, R, diag, ...)
{
  # number of variables 
  p <- dim(data)[2]
  
  # index vector for lower triangle
  lo <- lower.tri(matrix(NA, nrow=p, ncol=p), diag=diag)

  # bootstrap function
  boot.fun <- function(data, i) 
  {
    vec <- as.vector( fun(data[i,], ...)[lo] )
      
    # if we get NAs flag result as being erroneous
    if (sum(is.na(vec)) > 0) class(vec) <- "try-error"

    return( vec )
  }   
     
  #bag variable 
  #boot.out <- boot(data=data, statistic=boot.fun, R=R)
  boot.out <- robust.boot(data=data, statistic=boot.fun, R=R)
  
  bag <- apply( boot.out$t, 2, mean)
    
  return( bag )
}


# simple bootstrap function (robust against errors)
robust.boot <- function(data, statistic, R)
{
  idx <- 1:dim(data)[1]
  
  # determine dimension of statistic
  repeat
  {
    bx <- sample(idx, replace=TRUE)
    val <- try(statistic(data, bx)) 
    
    if (class(val) != "try-error") break
  }
  dim.statistic <- length(val)
  output <- matrix(nrow=R, ncol=dim.statistic)
  
  replicate.count <- 0
  error.count <- 0
  while (replicate.count < R)
  {
    bx <- sample(idx, replace=TRUE)
    val <- try(statistic(data, bx)) 
    
    if (class(val) == "try-error") # if we get a numerical error we simply repeat the draw ..
    {
      error.count <- error.count+1
      #cat("Bootstrapping continues, drawing an alternative bootstrap sample ...\n")
      
      if (error.count > R) stop("Too many errors encountered during the bootstrap.")
    }
    else
    {
      replicate.count <- replicate.count+1
      output[replicate.count,] <- val
    }
  }
  
  if (error.count > 0) warning(paste(error.count, "out of", R,
   "bootstrap samples were repeated due to errors."))
  
  return(list(t=output))
} 



# for pcor.bagged


#
# compute partial correlations given the data x 
# using the pseudoinverse of cor (x)
#

pcor.pseudo <- function(x, tol)
{
  pc <- -psinv.cor(x, tol)
  diag(pc) <- -diag(pc)
  
  return(cov2cor(pc)) 
}


#
# compute inverse correlation matrix
#
# this is numerically equivalent to pseudoinverse(cor(x)) but much
# faster for n << p 
#
psinv.cor <- function (x, tol)
{
    n <- dim(x)[1]
    p <- dim(x)[2]
    
    xs <- scale(x) # standardize
   
    if (n < p)
    {
        # the following is *much* faster than inverting the
    # p x p cor matrix directly
         
        xsvd <- fast.svd(xs, tol)   # fast svd on "fat" matrix (using svd on n x n matrix)
        if (length(xsvd$d) == 0)
        {
           ic <- array(0, c(p,p))
        }
        else
        {
           ic <- xsvd$v %*% ((n-1)/xsvd$d^2 * t(xsvd$v))
        }
        
    }
    else
    {
    ic <- pseudoinverse(crossprod(xs)/(n-1), tol)   # invert p x p matrix using svd 
    }
      
    return(ic)
}

### fast.svd.R  (2005-07-19)
###
###    Efficient Computation of the Singular Value Decomposition
###
### Copyright 2003-05 Korbinian Strimmer
###
###
### This file is part of the `corpcor' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA

# private functions

# this works just like svd, with the difference that only LAPACK
# methods are used, and that there is an automatic fallback
# to algorithm "dgesvd" if algorithm "dgesdd" throws an error.

# note that LAPACK.svd returns "v" not "vt" as La.svd


LAPACK.svd <- function(x, nu = min(n, p), nv = min(n, p))
{
    if (!is.numeric(x) && !is.complex(x))
        stop("argument to 'LAPACK.svd' must be numeric or complex")
    if (any(!is.finite(x)))
        stop("infinite or missing values in 'x'")
         
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    if (!n || !p)
        stop("0 extent dimensions")


    if( is.complex(x) ) # if complex we have to use the "dgesvd" algorithm
    {
        method = "dgesvd"
        res <- La.svd(x, nu=nu, nv=nv, method=method) 
    
    }
    else # but otherwise we try the "dgesdd" algorithm first (10x faster)
    {
        method = "dgesdd"
        res <- try( La.svd(x, nu=nu, nv=nv, method=method), silent=TRUE )
  
        if ( class(res) == "try-error" || any(res$d < 0) ) 
    # and use the "dgesvd" algorithm if there is an error,
    # or if there are negative singular values (happens on AMD64/ATLAS!!)  
        {
            method = "dgesvd"
            res <- La.svd(x, nu=nu, nv=nv, method=method)
        }
    }

    if  (is.complex(x))
    {
         out <- list(d = res$d, u = if (nu) res$u, v = if (nv) Conj(t(res$vt)))
    }
    else
    {
         out <- list(d = res$d, u = if (nu) res$u, v = if (nv) t(res$vt))
    }

    attr(out, "svd.algorithm") <- method
    return(out)
}



# standard svd returning only positive singular values
positive.svd <- function(m, tol)
{
  s <- LAPACK.svd(m)
  
  if( missing(tol) ) 
      tol <- max(dim(m))*max(s$d)*.Machine$double.eps
  Positive <- s$d > tol

  return(list(
      d=s$d[Positive],
      u=s$u[, Positive, drop=FALSE],
      v=s$v[, Positive, drop=FALSE]
      ))
}

# fast computation of svd(m) if n << p  
# (n are the rows, p are columns)
nsmall.svd <- function(m, tol)
{
   B <- m %*% t(m)     # nxn matrix
   s <- LAPACK.svd(B,nv=0)    # of which svd is easy..

   # determine rank of B  (= rank of m)
   if( missing(tol) ) 
      tol <- dim(B)[1]*max(s$d)*.Machine$double.eps 
   Positive <- s$d > tol                            
           
   # positive singular values of m  
   d <- sqrt(s$d[Positive])
      
   # corresponding orthogonal basis vectors
   u <- s$u[, Positive, drop=FALSE]
   v <- crossprod(m, u) %*% diag(1/d, nrow=length(d))   
  
   return(list(d=d,u=u,v=v))
}

# fast computation of svd(m) if n >> p  
# (n are the rows, p are columns)
psmall.svd <- function(m, tol)
{
   B <- crossprod(m)   # pxp matrix
   s <- LAPACK.svd(B,nu=0)    # of which svd is easy..

   # determine rank of B  (= rank of m)
   if( missing(tol) ) 
      tol <- dim(B)[1]*max(s$d)*.Machine$double.eps 
   Positive <- s$d > tol                            
           
   # positive singular values of m  
   d <- sqrt(s$d[Positive])
      
   # corresponding orthogonal basis vectors
   v <- s$v[, Positive, drop=FALSE]
   u <- m %*% v %*% diag(1/d, nrow=length(d))
  
   return(list(d=d,u=u,v=v))
}


# public functions

# fast computation of svd(m)

# note that the signs of the columns vectors in u and v
# may be different from that given by svd()

# note that also only positive singular values are returned

fast.svd <- function(m, tol)
{  
  n <- dim(m)[1]
  p <- dim(m)[2]
 
 
  EDGE.RATIO <- 2 # use standard SVD if matrix almost square
  if (n > EDGE.RATIO*p)
  {
     return(psmall.svd(m,tol))
  }
  else if (EDGE.RATIO*n < p)
  {  
     return(nsmall.svd(m,tol)) 
  }
  else # if p and n are approximately the same
  {
     return(positive.svd(m, tol))
  }
}


### pseudoinverse.R  (2004-09-25)
###
###    Computation of the Pseudoinverse of a Matrix
###
### Copyright 2003-04 Korbinian Strimmer
###
###
### This file is part of the `corpcor' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA



pseudoinverse <- function (m, tol)
{
    msvd <- fast.svd(m, tol)
    
    if (length(msvd$d) == 0)
    {
       return(
            array(0, dim(m)[2:1])
            )
    }
    else
    {
       return( 
            msvd$v %*% (1/msvd$d * t(msvd$u))
            )
     }    
}
### condition.R  (2005-12-12)
###
###     Rank, condition, and positive definiteness of a matrix
###
### Copyright 2003-05 Korbinian Strimmer
###
###
### This file is part of the `corpcor' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


# checks whether a matrix is positive definite
is.positive.definite <- function (m, tol, method=c("eigen", "chol"))
{
    method <- match.arg(method)
    
    if (!is.matrix(m)) m <- as.matrix(m)

    if (method=="eigen")
    {
        eval <- eigen(m, only.values = TRUE)$values

        if( missing(tol) )
            tol <- max(dim(m))*max(abs(eval))*.Machine$double.eps
   
        if (sum(eval > tol) == length(eval))
            return(TRUE)
        else
            return(FALSE)
    }
    
    if (method=="chol")
    {
    val <- try(chol(m), silent=TRUE)
  
        if (class(val) == "try-error")
            return(FALSE)
        else
            return(TRUE)    
    }
}


# Method by Higham 1988
make.positive.definite <- function(m, tol)
{
  if (!is.matrix(m)) m <- as.matrix(m)

  d <- dim(m)[1] 
  if ( dim(m)[2] != d ) stop("Input matrix is not square!")
   
  es <- eigen(m)
  esv <- es$values
  
  if (missing(tol))
      tol <- d*max(abs(esv))*.Machine$double.eps 
  delta <-  2*tol # factor to is just to make sure the resulting
                  # matrix passes all numerical tests of positive definiteness
  
  tau <- pmax(0, delta - esv)
  dm <- es$vectors %*% diag(tau, d) %*% t(es$vectors)    
  
  #print(max(DA))
  #print(esv[1]/delta)
      
  return( m +  dm )
}




# rank and condition of a matrix 
rank.condition <- function (m, tol)
{
    d <- LAPACK.svd(m, nv=0, nu=0)$d # compute only singular values
    
    max.d <- d[1]
    min.d <- d[length(d)]
    
    if( missing(tol) ) 
        tol <- max(dim(m))*max.d*.Machine$double.eps
    
    r <- sum(d > tol) # rank: number of singular values larger than tol
    
    if (r < min(dim(m)) ) min.d <- 0 # if matrix is singular then set the  smallest
                                     # singular value to 0, and hence condition = INF
    
    c <- max.d/min.d
    
    return(list(rank = r, condition = c, tol=tol))
}

### rebuild.cov.R (2004-01-15)
###
###    Rebuild Covariance Matrix from Correlation Matrix and Variances
###    
###
### Copyright 2003-04 Korbinian Strimmer
###
### This file is part of the `corpcor' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA



#
# rebuild covariance matrix
#
# input:  correlation matrix           rho(ij)
#         vector with variances        var(i) 
# output: correlation matrix   rho(ij)*sqrt(var(i)*var(j))
#
rebuild.cov <- function(r, v)
{
  resid.sd <- sqrt(v)
  m <- sweep(sweep(r, 1, resid.sd, "*"), 2, resid.sd, "*") 
    
  return(m)
}


### smtools.R  (2004-01-15)
###
###     Convert symmetric matrix to vector and back
###
### Copyright 2003-04 Korbinian Strimmer
###
###
### This file is part of the `corpcor' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


# convert symmetric matrix to vector
sm2vec <- function(m, diag = FALSE)
{
    return( as.vector(m[lower.tri(m, diag)]) )
}

# corresponding indexes
sm.indexes <- function(m, diag = FALSE)
{
  m.dim <- length(diag(m))
 
  if (diag == TRUE)
    num.entries <- m.dim*(m.dim+1)/2
  else
    num.entries <- m.dim*(m.dim-1)/2
    
  index1 <- rep(NA, num.entries )
  index2 <- rep(NA, num.entries )

  if (diag == TRUE)
    delta <- 0
  else
    delta <- 1

  z <- 1
  for (i in 1:(m.dim-delta))
    for (j in (i+delta):m.dim)
    {
      index1[z] <- i
      index2[z] <- j
      z <- z+1
    }
      
 return( cbind(index1, index2) )
}

# convert vector to symmetric matrix
#
# note: if diag=FALSE then the diagonal will consist of NAs
#
vec2sm <- function(vec, diag = FALSE, order = NULL)
{
  # dimension of matrix
  n <- (sqrt(1+8*length(vec))+1)/2
  if (diag == TRUE) n <- n-1
  if ( ceiling(n) != floor(n) )
    stop("Length of vector incompatible with symmetric matrix")
       
  # fill lower triangle of matrix     
  m <- matrix(NA, nrow=n, ncol=n)
  lo <- lower.tri(m, diag)
  if (is.null(order))
  {
    m[lo] <- vec
  }
  else
  {
    # sort vector according to order
    vec.in.order <- rep(NA, length(order))
    vec.in.order[order] <- vec
    m[lo] <- vec.in.order
  }
  
  # symmetrize
  for (i in 1:(n-1))
    for (j in (i+1):n)
         m[i, j] <- m[j, i]   
  
  return( m )
}
### cov.shrink.R  (2005-09-28)
###
###    Variance of the Entries of the Covariance Matrix
###
### Copyright 2005 Korbinian Strimmer
###
###
###
### This file is part of the `corpcor' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


# compute the empirical covariance matrix S=cov(x) given a data matrix x
# as well as the *variances* associated with the individual entries S[i,j]
#

varcov <- function(x, type=c("unbiased", "ML"), verbose=TRUE)
{
    if (!is.matrix(x)) x <- as.matrix(x)     
    n <- dim(x)[1]
    p <- dim(x)[2]
 
            
    # weights for the "unbiased" and "ML" cases
    type <- match.arg(type)
    if (type=="unbiased")
    {
      h1 <- 1/(n-1)
      h2 <- n/(n-1)/(n-1)
    }    
    if (type=="ML")
    {
      h1 <- 1/n
      h2 <- (n-1)/n/n
    }
 
    s <- matrix(NA, ncol=p, nrow=p)   
    vs <- matrix(NA, ncol=p, nrow=p)
    xc <- scale(x, scale=FALSE) # center the data
    
    # diagonal elements
    for (i in 1:p)
    {
      zii <- xc[,i]^2
      s[i,i] <- sum(zii)*h1
      vs[i,i] <- var(zii)*h2
    }
    
    if (p == 1) return(list(S=s, var.S=vs))
    
    if (verbose && p > 50)
      cat(paste("Computing ... wait for", p, "dots (50 per row):\n")) 
    
    # off-diagonal elements
    for (i in 1:(p-1))
    {
      if (verbose && p > 50)
      {
        cat(".")
        if (i %% 50 == 0) cat(paste(" ", i, "\n"))
      }
      
      for (j in (i+1):p)
      {
        zij <- xc[,i]*xc[,j] 
    s[i,j] <- sum(zij)*h1
        s[j,i] <- s[i,j]
        
        vs[i,j] <- var(zij)*h2
        vs[j,i] <- vs[i,j]   
      }
      
    }
    if (verbose && p > 50) cat(paste(". ", i+1, "\n"))

    return(list(S=s, var.S=vs))
}


################################################################################

