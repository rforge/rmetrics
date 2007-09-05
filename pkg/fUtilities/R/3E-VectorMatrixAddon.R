
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file
    
  
# fEcofin::VectorMatrixAddon.R  
################################################################################
# GENERATION:               DESCRIPTION:
#  matrix                    R  Creates a matrix from the given set of values
#   diag                     R  Creates a diagonal matrix or extracts diagonals
#   triang                   M  Extracs the lower tridiagonal part from a matrix
#   Triang                   M  Extracs the upper tridiagonal part from a matrix
#   pascal                   M  Creates a Pascal matrix
#   hilbert                  M  Creates a Hilbert matrix
#   colVec                   M  Creates a column vector from a data vector
#   rowVec                   M  Creates a row vector from a data vector
#  as.matrix                 R  Attempts to turn its argument into a matrix     
#  is.matrix                 R  Tests if its argument is a (strict) matrix
#  isPositiveDefinite        M  Checks if the matrix X is positive definite
#  makePositiveDefinite      M  Forces the matrix x to be positive definite
#  dimnames                  R  Retrieves or sets the dimnames of an object
#  colnames|rownames         R  Retrieves or sets the row or column names 
#  colIds|rowIds             M  ... use alternatively
#  colIds<-|rowIds<-         M  ... for assignments
# SUBSETS:                  DESCRIPTION:
#  dim                       R  Returns the dimension of a matrix object
#  ncol|nrow                 R  Counts columns|rows of a matrix object
#  length                    R  Counts elements of a matrix object
#   "["|"[["                 R  Subsets a matrix object
#   (Arith)                  R  Elementwise Arithmetic: + - * /
#   (Lops)                   R  Elementwise logical Ops: > < >= <= == !=
#  cbind|rbind               R  Augments a matrix object by columns|rows
#  na.omit                   R  Removes NA from a matrix object
# BASIC STATISTICS:         DESCRIPTION:
#  var                       R  Returns the variance matrix
#  cov                       R  Returns the covariance matrix
#  col|rowStats              B  calculates column|row statistics 
#   col|rowMeans             R  calculates column|row means
#   col|rowAvgs              B  calculates column|row averages
#   col|rowVars              B  calculates column|row variances
#   col|rowStdevs            B  calculates column|row standard deviations
#   col|rowSkewness          B  calculates column|row skewness 
#   col|rowKurtosis          B  calculates column|row kurtosis 
#   col|rowCumsums           B  calculates column|row cumulated sums 
# LINEAR ALGEBRA:           DESCRIPTION:
#  t                         R  Returns the transposed matrix
#  det                       R  Returns the determinant of a matrix
#  inv                       M  returns the inverse of a matrix, synonyme
#  chol2inv                  R  Returns the inverse of a matrix
#  norm                      M  returns the norm of a matrix
#  rk                        M  returns the rank of a matrix
#  tr                        M  returns the trace of a matrix
#  %*%                       R  Returns the product of two matrices
#  %x%                       R  Returns the Kronecker product
#  kron                      S  returns the Kronecker product
#  vec                       M  is the operator that stacks a matrix
#  vech                      M  is the operator that stacks the lower triangle
# MORE LINEAR ALGEBRA:      DESCRIPTION:
#  chol                      R  Returns the Cholesky factor matrix
#  eigen                     R  Returns eigenvalues and eigenvectors
#  svd                       R  Returns the singular value decomposition
#  kappa                     R  Returns the condition number of a matrix
#  qr                        R  Returns the QR decomposition of a matrix
#  solve                     R  Solves a system of linear equations
#  backsolve                 R  ... use when the matrix is upper triangular
#  forwardsolve              R  ... use when the matrix is lower triangular
# TIME SERIES               DESCRIPTION:
#  tslag                     R  Lagged/leading vector/matrix of selected orders 
#  .tslag1                      Internal Function used by tslag
#  pdl                       R  Regressor matrix for polynomial distributed lags  
# NOTES:                   WHERE YOU FIND THE FUCTIONS?
#                            R  Basic R Package
#                            B  Rmetrics fBasics Package
#                            M  This Rmetrics fMultivar Package
################################################################################


################################################################################
#  matrix               R  Creates a matrix from the given set of values
#   diag                R  Creates a diagonal matrix or extracts diagonals
#   triang              M  Extracs the lower tridiagonal part from a matrix
#   Triang              M  Extracs the upper tridiagonal part from a matrix
#   pascal              M  Creates a Pascal matrix
#   hilbert             M  Creates a Hilbert matrix
#   colVec              M  Creates a column vector from a vector
#   rowVec              M  Creates a row vector from a vector
#  as.matrix            R  Attempts to turn its argument into a matrix     
#  is.matrix            R  Tests if its argument is a (strict) matrix
#  isPositiveDefinite   M  Checks if the matrix X is positive definite
#  makePositiveDefinite M  Forces the matrix x to be positive definite
#  dimnames             R  Retrieves or sets the dimnames of an object
#  colnames|rownames    R  Retrieves or sets the row or column names 
#  colIds|rowIds        M  ... use alternatively
#  colIds<-|rowIds<-    M  ... for assignments


triang = 
function(x) 
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Returns lower triangle matrix
        
    # FUNCTION:
    
    # Triangulate:
    x[row(x) < col(x)] = 0 
        
    # Return Value:
    x 
}
    

# ------------------------------------------------------------------------------

            
Triang = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns upper triangle matrix
    
    # FUNCTION:
    
    # Triangulate
    x[row(x) > col(x)] = 0 
    
    # Return Value:
    x 
} 
        

# ------------------------------------------------------------------------------


pascal = 
function(n) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a Pascal matrix
    
    # Arguments:
    #   n - the dimension of the square matrix
    
    # Details:
    #   http://mathworld.wolfram.com/PascalMatrix.html
    #   Pascal matrices are symmetric and positive definite. 
    #   The determinant of a Pascal matrix is 1. 
    #   The inverse of a Pascal matrix has integer entries. 
    #   If lambda is an eigenvalue of a Pascal matrix, 
    #       then 1/lambda is also an eigenvalue of the matrix.
    #   The Cholesky factor of a Pascal matrix consists of 
    #       the elements of Pascal’s triangle
        
    # FUNCTION:
    
    # Pascal:
    N = n-1
    n.over.r = function(n, r) { 
        prod(1:n) / (prod(1:(n-r)) * prod(1:r) ) }
    X = rep(1, N)
    for ( i in 1:N )
        for ( j in 1:N )
        X = c(X, n.over.r(i+j, j))
        X = cbind(rep(1, N+1), matrix(X, byrow = TRUE, ncol = N))
        
    # Return Value:
    X 
}


# ------------------------------------------------------------------------------
 

hilbert = 
function(n) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a Hilbert matrix
    
    # Arguments:
    #   n - the dimension of the square matrix
    
    # FUNCTION:
    
    # Hilbert:
    i = 1:n
    X = 1 / outer(i - 1, i, "+")
        
    # Return Value:
    X 
}


# ------------------------------------------------------------------------------


colVec = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a vector to a column vector
    
    # Details:
    #   A column vector is a matrix with one column.
    
    # Return Value:
    
    # FUNCTION:
    
    # Double Transpose:
    ans = t(t(x)) 
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


rowVec = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a vector to a row vector
    
    # Details:
    #   A row vector is a matrix with one row.
    
    # FUNCTION:
    
    # Transpose:
    ans = t(x) 
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


isPositiveDefinite =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Checks if the matrix x is positive definite
    
    # Arguments:
    #   x - a symmetric matrix or any other rectangular object
    #       describing a covariance matrix which can be converted into
    #       a matrix by the function 'as.matrix'. 
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Check if matrix is positive definite:
    ans = .is.positive.definite(m = x)
    
    # Return Value:
    ans
}


.is.positive.definite = 
function (m, tol, method = c("eigen", "chol"))
{
    # Author:
    #   Copyright 2003-05 Korbinian Strimmer
    #   Rank, condition, and positive definiteness of a matrix
    #   GNU General Public License, Version 2
    
    method = match.arg(method)
    if (!is.matrix(m)) {
        m = as.matrix(m)
    }
    if (method == "eigen") {
        eval = eigen(m, only.values = TRUE)$values
        if( missing(tol) ) {
            tol = max(dim(m))*max(abs(eval))*.Machine$double.eps
        }
        if (sum(eval > tol) == length(eval)) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    } else if (method == "chol") {
        val = try(chol(m), silent = TRUE)
        if (class(val) == "try-error") {
            return(FALSE)
        } else {
            return(TRUE)  
        }  
    }
}


# ------------------------------------------------------------------------------


makePositiveDefinite =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Forces the matrix x to be positive definite
    
    # Arguments:
    #   x - a symmetric matrix or any other rectangular object
    #       describing a covariance matrix which can be converted into
    #       a matrix by the function 'as.matrix'. 
    
    # FUNCTION:
    
    # Make Positive Definite:
    ans = .make.positive.definite(m = x)
    
    # Return Value:
    ans
}


.make.positive.definite = 
function(m, tol)
{
    # Author:
    #   Copyright 2003-05 Korbinian Strimmer
    #   Rank, condition, and positive definiteness of a matrix
    #   GNU General Public License, Version 2
    
    # Method by Higham 1988
    
    if (!is.matrix(m)) {
        m = as.matrix(m)
    }

    d = dim(m)[1] 
    if ( dim(m)[2] != d ) {
        stop("Input matrix is not square!")
    }
    
    es = eigen(m)
    esv = es$values
    
    if (missing(tol)) {
        tol = d*max(abs(esv))*.Machine$double.eps 
    }
    delta =  2*tol 
        # factor two is just to make sure the resulting
        # matrix passes all numerical tests of positive definiteness
    
    tau = pmax(0, delta - esv)
    dm = es$vectors %*% diag(tau, d) %*% t(es$vectors)    
    
    # print(max(DA))
    # print(esv[1]/delta)
      
    return( m + dm )
}
    

# ------------------------------------------------------------------------------

       
colIds = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Retrieves row names of a matrix-like object
    
    # FUNCTION:
    
    # Convert to Matrix
    x = as.matrix(x)
    
    # Return Value:
    colnames(x, ...) 
}
        

# ------------------------------------------------------------------------------

        
rowIds = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Retrieves row names of a matrix-like object
    
    # FUNCTION:
    
    # Convert to Matrix
    x = as.matrix(x)
    
    # Return Value:
    rownames(x, ...) }
        

# ------------------------------------------------------------------------------


"colIds<-" = 
function(x, value)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Sets column names of a matrix-like object
    
    # FUNCTION:
    
    # Column Names:
    dn = dimnames(x)
    if(is.null(dn)) {
        if(is.null(value)) return(x)
        if((nd = length(dim(x))) < 2)
            stop("Object has less than two dimensions")
        dn = vector("list", nd)
    }
    if(length(dn) < 2)
        stop("Object has less than two dimensions")
    if(is.null(value)) dn[2] = list(NULL) else dn[[2]] = value
    dimnames(x) = dn
    
    # Return Value:
    x
}
 

# ------------------------------------------------------------------------------

       
"rowIds<-" = 
function(x, value) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Sets row names of a matrix-like object
    
    # FUNCTION:
    
    # Row names:
    dn = dimnames(x)
    if(is.null(dn)) {
        if(is.null(value)) return(x)
        if((nd = length(dim(x))) < 1)
            stop("attempt to set rownames on object with no dimensions")
        dn = vector("list", nd) }
    if(length(dn) < 1)
        stop("attempt to set rownames on object with no dimensions")
    if(is.null(value)) dn[1] = list(NULL) else dn[[1]] = value
    dimnames(x) = dn
    
    # Return Value:
    x
}


################################################################################
#  dim                  R  Returns the dimension of a matrix object
#  ncol|nrow            R  Counts columns|rows of a matrix object
#  length               R  Counts elements of a matrix object
#   "["|"[["            R  Subsets a matrix object
#   (Arith)             R  Elementwise Arithmetic: + - * /
#   (Lops)              R  Elementwise logical Ops: > < >= <= == !=
#  cbind|rbind          R  Augments a matrix object by columns|rows
#  na.omit              R  Removes NA from a matrix object


################################################################################
#  t                    R  returns the transposed matrix
#  det                  R  returns the determinant of a matrix
#  inv|chol2inv       M|R  returns the inverse of a matrix
#  norm                 M  returns the norm of a matrix
#  rk                   M  returns the rank of a matrix
#  tr                   M  returns the trace of a matrix
#  %*%                  R  returns the product of two matrices
#  %x%|kron           R|S  returns the Kronecker product
#  vec                  M  is the operator that stacks a matrix
#  vech                 M  is the operator that stacks the lower triangle


inv = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the inverse of a matrix
    
    # FUNCTION:
    
    # Inverse:
    # ans = chol2inv(chol(x))
    # or ...
    ans = solve(x)
    
    # Return Value:
    ans 
}  


# ------------------------------------------------------------------------------


norm = 
function(x, p = 2) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the spectral norm of a matrix
    
    # Details:
    #   http://mathworld.wolfram.com/MatrixNorm.html:
    #   For p = 1
    #       The maximum absolute column sum norm |A|_1 is defined 
    #       as the maximum of the sum of the absolute valued elements
    #       of columns of the matrix.
    #   For p = 2:
    #       The spectral |A|_2 norm is "the" of a matrix. This value
    #       is computed as the square root of the maximum eigenvalue   
    #       of A^H A where A^H is the conjugate transpose.
    #   For p = Inf:
    #       The maximum absolute row sum norm |A|_inf is defined 
    #       as the maximum of the sum of the absolute valued elements
    #       of rows of the matrix.

    # FUNCTION:
    
    # Compute Norm:
    ans = NA
    if (p == 1) {
        x = abs(x)
        ans = max(apply(x, 2, sum)) 
    }
    if (p == 2) {
        ans = sqrt(max(eigen(t(x) %*% x)$values))
    }
    if (p == Inf) {
        x = abs(x)
        ans = max(apply(x, 1, sum)) 
    }
    if (is.na(ans)) stop("Invalid value for p")
        
    # Return value:
    ans
}


# ------------------------------------------------------------------------------

        
rk = 
function(x, method = c("qr", "chol")) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the rank of a matrix
    
    # FUNCTION:
    
    # Rank:
    method = method[1]
    if (method == "chol") {
        ans = attr(chol(x, pivot = TRUE), "rank") 
    } else {
        ans = qr(x)$rank 
    }
    
    # Return Value:
    ans 
}
        

# ------------------------------------------------------------------------------

    
tr = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the trace of a matrix
    
    # FUNCTION:
    
    # Trace:
    if (dim(x)[1] != dim(x)[2] ) {
        return(NA) 
    } else {
        return(sum(diag(x))) 
    } 
        
    # Return Value:
    invisible()
}          


# ------------------------------------------------------------------------------


kron = 
function(x, y) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns Kronecker product
    
    # FUNCTION:
    
    # Kronecker Product:
    ans = x %*% y 
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


vec = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   vec is the operator that stacks a matrix
    #   as a column vector:
    #   vec(X) = (X11, X21, ..., XN1, X12, X22, ..., XNN)'

    # Note:
    #   Example for a 3x3 Matrix:
    #   X11, X21, X22, X31, X32, X33
    
    # FUNCTION:
    
    # Return Value:
    t(t(as.vector(x)))
}


# ------------------------------------------------------------------------------

vech = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   vech is the operator that stacks the lower triangle
    #   of a NxN matrix as an N(N+1)/2x1 vector:
    #   vech(X) =(X11, X21, X22, X31, ..., XNN)'
    
    # Note:
    #   Example for a 3x3 Matrix:
    #   X11, X21, X22, X31, X32, X33
    
    # FUNCTION:
    
    # Return Value:
    t(x[!upper.tri(x)])
}


################################################################################
#  chol                 R  returns the Cholesky factor matrix
#  eigen                R  returns eigenvalues and eigenvectors
#  svd                  R  returns the singular value decomposition
#  kappa                R  returns the condition number of a matrix
#  qr                   R  returns the QR decomposition of a matrix
#  solve                R  solves a system of linear equations
#  backsolve            R  ... use when the matrix is upper triangular
#  forwardsolve         R  ... use when the matrix is lower triangular


################################################################################
#  tslag                R  Lagged or leading vector/matrix of selected order(s)
#  .tslag1                 Internal Function used by tslag
#  pdl                  R  Regressor matrix for polynomial distributed lags


.tslag1 = 
function(x, k) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function used by function tslag.
          
    # FUNCTION:
    y = x
    if (k > 0) y = c(rep(NA, times = k), x[1:(length(x)-k)])
    if (k < 0) y = c(x[(-k+1):length(x)], rep(NA, times = -k))
    
    # Return Value:
    y 
}


# ------------------------------------------------------------------------------


tslag = 
function(x, k = 1, trim = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a lagged or leading vector/matrix of selected order(s).
    
    # Arguments:
    #   x - a vector of data, missing values (NA) are allowed. 
    #   k - the number of positions the new series is to lag 
    #       or to lead the input series. 
    #   trim - a logical flag, if TRUE, the missing values at the 
    #       beginning or end of the returned series will be trimmed. 
    #       The default value is FALSE. 
    
    # Details:
    #   With a positive value of "k" we get a lagged series and with
    #   a negative value we get a leading series. 
    
    # Examples:
    #   tslag(rnorm(10), 2)
    #   tslag(rnorm(10), -2:2)
    #   tslag(rnorm(10), -2:2, trim = TRUE)
    
    # FUNCTION:
        
    # Bind:
    ans = NULL
    for ( i in k) {
        ans = cbind(ans, .tslag1(x, i)) 
    }
        
    # Trim:
    if (trim) {
        indexes = (1:length(ans[,1]))[!is.na(apply(ans, 1, sum))]
        ans = ans[indexes, ] 
    }
        
    # As Vector:
    if (length(k) == 1) ans = as.vector(ans)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


pdl = 
function(x, d = 2, q = 3, trim = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Regressor matrix for polynomial distributed lags
    
    # Aruments:
    #   x - a numeric vector.
    #   d - an integer specifying the order of the polynomial. 
    #   q - an integer specifying the number of lags to use in 
    #       creating polynomial distributed lags. This must be 
    #       greater than d. 
    #   trim - a logical flag; if TRUE, the missing values at 
    #       the beginning of the returned matrix will be trimmed. 

    # Value:
    #   Returns a matrix representing the regressor matrix. 

    # Example:
    #   stack.loss = c(
    #       42, 37, 37, 28, 18, 18, 19, 20, 15, 14, 14, 
    #       13, 11, 12,  8,  7,  8,  8,  9, 15, 15)
    #   pdl(stack.loss)
    
    # FUNCTION:
    
    # Check:
    stopifnot(q > d)

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

