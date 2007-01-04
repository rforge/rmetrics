\name{VectorMatrixAddon}

\alias{VectorMatrixAddon}

\alias{triang}
\alias{Triang}
\alias{pascal}
\alias{colVec}
\alias{rowVec}
\alias{colIds}
\alias{rowIds}
\alias{colIds<-}
\alias{rowIds<-}

\alias{isPositiveDefinite}
\alias{makePositiveDefinite}

\alias{inv}
\alias{norm}
\alias{rk}
\alias{tr}
\alias{kron}

\alias{vec}
\alias{vech}


\alias{tslag}
\alias{pdl}


\title{Vector/Matrix Arithmetics and Linear Algebra}


\description{
    
    A collection and description of functions 
    available for matrix arithmetics and linear 
    algebra. 
    \cr
    
    Only functions which have been added by Rmetrics 
    are documented. These functions are often useful 
    for the manipulation of multivariate time series. 
    \cr
    
    The origin of the functions is marked in the following way:
    
    R: part of R's base packages, \cr
    B: part of Rmetrics' fBasics package, \cr
    M: part of this Rmetrics package, fMultivar.
    \cr
    
    The functions are listed by topic.
    \cr
 
    General Matrix Functions:
    
    \tabular{ll}{  
    \code{matrix} \tab Creates a matrix from the given set of values, \cr
    \code{diag} \tab Creates a diagonal matrix or extracts diagonals, \cr
    \code{triang} \tab Extracs the lower tridiagonal part from a matrix, \cr
    \code{Triang} \tab Extracs the upper tridiagonal part from a matrix, \cr
    \code{pascal} \tab Creates a Pascal matrix, \cr
    \code{hilbert} \tab Creates a Hilbert matrix, \cr
    \code{colVec} \tab Creates a column vector from a vector, \cr
    \code{rowVec} \tab Creates a row vector from a vector, \cr
    \code{as.matrix} \tab Attempts to turn its argument into a matrix, \cr
    \code{is.matrix} \tab Tests if its argument is a (strict) matrix, \cr
    \code{isPositiveDefinite} \tab Checks if a matrix is positive definite, \cr
    \code{makePositiveDefinite} \tab Forces a matrix to be positive definite, \cr
    \code{dimnames} \tab Retrieves or sets the dimnames of an object, \cr
    \code{colnames} \tab Retrieves or sets the column names, \cr 
    \code{colIds} \tab ... use alternatively, \cr 
    \code{rownames} \tab Retrieves or sets the row names, \cr   
    \code{rowIds} \tab ... use alternatively. }
    
    Simple Matrix Operations:
            
    \tabular{ll}{ 
    \code{dim} \tab Returns the dimension of a matrix object, \cr
    \code{ncol} \tab Counts columns of a matrix object, \cr 
    \code{nrow} \tab Counts rows of a matrix object, \cr 
    \code{length} \tab Counts elements of a matrix object, \cr 
    \code{"["} \tab Subsets a matrix object, \cr
    \code{"[["} \tab Subsets a matrix object, \cr
    \code{cbind} \tab Augments a matrix object by columns, \cr 
    \code{rbind} \tab Augments a matrix object by rows. } 

    Basic Statistical Functions:
            
    \tabular{ll}{
    \code{var} \tab Returns the variance matrix, \cr
    \code{cov} \tab Returns the covariance matrix, \cr 
    \code{colStats} \tab Calculates column statistics, \cr
    \code{rowStats} \tab Calculates row statistics, \cr
    \code{colMeans} \tab Calculates column means, \cr 
    \code{rowMeans} \tab Calculates row means, \cr 
    \code{colAvgs} \tab Calculates column averages, \cr
    \code{rowAvgs} \tab Calculates row averages, \cr
    \code{colVars} \tab Calculates column variances, \cr 
    \code{rowVars} \tab Calculates row variances, \cr 
    \code{colStdevs} \tab Calculates column standard deviations, \cr
    \code{rowStdevs} \tab Calculates row standard deviations, \cr
    \code{colSkewness} \tab Calculates column skewness, \cr
    \code{rowSkewness} \tab Calculates row skewness, \cr
    \code{colKurtosis} \tab Calculates column kurtosis, \cr 
    \code{rowKurtosis} \tab Calculates row kurtosis, \cr 
    \code{colCumsums} \tab Calculates column cumulated sums, \cr  
    \code{rowCumsums} \tab Calculates row cumulated sums. }  
            
    Linear algebra:
    
    \tabular{ll}{
    \code{\%*\%} \tab Returns the product of two matrices, \cr
    \code{\%x\%}, \code{kron} \tab Returns the Kronecker product, \cr 
    \code{det} \tab Returns the determinante of a matrix, \cr
    \code{inv} \tab Returns the inverse of a matrix, \cr
    \code{norm} \tab Returns the norm of a matrix, \cr 
    \code{rk} \tab Returns the rank of a matrix, \cr
    \code{tr} \tab Returns trace of a matrix, \cr 
    \code{t} \tab Returns the transposed matrix, \cr
    \code{vech} \tab Is the operator that stacks the lower triangle, \cr
    \code{vec} \tab Is the operator that stacks a matrix.}
    
    More linear algebra:
    
    \tabular{ll}{
    \code{chol} \tab Returns the Cholesky factor matrix, \cr
    \code{eigen} \tab Returns eigenvalues and eigenvectors, \cr 
    \code{svd} \tab Returns the singular value decomposition, \cr
    \code{kappa} \tab Returns the condition number of a matrix, \cr 
    \code{qr} \tab Returns the QR decomposition of a matrix, \cr
    \code{solve} \tab Solves a system of linear equations, \cr
    \code{backsolve} \tab ... use when the matrix is upper triangular, \cr
    \code{forwardsolve} \tab ... use when the matrix is lower triangular. } 
    
    Time Series Generation:
    
    \tabular{ll}{
    \code{tslag} \tab Lagged or leading vector/matrix of selected order(s), \cr
    \code{pdl} \tab Regressor matrix for polynomial distributed lags. } 
     

}


\usage{
triang(x)
Triang(x)
pascal(n)
hilbert(n)

isPositiveDefinite(x)
makePositiveDefinite(x)

colVec(x)
rowVec(x)
colIds(x, \dots)
rowIds(x, \dots)

isPositiveDefinite(x)
makePositiveDefinite(x)

inv(x)
norm(x, p = 2)
rk(x, method = c("qr", "chol"))
tr(x)
kron(x, y)

vec(x)
vech(x)

pdl(x, d = 2, q = 3, trim = FALSE)
tslag(x, k = 1, trim = FALSE)
}


\arguments{

    \item{d}{
        [pdl] - \cr
        an integer specifying the order of the polynomial. 
        }
    \item{k}{
        [tslag] - \cr
        an integer value, the number of positions the new series is 
        to lag or to lead the input series. 
        }
    \item{method}{
        [rk] - \cr
        a character value, the dimension of the square matrix. One can
        choose from two methods: For \code{method = "qr"} the rank is
        computed as \code{qr(x)\$rank}, or alternatively for 
        \code{method="chol"} the rank is
        computed as \code{attr(chol(x, pivot=TRUE), "rank")}.
        }
    \item{n}{
        [pascal] - \cr
        an integer value, the dimension of the square matrix.
        }
    \item{p}{
        [norm] - \cr
        an integer value, \code{1}, \code{2} or \code{Inf}.
        \code{p=1} - The maximum absolute column sum norm which is defined 
        as the maximum of the sum of the absolute valued elements of columns 
        of the matrix.
        \code{p=2} - The spectral norm is "the norm" of a matrix \code{X}. 
        This value is computed as the square root of the maximum eigenvalue 
        of \code{CX} where \code{C} is the conjugate transpose.
        \code{p=Inf} - The maximum absolute row sum norm is defined 
        as the maximum of the sum of the absolute valued elements
        of rows of the matrix.
        }
    \item{q}{
        [pdl] - \cr
        an integer specifying the number of lags to use in 
        creating polynomial distributed lags. This must be 
        greater than d. 
        }
    \item{trim}{
        [pdl] - \cr
        a logical flag; if TRUE, the missing values at 
        the beginning of the returned matrix will be trimmed. 
        \cr
        [tslag] - \cr
         a logical flag, if TRUE, the missing values at the 
        beginning or end of the returned series will be trimmed. 
        The default value is FALSE. 
        }
    \item{x, y}{
        a numeric matrix.
        \cr
        [tslag] - \cr
        a numeric vector, missing values are allowed.
        \cr
        [pdl] - \cr
        a numeric vector.
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\details{

    \bold{Function from R's Base Package:}
    \cr
    
    Most of the functions are described in their R help pages which we
    recommend to consult for further information. For the additiotnal 
    functions added by Rmetrics we give a brief introduction.
    \cr
    
    
    \bold{General Functions:}
    \cr
    
    Functions to generate matrices and related functions are described 
    in the help page \code{\link{matrix}}. To "decorate" these objects 
    several naming functions are available, a description can be found 
    on the help pages \code{\link{dimnames}} and \code{\link{rownames}}.
    \cr
    
    The function \code{pascal} generates a Pascal matrix of order \code{n} 
    which is a symmetric, positive, definite matrix with integer entries 
    made up from Pascal's triangle. The determinant of a Pascal matrix is 1. 
    The inverse of a Pascal matrix has integer entries. If \code{lambda} 
    is an eigenvalue of a Pascal matrix, then \code{1/lambda} is also an 
    eigenvalue of the matrix. Pascal matrices are ill-conditioned. 
    \cr
    
    The function \code{hilbert} generates a Hilbert matrix of order \code{n}.
    \cr
    
    The functions \code{triang} and \code{Triang} allow to transform a
    square matrix to a lower or upper triangular form. 
    A triangular matrix is either an upper triangular matrix or lower 
    triangular matrix. For the first case all matrix elements \code{a[i,j]}
    of matrix \code{A} are zero for \code{i>j}, whereas in the second case
    we have just the opposite situation. A lower triangular matrix is 
    sometimes also called left triangular. In fact, triangular matrices 
    are so useful that much computational linear algebra begins with 
    factoring or decomposing a general matrix or matrices into triangular 
    form. Some matrix factorization methods are the Cholesky factorization 
    and the LU-factorization. Even including the factorization step, 
    enough later operations are typically avoided to yield an overall 
    time savings. Triangular matrices have the following properties: the 
    inverse of a triangular matrix is a triangular matrix, the product of 
    two triangular matrices is a triangular matrix, the determinant of a 
    triangular matrix is the product of the diagonal elements, the 
    eigenvalues of a triangular matrix are the diagonal elements.
    \cr
    
    The function \code{isPositiveDefinite} checks if a square matrix is 
    positive definite, and the function \code{makePositiveDefinite} forces 
    a matrix to be positive definite.
    \cr
    
    The functions \code{colVec} and \code{rowVec} transform a vector into 
    a column and row vector, respectively. A column vector is a matrix 
    object with one column, and a row vector is a matrix object with one row.
    \cr
    
    The functions \code{dimnames}, \code{colname}, and \code{rowname}
    can be used to retrieve and set the names of matrices. The functions 
    \code{rowIds}, \code{colIds}, are S-Plus like synonyms.
    \cr
    
    
    \bold{Simple Matrix Operations:}
    \cr
    
    The functions \code{\link{dim}}, \code{\link{nrow}} and 
    \code{\link{ncol}} are functions to extract the dimension and  
    the number of rows and columns of a matrix. 
    \cr
    
    The usual arithmetic operators, logical operators and mathematical 
    functions like \code{sqrt} or \code{exp} and \code{log}
    operate on matrices element by element. Note, that \code{"*"}
    is not matrix multiplication, instead we have to use \code{"\%*\%"}.
    \cr
    
    The methods \code{"["} and \code{"[["} are suited to extract
    subsets from a matrix, to delete rows and columns, or to permute
    rows and columns. 
    \cr
    
    
    \bold{Basic Statistical Functions:}
    \cr
    
    The functions \code{var} and \code{cov} compute the variance and
    covariance of a matrix.
    \cr
    
    Beside these functions \code{\link{colMeans}} and \code{\link{rowMeans}} 
    are R functions which compute the mean of columns and rows of a matrix. 
    Rmetrics has added further functions to compute column- or rowwise 
    variances, standard deviations, skewness, kurtosis and cumulated sums. 
    Two general  functions named \code{rowStats} and 
    \code{colStats} allow to apply through the argument list any 
    function to compute row and column statistics from matrices.
    \cr
    
    
    \code{Linear Algebra:}
    \cr
    
    Matrix multiplication is done using the operator \code{\%*\%}. 
    \cr
    
    The \emph{Kronecker product} can be computed using the operator 
    \code{\%x\%} or alternatively using the function \code{kron}.
    \cr

    The function \code{det} computes the determinant of a matrix.
    \cr
    
    The inverse of a square matrix \code{inv(X)} of dimension
    \code{n} is defined so that 
    \code{X \%*\% inv(X) = inv(X) \%*\% X = diag(n)} where
    the matrix \code{diag(n)} is the \code{n}-dimensional identity matrix.
    A precondition for the existence of the matrix inverse is that the
    determinant of the matrix \code{det(X)} is nonzero. 
    \cr

    The function \code{t} computes the transposed of a square matrix.
    \cr
    
    The function \code{vec} implements the operator that stacks a matrix
    as a column vector, to be more precise in a matrix with one column.
    vec(X) = (X11, X21, ..., XN1, X12, X22, ..., XNN).
    The function \code{vech} implements the operator that stacks the lower 
    triangle of a NxN matrix as an N(N+1)/2x1 vector:
    vech(X) =(X11, X21, X22, X31, ..., XNN), to be more precise in a 
    matrix with one row.
    \cr

    The function \code{tr} computes the trace of a square matrix which
    is the sum of the diagonal elements of the matrix under consideration.
    \cr
    
    The function \code{rk} computes the rank of a matrix which is 
    the dimension of the range of the matrix corresponding to the number 
    of linearly independent rows or columns of the matrix, or to the 
    number of nonzero singular values. The rank of a matrix is also
    named inear map. 
    \cr
    
    The function \code{norm} computes the norm of a matrix. Three choices 
    are possible: 
    \code{p=1} - The maximum absolute column sum norm which is defined 
    as the maximum of the sum of the absolute valued elements of columns 
    of the matrix.
    \code{p=2} - The spectral norm is "the norm" of a matrix \code{X}. 
    This value is computed as the square root of the maximum eigenvalue 
    of \code{CX} where \code{C} is the conjugate transpose.
    \code{p=Inf} - The maximum absolute row sum norm is defined 
    as the maximum of the sum of the absolute valued elements
    of rows of the matrix.
    \cr
    

    \code{More Linear Algebra:}
    \cr
    
    The function \code{chol} returns the Cholesky factor matrix,
    \code{eigen} returns eigenvalues and eigenvectors,
    \code{svd} returns the singular value decomposition,
    \code{kappa} estimate the condition number of a matrix,
    \code{qr} returns the QR decomposition of a matrix,
    \code{ginv} returns the Moore-Penrose generalized inverse,
    \code{solve} solves a system of linear equations, use
    \code{backsolve} when the matrix is upper triangular, and
    \code{forwardsolve} when the matrix is lower triangular.
    \cr
    
    
    \code{Time Series:}
    \cr
    The function \code{pdl} returns a regressor matrix suitable 
    for polynomial distributed lags. 
    \cr
    The function \code{tslag} returns a lagged/led vector or matrix 
    for given time series data.      
}


\references{

Higham N.J., (2002);
    \emph{Accuracy and Stability of Numerical Algorithms}, 
    2nd ed., SIAM.

Golub, van Loan, (1996);
    \emph{Matrix Computations}, 
    3rd edition. Johns Hopkins University Press. 
    
Ward, R.C., (1977);  
    \emph{Numerical computation of the matrix exponential with 
    accuracy estimate},
    SIAM J. Num. Anal. 14, 600--610.

Moler C., van Loan C., (2003);
    \emph{Nineteen dubious ways to compute the exponential of a matrix, 
    twenty-five years later},
    SIAM Review 45, 3--49.
}


\examples{
## Create Pascal Matrix:
   P = pascal(3)
   P
   # Create lower triangle matrix
   L = triang(P)
   L
   # Extract diagonal part
   diag(P)
   
## Add/Subtract/Multiply/Divide:  
   X = P
   # Multiply matrix with a constant
   3 * X
   # Multiply two matrices elementwise
   X * P                     
   # Multiplies rows/columns of a matrix by a vector
   X \%*\% diag(P)            
   diag(P) \%*\% X           
       
## Operate on Subsets of a Matrix:
   n = 3; i = 2; j = 3
   D = diag(1:3)
   # Return the dimension of a matrix
   dim(P)                         
   # Get the last colum of a matrix
   P[, ncol(P)]                   
   # Delete a column of a matrix
   P[, -i]                      
   # Permute the columns of a matrix
   P[c(3, 1, 2), ]              
   # Augments matrix horizontally 
   cbind(P, D)                           
      
## Apply a function to all Elements of a Matrix: 
   # Return square root for each element
   sqrt(P)
   # Exponentiate the matrix elementwise
   exp(P)
   # Compute the median of each column
   apply(P, 2, "median") 
   # Test on all elements of a matrix       
   all( P > 2 )   
   # test on any element in a matrix                
   any( P > 2 )                  
     
## More Matrix Operations:
   # Return the product of two matrices
   P \%*\% D   
   # Return the Kronecker Product                     
   P \%x\% D                        
   # Return the transposed matrix
   t(P)                           
   # Return the inverse of a matrix
   inv(P)  
   # Return the norm of a matrix                      
   norm(P)                        
   # Return the determinante of a matrix
   det(P)                         
   # Return the rank of a matrix
   rk(P)                            
   # Return trace of a matrix
   tr(P)                          
   # Return the variance matrix
   var(P)     
   # Return the covariance matrix                   
   cov(P) 
   # Stack a matrix
   vec(P) 
   # Stack the lower triangle
   vech(P)
   
## More Linear Algebra:
   X = P; b = c(1, 2, 3)
   # Return the Cholesky factor matrix
   chol(X)                        
   # Return eigenvalues and eigenvectors
   eigen(X)                       
   # Return the singular value decomposition
   svd(X)                         
   # Return the condition number of a matrix
   kappa(X)                       
   # Return the QR decomposition of a matrix
   qr(X)                          
   # Solve a system of linear equations
   # ... use backsolve when the matrix is upper triangular
   # ... use forwardsolve when the matrix is lower triangular
   solve(X, b)  
   backsolve(Triang(X), b)
   solve(Triang(X), b)                 
   forwardsolve(triang(X), b) 
   solve(triang(X), b)                                        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{math}

