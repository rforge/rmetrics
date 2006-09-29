
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
# GENERATION:           DESCRIPTION:
#  matrix               R  Creates a matrix from the given set of values
#   diag                R  Creates a diagonal matrix or extracts diagonals
#   triang              M  Extracs the lower tridiagonal part from a matrix
#   Triang              M  Extracs the upper tridiagonal part from a matrix
#   pascal              M  Creates a Pascal matrix
#   colVec              M  Creates a column vector from a vector
#   rowVec              M  Creates a row vector from a vector
#  as.matrix            R  Attempts to turn its argument into a matrix     
#  is.matrix            R  Tests if its argument is a (strict) matrix
#  dimnames             R  Retrieves or sets the dimnames of an object
#  colnames|rownames    R  Retrieves or sets the row or column names 
#  colIds|rowIds        M  ... use alternatively
#  colIds<-|rowIds<-    M  ... for assignments
# SUBSETS:              DESCRIPTION:
#  dim                  R  Returns the dimension of a matrix object
#  ncol|nrow            R  Counts columns|rows of a matrix object
#  length               R  Counts elements of a matrix object
#   "["|"[["            R  Subsets a matrix object
#   (Arith)             R  Elementwise Arithmetic: + - * /
#   (Lops)              R  Elementwise logical Ops: > < >= <= == !=
#  cbind|rbind          R  Augments a matrix object by columns|rows
#  na.omit              R  Removes NA from a matrix object
# BASIC STATISTICS:     DESCRIPTION:
#  var                  R  returns the variance matrix
#  cov                  R  returns the covariance matrix
#  col|rowStats         B  calculates column|row statistics 
#   col|rowMeans        R  calculates column|row means
#   col|rowAvgs         B  calculates column|row averages
#   col|rowVars         B  calculates column|row variances
#   col|rowStdevs       B  calculates column|row standard deviations
#   col|rowSkewness     B  calculates column|row skewness 
#   col|rowKurtosis     B  calculates column|row kurtosis 
#   col|rowCumsums      B  calculates column|row cumulated sums 
# LINEAR ALGEBRA:       DESCRIPTION:
#  t                    R  returns the transposed matrix
#  det                  R  returns the determinant of a matrix
#  inv|chol2inv       M|R  returns the inverse of a matrix
#  norm                 M  returns the norm of a matrix
#  rk                   M  returns the rank of a matrix
#  tr                   M  returns the trace of a matrix
#  %*%                  R  returns the product of two matrices
#  %x%|kron           R|S  returns the Kronecker product
#  mexp                 M  computes the exponential of a square matrix
#  vec                  M  is the operator that stacks a matrix
#  vech                 M  is the operator that stacks the lower triangle
# MORE LINEAR ALGEBRA:  DESCRIPTION:
#  chol                 R  returns the Cholesky factor matrix
#  eigen                R  returns eigenvalues and eigenvectors
#  svd                  R  returns the singular value decomposition
#  kappa                R  returns the condition number of a matrix
#  qr                   R  returns the QR decomposition of a matrix
#  solve                R  solves a system of linear equations
#  backsolve            R  ... use when the matrix is upper triangular
#  forwardsolve         R  ... use when the matrix is lower triangular
# TIME SERIES           DESCRIPTION:
#  tslag                R  Lagged or leading vector/matrix of selected order(s)
#  .tslag1                 Internal Function used by tslag
#  pdl                  R  Regressor matrix for polynomial distributed lags  
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(VectorMatrixAddon); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

test.xxx =
function()
{
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

if (FALSE) {
    require(RUnit)
    testResult = runTestFile("C:/Rmetrics/SVN/trunk/fMultivar/test/runit6A.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
