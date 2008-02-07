
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# LINEAR ALGEBRA:           DESCRIPTION:
#  inv                       Returns the inverse of a matrix, synonyme
#  norm                      Returns the norm of a matrix
#  rk                        Returns the rank of a matrix
#  tr                        Returns the trace of a matrix
#  kron                      Returns the Kronecker product
# R BASE FUNCTIONS:         DESCRIPTION:
#  t                         Returns the transposed matrix
#  det                       Returns the determinant of a matrix
#  chol2inv                  Returns the inverse of a matrix
#  %*%                       Returns the product of two matrices
#  %x%                       Returns the Kronecker product
#  chol                      Returns the Cholesky factor matrix
#  eigen                     Returns eigenvalues and eigenvectors
#  svd                       Returns the singular value decomposition
#  kappa                     Returns the condition number of a matrix
#  qr                        Returns the QR decomposition of a matrix
#  solve                     Solves a system of linear equations
#  backsolve                 ... use when the matrix is upper triangular
#  forwardsolve              ... use when the matrix is lower triangular
################################################################################


inv <-
    function(x)
{
    # A function implemented by Diethelm Wuertz

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


norm <-
    function(x, p = 2)
{
    # A function implemented by Diethelm Wuertz

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


rk <-
    function(x, method = c("qr", "chol"))
{
    # A function implemented by Diethelm Wuertz

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


tr <-
    function(x)
{
    # A function implemented by Diethelm Wuertz

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


kron <-
    function(x, y)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns Kronecker product

    # FUNCTION:

    # Kronecker Product:
    ans = x %*% y

    # Return Value:
    ans
}


################################################################################

