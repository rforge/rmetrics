
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
# You should have received A copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:                DESCRIPTION:
#  akimaInterp              Interpolates irregularly spaced points
#  akimaInterpp             Interpolates and smoothes pointwise
################################################################################

akimaInterp <-
  function(x, y = NULL, z = NULL, gridPoints = 21,
           xo = seq(min(x), max(x), length = gridPoints),
           yo = seq(min(y), max(y), length = gridPoints), extrap = FALSE)
  {
    # A function implemented by Diethelm Wuertz

### ==> ../man/stats-interpAkima.Rd
###            ~~~~~~~~~~~~~~~~~~~~

    # Description:
    #   Interpolates and Smoothes Irregularly Distributed Data Points

    # Arguments:
    #   x, y, z - either three numeric vectors of equal length or if
    #       y and z are NULL, a list with entries x, y, a, or named
    #       data.frame with x in the first, y in the second, and z in
    #       the third column.
    #   gridPoints - number of grid points in x and y direction.
    #   xo, yo, a sequence of data points spanning the grid
    #   extrap - a logical, if TRUE then the data points are extrapolated.\

    # Value:
    #   A list with three elements, $x and $y which are vectors of length
    #   'gridPoints' and $z which is a matrix of size 'gridPoints^2'.

    # Example:
    #   set.seed(1953)
    #   x = runif(999)-0.5; y = runif(999)-0.5; z = cos(2*pi*(x^2+y^2))
    #   ans = akimaInterp(x, y, z, extrap = FALSE)
    #   persp(ans, theta = -50, phi = 30, col = "steelblue")
    #   ans = akimaInterp(x, y, z, extrap = TRUE)
    #   persp(ans, theta = -50, phi = 30, col = "steelblue")

    # FUNCTION:

    if (!requireNamespace("akima", quietly = TRUE))
      stop("Needs Package 'akima' which is not auto-installed because of a different licence\n")

    # Arguments:
    if (is.data.frame(x))
        x <- as.matrix.data.frame(x)
    else if (is.list(x))
        x <- matrix(unlist(x), ncol = 3)
    if (is.matrix(x)) {
      z = x[, 3]
      y = x[, 2]
      x = x[, 1]
    }

    # Interpolation:
    ans <- akima::interp(x, y, z, xo, yo, linear = FALSE,
                         extrap = extrap, duplicate = "median", dupfun = NULL)
    colnames(ans$z) <- as.character(signif(ans$x, round(log(gridPoints), 0)))
    rownames(ans$z) <- as.character(signif(ans$y, round(log(gridPoints), 0)))
    class(ans) <- "gridData"

    # Return Value:
    ans
  }

# ------------------------------------------------------------------------------

akimaInterpp <-
  function(x, y = NULL, z = NULL, xo, yo, extrap = FALSE)
  {
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Interpolates and Smoothes Irregularly Distributed Data Points

    # Arguments:
    #   x, y, z - either three numeric vectors of equal length or if
    #       y and z are NULL, a list with entries x, y, a, or named
    #       data.frame with x in the first, y in the second, and z in
    #       the third column.
    #   gridPoints - number of grid points in x and y direction.
    #   xo, yo, a sequence of data points for pointwise interpolation

    # Note:
    #   Extrapolation is not possible in the case of linear interpolation.

    # Value:
    #   A list with three elements, $x and $y which are vectors of length
    #   'gridPoints' and $z which is a matrix of size 'gridPoints^2'.

    # Requirements:
    #   akima Builtin Fortran Code.

    # Example:
    #   set.seed(1953)
    #   x = runif(999)-0.5; y = runif(999)-0.5; z = cos(2*pi*(x^2+y^2))
    #   ans = akimaInterpp(x, y, z, c(mean(x), 0, 100), c(mean(y), 0, 100))
    #   persp(ans, theta = -50, phi = 30, col = "steelblue")

    # Note:
    #   Uses Fortran akima Builtin

    # FUNCTION:

    if (!requireNamespace("akima", quietly = TRUE))
      stop("Needs package 'akima' which is not auto-installed because of a different licence\n")

    # Arguments:
    if (is.data.frame(x))
        x <- as.matrix.data.frame(x)
    else if (is.list(x))
        x <- matrix(unlist(x), ncol = 3)
    if (is.matrix(x)) {
      z = x[, 3]
      y = x[, 2]
      x = x[, 1]
    }

    # Settings:
    duplicate = "median"
    dupfun = NULL
    linear = TRUE

    # Interpolation:
    interpp <- eval(parse(text=paste0("akima",":::","interpp")))
    ans <- interpp(x, y, z, xo, yo, linear = FALSE,
                   extrap = extrap, duplicate = "median", dupfun = NULL)
    ans <- data.frame(matrix(unlist(t(ans)), 3))
    colnames(ans) = c("x", "y", "z")

    # Return Value:
    ans
  }

################################################################################
