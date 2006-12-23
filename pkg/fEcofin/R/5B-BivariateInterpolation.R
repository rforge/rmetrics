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
# FUNCTION:           BIVARIATE INTERPOLATION:
#  akimaInterp         Interpolates and Smoothes Irregularly Distributed Points
#  krigeInterp         Kriges Irregularly Distributed Data Points
# .circlesPlot         Creates a scatterplot with circle size as third variable
################################################################################


akimaInterp =   
function(x, y = NULL, z = NULL, gridPoints = 21,
xo = seq(min(x), max(x), length = gridPoints),
yo = seq(min(y), max(y), length = gridPoints), extrap = TRUE)
{   # A function implemented by Diethelm Wuertz

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

    # Requirements:
    #   akima Builtin Fortran Code.
    
    # Example:
    #   x = runif(999)-0.5; y = runif(999)-0.5; z = cos(2*pi*(x^2+y^2))
    #   ans = akimaInterp(x, y, z, extrap = FALSE)
    #   persp(ans, theta = -50, phi = 30, col = "steelblue")
    
    # Note:
    #   Uses Fortran akima Builtin
    
    # FUNCTION:
    
    # Arguments:
    if (is.list(x)) x = matrix(unlist(x), ncol = 3)
    if (is.data.frame(x)) x = as.matrix.data.frame(x)
    if (is.matrix(x)) {
        z = x[, 3]
        y = x[, 2]
        x = x[, 1]
    }
    
    # Settings:
    ncp = NULL
    duplicate = "median"
    dupfun = NULL
    linear = FALSE
    
    # Interpolation:
    if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
        stop("missing values and Infs not allowed")
    drx = diff(range(x))
    dry = diff(range(y))
    if (drx == 0 || dry == 0)
        stop("all data collinear")  
    if (drx/dry > 10000 || drx/dry < 0.0001)
        stop("scales of x and y are too dissimilar")
    n = length(x)
    nx = length(xo)
    ny = length(yo)
    xy = paste(x, y, sep = ",")
    i = match(xy, xy)
    z = unlist(lapply(split(z, i), median))
    ord = !duplicated(xy)
    x = x[ord]
    y = y[ord]
    n = length(x)
    zo = matrix(0, nx, ny)
    storage.mode(zo) = "double"
    miss = !extrap   
    extrap = matrix(TRUE, nx, ny)
    ans = .Fortran("sdsf3p",
        as.integer(1),
        as.integer(n),
        as.double(x),
        as.double(y),
        as.double(z),
        as.integer(nx),
        x = as.double(xo),
        as.integer(ny),
        y = as.double(yo),
        z = zo,
        ier = integer(1),
        double(36 * n),
        integer(25 * n),
        extrap = as.logical(extrap),
        near = integer(n),
        nxt = integer(n),
        dist = double(n),
        PACKAGE = "fEcofin")
    temp = ans[c("x", "y", "z", "extrap")]
    if (miss) temp$z[temp$extrap] = NA
    ans = temp[c("x", "y", "z")]
    class(ans) = "gridData"
     
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


krigeInterp = 
function(x, y = NULL, z = NULL, gridPoints = 21,
xo = seq(min(x), max(x), length = gridPoints),
yo = seq(min(y), max(y), length = gridPoints), extrap = TRUE, polDegree = 6)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Kriges Irregularly Distributed Data Points
    
    # Arguments:
    #   x, y, z - either three numeric vectors of equal length or if 
    #       y and z are NULL, a list with entries x, y, a, or named 
    #       data.frame with x in the first, y in the second, and z in
    #       the third column.
    #   gridPoints - number of grid points in x and y direction.
    #   xo, yo, a sequence of data points spanning the grid
    #   extrap - a logical, if TRUE then the data points are extrapolated.
    #   polDegree - polynomial degree, an integer ranging between 1 and 6.
    
    # Value:
    #   A list with three elements, $x and $y which are vectors of length 
    #   'gridPoints' and $z which is a matrix of size 'gridPoints^2'.
    
    # Example:
    #   x = runif(999)-0.5; y = runif(999)-0.5; z = cos(2*pi*(x^2+y^2))
    #   require(spatial)
    #   ans = krigeInterp(x, y, z, extrap = FALSE)
    #   persp(ans, theta = -50, phi = 30, col = "steelblue")
    
    # Note:
    #   Requires Recommended R Package "spatial"
    
    # FUNCTION:
    
    # Arguments:
    if (is.list(x)) x = matrix(unlist(akima), ncol = 3)
    if (is.data.frame(x)) x = as.matrix.data.frame(x)
    if (is.matrix(x)) {
        z = x[, 3]
        y = x[, 2]
        x = x[, 1]
    }
    
    # Interpolate:
    krige = surf.gls(np = polDegree, covmod = expcov, 
        x = x, y = y, z = z, d = 0.5, alpha = 1)
    ans = prmat(krige, 
        xl = min(xo), xu = max(xo), yl = min(yo), yu = max(yo), 
        n = gridPoints-1)
      
    # Extrapolate ? 
    # - this should be done more efficiently
    if (!extrap) {
        E = akimaInterp(x = x, y = y, z = z, gridPoints = gridPoints, 
            extrap = extrap)
        ans$z[is.na(E$z)] = NA
    }
    class(ans) = "gridData"
    
    # Return Value:
    ans
}
    
    
################################################################################


.circlesPlot = 
function(x, y = NULL, z = NULL, scale = 1, points = TRUE, 
labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:  
    #   Creates a scatterplot with circle size as third variable
    
    # Example:
    #   circlesPlot(x=rnorm(50), y=rnorm(50), z=rnorm(50))
    #   circlesPlot(x=rnorm(50), y=rnorm(50), z=rnorm(50), labels= FALSE)
 
    # FUNCTION:
    
    # Transfor Input:
    if (is.list(x)) x = matrix(unlist(x), ncol = 3)
    if (is.data.frame(x)) x = as.matrix.data.frame(x)
    if (is.matrix(x)) {
        z = x[, 3]
        y = x[, 2]
        x = x[, 1]
    }
    nX = length(x)
    nY = length(y)
    nZ = length(z)
    stopifnot(nX == nY)
    stopifnot(nX == nZ || nX*nY == nZ)
    
    # Create Circle Plot:
    if (labels) {
        plot(x, y, type = "n")
    } else {
        plot(x, y, xlab = "", ylab = "", type = "n")
    }
    symbols(x, y, add = TRUE, circles = abs(z)^scale, inches = 0.25, 
        fg = "black", bg = "steelblue", ...) 
    X = x[z<0]
    Y = y[z<0]
    Z = z[z<0]
    symbols(X, Y, add = TRUE, circles = abs(Z)^scale, inches = 0.25, 
        fg = "black", bg = "orange", ...)
    if (points) points(x, y, pch = 19)
    grid()
    
    # Return Value:
    invisible(NULL)
}


################################################################################

