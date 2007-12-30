
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 BIVARIATE GRIDDED INTERPOLATION:
#  linearInterp             Interpolates linearly irregularly spaced data points
#  linearInterpp            Interpolates linearly pointwise
#  akimaInterp              Interpolates and smoothes irregularly spaced points
#  akimaInterpp             Interpolates and smoothes pointwise
#  krigeInterp              Kriges irregularly distributed data points
################################################################################


################################################################################
# FUNCTION:                 BIVARIATE GRIDDED INTERPOLATION:
#  linearInterp             Interpolates linearly irregularly spaced data points
#  linearInterpp            Interpolates linearly pointwise
#  akimaInterp              Interpolates and smoothes irregularly spaced points
#  akimaInterpp             Interpolates and smoothes pointwise
#  krigeInterp              Kriges irregularly distributed data points


linearInterp <- 
    function(x, y = NULL, z = NULL, gridPoints = 21,
    xo = seq(min(x), max(x), length = gridPoints),
    yo = seq(min(y), max(y), length = gridPoints))
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Interpolates Linearly Irregularly Distributed Data Points
    
    # Arguments:
    #   x, y, z - either three numeric vectors of equal length or if 
    #       y and z are NULL, a list with entries x, y, a, or named 
    #       data.frame with x in the first, y in the second, and z in
    #       the third column.
    #   gridPoints - number of grid points in x and y direction.
    #   xo, yo, a sequence of data points spanning the grid
    
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
    #   ans = linearInterp(x, y, z)
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
    
    # Interpolation:
    ans = .interp.old(x, y, z, xo, yo, ncp = 0, extrap = FALSE, 
        duplicate = "median", dupfun = NULL)
    colnames(ans$z) = as.character(signif(ans$x, round(log(gridPoints), 0)))
    rownames(ans$z) = as.character(signif(ans$y, round(log(gridPoints), 0)))
    class(ans) = "gridData"

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


linearInterpp <- 
    function(x, y = NULL, z = NULL, xo, yo)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Interpolates Linearly Irregularly Distributed Data Points
    
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
    #   ans = linearInterpp(x, y, z, c(mean(x), 0, 100), c(mean(y), 0, 100))
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
    
    # Interpolation:
    ans = .interpp.old(x, y, z, xo, yo, ncp = 0, extrap = FALSE, 
        duplicate = "median", dupfun = NULL)
    ans = data.frame(x = ans$x, y = ans$y, z = ans$z)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


akimaInterp <- 
    function(x, y = NULL, z = NULL, gridPoints = 21,
    xo = seq(min(x), max(x), length = gridPoints),
    yo = seq(min(y), max(y), length = gridPoints), extrap = FALSE)
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
    #   xo, yo, a sequence of data points spanning the grid
    #   extrap - a logical, if TRUE then the data points are extrapolated.\
    
    # Value:
    #   A list with three elements, $x and $y which are vectors of length 
    #   'gridPoints' and $z which is a matrix of size 'gridPoints^2'.

    # Requirements:
    #   akima Builtin Fortran Code.
    
    # Example:
    #   set.seed(1953)
    #   x = runif(999)-0.5; y = runif(999)-0.5; z = cos(2*pi*(x^2+y^2))
    #   ans = akimaInterp(x, y, z, extrap = FALSE)
    #   persp(ans, theta = -50, phi = 30, col = "steelblue")
    #   ans = akimaInterp(x, y, z, extrap = TRUE)
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
    
    # Interpolation:
    ans = .interp.new(x, y, z, xo, yo, linear = FALSE, ncp = NULL,
        extrap = extrap, duplicate = "median", dupfun = NULL)
    colnames(ans$z) = as.character(signif(ans$x, round(log(gridPoints), 0)))
    rownames(ans$z) = as.character(signif(ans$y, round(log(gridPoints), 0)))
    class(ans) = "gridData"

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
    
    # Arguments:
    if (is.list(x)) x = matrix(unlist(x), ncol = 3)
    if (is.data.frame(x)) x = as.matrix.data.frame(x)
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
    ans = .interpp.new(x, y, z, xo, yo, 
        extrap = extrap, duplicate = "median", dupfun = NULL)       
    ans = data.frame(matrix(unlist(t(ans)), 3))
    colnames(ans) = c("x", "y", "z")

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


krigeInterp <- 
    function(x, y = NULL, z = NULL, gridPoints = 21,
    xo = seq(min(x), max(x), length = gridPoints),
    yo = seq(min(y), max(y), length = gridPoints), extrap = FALSE, 
    polDegree = 6)
{   
    # A function implemented by Diethelm Wuertz

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
    if (is.list(x)) x = matrix(unlist(x), ncol = 3)
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
# Note, do linear Interpolation with the "old" algorithm, Spline Interpolation
# with the "new" algorithm!


.interp.old <- 
    function (x, y, z, xo = seq(min(x), max(x), length = 40), yo = seq(min(y), 
    max(y), length = 40), ncp = 0, extrap = FALSE, duplicate = "error", 
    dupfun = NULL) 
{   
    # A copy from contributed package akima

    if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z)))) 
        stop("missing values and Infs not allowed")
    if (ncp > 25) {
        ncp <- 25
        cat("ncp too large, using ncp=25\n")
    }
    drx <- diff(range(x))
    dry <- diff(range(y))
    if (drx == 0 || dry == 0) 
        stop("all data collinear")
    if (drx/dry > 10000 || drx/dry < 1e-04) 
        stop("scales of x and y are too dissimilar")
    n <- length(x)
    nx <- length(xo)
    ny <- length(yo)
    if (length(y) != n || length(z) != n) 
        stop("Lengths of x, y, and z do not match")
    xy <- paste(x, y, sep = ",")
    if (duplicate == "error") {
        if (any(duplicated(xy))) 
            stop("duplicate data points: need to set 'duplicate = ..' ")
    } else {
        i <- match(xy, xy)
        if (duplicate == "user") 
            dupfun <- match.fun(dupfun)
        ord <- !duplicated(xy)
        if (duplicate != "strip") {
            centre <- function(x) switch(duplicate, mean = mean(x), 
                median = median(x), user = dupfun(x))
            z <- unlist(lapply(split(z, i), centre))
        } else {
            z <- z[ord]
        }
        x <- x[ord]
        y <- y[ord]
        n <- length(x)
    }
    zo <- matrix(0, nx, ny)
    storage.mode(zo) <- "double"
    miss <- !extrap
    misso <- matrix(miss, nx, ny)
    if (extrap & ncp == 0) 
        warning("Cannot extrapolate with linear option")
    ans <- .Fortran("idsfft", as.integer(1), as.integer(ncp), 
        as.integer(n), as.double(x), as.double(y), as.double(z), 
        as.integer(nx), as.integer(ny), x = as.double(xo), y = as.double(yo), 
        z = zo, integer((31 + ncp) * n + nx * ny), double(5 * 
            n), misso = as.logical(misso), PACKAGE = "akima")[c("x", 
        "y", "z", "misso")]
    ans$z[ans$misso] <- NA
    ans[c("x", "y", "z")]
}


# ------------------------------------------------------------------------------


.interp.new <- 
    function (x, y, z, xo = seq(min(x), max(x), length = 40), yo = seq(min(y), 
    max(y), length = 40), linear = FALSE, ncp = NULL, extrap = FALSE, 
    duplicate = "error", dupfun = NULL) 
{   
    # A copy from contributed package akima

    if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z)))) 
        stop("missing values and Infs not allowed")
    if (!is.null(ncp)) {
        if (ncp != 0) 
            warning("'ncp' not supported, automatically choosen by Fortran code\n")
        else linear <- TRUE
    }
    if (linear) 
        stop("linear interpolation not implemented in interp.new().\n", 
            "use 'interp()' (or 'interp.old()').")
    drx <- diff(range(x))
    dry <- diff(range(y))
    if (drx == 0 || dry == 0) 
        stop("all data collinear")
    if (drx/dry > 10000 || drx/dry < 1e-04) 
        stop("scales of x and y are too dissimilar")
    n <- length(x)
    nx <- length(xo)
    ny <- length(yo)
    if (length(y) != n || length(z) != n) 
        stop("Lengths of x, y, and z do not match")
    xy <- paste(x, y, sep = ",")
    if (duplicate == "error") {
        if (any(duplicated(xy))) 
            stop("duplicate data points: need to set 'duplicate = ..' ")
    } else {
        i <- match(xy, xy)
        if (duplicate == "user") 
            dupfun <- match.fun(dupfun)
        ord <- !duplicated(xy)
        if (duplicate != "strip") {
            centre <- function(x) switch(duplicate, mean = mean(x), 
                median = median(x), user = dupfun(x))
            z <- unlist(lapply(split(z, i), centre))
        } else {
            z <- z[ord]
        }
        x <- x[ord]
        y <- y[ord]
        n <- length(x)
    }
    zo <- matrix(0, nx, ny)
    storage.mode(zo) <- "double"
    miss <- !extrap
    misso <- matrix(TRUE, nx, ny)
    if (extrap && if (is.null(ncp)) 
        linear
    else (ncp == 0)) 
        warning("Cannot extrapolate with linear option")
    ans <- .Fortran("sdsf3p", as.integer(1), as.integer(n), as.double(x), 
        as.double(y), as.double(z), as.integer(nx), x = as.double(xo), 
        as.integer(ny), y = as.double(yo), z = zo, ier = integer(1), 
        double(36 * n), integer(25 * n), extrap = as.logical(misso), 
        near = integer(n), nxt = integer(n), dist = double(n), 
        PACKAGE = "akima")[c("x", "y", "z", "extrap")]
    if (miss) 
        ans$z[ans$extrap] <- NA
    ans[c("x", "y", "z")]
}


# ------------------------------------------------------------------------------


.interpp.old <- 
    function (x, y, z, xo, yo, ncp = 0, extrap = FALSE, duplicate = "error", 
    dupfun = NULL) 
{   
    # A copy from contributed package akima

    if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z)))) 
        stop("missing values and Infs not allowed")
    if (is.null(xo)) 
        stop("xo missing")
    if (is.null(yo)) 
        stop("yo missing")
    if (ncp > 25) {
        ncp <- 25
        cat("ncp too large, using ncp=25\n")
    }
    drx <- diff(range(x))
    dry <- diff(range(y))
    if (drx == 0 || dry == 0) 
        stop("all data collinear")
    if (drx/dry > 10000 || drx/dry < 1e-04) 
        stop("scales of x and y are too dissimilar")
    n <- length(x)
    np <- length(xo)
    if (length(yo) != np) 
        stop("length of xo and yo differ")
    if (length(y) != n || length(z) != n) 
        stop("Lengths of x, y, and z do not match")
    xy <- paste(x, y, sep = ",")
    i <- match(xy, xy)
    if (duplicate == "user" && !is.function(dupfun)) 
        stop("duplicate=\"user\" requires dupfun to be set to a function")
    if (duplicate != "error") {
        centre <- function(x) {
            switch(duplicate, mean = mean(x), median = median(x), 
                user = dupfun(x))
        }
        if (duplicate != "strip") {
            z <- unlist(lapply(split(z, i), centre))
            ord <- !duplicated(xy)
            x <- x[ord]
            y <- y[ord]
            n <- length(x)
        } else {
            ord <- (hist(i, plot = FALSE, freq = TRUE, breaks = seq(0.5, 
                max(i) + 0.5, 1))$counts == 1)
            x <- x[ord]
            y <- y[ord]
            z <- z[ord]
            n <- length(x)
        }
    } else if (any(duplicated(xy))) 
        stop("duplicate data points")
    zo <- rep(0, np)
    storage.mode(zo) <- "double"
    miss <- !extrap
    misso <- seq(miss, np)
    if (extrap & ncp == 0) 
        warning("Cannot extrapolate with linear option")
    ans <- .Fortran("idbvip", as.integer(1), as.integer(ncp), 
        as.integer(n), as.double(x), as.double(y), as.double(z), 
        as.integer(np), x = as.double(xo), y = as.double(yo), 
        z = zo, integer((31 + ncp) * n + np), double(8 * n), 
        misso = as.logical(misso), PACKAGE = "akima")
    temp <- ans[c("x", "y", "z", "misso")]
    temp$z[temp$misso] <- NA
    temp[c("x", "y", "z")]
}


# ------------------------------------------------------------------------------


.interpp.new <- 
    function (x, y, z, xo, yo, extrap = FALSE, duplicate = "error", 
    dupfun = NULL) 
{   
    # A copy from contributed package akima

    if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z)))) 
        stop("missing values and Infs not allowed")
    if (is.null(xo)) 
        stop("xo missing")
    if (is.null(yo)) 
        stop("yo missing")
    drx <- diff(range(x))
    dry <- diff(range(y))
    if (drx == 0 || dry == 0) 
        stop("all data collinear")
    if (drx/dry > 10000 || drx/dry < 1e-04) 
        stop("scales of x and y are too dissimilar")
    n <- length(x)
    np <- length(xo)
    if (length(yo) != np) 
        stop("length of xo and yo differ")
    if (length(y) != n || length(z) != n) 
        stop("Lengths of x, y, and z do not match")
    xy <- paste(x, y, sep = ",")
    i <- match(xy, xy)
    if (duplicate == "user" && !is.function(dupfun)) 
        stop("duplicate=\"user\" requires dupfun to be set to a function")
    if (duplicate != "error") {
        centre <- function(x) {
            switch(duplicate, mean = mean(x), median = median(x), 
                user = dupfun(x))
        }
        if (duplicate != "strip") {
            z <- unlist(lapply(split(z, i), centre))
            ord <- !duplicated(xy)
            x <- x[ord]
            y <- y[ord]
            n <- length(x)
        }
        else {
            ord <- (hist(i, plot = FALSE, freq = TRUE, breaks = seq(0.5, 
                max(i) + 0.5, 1))$counts == 1)
            x <- x[ord]
            y <- y[ord]
            z <- z[ord]
            n <- length(x)
        }
    } else if (any(duplicated(xy))) 
        stop("duplicate data points")
    zo <- rep(0, np)
    storage.mode(zo) <- "double"
    miss <- !extrap
    extrap <- seq(TRUE, np)
    ans <- .Fortran("sdbi3p", as.integer(1), as.integer(n), as.double(x), 
        as.double(y), as.double(z), as.integer(np), x = as.double(xo), 
        y = as.double(yo), z = zo, ier = integer(1), wk = double(17 * 
            n), iwk = integer(25 * n), extrap = as.logical(extrap), 
        near = integer(n), nxt = integer(n), dist = double(n), 
        PACKAGE = "akima")
    temp <- ans[c("x", "y", "z", "extrap")]
    if (miss) 
        temp$z[temp$extrap] <- NA
    temp[c("x", "y", "z")]
}


################################################################################

