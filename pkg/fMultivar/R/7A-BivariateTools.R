
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
# FUNCTION:             DESCRIPTION:
#  grid2d                Returns from two vectors x-y grid coordinates
#  density2d             Returns 2D Kernel Density Estimates
#  hist2d                Returns 2D Histogram Counts
#  integrate2d           Integrates over a two dimensional unit square
# FUNCTION:             BIVARIATE DISTRIBUTIONS:
#  pnorm2d               Computes bivariate Normal probability function
#  dnorm2d               Computes bivariate Normal density function
#  rnorm2d               Generates bivariate normal random deviates
#  pcauchy2d             Computes bivariate Cauchy probability function
#  dcauchy2d             Computes bivariate Cauchy density function
#  rcauchy2d             Generates bivariate Cauchy random deviates
#  pt2d                  Computes bivariate Student-t probability function
#  dt2d                  Computes bivariate Student-t density function
#  rt2d                  Generates bivariate Student-t random deviates
# FUNCTION:             ELLIPTICAL DISTRIBUTIONS:
#  delliptical2d         Computes density for elliptical distributions
#  .gfunc2d              Generator Function for elliptical distributions
#  .delliptical2dSlider  Slider for bivariate densities
#  .delliptical2d.RUnit  RUnit Test for elliptical distributions
################################################################################


grid2d =
function(x = (0:10)/10, y = x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates from two vectors x-y grid coordinates
    
    # Arguments:
    #   x, y - two numeric vectors defining the x and y coordinates.
    
    # Value:
    #   returns a list with two vectors named $x and $y spanning the 
    #   grid defined by the coordinates x and y.
    
    # Example:
    #   > grid2d(1:3, 1:2)
    #       $x
    #       [1] 1 2 3 1 2 3
    #       $y
    #       [1] 1 1 1 2 2 2

    # FUNCTION: 
    
    # Prepare for Input:
    nx  = length(x)
    ny  = length(y)
    xoy = cbind(rep(x, ny), as.vector(matrix(y, nx, ny, byrow = TRUE)))
    XY  = matrix(xoy, nx * ny, 2, byrow = FALSE)
    
    # Return Value:
    list(x = XY[, 1], y = XY[, 2])
}


# ------------------------------------------------------------------------------


density2d = 
function (x, y = NULL, n = 20, h = NULL, limits = c(range(x), range(y))) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns 2D Kernel Density Estimates
    
    # Arguments:
    #   x, y - two vectors of coordinates of data. If y is NULL then x
    #       is assumed to be a two column matrix, where the first column 
    #       contains the x data, and the second column the y data. 
    #   n - Number of grid points in each direction. 
    #   h - a vector of bandwidths for x and y directions. Defaults to
    #       normal reference bandwidth. 
    #   limits - the limits of the rectangle covered by the grid.    
    
    # Value:
    #    A list with three elements x, y, and z. x and y are vectors
    #       spanning the two dimensioanl grid and z the corresponding
    #       matrix. The output can directly serve as input to the
    #       plotting functions image, contour and persp.
    
    # Details:
    #   Two-dimensional kernel density estimation with an axis-aligned
    #   bivariate normal kernel, evaluated on a square grid.
    
    # Note:
    #   Partly copied from R Package MASS, function 'kde2d'.
    
    # Reference:
    #   Venables, W.N., Ripley, B. D. (2002); 
    #       Modern Applied Statistics with S.
    #       Fourth edition, Springer.
    
    # FUNCTION:
    
    # Settings:
    lims = limits
    if (is.null(y)) {
        y = x[, 2]
        x = x[, 1]
    }
    
    # Bandwidth:
    .bandwidth.nrd = function (x) {
        r = quantile(x, c(0.25, 0.75))
        h = (r[2] - r[1])/1.34
        4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5) }
        
    # Kernel Density Estimator:
    nx = length(x)
    if (length(y) != nx) stop("Data vectors must be the same length")
    gx = seq(lims[1], lims[2], length = n)
    gy = seq(lims[3], lims[4], length = n)
    if (is.null(h)) h = c(.bandwidth.nrd(x), .bandwidth.nrd(y))
    h = h/4
    ax = outer(gx, x, "-")/h[1]
    ay = outer(gy, y, "-")/h[2]
    z = matrix(dnorm(ax), n, nx) %*% t(matrix(dnorm(ay), n, 
        nx))/(nx * h[1] * h[2])
    
    # Return Value:    
    list(x = gx, y = gy, z = z)
}


# ------------------------------------------------------------------------------


hist2d = 
function(x, y = NULL, n = c(20, 20))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns 2D Histogram Counts
    
    # Arguments:
    #   x, y - two vectors of coordinates of data. If y is NULL then x 
    #       is assumed to be a two column matrix, where the first column 
    #       contains the x data, and the second column the y data. 
    #   n - number of bins in each dimension, may be a scalar or a 2
    #       element vector. The default value is 20.
    
    # Value:
    #    A list with three elements x, y, and z. x and y are vectors
    #       spanning the two dimensioanl grid and z the corresponding
    #       matrix. The output can directly serve as input to the
    #       plotting functions image, contour and persp.
   
    # Note:
    #   Partly copied from R Package gregmisc, function 'hist2d'.
    
    # FUNCTION:
    
    # 2D Histogram Counts:
    if (is.null(y)) {
        y = x[, 2]
        x = x[, 1]
    }
    if (length(n) == 1) {
        nbins = c(n, n)
    } else {
        nbins = n
    }
    nas = is.na(x) | is.na(y)
    x.cuts = seq(from = min(x, y), to = max(x,y), length = nbins[1]+1)
    y.cuts = seq(from = min(x, y), to = max(x,y), length = nbins[2]+1)
    index.x = cut(x, x.cuts, include.lowest = TRUE)
    index.y = cut(y, y.cuts, include.lowest = TRUE)
    m = matrix(0, nrow=nbins[1], ncol = nbins[2],
        dimnames = list( levels(index.x), levels(index.y) ) )
    for ( i in 1:length(index.x) ) {
        m[index.x[i], index.y[i] ] = m[index.x[i], index.y[i] ] + 1
    }
    xvals = x.cuts[1:nbins[1]]
    yvals = y.cuts[1:nbins[2]]

    # Return Value:
    list(x = xvals, y = yvals, z = m)
}


# ------------------------------------------------------------------------------


integrate2d = function(fun, error = 1.0e-5, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   2-dimension quadrature rule on [0,1]^2 
    
    # Arguments:
    #   fun - function to be integrated. The first argument requests
    #       the x values, the second the y values, and the remaining
    #       are reserved for additional parameters.
    #   ... - parameters passed to the function to be integrated
    
    # Details:
    #   see: Abramowitz and Stegun, p. 892
    
    # FUNCTION:
    
    # Estimate a reasonable number of subintervals:
    H = sqrt(sqrt(error))
    n = ceiling(1/H + 1)
    blocks = ceiling(log(n+1)/log(2))
    n = 2^blocks-1
    h = 1/(n-1)
    
    # The error will be of order h^4:
    error = h^4
    
    # Create all grid coordinates:
    x = y = h*seq(1, n-1, by = 2)
    nx = ny = length(x)
    xoy = cbind(rep(x, ny), as.vector(matrix(y, nx, ny, byrow = TRUE)))
    XY = matrix(xoy, nx * ny, 2, byrow = FALSE)
    
    # The integration rule:      
    rule = function(x, h, ...) {         
        X = x[1] + h*c(  0, -1, -1,  1,  1, -1,  1,  0,  0)
        Y = x[2] + h*c(  0, -1,  1, -1,  1,  0,  0, -1,  1)
        W =  c( 16,  1,  1,  1,  1,  4,  4,  4,  4)/36
        ans = sum( W * fun(X, Y, ...) )
    }
     
    # Result:
    ans = (4*h^2)*sum(apply(XY, 1, rule, h = h, ...))
    
    # Return Value:
    list(value = ans, error = error, points = n)
}



################################################################################
# BIVARIATE DISTRIBUTIONS:


pnorm2d = 
function(x, y = x, rho = 0) 
{   # pnorm2d: A copy from R package "sn"

    # Description:
    #   Computes bivariate Normal probability function
    
    # Arguments:
    #   x, y - two numeric values or vectors of the same length at
    #       which the probability will be computed. 
    
    # Value:
    #   returns a numeric vector of probabilities of the same length
    #   as the input vectors
   
    # FUNCTION:
    
    # Probaility:
    X = cbind(x, y)
    ans = apply(X, 1, .pnorm2d, rho = rho)
    attr(ans, "control") = c(rho = rho)
    
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.pnorm2d = 
function(X, rho = 0) 
{   # pnorm2d: A copy from R package "sn"

    # Description:
    #   Bivariate Normal probability function
    
    # Arguments:
    #   x, y - two numeric values at which the probability will 
    #   be computed. 
    
    # Value:
    #   returns a numeric vector of probabilities of the same length
    #   as the input vectors
   
    # FUNCTION:
    
    # Probability:
    x = X[1]
    y = X[2]
    if (x == 0 & y == 0) {
        return(0.25 + asin(rho)/(2 * pi))
    }
    p = 0.5 * (pnorm(x) + pnorm(y))
    if (x == 0) {
        p = p - 0.25 * sign(y)
    } else {
        if (is.finite(x)) {
            Y = (y - rho * x)/(x * sqrt(1 - rho^2))
        } else {
            Y = -rho/sqrt(1-rho^2)
        }
        p = p - .TOwen(x, Y)
    }
    if (y == 0) {
        p = p - 0.25 * sign(x)
    } else {
        if (is.finite(y)) {
            X = (x - rho * y)/(y * sqrt(1 - rho^2))
        } else {
            X = -rho/sqrt(1-rho^2)
        }
        p = p - .TOwen(y, X)
    }
    if (is.finite(x) & is.finite(y)) {
        if ((x * y < 0) | ((x * y == 0) & (x + y) < 0)) {
            p = p - 0.5
        }
    }
    
    # Return Value:
    return(p)
}


# ------------------------------------------------------------------------------


.TInt = 
function(h, a, jmax, cut.point) 
{   # T.int: A copy from R package "sn"

    # Note:
    #   Required by .pnorm2d and .TOwen
    
    # FUNCTION:
    
    .fui = function(h, i) (h^(2 * i))/((2^i) * gamma(i + 1))
    seriesL = seriesH = NULL
    i = 0:jmax
    low = (h <= cut.point)
    hL = h[low]
    hH = h[!low]
    L = length(hL)
    if (L > 0) {
        b = outer(hL, i, .fui)
        cumb = apply(b, 1, cumsum)
        b1 = exp(-0.5 * hL^2) * t(cumb)
        matr = matrix(1, jmax + 1, L) - t(b1)
        jk = rep(c(1, -1), jmax)[1:(jmax + 1)]/(2 * i + 1)
        matr = t(matr * jk) %*% a^(2 * i + 1)
        seriesL = (atan(a) - as.vector(matr))/(2 * pi)
    }
    if (length(hH) > 0) {
        seriesH = atan(a) * exp(-0.5 * (hH^2) * a/atan(a)) * 
            (1 + 0.00868 * (hH^4) * a^4)/(2 * pi)
    }
    series = c(seriesL, seriesH)
    id = c((1:length(h))[low], (1:length(h))[!low])
    series[id] = series
    
    # Return Value:
    series
}


# ------------------------------------------------------------------------------


.TOwen = 
function (h, a, jmax = 50, cut.point = 6) 
{   # T.Owen: A copy from R package "sn"

    # Note:
    #   Required by .pnorm2d
    
    # FUNCTION:
    
    if (!is.vector(a) | length(a) > 1) 
        stop("a must be a vector of length 1")
    if (!is.vector(h)) 
        stop("h must be a vector")
    aa = abs(a)
    ah = abs(h)
    if (aa == Inf) 
        return(0.5 * pnorm(-ah))
    if (aa == 0) 
        return(rep(0, length(h)))
    na = is.na(h)
    inf = (ah == Inf)
    ah = replace(ah, (na | inf), 0)
    if (aa <= 1) { 
        owen = .TInt(ah, aa, jmax, cut.point)
    } else {
        owen = 0.5 * pnorm(ah) + pnorm(aa * ah) * (0.5 - pnorm(ah)) - 
            .TInt(aa * ah, (1/aa), jmax, cut.point)
    }
    owen = replace(owen, na, NA)
    owen = replace(owen, inf, 0)
    ans = return(owen * sign(a))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


dnorm2d =
function(x, y = x, rho = 0)
{   # A function implemented by Diethelm Wuertz

    # Arguments:
    #   x,y - two numeric vectors
    #   rho - the linear correlation, a numeric value between 
    #       minus one and one.
    
    # FUNCTION:
    
    # Argument:
    xoy = (x^2 - 2*rho*x*y + y^2)/ (2*(1 - rho^2))
    
    # Density:
    density = exp(-xoy) / ( 2*pi*sqrt(1-rho^2))
    attr(density, "control") = c(rho = rho)
    
    # Return Value:
    density
}


# ------------------------------------------------------------------------------


.dnorm2d = 
function(x, y = x, rho = 0)
{   # A function implemented by Diethelm Wuertz

    # Arguments:
    #   x,y - two numeric vectors
    #   rho - the linear correlation, a numeric value between 
    #       minus one and one.
    
    # Note:
    #   Partly copied from contributed R package 'mvtnorm'
    #   Author Friedrich Leisch
    
    # FUNCTION
    
    # Settings:
    mean = c(0,0)
    sigma = diag(2) 
    sigma[1,2] = sigma[2,1] = rho 
    log = FALSE
    x = cbind(x, y)
    
    # From mvtnorm - Check:
    if (is.vector(x)) {
        x = matrix(x, ncol = length(x))
    }
    if (missing(mean)) {
        mean = rep(0, length = ncol(x))
    }
    if (missing(sigma)) {
        sigma = diag(ncol(x))
    }
    if (ncol(x) != ncol(sigma)) {
        stop("x and sigma have non-conforming size")
    }
    if (nrow(sigma) != ncol(sigma)) {
        stop("sigma meanst be a square matrix")
    }
    if (length(mean) != nrow(sigma)) {
        stop("mean and sigma have non-conforming size")
    }
    
    # From mvtnorm - Density:
    distval = mahalanobis(x, center = mean, cov = sigma)
    logdet = sum(log(eigen(sigma, symmetric = TRUE, only.values = TRUE)$values))
    logretval = -(ncol(x)*log(2*pi) + logdet + distval)/2
    if(log) return(logretval)
    ans = exp(logretval)
    attr(ans, "control") = c(rho = rho)
    
    # Return value:
    ans
}


# ------------------------------------------------------------------------------


rnorm2d =
function(n, rho = 0) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates bivariate normal random deviates
    
    # Arguments:
    #   n - number of random deviates to be generated
    #   rho - the linear correlation, a numeric value between 
    #       minus one and one.
    
    # Note:
    #   Partly copied from contributed R package 'mvtnorm'
    #   Author Friedrich Leisch
    
    # FUNCTION
    
    # Settings:
    mean = c(0,0)
    sigma = diag(2) 
    sigma[1,2] = sigma[2,1] = rho 
    
    # From mvtnorm - Random Numbers:
    ev = eigen(sigma, sym = TRUE)$values
    if (!all(ev >= -sqrt(.Machine$double.eps) * abs(ev[1]))) 
        warning("sigma is numerically not positive definite")
    sigsvd = svd(sigma)
    ans = t(sigsvd$v %*% (t(sigsvd$u) * sqrt(sigsvd$d)))
    ans = matrix(rnorm(n * ncol(sigma)), nrow = n) %*% ans
    ans = sweep(ans, 2, mean, "+")
    attr(ans, "control") = c(rho = rho)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.rnorm2d = 
function(n, rho = 0)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Alternative direct algorithm from Lindskog Master Thesis
    
    # Arguments:
    #   n - number of random deviates to be generated
    #   rho - the linear correlation, a numeric value between 
    #       minus one and one.
    
    # FUNCTION:
    
    # Random Deviates
    x = matrix(c(1, rho, rho,1), 2)
    V = NULL
    U = chol(x)
    siz = dim(x)[1]
    for(i in 1:n) {
        Z = rnorm(siz)
        res = t(U)%*%Z
        V = cbind(V,res)
    }
    rmn = t(V)
    
    # Return Value:
    rmn
}


# ******************************************************************************


pcauchy2d = 
function(x, y = x, rho = 0) 
{   # A function Implemented by Diethelm Wuertz

    # Description:
    #   Computes bivariate Cauchy probability function
    
    # Arguments:
    #   x, y - two numeric values or vectors of the same length at
    #       which the probability will be computed. 
    
    # Example:
    #   pt2d(rnorm(5), rnorm(5), 0.5, 5)
    
    # Value:
    #   returns a numeric vector of probabilities of the same length
    #   as the input vectors
   
    # FUNCTION:
    
    # Settings:
    # Probaility:
    ans  = pt2d(x = x, y = y, rho = rho, nu = 1) 
    attr(ans, "control") = c(rho = rho)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


dcauchy2d = 
function(x, y = x, rho = 0)
{   # A function implemented by Diethelm Wuertz

    # Arguments:
    #   n - number of random deviates to be generated
    #   rho - the linear correlation, a numeric value between 
    #       minus one and one.
    
    # Description:
    #   Computes bivariate Cauchy density function
    
    # Note:
    #   Partly copied from contributed R package 'sn'
    
    # FUNCTION:
    
    # Density:
    density = dt2d(x = x, y = y, rho = rho, nu = 1)
    attr(density, "control") = c(rho = rho)
    
    # Return value:
    density
}


# ------------------------------------------------------------------------------


rcauchy2d =
function(n, rho = 0) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates bivariate Cauchy random deviates
    
    # Arguments:
    #   n - number of random deviates to be generated
    #   rho - the linear correlation, a numeric value between 
    #       minus one and one.
    
    # Note:
    #   Partly copied from contributed R package 'mvtnorm'
    #   Author Friedrich Leisch
    
    # FUNCTION:
    
    # Random Deviates:
    ans = rt2d(n = n, rho = rho)
    attr(ans, "control") = c(rho = rho)
    
    # Return Value:
    ans
}


# ******************************************************************************


pt2d = 
function(x, y = x, rho = 0, nu = 4) 
{   # pnorm2d: A copy from R package "sn"

    # Description:
    #   Computes bivariate Student-t probability function
    
    # Arguments:
    #   x, y - two numeric values or vectors of the same length at
    #       which the probability will be computed. 
    
    # Example:
    #   pt2d(rnorm(5), rnorm(5), 0.5, 5)
    
    # Value:
    #   returns a numeric vector of probabilities of the same length
    #   as the input vectors
   
    # FUNCTION:
    
    # Normal Limit:
    if (nu == Inf) return(pnorm2d(x = x, y = y, rho = rho)) 
    
    # Settings:
    sigma = diag(2) 
    sigma[1, 2] = sigma[2, 1] = rho 
    X = cbind(x, y)
    
    # Probaility:
    ans  = pmvst(X, dim = 2, mu = c(0, 0), Omega = sigma, 
        alpha = c(0, 0), df = nu) 
    attr(ans, "control") = c(rho = rho, nu = nu)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


dt2d = 
function(x, y = x, rho = 0, nu = 4)
{   # A function implemented by Diethelm Wuertz

    # Arguments:
    #   n - number of random deviates to be generated
    #   rho - the linear correlation, a numeric value between 
    #       minus one and one.
    
    # Description:
    #   Computes bivariate Student-t density function
    
    # Example:
    #   dt2d(rnorm(5), rnorm(5), 0.5, 5)
    
    # Note:
    #   Partly copied from contributed R package 'sn'
    
    # FUNCTION:
    
    # Normal Limit:
    if (nu == Inf) return(dnorm2d(x = x, y = y, rho = rho)) 
    
    # Argument:
    xoy = (x^2 - 2*rho*x*y + y^2)/ (2*(1 - rho^2))
    
    # Density:
    density = (1 + 2*xoy/nu)^(-(nu+2)/2) / (2*pi*sqrt(1-rho^2))
    attr(density, "control") = c(rho = rho, nu = nu)
    
    # Return value:
    density
}


# ------------------------------------------------------------------------------


rt2d =
function(n, rho = 0, nu = 4) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates bivariate Student-t random deviates
    
    # Arguments:
    #   n - number of random deviates to be generated
    #   rho - the linear correlation, a numeric value between 
    #       minus one and one.
    
    # Note:
    #   Partly copied from contributed R package 'mvtnorm'
    #   Author Friedrich Leisch
    
    # FUNCTION:
    
    # Normal Limit:
    if (nu == Inf) return(rnorm2d(n = n, rho = rho)) 
    
    # Random Deviates:
    ans = rnorm2d(n, rho)/sqrt(rchisq(n, nu)/nu)
    attr(ans, "control") = c(rho = rho, nu = nu)
    
    # Return Value:
    ans
}


################################################################################


delliptical2d =
function(x, y = x, rho = 0, param = NULL, type = c("norm", "cauchy", "t", 
"logistic", "laplace", "kotz", "epower"), output = c("vector", "list"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Density function for bivariate elliptical distributions
    
    # Arguments:
    #   x, y -  two numeric vectors of the same length.
    #   rho -  a anumeric value specifying the correlation.
    #   param - NULL, a numeric value, or a numeric vector adding
    #       additional parameters to the generator function.
    #   type -  a character string denoting the type of distribution.
    #       This may be either 
    #       "norm" for the normal distribution, or
    #       "cauchy" for the Cauchy distribution, or
    #       "t" for the Student-t distribution, or
    #       "logistic" for the logistic distribution, or
    #       "laplace" for the distribution, or
    #       "kotz" for the original Kotz distribution, or 
    #       "epower" for the exponential power distribution
        
    # FUNCTION:
    
    # Type:
    type = type[1]
    
    # Settings:
    if (is.list(x)) {
        y = x$y
        x = x$x
    }
    if (is.matrix(x)) {
        y = x[, 2]
        x = x[, 2]
    }

    # Add Default Parameters:
    if (is.null(param)) {
        if (type == "t") param = c(nu = 4)
        if (type == "kotz") param = c(r = sqrt(2))
        if (type == "epower") param = c(r = sqrt(2), s = 1/2)
    }
    
    # Density:
    xoy = ( x^2 - 2*rho*x*y + y^2 ) / (1-rho^2)
    lambda = .gfunc2d(param = param, type = type)[[1]]
    density = lambda * .gfunc2d(x = xoy, param = param, type = type) /
        sqrt(1 - rho^2)
        
    # Add attributes:
    if (is.null(param)) {
        attr(density, "control") = unlist(list(type = type, rho = rho))
    } else {
        attr(density, "control") = unlist(list(type = type, rho = rho, 
            param = param))
    }
    
    # As List ?
    if (output[1] == "list") {
        N = sqrt(length(x))
        x = x[1:N]
        y = matrix(y, ncol = N)[1, ]
        density = list(x = x, y = y,  z = matrix(density, ncol = N))
    }
    
    # Return Value:
    density
}


# ------------------------------------------------------------------------------


.gfunc2d = 
function(x, param = NULL, type = c("norm", "cauchy", "t", "logistic", 
"laplace", "kotz", "epower"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generator function for elliptical distributions
    
    # Note:
    #   A copy from fExtremes 'gfunc'
    
    # Arguments:
    #   x -  a numeric vector
    #   param - NULL, a numeric value, or a numeric vector adding.
    #       additional parameters to the generator function.
    #   type -  a character string denoting the type of distribution.
    #       This may be either 
    #       "norm" for the normal distribution, or
    #       "cauchy" for the Cauchy distribution, or
    #       "t" for the Student-t distribution, or
    #       "logistic" for the logistic distribution, or
    #       "laplace" for the distribution, or
    #       "kotz" for the original Kotz distribution, or 
    #       "epower" for the exponential power distribution
    
    # Value:
    #   Returns a numeric vector "g(x)" for the generator computed at
    #   the x values taken from the input vector. If x is missing,
    #   the normalizing constant "lambda" will be returned.
    
    # FUNCTION:
    
    # Handle Missing x:
    if (missing(x)) {
        x = NA
        output = "lambda"
    } else {
        output = "g"
    }
    
    # Get Type:
    type = type[1]
    
    # Get Parameters:   
    # if (is.null(param)) param = .ellipticalParam$param
    
    # Create Generator:
    if (type == "norm") {
        g = exp(-x/2)
        lambda = 1 / (2*pi)
        param = NULL
    }
    if (type == "cauchy") {
        g = ( 1 + x )^ (-3/2 )
        lambda = 1 / (2*pi)
        param = NULL
    }
    if (type == "t") {
        if (is.null(param)) {
            nu = 4
        } else {
            nu = param[[1]]
        }
        g = ( 1 + x/nu )^ ( -(nu+2)/2 )
        lambda = 1/(2*pi)
        param = c(nu = nu)
    }
    if (type == "logistic"){
        g = exp(-x/2)/(1+exp(-x/2))^2
        # lambda:
        # integrate(function(x) { exp(-x)/(1+exp(-x))^2}, 0, Inf, 
        #   subdivision = 10000, rel.tol = .Machine$double.eps^0.8)
        # 0.5 with absolute error < 2.0e-13
        lambda = 1 / pi 
        param = NULL
    }
    if (type == "laplace") { # or "double exponential"
        # epower:
        r = sqrt(2)
        s = 1/2
        g = exp(-r*(x/2)^s)
        lambda = s * r^(1/s) / ( 2 * pi * gamma(1/s) )
        param = NULL
    }
    if (type == "kotz") {
        # epower: s = 1
        if (is.null(param)) {
            r = sqrt(2)
        } else {
            r = param
        }
        g = exp(-r*(x/2))
        lambda = r/(2*pi)
        param = c(r = r)
    }
    if (type == "epower") {
        if (is.null(param)) {
            r = sqrt(2) 
            s = 1/2
        } else {
            r = param[[1]]
            s = param[[2]]
        }
        g = exp(-r*(x/2)^s)
        lambda = s * r^(1/s) / ( 2 * pi * gamma(1/s) )
        param = c(r = r, s = s)
    }
    
    # Output:
    output = output[1]
    if (output == "g") {
        ans = g
    } else if (output == "lambda") {
        ans = lambda
    }
    
    # Add Control:
    if (output == "g") {
        attr(ans, "control") = c(type = type, lambda = as.character(lambda))
    } else if (output == "lambda") {
        if (is.null(param)) {
            attr(ans, "control") = unlist(list(type = type))
        } else {
            attr(ans, "control") = unlist(list(type = type, param = param))
        }
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.delliptical2dSlider =
function(B = 10, eps = 1.e-3)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Displays interactively perspective plots of density
    
    #FUNCTION:
    
    # Graphic Frame:
    par(mfrow = c(1, 1), cex = 0.7)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        Distribution = .sliderMenu(no = 1)
        N = .sliderMenu(no = 2)
        rho = .sliderMenu(no = 3)
        nu = .sliderMenu(no = 4)
        r = .sliderMenu(no = 5)
        s = .sliderMenu(no = 6)
        nlev = .sliderMenu(no = 7)
        ncol = .sliderMenu(no = 8)
        if (rho == +1) rho = rho - eps
        if (rho == -1) rho = rho + eps
        
        # Title:
        Names = c("- Normal", "- Cauchy", "- Student t", "- Logistic",
            "- Laplace", "- Kotz", "- Exponential Power")      
        Title = paste("Elliptical Density No:", as.character(Distribution), 
            Names[Distribution], "\nrho = ", as.character(rho)) 
        if (Distribution == 3) Title = paste(Title, "nu =", as.character(nu))
        if (Distribution >= 6) Title = paste(Title, "r =", as.character(r))
        if (Distribution >= 7) Title = paste(Title, "s =", as.character(s))
        
        # Plot: 
        xy= grid2d(x = seq(-5, 5, length = N))
        Type = c("norm", "cauchy", "t", "logistic", "laplace", "kotz", "epower")
        param = NULL
        if (Distribution == 3) param = nu
        if (Distribution == 6) param = r
        if (Distribution == 7) param = c(r, s)
        D = delliptical2d(x = xy, rho = rho, param = param, 
            type = Type[Distribution], output = "list")
        image(D, col = heat.colors(ncol), xlab = "x", ylab = "y" )
        contour(D, nlevels = nlev, add = TRUE)
        title(main = Title)
        
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    plot.names = c("Plot - levels", "... colors")
    .sliderMenu(refresh.code,
        names = c("Distribution", "N", "rho", "t: nu", "r", "s", plot.names),
        minima      = c(       1,  10,    -1,       1,   0,   0,   10,   12),
        maxima      = c(       7, 100,    +1,       B,   B,   B,  100,  256),
        resolutions = c(       1,  10,   0.1,     0.1, 0.1, 0.1,   10,    1),
        starts      = c(       1,  10,     0,       4,   1,   1,   10,   12)) 
}


# ------------------------------------------------------------------------------


.delliptical2d.RUnit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   RUnit test for elliptical distributions
    
    # FUNCTION:
    
    # Settings:
    xy = grid2d((-50:50)/10)
    
    # Contour Plots:
    par(mfrow = c(3, 2), cex = 0.7)
    contour(delliptical2d(xy, rho = 0.75, param = NULL, 
        type = "norm", output = "list"))
    contour(delliptical2d(xy, rho = 0.75, param = NULL, 
        type = "cauchy", output = "list"))
    contour(delliptical2d(xy, rho = 0.75, param = 4, 
        type = "t", output = "list"))
    contour(delliptical2d(xy, rho = 0.75, param = NULL, 
        type = "laplace", output = "list"))
    contour(delliptical2d(xy, rho = 0.75, param = NULL, 
        type = "kotz", output = "list"))
    contour(delliptical2d(xy, rho = 0.75, param = NULL, 
        type = "epower", output = "list"))
        
    # Return Value:
    invisible()
}


################################################################################

