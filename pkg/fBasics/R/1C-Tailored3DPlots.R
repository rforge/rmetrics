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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:           DESCRIPTION:
# .akimaInterp         Interpolates and Smoothes Irregularly Distributed Points
# .krigeInterp         Interpolates and Smoothes Irregularly Distributed Points
# FUNCTION:           DESCRIPTION:
# .squareBinning       Squatre Binning of Irregularly Distributed Points
# .sqarePlot           Plots Square Binned Data Points
# FUNCTION:           DESCRIPTION:
# .hexBinning          Hexagonal Binning of Irregularly Distributed Points
# .hexPlot             Plots Hexagonal Binned Data Points
# FUNCTION:           DESCRIPTION:
# .surfacePlot         Perspective Plot of Irregularly Distributed Points
# .levelPlot           Contour Plot of Irregularly Distributed Points
# .circles2Plot        Circles Plot of Irregularly Distributed Points
################################################################################


.akimaInterp =   
function(x, y = NULL, z = NULL, nGrid = 21,
xo = seq(min(x), max(x), length = nGrid),
yo = seq(min(y), max(y), length = nGrid), extrap = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Interpolates and Smoothes Irregularly Distributed Data Points
    
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
        PACKAGE = "fBasics")
    temp = ans[c("x", "y", "z", "extrap")]
    if (miss) temp$z[temp$extrap] = NA
    ans = temp[c("x", "y", "z")]
     
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.krigeInterp = 
function(x, y = NULL, z = NULL, nGrid = 21,
xo = seq(min(x), max(x), length = nGrid),
yo = seq(min(y), max(y), length = nGrid), extrap = TRUE, polDegree = 6)
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   Requires Recommendet R Package "spatial"
    
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
    Z = prmat(krige, 
        xl = min(xo), xu = max(xo), yl = min(yo), yu = max(yo), 
        n = nGrid-1)
      
    # Extrapolate ?  
    if (!extrap) {
        E = .akimaInterp(x = x, y = y, z = z, nGrid = nGrid, extrap = extrap)
        Z$z[is.na(E$z)] = NA
    }
    
    # Return Value:
    Z
}


################################################################################


.squareBinning = 
function(x, y = NULL, bins = 30)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns 2D Histogram Counts
    
    # Arguments:
    #   x, y - two vectors of coordinates of data. If y is NULL then x 
    #       is assumed to be a two column matrix, where the first column 
    #       contains the x data, and the second column the y data. 
    #   nGrid - number of bins in each dimension, may be a scalar or a 2
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
    n = bins - 1
    if (length(n) == 1) {
        nbins = c(n, n)
    } else {
        nbins = n
    }
    xo = seq(min(x), max(x), length = bins[1])
    yo = seq(min(y), max(y), length = bins[2])

    nas = is.na(x) | is.na(y)
    index.x = cut(x, xo, include.lowest = TRUE)
    index.y = cut(y, yo, include.lowest = TRUE)
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


.squarePlot =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Return Value:
    NA 
}


################################################################################


.hexBinning = 
function(x, y = NULL, bins = 30)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #
    
    # Extract Series:
    if (is.null(y)) {
        y = x[, 2]
        x = x[, 1]
    }
    
    # Set parameters:
    shape = 1
    n = length(x)
    xbnds = range(x)
    ybnds = range(y)
    jmax = floor(bins + 1.5001)
    c1 = 2 * floor((bins *shape)/sqrt(3) + 1.5001)
    imax = trunc((jmax*c1 -1)/jmax + 1)
    lmax = jmax * imax     
    cell = cnt = xcm = ycm = rep(0, times = max(n, lmax))
    xmin = xbnds[1]
    ymin = ybnds[1]
    xr = xbnds[2] - xmin
    yr = ybnds[2] - ymin
    c1 = bins/xr
    c2 = bins*shape/(yr*sqrt(3.0))
    jinc = jmax
    lat = jinc + 1
    iinc = 2*jinc
    con1 = 0.25
    con2 = 1.0/3.0

    # Count Bins:
    for ( i in 1:n ) {
        sx = c1 * (x[i] - xmin)
        sy = c2 * (y[i] - ymin)
        j1 = floor(sx + 0.5)
        i1 = floor(sy + 0.5)
        dist1 = (sx-j1)^2 + 3.0*(sy-i1)^2
        if( dist1 < con1) {
            L = i1*iinc + j1 + 1
        } else if (dist1 > con2) {
            L = floor(sy)*iinc + floor(sx) + lat
        } else {
            j2 = floor(sx)
            i2 = floor(sy)
            test = (sx-j2 -0.5)^2 + 3.0*(sy-i2-0.5)^2
            if ( dist1 <= test ) {
                L = i1*iinc + j1 + 1
            } else {
                L = i2*iinc + j2 + lat
            }
        }
        cnt[L] = cnt[L]+1
        xcm[L] = xcm[L] + (x[i] - xcm[L])/cnt[L]
        ycm[L] = ycm[L] + (y[i] - ycm[L])/cnt[L]
    }

    # Reduce to Non-Empty Cells:
    nc = 0
    for ( L in 1:lmax ) {
        if(cnt[L] > 0) {
            nc = nc + 1
            cell[nc] = L
            cnt[nc] = cnt[L]
            xcm[nc] = xcm[L]
            ycm[nc] = ycm[L]
        }
    }
    bnd = c(imax, jmax)
    bnd[1] = (cell[nc]-1)/bnd[2] + 1
    length(cell) = nc
    length(cnt) = nc
    length(xcm) = nc
    length(ycm) = nc
    if(sum(cnt) != n) warning("Lost counts in binning")
    
    # Compute Positions:
    c3 = diff(xbnds)/bins
    ybnds = ybnds
    c4 = (diff(ybnds) * sqrt(3))/(2 * shape * bins)
    cell = cell - 1
    i = cell %/% jmax
    j = cell %% jmax
    y = c4 * i + ybnds[1]
    x = c3 * ifelse(i %% 2 == 0, j, j + 0.5) + xbnds[1]
    
    # Return Value:
    list(x = x, y = y, z = cnt, xcm = xcm, ycm = ycm, 
        bins = bins, dim = c(imax, jmax))
}


# ------------------------------------------------------------------------------


.hexPlot =
function(x, y, bins = 30, col = heat.colors(12), ...)
{
    
    # Binning:
    hbin = .hexBinning(x, y, bins)
    X = hbin$x
    Y = hbin$y
    
    # Plot Center Points:
    plot(X, Y, pch = 19, col = "steelblue", ...)
      
    # Create Hexagon Coordinates:
    rx = min(diff(unique(sort(X))))
    ry = min(diff(unique(sort(Y))))
    rt = 2*ry
    u =         c(rx,  0, -rx, -rx,   0,  rx)
    v = (1/3) * c(ry, rt,  ry, -ry, -rt, -ry)

    # Create Color Palette:
    N = length(col)
    z = hbin$z
    zMin = min(z)
    zMax = max(z)
    Z = (z - zMin)/(zMax - zMin)
    Z = trunc(Z*(N-1)+1)
    
    # Add Colored Hexagon Polygons:
    for (i in 1:length(x)) {
        polygon(u+X[i], v+Y[i], col = col[Z[i]], border = "white")
    }
    
    # Add Center of Mass Points:
    points(hbin$xcm, hbin$ycm, pch = 19, cex = 0.25, col = "black")
    
    # Return Value:
    invisible(hbin)
}



################################################################################


.surfacePlot = 
function(x, y = NULL, z = NULL, gridPoints = 21,
xo = seq(min(x), max(x), length = gridPoints),
yo = seq(min(y), max(y), length = gridPoints),
interp = c("akima", "krige"), extrap = TRUE, 
theta = -40, phi = 30, col = "steelblue", ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns a perspecvtive plot
    
    # Notes:
    #   A synonyme call for function 'persp'
  
    # FUNCTION:
    
    # Check Arguments:
    interp = match.arg(interp)
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
    
    if (nX == nZ) {
        # Interpolation:  
        if (interp == "spline") {
            Z = .akimaInterp(x, y, z, xo, yo, gridPoints, interp, extrap)
        } else if (interp == "krige") {
            Z = .krigeInterp(x, y, z, xo, yo, gridPoints, interp, extrap, 
                polDegree)
        }
    }
          
    # Perspective Plot:
    if (!exists("ticktype")) ticktype = "detailed"
    if (!exists("expand")) expand = 0.6
    if (!exists("r")) r = 500
    ans = persp(x = Z, theta = theta, phi = phi, 
        col = col, ticktype = ticktype, expand = expand, ...) 
        
    # Return Value:
    invisible(ans)
}



# ------------------------------------------------------------------------------


.levelPlot = 
function(x, y = NULL, z = NULL, gridPoints = 40, 
xo = seq(min(x), max(x), length = gridPoints),
yo = seq(min(y), max(y), length = gridPoints),
interp = c("akima", "krige"), extrap = TRUE, 
image = TRUE, points = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns a contour plot
    
    # Notes:
    #   A synonyme call for function 'contour'
 
    # FUNCTION:   
    
    # Interpolation:  

    XYZ = .akima2D(x, y, z, xo, yo, interp, extrap = TRUE)
    X = XYZ$x
    Y = XYZ$y
    Z = XYZ$z
    
    # Contour Plot:
    if (class(version) == "Sversion") {
        # we assume SPlus:
        ans = contour(x = X, y = Y, z = Z, ...) 
    } else {
        # R:
        image(x = X, y = Y, z = Z, ...) 
        ans = contour(x = X, y = Y, z = Z, add = image, ...) 
        if (points) points(x, y, pch = 19)
    }
        
    # Return Value:
    invisible(ans)
}



# ------------------------------------------------------------------------------


.circles2Plot = 
function(x, y, z, scale = 1, points = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:  
    #   Creates a scatterplot with circle size indexing a 
    #   third variable.
    
    # Example:
    #   circlesPlot(x = rnorm(50), y = rnorm(50), z = abs(rnorm(50)))
 
    # FUNCTION:
    
    # Settings:
    stopifnot(length(x) == length(y))
    stopifnot(length(y) == length(z))
    
    # Create Circle Plot:
    plot(x, y, type = "n", ...)
    symbols(x, y, add = TRUE, circles = abs(z)^scale, inches = 0.25, 
        fg = "black", bg = "steelblue",...) 
    X = x[z<0]
    Y = y[z<0]
    Z = z[z<0]
    symbols(X, Y, add = TRUE, circles = abs(Z)^scale, inches = 0.25, 
        fg = "black", bg = "orange", ...)
    if (points) points(x, y, pch = 19)
    grid()
    
    # Return Value:
    invisible(data.frame(x = x, y = y, z = z))
}
    
    
################################################################################

