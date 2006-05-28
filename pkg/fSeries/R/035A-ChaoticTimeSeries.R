# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA. 

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
# FUNCTION:             CHAOTIC TIME SERIES MAPS:
#  tentSim               Simulates series from Tent map
#  henonSim              Simulates series from Henon map 
#  ikedaSim              Simulates series from Ikeda map
#  logisticSim           Simulates series from Logistic map
#  lorentzSim            Simulates series from Lorentz map
#  roesslerSim           Simulates series from Roessler map
#  .rk4                  Internal Funtion - Runge-Kutta Solver
################################################################################

################################################################################
# FUNCTION:             PHASE SPACE REPRESENTATION:
#  .embeddPSR
#  .checkEmbParams
#  mutualPlot            Creates mutual information plot
#  .mutual.RUnit
#  fnnPlot               Creates false nearest neigbours plot
#  .fnn.RUnit
# FUNCTION:             NON STATIONARITY:
#  recurrencePlot        Creates recurrence plot
#  .recurrence.RUnit
#  separationPlot        Creates space-time separation plot
#  .separation.RUnit
# FUNCTION:             LYAPUNOV EXPONENTS:
#  lyapunovPlot          Maximum Lyapunov plot    
#  .find.nearest
#  .follow.points
#  .lyapunovFit            
#  .lyapunov.RUnit
# FUNCTION:             DIMENSIONS AND ENTROPY:
#
################################################################################

################################################################################
# FUNCTION:             TIME SERIES TESTS:
#                        -> B1-TimeSeriesTests
################################################################################


################################################################################
# CHAOTIC TIME SERIES MAPS:


tentSim = 
function(n = 1000, n.skip = 100, parms = c(a = 2), start = runif(1), 
doplot = FALSE)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Simulate Data from Tent Map
    
    # Arguments:
    #   n - number of points x, y
    #   n.skip - number of transients discarded
    #   start - initial x
    
    # Details:
    #   Creates iterates of the Tent map:
    #   *   x(n+1)  =  a * x(n)         if x(n) <  0.5
    #   *   x(n+1)  =  a * ( 1 - x(n))  if x(n) >= 0.5 

    # FUNCTION:
    
    # Simulate Map: 
    a = parms[1]
    if (a == 2) a = a - .Machine$double.eps
    x = rep(0, times = (n+n.skip))
    i = 1
    x[i] = start 
    for ( i in 2:(n+n.skip) ) {
        x[i] = (a/2) * ( 1 - 2*abs(x[i-1]-0.5) )
    }
    x = x[(n.skip+1):(n.skip+n)] 

    # Plot Map:
    if (doplot) {
        # Time Series Plot:
        # plot(x = x, type = "l", xlab = "n", ylab = "x[n]", 
        #   main = paste("Tent Map \n a =", as.character(a)),
        #   col = "steelblue")
        # abline(h = 0.5, col = "grey", lty = 3)
        # Delay Plot:
        plot(x[-n], x[-1], xlab = "x[n]", ylab = "x[n+1]",
            main = paste("Tent Map\n a =", as.character(a)), 
            cex = 0.25, col = "steelblue")
    }
    
    # Return Value:
    ts(x)   
}


# ------------------------------------------------------------------------------


henonSim = 
function(n = 1000, n.skip = 100, parms = c(a = 1.4, b = 0.3), 
start = runif(2), doplot = FALSE)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Simulate Data from Henon Map
    
    # Arguments:
    #   n - number of points x, y
    #   n.skip - number of transients discarded
    #   a - parameter a
    #   b - parameter b
    #   start[1] - initial x
    #   start[2] - initial y
    
    # Details:
    #   Creates iterates of the Henon map:
    #   *   x(n+1)  =  1 - a*x(n)^2 + b*y(n)
    #   *   y(n+1)  =  x(n)
    
    # FUNCTION:
    
    # Simulate Map: 
    a = parms[1]
    b = parms[2]
    x = rep(0, times = (n+n.skip))
    y = rep(0, times = (n+n.skip))
    x[1] = start[1]
    y[1] = start[2]
    for ( i in 2:(n+n.skip) ) {
        x[i]  =  1 - a*x[i-1]^2 + b*y[i-1]
        y[i]  =  x[i-1] }
    x = x[(n.skip+1):(n.skip+n)] 
    y = y[(n.skip+1):(n.skip+n)] 

    # Plot Map:
    if (doplot) {
        # Time Series Plot:
        # ...
        # Delay Plot:
        plot(x = x, y = y, type = "n", xlab = "x[n]", ylab = "y[n]", 
            main = paste("Henon Map \n a =", as.character(a),
                " b =", as.character(b)) )
        points(x = x, y = y, col = "steelblue", cex = 0.25) 
    }
    
    # Return Value:
    ts(cbind(x, y))    
}


# ------------------------------------------------------------------------------


ikedaSim = 
function(n = 1000, n.skip = 100, parms = c(a = 0.4, b = 6.0, c = 0.9), 
start = runif(2), doplot = FALSE)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Simulate Ikeda Map Data
    
    # Arguments:
    #   n - number of points z
    #   n.skip - number of transients discarded
    #   a - parameter a
    #   b - parameter b; 6.0 
    #   c - parameter c; 0.9 
    #   start[1] - initial Re(z)
    #   start[2] - initial Im(z)
    
    # Details:
    #   Prints iterates of the Ikeda map (Re(z) and Im(z)): 
    #                                        i*b
    #   z(n+1)  =  1 + c*z(n)* exp( i*a - ------------ )
    #                                     1 + |z(n)|^2

    # FUNCTION:
    
    # Simulate Map: 
    A = a = parms[1]
    B = b = parms[2]
    C = c = parms[3]
    a = complex(real = 0, imag = a)
    b = complex(real = 0, imag = b)
    z = rep(complex(real = start[1], imag = start[2]), times = (n+n.skip))
    for ( i in 2:(n+n.skip) ) {
        z[i] = 1 + c*z[i-1] * exp(a-b/(1+abs(z[i-1])^2)) }
    z = z[(n.skip+1):(n.skip+n)] 
    
    # Plot Map:
    if (doplot) {
        x = Re(z)
        y = Im(z)
        plot(x, y, type = "n", xlab = "x[n]", ylab = "y[n]", 
            main = paste("Ikeda Map \n", "a =", as.character(A),
            " b =", as.character(B), " c =", as.character(C)) )
        points(x, y, col = "steelblue", cex = 0.25)
        x = Re(z)[1:(length(z)-1)]
        y = Re(z)[2:length(z)]
        plot(x, y, type = "n", xlab = "x[n]", ylab = "x[n+1]", 
            main = paste("Ikeda Map \n", "a =", as.character(A),
            " b =", as.character(B), " c =", as.character(C)) )
        points(x, y, col = "steelblue", cex = 0.25) }
    
    # Return Value:
    ts(cbind(Re = Re(z), Im = Im(z)))  
}
 

# ------------------------------------------------------------------------------


logisticSim = 
function(n = 1000, n.skip = 100, parms = c(r = 4), start = runif(1), 
doplot = FALSE)
{   # A function written by Diethelm Wuertz
    
    # Description:
    #   Simulate Data from Logistic Map

    # Arguments:
    #   n - number of points x, y
    #   n.skip - number of transients discarded
    #   r - parameter r
    #   start - initial x
    
    # Details:
    #   Creates iterates of the Logistic Map:
    #   *   x(n+1)  =  r * x[n] * ( 1 - x[n] )
    
    # FUNCTION:
    
    # Simulate Map: 
    r = parms[1]
    x = rep(0, times = (n+n.skip))
    x[1] = start
    for ( i in 2:(n+n.skip) ) {
        x[i]  =  r * x[i-1] * ( 1 - x[i-1] ) }
    x = x[(n.skip+1):(n.skip+n)] 

    # Plot Map:
    if (doplot) {
        plot(x = x[1:(n-1)], y = x[2:n], type = "n", xlab = "x[n-1]", 
            ylab = "x[n]", main = paste("Logistic Map \n r =",
            as.character(r)) )
        points(x = x[1:(n-1)], y = x[2:n], col = "steelblue", cex = 0.25) }
    
    # Return Value:
    ts(x)   
}
    
                       
# ------------------------------------------------------------------------------


lorentzSim = 
function(times = seq(0, 40, by = 0.01), parms = c(sigma = 16, r = 45.92, b = 4),
start = c(-14, -13, 47), doplot = TRUE, ...)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Simulates a Lorentz Map
    
    # Notes:
    #   Requires rk4 from R package "odesolve"
   
    # FUNCTION:
    
    # Requirements:
    # BUILTIN - require(odesolve)
    
    # Settings:
    sigma = parms[1]
    r = parms[2]
    b = parms[3]
    
    # Attractor:
    lorentz = 
    function(t, x, parms) {
        X = x[1]
        Y = x[2]
        Z = x[3] 
        with(as.list(parms), {
            dX = sigma * ( Y - X )
            dY = -X*Z + r*X - Y
            dZ = X*Y - b*Z
            list(c(dX, dY, dZ))}) 
    }

    # Classical RK4 with fixed time step:
    s = .rk4(start, times, lorentz, parms)

    # Display:
    if (doplot) {
        xylab = c("x", "y", "z", "x")
        for (i in 2:4) 
            plot(s[, 1], s[, i], type = "l", 
                xlab = "t", ylab = xylab[i-1], col = "steelblue", 
                main = paste("Lorentz \n", "sigma =", as.character(sigma), 
                " r =", as.character(r), " b =", as.character(b)), ...)
        k = c(3, 4, 2)
        for (i in 2:4) plot(s[, i], s[, k[i-1]], type = "l", 
            xlab = xylab[i-1], ylab = xylab[i], col = "steelblue",
            main = paste("Lorentz \n", "sigma =", as.character(sigma), 
            " r =", as.character(r), " b =", as.character(b)), ...)
    }
    
    # Result:
    colnames(s) = c("t", "x", "y", "z")
        
    # Return Value:
    ts(s)
}


# ------------------------------------------------------------------------------


roesslerSim = 
function(times = seq(0, 100, by = 0.1), parms = c(a = 0.2, b = 0.2, c = 8.0),
start = c(-1.894, -9.920, 0.0250), doplot = TRUE, ...)
{   # A function written by Diethelm Wuertz
    
    # Description:
    #   Simulates a Lorentz Map
    
    # Notes:
    #   Requires contributed R package "odesolve"
   
    # FUNCTION:
    
    # Settings:
    a = parms[1]
    b = parms[2]
    c = parms[3]
    
    # Attractor:
    roessler = function(t, x, parms) {
        X = x[1]; Y = x[2]; Z = x[3] 
        with(as.list(parms), {
            dX = -(Y+Z)
            dY = X + a*Y
            dZ = b + X*Z -c*Z
            list(c(dX, dY, dZ))}) }

    # Classical RK4 with fixed time step:
    s = .rk4(start, times, roessler, parms)

    # Display:
    if (doplot) {
        xylab = c("x", "y", "z", "x")
        for (i in 2:4) plot(s[, 1], s[, i], type = "l", 
            xlab = "t", ylab = xylab[i-1], col = "steelblue",
            main = paste("Roessler \n", "a = ", as.character(a),
                " b = ", as.character(b), " c = ", as.character(c)), ...)
        k = c(3, 4, 2)
        for (i in 2:4) plot(s[, i], s[, k[i-1]], type = "l", 
            xlab = xylab[i-1], ylab = xylab[i], col = "steelblue",
            main = paste("Roessler \n", "a = ", as.character(a),
                " b = ", as.character(b), " c = ", as.character(c)), ...)
    }
    
    # Result:
    colnames(s) = c("t", "x", "y", "z")
        
    # Return Value:
    ts(s)
}


# ------------------------------------------------------------------------------


.rk4 =  
function(y, times, func, parms) 
{
    # Description:
    #   Classical Runge-Kutta-fixed-step-integration
    
    # Autrhor:
    #   R-Implementation by Th. Petzoldt,
    
    # Notes:
    #   From Package: odesolve
    #   Version: 0.5-12
    #   Date: 2004/10/25
    #   Title: Solvers for Ordinary Differential Equations
    #   Author: R. Woodrow Setzer <setzer.woodrow@epa.gov>
    #   Maintainer: R. Woodrow Setzer <setzer.woodrow@epa.gov>
    #   Depends: R (>= 1.4.0)   
    #   License: GPL version 2 
    #   Packaged: Mon Oct 25 14:59:00 2004
    
    # FUNCTION:

    # Checks:
    if (!is.numeric(y)) stop("`y' must be numeric")
    if (!is.numeric(times)) stop("`times' must be numeric")
    if (!is.function(func)) stop("`func' must be a function")
    if (!is.numeric(parms)) stop("`parms' must be numeric")

    # Dimension:
    n = length(y)

    # Call func once to figure out whether and how many "global"
    # results it wants to return and some other safety checks
    rho = environment(func)
    tmp = eval(func(times[1], y,parms), rho)
    if (!is.list(tmp)) stop("Model function must return a list\n")
    if (length(tmp[[1]]) != length(y))
        stop(paste("The number of derivatives returned by func() (",
            length(tmp[[1]]),
             "must equal the length of the initial conditions vector (",
             length(y),")", sep = ""))
    Nglobal = if (length(tmp) > 1) length(tmp[[2]]) else 0
     
    y0 = y
    out = c(times[1], y0)
    for (i in 1:(length(times)-1)) {
        t  = times[i]
        dt = times[i+1] - times[i]
        F1 = dt * func(t,      y0,            parms)[[1]]
        F2 = dt * func(t+dt/2, y0 + 0.5 * F1, parms)[[1]]
        F3 = dt * func(t+dt/2, y0 + 0.5 * F2, parms)[[1]]
        F4 = dt * func(t+dt  , y0 + F3,       parms)[[1]]
        dy = (F1 + 2 * F2 + 2 * F3 + F4)/6
        y1 = y0 + dy
        out<- rbind(out, c(times[i+1], y1))
        y0 = y1
    }

    nm = c("time", 
        if (!is.null(attr(y, "names"))) {
            names(y) 
        } else {
            as.character(1:n))
        }
    if (Nglobal > 0) {
        out2 = matrix(nrow=nrow(out), ncol = Nglobal)
        for (i in 1:nrow(out2)) {
            out2[i,] = func(out[i,1], out[i,-1], parms)[[2]]
        }
        out = cbind(out, out2)
        nm = c(nm,
            if (!is.null(attr(tmp[[2]],"names"))) {
                names(tmp[[2]])
            } else {
                as.character((n+1) : (n + Nglobal)))
            }
    }
    dimnames(out) = list(NULL, nm)
    
    # Return Value:
    out
}


################################################################################
# PART II: CHAOTIC TIME SERIES ANALYSIS
# Package: tseriesChaos
# Title: Analysis of nonlinear time series
# Date: 2005-07-24
# Version: 0.1
# Author: Antonio, Fabio Di Narzo
# Description: Routines for the analysis of nonlinear time series. 
#   This work is largely inspired by the TISEAN project, by Rainer 
#   Hegger, Holger Kantz and Thomas Schreiber: 
#   http://www.mpipks-dresden.mpg.de/~tisean/
# Maintainer: Antonio, Fabio Di Narzo <antonio.dinarzo@studio.unibo.it>
# License: GPL version 2 or newer
# Packaged: Sun Jul 24 10:58:36 2005; antonio
# CONTENT:
#   1. PHASE SPACE REPRESENTATION
#   2. NON STATIONARITY
#   3. LYAPUNOV EXPONENTS
#   4. DIMENSIONS AND ENTROPY
################################################################################


# ******************************************************************************
# 1. PHASE SPACE REPRESENTATION:


.embeddPSR = 
function(x, m, d) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Embedding of a time series with provided time delay and 
    #   embedding dimension parameters.
    
    # Arguments
    #   x - time series
    #   m - embedding dimension
    #   d - time delay
    
    # Value:
    #   Matrix with columns corresponding to lagged time series.

    # Author:
    #   Antonio, Fabio Di Narzo 
    #   of the original function from the 'tseriesChaos' package
    
    # FUNCTION:
    
    .checkEmbParms(x, m, d)
    n = length(x) - (m-1)*d
    res = matrix(0, n, m)
    for(i in 1:m) res[,i] = x[((i-1)*d+1):(n+(i-1)*d)]
    
    # Return Value:
    res
}


# ------------------------------------------------------------------------------


.checkEmbParms =
function(series, m, d, t = 0, s = 1, ref = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Check embedding parameters
    
    # Author:
    #   Antonio, Fabio Di Narzo 
    #   of the original function from the 'tseriesChaos' package
    
    # FUNCTION:
    
    n = length(series)-(m-1)*d
    if (n <= 0) 
        stop("Not enough points to handle these parameters")
    if (!is.null(ref)) if (ref > n) 
        stop("Not enough points to handle these parameters")
    if (t < 0) 
        stop("Theiler window t must be non-negative")
    if (s <= 0) 
        stop("Number of steps must be positive")
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


mutualPlot = 
function(x, partitions = 16, lag.max = 20, doplot = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Estimate the mutual information index of a given time 
    #   series for a specified number of lags   
    
    # Arguments:
    #   x - a numeric vector, or an object either of class 'ts' or
    #       of class 'timeSeries'.
    #   partitions - an integer value setting the number of bins, by
    #       default 16.
    #   lag.max - an integer value setting the number of
    #       maximum lags, by default 20/
    #   doplot - a logical flag. Should a plot be displayed?

    # Author:
    #   Antonio, Fabio Di Narzo 
    #   of the original function from the 'tseriesChaos' package
    
    # FUNCTION:
    
    # Settings:
    if (class(x) == "timeSeries") x = as.vector(x)
    x = as.ts(x)
    series = (x-min(x))/(diff(range(x)))
    corr = numeric(lag.max+1)
    
    # Mutual Information:
    for(i in 0:lag.max) {
        hist = matrix(0, partitions, partitions)
        hist = .C("mutual", 
            series = as.double(series), 
            length = as.integer(length(series)), 
            lag = as.integer(i), 
            partitions = as.integer(partitions), 
            hist = as.double(hist), 
            PACKAGE = "fSeries")[["hist"]]
        hist = matrix(hist, partitions, partitions)/sum(hist)
        histx = apply(hist, 1, sum)
        hist = hist[hist != 0]
        histx<- histx[histx != 0]
        corr[i+1] = sum(hist*log(hist)) - 2*sum(histx*log(histx))
    }
    names(corr) = paste(0:lag.max)
    
    # Plot:
    if (doplot) {
        plot(0:lag.max, corr, xlab = "Lag", type = "b", pch = 19, cex = 0.25,
            col = "steelblue", main = "Mutual Information", ...) 
    }

    # Return Value:
    corr
}


# ------------------------------------------------------------------------------


.mutual.RUnit =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   RUnit test function
    
    # FUNCTION:
    
    plot(0:20, mutual(lorenz.ts), type = "b", xlab = "lag", ylab = "I", 
        main = "Mutual information Index", 
        sub = "Lorenz simulated time series")
}


# ------------------------------------------------------------------------------


falsennPlot = 
function(x, m, d, t, rt = 10, eps = NULL, doplot = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Use the Method of false nearest neighbours to help deciding 
    #   the optimal embedding dimension 
    
    # Arguments:
    #   x - time series
    #   m - maximum embedding dimension
    #   d - delay parameter
    #   t - Theiler window
    #   rt - escape factor
    #   eps - neighborhood diameter

    # Value:
    #   Fraction of false neighbors (first row) and total number of 
    #   neighbors (second row) for each specified embedding dimension 
    #   (columns)

    # Author:
    #   Antonio, Fabio Di Narzo 
    #   of the original function from the 'tseriesChaos' package
    
    # FUNCTION:
    
    # Settings:
    if (class(x) == "timeSeries") x = as.vector(x)
    series = as.ts(x)
    if (is.null(eps)) eps = sd(series)/10
    res = numeric(m)
    res2 = numeric(m)
    
    # False Nearest Neigbours:
    for(i in 1:m) {
        a = .C("falseNearest", 
            series = as.double(series), 
            length = as.integer(length(series)), 
            m = as.integer(i), 
            d = as.integer(d), 
            t = as.integer(t), 
            eps = as.double(eps), 
            rt = as.double(rt), 
            out = as.double(res[i]), 
            out2 = as.integer(res2[i]), 
            PACKAGE = "fSeries")
        res[i] = a[["out"]]
        res2[i]= a[["out2"]]
    }
    res = rbind(res, res2)
    rownames(res) = c("fraction", "total")
    colnames(res) = paste("m", 1:m, sep = "")
    
    # Plot:
    if (doplot) {
        plot(res[1, ], type = "b", col = "steelblue", pch = 19,
            cex = 0.25, xlab = "Dimension", ylab = "Fraction of ffn",
            main = "False Nearest Neigbours", ...)
    }
    
    # Return Value:
    res
}


# ------------------------------------------------------------------------------


.fnn.RUnit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   RUnit test function
    
    fnnPlot(x = rossler.ts, m = 6, d = 8, t = 180, eps = 1, rt = 3)
}


# ******************************************************************************
# NON STATIONARITY:


recurrencePlot = 
function(x, m, d, end.time, eps, nt = 10, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a recurrence plot
    
    # Arguments
    #   x - time series
    #   m - embedding dimension
    #   d - time delay
    #   end.time - ending time (as no. of observations)
    #   eps - neighbourhood threshold
    #   nt - observations in each step
    #   ... - further parameters to be passed to plot
    
    # Value:
    #   Produces the recurrence plot, as proposed by Eckmann et al. (1987). 
    #   To reduce the number of points plotted (especially with highly 
    #   sampled data), each nt observations, one single point is plotted.

    # FUNCTION:
    
    # Settings:
    if (class(x) == "timeSeries") x = as.vector(x)
    series = as.ts(x)
    w = (0:(m-1))*d
    .dist = function(i, j) { sum((series[i+w]-series[j+w])^2) }

    .checkEmbParms(series, m, d)
    if (eps <= 0) stop("eps must be positive")
    nt = as.integer(nt)
    if (nt<=0) nt = 1
    n = length(series)-(m-1)*d
    if(end.time > n) end.time = n
    eps = eps^2
    xyz = .embeddPSR(series, m = m, d = d)[1:end.time, ]
    
    # Plot:
    if (TRUE) {
        plot(0, xlim = c(0, end.time), ylim = c(0, end.time), type = "n", 
            main = "Recurrence Plot", xlab = "i", ylab = "j")
        for(i in seq(1, end.time, by = nt)) 
            for(j in seq(i,end.time, by = nt))
                if(.dist(i,j) < eps) points(c(i, j), c(j, i), ...)
    }
    
    # Return Value:
    invisible()         
}


# ------------------------------------------------------------------------------


.recurrence.RUnit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   RUnit test function
    
    # FUNCTION:
    
    recurrencePlot(lorenz.ts, m = 3, d = 2, end.time = 800, eps = 3, 
        nt = 5, pch = '.', cex = 2)
}


# ------------------------------------------------------------------------------


separationPlot = 
function(x, m, d, mdt, idt = 1, doplot = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a space-time separation plot 

    # Arguments:
    #   x - time series
    #   m - embedding dimension
    #   d - time delay
    #   idt - observation steps in each iteration
    #   mdt - number of iterations
    
    # Value:
    #   Returns lines of costant probability at 10%, 20%, ..., 100%.
    
    # Author:
    #   Antonio, Fabio Di Narzo 
    #   of the original function from the 'tseriesChaos' package
    
    # FUNCTION:
    
    # Settings:
    if (class(x) == "timeSeries") x = as.vector(x)
    series = as.ts(x)
    .checkEmbParms(series, m, d)
    
    # Space Time Separations:
    eps.max = diff(range(series))*sqrt(m)
    res = matrix(0, 10, mdt)
    res = .C("stplot", 
        series = as.double(series), 
        length = as.integer(length(series)), 
        m = as.integer(m), 
        d = as.integer(d), 
        mdt = as.integer(mdt), 
        idt = as.integer(idt),
        eps.max = as.double(eps.max), 
        res = as.double(res), 
        PACKAGE = "fSeries")[["res"]]
    stp = matrix(res, 10, mdt)
    eps.m = min(stp)
    eps.M = max(stp)
    
    # Plot:
    if (doplot) {
        plot(0, xlim = c(0, mdt*idt/frequency(series)), 
            ylim = c(eps.m*0.99, eps.M*1.01), 
            xlab = "Time", ylab = "Distance", type = "n", 
            main = "Space-time Separation Plot")
        x = seq(1/frequency(series), mdt*idt/frequency(series), 
            by = idt/frequency(series))
        for(i in 1:10) lines(x, stp[i, ], col = "steelblue")
    }
    
    # Return Value:
    invisible(stp)
}


# ------------------------------------------------------------------------------


.separation.RUnit =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   RUnit test function
    
    # FUNCTION:
    
    # Return Value:
    separationPlot(rossler.ts, m = 3, d = 8, idt = 1, mdt = 250)    
}


# ******************************************************************************
# LYAPUNOV EXPONENTS


lyapunovPlot = 
function(x, m, d, t, ref, s, eps, k = 1, doplot = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Evaluate the maximal Lyapunov exponent of a dynamic system 
    #   from an univariate time series  
    
    # Arguments
    #   x - time series
    #   m - embedding dimension
    #   d - time delay
    #   k - number of considered neighbours
    #   eps - radius where to find nearest neighbours
    #   s - iterations along which follow the neighbours of each point
    #   ref - number of points to take into account
    #   t - Theiler window
    
    # Value:
    #   Returns the logarithm of the stretching factor in time. 
    
    # Author:
    #   Antonio, Fabio Di Narzo 
    #   of the original function from the 'tseriesChaos' package
    
    # FUNCTION:
    
    # Settings:
    if (class(x) == "timeSeries") x = as.vector(x)
    series = as.ts(x)
    .checkEmbParms(series, m, d, t, s, ref)
    n = length(series) - (m-1)*d - s 
    if(ref < 0) ref = n
    trash = numeric()
    ref = 1:ref
    
    # Finding Nearest Neighbours:
    cat("Finding nearests\n")
    nearest = .find.nearest(series, m = m, d = d, t = t, ref = length(ref), 
        s = s, eps = eps, k = k)
    trash = apply(nearest, 1, function(x) any(is.na(x)))
    ref = ref[!trash]
    if(length(ref) == 0) 
        stop("not enough neighbours found")
    cat("Keeping ", length(ref)," reference points\n")
    
    # Following Points:
    cat("Following points\n")
    res = .follow.points(series, m = m, d = d, s = s, ref = ref, 
        nearest = nearest, k = k)
    ans = ts(res, freq = frequency(series), start = 0)
    
    # Plot:
    if (doplot) {
        plot(ans, col = "steelblue", main = "Max Lyapunov Exponents", ...)
    }
    
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.find.nearest = 
function(series, m, d, t, eps, ref, k, s)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function called by 'lyapunovPlot'  
    
    # Author:
    #   Antonio, Fabio Di Narzo 
    #   of the original function from the 'tseriesChaos' package
    
    # FUNCTION:
    
    res = numeric(ref*k)
    res = .C("find_nearest", 
        series = as.double(series), 
        m = as.integer(m), 
        d = as.integer(d), 
        t = as.integer(t), 
        length = as.integer(length(series)), 
        eps = as.double(eps),
        ref = as.integer(ref), 
        k = as.integer(k), 
        s = as.integer(s), 
        res = as.integer(res), 
        PACKAGE = "fSeries")[["res"]]
    res[res == -1] = NA
    
    matrix(res, ref, k)
}


# ------------------------------------------------------------------------------


.follow.points = 
function(series, m, d, ref, k, s, nearest) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function called by 'lyapunovPlot'
    
    # Author:
    #   Antonio, Fabio Di Narzo 
    #   of the original function from the 'tseriesChaos' package
    
    # FUNCTION:
    
    res = numeric(s)
    nearest[is.na(nearest)] = -1
    ans = .C("follow_points",
        series = as.double(series), 
        m = as.integer(m), 
        d = as.integer(d), 
        length = as.integer(length(series)), 
        nref = as.integer(length(ref)), 
        nrow = as.integer(nrow(nearest)), 
        k = as.integer(k), 
        s = as.integer(s), 
        nearest = as.integer(nearest), 
        ref = as.integer(ref), 
        res = as.double(res),
        PACKAGE = "fSeries")[["res"]]
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.lyapunovFit = 
function(x, start, end) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Lyapunov Fit
    
    # Arguments:
    #   x - Should be the output of a call to lyap_k (see the example)
    #   start - Starting time of the linear bite of dsts
    #   end - Ending time of the linear bite of dsts

    # Value:
    #   Returns the regression coefficients of the specified input sequence.
    # Author:
    #   Antonio, Fabio Di Narzo 
    #   of the original function from the 'tseriesChaos' package
    
    # FUNCTION:
    
    # Settings:
    dsts = as.ts(x)
    sf = window(dsts, start, end)
    start = start(sf)[1] + (start(sf)[2]-1)/frequency(sf)
    end = end(sf)[1] + (end(sf)[2]-1)/frequency(sf)
    lambda = seq(start, end, by = 1/frequency(dsts))
    
    # Fit:
    ans = lm(sf ~ lambda, data = data.frame(sf = sf, lambda = lambda))$coeff
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.lyapunov.RUnit =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   R Unit test function    
    
    output = lyapunovPlot(lorenz.ts, m = 3, d = 2, s = 200, t = 40, 
        ref = 1700, k = 2, eps = 4)
    lyapunovFit(output, start = 0.73, end = 2.47)
}


# ******************************************************************************
# DIMENSIONS AND ENTROPY:


.C2 = 
function(x, m, d, t, eps)
{
    # Settings:
    if (class(x) == "timeSeries") x = as.vector(x)
    series = as.ts(x)
    .checkEmbParms(series, m, d, t)
    if (eps <= 0) stop("eps must be positive")
    res = numeric(1)
    
    # C2:
    ans = .C("C2", 
        series = as.double(series), 
        m = as.integer(m), 
        d = as.integer(d),
        length = as.integer(length(series)), 
        t = as.integer(t), 
        eps = as.double(eps), 
        res = as.double(res), 
        PACKAGE = "fSeries")[["res"]]
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.d2 = 
function(series, m, d, t, eps.min, neps = 100) 
{
    # Settings:
    if (class(x) == "timeSeries") x = as.vector(x)
    series = as.ts(x)
    .checkEmbParms(series, m, d, t)
    if (eps.min <= 0) stop("eps.min must be positive")
    neps = as.integer(neps)
    if (neps <= 0) neps = 100
    res = numeric(neps*m)
    eps.max = diff(range(series))*sqrt(m)
    
    # d2:
    res = .C("d2", 
        series = as.double(series), 
        length = as.integer(length(series)), 
        m = as.integer(m), d = as.integer(d), 
        t = as.integer(t), neps = as.integer(neps), 
        eps.max = as.double(eps.max), 
        eps.min = as.double(eps.min), 
        res = as.double(res), 
        PACKAGE = "fSeries")[["res"]]
    res = matrix(res, neps, m)
    res = res[neps:1,]
    denom = length(series) - (m-1)*d
    denom = (denom-t+1)*(denom-t)/2
    res = apply(res, 2, cumsum)/denom
    a = -log(eps.min/eps.max)/(neps-1)
    eps = eps.max*exp((1-1:neps)*a)
    eps = eps[neps:1]
    res = cbind(eps, res)
    colnames(res) = c("eps",paste("m", 1:m, sep = ""))
    plot(res[ , c(1,m+1)], type = "l", log = "xy", 
        main = "Sample correlation integral", 
        xlab = expression(epsilon), ylab = expression(C(epsilon)))
    for (i in m:2) lines(res[,c(1, i)])
    
    # Return Value:
    invisible(res)
}


################################################################################

