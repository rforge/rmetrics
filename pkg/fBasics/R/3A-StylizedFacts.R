
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


################################################################################
# FUNCTION:             DESCRIPTION:
#  acfPlot               Displays tailored autocorrelations function plot
#  pacfPlot              Displays tailored partial autocorrelation function plot
#  ccfPlot               Displays tailored cross correlation function plot
#  teffectPlot           Estimates and displays the Taylor effect
#  lmacfPlot             Estimates and displays the long memory ACF
#  lacfPlot              Displays lagged autocorrelations
#  logpdfPlot            Displays a pdf plot on logarithmic scale(s)
#  qqgaussPlot           Displays a tailored Gaussian quantile-quantile plot
#  scalinglawPlot        Evaluates and displays scaling law behavior
################################################################################


################################################################################
# FUNCTION:             DESCRIPTION:
#  acfPlot               Displays autocorrelations function plot
#  pacfPlot              Displays partial autocorrelation function plot
#  ccfPlot               Displays cross correlation function plot
#  teffectPlot           Estimates and plots the Taylor effect
#  lmacfPlot             Estimates and plots the long memory ACF
#  lacfPlot              Plots lagged autocorrelations
#  logpdfPlot            Returns a pdf plot on logarithmic scale(s)
#  qqgaussPlot           Returns a Gaussian quantile-quantile plot
#  scalinglawPlot        Evaluates and plots scaling law behavior


acfPlot = 
function(x, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Autocorrelations function plot
  
    # FUNCTION:
    
    # Convert Type:
    if (class(x) == "timeSeries") stopifnot(isUnivariate(x))
    x = as.vector(x)
    
    # Labels:
    if (labels) {
        main = "Autocorrelation Function"
        xlab = "lag"
        ylab = "ACF"
    } else {
        main = xlab = ylab = ""
    }
    
    # ACF:
    ans = acf(x = x, main = main, xlab = xlab, ylab = ylab, ...)
    
    # Return Value:
    invisible(ans) 
}


# ------------------------------------------------------------------------------


pacfPlot = 
function(x, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Partial autocorrelation function plot
    
    # FUNCTION:
    
    # Convert Type:
    if (class(x) == "timeSeries") stopifnot(isUnivariate(x))
    x = as.vector(x)
    
    # Labels:
    if (labels) {
        main = "Partial ACF"
        xlab = "lag"
        ylab = "PACF"
    } else {
        main = xlab = ylab = ""
    }
    
    # For S-Plus compatibility:
    if (exists("pacf")) {
        ans = pacf(x = x, 
            main = main, xlab = xlab, ylab = ylab, ...)
    } else {
        ans = acf(x = x, type = "partial", 
            main = main, xlab = xlab, ylab = ylab, ...)
    }
    
    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


ccfPlot = 
function(x, y, lag.max = max(2, floor(10*log10(length(x)))), 
type = c("correlation", "covariance", "partial"), labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Cross correlation function plot
     
    # FUNCTION:
    
    # Convert Type:
    if (class(x) == "timeSeries") stopifnot(isUnivariate(x))
    if (class(y) == "timeSeries") stopifnot(isUnivariate(y))
    x = as.vector(x)
    y = as.vector(y)
    
    # Labels:
    if (labels) {
        main = "Crosscorrelation Function"
        xlab = "lag"
        ylab = "CCF"
    } else {
        main = xlab = ylab = ""
    }
    
    # Result:
    # A copy from R's ccf - So you can use it also under SPlus:
    X = cbind(x = x, y = y)
    acf.out =  acf(X, lag.max = lag.max, plot = FALSE, type = type[1])
    lag = c(rev(acf.out$lag[-1, 2, 1]), acf.out$lag[, 1, 2])
    y = c(rev(acf.out$acf[-1, 2, 1]), acf.out$acf[, 1, 2])
    acf.out$acf = array(y, dim = c(length(y), 1, 1))
    acf.out$lag = array(lag, dim = c(length(y), 1, 1))
    acf.out$snames = paste(acf.out$snames, collapse = " & ")
    plot(acf.out, main = main, xlab = xlab, ylab = ylab, ...)
    
    # Return Value:
    invisible(acf.out)  
}



# ------------------------------------------------------------------------------


teffectPlot =
function (x, deltas = seq(from = 0.2, to = 3.0, by = 0.2), lag.max = 10, 
ymax = NA, standardize = TRUE, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Evaluate and Display Taylor Effect
  
    # FUNCTION:
    
    # Convert Type:
    if (class(x) == "timeSeries") stopifnot(isUnivariate(x))
    x = as.vector(x)
    
    # Labels:
    if (labels) {
        main = "Taylor Effect"
        xlab = "Exponent Delta"
        ylab = "Autocorrelation"
    } else {
        main = xlab = ylab = ""
    }
    
    # Standardize:
    if(standardize) x = (x-mean(x))/sqrt(var(x))
        data = matrix(data = rep(0, times = lag.max*length(deltas)),
        nrow = lag.max, byrow = TRUE)
    for (id in 1:length(deltas))
        data[,id] = as.double(acf(abs(x)^deltas[id], 
            lag.max = lag.max, type="corr", plot = FALSE)$acf)[2:(lag.max+1)]
    if (is.na(ymax)) ymax = max(data)
    
    # Plot:
    plot(deltas, data[1,], ylim = c(0, ymax), type = "n", 
        main = main, xlab = xlab, ylab = ylab, ...)
    xl = 1:length(deltas)
    for (il in 1:(lag.max)) {
        yp = max(data[il, ])
        yl = xl[data[il, ] == yp]
        lines(deltas, data[il, ], col = il)
        points(deltas[yl], yp, pch = 19)
        lines (c(1, 1), c(0, ymax))
    }
    
    # Grid:
    if (labels) grid()
            
    # Return Value:
    invisible(data)
}


# ------------------------------------------------------------------------------


lmacfPlot = 
function(x, lag.max = max(2, floor(10*log10(length(x)))), 
ci = 0.95, type = c("both", "acf", "hurst"), labels = TRUE, 
details = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Evaluate and display long memory autocorrelation Function.

    # FUNCTION:
    
    # Convert Type:
    if (class(x) == "timeSeries") stopifnot(isUnivariate(x))
    x = as.vector(x)
    
    # Plot Type:
    type = match.arg(type)
    
    # Labels:
    if (labels) {
        main1 = "ACF"
        xlab1 = "lag"
        ylab1 = "ACF"
        main2 = "log-log ACF"
        xlab2 = "log lag"
        ylab2 = "log ACF"
    } else {
        main1 = xlab1 = ylab1 = ""
        main2 = xlab2 = ylab2 = ""
    }
    
    # Transform:
    x.ret = x
    x = abs(x.ret)
    
    # Compute:
    z = acf(x, lag.max = lag.max, type = "correlation", plot = FALSE)
    z$acf[1] = 0
    cl = qnorm(0.5 + ci/2)/sqrt(z$n.used)
    z.min = min(z$acf, -cl)
    
    # lin-lin plot excluding one:
    x = seq(0, lag.max, by = 1)
    y = z$acf 
    if (type == "both" | type == "acf") {
        plot(x = x[-1], y = y[-1], type = "l", main = main1, 
            col = "steelblue", xlab = xlab1, ylab = ylab1, 
            xlim = c(0, lag.max), ylim = c(-2*cl, max(y[-1])), ...)
        # abline(h = 0, lty = 3)
    }
    if (details) {
        cat ("\nLong Memory Autocorrelation Function:")
            paste (cat ("\n  Maximum Lag        "), cat(lag.max))
            paste (cat ("\n  Cut-Off ConfLevel  "), cat(cl))
    }
    ACF = acf(x.ret, lag.max = lag.max, plot = FALSE)$acf[,,1]
    lines(x = 1:lag.max, y = ACF[-1], type = "l", col = "steelblue")
    lines(x = c(-0.1, 1.1)*lag.max, y = c(+cl, +cl), lty = 3, col = 'grey')
    lines(x = c(-0.1, 1.1)*lag.max, y = c(-cl, -cl), lty = 3, col = 'grey')
    
    # log-log:
    x = x[y > cl]
    y = y[y > cl]
    # log-log:
    if (length(x) < 10) {
        Fit = c(NA, NA)
        hurst = NA
        cat("\n  The time series exhibits no long memory! \n") 
    } else {
        if (type == "both" | type == "hurst") {
            plot(x = log(x), y = log(y), type = "l", xlab = xlab2, 
                ylab = ylab2, main = main2, col = "steelblue", ...)
            # Grid:
            if (labels) grid()
        }
        Fit = lsfit(log(x), log(y))
        fit = unlist(Fit)[1:2]
        ### fit = l1fit(log(x), log(y))$coefficients
        abline(fit[1], fit[2], col = 1)
        hurst = 1 + fit[2]/2 
        if (details) {
            paste (cat ('\n  Plot-Intercept     '), cat(fit[1]))
            paste (cat ('\n  Plot-Slope         '), cat(fit[2]))
            paste (cat ('\n  Hurst Exponent     '), cat(hurst), cat("\n")) 
        } 
    }
                
    # Return Value:
    invisible(list(fit = Fit, hurst = hurst))
}


# ------------------------------------------------------------------------------


lacfPlot = 
function(x, n = 12, lag.max = 20, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the lagged autocorrelation function
    
    # Arguments:
    #   x - numeric vector of prices or Index Values:
    
    # FUNCTION:
    
    # Convert Type:
    if (class(x) == "timeSeries") stopifnot(isUnivariate(x))
    x = as.vector(x)
    
    # Labels:
    if (labels) {
        main = "Lagged Correlations"
        xlab = "tau"
        ylab = "Correlation"
    } else {
        main = xlab = ylab = ""
    }
    
    # Truncate to multiple of n:
    N = trunc(length(x)/n)
    M = length(x) - n*N
    if (M > 0) x = x[-c(1:M)]
    
    # One Step Volatilities:
    x.ret = c(0, diff(log(x)))
    x.mat = matrix(x.ret, byrow = TRUE, ncol = n)
    u = apply(abs(x.mat), 1, mean)
    
    # n-step Volatilities:
    index = n*(1:N)
    v = abs(c(0, diff(log(x[index]))))
    
    # Zero Tau:
    L = length(u)
    RhoZero = cor(u, v)
    # print(RhoZero)
    
    # Positive Tau:
    RhoPos = NULL
    for (tau in 1:lag.max) {
        X = u[-((L-tau+1):L)]
        X2 = X 
        Y = v[-((L-tau+1):L)]
        Y2 = v[-(1:tau)]
        X.mean = mean(X)
        Y.mean = mean(Y)        
        X1 = sum((X - X.mean)^2)
        Y1 = sum((Y - Y.mean)^2)    
        XY1 = sum( (X2-X.mean)*(Y2-Y.mean) )
        rho = XY1/sqrt(X1*Y1)
        RhoPos = c(RhoPos, rho)
    }
    
    # Negative Tau:
    RhoNeg = NULL
    for (tau in 1:lag.max) {
        X = v[-((L-tau+1):L)]
        X2 = X 
        Y = u[-((L-tau+1):L)]
        Y2 = u[-(1:tau)]
        X.mean = mean(X)
        Y.mean = mean(Y)        
        X1 = sum((X - X.mean)^2)
        Y1 = sum((Y - Y.mean)^2)    
        XY1 = sum( (X2-X.mean)*(Y2-Y.mean) )
        rho = XY1/sqrt(X1*Y1)
        RhoNeg = c(RhoNeg, rho)
    }
    
    # Correlations:
    Lagged = RhoPos - RhoNeg
    Rho = c(rev(RhoNeg), RhoZero, RhoPos)
    
    # Plot:
    plot(x = (-lag.max):(lag.max), y = Rho, type = "l", xlab = xlab, 
        ylab = ylab, ylim = c(min(Lagged), max(Rho)),
        main = main, ...)
    points(-lag.max:lag.max, Rho, pch = 19, cex = 0.7)
    lines(0:lag.max, c(0, Lagged), col = "red")
    points(0:lag.max, c(0, Lagged), pch = 19, cex = 0.7, col = "red")
    abline(h = 0, col = "grey", lty = 3)
    ci = 1/sqrt(length(u))
    abline(h = +ci, col = "blue")
    abline(h = -ci, col = "blue")
    if (labels) grid()
    
    # Grid:
    if (labels) grid()
    
    # Return Value:
    invisible(list(Rho = Rho, Lagged = Lagged))
}


# ------------------------------------------------------------------------------


logpdfPlot = 
function(x, n = 50, type = c("lin-log", "log-log"), 
doplot = TRUE, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns a pdf plot on a lin-log scale in
    #   comparisin to a Gaussian density plot
    #   and return the break-midpoints and the
    #   counts obtained from the histogram of
    #   the empirical data.

    # FUNCTION:
    
    # Convert Type:
    if (class(x) == "timeSeries") stopifnot(isUnivariate(x))
    x = as.vector(x)
    
    # Select Type:
    type = match.arg(type)
    
    # Labels:
    if (labels) {
        if (type == "lin-log") {
            main = "log PDF"
            xlab = "x"
            ylab = "log PDF"
        } else if (type == "log-log") {
            main = "log PDF"
            xlab = "log x"
            ylab = "log PDF"
        }    
    } else {
        main = xlab = ylab = ""
    }
    

    # Lin-Log Plot:
    if (type == "lin-log") {
          
        # Histogram PDF:
        result = hist(x, nclass = n, plot = FALSE)
        prob.counts = result$counts/sum(result$counts) / diff(result$breaks)[1]
        histogram = list(breaks = result$breaks, counts = prob.counts) 
        
        # Histogram Count & Break-Midpoints:
        yh = histogram$counts
        xh = histogram$breaks
        xh = xh[1:(length(xh)-1)] + diff(xh)/2
        xh = xh[yh > 0]
        yh = log(yh[yh > 0])
        if (doplot) {
            par(err = -1)
            plot(xh, yh, type = "p", pch = 19, col = "steelblue", 
                main = main, xlab = xlab, ylab = ylab, ...)
        } 
        
        # Compare with a Gaussian Plot:
        xg = seq(from = xh[1], to = xh[length(xh)], length = n)
        yg = log(dnorm(xg, mean(x), sqrt(var(x))))
        if (doplot) { 
            par(err = -1)
            lines(xg, yg, col = "brown")
        }
        
        # Return Value:
        result = list(breaks = xh, counts = yh, fbreaks = xg, 
            fcounts = yg)      
    }
    
    # Log-Log Plot:
    if (type == "log-log") {
        
        # Histogram PDF:
        result = hist(x, nclass = n, plot = FALSE)
        prob.counts = result$counts/sum(result$counts) / diff(result$breaks)[1]
        histogram = list(breaks = result$breaks, counts = prob.counts) 
         
        # Histogram Count & Breaks:
        yh = histogram$counts
        xh = histogram$breaks
        xh = xh[1:(length(xh)-1)] + diff(xh)/2
        xh = xh[yh > 0]
        yh = yh[yh > 0]
        yh1 = yh[xh < 0]
        xh1 = abs(xh[xh < 0])
        yh2 = yh[xh > 0]
        xh2 = xh[xh > 0]
        if (doplot) {
            plot(log(xh1), log(yh1), type = "p", pch = 19, col = "steelblue", 
                main = main, xlab = xlab, ylab = ylab, ...) 
            par(err = -1)
            points(log(xh2), log(yh2), col = 2) 
        }
        
        # Compare with a Gaussian plot:
        xg = seq(from = min(xh1[1], xh[2]), 
            to = max(xh1[length(xh1)], xh2[length(xh2)]), length = n)
        xg = xg[xg > 0]
        yg = log(dnorm(xg, mean(x), sqrt(var(x))))
        if (doplot) {
            par(err = -1)
            lines(log(xg), yg, col = "brown")
        }
        
        # Result:
        result = list(breaks = c(xh1, xh2), counts = c(yh1, yh2), 
            fbreaks = c(-rev(xg), xg), fcounts = c(-rev(yg), yg))
    }
    
    # Grid:
    if (labels) grid()
    
    # Return Value:
    invisible(result)
}


# ------------------------------------------------------------------------------


qqgaussPlot = 
function(x, span = 5, col = "steelblue", labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns a Quantile-Quantile plot.
    
    # FUNCTION:
    
    # Settings:
    # if (SPLUSLIKE) splusLikePlot(TRUE)
    
    # Convert Type:
    if (class(x) == "timeSeries") stopifnot(isUnivariate(x))
    x = as.vector(x)
    
    # Labels:
    if (labels) {
        main = "Normal QQ Plot"
        xlab = "Theoretical Quantiles"
        ylab = "Sample Quantiles"
    } else {
        main = xlab = ylab = ""
    }
    
    # Standardized qqnorm():
    y = (x-mean(x)) / sqrt(var(x))
    
    # Further Settings:
    y[abs(y) < span]
    lim = c(-span, span)
    
    # Plot qqnorm:
    qqnorm(y, main = main, xlab = xlab, ylab = ylab, 
        xlim = lim, ylim = lim, col = col, ...) 

    # Add Line:
    qqline(y, ...)
    
    # Grid:
    if (labels) grid()
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


scalinglawPlot =
function(x, span = ceiling(log(length(x)/252)/log(2)), doplot = TRUE, 
labels = TRUE, details = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
  
    # Description:
    #   Investigates the scaling law.
    #   The input "x" requires log-returns.
 
    # FUNCTION: 
    
    # Convert Type:
    if (class(x) == "timeSeries") stopifnot(isUnivariate(x))
    x = as.vector(x)
    
    # Labels:
    if (labels) {
        main = "Scaling Law Plot"
        xlab = "log-time"
        ylab = "log-volatility"
    } else {
        main = xlab = ylab = ""
    }
    
    # Settings:
    logtimesteps = span
    xmean = mean(x)
    
    # x have to be logarithmic returns
    y = (x-xmean)
    logprices = cumsum(y)
    
    # Scaling Power Low:
    scale = function (nx, logprices) {
        sum(abs(diff(logprices, lag = (2^nx))))}     
    nx = 0:logtimesteps; x = nx*log(2)
    y = log(apply(matrix(nx), 1, scale, logprices))
    # fit = lsfit(x, y)$coefficients
    
    # Runs in both environments, R and SPlus:
    fit = lsfit(x, y)
    
    # Robust Fit:       
    # fit = l1fit(x, y) 
    
    # Fit Result:
    Fit = unlist(fit)[1:2]
    alpha = 1.0/Fit[2]
    if (doplot) { 
        plot(x, y, main = main, xlab = xlab, ylab = ylab, ...)
        abline(Fit[1], Fit[2], col = 2)
        abline(Fit[1], 0.5, col = 3) 
    }
    if (labels) grid()
    
    # Details:
    if (details) {
        cat ("\nScaling Law:")
        cat ("\n  Plot Intercept     ", fit$coefficients[1])
        cat ("\n  Plot Slope         ", fit$coefficients[2])
        cat ("\n  Plot Inverse Slope ", 1/fit$coefficients[2])
        cat ("\n\n") 
    } 
    
    # Return Value:
    invisible(list(exponent = as.numeric(alpha), fit = fit))
}


################################################################################

