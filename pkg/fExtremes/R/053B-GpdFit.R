
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
# FUNCTION:                 GPD MODELLING FROM EVIS:
#  gpdSim                    Simulates GPD rvs
#  gpdFit                    Fits GPD Distribution
#   print.gpd                 Print Method for object of class "gpd"
#   plot.gpd                  Plot Method for object of class "gpd"
#   summary.gpd               Summary Method for object of class "gpd"
# FUNCTION:                 ADDITIONAL PLOTS:
#  gpdtailPlot               Plots Tail Estimate From GPD Model
#  gpdquantPlot              Plots of GPD Tail Estimate of a High Quantile
#  gpdshapePlot              Plots for GPD Shape Parameter
#  gpdqPlot                  Adds Quantile Estimates to plot.gpd
#  gpdsfallPlot              Adds Expected Shortfall Estimates to a GPD Plot
# FUNCTION:                 ADDITIONAL FUNCTION:
#   gpdriskmeasures          Calculates Quantiles and Expected Shortfalls
################################################################################


gpdSim = 
function(model = list(shape = 0.25, location = 0, scale = 1), n = 1000)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates random variates from a GPD distribution
    
    
    # FUNCTION:
    
    # Simulate:
    rgpd(n = n, xi = model$shape, mu = model$location, beta = model$scale)  
}


# ------------------------------------------------------------------------------


gpdFit =
function(x, threshold = NA, nextremes = NA, type = c("mle", "pwm"),
information = c("observed", "expected"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns an object of class `"gpd"' representing the fit of a
    #   generalized Pareto model to excesses over a high threshold
    
    # Notes:
    #   This is a wrapper to EVIR's 'gpd' function.

    # FUNCTION:
    
    # Make the fit:
    call = match.call()
    type = type[1]
    # if (is.na(threshold) & is.na(nextremes)) threshold = min(x)
    if (type == "mle") {
        type = "ml"
    }
    fitted = gpd(data = x, threshold = threshold, nextremes = nextremes, 
        method = type, information = information, ...) 
        
    # Residuals:
    xi = fitted$par.ests["xi"]
    beta = fitted$par.ests["beta"]
    excess = as.numeric(fitted$data) - fitted$threshold
    residuals = log(1 + (xi * excess)/beta)/xi
    
    # Make Unique:
    fit = list()
    fit$fit =  fitted
    fit$call = call
    fit$type = c("gpd", type[1])
    fit$par.ests = fitted$par.ests
    fit$par.ses = fitted$par.ses
    fit$residuals = residuals
    fit$fitted.values = fitted$data - residuals
    fit$llh = fitted$nllh.final
    fit$converged = fitted$converged
    
    # Return Value:
    class(fit) = "gpdFit"   
    fit
}


# ------------------------------------------------------------------------------


print.gpdFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print Method for an object of class 'gpdFit'
    
    # FUNCTION:
    
    # Function Call:
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "") 
            
    # Estimation Type:
    cat("\nEstimation Type:", x$type, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(x$par.ests)
    cat("\n")
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


plot.gpdFit =
function(x, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for objects of class 'gpdFit'

    # FUNCTION:
    
    # Plot Functions:
    plot.1 <<- function(x, ...) {
        fit = x
        data = fit$fit$data
        xi = fit$par.ests[1]
        beta = fit$par.est[2]
        threshold = fit$fit$threshold
        optlog = NA
        extend = 1.5
        labels = TRUE
        # Start:
        plotmin = threshold
        if (extend <= 1) stop("extend must be > 1")
        plotmax = max(data) * extend
        xx = seq(from = 0, to = 1, length = 1000)
        z = qgpd(xx, xi, threshold, beta)
        z = pmax(pmin(z, plotmax), plotmin)
        ypoints = ppoints(sort(data))
        y = pgpd(z, xi, threshold, beta) 
        type = "eplot"
        if (!is.na(optlog)) alog = optlog
        else alog = "x"
        if (alog == "xy") stop("Double log does not make much sense")
        yylab = "Fu(x-u)"
        shape = xi
        scale = beta
        location = threshold
        plot(sort(data), ypoints, xlim = range(plotmin, plotmax),
            ylim = range(ypoints, y, na.rm = TRUE), xlab = "",
            ylab = "", log = alog, axes = TRUE, 
            main = "Excess Distribution", ...)
        lines(z[y >= 0], y[y >= 0])    
        xxlab = "x"
        if (alog == "x" || alog == "xy" || alog == "yx")
            xxlab = paste(xxlab, "(on log scale)")
        if (alog == "xy" || alog == "yx" || alog == "y")
            yylab = paste(yylab, "(on log scale)")
        title(xlab = xxlab, ylab = yylab) } 
    plot.2 <<- function(x, ...) {
        fit = x
        data = fit$fit$data
        xi = fit$par.ests[1]
        beta = fit$par.est[2]
        threshold = fit$fit$threshold
        optlog = NA
        extend = 1.5 #; if(extend <= 1) stop("extend must be > 1")
        labels = TRUE
        # Start:
        plotmin = threshold
        if (extend <= 1) stop("extend must be > 1")
        plotmax = max(data) * extend
        xx = seq(from = 0, to = 1, length = 1000)
        z = qgpd(xx, xi, threshold, beta)
        z = pmax(pmin(z, plotmax), plotmin)
        ypoints = ppoints(sort(data))
        y = pgpd(z, xi, threshold, beta)
        type = "tail"
        if (!is.na(optlog)) alog = optlog
        else alog = "xy"
        prob = fit$fit$p.less.thresh
        ypoints = (1 - prob) * (1 - ypoints)
        y = (1 - prob) * (1 - y)
        yylab = "1-F(x)"
        shape = xi
        scale = beta * (1 - prob)^xi
        location = threshold - (scale * ((1 - prob)^( - xi) - 1))/xi
        plot(sort(data), ypoints, xlim = range(plotmin, plotmax),
            ylim = range(ypoints, y, na.rm = TRUE), xlab = "",
            ylab = "", log = alog, axes = TRUE, 
            main = "Tail of Underlying Distribution", ...)
        lines(z[y >= 0], y[y >= 0])    
        xxlab = "x"
        if (alog == "x" || alog == "xy" || alog == "yx")
            xxlab = paste(xxlab, "(on log scale)")
        if (alog == "xy" || alog == "yx" || alog == "y")
            yylab = paste(yylab, "(on log scale)")
        title(xlab = xxlab, ylab = yylab) }
    plot.3 <<- function(x, ...) {
        res = x$residuals
        plot(res, 
            ylab = "Residuals", 
            xlab = "Ordering", 
            main = "Scatterplot of Residuals", ...)
        lines(lowess(1:length(res), res)) }      
    plot.4 <<- function(x, ...) {
        qplot(x$residuals, 
            main = "QQ-Plot of Residuals", ...) }
            
    # Plot:
    interactivePlot(
        x = x,
        choices = c(
            "Excess Distribution",
            "Tail of Underlying Distribution",
            "Scatterplot of Residuals", 
            "QQ-Plot of Residuals"),
        plotFUN = c(
            "plot.1", 
            "plot.2", 
            "plot.3",
            "plot.4"),
        which = which)
                    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


summary.gpdFit = 
function(object, doplot = TRUE, which = "all", ...) 
{   # A function written by Diethelm Wuertz

    # Description:
    #   Summary method for objects of class "gpdFit"
    
    # FUNCTION:
    
    # Print:
    print(object, ...)
    
    # Summary:
    # For MLE print additionally:
    cat("\nStandard Deviations:\n"); print(object$par.ses)
    cat("\nLog-Likelihood Value: ", object$llh)
    cat("\nType of Convergence:  ", object$conv, "\n") 
    cat("\n")
    
    # Plot:
    if (doplot) plot(object, which = which, ...)
    cat("\n")
    
    # Return Value:
    invisible(object)
}


# ******************************************************************************


gpdtailPlot =
function(fit, optlog = NA, extend = 1.5, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots Tail Estimate From GPD Model
    
    # FUNCTION:
    
    # Return Value:
    tailplot(x = fit$fit, optlog = optlog, extend = extend, 
        labels = labels, ...)
}


# ------------------------------------------------------------------------------


gpdquantPlot =
function(data, p = 0.99, models = 30, start = 15, end = 500,
reverse = TRUE, ci = 0.95, autoscale = TRUE, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots of GPD Tail Estimate of a High Quantile
    
    # FUNCTION:

    # Return Value:
    quant(data = data, p = p, models = models, start = start, end = end,
        reverse = reverse, ci = ci, auto.scale = autoscale, labels = labels,
        ...)
}


# ------------------------------------------------------------------------------


gpdshapePlot =
function(data, models = 30, start = 15, end = 500, reverse = TRUE, 
ci = 0.95, autoscale = TRUE, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots for GPD Shape Parameter
    
    # FUNCTION:

    # Return Value:
    shape(data = data, models = models, start = start, end = end, 
        reverse = reverse, ci = ci, auto.scale = autoscale, 
        labels = labels, ...)
}


# ------------------------------------------------------------------------------


gpdqPlot =
function(x, pp = 0.99, ci.type = c("likelihood", "wald"), ci.p = 0.95, 
like.num = 50)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Adds Quantile Estimates to plot.gpd
    
    # Arguments:
    #   x  - an object of class 'gpdFit'
    #   pp - the probability level
    
    # FUNCTION:

    # Return Value:
    gpd.q(x = x, pp = pp, ci.type = ci.type, ci.p = ci.p, like.num = like.num)
}


# ------------------------------------------------------------------------------


gpdsfallPlot =
function(x, pp = 0.99, ci.p = 0.95, like.num = 50)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Adds Expected Shortfall Estimates to a GPD Plot 
    
    # Arguments:
    #   x  - an object of class 'gpdFit'
    #   pp - the probability level
    
    # FUNCTION:

    # Return Value:
    gpd.sfall(x = x, pp = pp, ci.p = ci.p, like.num = like.num)
}


# ------------------------------------------------------------------------------


gpdriskmeasures = 
function(x, plevels = c(0.99, 0.995, 0.999, 0.9995, 0.9999))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates Quantiles and Expected Shortfalls
    
    # Arguments:
    #   x  - an object of class 'gpdFit'
    #   p - a numeric value or vector of probability levels 
    
    # FUNCTION:
    
    # Return Value:
    as.data.frame(riskmeasures(x = x$fit, p = plevels))
}


# ******************************************************************************

