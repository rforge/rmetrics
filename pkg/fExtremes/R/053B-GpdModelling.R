
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


# ##############################################################################
# FUNCTION:              GPD DISTRIBUTION FAMILY:
# dgpd                    Density for the Generalized Pareto DF [USE FROM EVIS]
#  pgpd                    Probability for the Generalized Pareto DF
#  qgpd                    Quantiles for the Generalized Pareto DF
#  rgpd                    Random variates for the Generalized Pareto DF
# FUNCTION:              MOMENTS:
#  .gpdMoments            Computes true statistics for GPD distribution
# FUNCTION:              GPD MODELLING FROM EVIS:
#  gpdSim                 Simulates GPD rvs
#  fGPD                   S4 Class Representation
#  gpdFit                 Fits GPD Distribution
#   print.gpd              Print Method for object of class "gpd"
#   plot.gpd               Plot Method for object of class "gpd"
#   summary.gpd            Summary Method for object of class "gpd"
# FUNCTION:              ADDITIONAL PLOTS:
#  gpdtailPlot            Plots Tail Estimate From GPD Model
#  gpdquantPlot           Plots of GPD Tail Estimate of a High Quantile
#  gpdshapePlot           Plots for GPD Shape Parameter
#  gpdqPlot               Adds Quantile Estimates to plot.gpd
#  gpdsfallPlot           Adds Expected Shortfall Estimates to a GPD Plot
# FUNCTION:              ADDITIONAL FUNCTION:
#  gpdriskmeasures        Calculates Quantiles and Expected Shortfalls
################################################################################


################################################################################
# GPD DISTRIBUTION FAMILY:


dgpd = 
function(x, xi = 1, mu = 0, beta = 1)
{   # A function written by Diethelm Wuertz

    # FUNCTION:
    
    # Density:
    y = (x - mu)
    if (xi == 0) {
        d = (1-exp(-y/beta))/beta 
    } else {
        d = 1/beta * (1 + (xi*y)/beta)^((-1/xi) - 1) }  
    
    d[y < 0] = 0
    if (xi < 0) d[y > (-1/xi)] = 0
    
    # Return Value:
    d
}


# ------------------------------------------------------------------------------


pgpd = 
function(q, xi = 1, mu = 0, beta = 1)
{   # A function written by Diethelm Wuertz

    # FUNCTION:
    
    # Probability:
    y = (q - mu)
    if (xi == 0) {
        p = y/beta + exp(-y/beta) -1 
    } else {
        p = (1 - (1 + (xi*y)/beta)^(-1/xi)) }   
    
    p[y < 0] = 0
    if (xi < 0) p[y > (-1/xi)] = 1
    
    # Return Value:
    p
}


# ------------------------------------------------------------------------------


qgpd = 
function(p, xi = 1, mu = 0, beta = 1)
{   # A function written by Diethelm Wuertz

    # FUNCTION:
    
    # Quantiles:
    if (xi == 0) {
        q = mu - beta*log(1-p)
    } else {
        q = mu + (beta/xi) * ((1 - p)^( - xi) - 1)
    }
    
    # Return Value:
    q
}


# ------------------------------------------------------------------------------


rgpd = 
function(n, xi = 1, mu = 0, beta = 1)
{   # A function written by Diethelm Wuertz

    # FUNCTION:
    
    # Random variates:
    rvs = mu + (beta/xi) * ((1 - runif (n))^( - xi) - 1)
    
    # Return Value:
    rvs
}


# ------------------------------------------------------------------------------


.gpdMoments = 
function(xi, mu = 0, beta = 1)
{   # A function implemented by Diethelm Wuertz
 
    # Description:
    #   Compute true statistics for Generalized Pareto distribution
    
    # Value:
    #   Returns true mean of Generalized Pareto distribution 
    #   for xi < 1 else NaN
    #   Returns true variance of Generalized Pareto distribution 
    #   for xi < 1 else NaN

    # FUNCTION: 
    
    # MEAN: Rreturns 1 for x <= 0 and -Inf's's else
    a = c(1, NaN, NaN)
    gpdMean = beta/(1-xi)*a[sign(xi-1)+2]
    
    # VAR: Rreturns 1 for x <= 0 and -Inf's's else
    a = c(1, NaN, NaN)
    gpdVar = beta*beta/(1-xi)^2/(1-2*xi) * a[sign(2*xi-1)+2]

    # Return Value:
    list(mean = gevMean, var = gevVar)              
}


################################################################################


gpdSim = 
function(model = list(shape = 0.25, location = 0, scale = 1), n = 1000, 
seed = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates random variates from a GPD distribution
    
    # FUNCTION:
    
    # Seed:
    if (is.null(seed)) seed = NA else set.seed(seed)
    
    # Simulate:
    ans = rgpd(n = n, xi = model$shape, mu = model$location, 
        beta = model$scale)  
    ans = as.ts(ans)

    # Control:
    attr(ans, "control") = 
        data.frame(t(unlist(model)), seed = seed, row.names = "")
        
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


setClass("fGPD", 
    representation(
        call = "call",
        data = "list",
        method = "character",
        fit = "list",
        title = "character",
        description = "character"
    )  
)


# ------------------------------------------------------------------------------


.old.gpdFit =
function(x, threshold = NA, nextremes = NA, type = c("mle", "pwm"),
information = c("observed", "expected"), title = NULL, description = NULL, ...)
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
    fitted = .gpd(data = x, threshold = threshold, nextremes = nextremes, 
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
    class(fit) = c("list", "gpdFit")
    
    # Add title and description:
    if (is.null(title)) title = "POT Parameter Estimation"
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    new("fGEV",
        call = match.call(),
        data = list(x = x),
        method = fit$type,
        fit = fit,
        title = "character",
        description = "character")
}


# ------------------------------------------------------------------------------


gpdFit =
function(x, u = quantile(x, 0.95), type = c("mle", "pwm"),
information = c("observed", "expected"), title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns an object of class `"gpd"' representing the fit of a
    #   generalized Pareto model to excesses over a high threshold
    
    # Notes:
    #   This is a wrapper to EVIR's 'gpd' function.

    # FUNCTION:
    
    # Make the fit:
    type = type[1]

    # parameter Estimation:
    if (type[1] == "mle") {
        fit = .gpdmleFit(x, u)
    } else if (type[1] == "pwm") {
        fit = .gpdpwmFit(x, u)
    }
    fit$threshold = u
    data = as.vector(x)
    fit$prob = 1 - length(data[data>u])/length(data)
   
    # Residuals:
    xi = fit$par.ests["xi"]
    beta = fit$par.ests["beta"]
    x = as.vector(x)
    excess = x[x > u] - u
    residuals = log(1 + (xi * excess)/beta)/xi
    
    # Add title and description:
    if (is.null(title)) title = "POT Parameter Estimation"
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    new("fGPD",
        call = match.call(),
        data = list(x = x, excess = excess, residuals = residuals),
        method = c("gpd", type[1]), 
        fit = fit,
        title = title,
        description = description)
}


# ------------------------------------------------------------------------------


.gpdmleFit = 
function (x, u = quantile(x, 0.95), 
information = c("observed", "expected"), ...) 
{   # A function implemented by Diethelm Wuertz

    x = as.vector(x)
    excess = x[x > u] - u
    theta = .gpdpwmFit(x = x, u = u)$par.ests
    
    # Parameter Estimation:
    fit = optim(theta, .gpdLLH, hessian = TRUE, excess = excess)
    names(fit$par) = c("xi", "beta")
    
    # Error Estimates:
    if (information[1] == "observed") 
        varcov = solve(fit$hessian)
    if (information[1] == "expected") {
        one = (1 + par.ests[1])^2/Nu
        two = (2 * (1 + par.ests[1]) * par.ests[2]^2)/Nu
        cov = -((1 + par.ests[1]) * par.ests[2])/Nu
        varcov = matrix(c(one, cov, cov, two), 2)
    }
    par.ses = sqrt(diag(varcov))
    names(par.ses) = c("xi", "beta")  
    
    # Return Value:
    list(par.ests = fit$par, par.ses = par.ses, fit = fit, varcov = varcov)
}


# ------------------------------------------------------------------------------


.gpdLLH =
function(theta, excess)
{   # A function implemented by Diethelm Wuertz

    xi = theta[1]
    beta = theta[2]
    cond = (beta <= 0) || ((xi <= 0) && (max(excess) > (-beta/xi)))
    if (cond) {
        func = 1.0e+6
    } else {
        y = log(1+(xi*excess)/beta) / xi
        func = length(excess) * log(beta) + (1+xi)*sum(y)
    }
    func
}


# ------------------------------------------------------------------------------


.gpdpwmFit = 
function (x, u = quantile(x, 0.95)) 
{   # A function implemented by Diethelm Wuertz

    x = as.vector(x)
    excess = x[x > u] - u 
    Nu = length(excess)
    a0 = mean(excess)
    pvec = ((1:Nu) - 0.35)/Nu
    a1 = mean(sort(excess) * (1 - pvec))
    xi = 2 - a0/(a0 - 2 * a1)
    beta = (2 * a0 * a1)/(a0 - 2 * a1)
    par.ests = c(xi = xi, beta = beta)
    list(par.ests = par.ests, par.ses = NA, fit = NA, varcov = NA)
}


# ------------------------------------------------------------------------------


print.fGPD =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print Method for an object of class 'gpdFit'
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n ", x@title, "\n")
    
    # Function Call:
    cat("\nCall:\n ")
    cat(paste(deparse(x@call), sep = "\n", collapse = "\n"), "\n", sep = "") 
            
    # Estimation Type:
    cat("\nEstimation Method:\n ", x@method, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n ")
    print(x@fit$par.ests)
    
    # Desription:
    cat("\nDescription\n ", x@description, "\n\n")
    
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


plot.fGPD =
function(x, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for objects of class 'fGPD'

    # FUNCTION:
    
    # Plot Functions:
    plot.1 <<- function(x, ...) {
        fit = x@fit
        data = as.vector(x@data$x)
        xi = fit$par.ests[1]
        beta = fit$par.est[2]
        threshold = fit$threshold
        optlog = NA
        extend = 1.5
        labels = TRUE
        # Start:
        plotmin = threshold
        if (extend <= 1) {
            stop("extend must be > 1")
        }
        plotmax = max(data) * extend
        xx = seq(from = 0, to = 1, length = 1000)
        z = qgpd(xx, xi, threshold, beta)
        z = pmax(pmin(z, plotmax), plotmin)
        ypoints = ppoints(sort(data))
        y = pgpd(z, xi, threshold, beta) 
        type = "eplot"
        if (!is.na(optlog)) {
            alog = optlog
        } else {
            alog = "x"
        }
        if (alog == "xy") {
            stop("Double log does not make much sense")
        }
        yylab = "Fu(x-u)"
        shape = xi
        scale = beta
        location = threshold
        plot(sort(data), ypoints, xlim = range(plotmin, plotmax),
            ylim = range(ypoints, y, na.rm = TRUE), xlab = "",
            ylab = "", log = alog, axes = TRUE, col = "steelblue",
            pch = 19, main = "Excess Distribution")
        lines(z[y >= 0], y[y >= 0])    
        xxlab = "x"
        if (alog == "x" || alog == "xy" || alog == "yx") {
            xxlab = paste(xxlab, "(on log scale)")
        }
        if (alog == "xy" || alog == "yx" || alog == "y") {
            yylab = paste(yylab, "(on log scale)")
        }
        title(xlab = xxlab, ylab = yylab) 
    } 
    plot.2 <<- function(x, ...) {
        fit = x@fit
        data = as.vector(x@data$x)
        xi = fit$par.ests[1]
        beta = fit$par.est[2]
        threshold = fit$threshold
        optlog = NA
        extend = 1.5 #; if (extend <= 1) stop("extend must be > 1")
        labels = TRUE
        # Start:
        plotmin = threshold
        if (extend <= 1) {
            stop("extend must be > 1")
        }
        plotmax = max(data) * extend
        xx = seq(from = 0, to = 1, length = 1000)
        z = qgpd(xx, xi, threshold, beta)
        z = pmax(pmin(z, plotmax), plotmin)
        ypoints = ppoints(sort(data))
        y = pgpd(z, xi, threshold, beta)
        type = "tail"
        if (!is.na(optlog)) {
            alog = optlog
        } else {
            alog = "xy"
        }
        prob = fit$fit$p.less.thresh
        ypoints = (1 - prob) * (1 - ypoints)
        y = (1 - prob) * (1 - y)
        yylab = "1-F(x)"
        shape = xi
        scale = beta * (1 - prob)^xi
        location = threshold - (scale * ((1 - prob)^( - xi) - 1))/xi
        plot(sort(data), ypoints, xlim = range(plotmin, plotmax),
            ylim = range(ypoints, y, na.rm = TRUE), xlab = "",
            ylab = "", log = alog, axes = TRUE, col = "steelblue",
            pch = 19, main = "Tail of Underlying Distribution")
        lines(z[y >= 0], y[y >= 0])    
        xxlab = "x"
        if (alog == "x" || alog == "xy" || alog == "yx") {
            xxlab = paste(xxlab, "(on log scale)")
        }
        if (alog == "xy" || alog == "yx" || alog == "y") {
            yylab = paste(yylab, "(on log scale)") 
        }
        title(xlab = xxlab, ylab = yylab)  
    }
    plot.3 <<- function(x, ...) {
        residuals = x@data$residuals
        plot(residuals, ylab = "Residuals", xlab = "Ordering", 
           col = "steelblue", pch = 19,
           main = "Scatterplot of Residuals")
        lines(lowess(1:length(residuals), residuals)) 
        grid()
    }      
    plot.4 <<- function(x, ...) {
        residuals = x@data$residuals
        xi = 0 # x@fit$par.ests["xi"]
        qPlot(residuals, xi, col = "steelblue", pch = 19, labels = FALSE, ...)
        title(xlab = "Ordered Data", ylab = "Exponential Quantiles",
            main = "QQ-Plot of Residuals") 
    }
            
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


summary.fGPD = 
function(object, doplot = TRUE, which = "all", ...) 
{   # A function written by Diethelm Wuertz

    # Description:
    #   Summary method for objects of class "gpdFit"
    
    # FUNCTION:
    
    # @fit Slot:
    title = object@title
    description = object@description
    plotObject = object
    object = object@fit
    class(object) = "gpdFit"
    
    # Title:
    cat("\nTitle:\n" , title, "\n")
    
    # Function Call:
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "") 
            
    # Estimation Type:
    cat("\nEstimation Type:\n ", x$type, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(x$par.ests)
    
    # Summary - For MLE print additionally:
    cat("\nStandard Deviations:\n"); print(object$par.ses)
    cat("\nLog-Likelihood Value: ", object$llh)
    cat("\nType of Convergence:  ", object$conv, "\n") 
    cat("\n")
    
    # Plot:
    if (doplot) {
        plot(plotObject, which = which, ...)
    }
    
    # Desription:
    cat("\nDescription\n ", description, "\n\n")
    
    # Return Value:
    invisible(object)
}


################################################################################


tailPlot =
function(object, plottype = c("xy", "x", "y", ""), extend = 1.5, 
labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots Tail Estimate From GPD Model
    
    # Example:
    #   object = gpdFit(as.timeSeries(data(danishClaims)), u = 10)
    #   gpdtailPlot(object)
    
    # FUNCTION:   
    
    # Settings:
    threshold = object@fit$threshold
    x = as.vector(object@data$x)
    data = x[x > threshold]
    xi = as.numeric(object@fit$par.ests["xi"])
    beta = as.numeric(object@fit$par.ests["beta"])
    
    # Points:
    plotmin = threshold
    plotmax = max(data) * max(1, extend)
    z = qgpd(seq(from = 0, to = 1, length = 501), xi, threshold, beta)
    z = pmax(pmin(z, plotmax), plotmin)    
    invProb = 1 - length(data)/length(x)
    ypoints = invProb*(1-ppoints(sort(data)))
    y = invProb*(1-pgpd(z, xi, threshold, beta))

    # Parameters:
    shape = xi
    scale = beta * invProb^xi
    location = threshold - (scale*(invProb^(- xi)-1))/xi
    
    # Plot:
    plot(sort(data), ypoints, xlim = range(plotmin, plotmax), 
         ylim = range(ypoints, y, na.rm = TRUE), col = "steelblue",
         pch = 19, xlab = "", ylab = "", log = plottype[1], axes = TRUE)
    lines(z[y >= 0], y[y >= 0])
    grid()
    
    # Labels:
    alog = plottype[1]
    if (labels) {
        xLab = "x"
        if (alog == "x" || alog == "xy" || alog == "yx")
            xLab = paste(xLab, "(on log scale)")
        yLab = "1-F(x)"
        if (alog == "xy" || alog == "yx" || alog == "y")
            yLab = paste(yLab, "(on log scale)")
        title(xlab = xLab, ylab = yLab)
    }
    
    # Result:
    ans = list(lastfit = x, type = "tail", dist = "gpd",
        plotmin = plotmin, plotmax = plotmax, alog = plottype[1], 
        location = location, shape = shape, scale = scale)
    
    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


quantPlot =
function(x, models = 30, p = 0.99, start = 15, end = 500,
ci = 0.95, doplot = TRUE, plottype = c("normal", "reverse"), 
labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots of GPD Tail Estimate of a High Quantile
    
    # Example:
    #   Danish = as.timeSeries(data(danishClaims))
    #   quantPlot(Danish)
    
    # FUNCTION:

    # Settings:
    data = as.vector(x)
    n = length(data)
    exceed = trunc(seq(from = min(end, n), to = start, length = models))
    p = max(p, 1 - min(exceed)/n)
    start = max(start, ceiling(length(data) * (1 - p)))

    # Internal Function:
    .quantFit = function(nex, data) {
        prob = 1 - nex/length(as.vector(data))
        fit = gpdFit(data, u = quantile(data, prob))@fit
        c(fit$threshold, fit$par.ests, fit$par.ses, 
            as.vector(fit$varcov)[c(1,4,2)])
    }
    
    # Compute:
    mat = apply(as.matrix(exceed), 1, .quantFit, data = data)
    thresh = mat[1, ]
    xihat = mat[2, ]
    betahat = mat[3, ]
    lambda = length(data)/exceed
    a = lambda * (1 - p)
    gfunc = function(a, xihat) (a^( - xihat) - 1) / xihat
    qest = thresh + betahat * gfunc(a, xihat)
    l = u = qest
    yrange = range(qest)
    
    # Add Confidence Intervals:
    if (ci) {
        qq = qnorm(1 - (1 - ci)/2)
        xivar = mat[4, ]
        betavar = mat[5,  ]
        covar = mat[6,  ]
        scaling = thresh
        betahat = betahat/scaling
        betavar = betavar/(scaling^2)
        covar = covar/scaling
        gfunc.deriv = function(a, xihat)
            (-(a^(-xihat)-1)/xihat-a^(-xihat)*log(a))/xihat
        term1 = betavar * (gfunc(a, xihat))^2
        term2 = xivar * (betahat^2) * (gfunc.deriv(a, xihat))^2
        term3 = 2 * covar * betavar * gfunc(a, xihat) * gfunc.deriv(a, xihat)
        qvar = term1 + term2 + term3
        if (min(qvar) < 0)
            stop(paste(
                "Conditioning problems lead to estimated negative",
                "quantile variance", sep = "\n"))
        qse = scaling * sqrt(qvar)
        u = qest + qse * qq
        l = qest - qse * qq
        yrange = range(qest, u, l)
    }
    
    # Result matrix:
    mat = rbind(thresh, qest, exceed, l, u)
    rownames(mat) = c("threshold", "qest", "exceedances", "lower", "upper")
    colnames(mat) = paste(1:dim(mat)[2])
    
    # Plot:
    if (doplot) {
        if (plottype[1] == "normal") {
            index = exceed
        } else if (plottype[1] == "reverse") {
            index =  -exceed
        }
        plot(index, qest, ylim = yrange, type = "l", xlab = "", ylab = "",
            axes = FALSE)
        axis(1, at = index, lab = paste(exceed))
        axis(2)
        axis(3, at = index, lab = paste(format(signif (thresh, 3))))
        box()
        if (ci) {
            lines(index, l, lty = 2, col = "steelblue")
            lines(index, u, lty = 2, col = "steelblue")
        }
        if (labels) {
            title(xlab = "Exceedances",
                ylab = paste("Quantiles:", substitute(x)))
            mtext("Threshold", side = 3, line = 3)
        }
        p = round(p, 3)
        ci = round(ci, 3)
        text = paste("p =", p, "| ci =", ci, "| start =", 
            start, "| end =", end )
        mtext(text, side = 4, adj = 0, cex = 0.7)
    }
    
    # Add Attributes:
    mat = t(mat)
    attr(mat, "control") = data.frame(cbind(p = p, ci = ci, 
        start = start, end = end), row.names = "")
    
    invisible(mat)
}


# ------------------------------------------------------------------------------


shapePlot =
function(x, models = 30, start = 15, end = 500, ci = 0.95, 
doplot = TRUE, plottype = c("normal", "everse"), labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots for GPD Shape Parameter
    
    # Example:
    
    # FUNCTION:

    # Settings:
    data = as.vector(x)
    X = trunc(seq(from = min(end, length(data)), to = start, length = models))
    
    # Internal Function:
    .shapeFit = function(nex, data) {
        prob = 1 - nex/length(as.vector(data))
        fit = gpdFit(data, u = quantile(data, prob), 
            information = "expected")@fit
        c(fit$threshold, fit$par.ests[1], fit$par.ses[1]) 
    }
    
    # Result Matrix:
    mat = apply(as.matrix(X), 1, .shapeFit, data = data)
    mat = rbind(mat, X)
    rownames(mat) = c("threshold", "shape", "se", "exceedances")
    colnames(mat) = paste(1:dim(mat)[2])

    # Plot:
    if (doplot) {
        thresh = mat[1, ]
        y = mat[2, ]
        yrange = range(y)
        if (plottype[1] == "normal") {
            index = X
        } else if (plottype == "reverse") {
            index =  -X
        }
        if (ci) {
            sd = mat[3, ] * qnorm(1 - (1 - ci)/2)
            yrange = range(y, y + sd, y - sd)
        }
        plot(index, y, ylim = yrange, type = "l", xlab = "", ylab = "",
            axes = FALSE)
        axis(1, at = index, lab = paste(X))
        axis(2)
        axis(3, at = index, lab = paste(format(signif(thresh, 3))))
        box()
        grid()
        if (ci) {
            sd = mat[3, ] * qnorm(1 - (1 - ci)/2)
            yrange = range(y, y + sd, y - sd)
            lines(index, y + sd, lty = 2, col = "steelblue")
            lines(index, y - sd, lty = 2, col = "steelblue")
        }
        if (labels) {
            title(xlab = "Exceedances",
                ylab = paste("Shape:", substitute(x)))
            mtext("Threshold", side = 3, line = 3)
        }
        text = paste("ci =", ci, "| start =", start, "| end =", end )
        mtext(text, side = 4, adj = 0, cex = 0.7)
    }
    
    # Add Attributes:
    attr(mat, "control") = data.frame(cbind(ci = ci, 
        start = start, end = end), row.names = "")
    mat = t(mat)
    
    # Return Value:
    invisible(mat)
}


# ------------------------------------------------------------------------------


gpdqPlot =
function(x, p = 0.99, type = c("likelihood", "wald"), ci = 0.95, 
like.num = 50)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Adds Quantile Estimates to plot.gpd
    
    # Arguments:
    #   x  - an object of class 'gpdFit'
    #   pp - the probability level
    
    # FUNCTION:

    # Return Value:
    .gpd.q(x = x, pp = p, ci.type = type[1], ci.p = ci, like.num = like.num)
}


# ------------------------------------------------------------------------------


.gpd.q =  
function(x, pp, ci.type = c("likelihood", "wald"), ci.p = 0.95,
like.num = 50)
{
    #   x - a list object returned by 'plot.gpd' or 'tailplot'
    #   p - the desired probability for quantile estimate (e.g. 
    #       0.99 for the 99th percentile)
    #   type - method for calculating a confidence interval: 
    #       '"likelihood"' or '"wald"'
    #   ci - probability for confidence interval (must be less 
    #       than 0.999)
    #   like.num - number of times to evaluate profile likelihood


    if (x$dist != "gpd")
        stop("This function is used only with GPD curves")
    if (length(pp) > 1)
        stop("One probability at a time please")
        
    threshold = x$lastfit$threshold
    par.ests = x$lastfit$par.ests
    xihat = par.ests["xi"]
    betahat = par.ests["beta"]
    varcov = x$lastfit$varcov
    p.less.thresh = x$lastfit$p.less.thresh
    lambda = 1
    if (x$type == "tail") lambda = 1/(1 - p.less.thresh)
    a = lambda * (1 - pp)
    
    gfunc = function(a, xihat) (a^( - xihat) - 1) / xihat
    
    gfunc.deriv = function(a, xihat)
        ( - (a^( - xihat) - 1)/xihat - a^( - xihat) * log(a)) / xihat
    q = threshold + betahat * gfunc(a, xihat)
    if (q < x$plotmax) abline(v = q, lty = 2)
    out = as.numeric(q)
    ci.type = match.arg(ci.type)
    
    if(ci.type == "wald") {
        if(class(x$lastfit) != "gpd")
        stop("Wald method requires model be fitted with gpd (not pot)")
        scaling = threshold
        betahat = betahat/scaling
        xivar = varcov[1, 1]
            betavar = varcov[2, 2]/(scaling^2)
        covar = varcov[1, 2]/scaling
        term1 = betavar * (gfunc(a, xihat))^2
        term2 = xivar * (betahat^2) * (gfunc.deriv(a, xihat))^2
        term3 = 2 * covar * betavar * gfunc(a, xihat) * gfunc.deriv(a, xihat)
        qvar = term1 + term2 + term3
        if(qvar < 0) stop("Negative estimate of quantile variance")
        qse = scaling * sqrt(qvar)
        qq = qnorm(1 - (1 - ci.p)/2)
        upper = q + qse * qq
        lower = q - qse * qq
        if(upper < x$plotmax) abline(v = upper, lty = 2, col = 2)
        if(lower < x$plotmax) abline(v = lower, lty = 2, col = 2)
        out = as.numeric(c(lower, q, qse, upper))
        names(out) = c("Lower CI", "Estimate", "Std.Err", "Upper CI")
    }
    
    if (ci.type == "likelihood") {
        parloglik = function(theta, tmp, a, threshold, xpi) {
            beta = (theta * (xpi - threshold))/(a^( - theta) - 1)
            if((beta <= 0) || ((theta <= 0) && (max(tmp) > ( - beta/theta))))
                f = 1e+06
            else {
                y = log(1 + (theta * tmp)/beta)
            y = y/theta
            f = length(tmp) * log(beta) + (1 + theta) * sum(y)
            }
            f
        }
        theta = xihat
        parmax = NULL
        xp = exp(seq(from = log(threshold), to = log(x$plotmax),
                          length = like.num))
            excess = as.numeric(x$lastfit$data - threshold)
        for (i in 1:length(xp)) {
                fit2 = optim(theta, parloglik, method = "BFGS", hessian = FALSE,
                    tmp = excess, a = a, threshold = threshold, xpi = xp[i])
            parmax = rbind(parmax, fit2$value)
        }
        parmax =  - parmax
        overallmax =  - parloglik(xihat, excess, a, threshold, q)
        crit = overallmax - qchisq(0.999, 1)/2
        cond = parmax > crit
        xp = xp[cond]
        parmax = parmax[cond]
        par(new = T)
        dolog = ""
            if(x$alog == "xy" || x$alog == "x") dolog = "x"
        plot(xp, parmax, type = "n", xlab = "", ylab = "", axes = F,
             xlim = range(x$plotmin, x$plotmax),
             ylim = range(overallmax, crit), log = dolog)
        axis(4, at = overallmax - qchisq(c(0.95, 0.99), 1)/2,
                 labels = c("95", "99"), tick = TRUE)
        aalpha = qchisq(ci.p, 1)
        abline(h = overallmax - aalpha/2, lty = 2, col = 2)
        cond = !is.na(xp) & !is.na(parmax)
        smth = spline(xp[cond], parmax[cond], n = 200)
        lines(smth, lty = 2, col = 2)
        ci = smth$x[smth$y > overallmax - aalpha/2]
        out = c(min(ci), q, max(ci))
        names(out) = c("Lower CI", "Estimate", "Upper CI")
    }
    
    
    out
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
    .gpd.sfall(x = x, pp = pp, ci.p = ci.p, like.num = like.num)
}


# ------------------------------------------------------------------------------


.gpd.sfall =  
function(x, pp = 0.99, ci.p = 0.95, like.num = 50)
{
    # Arguments:
    #   x - a list object returned by 'plot.gpd' or 'tailplot'
    #   pp - the desired probability for expected shortfall 
    #       estimate (e.g. 0.99 for the 99th percentile)
    #   ci.p - probability for confidence interval (must be 
    #       less than 0.999)
    #   like.num - number of times to evaluate profile likelihood

    # data(danish); gpd.sfall(tailplot(gpd(danish, 10)), 0.99)



    if(x$dist != "gpd")
        stop("This function is used only with GPD curves")
    if(length(pp) > 1)
        stop("One probability at a time please")
      
    object = gpdFit(as.timeSeries(danishClaims), 10)  
    threshold = object@fit$threshold
    par.ests = object@fit$par.ests
    xihat = par.ests["xi"]
    betahat = par.ests["beta"]
    varcov = object@fit$varcov
    
    p.less.thresh = object@fit$prob
    lambda = 1
    
    # if (x$type == "tail") 
    		lambda = 1/(1 - p.less.thresh)
    a = lambda * (1 - pp)
    gfunc = function(a, xihat) (a^( - xihat) - 1) / xihat
    q = threshold + betahat * gfunc(a, xihat)
    s = q + (betahat + xihat * (q - threshold))/(1 - xihat)
    if (s < x$plotmax) abline(v = s, lty = 2)
    out = as.numeric(s)
    
    parloglik = function(theta, tmp, a, threshold, xpi)
    {
        beta = ((1 - theta) * (xpi - threshold)) /
            (((a^( - theta) - 1)/theta) + 1)
        if((beta <= 0) || ((theta <= 0) && (max(tmp) > ( - beta/theta)))) {
            f = 1e+06
        } else {
            y = log(1 + (theta * tmp)/beta)
            y = y/theta
            f = length(tmp) * log(beta) + (1 + theta) * sum(y)
        }
        f
    }
    
    theta = xihat
    parmax = NULL
    xp = exp(seq(from = log(threshold), to = log(x$plotmax),
		length = like.num))
    excess = as.numeric(x$lastfit$data - threshold)
    
    for (i in 1:length(xp)) {
        fit2 = optim(theta, parloglik, method = "BFGS", hessian = FALSE,
			tmp = excess, a = a, threshold = threshold, xpi = xp[i])
        parmax = rbind(parmax, fit2$value)
    }
    
    parmax =  - parmax
    overallmax =  - parloglik(xihat, excess, a, threshold, s)
    crit = overallmax - qchisq(0.999, 1)/2
    cond = parmax > crit
    xp = xp[cond]
    parmax = parmax[cond]
    par(new = TRUE)
    dolog = ""
    
    if(x$alog == "xy" || x$alog == "x") dolog = "x"
    
    plot(xp, parmax, type = "n", xlab = "", ylab = "", axes = FALSE, 
        xlim = range(x$plotmin, x$plotmax), 
        ylim = range(overallmax, crit), log = dolog)
    axis(4, at = overallmax - qchisq(c(0.95, 0.99), 1)/2,
        labels = c("95", "99"), tick = TRUE)
        
    aalpha = qchisq(ci.p, 1)
    abline(h = overallmax - aalpha/2, lty = 2, col = 2)
    cond = !is.na(xp) & !is.na(parmax)
    smth = spline(xp[cond], parmax[cond], n = 200)
    lines(smth, lty = 2, col = 2)
    ci = smth$x[smth$y > overallmax - aalpha/2]
    
    out = c(min(ci), s, max(ci))
    names(out) = c("Lower CI", "Estimate", "Upper CI")
    
    out
}


# ------------------------------------------------------------------------------


riskMeasures = 
function(object, prob = c(0.99, 0.995, 0.999, 0.9995, 0.9999))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates Quantiles and Expected Shortfalls
    
    # Arguments:
    #   x  - an object of class 'gpdFit'
    #   prob - a numeric value or vector of probability levels 
    
    # FUNCTION:
    
    # Settings:
    u = object@fit$threshold
    par.ests = object@fit$par.ests
    xi = par.ests["xi"]
    beta = par.ests["beta"]
    lambda = 1/(1 - object@fit$prob)
    
    # Quantile Risk:
    q = u + (beta * ((lambda * (1 - prob))^( - xi) - 1))/xi 

    # Sortfall Risk:  
    es = (q * (1 + (beta - xi * u)/q)) / (1 - xi) 
  
    # Risk Matrix:
    rtn = data.frame(p = prob, quantile = q, shortfall = es)
    
    # Return Value:
    rtn
}


################################################################################

