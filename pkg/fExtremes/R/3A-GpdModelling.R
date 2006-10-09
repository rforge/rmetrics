
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
# FUNCTION:              GPD MODELLING FROM EVIS:
#  gpdSim                 Simulates GPD rvs
#  fGPDFIT                   S4 Class Representation
#  gpdFit                 Fits GPD Distribution
#   print.gpd              Print Method for object of class "gpd"
#   plot.gpd               Plot Method for object of class "gpd"
#   summary.gpd            Summary Method for object of class "gpd"
# FUNCTION:              ADDITIONAL PLOTS:
#  gpdTailPlot            Plots Tail Estimate From GPD Model
#  gpdQuantPlot           Plots of GPD Tail Estimate of a High Quantile
#  gpdShapePlot           Plots for GPD Shape Parameter
#  gpdQPlot               Adds Quantile Estimates to plot.gpd
#  gpdSfallPlot           Adds Expected Shortfall Estimates to a GPD Plot
# FUNCTION:              ADDITIONAL FUNCTION:
#  gpdriskMeasures        Calculates Quantiles and Expected Shortfalls
################################################################################


################################################################################


gpdSim = 
function(model = list(xi = 0.25, mu = 0, beta = 1), n = 1000, 
seed = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates random variates from a GPD distribution
    
    # FUNCTION:
    
    # Seed:
    if (is.null(seed)) seed = NA else set.seed(seed)
    
    # Simulate:
    ans = rgpd(n = n, xi = model$xi, mu = model$mu, beta = model$beta)  
    ans = as.ts(ans)

    # Control:
    attr(ans, "control") = 
        data.frame(t(unlist(model)), seed = seed, row.names = "")
        
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


setClass("fGPDFIT", 
    representation(
        call = "call",
        method = "character",
        parameter = "list",
        data = "list",
        fit = "list",
        residuals = "numeric",
        title = "character",
        description = "character"
    )  
)


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
    
    # Settings:
    call = match.call()
    type = match.arg(type)
    information = match.arg(information)
    
    # Check Type and Convert:
    X = x  
    xClass = class(x)
    x = as.timeSeries(x)
    stopifnot(isUnivariate(x))
    
    # Compute Exceedances:
    exceedances = x[x@Data > u]
    if (xClass == "numeric") {
        Names = as.character((1:dim(x)[1])[x@Data > u])
        exceedances = as.vector(exceedances)
        names(exceedances) = Names
    }
    if (xClass == "ts") {
        Names = as.character((1:dim(x)[1])[x@Data > u])
        exceedances = as.ts(as.vector(exceedances))
        names(exceedances) = Names
    }

    # Estimate Parameters:
    x = as.vector(x)
    if (type == "mle") {
        fit = .gpdmleFit(x, u, information)
        fit$llh = fit$fit$value
        fit$convergence = fit$fit$convergence
    } else if (type == "pwm") {
        fit = .gpdpwmFit(x, u)
        fit$llh = NA
        fit$convergence = NA
    }
    fit$prob = 1 - length(x[x > u]) / length(x)
   
    # Compute Residuals:
    xi = fit$par.ests["xi"]
    beta = fit$par.ests["beta"]
    residuals = log(1 + (xi * (as.vector(exceedances)-u))/beta)/xi
    
    # Add title and description:
    if (is.null(title)) title = "GPD Parameter Estimation"
    if (is.null(description)) description = as.character(date())
    
    # Compose Parameter List:
    parameter = list(u = u, type = type)
    if (information == "mle") parameter$information = information
    
    # Return Value:
    new("fGPDFIT",
        call = call,
        method = c("gpd", type), 
        parameter = parameter,
        data = list(x = X, exceedances = exceedances),
        fit = fit,
        residuals = residuals,
        title = title,
        description = description)
}


# ------------------------------------------------------------------------------


.gpdmleFit = 
function(x, u = quantile(x, 0.95), 
information = c("observed", "expected"), ...) 
{   # A function implemented by Diethelm Wuertz

    x = as.vector(x)
    excess = x[x > u] - u
    theta = .gpdpwmFit(x = x, u = u)$par.ests
    
    # Parameter Estimation:
    fit = optim(theta, .gpdLLH, hessian = TRUE, excess = excess)
    names(fit$par) = c("xi", "beta")
    
    # Error Estimates:
    if (information[1] == "observed") {
        varcov = solve(fit$hessian)
    }
    if (information[1] == "expected") {
        Nu = length(excess)
        one = (1 + fit$par[1])^2/Nu
        two = (2 * (1 + fit$par[1]) * fit$par[2]^2)/Nu
        cov = -((1 + fit$par[1]) * fit$par[2])/Nu
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
        func = NA
    } else {
        y = log(1+(xi*excess)/beta) / xi
        func = length(excess) * log(beta) + (1+xi)*sum(y)
    }
    func
}


# ------------------------------------------------------------------------------


.gpdpwmFit = 
function(x, u = quantile(x, 0.95)) 
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
    par.ses = c(xi = NA, beta = NA)
    list(par.ests = par.ests, par.ses = par.ses, fit = NA, varcov = NA)
}


# ------------------------------------------------------------------------------


print.fGPDFIT =
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
    cat("\nEstimated Parameters:\n")
    print(x@fit$par.ests)
    
    # Desription:
    cat("\nDescription\n ", x@description, "\n\n")
    
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


plot.fGPDFIT =
function(x, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for objects of class 'fGPDFIT'

    # Example:
    #   x = as.timeSeries(danishClaims); plot(gpdFit(x, 4), "ask")
    
    # FUNCTION:
            
    # Plot:
    interactivePlot(
        x = x,
        choices = c(
            "Excess Distribution",
            "Tail of Underlying Distribution",
            "Scatterplot of Residuals", 
            "QQ-Plot of Residuals"),
        plotFUN = c(
            ".gpd1Plot", 
            ".gpd2Plot", 
            ".gpd3Plot",
            ".gpd4Plot"),
        which = which)
                    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


.gpd1Plot =
function(x, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Empirical Distribution Plot:
    
    # Arguments:
    #   x - an object of class fGPDFIT
    #   labels - a logical flag. Should labels be printed?
    
    # FUNCTION:
    
    # Data:
    extend = 1.5
    u = x@parameter$u
    data = as.vector(x@data$exceedances)
    sorted = sort(data)
    shape = xi = x@fit$par.ests["xi"]
    scale = beta = x@fit$par.est["beta"]
    ypoints = ppoints(sorted)
    U = max(sorted)*extend
    z = qgpd(seq(0, 1, length = 1000), xi, u, beta)
    z = pmax(pmin(z, U), u)
    y = pgpd(z, xi, u, beta) 
    
    # Labels:
    if (labels) {
        xlab = "Fu(x-u)"
        ylab = "x [log Scale]"
        main = "Excess Distribution"
    } else {
        xlab = ylab = main = ""
    }
    
    # Plot:
    plot(x = sorted, y = ypoints, 
        xlim = range(u, U), ylim = range(ypoints, y, na.rm = TRUE), 
        main = main, xlab = xlab, ylab = ylab, 
        log = "x", axes = TRUE, 
        col = "steelblue", pch = 19, ...)
    lines(z[y >= 0], y[y >= 0], col = "brown") 
    
    # Addon:
    if (labels) {
        u = signif (x@parameter$u, 3)
        text = paste("u =", u)
        mtext(text, side = 4, adj = 0, cex = 0.7)
        grid()
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.gpd2Plot =
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tail of Underlying DistributioN
    
    # Arguments:
    #   x - an object of class fGPDFIT
    #   labels - a logical flag. Should labels be printed?
    
    # FUNCTION:
    
    # Settings:
    extend = 1.5
    u = x@parameter$u
    data = as.vector(x@data$x)
    sorted = sort(data[data > u])
    prob = x@fit$prob
    shape = xi = x@fit$par.ests["xi"]
    beta = x@fit$par.ests["beta"]
    scale = beta * (1-prob)^xi
    location = u - (scale*((1 - prob)^(-xi)-1))/xi

    # Labels:
    if (labels) {
        xlab = "x [log scale]"
        ylab = "1-F(x) [log scale]"
        main = "Tail of Underlying Distribution"
    } else {
        xlab = ylab = main = ""
    }
    
    # Plot:
    U = max(data) * extend
    ypoints = ppoints(sorted)
    ypoints = (1 - prob) * (1 - ypoints)
    z = qgpd(seq(0, 1, length = 1000), xi, u, beta)
    z = pmax(pmin(z, U), u)
    y = pgpd(z, xi, u, beta)   
    y = (1 - prob) * (1 - y)
    plot(x = sorted, y = ypoints, 
        xlim = range(u, U), ylim = range(ypoints, y, na.rm = TRUE), 
        main = main, xlab = xlab, ylab = ylab, 
        log = "xy", axes = TRUE, 
        col = "steelblue", pch = 19, ...)
        
    # Line:
    lines(z[y >= 0], y[y >= 0], col = "brown")    
    if (labels) grid()
    
    # Return Value:
    invisible(list(x = sorted, y = ypoints))
}


# ------------------------------------------------------------------------------


.gpd3Plot =
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Scatterplot of GPD Residuals.
    
    # Arguments:
    #   x - an object of class fGPDFIT
    #   labels - a logical flag. Should labels be printed?
    
    # FUNCTION:
    
    # Residuals:
    residuals = x@residuals
    
    # Labels:
    if (labels) {
        ylab = "Residuals"
        xlab = "Ordering"
        main = "Scatterplot of Residuals"
    } else {
        xlab = ylab = main = ""
    }
    
    # Plot:
    plot(residuals, 
       main = main, ylab = ylab, xlab = xlab, 
       col = "steelblue", pch = 19, ...)
    lines(lowess(1:length(residuals), residuals), col = "brown") 
    if (labels) grid()
    
    # Return Value:
    invisible(list(x = 1:(length(residuals)), y = residuals))
}


# ------------------------------------------------------------------------------


.gpd4Plot = 
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   QQ Plot of GPD Residuals.
    
    # Arguments:
    #   x - an object of class fGPDFIT
    #   labels - a logical flag. Should labels be printed?
    
    # FUNCTION:
    
    # Data:
    data = x@residuals
    sorted = sort(data)
    
    # Labels:
    if (labels) {
        xlab = "Ordered Data"
        ylab = "Exponential Quantiles"
        main = "QQ-Plot of Residuals"
    } else {
        xlab = ylab = main = ""
    }
    
    # Plot:
    y = qexp(ppoints(data))
    plot(x = sorted, y = y, 
        main = main, xlab = xlab, ylab = ylab, 
        col = "steelblue", pch = 19, ...)
    abline(lsfit(sorted, y), col = "brown")
    if (labels) grid()
    
    # Return Value:
    invisible(list(x = sorted, y = y))
}


# ------------------------------------------------------------------------------


summary.fGPDFIT = 
function(object, doplot = TRUE, which = "all", ...) 
{   # A function written by Diethelm Wuertz

    # Description:
    #   Summary method for objects of class "gpdFit"
    
    # FUNCTION:

    # Title:
    cat("\nTitle:\n" , object@title, "\n")
    
    # Function Call:
    cat("\nCall:\n")
    cat(paste(deparse(object@call), sep = "\n", 
        collapse = "\n"), "\n", sep = "") 
            
    # Estimation Type:
    cat("\nEstimation Type:\n ", object@method, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(object@fit$par.ests)
    
    # Summary - For MLE print additionally:
    if (object@method[2] == "mle") {
        cat("\nStandard Deviations:\n"); print(object@fit$par.ses)
        if (!is.na(object@fit$llh))
            cat("\nLog-Likelihood Value:\n ", object@fit$llh, "\n")
        if (!is.na(object@fit$convergence))
            cat("\nType of Convergence:\n ", object@fit$convergence, "\n") 
    }
    
    # Plot:
    if (doplot) {
        plot(object, which = which, ...)
    }
    
    # Desription:
    cat("\nDescription\n ", object@description, "\n\n")
    
    # Return Value:
    invisible(object)
}


################################################################################


gpdTailPlot =
function(object, plottype = c("xy", "x", "y", ""), extend = 1.5, 
labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots Tail Estimate From GPD Model
    
    # Note:
    #   Code partly copied from R package evir
    
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
         pch = 19, xlab = "", ylab = "", log = plottype[1], axes = TRUE, ...)
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
        title(main = "Tail Estimate Plot")
    }
    
    # Result:
    ans = list(lastfit = x, type = "tail", dist = "gpd",
        plotmin = plotmin, plotmax = plotmax, alog = plottype[1], 
        location = location, shape = shape, scale = scale)
    
    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


gpdQuantPlot =
function(x, p = 0.99, ci = 0.95, models = 30, start = 15, end = 500,
doplot = TRUE, plottype = c("normal", "reverse"), labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots of GPD Tail Estimate of a High Quantile
    
    # Example:
    #   Danish = as.timeSeries(data(danishClaims))
    #   gpdquantPlot(Danish)
    
    # Note:
    #   Code partly copied from R package evir
    
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
    
    # Return Value:
    invisible(mat)
}


# ------------------------------------------------------------------------------


gpdShapePlot =
function(x, ci = 0.95, models = 30, start = 15, end = 500,  
doplot = TRUE, plottype = c("normal", "reverse"), labels = TRUE, ...)
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


gpdQPlot =
function(x, p = 0.99, ci = 0.95, type = c("likelihood", "wald"), 
like.num = 50)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Adds Quantile Estimates to plot.gpd
    
    # Arguments:
    #   x - a list object returned by 'plot.gpd' or 'tailplot'
    #   p - the desired probability for quantile estimate (e.g. 
    #       0.99 for the 99th percentile)
    #   type - method for calculating a confidence interval: 
    #       '"likelihood"' or '"wald"'
    #   ci - probability for confidence interval (must be less 
    #       than 0.999)
    #   like.num - number of times to evaluate profile likelihood
    
    # Note:
    #   A copy from contributed R package evir.
    
    # FUNCTION:

    # Check:
    if (x$dist != "gpd")
        stop("This function is used only with GPD curves")
    if (length(p) > 1)
        stop("One probability at a time please")
      
    # Threshold: 
    threshold = x$lastfit$threshold
    par.ests = x$lastfit$par.ests
    xihat = par.ests["xi"]
    betahat = par.ests["beta"]
    varcov = x$lastfit$varcov
    p.less.thresh = x$lastfit$p.less.thresh
    lambda = 1
    if (x$type == "tail") lambda = 1/(1 - p.less.thresh)
    a = lambda * (1-p)
    
    # Internal Functions:
    gfunc = 
    function(a, xihat) {
        (a^(-xihat)-1) / xihat
    }
    gfunc.deriv = 
    function(a, xihat) {
        (-(a^(-xihat)-1)/xihat-a^(-xihat)*log(a)) / xihat
    }
    
    q = threshold + betahat * gfunc(a, xihat)
    if (q < x$plotmax) abline(v = q, lty = 2)
    out = as.numeric(q)
    
    # Type:
    type = match.arg(type)
    
    # Wald:
    if (type == "wald") {
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
        qq = qnorm(1 - (1 - ci)/2)
        upper = q + qse * qq
        lower = q - qse * qq
        if(upper < x$plotmax) abline(v = upper, lty = 2, col = 2)
        if(lower < x$plotmax) abline(v = lower, lty = 2, col = 2)
        out = as.numeric(c(lower, q, qse, upper))
        names(out) = c("Lower CI", "Estimate", "Std.Err", "Upper CI")
    }
    
    # Likelihood:
    if (type == "likelihood") {
        parloglik = 
        function(theta, tmp, a, threshold, xpi) {
            beta = (theta * (xpi - threshold))/(a^( - theta) - 1)
            if ((beta <= 0) || ((theta <= 0) && (max(tmp) > ( - beta/theta)))) {
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
        aalpha = qchisq(ci, 1)
        abline(h = overallmax - aalpha/2, lty = 2, col = 2)
        cond = !is.na(xp) & !is.na(parmax)
        smth = spline(xp[cond], parmax[cond], n = 200)
        lines(smth, lty = 2, col = 2)
        ci = smth$x[smth$y > overallmax - aalpha/2]
        out = c(min(ci), q, max(ci))
        names(out) = c("Lower CI", "Estimate", "Upper CI")
    }
    
    # Return Value:
    out
}


# ------------------------------------------------------------------------------


gpdSfallPlot =
function(x, p = 0.99, ci = 0.95, like.num = 50)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Adds Expected Shortfall Estimates to a GPD Plot 
    
    # Arguments:
    #   x  - an object of class 'gpdFit'
    #   pp - the probability level
    
    # FUNCTION:

    # Return Value:
    .gpd.sfall(x = x, pp = p, ci.p = ci, like.num = like.num)
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
    
    # Return Value:
    out
}


# ------------------------------------------------------------------------------


gpdRiskMeasures = 
function(object, prob = c(0.99, 0.995, 0.999, 0.9995, 0.9999))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates Quantiles and Expected Shortfalls
    
    # Arguments:
    #   x  - an object of class 'gpdFit'
    #   prob - a numeric value or vector of probability levels 
    
    # FUNCTION:
    
    # Settings:
    u = object@parameter$u
    par.ests = object@fit$par.ests
    xi = par.ests["xi"]
    beta = par.ests["beta"]
    lambda = 1/(1 - object@fit$prob)
    
    # Quantile Risk:
    q = u + (beta * ((lambda * (1 - prob))^( - xi) - 1))/xi 

    # Shortfall Risk:  
    es = (q * (1 + (beta - xi * u)/q)) / (1 - xi) 
  
    # Risk Matrix:
    ans = data.frame(p = prob, quantile = q, shortfall = es)
    
    # Return Value:
    ans
}


################################################################################

