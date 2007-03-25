
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
# FUNCTION:                POT SERIES SIMULATION:
#  potSim                   Peaks over a threshold from arbitrary series
# FUNCTION:                POT PARAMETER ESTIMATION:
#  'fPOTFIT'                S4 Class Representation
#  potFit                   Fits with POT method
#   show.fPOTFIT            Print Method for object of class "fPOT"
#   plot.fPOTFIT             Print Method for object of class "fPOT"
#   .pot*Plot                Internal Plot Functions
#   summary.fPOTFIT          Summary Method for object of class "fPOT"
################################################################################


potSim = 
function(x, u = quantile(x, 0.95), run = 1)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulates a point process from a given time series object
    
    # Arguments:
    #   x - timSeries object
    #   u - threshold value
    #   run - cluster size
    
    # Example:
    #   z = potSim(x = as.timeSeries(data(daxRet))); plot(z)
    #   z = potSim(x = as.timeSeries(data(daxRet)), run = 20); plot(z)
    
    # FUNCTION:
    
    # Transform into a timeSeries Object:
    x = as.timeSeries(x)
    
    # Simulated POT Process:
    ans = pointProcess(x = x, u = u)
    
    # Optionally Decluster:
    if (run > 1) ans = deCluster(ans, run = run)
    
    # Add Control:
    attr(ans, "control") = 
        data.frame(u = u, q = names(u), run = run, row.names = "")   
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


setClass("fPOTFIT", 
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


potFit = 
function(x, u = quantile(x, 0.95), run = 1, title = NULL, 
description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Save Call:
    call = match.call()
    
    # Prepare Series:
    n = length(x@Data) 
    span = as.numeric(end(x) - start(x))
    X = pointProcess(x = x, u = u)
    if (run > 1) X = deCluster(x = X, run = run)
    
    # Statistics:
    nX = length(X@Data)
    meanX = mean(X@Data)-u
    varX = var(X@Data)

    # Start Solution:
    shape0 = -0.5*(((meanX^2)/varX)-1)
    scale0 = 0.5*meanX*(((meanX^2)/varX)+1) /
        (1+shape0*((nX/span)^(-shape0)-1)/shape0)
    loc0 = 0
    theta = c(shape0, scale0, loc0)

    # Fit Parameter:
    fit = optim(theta, .potLLH.evir, hessian = TRUE,  
        exceedances = X@Data, threshold = u, span = span)
        
    # Output:
    fit$call = call
    fit$type = c("pot", "mle")
    # fit$span = span
    # fit$n.exceed = nX   
    fit$par.ests = fit$par
    fit$par.ses = sqrt(diag(solve(fit$hessian)))   
    fit$beta = fit$par.ests[2] + fit$par.ests[1] * (u-fit$par.ests[3])
    fit$par.ests = c(fit$par.ests, fit$beta)
    fit$llh = fit$value
    names(fit$par.ests) = c("xi", "sigma", "mu", "beta")
    names(fit$par.ses) = c("xi", "sigma", "mu")
    
    # Compute Residuals:
    xi = fit$par.ests[1]
    beta = fit$par.ests[4]
    residuals = as.vector(log(1 + (xi * (as.vector(X) - u))/beta)/xi)
    # fit$fitted.values = as.vector(X) - fit$residuals     
    
    # Add title and description:
    if (is.null(title)) title = "POT Parameter Estimation"
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    new("fPOTFIT",
        call = match.call(),
        method = fit$type,
        parameter = list(u = u, run = run),
        data = list(x = x, exceedances = X),
        fit = fit,
        residuals = residuals,
        title = title,
        description = description)
}


# ------------------------------------------------------------------------------



show.fPOTFIT =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print Method for object of class "fPOTFIT"
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n" , x@title, "\n")
    
    # Function Call:
    cat("\nCall:\n ")
    cat(paste(deparse(x@call), sep = "\n", collapse = "\n"), "\n", sep = "")
            
    # Point Process and Decluster Parameters:
    parameter = cbind(u = x@parameter$u, run = x@parameter$run)
    rownames(parameter) = paste("", rownames(parameter))
    cat("\nModel Parameters:\n")
    print(parameter)
    
    # Estimation Type:
    cat("\nEstimation Method:\n", x@method, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(x@fit$par.ests)

    # Desription:
    cat("\nDescription\n", x@description, "\n\n")
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


plot.fPOTFIT =
function(x, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for objects of class "fPOTFIT".

    # FUNCTION:
    
    # Plot functions:   
    plot.8 <<- function (x, ...) { 
        if (which == "ask") {
            fit = x@fit
            plot.fGPDFIT(fit)
            plot.fPOTFIT(x, which = "ask") } 
    }

    # Plot:
    interactivePlot(
        x,
        choices = c(
            "Point Process of Exceedances", 
            "Scatterplot of Gaps", 
            "QQ-Plot of Gaps", 
            "ACF of Gaps", 
            "Scatterplot of Residuals", 
            "QQ-Plot of Residuals", 
            "ACF of Residuals",
            "GOTO GPD Plots"),
        plotFUN = c(
            ".pot1Plot", 
            ".pot2Plot", 
            ".pot3Plot",
            ".pot4Plot", 
            ".pot5Plot", 
            ".pot6Plot",
            ".pot7Plot",
            "plot.8"),
        which = which)
                    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


.pot1Plot =
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Point Process of Exceedances   
    
    # FUNCTION:
    
    # Data:
    exceedances = x@data$exceedances
    
    # Labels:
    if (labels) {
        main = "Point Process of Exceedances"
        xlab = "Index"
        ylab = "Exceedances"
    } else {
        main = xlab = ylab = ""
    }
              
    # Plot:
    plot(exceedances, type = "h", 
        main = main, xlab = xlab, ylab = ylab, 
        col = "steelblue", ...) 
    
    # Addon:
    if (labels) {
        u = signif (x@parameter$u, 3)
        run = x@parameter$run
        text = paste("u =", u, "| run =", run)
        mtext(text, side = 4, adj = 0, cex = 0.7)
        grid()
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.pot2Plot =
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Scatterplot of Gaps  
    
    # FUNCTION:
    
    # Data:
    exceedances = x@data$exceedances
    gaps = as.numeric(diff(seriesPositions(exceedances))) / (24*3600)
    
    # Labels:
    if (labels) {
        main = "Scatterplot of Gaps"
        xlab = "Ordering"
        ylab = "Gap Lengths"
    } else {
        main = xlab = ylab = ""
    }
    
    # Plot:
    plot(gaps, 
        main = main, xlab = xlab, ylab = ylab, 
        pch = 19, col = "steelblue", ...)
    lines(lowess(1:length(gaps), gaps), col = "brown")
    
    # Addon:
    if (labels) {
        u = signif (x@parameter$u, 3)
        run = x@parameter$run
        text = paste("u =", u, "| run =", run)
        mtext(text, side = 4, adj = 0, cex = 0.7)
        grid()
    }
    
    # Return Value:
    invisible()
} 


# ------------------------------------------------------------------------------


.pot3Plot =
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Quantile Plot of Gaps
    
    # FUNCTION:
    
    # Data:
    exceedances = x@data$exceedances
    gaps = as.numeric(diff(seriesPositions(exceedances))) / (24*3600)
    sorted = sort(gaps)
    y <- qexp(ppoints(gaps))
    
    # Labels:
    if (labels) {
        main = "Quantile Plot of Gaps"
        xlab = "Ordered Data"
        ylab = "Exponential Quantiles"
    } else {
        main = xlab = ylab = ""
    }
    
    # Plot:
    plot(x = sorted, y = y, 
        main = main, xlab = xlab, ylab = ylab,
        pch = 19, col = "steelblue", ...)   
    abline(lsfit(sorted, y))
    
    # Addon:
    if (labels) {
        u = signif (x@parameter$u, 3)
        run = x@parameter$run
        text = paste("u =", u, "| run =", run)
        mtext(text, side = 4, adj = 0, cex = 0.7)
        grid()
    }
    
    # Return Value:
    invisible()
}  


# ------------------------------------------------------------------------------


.pot4Plot =
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   ACF Plot of Gaps
    
    # FUNCTION:
    
    # Data:
    exceedances = x@data$exceedances
    gaps = as.numeric(diff(seriesPositions(exceedances))) / (24*3600)
    sorted = sort(gaps)
    y <- qexp(ppoints(gaps))
    
    # Labels:
    if (labels) {
        main = "ACF Plot of Gaps"
        xlab = "Lag"
        ylab = "ACF"
    } else {
        main = xlab = ylab = ""
    }
    
    # Plot:
    acf(gaps, main = main, xlab = xlab, ylab = ylab,
        col = "steelblue", ...) 
    # Addon:
    if (labels) {
        u = signif (x@parameter$u, 3)
        run = x@parameter$run
        text = paste("u =", u, "| run =", run)
        mtext(text, side = 4, adj = 0, cex = 0.7)
        # grid()
    }
    
    # Return Value:
    invisible()
}        


# ------------------------------------------------------------------------------


.pot5Plot =
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   ACF Plot of Gaps
    
    # FUNCTION:
    
    # Data:
    residuals = x@residuals
    
    # Labels:
    if (labels) {
        main = "Scatterplot of Residuals"
        xlab = "Ordering"
        ylab = "Residuals"
    } else {
        main = xlab = ylab = ""
    }
    
    # Plot:
    plot(residuals, 
        main = main, xlab = xlab, ylab = ylab,
        pch = 19, col = "steelblue", ...)   
    lines(lowess(1:length(residuals), residuals), col = "brown") 
    
    # Addon:
    if (labels) {
        u = signif (x@parameter$u, 3)
        run = x@parameter$run
        text = paste("u =", u, "| run =", run)
        mtext(text, side = 4, adj = 0, cex = 0.7)
        grid()
    }
    
    # Return Value:
    invisible()
}   


# ------------------------------------------------------------------------------


.pot6Plot =
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Quantile Plot of Residuals
    
    # FUNCTION:
    
    # Data:
    residuals = x@residuals
    sorted = sort(residuals)
    y <- qexp(ppoints(residuals))
    
    # Labels:
    if (labels) {
        main = "Quantile Plot of Residuals"
        xlab = "Ordered Data"
        ylab = "Exponential Quantiles"
    } else {
        main = xlab = ylab = ""
    }
    
    # Plot:
    plot(x = sorted, y = y,
        main = main, xlab = xlab, ylab = ylab,
        pch = 19, col = "steelblue", ...)   
    abline(lsfit(sorted, y), col = "brown")
    
    # Addon:
    if (labels) {
        u = signif (x@parameter$u, 3)
        run = x@parameter$run
        text = paste("u =", u, "| run =", run)
        mtext(text, side = 4, adj = 0, cex = 0.7)
        grid()
    }
    
    # Return Value:
    invisible()
}      


# ------------------------------------------------------------------------------


.pot7Plot =
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   ACF Plot of Residuals
    
    # FUNCTION:
    
    # Data:
    residuals = x@residuals
    
    # Labels:
    if (labels) {
        main = "ACF Plot of Residuals"
        xlab = "Lag"
        ylab = "ACF"
    } else {
        main = xlab = ylab = ""
    }
    
    # Plot:
    acf(residuals, main = main, xlab = xlab, ylab = ylab,
        col = "steelblue", ...) 
    # Addon:
    if (labels) {
        u = signif (x@parameter$u, 3)
        run = x@parameter$run
        text = paste("u =", u, "| run =", run)
        mtext(text, side = 4, adj = 0, cex = 0.7)
        # grid()
    }
    
    # Return Value:
    invisible()
}        
    

# ------------------------------------------------------------------------------


summary.fPOTFIT =
function(object, doplot = TRUE, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Summary Method for object of class "fPOTFIT"
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n" , object@title, "\n")
    
    # Function Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", 
        collapse = "\n"), "\n", sep = "")
            
    # Estimation Type:
    cat("\nEstimation Method:\n", object@method, "\n") 
    
    # Point Process and Decluster Parameters:
    parameter = cbind(u = object@parameter$u, run = object@parameter$run)
    rownames(parameter) = paste("", rownames(parameter))
    cat("\nModel Parameters:\n")
    print(parameter)
      
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(object@fit$par.ests)
    
    # Summary:
    cat("\nStandard Deviations:\n")
    print(object@fit$par.ses)
    cat("\nLog-Likelihood Value:\n ", object@fit$llh)
    cat("\n\nType of Convergence:\n ", object@fit$convergence) 
    
    # Desription:
    cat("\n\nDescription\n ", object@description, "\n\n")
    
    # Plot:
    if (doplot) {
        plot(object, which = which, ...)
    }
    
    # Return Value:
    invisible(object)
}


################################################################################
# COPY FROM EVIR


.pot.evir = 
function(data, threshold = NA, nextremes = NA, run = NA, picture = TRUE, ...)
{   # A copy from R package evir

    # FUNCTION:
    
    n = length(as.numeric(data))
    times = attributes(data)$times
    if (is.null(times)) {
        times = 1:n
        attributes(data)$times = times
        start = 1
        end = n
        span = end - start
    } else {
        start = times[1]
        end = times[n]
        span = as.numeric(difftime(as.POSIXlt(times)[n],
            as.POSIXlt(times)[1], units = "days"))
    }
    if (is.na(nextremes) && is.na(threshold)) {
        stop("Enter either a threshold or the number of upper extremes")
    }
    if (!is.na(nextremes) && !is.na(threshold)) {
        stop("Enter EITHER a threshold or the number of upper extremes")
    }
    if (!is.na(nextremes)) {
        threshold = findthresh(as.numeric(data), nextremes)
    }
    if (threshold > 10) {
        factor = 10^(floor(log10(threshold)))
        cat(paste("If singularity problems occur divide data",
            "by a factor, perhaps", factor, "\n"))
    }
    exceedances.its = structure(data[data > threshold], times =
        times[data > threshold])
    n.exceed = length(as.numeric(exceedances.its))
    p.less.thresh = 1 - n.exceed/n
    if (!is.na(run)) {
        exceedances.its = decluster(exceedances.its, run, picture)
        n.exceed = length(exceedances.its)
    }
    intensity = n.exceed/span
    exceedances = as.numeric(exceedances.its)
    xbar = mean(exceedances) - threshold
    s2 = var(exceedances)
    shape0 = -0.5 * (((xbar * xbar)/s2) - 1)
    extra = ((length(exceedances)/span)^( - shape0) - 1)/shape0
    betahat = 0.5 * xbar * (((xbar * xbar)/s2) + 1)
    scale0 = betahat/(1 + shape0 * extra)
    loc0 = 0
    theta = c(shape0, scale0, loc0)
    fit = optim(theta, .potLLH.evir, hessian = TRUE, ..., 
        exceedances = exceedances, threshold = threshold, span = span)
    if (fit$convergence) {
        warning("optimization may not have succeeded")
    }
    par.ests = fit$par
    varcov = solve(fit$hessian)
    par.ses = sqrt(diag(varcov))   
    beta = par.ests[2] + par.ests[1] * (threshold - par.ests[3])
    par.ests = c(par.ests, beta)
    out = list(n = length(data), period = c(start, end), data = 
        exceedances.its, span = span, threshold = threshold,
        p.less.thresh = p.less.thresh, n.exceed = n.exceed, run = run,
        par.ests = par.ests, par.ses = par.ses, varcov = varcov, 
        intensity = intensity, nllh.final = fit$value, 
        converged = fit$convergence)
    names(out$par.ests) = c("xi", "sigma", "mu", "beta")
    names(out$par.ses) = c("xi", "sigma", "mu")
    class(out) = "potd"
    out
}


# ------------------------------------------------------------------------------


.potLLH.evir = 
function(theta, exceedances, threshold, span)
{   # A copy from R package evir

    # FUNCTION:
    
    test = (theta[2] <= 0) || (min(1 + (theta[1] * 
        (exceedances - theta[3])) / theta[2]) <= 0)
    if (test) {
        f = 1.0e+6
    } else {
        y = logb(1 + (theta[1] * (exceedances - theta[3])) / theta[2])
        term3 = (1/theta[1] + 1) * sum(y)
        term1 = span * (1 + (theta[1] * (threshold - theta[3])) /
            theta[2])^(-1/theta[1])
        term2 = length(y) * logb(theta[2])
        f = term1 + term2 + term3
    }
    f
}


################################################################################

