
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
# FUNCTION:                POT MODELLING FROM EVIS:
#  potSim                   Peaks over a threshold from arbitrary series
#  fPOT                     S4 Class Representation
#  potFit                   Fits with POT method
#   print.fPOT               Print Method for object of class "fPOT"
#   plot.fPOT                Print Method for object of class "fPOT"
#   summary.fPOT             Summary Method for object of class "fPOT"
################################################################################
#
################################################################################



potSim = 
function(x, u = quantile(x, 0.95), run = NULL, column = 1)
{   # A function implemented by Diethelm Wuertz

    # Example:
    #   z = potSim(x = as.timeSeries(data(daxRet))); plot(z)
    
    # Simulated POT Process:
    ans = pointProcess(x = x, u = u, column = column)
    
    # Optionally Decluster:
    if (!is.null(run)) ans = deCluster(ans, run = run)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.old.potSim = 
function(x, threshold, nextremes = NA, run = NA)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates from an arbitray rvs sequence a series with
    #   the peaks over a threshold
    
    # Settings:
    data = x
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
        span = as.numeric(difftime(as.POSIXlt(times)[n], as.POSIXlt(times)[1], 
            units = "days"))
    }
            
    if (is.na(nextremes) && is.na(threshold)) 
        stop("Enter either a threshold or the number of upper extremes")
    if (!is.na(nextremes) && !is.na(threshold)) 
        stop("Enter EITHER a threshold or the number of upper extremes")
    if (!is.na(nextremes)) 
        threshold = findthresh(as.numeric(data), nextremes)
    if (threshold > 10) {
        factor = 10^(floor(log10(threshold)))
        cat(paste("If singularity problems occur divide data", 
            "by a factor, perhaps", factor, "\n")) }
            
    exceedances.its = structure(data[data > threshold], times = times[data > 
        threshold])
    n.exceed = length(as.numeric(exceedances.its))
    p.less.thresh = 1 - n.exceed/n
    if (!is.na(run)) {
        exceedances.its = decluster(exceedances.its, run, picture)
        n.exceed = length(exceedances.its) }
    intensity = n.exceed/span
    exceedances = as.numeric(exceedances.its)
    
    # Return Value:
    exceedances
}


# ------------------------------------------------------------------------------


setClass("fPOT", 
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


potFit = 
function(x, u = quantile(x, 0.95), run = NULL, title = NULL, description = NULL)
{
    # FUNCTION:
    
    # Save Call:
    call = match.call()
    
    # Prepare Series:
    n = length(x@Data) 
    span = as.numeric(end(x) - start(x))
    X = .pointProcess(x = x, u = u)
    if (!is.null(run)) X = .deCluster(x = X, run = run)
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
    fitted = optim(theta, .potLLH.evir, hessian = TRUE,  
        exceedances = X@Data, threshold = u, span = span)
    fit = list()
    fit$fit = fitted
    fit$call = call
    fit$type = c("pot", "mle")
    fit$span = span
    fit$threshold = u
    fit$n.exceed = nX   
    fit$fit$run = run
    fit$par.ests = fitted$par
    fit$par.ses = sqrt(diag(solve(fitted$hessian)))   
    fit$beta = fit$par.ests[2] + fit$par.ests[1] * (u-fit$par.ests[3])
    fit$par.ests = c(fit$par.ests, fit$beta)
    fit$nllh.final = fitted$value
    names(fit$par.ests) = c("xi", "sigma", "mu", "beta")
    names(fit$par.ses) = c("xi", "sigma", "mu")
    
    # Compute Residuals:
    xi = fit$par.ests[1]
    beta = fit$par.ests[4]
    threshold = fit$threshold 
    fit$residuals = as.vector(log(1 + (xi * (as.vector(X) - u))/beta)/xi)
    fit$fitted.values = as.vector(X) - fit$residuals
    fit$llh = fitted$nllh.final
    fit$converged = fitted$converged       
    
    # Add title and description:
    if (is.null(title)) title = "POT Parameter Estimation"
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    new("fPOT",
        call = match.call(),
        data = list(x = X),
        method = fit$type,
        fit = fit,
        title = title,
        description = description)
}


# ------------------------------------------------------------------------------


.old.potFit =
function(x, threshold = NA, nextremes = NA, run = NA, title = NULL,
description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Parameter Estimation for the POT model.

    # FUNCTION:
    
    # Save Call:
    call = match.call()
    
    # Call pot() from evir: 
    fitted = .pot.evir(data = x, threshold = threshold, 
        nextremes = nextremes, run = run, picture = FALSE, ...) 
        
    # Compute Residuals:
    xi = fitted$par.ests[1]
    beta = fitted$par.ests[4]
    threshold = fitted$threshold 
    fitted$residuals = 
        as.vector(log(1 + (xi * (fitted$data - threshold))/beta)/xi)
    
    # Gaps:
    x = fitted
    x$rawdata = x$data
    n = length(as.numeric(x$rawdata))
    x$times = attributes(x$rawdata)$times
    if (is.character(x$times) || inherits(x$times, "POSIXt") || 
        inherits(x$times, "date") || inherits(x$times, "dates")) {
        x$times = as.POSIXlt(x$times)
        x$gaps = as.numeric(difftime(x$times[2:n], x$times[1:(n - 1)], 
            units = "days")) * x$intensity 
    } else {
        x$times = 1:n
        x$gaps = as.numeric(diff(x$times)) * x$intensity 
    }
    fitted$times = x$times
    fitted$rawdata = x$rawdata
    fitted$gaps = x$gaps
    
    # Add:
    fit = list()
    fit$fit = fitted
    fit$call = call
    fit$type = c("pot", "mle")
    fit$par.ests = fitted$par.ests
    fit$par.ses = fitted$par.ses
    fit$residuals = fitted$residuals
    fit$fitted.values = fitted$data - fitted$residuals
    fit$llh = fitted$nllh.final
    fit$converged = fitted$converged        
    class(fit) = c("list", "potFit")
    
    # Add title and description:
    if (is.null(title)) title = "POT Parameter Estimation"
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    new("fPOT",
        call = match.call(),
        data = list(x = x),
        method = fit$type,
        fit = fit,
        title = title,
        description = description)
}


# ------------------------------------------------------------------------------


print.fPOT =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print Method for object of class "fPOT"
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n" , x@title, "\n")
    
    # Function Call:
    cat("\nCall:\n ")
    cat(paste(deparse(x@call), sep = "\n", collapse = "\n"), "\n", sep = "")
            
    # Estimation Type:
    cat("\nEstimation Method:\n", x@method, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(x@fit$par.ests)
    
    # Decluster Run Length:
    if (!is.na(x@fit$fit$run))
    cat("\nDecluster Runlength:", x@fit$fit$run, "\n")
    
    # Desription:
    cat("\nDescription\n ", x@description, "\n\n")
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


plot.fPOT =
function(x, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for objects of class "fPOT".

    # FUNCTION:
    
    # Plot functions:   
    plot.1 <<- function(x, ...) {
        exceedances = x@data$x
        plot(exceedances, type = "h", col = "steelblue",
            main = "Point Process of Exceedances", ...) 
    }
    plot.2 <<- function(x, ...) {
	    gaps = as.numeric(diff(seriesPositions(x@data$x)))/(24*3600)
        plot(gaps, col = "steelblue", pch = 19,
            ylab = "Gaps", xlab = "Ordering", 
            main = "Scatterplot of Gaps", ...)
        lines(lowess(1:length(gaps), gaps)) 
        grid()
    } 
    plot.3 <<- function(x, ...) {
        gaps = as.numeric(diff(seriesPositions(x@data$x)))/(24*3600)
        .qplot.evir(gaps, col = "steelblue", pch = 19, 
            main = "QQ-Plot of Gaps", ...) 
        grid()
    }           
    plot.4 <<- function(x, ...) {
        gaps = as.numeric(diff(seriesPositions(x@data$x)))/(24*3600)
        acf(gaps, lag.max = 20, col = "steelblue",
            main = "ACF of Gaps", ...) 
    }
    plot.5 <<- function(x, ...) {
	    residuals = x@fit$residuals
        plot(residuals, col = "steelblue", pch = 19, 
            ylab = "Residuals", xlab = "Ordering", 
            main = "Scatterplot of Residuals", ...)
        lines(lowess(1:length(residuals), residuals)) 
        grid()
    }
    plot.6 <<- function (x, ...) {
	    residuals = x@fit$residuals
        .qplot.evir(residuals, col = "steelblue", pch = 19, 
            main = "QQ-Plot of Residuals", ...) 
    }
    plot.7 <<- function (x, ...) { 
        residuals = x@fit$residuals
        acf(residuals, lag.max = 20, 
            main = "ACF of Residuals", ...) 
    }
    plot.8 <<- function (x, ...) { 
        if (which == "ask") {
	        fit = x@fit
            plot.fGPD(fit)
            plot.fPOT(x, which = "ask") } 
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
            "plot.1", 
            "plot.2", 
            "plot.3",
            "plot.4", 
            "plot.5", 
            "plot.6",
            "plot.7",
            "plot.8"),
        which = which)
                    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


summary.fPOT =
function(object, doplot = TRUE, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Summary Method for object of class "fPOT"
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n" , object@title, "\n")
    
    # Function Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", 
        collapse = "\n"), "\n", sep = "")
            
    # Estimation Type:
    cat("\nEstimation Method:\n", object@method, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(object@fit$par.ests)
    cat("\n")
    
    # Decluster Run Length:
    if (!is.na(object@fit$fit$run))
    cat("\nDecluster Runlength:", object@fit$fit$run, "\n")
    
    # Summary:
    cat("\nStandard Deviations:\n"); print(object@fit$par.ses)
    cat("\nLog-Likelihood Value:\n ", object@fit$llh)
    cat("\n\nType of Convergence:\n ", object@fit$converged) 
    
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
# FROM EVIR


.pot.evir = 
function(data, threshold = NA, nextremes = NA, run = NA, picture = TRUE, ...)
{   # A copy from evir

    n = length(as.numeric(data))
    times = attributes(data)$times
    if(is.null(times)) {
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
    if(is.na(nextremes) && is.na(threshold)) {
        stop("Enter either a threshold or the number of upper extremes")
    }
    if(!is.na(nextremes) && !is.na(threshold)) {
        stop("Enter EITHER a threshold or the number of upper extremes")
    }
    if(!is.na(nextremes)) {
        threshold = findthresh(as.numeric(data), nextremes)
    }
    if(threshold > 10) {
        factor = 10^(floor(log10(threshold)))
        cat(paste("If singularity problems occur divide data",
            "by a factor, perhaps", factor, "\n"))
    }
    exceedances.its = structure(data[data > threshold], times =
        times[data > threshold])
    n.exceed = length(as.numeric(exceedances.its))
    p.less.thresh = 1 - n.exceed/n
    if(!is.na(run)) {
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
{   # A copy from evir

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

