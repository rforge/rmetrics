
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
#  potFit                   Fits with POT method
#   print.potFit             Print Method for object of class "potFit"
#   plot.potFit              Print Method for object of class "potFit"
#   summary.potFit           Summary Method for object of class "potFit"
# REQUIRES:
#  ts                       Package ts (is preloaded)
################################################################################


potSim = 
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
            units = "days"))}
            
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


potFit =
function(x, threshold = NA, nextremes = NA, run = NA, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Parameter Estimation for the POT model.

    # FUNCTION:
    
    # Call pot() from evir: 
    call = match.call()
    fitted = pot(data = x, threshold = threshold, nextremes = nextremes, 
        run = run, picture = FALSE, ...) 
        
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
            units = "days")) * x$intensity }
    else {
        x$times = 1:n
        x$gaps = as.numeric(diff(x$times)) * x$intensity }
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
    
    # Return Value:
    class(fit) = "potFit"
    fit
}


# ******************************************************************************


print.potFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print Method for object of class "potFit"
    
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
    
    # Decluster Run Length:
    if (!is.na(fit$fit$run))
    cat("\nDecluster Runlength:", x$fit$run, "\n")
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


plot.potFit =
function(x, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for objects of class "potFit".

    # FUNCTION:
             
    # Plot functions:   
    plot.1 <<- function(x, ...) {
        plot(x$times, x$rawdata , type = "h", 
            main = "Point Process of Exceedances", ...) }
    plot.2 <<- function(x, ...) {
        plot(x$gaps, ylab = "Gaps", xlab = "Ordering", 
            main = "Scatterplot of Gaps", ...)
        lines(lowess(1:length(x$gaps), x$gaps)) } 
    plot.3 <<- function(x, ...) {
        qplot(x$gaps, 
            main = "QQ-Plot of Gaps", ...) }           
    plot.4 <<- function(x, ...) {
        acf(x$gaps, lag.max=20, 
            main = "ACF of Gaps", ...) }
    plot.5 <<- function(x, ...) {
        plot(x$residuals, ylab = "Residuals", xlab = "Ordering", 
            main = "Scatterplot of Residuals", ...)
        lines(lowess(1:length(x$residuals), x$residuals)) }
    plot.6 <<- function (x, ...) {
        qplot(x$residuals, 
            main = "QQ-Plot of Residuals", ...) }
    plot.7 <<- function (x, ...) { 
        acf(x$residuals, lag.max = 20, 
            main = "ACF of Residuals", ...) }
    fit <<- fit; plot.8 <<- function (x, ...) { 
        if (which == "ask") {
            plot.gpd(x)
            plot.potFit(fit, which = "ask") } }

    # Plot:
    interactivePlot(
        x = x$fit,
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


summary.potFit =
function(object, doplot = TRUE, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Summary Method for object of class "potFit"
    
    # FUNCTION:

    # Print:
    print(object, ...)
    
    # Summary:
    cat("\nStandard Deviations:\n"); print(object$par.ses)
    cat("\nLog-Likelihood Value: ", object$llh)
    cat("\nType of Convergence:  ", object$converged, "\n") 
    cat("\n")
    
    # Plot:
    if (doplot) {
        plot.potFit(object, which = which, ...)
    }
    cat("\n")
    
    # Return Value:
    invisible(object)
}


################################################################################

