
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
# FUNCTION:                 R-LARGEST ORDER MODELLING FROM ISMEV:
#  rlargFit                  Fits r-largest Order Statistic Model
#   print.rlargFit            Print Method for object of class "rlargFit"
#   plot.rlargFit             Plot Method for object of class "rlargFit"
#   summary.rlargFit          Summary Method for object of class "rlargFit"
################################################################################


rlargFit =
function(x, r = dim(x)[2], y = NULL, mul = NULL, sigl = NULL, shl = NULL, 
mulink = identity, siglink = identity, shlink = identity, method = 
"Nelder-Mead", maxit = 10000, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Maximum-likelihood fitting for the order statistic model,
    #   including generalized linear modelling of each parameter.
    
    # FUNCTION:
   
    # Function Call:
    call = match.call()
    
    # Fit Parameters
    fitted = rlarg.fit(xdat = x, r = r, ydat = y, mul = mul, sigl = sigl, 
        shl = shl, mulink = mulink, siglink = siglink, shlink = shlink, 
        show = FALSE, method = method, maxit = maxit, ...)
    
    # Further Values:
    mle = rev(fitted$mle)
    se = rev(fitted$se)
    names(mle) = names(se) = c("xi", "sigma", "mu")
    covar = fitted$cov
    covar[1,1] = fitted$cov[3,3]
    covar[3,3] = fitted$cov[1,1]
    covar[1,2] = covar[2,1] = fitted$cov[2,3]
    covar[2,3] = covar[3,2] = fitted$cov[1,2]
    
    # Make Unique:
    fit = list()
    fit$fit = fitted    
    fit$call = call
    fit$type = c("mle", "rlarg")
    fit$par.ests = mle
    fit$par.ses = se
    fit$residuals = as.matrix(fitted$data)
    fit$fitted.values = as.matrix(x) - fit$residuals
    fit$cov = covar
    fit$llh = fitted$nllh 
    fit$converged = fitted$conv 
    
    # Return Value:
    class(fit) = "rlargFit"
    fit
}


# ******************************************************************************


print.rlargFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print Method for object of class "rlargFit"
    
    # Notes:
    #   The ismev package has no print method. It uses the command
    #   > summary.rlargFit(fit = fit, details = FALSE, doplot = FALSE, ...) 

    # FUNCTION:
    
    # Function Call:
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "") 
            
    # Estimation Type:
    cat("\nEstimation Type:", x$type, "\n") 
    
    # Estimated summaryParameters:
    cat("\nEstimated Parameters:\n")
    print(x$par.ests)
    cat("\n")
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


plot.rlargFit = 
function(x, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for objects of class "rlargFit".
    
    # FUNCTION:
    
    # Plot Functions:
    if (x$fit$trans) {
        # Non-Stationary:
        plot.1 <<-  function(x, ...) {
            for (i in 1:z$r) {
                # Probability and Quantile Plots:
                rlarg.pp(c(0, 1, 0), x$data[, 1:x$r], i)
                rlarg.qq(c(0, 1, 0), x$data[, 1:x$r], i) } } }
    else {  
        # Stationary - GEV Plots:
        plot.1 <<- function(x, ...) {
            gev.pp(x$mle, x$data[, 1]) }
        plot.2 <<- function(x, ...) {
            gev.qq(x$mle, x$data[, 1]) }
        plot.3 <<- function(x, ...) {
            gev.rl(x$mle, x$cov, x$data[, 1]) }
        plot.4 <<- function(x, ...) {
            gev.his(x$mle, x$data[, 1]) }  
        fit <<- fit; plot.5 <<- function(x, ...) {
            par(ask = TRUE)
            for (i in 1:fit$fit$r) {
                # Probability and Quantile Plots:
                rlarg.pp(x$mle, x$data, i)
                rlarg.qq(x$mle, x$data, i) } 
            par(ask = FALSE) } } 
                        
    # Plot:
    if (x$fit$trans) {
        interactivePlot(
            x = x$fit,
            choices = c(
                "Probability Plot",
                "Quantile Plot"),
            plotFUN = c(
                "plot.1", 
                "plot.2"),
            which = which) }
    else {
        interactivePlot(
            x = x$fit,
            choices = c(
                "GEV Probability Plot",
                "GEV Quantile Plot",
                "GEV Return Level Plot",
                "GEV Histogram Plot",
                "R-Largest PP and QQ Plots"),
            plotFUN = c(
                "plot.1", 
                "plot.2",
                "plot.3", 
                "plot.4",
                "plot.5"),
            which = which) }
        
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


summary.rlargFit =
function(object, doplot = TRUE, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Summary Method for object of class "rlargFit".
    
    # FUNCTION:
    
    # Print:
    print(object, ...)
    
    # Summary:
    cat("\nStandard Deviations:\n"); print(object$par.ses)
    cat("\nLog-Likelihood Value: ", object$llh)
    cat("\nType of Convergence:  ", object$converged, "\n") 
    cat("\n")
    
    # Plot:
    if (doplot) plot(object, which = which, ...)
    cat("\n")

    # Return Value:
    invisible(object)
}


################################################################################

