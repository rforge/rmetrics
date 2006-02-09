
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
# FUNCTION:					POINT PROCESS MODELLING FROM ISMEV:
#  ppFit					 Fits Point Process Model
#   print.ppFit			      Print Method for object of class "ppFit"
#   plot.ppFit			      Plot Method for object of class "ppFit"
#   summary.ppFit			  Summary Method for object of class "ppFit"
################################################################################


ppFit =
function(x, threshold, npy = 365, y = NULL, mul = NULL, sigl = NULL, 
shl = NULL, mulink = identity, siglink = identity, shlink = identity, 
method = "Nelder-Mead", maxit = 10000, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	
	# FUNCTION:
    
    # Function Call:
    call = match.call()
    # Fit Parameters:
	fitted = pp.fit(xdat = x, threshold = threshold, npy = npy, ydat = y, 
		mul = mul, sigl = sigl, shl = shl, mulink = mulink, siglink = siglink, 
		shlink = shlink, show = FALSE, method = method, maxit = maxit, ...)	
	names(fitted$mle) =names(fitted$se) = c("xi", "sigma", "mu")
	# Compute Residuals:
	residuals = NA
	# cat("\nVariance Covariance Matrix:\n")
	# covar = fit$cov
	# covar[1,1] = fit$cov[3,3]
	# covar[3,3] = fit$cov[1,1]
	# covar[1,2] = covar[2,1] = fit$cov[2,3]
	# covar[2,3] = covar[3,2] = fit$cov[1,2] 
	# print(covar)	
		
	# Add:
	fit= list()
	fit$fit = fitted
	fit$call = call
	fit$type = c("pp", "mle")
	fit$par.ests = fitted$mle
	fit$par.ses = fitted$se
	fit$residuals = residuals
	fit$fitted.values = x - residuals
	fit$llh = fitted$nllh
	fit$converged = fitted$conv	
	
	# Return Value:
	class(fit) = "ppFit"
    fit
}


# ------------------------------------------------------------------------------


print.ppFit =
function(x, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Print method for an object of class 'ppFit'
	
	# FUNCTION:
	
	# Print Call:
	cat("\nCall:\n")
	cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "") 	
	
	if (x$fit$trans) {	
		# Still to do:
		print.default(x) }	
	else {	
		# Parameters - We use the same order as in gevFit:
		cat("\nParameter Estimate:\n")
		print(x$par.ests) }
		
	# Return Value:
	invisible(x)
}
	

# ------------------------------------------------------------------------------


plot.ppFit =
function(x, which = "ask", ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Plot method for an object of class 'ppFit'
	
	# FUNCTION:
	
	# Plot Functions:
    if (x$fit$trans) {   
	    plot.1 <<- function(x, ...) {
		    n <- length(x$data)
	    	xx <- (1:n)/(n + 1)
	    	plot(xx, sort(x$data), xlab = "empirical", ylab = "model")
	        abline(0, 1, col = 3)
	        title("Residual Probability Plot") }  
        plot.2 <<- function(x, ...) {
	        n <- length(x$data)
	    	xx <- (1:n)/(n + 1)
	    	plot(-log(1 - xx), -log(1 - sort(x$data)), ylab = "empirical", 
	            xlab = "model")
	        abline(0, 1, col = 3)
	        title("Residual quantile Plot (Exptl. Scale)") } }
    else {
	    plot.1 <<- function(x, ...) {
		    # Probability Plot:
        	pp.pp(x$mle, x$threshold, x$npy, x$data) }      
        plot.2 <<- function(x, ...) {
	        # Quantile Plot:
        	pp.qq(x$mle, x$threshold, x$npy, x$data) } }
	
	# Plot:
	if (x$fit$trans) {
		interactivePlot(
			x = x$fit,
			choices = c(
				"Residual Probability Plot",
				"Residual Quantile Plot"),
			plotFUN = c(
				"plot.1", 
				"plot.2"),
			which = which) }
	else {
		interactivePlot(
			x = x$fit,
			choices = c(
				"Probability Plot",
				"Quantile Plot"),
			plotFUN = c(
				"plot.1", 
				"plot.2"),
			which = which) }
			
    # Return Value:
    invisible(x)
}

	
# ------------------------------------------------------------------------------


summary.ppFit =
function(object, doplot = TRUE, which = "all", ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Summary method for an object of class 'ppFit'
	
	# FUNCTION:
	
	# Print:
	print(object, ...)
	
	# Summary:
	cat("\nStandard Deviations:\n"); print(object$par.ses)
	cat("\nLog-Likelihood Value: ", object$llh)
	cat("\nType of Convergence:  ", object$converged, "\n") 
	cat("\n")
	
	# Plot:
	if (doplot) plot.ppFit(object, which = which, ...)
	cat("\n")
	
	# Return Value:
	invisible(object)
}


# ******************************************************************************

